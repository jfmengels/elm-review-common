module NoMissingTypeExpose exposing (rule)

import Dict exposing (Dict)
import Elm.Module
import Elm.Project exposing (Project)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Project.Dependency exposing (name)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMissingTypeExpose" initialContext
        |> Rule.withElmJsonModuleVisitor elmJsonVisitor
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


elmJsonVisitor : Maybe Project -> Context -> Context
elmJsonVisitor maybeProject context =
    case maybeProject of
        Just (Elm.Project.Package { exposed }) ->
            case exposed of
                Elm.Project.ExposedList list ->
                    { context
                        | exposedModules =
                            list
                                |> List.map Elm.Module.toString
                                |> Package
                    }

                Elm.Project.ExposedDict list ->
                    { context
                        | exposedModules =
                            list
                                |> List.concatMap
                                    (Tuple.second
                                        >> List.map Elm.Module.toString
                                    )
                                |> Package
                    }

        Just (Elm.Project.Application _) ->
            { context | exposedModules = Application }

        Nothing ->
            context


moduleDefinitionVisitor : Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor (Node _ mod) context =
    ( [], { context | exposes = Module.exposingList mod } )


importVisitor : Node Import -> Context -> ( List nothing, Context )
importVisitor (Node _ { moduleName, moduleAlias, exposingList }) context =
    ( []
    , context
        |> rememberImportedModuleAlias (Node.value moduleName) moduleAlias
        |> rememberImportedExposingList (Node.value moduleName) exposingList
    )


rememberImportedModuleAlias : ModuleName -> Maybe (Node ModuleName) -> Context -> Context
rememberImportedModuleAlias moduleName maybeModuleAlias context =
    case maybeModuleAlias of
        Just (Node _ moduleAlias) ->
            { context
                | exposedModules =
                    addExposedModuleAlias moduleName
                        (String.join "." moduleAlias)
                        context.exposedModules
            }

        Nothing ->
            context


rememberImportedExposingList : ModuleName -> Maybe (Node Exposing) -> Context -> Context
rememberImportedExposingList moduleName maybeExposing context =
    case maybeExposing of
        Just (Node _ (Exposing.Explicit list)) ->
            List.foldl (rememberImportedExpose moduleName) context list

        Just (Node _ (Exposing.All _)) ->
            context

        Nothing ->
            context


rememberImportedExpose : ModuleName -> Node Exposing.TopLevelExpose -> Context -> Context
rememberImportedExpose moduleName (Node _ expose) context =
    case expose of
        Exposing.TypeExpose { name } ->
            context |> rememberImportedType moduleName name

        Exposing.TypeOrAliasExpose name ->
            context |> rememberImportedType moduleName name

        Exposing.FunctionExpose _ ->
            context

        Exposing.InfixExpose _ ->
            context


rememberImportedType : ModuleName -> String -> Context -> Context
rememberImportedType moduleName typeName context =
    { context
        | importedTypes = Dict.insert typeName moduleName context.importedTypes
    }


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor nodes context =
    case context.exposes of
        Exposing.All _ ->
            ( [], context )

        _ ->
            ( []
            , List.foldl rememberDeclaration context nodes
            )


rememberDeclaration : Node Declaration -> Context -> Context
rememberDeclaration (Node _ declaration) context =
    case declaration of
        Declaration.CustomTypeDeclaration { name, constructors } ->
            context
                |> rememberDeclaredType name
                |> rememberValueConstructorList name constructors

        Declaration.FunctionDeclaration { signature } ->
            rememberFunctionSignature signature context

        _ ->
            context


rememberDeclaredType : Node String -> Context -> Context
rememberDeclaredType (Node _ name) context =
    { context | declaredTypes = Set.insert name context.declaredTypes }


rememberValueConstructorList : Node String -> List (Node Type.ValueConstructor) -> Context -> Context
rememberValueConstructorList (Node _ name) list context =
    if isTypeExposedOpen context.exposes name then
        List.foldl rememberValueConstructor context list

    else
        context


isTypeExposedOpen : Exposing -> String -> Bool
isTypeExposedOpen exposes name =
    case exposes of
        Exposing.All _ ->
            True

        Exposing.Explicit list ->
            List.any (isExposingAnOpenTypeNamed name) list


isExposingAnOpenTypeNamed : String -> Node Exposing.TopLevelExpose -> Bool
isExposingAnOpenTypeNamed needle (Node _ expose) =
    case expose of
        Exposing.TypeExpose { name, open } ->
            name == needle && open /= Nothing

        _ ->
            False


rememberValueConstructor : Node Type.ValueConstructor -> Context -> Context
rememberValueConstructor (Node _ { arguments }) context =
    rememberTypeAnnotationList arguments context


rememberFunctionSignature : Maybe (Node Signature) -> Context -> Context
rememberFunctionSignature maybeSignature context =
    case maybeSignature of
        Just (Node _ { name, typeAnnotation }) ->
            if Exposing.exposesFunction (Node.value name) context.exposes then
                rememberTypeAnnotation typeAnnotation context

            else
                context

        Nothing ->
            context


rememberRecordFieldList : List (Node TypeAnnotation.RecordField) -> Context -> Context
rememberRecordFieldList fields context =
    List.foldl rememberRecordField context fields


rememberRecordField : Node TypeAnnotation.RecordField -> Context -> Context
rememberRecordField (Node _ ( _, typeAnnotation )) context =
    context
        |> rememberTypeAnnotation typeAnnotation


rememberTypeAnnotationList : List (Node TypeAnnotation) -> Context -> Context
rememberTypeAnnotationList list context =
    List.foldl rememberTypeAnnotation context list


rememberTypeAnnotation : Node TypeAnnotation -> Context -> Context
rememberTypeAnnotation (Node _ typeAnnotation) context =
    case typeAnnotation of
        TypeAnnotation.Typed name list ->
            context
                |> rememberExposedSignatureType name
                |> rememberTypeAnnotationList list

        TypeAnnotation.FunctionTypeAnnotation left right ->
            context
                |> rememberTypeAnnotation left
                |> rememberTypeAnnotation right

        TypeAnnotation.Tupled list ->
            context
                |> rememberTypeAnnotationList list

        TypeAnnotation.Record fields ->
            context
                |> rememberRecordFieldList fields

        TypeAnnotation.GenericRecord _ (Node _ fields) ->
            context
                |> rememberRecordFieldList fields

        TypeAnnotation.Unit ->
            context

        TypeAnnotation.GenericType _ ->
            context


rememberExposedSignatureType : Node ( ModuleName, String ) -> Context -> Context
rememberExposedSignatureType qualifiedName context =
    { context
        | exposedSignatureTypes = qualifiedName :: context.exposedSignatureTypes
    }


finalEvaluation : Context -> List (Rule.Error {})
finalEvaluation context =
    context.exposedSignatureTypes
        |> List.filter (isTypePrivate context)
        |> List.map makeError


isTypePrivate : Context -> Node ( ModuleName, String ) -> Bool
isTypePrivate context (Node _ typeCall) =
    case moduleNameForType context typeCall of
        ( [], name ) ->
            case context.exposes of
                Exposing.All _ ->
                    False

                Exposing.Explicit list ->
                    if Set.member name context.declaredTypes then
                        not (List.any (isExposingATypeNamed name) list)

                    else
                        False

        ( moduleName, _ ) ->
            not (isModuleExposed context.exposedModules moduleName)


moduleNameForType : Context -> ( ModuleName, String ) -> ( ModuleName, String )
moduleNameForType context ( moduleName, typeName ) =
    case Dict.get typeName context.importedTypes of
        Just typeModuleName ->
            ( typeModuleName, typeName )

        _ ->
            ( moduleName, typeName )


isExposingATypeNamed : String -> Node Exposing.TopLevelExpose -> Bool
isExposingATypeNamed needle (Node _ topLevelExpose) =
    case topLevelExpose of
        Exposing.InfixExpose _ ->
            False

        Exposing.FunctionExpose _ ->
            False

        Exposing.TypeOrAliasExpose name ->
            name == needle

        Exposing.TypeExpose { name } ->
            name == needle


addExposedModuleAlias : ModuleName -> String -> ExposedModules -> ExposedModules
addExposedModuleAlias moduleName moduleAlias exposedModules =
    case exposedModules of
        Application ->
            exposedModules

        Package list ->
            if List.member (String.join "." moduleName) list then
                Package (moduleAlias :: list)

            else
                exposedModules


isModuleExposed : ExposedModules -> ModuleName -> Bool
isModuleExposed exposedModules moduleName =
    case exposedModules of
        Application ->
            True

        Package list ->
            List.member (String.join "." moduleName) list


makeError : Node ( ModuleName, String ) -> Rule.Error {}
makeError (Node range typeName) =
    let
        formattedName =
            formatTypeName typeName
    in
    Rule.error
        { message = "Private type `" ++ formattedName ++ "` used by exposed function"
        , details =
            [ "Type `" ++ formattedName ++ "` is not exposed but is used by an exposed function."
            ]
        }
        range


formatTypeName : ( ModuleName, String ) -> String
formatTypeName ( moduleName, name ) =
    String.join "." (moduleName ++ [ name ])


initialContext : Context
initialContext =
    { exposes = Exposing.Explicit []
    , exposedModules = Application
    , exposedSignatureTypes = []
    , declaredTypes = Set.empty
    , importedTypes = Dict.empty
    }


type alias Context =
    { exposes : Exposing
    , exposedModules : ExposedModules
    , exposedSignatureTypes : List (Node ( ModuleName, String ))
    , declaredTypes : Set String
    , importedTypes : Dict String ModuleName
    }


type ExposedModules
    = Application
    | Package (List String)
