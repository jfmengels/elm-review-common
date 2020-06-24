module NoMissingTypeExpose exposing (rule)

import Dict exposing (Dict)
import Elm.Docs exposing (Module)
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
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newProjectRuleSchema "NoMissingTypeExpose" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withDependenciesProjectVisitor dependencyDictVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModuleContext
            , fromModuleToProject = fromModuleToProjectContext
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.fromProjectRuleSchema


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject context =
    case maybeProject of
        Just { project } ->
            ( [], context |> rememberElmJsonProject project )

        Nothing ->
            ( [], context )


rememberElmJsonProject : Project -> ProjectContext -> ProjectContext
rememberElmJsonProject project context =
    case project of
        Elm.Project.Package { exposed } ->
            case exposed of
                Elm.Project.ExposedList list ->
                    { context
                        | exposedModules =
                            list
                                |> List.map Elm.Module.toString
                                |> Set.fromList
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
                                |> Set.fromList
                                |> Package
                    }

        Elm.Project.Application _ ->
            { context | exposedModules = Application }


dependencyDictVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependencyDictVisitor dependencies context =
    ( [], dependencies |> Dict.values |> List.foldl rememberDependency context )


rememberDependency : Dependency -> ProjectContext -> ProjectContext
rememberDependency dependency context =
    dependency |> Dependency.modules |> List.foldl rememberDependencyModule context


rememberDependencyModule : Elm.Docs.Module -> ProjectContext -> ProjectContext
rememberDependencyModule { name } context =
    { context
        | exposedModules = addExposedModule name context.exposedModules
    }


moduleVisitor :
    Rule.ModuleRuleSchema state ModuleContext
    -> Rule.ModuleRuleSchema { state | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor (Node _ mod) context =
    ( [], { context | exposes = Module.exposingList mod } )


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor (Node _ { moduleName, moduleAlias, exposingList }) context =
    ( []
    , context
        |> rememberImportedModuleAlias (Node.value moduleName) moduleAlias
        |> rememberImportedExposingList (Node.value moduleName) exposingList
    )


rememberImportedModuleAlias : ModuleName -> Maybe (Node ModuleName) -> ModuleContext -> ModuleContext
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


rememberImportedExposingList : ModuleName -> Maybe (Node Exposing) -> ModuleContext -> ModuleContext
rememberImportedExposingList moduleName maybeExposing context =
    case maybeExposing of
        Just (Node _ (Exposing.Explicit list)) ->
            List.foldl (rememberImportedExpose moduleName) context list

        Just (Node _ (Exposing.All _)) ->
            rememberImportedModuleTypes moduleName context

        Nothing ->
            context


rememberImportedExpose : ModuleName -> Node Exposing.TopLevelExpose -> ModuleContext -> ModuleContext
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


rememberImportedModuleTypes : ModuleName -> ModuleContext -> ModuleContext
rememberImportedModuleTypes moduleName context =
    case Dict.get moduleName context.moduleTypes of
        Just types ->
            Set.foldl (rememberImportedType moduleName) context types

        Nothing ->
            context


rememberImportedType : ModuleName -> String -> ModuleContext -> ModuleContext
rememberImportedType moduleName typeName context =
    { context
        | importedTypes = Dict.insert typeName moduleName context.importedTypes
    }


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor nodes context =
    ( []
    , List.foldl rememberDeclaration context nodes
    )


rememberDeclaration : Node Declaration -> ModuleContext -> ModuleContext
rememberDeclaration (Node _ declaration) context =
    case declaration of
        Declaration.CustomTypeDeclaration { name, constructors } ->
            context
                |> rememberExposedType name
                |> rememberDeclaredType name
                |> rememberValueConstructorList name constructors

        Declaration.FunctionDeclaration { signature } ->
            rememberFunctionSignature signature context

        _ ->
            context


rememberExposedType : Node String -> ModuleContext -> ModuleContext
rememberExposedType (Node _ name) context =
    if isTypeExposed context.exposes name then
        { context | exposedTypes = Set.insert name context.exposedTypes }

    else
        context


rememberDeclaredType : Node String -> ModuleContext -> ModuleContext
rememberDeclaredType (Node _ name) context =
    { context | declaredTypes = Set.insert name context.declaredTypes }


rememberValueConstructorList : Node String -> List (Node Type.ValueConstructor) -> ModuleContext -> ModuleContext
rememberValueConstructorList (Node _ name) list context =
    if isTypeExposedOpen context.exposes name then
        List.foldl rememberValueConstructor context list

    else
        context


rememberValueConstructor : Node Type.ValueConstructor -> ModuleContext -> ModuleContext
rememberValueConstructor (Node _ { arguments }) context =
    rememberTypeAnnotationList arguments context


rememberFunctionSignature : Maybe (Node Signature) -> ModuleContext -> ModuleContext
rememberFunctionSignature maybeSignature context =
    case maybeSignature of
        Just (Node _ { name, typeAnnotation }) ->
            if Exposing.exposesFunction (Node.value name) context.exposes then
                rememberTypeAnnotation typeAnnotation context

            else
                context

        Nothing ->
            context


rememberRecordFieldList : List (Node TypeAnnotation.RecordField) -> ModuleContext -> ModuleContext
rememberRecordFieldList fields context =
    List.foldl rememberRecordField context fields


rememberRecordField : Node TypeAnnotation.RecordField -> ModuleContext -> ModuleContext
rememberRecordField (Node _ ( _, typeAnnotation )) context =
    context
        |> rememberTypeAnnotation typeAnnotation


rememberTypeAnnotationList : List (Node TypeAnnotation) -> ModuleContext -> ModuleContext
rememberTypeAnnotationList list context =
    List.foldl rememberTypeAnnotation context list


rememberTypeAnnotation : Node TypeAnnotation -> ModuleContext -> ModuleContext
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


rememberExposedSignatureType : Node ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberExposedSignatureType qualifiedName context =
    { context
        | exposedSignatureTypes = qualifiedName :: context.exposedSignatureTypes
    }


finalEvaluation : ModuleContext -> List (Rule.Error {})
finalEvaluation context =
    context.exposedSignatureTypes
        |> List.filter (isTypePrivate context)
        |> List.map makeError


isTypePrivate : ModuleContext -> Node ( ModuleName, String ) -> Bool
isTypePrivate context (Node _ typeCall) =
    case moduleNameForType context typeCall of
        ( [], name ) ->
            if Set.member name context.declaredTypes then
                not (isTypeExposed context.exposes name)

            else
                False

        ( moduleName, _ ) ->
            not (isModuleExposed context.exposedModules moduleName)


moduleNameForType : ModuleContext -> ( ModuleName, String ) -> ( ModuleName, String )
moduleNameForType context ( moduleName, typeName ) =
    case Dict.get typeName context.importedTypes of
        Just typeModuleName ->
            ( typeModuleName, typeName )

        _ ->
            ( moduleName, typeName )


isTypeExposed : Exposing -> String -> Bool
isTypeExposed exposes name =
    case exposes of
        Exposing.All _ ->
            True

        Exposing.Explicit list ->
            List.any (isExposingATypeNamed name) list


isTypeExposedOpen : Exposing -> String -> Bool
isTypeExposedOpen exposes name =
    case exposes of
        Exposing.All _ ->
            True

        Exposing.Explicit list ->
            List.any (isExposingAnOpenTypeNamed name) list


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


isExposingAnOpenTypeNamed : String -> Node Exposing.TopLevelExpose -> Bool
isExposingAnOpenTypeNamed needle (Node _ expose) =
    case expose of
        Exposing.TypeExpose { name, open } ->
            name == needle && open /= Nothing

        _ ->
            False


addExposedModule : String -> ExposedModules -> ExposedModules
addExposedModule moduleName exposedModules =
    case exposedModules of
        Application ->
            exposedModules

        Package list ->
            Package (Set.insert moduleName list)


addExposedModuleAlias : ModuleName -> String -> ExposedModules -> ExposedModules
addExposedModuleAlias moduleName moduleAlias exposedModules =
    case exposedModules of
        Application ->
            exposedModules

        Package list ->
            if Set.member (String.join "." moduleName) list then
                Package (Set.insert moduleAlias list)

            else
                exposedModules


isModuleExposed : ExposedModules -> ModuleName -> Bool
isModuleExposed exposedModules moduleName =
    case exposedModules of
        Application ->
            True

        Package list ->
            Set.member (String.join "." moduleName) list


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


fromProjectToModuleContext : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModuleContext _ _ { exposedModules, moduleTypes } =
    initialModuleContext exposedModules moduleTypes


fromModuleToProjectContext : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProjectContext _ (Node _ moduleName) { exposedTypes } =
    { initialProjectContext
        | moduleTypes = Dict.singleton moduleName exposedTypes
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts old new =
    { exposedModules = foldExposedModules old.exposedModules new.exposedModules
    , moduleTypes = foldModuleTypes old.moduleTypes new.moduleTypes
    }


foldExposedModules : ExposedModules -> ExposedModules -> ExposedModules
foldExposedModules oldExposedModules newExposedModules =
    case ( oldExposedModules, newExposedModules ) of
        ( Application, Application ) ->
            Application

        ( Application, Package _ ) ->
            newExposedModules

        ( Package _, Application ) ->
            oldExposedModules

        ( Package oldList, Package newList ) ->
            Package (Set.union oldList newList)


foldModuleTypes : Dict ModuleName (Set String) -> Dict ModuleName (Set String) -> Dict ModuleName (Set String)
foldModuleTypes oldModuleTypes newModuleTypes =
    Dict.foldl foldModuleTypesHelp oldModuleTypes newModuleTypes


foldModuleTypesHelp : ModuleName -> Set String -> Dict ModuleName (Set String) -> Dict ModuleName (Set String)
foldModuleTypesHelp moduleName newTypes moduleTypes =
    case Dict.get moduleName moduleTypes of
        Just oldTypes ->
            Dict.insert moduleName (Set.union oldTypes newTypes) moduleTypes

        Nothing ->
            Dict.insert moduleName newTypes moduleTypes


initialProjectContext : ProjectContext
initialProjectContext =
    { exposedModules = Application
    , moduleTypes = Dict.empty
    }


initialModuleContext : ExposedModules -> Dict ModuleName (Set String) -> ModuleContext
initialModuleContext exposedModules moduleTypes =
    { exposes = Exposing.Explicit []
    , exposedModules = exposedModules
    , exposedSignatureTypes = []
    , exposedTypes = Set.empty
    , declaredTypes = Set.empty
    , importedTypes = Dict.empty
    , moduleTypes = moduleTypes
    }


type alias ProjectContext =
    { exposedModules : ExposedModules
    , moduleTypes : Dict ModuleName (Set String)
    }


type alias ModuleContext =
    { exposes : Exposing
    , exposedModules : ExposedModules
    , exposedSignatureTypes : List (Node ( ModuleName, String ))
    , exposedTypes : Set String
    , declaredTypes : Set String
    , importedTypes : Dict String ModuleName
    , moduleTypes : Dict ModuleName (Set String)
    }


type ExposedModules
    = Application
    | Package (Set String)
