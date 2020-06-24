module NoMissingTypeExpose exposing (rule)

{-|

@docs rule

-}

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


{-| Forbids exposed functions that use or return a private type.

If a used type is not exposed then it is impossible to annotate functions or values that use them outside of the module.

    import NoMissingTypeExpose

    config : List Rule
    config =
        [ NoMissingTypeExpose.rule
        ]


## Fail

    module Happiness exposing (happy, toString)

    -- Type `Happiness` is private because it's not been exposed

    type Happiness
        = Happy

    -- Private type `Happiness` used by exposed function `toString`
    toString : Happiness -> String
    toString happiness =
        "Happy"

    -- Private type `Happiness` used by exposed function `happy`
    happy : Happiness
    happy =
        Happy

## Success

    module Happiness exposing (Happy, happy, toString)

    type Happiness
        = Happy

    toString : Happiness -> String
    toString happiness =
        "Happy"

    happy : Happiness
    happy =
        Happy

-}
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
            { context
                | exposedModules =
                    Package (exposed |> elmProjectExposedList |> Set.fromList)
            }

        Elm.Project.Application _ ->
            { context | exposedModules = Application }


elmProjectExposedList : Elm.Project.Exposed -> List String
elmProjectExposedList exposed =
    case exposed of
        Elm.Project.ExposedList list ->
            List.map Elm.Module.toString list

        Elm.Project.ExposedDict dict ->
            List.foldl
                (Tuple.second
                    >> List.map Elm.Module.toString
                    >> (++)
                )
                []
                dict


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
    case context of
        InternalModule data ->
            ( [], InternalModule { data | exposes = Module.exposingList mod } )

        ExposedModule data ->
            ( [], ExposedModule { data | exposes = Module.exposingList mod } )


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor (Node _ { moduleName, moduleAlias, exposingList }) context =
    case context of
        InternalModule _ ->
            ( [], context )

        ExposedModule data ->
            ( []
            , ExposedModule
                (data
                    |> rememberImportedModuleAlias (Node.value moduleName) moduleAlias
                    |> rememberImportedExposingList (Node.value moduleName) exposingList
                )
            )


rememberImportedModuleAlias : ModuleName -> Maybe (Node ModuleName) -> ExposedModuleData -> ExposedModuleData
rememberImportedModuleAlias moduleName maybeModuleAlias data =
    case maybeModuleAlias of
        Just (Node _ moduleAlias) ->
            { data
                | exposedModules =
                    addExposedModuleAlias moduleName
                        (String.join "." moduleAlias)
                        data.exposedModules
            }

        Nothing ->
            data


rememberImportedExposingList : ModuleName -> Maybe (Node Exposing) -> ExposedModuleData -> ExposedModuleData
rememberImportedExposingList moduleName maybeExposing data =
    case maybeExposing of
        Just (Node _ (Exposing.Explicit list)) ->
            List.foldl (rememberImportedExpose moduleName) data list

        Just (Node _ (Exposing.All _)) ->
            rememberImportedModuleTypes moduleName data

        Nothing ->
            data


rememberImportedExpose : ModuleName -> Node Exposing.TopLevelExpose -> ExposedModuleData -> ExposedModuleData
rememberImportedExpose moduleName (Node _ expose) data =
    case expose of
        Exposing.TypeExpose { name } ->
            data |> rememberImportedType moduleName name

        Exposing.TypeOrAliasExpose name ->
            data |> rememberImportedType moduleName name

        Exposing.FunctionExpose _ ->
            data

        Exposing.InfixExpose _ ->
            data


rememberImportedModuleTypes : ModuleName -> ExposedModuleData -> ExposedModuleData
rememberImportedModuleTypes moduleName data =
    case Dict.get moduleName data.moduleTypes of
        Just types ->
            Set.foldl (rememberImportedType moduleName) data types

        Nothing ->
            data


rememberImportedType : ModuleName -> String -> ExposedModuleData -> ExposedModuleData
rememberImportedType moduleName typeName data =
    { data
        | importedTypes = Dict.insert typeName moduleName data.importedTypes
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
            case context of
                InternalModule data ->
                    InternalModule (rememberExposedType name data)

                ExposedModule data ->
                    ExposedModule
                        (data
                            |> rememberDeclaredType name
                            |> rememberValueConstructorList name constructors
                        )

        Declaration.AliasDeclaration { name } ->
            case context of
                InternalModule data ->
                    InternalModule (rememberExposedType name data)

                ExposedModule data ->
                    ExposedModule (rememberDeclaredType name data)

        Declaration.FunctionDeclaration { signature } ->
            case context of
                InternalModule _ ->
                    context

                ExposedModule data ->
                    ExposedModule (rememberFunctionSignature signature data)

        _ ->
            context


rememberExposedType : Node String -> InternalModuleData -> InternalModuleData
rememberExposedType (Node _ name) data =
    if isTypeExposed data.exposes name then
        { data | exposedTypes = Set.insert name data.exposedTypes }

    else
        data


rememberDeclaredType : Node String -> ExposedModuleData -> ExposedModuleData
rememberDeclaredType (Node _ name) data =
    { data | declaredTypes = Set.insert name data.declaredTypes }


rememberValueConstructorList : Node String -> List (Node Type.ValueConstructor) -> ExposedModuleData -> ExposedModuleData
rememberValueConstructorList (Node _ name) list data =
    if isTypeExposedOpen data.exposes name then
        List.foldl rememberValueConstructor data list

    else
        data


rememberValueConstructor : Node Type.ValueConstructor -> ExposedModuleData -> ExposedModuleData
rememberValueConstructor (Node _ { arguments }) data =
    rememberTypeAnnotationList arguments data


rememberFunctionSignature : Maybe (Node Signature) -> ExposedModuleData -> ExposedModuleData
rememberFunctionSignature maybeSignature data =
    case maybeSignature of
        Just (Node _ { name, typeAnnotation }) ->
            if Exposing.exposesFunction (Node.value name) data.exposes then
                rememberTypeAnnotation typeAnnotation data

            else
                data

        Nothing ->
            data


rememberRecordFieldList : List (Node TypeAnnotation.RecordField) -> ExposedModuleData -> ExposedModuleData
rememberRecordFieldList fields data =
    List.foldl rememberRecordField data fields


rememberRecordField : Node TypeAnnotation.RecordField -> ExposedModuleData -> ExposedModuleData
rememberRecordField (Node _ ( _, typeAnnotation )) data =
    rememberTypeAnnotation typeAnnotation data


rememberTypeAnnotationList : List (Node TypeAnnotation) -> ExposedModuleData -> ExposedModuleData
rememberTypeAnnotationList list data =
    List.foldl rememberTypeAnnotation data list


rememberTypeAnnotation : Node TypeAnnotation -> ExposedModuleData -> ExposedModuleData
rememberTypeAnnotation (Node _ typeAnnotation) data =
    case typeAnnotation of
        TypeAnnotation.Typed name list ->
            data
                |> rememberExposedSignatureType name
                |> rememberTypeAnnotationList list

        TypeAnnotation.FunctionTypeAnnotation left right ->
            data
                |> rememberTypeAnnotation left
                |> rememberTypeAnnotation right

        TypeAnnotation.Tupled list ->
            data
                |> rememberTypeAnnotationList list

        TypeAnnotation.Record fields ->
            data
                |> rememberRecordFieldList fields

        TypeAnnotation.GenericRecord _ (Node _ fields) ->
            data
                |> rememberRecordFieldList fields

        TypeAnnotation.Unit ->
            data

        TypeAnnotation.GenericType _ ->
            data


rememberExposedSignatureType : Node ( ModuleName, String ) -> ExposedModuleData -> ExposedModuleData
rememberExposedSignatureType qualifiedName data =
    { data
        | exposedSignatureTypes = qualifiedName :: data.exposedSignatureTypes
    }


finalEvaluation : ModuleContext -> List (Rule.Error {})
finalEvaluation context =
    case context of
        InternalModule _ ->
            []

        ExposedModule data ->
            data.exposedSignatureTypes
                |> List.filter (isTypePrivate data)
                |> List.map makeError


isTypePrivate : ExposedModuleData -> Node ( ModuleName, String ) -> Bool
isTypePrivate data (Node _ typeCall) =
    case moduleNameForType data typeCall of
        ( [], name ) ->
            if Set.member name data.declaredTypes then
                not (isTypeExposed data.exposes name)

            else
                False

        ( moduleName, _ ) ->
            not (isModuleExposed data.exposedModules moduleName)


moduleNameForType : ExposedModuleData -> ( ModuleName, String ) -> ( ModuleName, String )
moduleNameForType data ( moduleName, typeName ) =
    case Dict.get typeName data.importedTypes of
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
            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
            ]
        }
        range


formatTypeName : ( ModuleName, String ) -> String
formatTypeName ( moduleName, name ) =
    String.join "." (moduleName ++ [ name ])


fromProjectToModuleContext : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModuleContext _ (Node _ moduleName) { exposedModules, moduleTypes } =
    if isModuleExposed exposedModules moduleName then
        initialExposedModuleContext exposedModules moduleTypes

    else
        initialInternalModuleContext


fromModuleToProjectContext : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProjectContext _ (Node _ moduleName) context =
    case context of
        InternalModule { exposedTypes } ->
            { initialProjectContext | moduleTypes = Dict.singleton moduleName exposedTypes }

        ExposedModule _ ->
            initialProjectContext


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


initialInternalModuleContext : ModuleContext
initialInternalModuleContext =
    InternalModule initialAnyModuleData


initialExposedModuleContext : ExposedModules -> Dict ModuleName (Set String) -> ModuleContext
initialExposedModuleContext exposedModules moduleTypes =
    ExposedModule
        { declaredTypes = Set.empty
        , exposedModules = exposedModules
        , exposedSignatureTypes = []
        , exposes = Exposing.Explicit []
        , importedTypes = Dict.empty
        , moduleTypes = moduleTypes
        }


initialAnyModuleData : InternalModuleData
initialAnyModuleData =
    { exposedTypes = Set.empty
    , exposes = Exposing.Explicit []
    }


type alias ProjectContext =
    { exposedModules : ExposedModules
    , moduleTypes : Dict ModuleName (Set String)
    }


type ModuleContext
    = InternalModule InternalModuleData
    | ExposedModule ExposedModuleData


type alias InternalModuleData =
    { exposedTypes : Set String
    , exposes : Exposing
    }


type alias ExposedModuleData =
    { declaredTypes : Set String
    , exposedModules : ExposedModules
    , exposedSignatureTypes : List (Node ( ModuleName, String ))
    , exposes : Exposing
    , importedTypes : Dict String ModuleName
    , moduleTypes : Dict ModuleName (Set String)
    }


type ExposedModules
    = Application
    | Package (Set String)
