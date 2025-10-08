module NoImportingEverythingWithFix exposing (rule)

{-| Rule to replace `exposing (..)` with explicit imports of what the module actually exposes.

This rule extends the existing NoImportingEverything rule to provide automatic fixes
by replacing `import Module exposing (..)` with `import Module exposing (explicit, list)`.

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs as Docs
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports imports using `exposing (..)` and provides automatic fixes to replace them with explicit imports.

    config =
        [ NoImportingEverythingWithFix.rule []
        ]

Teams often have an agreement on the list of imports from which it is okay to expose everything, so
you can configure a list of exceptions.

    config =
        [ NoImportingEverythingWithFix.rule [ "Html", "Html.Attributes" ]
        ]

🔧 Running with `--fix` will automatically fix all the reported errors by replacing `(..)` with explicit imports.

This rule analyzes external dependencies to get precise export information and also
collects internal module exports as it processes each file.

-}
rule : List String -> Rule
rule exceptions =
    Rule.newProjectRuleSchema "NoImportingEverythingWithFix" (initialProjectContext exceptions)
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema



-- CONTEXT


type alias ProjectContext =
    { exceptions : Set String
    , internalExports : Dict String (List String) -- Internal project modules: "Module.Name" -> ["export1", "export2"]
    , externalExports : Dict String (List String) -- External packages: "Module.Name" -> ["export1", "export2"]
    , importIssues : List ImportIssue -- Issues found during module processing
    }


type alias ImportIssue =
    { moduleName : String
    , importRange : Range
    , moduleKey : Rule.ModuleKey
    }


type alias ModuleContext =
    { exceptions : Set String
    , internalExports : Dict String (List String)
    , externalExports : Dict String (List String)
    , currentModuleName : String
    , currentModuleExports : List String -- Collect exports as we traverse the current module
    , currentModuleExposesAll : Bool -- Track if current module uses exposing (..)
    , importIssues : List ImportIssue -- Import issues found in this module
    , moduleKey : Rule.ModuleKey -- Module key for error reporting
    }


initialProjectContext : List String -> ProjectContext
initialProjectContext exceptions =
    { exceptions = Set.fromList exceptions
    , internalExports = Dict.empty
    , externalExports = Dict.empty
    , importIssues = []
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\moduleKey projectContext ->
            { exceptions = projectContext.exceptions
            , internalExports = projectContext.internalExports
            , externalExports = projectContext.externalExports
            , currentModuleName = ""
            , currentModuleExports = []
            , currentModuleExposesAll = False
            , importIssues = []
            , moduleKey = moduleKey
            }
        )
        |> Rule.withModuleKey


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            let
                -- If we collected exports for a module that exposes all, add them to internal exports
                updatedInternalExports : Dict String (List String)
                updatedInternalExports =
                    if moduleContext.currentModuleExposesAll && not (List.isEmpty moduleContext.currentModuleExports) then
                        Dict.insert moduleContext.currentModuleName moduleContext.currentModuleExports moduleContext.internalExports

                    else
                        moduleContext.internalExports
            in
            { exceptions = moduleContext.exceptions
            , internalExports = updatedInternalExports
            , externalExports = moduleContext.externalExports
            , importIssues = moduleContext.importIssues
            }
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exceptions = newContext.exceptions
    , internalExports = Dict.union newContext.internalExports previousContext.internalExports
    , externalExports = Dict.union newContext.externalExports previousContext.externalExports
    , importIssues = newContext.importIssues ++ previousContext.importIssues
    }



-- VISITORS


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List (Error { useErrorForModule : () }), ProjectContext )
dependenciesVisitor dependencies context =
    let
        processedExports : Dict String (List String)
        processedExports =
            dependencies
                |> Dict.toList
                |> List.concatMap
                    (\( _, dependency ) ->
                        Dependency.modules dependency
                            |> List.map (\moduleDoc -> ( moduleDoc.name, extractExportsFromDocsModule moduleDoc ))
                    )
                |> Dict.fromList
    in
    ( [], { context | externalExports = processedExports } )


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withImportVisitor importVisitor


moduleDefinitionVisitor : Node Module.Module -> ModuleContext -> ( List (Error {}), ModuleContext )
moduleDefinitionVisitor node context =
    let
        moduleDefinition : Module.Module
        moduleDefinition =
            Node.value node

        moduleName : String
        moduleName =
            moduleDefinition
                |> Module.moduleName
                |> String.join "."

        ( moduleExports, exposesAll ) =
            case Module.exposingList moduleDefinition of
                Exposing.All _ ->
                    -- Module exposes everything - we'll collect exports from declarations
                    ( [], True )

                Exposing.Explicit explicitExposes ->
                    -- Module has explicit exports
                    ( explicitExposes
                        |> List.filterMap (Node.value >> extractExplicitExpose)
                    , False
                    )

        updatedInternalExports : Dict String (List String)
        updatedInternalExports =
            if List.isEmpty moduleExports || exposesAll then
                context.internalExports

            else
                Dict.insert moduleName moduleExports context.internalExports
    in
    ( []
    , { context
        | internalExports = updatedInternalExports
        , currentModuleName = moduleName
        , currentModuleExports = []
        , currentModuleExposesAll = exposesAll
      }
    )


declarationListVisitor : List (Node Declaration.Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationListVisitor declarations context =
    if not context.currentModuleExposesAll then
        -- Only collect exports if module uses exposing (..)
        ( [], context )

    else
        let
            exports : List String
            exports =
                declarations
                    |> List.filterMap (Node.value >> extractDeclarationName)
        in
        ( [], { context | currentModuleExports = exports } )


importVisitor : Node Import -> ModuleContext -> ( List (Error {}), ModuleContext )
importVisitor node context =
    let
        import_ : Import
        import_ =
            Node.value node

        moduleName : String
        moduleName =
            import_.moduleName
                |> Node.value
                |> String.join "."

        updatedContext : ModuleContext
        updatedContext =
            if Set.member moduleName context.exceptions then
                context

            else
                case import_.exposingList |> Maybe.map Node.value of
                    Just (Exposing.All range) ->
                        let
                            -- Calculate the range for the entire "(..)" including parentheses
                            fullRange : { start : { row : Int, column : Int }, end : { row : Int, column : Int } }
                            fullRange =
                                { start = { row = range.start.row, column = range.start.column - 1 }
                                , end = { row = range.end.row, column = range.end.column + 1 }
                                }

                            importIssue : { moduleName : String, importRange : { start : { row : Int, column : Int }, end : { row : Int, column : Int } }, moduleKey : Rule.ModuleKey }
                            importIssue =
                                { moduleName = moduleName
                                , importRange = fullRange
                                , moduleKey = context.moduleKey
                                }
                        in
                        { context | importIssues = importIssue :: context.importIssues }

                    _ ->
                        context
    in
    ( [], updatedContext )


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation context =
    context.importIssues
        |> List.map (generateErrorForImportIssue context)


generateErrorForImportIssue : ProjectContext -> ImportIssue -> Error { useErrorForModule : () }
generateErrorForImportIssue context issue =
    case getModuleExportsFromContext issue.moduleName context of
        Just exports ->
            Rule.errorForModuleWithFix
                issue.moduleKey
                { message = "Prefer listing what you wish to import and/or using qualified imports"
                , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                }
                issue.importRange
                [ Fix.replaceRangeBy issue.importRange ("(" ++ String.join ", " exports ++ ")") ]

        Nothing ->
            Rule.errorForModule
                issue.moduleKey
                { message = "Prefer listing what you wish to import and/or using qualified imports"
                , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                }
                issue.importRange



-- HELPERS


{-| Extract all exports from an Elm.Docs.Module.
-}
extractExportsFromDocsModule : Docs.Module -> List String
extractExportsFromDocsModule docsModule =
    let
        aliases : List String
        aliases =
            docsModule.aliases
                |> List.map .name

        unions : List String
        unions =
            docsModule.unions
                |> List.map
                    (\union ->
                        if List.isEmpty union.tags then
                            -- Just the type name if no constructors
                            union.name

                        else
                            -- Include all constructors with (..) syntax
                            union.name ++ "(..)"
                    )

        values : List String
        values =
            docsModule.values
                |> List.map .name

        binops : List String
        binops =
            docsModule.binops
                |> List.map (\binop -> "(" ++ binop.name ++ ")")
    in
    aliases
        ++ unions
        ++ values
        ++ binops


{-| Get the exports for a module from project context, checking both internal and external modules
-}
getModuleExportsFromContext : String -> ProjectContext -> Maybe (List String)
getModuleExportsFromContext moduleName context =
    -- First check internal modules
    case Dict.get moduleName context.internalExports of
        Just exports ->
            Just exports

        Nothing ->
            -- Then check external modules
            Dict.get moduleName context.externalExports


{-| Extract explicit export from an exposing list item
-}
extractExplicitExpose : Exposing.TopLevelExpose -> Maybe String
extractExplicitExpose expose =
    case expose of
        Exposing.InfixExpose operator ->
            Just ("(" ++ operator ++ ")")

        Exposing.FunctionExpose functionName ->
            Just functionName

        Exposing.TypeOrAliasExpose typeName ->
            Just typeName

        Exposing.TypeExpose typeExpose ->
            let
                typeName : String
                typeName =
                    typeExpose.name
            in
            case typeExpose.open of
                Just _ ->
                    -- Type with (..) - expose all constructors
                    Just (typeName ++ "(..)")

                Nothing ->
                    -- Just the type name
                    Just typeName


{-| Extract the name from a declaration if it's exportable
-}
extractDeclarationName : Declaration.Declaration -> Maybe String
extractDeclarationName declaration =
    case declaration of
        Declaration.FunctionDeclaration function ->
            function.declaration
                |> Node.value
                |> .name
                |> Node.value
                |> Just

        Declaration.AliasDeclaration alias ->
            alias.name
                |> Node.value
                |> Just

        Declaration.CustomTypeDeclaration customType ->
            -- For custom types, include the type name with (..) to expose constructors
            customType.name
                |> Node.value
                |> (\name -> name ++ "(..)")
                |> Just

        Declaration.PortDeclaration signature ->
            signature.name
                |> Node.value
                |> Just

        Declaration.InfixDeclaration infix ->
            infix.operator
                |> Node.value
                |> (\op -> "(" ++ op ++ ")")
                |> Just

        Declaration.Destructuring _ _ ->
            -- Destructuring patterns don't create exports
            Nothing
