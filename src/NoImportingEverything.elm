module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbids importing everything from a module.

When you import everything from a module, it becomes harder to know where a function
or a type comes from. The official guide even
[recommends against importing everything](https://guide.elm-lang.org/webapps/modules.html#using-modules).

    config =
        [ NoImportingEverything.rule []
        ]

Teams often have an agreement on the list of imports from which it is okay to expose everything, so
you can configure a list of exceptions.

    config =
        [ NoImportingEverything.rule [ "Html", "Some.Module" ]
        ]


## Fail

    import A exposing (..)
    import A as B exposing (..)


## Success

    import A as B exposing (B(..), C, d)

    -- If configured with `[ "Html" ]`
    import Html exposing (..)


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoImportingEverything
```

-}
rule : List String -> Rule
rule exceptions =
    Rule.newProjectRuleSchema "NoImportingEverything" initialContext
        |> Rule.withModuleVisitor (moduleVisitor exceptions)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { constructorToType : Dict ModuleName (Dict String String)
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , importsExposingAll : Dict ModuleName ImportExposingAll
    , exposedTypes : ExposedTypes
    , constructorToType : Dict ModuleName (Dict String String)
    , localConstructorToType : Dict String String
    }


type ExposedTypes
    = ExposesAll
    | ExposesConstructorsOf (Set String)


type alias ImportExposingAll =
    { node : Node Import
    , exposingRange : Range
    , values : Set String
    }


initialContext : ProjectContext
initialContext =
    { constructorToType = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            , importsExposingAll = Dict.empty
            , exposedTypes = ExposesAll
            , constructorToType = projectContext.constructorToType
            , localConstructorToType = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName moduleContext ->
            { constructorToType =
                if Dict.isEmpty moduleContext.localConstructorToType then
                    Dict.empty

                else
                    Dict.singleton moduleName moduleContext.localConstructorToType
            }
        )
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { constructorToType = Dict.union newContext.constructorToType previousContext.constructorToType
    }


moduleVisitor :
    List String
    -> Rule.ModuleRuleSchema schemaState ModuleContext
    -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor exceptions schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List empty, ModuleContext )
moduleDefinitionVisitor node context =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            ( [], { context | exposedTypes = ExposesAll } )

        Exposing.Explicit list ->
            let
                constructors : Set String
                constructors =
                    List.foldl
                        (\(Node _ item) acc ->
                            case item of
                                Exposing.TypeExpose { name } ->
                                    Set.insert name acc

                                _ ->
                                    acc
                        )
                        Set.empty
                        list
            in
            ( [], { context | exposedTypes = ExposesConstructorsOf constructors } )


exceptionsToSet : List String -> Set ModuleName
exceptionsToSet exceptions =
    List.foldl
        (\moduleName acc -> Set.insert (String.split "." moduleName) acc)
        Set.empty
        exceptions


importVisitor : Set ModuleName -> Node Import -> ModuleContext -> ( List (Error nothing), ModuleContext )
importVisitor exceptions node context =
    let
        moduleName : ModuleName
        moduleName =
            importModuleName node
    in
    if Set.member moduleName exceptions then
        ( [], context )

    else
        case
            Node.value node
                |> .exposingList
                |> Maybe.map Node.value
        of
            Just (Exposing.All allRange) ->
                ( []
                , { context
                    | importsExposingAll =
                        Dict.insert moduleName
                            { node = node
                            , exposingRange = allRange
                            , values = Set.empty
                            }
                            context.importsExposingAll
                  }
                )

            _ ->
                ( [], context )


declarationVisitor : Node Declaration -> ModuleContext -> ( List empty, ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration type_ ->
            let
                typeName : String
                typeName =
                    Node.value type_.name
            in
            if isConstructorsExposed typeName context then
                let
                    localConstructorToType : Dict String String
                    localConstructorToType =
                        List.foldl
                            (\(Node _ constructor) acc ->
                                Dict.insert (Node.value constructor.name) typeName acc
                            )
                            context.localConstructorToType
                            type_.constructors
                in
                ( [], { context | localConstructorToType = localConstructorToType } )

            else
                ( [], context )

        _ ->
            ( [], context )


isConstructorsExposed : String -> { context | exposedTypes : ExposedTypes } -> Bool
isConstructorsExposed name context =
    case context.exposedTypes of
        ExposesAll ->
            True

        ExposesConstructorsOf set ->
            Set.member name set


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error nothing), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                Just moduleName ->
                    ( []
                    , useImportedValue context moduleName name
                    )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation context =
    context.importsExposingAll
        |> Dict.values
        |> List.map importError


importError : ImportExposingAll -> Error {}
importError ({ exposingRange } as importExposingAll) =
    Rule.errorWithFix
        { message = "Prefer listing what you wish to import and/or using qualified imports"
        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
        }
        { start = { row = exposingRange.start.row, column = exposingRange.start.column - 1 }
        , end = { row = exposingRange.end.row, column = exposingRange.end.column + 1 }
        }
        [ exposingFix importExposingAll ]


useImportedValue : ModuleContext -> ModuleName -> String -> ModuleContext
useImportedValue context moduleName name =
    case Dict.get moduleName context.importsExposingAll of
        Nothing ->
            context

        Just importExposingAll ->
            { context
                | importsExposingAll =
                    Dict.insert
                        moduleName
                        { importExposingAll | values = Set.insert name importExposingAll.values }
                        context.importsExposingAll
            }


importModuleName : Node Import -> ModuleName
importModuleName node =
    node
        |> Node.value
        |> .moduleName
        |> Node.value


exposingFix : ImportExposingAll -> Fix
exposingFix { node, exposingRange, values } =
    case Set.toList values of
        [] ->
            removeExposingFix node

        list ->
            replaceExposingFix list exposingRange


removeExposingFix : Node Import -> Fix
removeExposingFix node =
    case node |> Node.value |> .moduleAlias of
        Just aliasNode ->
            let
                endOfAliasName : Range.Location
                endOfAliasName =
                    aliasNode |> Node.range |> .end

                endOfImport : Range.Location
                endOfImport =
                    Node.range node |> .end
            in
            Fix.replaceRangeBy { start = endOfAliasName, end = endOfImport } ""

        Nothing ->
            let
                endOfModuleName : Range.Location
                endOfModuleName =
                    node |> Node.value |> .moduleName |> Node.range |> .end

                endOfImport : Range.Location
                endOfImport =
                    Node.range node |> .end
            in
            Fix.replaceRangeBy { start = endOfModuleName, end = endOfImport } ""


replaceExposingFix : List String -> Range -> Fix
replaceExposingFix values range =
    Fix.replaceRangeBy range (String.join ", " values)
