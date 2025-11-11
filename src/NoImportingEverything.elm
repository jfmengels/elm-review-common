module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
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
    Rule.newModuleRuleSchemaUsingContextCreator "NoImportingEverything" initialModuleContext
        |> Rule.withImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , importsExposingAll : Dict ModuleName ImportExposingAll
    }


type alias ImportExposingAll =
    { node : Node Import
    , exposingRange : Range
    , values : Set String
    }


initialModuleContext : Rule.ContextCreator () ModuleContext
initialModuleContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , importsExposingAll = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


exceptionsToSet : List String -> Set (List String)
exceptionsToSet exceptions =
    List.foldl
        (\moduleName acc -> Set.insert (String.split "." moduleName) acc)
        Set.empty
        exceptions


importVisitor : Set (List String) -> Node Import -> ModuleContext -> ( List (Error nothing), ModuleContext )
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


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error nothing), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                Just moduleName ->
                    ( []
                    , useImportedFunction context moduleName name
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


useImportedFunction : ModuleContext -> ModuleName -> String -> ModuleContext
useImportedFunction context moduleName name =
    { context
        | importsExposingAll = Dict.update moduleName (updateImportsUsed name) context.importsExposingAll
    }


updateImportsUsed : String -> Maybe ImportExposingAll -> Maybe ImportExposingAll
updateImportsUsed name maybeImport =
    Maybe.map (insertValueUsed name) maybeImport


insertValueUsed : String -> ImportExposingAll -> ImportExposingAll
insertValueUsed name importExposingAll =
    { importExposingAll | values = Set.insert name importExposingAll.values }


importModuleName : Node Import -> List String
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
