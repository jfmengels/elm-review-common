module NoDeprecated exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookuTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ NoDeprecated.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoDeprecated
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoDeprecated" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> { lookupTable = lookupTable })
        |> Rule.withModuleNameLookupTable


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor (Node nodeRange node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            case ModuleNameLookuTable.moduleNameAt context.lookupTable nodeRange of
                Just moduleName ->
                    if predicate moduleName name then
                        ( [ Rule.error
                                { message = "Found new usage of deprecated element"
                                , details = [ "REPLACEME" ]
                                }
                                nodeRange
                          ]
                        , context
                        )

                    else
                        ( [], context )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


type DeprecatedLookup
    = NotDeprecated Context
    | Deprecated Context


checkIfModuleIsDeprecated : ModuleName -> Context -> DeprecatedLookup
checkIfModuleIsDeprecated moduleName context =
    if containsDeprecated (String.join "." moduleName) then
        Deprecated context

    else
        NotDeprecated context


predicate : ModuleName -> String -> Bool
predicate moduleName name =
    containsDeprecated name
        || containsDeprecated (String.join "." moduleName)


containsDeprecated : String -> Bool
containsDeprecated name =
    name
        |> String.toLower
        |> String.contains "deprecated"
