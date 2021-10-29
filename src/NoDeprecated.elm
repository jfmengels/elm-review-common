module NoDeprecated exposing
    ( rule
    , checkInName
    )

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookuTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


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
rule : Configuration -> Rule
rule configuration =
    Rule.newModuleRuleSchemaUsingContextCreator "NoDeprecated" initialContext
        -- TODO Use a set of deprecated module names and store them in the ProjectContext
        |> Rule.withExpressionEnterVisitor (expressionVisitor configuration)
        |> Rule.fromModuleRuleSchema


type Configuration
    = Configuration
        { moduleNamePredicate : ModuleName -> Bool
        , elementPredicate : ModuleName -> String -> Bool
        }


checkInName : Configuration
checkInName =
    Configuration
        { moduleNamePredicate = String.join "." >> String.toLower >> containsDeprecated
        , elementPredicate = \_ name -> containsDeprecated name
        }


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , deprecatedModules : Set ModuleName
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , deprecatedModules = Set.singleton [ "Some", "DeprecatedModule" ]
            }
        )
        |> Rule.withModuleNameLookupTable


expressionVisitor : Configuration -> Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor (Configuration configuration) (Node nodeRange node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            case ModuleNameLookuTable.moduleNameAt context.lookupTable nodeRange of
                Just moduleName ->
                    if configuration.elementPredicate moduleName name || configuration.moduleNamePredicate moduleName then
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


containsDeprecated : String -> Bool
containsDeprecated name =
    name
        |> String.toLower
        |> String.contains "deprecated"
