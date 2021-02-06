module NoUnnecessaryTrailingUnderscore exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports... REPLACEME

    config =
        [ NoUnnecessaryTrailingUnderscore.rule
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
elm-review --template jfmengels/elm-review-common/example --rules NoUnnecessaryTrailingUnderscore
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnnecessaryTrailingUnderscore" ()
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                argNames : List ( Range, String )
                argNames =
                    function.declaration
                        |> Node.value
                        |> .arguments
                        |> List.filterMap
                            (\arg ->
                                case Node.value arg of
                                    Pattern.VarPattern name ->
                                        Just ( Node.range arg, name )

                                    _ ->
                                        Nothing
                            )

                argNamesInScope : Set String
                argNamesInScope =
                    argNames
                        |> List.map Tuple.second
                        |> Set.fromList
            in
            ( argNames
                |> List.filter (\( _, name ) -> String.endsWith "_" name && not (Set.member (String.dropRight 1 name) argNamesInScope))
                |> List.map
                    (\( range, name ) ->
                        Rule.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            }
                            range
                    )
            , context
            )

        _ ->
            ( [], context )


expressionVisitor node context =
    ( [], context )
