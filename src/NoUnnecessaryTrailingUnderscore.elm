module NoUnnecessaryTrailingUnderscore exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern as Pattern
import Review.Rule as Rule exposing (Rule)


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
            ( List.filterMap
                (\arg ->
                    case Node.value arg of
                        Pattern.VarPattern name ->
                            if String.endsWith "_" name then
                                Just
                                    (Rule.error
                                        { message = "REPLACEME"
                                        , details = [ "REPLACEME" ]
                                        }
                                        (Node.range arg)
                                    )

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                (function.declaration |> Node.value |> .arguments)
            , context
            )

        _ ->
            ( [], context )


expressionVisitor node context =
    ( [], context )
