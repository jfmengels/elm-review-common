module NoSimpleLetBody exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule)


{-| Reports when a let expression's body is a simple reference to a value declared in the let expression.

    config =
        [ NoSimpleLetBody.rule
        ]

The reasoning behind this rule is that it is not necessary to assign a name (and type annotation) to the result of a let expression,
since they are redundant with the value or function containing the expression.

If it feels necessary to give a name anyway because it helps clarify the context, then it might be a sign that the computation of that value should be extracted to a function.


## Fail

    a =
        let
            b =
                1

            c =
                b + 1
        in
        c


## Success

Anything that is not simply a reference to a value declared in the let expression is okay.

    a =
        let
            b =
                1
        in
        b + 1

The rule will not report when the referenced value was destructured in the let expression.

    first tuple =
        let
            ( value, _ ) =
                tuple
        in
        value


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoSimpleLetBody
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoSimpleLetBody" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor node =
    case Node.value node of
        Expression.LetExpression { declarations, expression } ->
            case Node.value expression of
                Expression.FunctionOrValue [] name ->
                    let
                        declared : List String
                        declared =
                            List.filterMap
                                (\declaration ->
                                    case Node.value declaration of
                                        Expression.LetFunction function ->
                                            function.declaration
                                                |> Node.value
                                                |> .name
                                                |> Node.value
                                                |> Just

                                        Expression.LetDestructuring _ _ ->
                                            Nothing
                                )
                                declarations
                    in
                    if List.member name declared then
                        [ Rule.error
                            { message = "The referenced value should be inlined."
                            , details =
                                [ "The name of the value is redundant with the surrounding expression."
                                , "If you feel like the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                                ]
                            }
                            (Node.range expression)
                        ]

                    else
                        []

                _ ->
                    []

        _ ->
            []
