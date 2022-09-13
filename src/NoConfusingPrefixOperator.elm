module NoConfusingPrefixOperator exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports the usage of confusing prefix operators.

    config =
        [ NoConfusingPrefixOperator.rule
        ]

In Elm, operators like `<` can be used as `a < b`. They can also be used as `(<) a b` or `(<) a`.
While this is valid syntax, there are a number of cases where this will be confusing.

For instance, writing `((-) 1)` looks like it be subtracting one from a number, but that is not the case. What this
corresponds to is the following

    (-) 1

    -- is the same as
    \number -> (-) 1 number

    -- which is the same as
    \number -> 1 - number

So `((-) 1)` is not subtracting one by a number, it is subtracting a number from 1.

Some people have an easier time with this syntax, but most Elm developers will take longer to understand
the version using `(-)` compared to a more explicit version using an anonymous function. It has therefore
been recommended against by most experienced developers.

This rule reports this pattern for operators that are not commutative. Commutative operations are the ones where the
order of the operands do not matter (`1 + 2` is the same as `2 + 1` for instance), whereas they do matter for
non-commutative operations (like `-`, `<`, and most Elm operators).


## Fail

The following operators will be reported: `-`, `/`, `//`, `^`, `<`, `>`, `<=`, `>=`, `++`, `|>`, `<|`, `>>`, `<<`, `|.`, `|=`, `</>` and `<?>`.

    a =
        (<) 1 2


## Success

The following operators will not be reported: `+`, `*`, `==`, `/=`, `&&` and `||`.

    a =
        1 < 2

    b =
        (+) 1

    c =
        (==) 1 2


## When (not) to enable this rule

This rule is not useful when all of the members on your team read this syntax as easily as the version with anonymous functions.

That said, I would still recommend this for when you hire new developers who might not be.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoConfusingPrefixOperator
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoConfusingPrefixOperator" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor node =
    case Node.value node of
        Expression.Application (fn :: _) ->
            case Node.value fn of
                Expression.PrefixOperator operator ->
                    if Set.member operator nonCommutativeOperators then
                        [ Rule.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            }
                            (Node.range fn)
                        ]

                    else
                        []

                _ ->
                    []

        _ ->
            []


nonCommutativeOperators : Set String
nonCommutativeOperators =
    Set.fromList
        [ "-"
        , "/"
        , "//"
        , "^"
        , "<"
        , ">"
        , "<="
        , ">="
        , "++"
        , "|>"
        , "<|"
        , ">>"
        , "<<"
        , "|."
        , "|="
        , "</>"
        , "<?>"
        ]
