module NoEarlyLet exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ NoEarlyLet.rule
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
elm-review --template jfmengels/elm-review-common/example --rules NoEarlyLet
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoEarlyLet" initialContext
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { letDeclarations : List (List (Node String))
    , used : List String
    }


initialContext : Context
initialContext =
    { letDeclarations = [ [ Node { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } } "z" ] ]
    , used = []
    }


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            let
                errors : List (Rule.Error {})
                errors =
                    List.filterMap createError declarations
            in
            ( [], context )

        _ ->
            ( [], context )


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            let
                errors : List (Rule.Error {})
                errors =
                    context.letDeclarations
                        |> List.concatMap identity
                        |> List.map createError2
            in
            ( errors, context )

        _ ->
            ( [], context )


createError : Node Expression.LetDeclaration -> Maybe (Rule.Error {})
createError node =
    case Node.value node of
        Expression.LetFunction { declaration } ->
            Just
                (Rule.error
                    { message = "REPLACEME"
                    , details = [ "REPLACEME" ]
                    }
                    (declaration |> Node.value |> .name |> Node.range)
                )

        _ ->
            Nothing


createError2 : Node String -> Rule.Error {}
createError2 node =
    Rule.error
        { message = "REPLACEME"
        , details = [ "REPLACEME" ]
        }
        (Node.range node)
