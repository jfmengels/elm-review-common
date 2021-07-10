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
                letDeclarations : List (Node String)
                letDeclarations =
                    List.concatMap collectDeclarations declarations
            in
            ( [], { context | letDeclarations = letDeclarations :: context.letDeclarations } )

        _ ->
            ( [], context )


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            case context.letDeclarations of
                head :: tail ->
                    let
                        errors : List (Rule.Error {})
                        errors =
                            List.map createError head
                    in
                    ( errors, { context | letDeclarations = tail } )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )


collectDeclarations : Node Expression.LetDeclaration -> List (Node String)
collectDeclarations node =
    case Node.value node of
        Expression.LetFunction { declaration } ->
            [ (Node.value declaration).name ]

        Expression.LetDestructuring _ _ ->
            -- TODO
            []


createError : Node String -> Rule.Error {}
createError node =
    Rule.error
        { message = "REPLACEME"
        , details = [ "REPLACEME" ]
        }
        (Node.range node)
