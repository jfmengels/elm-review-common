module NoEarlyLet exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import RangeDict exposing (RangeDict)
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
    , branch : Branch
    , currentBranching : List Range
    }


initialContext : Context
initialContext =
    { letDeclarations = []
    , branch =
        Branch
            { letDeclarations = []
            , used = []
            , branches = RangeDict.empty
            }
    , currentBranching = []
    }


type Branch
    = Branch BranchData


type alias BranchData =
    { letDeclarations : List (Node String)
    , used : List String
    , branches : RangeDict Branch
    }


updateCurrentBranch : (BranchData -> BranchData) -> List Range -> Branch -> Branch
updateCurrentBranch updateFn currentBranching (Branch branch) =
    case currentBranching of
        [] ->
            Branch (updateFn branch)

        range :: restOfBranching ->
            Branch
                { branch
                    | branches =
                        RangeDict.modify
                            range
                            (updateCurrentBranch updateFn restOfBranching)
                            branch.branches
                }


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            --( [], { context | used = name :: context.used } )
            ( [], context )

        Expression.LetExpression { declarations } ->
            let
                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b -> { b | letDeclarations = letDeclarations ++ b.letDeclarations })
                        context.currentBranching
                        context.branch

                letDeclarations : List (Node String)
                letDeclarations =
                    List.concatMap collectDeclarations declarations
            in
            ( [], { context | branch = branch } )

        _ ->
            ( [], context )


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            -- TODO Report for current branch from branch data
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
