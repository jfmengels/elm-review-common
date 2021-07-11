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
    { branch : Branch
    , currentBranching : List Range
    }


initialContext : Context
initialContext =
    { branch = newBranch
    , currentBranching = []
    }


newBranch : Branch
newBranch =
    Branch
        { letDeclarations = []
        , used = []
        , branches = RangeDict.empty
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


getCurrentBranch : List Range -> Branch -> Maybe Branch
getCurrentBranch currentBranching (Branch branch) =
    case currentBranching of
        [] ->
            Just (Branch branch)

        range :: restOfBranching ->
            RangeDict.get range branch.branches
                |> Maybe.andThen (getCurrentBranch restOfBranching)


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    let
        newContext : Context
        newContext =
            case getCurrentBranch context.currentBranching context.branch of
                Just (Branch branch) ->
                    if RangeDict.member (Node.range node) branch.branches then
                        { context | currentBranching = context.currentBranching ++ [ Node.range node ] }

                    else
                        context

                Nothing ->
                    context
    in
    ( [], expressionEnterVisitorHelp node newContext )


expressionEnterVisitorHelp : Node Expression -> Context -> Context
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            let
                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b -> { b | used = name :: b.used })
                        context.currentBranching
                        context.branch
            in
            { context | branch = branch }

        Expression.LetExpression { declarations } ->
            let
                letDeclarations : List (Node String)
                letDeclarations =
                    List.concatMap collectDeclarations declarations

                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b -> { b | letDeclarations = letDeclarations ++ b.letDeclarations })
                        context.currentBranching
                        context.branch
            in
            { context | branch = branch }

        Expression.IfBlock _ then_ else_ ->
            let
                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b ->
                            { b
                                | branches =
                                    b.branches
                                        |> RangeDict.insert (Node.range then_) newBranch
                                        |> RangeDict.insert (Node.range else_) newBranch
                            }
                        )
                        context.currentBranching
                        context.branch
            in
            { context | branch = branch }

        _ ->
            context


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    ( expressionExitVisitorHelp node context
    , popCurrentNodeFromBranching (Node.range node) context
    )


popCurrentNodeFromBranching : Range -> Context -> Context
popCurrentNodeFromBranching range context =
    if getLastListItem context.currentBranching == Just range then
        { context | currentBranching = List.filter (\n -> n /= range) context.currentBranching }

    else
        context


expressionExitVisitorHelp : Node Expression -> Context -> List (Rule.Error {})
expressionExitVisitorHelp node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            case getCurrentBranch context.currentBranching context.branch of
                Just (Branch branch) ->
                    branch.letDeclarations
                        |> List.map createError

                Nothing ->
                    []

        _ ->
            []


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


getLastListItem : List a -> Maybe a
getLastListItem list =
    case list of
        [] ->
            Nothing

        [ a ] ->
            Just a

        _ :: _ :: rest ->
            getLastListItem rest
