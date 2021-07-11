module NoEarlyLet exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location, Range)
import RangeDict exposing (RangeDict)
import Review.Fix as Fix exposing (Fix)
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
    { letDeclarations : List Declared
    , used : List String
    , branches : RangeDict Branch
    }


type alias Declared =
    { name : String
    , reportRange : Range
    , removeRange : Range
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

        Expression.RecordUpdateExpression name _ ->
            let
                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b -> { b | used = Node.value name :: b.used })
                        context.currentBranching
                        context.branch
            in
            { context | branch = branch }

        Expression.LetExpression { declarations, expression } ->
            let
                isDeclarationAlone : Bool
                isDeclarationAlone =
                    List.length declarations == 1

                letDeclarations : List Declared
                letDeclarations =
                    declarations
                        |> List.concatMap collectDeclarations
                        |> List.map
                            (\nameNode ->
                                { name = Node.value nameNode
                                , reportRange = Node.range nameNode
                                , removeRange =
                                    if isDeclarationAlone then
                                        { start = (Node.range node).start
                                        , end = (Node.range expression).end
                                        }

                                    else
                                        -- TODO
                                        Range.emptyRange
                                }
                            )

                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b -> { b | letDeclarations = letDeclarations ++ b.letDeclarations })
                        context.currentBranching
                        context.branch
            in
            { context | branch = branch }

        Expression.IfBlock _ then_ else_ ->
            addBranches [ then_, else_ ] context

        Expression.CaseExpression { cases } ->
            let
                branchNodes : List (Node Expression)
                branchNodes =
                    List.map (\( _, exprNode ) -> exprNode) cases
            in
            addBranches branchNodes context

        _ ->
            context


addBranches : List (Node a) -> Context -> Context
addBranches nodes context =
    let
        branch : Branch
        branch =
            updateCurrentBranch
                (\b -> { b | branches = insertNewBranches nodes b.branches })
                context.currentBranching
                context.branch
    in
    { context | branch = branch }


insertNewBranches : List (Node a) -> RangeDict Branch -> RangeDict Branch
insertNewBranches nodes rangeDict =
    case nodes of
        [] ->
            rangeDict

        node :: tail ->
            insertNewBranches tail (RangeDict.insert (Node.range node) newBranch rangeDict)


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    ( expressionExitVisitorHelp node context
    , popCurrentNodeFromBranching (Node.range node) context
    )


popCurrentNodeFromBranching : Range -> Context -> Context
popCurrentNodeFromBranching range context =
    -- TODO improve. Make currentbranching an array?
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
                    List.filterMap
                        (\declaration ->
                            if List.member declaration.name branch.used then
                                Nothing

                            else
                                canBeMovedToCloserLocation branch declaration.name
                                    |> Maybe.map InsertNewLet
                                    |> Maybe.map (createError declaration)
                        )
                        branch.letDeclarations

                Nothing ->
                    []

        _ ->
            []


collectDeclarations : Node Expression.LetDeclaration -> List (Node String)
collectDeclarations node =
    case Node.value node of
        Expression.LetFunction { declaration } ->
            -- TODO Add support for let functions? but need to check name clashes...
            if List.isEmpty (Node.value declaration).arguments then
                [ (Node.value declaration).name ]

            else
                []

        Expression.LetDestructuring _ _ ->
            -- TODO
            []


canBeMovedToCloserLocation : BranchData -> String -> Maybe Location
canBeMovedToCloserLocation branch name =
    let
        relevantUsages : List Range
        relevantUsages =
            branch.branches
                |> RangeDict.toList
                |> List.filterMap
                    (\( range, Branch b ) ->
                        case isUsingName name b of
                            DirectUse ->
                                Just range

                            IndirectUse ->
                                Just range

                            NoUse ->
                                Nothing
                    )
    in
    case relevantUsages of
        [ range ] ->
            Just range.start

        _ ->
            Nothing


type NameUse
    = DirectUse
    | IndirectUse
    | NoUse


isUsingName : String -> BranchData -> NameUse
isUsingName name branch =
    if List.member name branch.used then
        DirectUse

    else
        NoUse


type LetInsertPosition
    = InsertNewLet Location


createError : Declared -> LetInsertPosition -> Rule.Error {}
createError declared (InsertNewLet insertLocation) =
    Rule.errorWithFix
        { message = "REPLACEME"
        , details = [ "REPLACEME" ]
        }
        declared.reportRange
        [ Fix.insertAt insertLocation
            """let
      z = 1
    in
    """
        ]


getLastListItem : List a -> Maybe a
getLastListItem list =
    case list of
        [] ->
            Nothing

        [ a ] ->
            Just a

        _ :: _ :: rest ->
            getLastListItem rest
