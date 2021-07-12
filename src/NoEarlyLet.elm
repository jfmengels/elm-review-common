module NoEarlyLet exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
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
    Rule.newModuleRuleSchemaUsingContextCreator "NoEarlyLet" initialContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { extractSourceCode : Range -> String
    , branch : Branch
    , branching : Branching
    }


type Branch
    = Branch BranchData


type alias Branching =
    { full : List Range
    , last : Maybe Range
    }


newBranch : LetInsertPosition -> Branch
newBranch insertionLocation =
    Branch
        { letDeclarations = []
        , used = []
        , insertionLocation = insertionLocation
        , branches = RangeDict.empty
        }


emptyBranching : Branching
emptyBranching =
    { full = []
    , last = Nothing
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\extractSourceCode () ->
            { extractSourceCode = extractSourceCode
            , branch = newBranch (InsertNewLet { row = 0, column = 0 })
            , branching = emptyBranching
            }
        )
        |> Rule.withSourceCodeExtractor


type alias BranchData =
    { letDeclarations : List Declared
    , used : List String
    , insertionLocation : LetInsertPosition
    , branches : RangeDict Branch
    }


type LetInsertPosition
    = InsertNewLet Location
    | InsertExistingLet Location


type alias Declared =
    { name : String
    , reportRange : Range
    , declarationRange : Range
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


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            ( []
            , { extractSourceCode = context.extractSourceCode
              , branch = newBranch (figureOutInsertionLocation (declaration |> Node.value |> .expression))
              , branching = emptyBranching
              }
            )

        _ ->
            ( [], context )


figureOutInsertionLocation : Node Expression -> LetInsertPosition
figureOutInsertionLocation node =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            case declarations of
                first :: _ ->
                    InsertExistingLet (Node.range first).start

                [] ->
                    -- Should not happen
                    InsertNewLet (Node.range node).start

        _ ->
            InsertNewLet (Node.range node).start


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    let
        newContext : Context
        newContext =
            case getCurrentBranch context.branching.full context.branch of
                Just (Branch branch) ->
                    if RangeDict.member (Node.range node) branch.branches then
                        { context | branching = addBranching (Node.range node) context.branching }

                    else
                        context

                Nothing ->
                    context
    in
    ( [], expressionEnterVisitorHelp node newContext )


addBranching : Range -> Branching -> Branching
addBranching range branching =
    { full = branching.full ++ [ range ]
    , last = Just range
    }


removeLastBranchIfOnIt : Range -> Branching -> Maybe Branching
removeLastBranchIfOnIt range branching =
    if branching.last == Just range then
        let
            full : List Range
            full =
                List.take (List.length branching.full - 1) branching.full
        in
        Just
            { branching
                | full = full
                , last = getLastListItem full
            }

    else
        Nothing


popCurrentNodeFromBranching : Range -> Context -> Context
popCurrentNodeFromBranching range context =
    case removeLastBranchIfOnIt range context.branching of
        Just newBranching ->
            { context | branching = newBranching }

        Nothing ->
            context


expressionEnterVisitorHelp : Node Expression -> Context -> Context
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            let
                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b -> { b | used = name :: b.used })
                        context.branching.full
                        context.branch
            in
            { context | branch = branch }

        Expression.RecordUpdateExpression name _ ->
            let
                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b -> { b | used = Node.value name :: b.used })
                        context.branching.full
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
                            (\( nameNode, expressionRange, declaration ) ->
                                { name = Node.value nameNode
                                , reportRange = Node.range nameNode
                                , declarationRange = fullLines { start = (Node.range declaration).start, end = expressionRange.end }
                                , removeRange =
                                    if isDeclarationAlone then
                                        { start = (Node.range node).start
                                        , end = (Node.range expression).start
                                        }

                                    else
                                        { start = { row = (Node.range declaration).start.row, column = 1 }
                                        , end = expressionRange.end
                                        }
                                }
                            )

                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b -> { b | letDeclarations = letDeclarations ++ b.letDeclarations })
                        context.branching.full
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


fullLines : Range -> Range
fullLines range =
    { start = { row = range.start.row, column = 1 }
    , end = range.end
    }


addBranches : List (Node Expression) -> Context -> Context
addBranches nodes context =
    let
        branch : Branch
        branch =
            updateCurrentBranch
                (\b -> { b | branches = insertNewBranches nodes b.branches })
                context.branching.full
                context.branch
    in
    { context | branch = branch }


insertNewBranches : List (Node Expression) -> RangeDict Branch -> RangeDict Branch
insertNewBranches nodes rangeDict =
    case nodes of
        [] ->
            rangeDict

        node :: tail ->
            insertNewBranches tail
                (RangeDict.insert
                    (Node.range node)
                    (newBranch (figureOutInsertionLocation node))
                    rangeDict
                )


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    ( expressionExitVisitorHelp node context
    , popCurrentNodeFromBranching (Node.range node) context
    )


expressionExitVisitorHelp : Node Expression -> Context -> List (Rule.Error {})
expressionExitVisitorHelp node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            case getCurrentBranch context.branching.full context.branch of
                Just (Branch branch) ->
                    List.filterMap
                        (\declaration ->
                            if List.member declaration.name branch.used then
                                Nothing

                            else
                                case canBeMovedToCloserLocation branch declaration.name of
                                    [ location ] ->
                                        Just (createError context declaration location)

                                    _ ->
                                        Nothing
                        )
                        branch.letDeclarations

                Nothing ->
                    []

        _ ->
            []


collectDeclarations : Node Expression.LetDeclaration -> List ( Node String, Range, Node Expression.LetDeclaration )
collectDeclarations node =
    case Node.value node of
        Expression.LetFunction letFunction ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value letFunction.declaration
            in
            -- TODO Add support for let functions? but need to check name clashes...
            if List.isEmpty declaration.arguments then
                [ ( declaration.name
                  , Node.range declaration.expression
                  , node
                  )
                ]

            else
                []

        Expression.LetDestructuring _ _ ->
            -- TODO
            []


canBeMovedToCloserLocation : BranchData -> String -> List LetInsertPosition
canBeMovedToCloserLocation branch name =
    let
        relevantUsages : List LetInsertPosition
        relevantUsages =
            branch.branches
                |> RangeDict.values
                |> List.concatMap
                    (\(Branch b) ->
                        if List.member name b.used then
                            [ b.insertionLocation ]

                        else if RangeDict.isEmpty b.branches then
                            []

                        else
                            canBeMovedToCloserLocation b name
                    )
    in
    case relevantUsages of
        [ location ] ->
            [ location ]

        _ ->
            -- TODO Handle multiple cases
            []


type NameUse
    = DirectUse
    | NoUse


createError : Context -> Declared -> LetInsertPosition -> Rule.Error {}
createError context declared letInsertPosition =
    Rule.errorWithFix
        { message = "REPLACEME"
        , details = [ "REPLACEME" ]
        }
        declared.reportRange
        (fix context declared letInsertPosition)


fix : Context -> Declared -> LetInsertPosition -> List Fix
fix context declared letInsertPosition =
    case letInsertPosition of
        InsertNewLet insertLocation ->
            [ Fix.removeRange declared.removeRange
            , context.extractSourceCode declared.declarationRange
                |> wrapInLet declared.reportRange.start.column insertLocation.column
                |> Fix.insertAt insertLocation
            ]

        InsertExistingLet insertLocation ->
            [ Fix.removeRange declared.removeRange
            , context.extractSourceCode declared.declarationRange
                |> insertInLet insertLocation.column
                |> Fix.insertAt insertLocation
            ]


wrapInLet : Int -> Int -> String -> String
wrapInLet initialPosition column source =
    let
        padding : String
        padding =
            String.repeat (column - 1) " "
    in
    [ [ "let" ]
    , source
        |> String.lines
        |> List.map (\line -> String.repeat (column - initialPosition) " " ++ "    " ++ line)
    , [ padding ++ "in", padding ]
    ]
        |> List.concat
        |> String.join "\n"


insertInLet : Int -> String -> String
insertInLet column source =
    (source
        |> String.trim
        |> String.lines
        |> List.map (\line -> String.repeat 1 " " ++ line)
        |> String.join "\n"
    )
        ++ "\n"
        ++ String.repeat column " "


countBlanks : Int -> List Char -> Int
countBlanks count chars =
    case chars of
        [] ->
            count

        ' ' :: rest ->
            countBlanks (count + 1) rest

        _ ->
            count


getLastListItem : List a -> Maybe a
getLastListItem list =
    case list of
        [] ->
            Nothing

        [ a ] ->
            Just a

        _ :: _ :: rest ->
            getLastListItem rest
