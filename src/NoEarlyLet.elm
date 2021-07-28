module NoEarlyLet exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
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


type alias Branching =
    { full : List Range
    , last : Maybe Range
    }


type Branch
    = Branch BranchData
    | LetScope BranchData


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
    , introducesVariablesInImplementation : Bool
    , reportRange : Range
    , declarationRange : Range
    , removeRange : Range
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


updateCurrentBranch : (BranchData -> BranchData) -> List Range -> Branch -> Branch
updateCurrentBranch updateFn currentBranching segment =
    case currentBranching of
        [] ->
            updateBranch updateFn segment

        range :: restOfSegments ->
            updateBranch
                (\branch ->
                    { branch
                        | branches =
                            RangeDict.modify
                                range
                                (updateCurrentBranch updateFn restOfSegments)
                                branch.branches
                    }
                )
                segment


updateAllSegmentsOfCurrentBranch : (BranchData -> BranchData) -> List Range -> Branch -> Branch
updateAllSegmentsOfCurrentBranch updateFn currentBranching segment =
    case currentBranching of
        [] ->
            updateBranch updateFn segment

        range :: restOfSegments ->
            updateBranch
                (\branch ->
                    updateFn
                        { branch
                            | branches =
                                RangeDict.modify
                                    range
                                    (updateCurrentBranch updateFn restOfSegments)
                                    branch.branches
                        }
                )
                segment


updateBranch : (BranchData -> BranchData) -> Branch -> Branch
updateBranch updateFn segment =
    case segment of
        Branch branch ->
            Branch (updateFn branch)

        LetScope branch ->
            LetScope (updateFn branch)


getCurrentBranch : List Range -> Branch -> Maybe Branch
getCurrentBranch currentBranching branch =
    case currentBranching of
        [] ->
            Just branch

        range :: restOfBranching ->
            RangeDict.get range (getBranches branch)
                |> Maybe.andThen (getCurrentBranch restOfBranching)


getBranches : Branch -> RangeDict Branch
getBranches =
    getBranchData >> .branches


getBranchData : Branch -> BranchData
getBranchData branch =
    case branch of
        Branch b ->
            b

        LetScope b ->
            b


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
            case getCurrentBranch context.branching.full context.branch |> Maybe.map getBranches of
                Just branches ->
                    if RangeDict.member (Node.range node) branches then
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
            (removeLastBranchIfOnItRetry
                range
                { full = full
                , last = getLastListItem full
                }
            )

    else
        Nothing


removeLastBranchIfOnItRetry : Range -> Branching -> Branching
removeLastBranchIfOnItRetry range branching =
    if branching.last == Just range then
        let
            full : List Range
            full =
                List.take (List.length branching.full - 1) branching.full
        in
        { full = full
        , last = getLastListItem full
        }

    else
        branching


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
                                , introducesVariablesInImplementation = False
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

                newScope : Branch
                newScope =
                    LetScope
                        { letDeclarations = letDeclarations
                        , used = []
                        , insertionLocation = figureOutInsertionLocation node
                        , branches = RangeDict.empty
                        }

                branch : Branch
                branch =
                    updateCurrentBranch
                        (\b ->
                            { b
                                | branches =
                                    RangeDict.insert
                                        (Node.range node)
                                        newScope
                                        b.branches
                            }
                        )
                        context.branching.full
                        context.branch
            in
            { context
                | branch = branch
                , branching = addBranching (Node.range node) context.branching
            }

        Expression.IfBlock _ then_ else_ ->
            addBranches [ then_, else_ ] context

        Expression.CaseExpression { cases } ->
            let
                branchNodes : List (Node Expression)
                branchNodes =
                    List.map (\( _, exprNode ) -> exprNode) cases
            in
            addBranches branchNodes context

        Expression.LambdaExpression { args } ->
            let
                introducesVariablesInImplementation : Bool
                introducesVariablesInImplementation =
                    -- TODO Mock
                    True
            in
            if introducesVariablesInImplementation then
                { context | branch = markLetDeclarationsAsIntroducingVariables (Node.range node) context }

            else
                context

        _ ->
            context


markLetDeclarationsAsIntroducingVariables : Range -> Context -> Branch
markLetDeclarationsAsIntroducingVariables range context =
    updateAllSegmentsOfCurrentBranch
        (markDeclarationsAsUsed range)
        context.branching.full
        context.branch


markDeclarationsAsUsed : Range -> BranchData -> BranchData
markDeclarationsAsUsed range branchData =
    { branchData | letDeclarations = List.map (markDeclarationAsUsed range) branchData.letDeclarations }


markDeclarationAsUsed : Range -> Declared -> Declared
markDeclarationAsUsed range declared =
    if isRangeContained { outer = declared.declarationRange, inner = range } then
        { declared | introducesVariablesInImplementation = True }

    else
        declared


isRangeContained : { outer : Range, inner : Range } -> Bool
isRangeContained { outer, inner } =
    (Range.compareLocations outer.start inner.start /= GT)
        && (Range.compareLocations outer.end inner.end /= LT)


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
                Just (LetScope letScope) ->
                    List.filterMap
                        (\declaration ->
                            canBeMovedToCloserLocation True declaration.name (LetScope letScope)
                                |> List.head
                                |> Maybe.map (createError context declaration)
                        )
                        letScope.letDeclarations

                _ ->
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


canBeMovedToCloserLocation : Bool -> String -> Branch -> List LetInsertPosition
canBeMovedToCloserLocation isRoot name segment =
    let
        branch : BranchData
        branch =
            getBranchData segment
    in
    if List.member name branch.used then
        emptyIfTrue isRoot [ branch.insertionLocation ]

    else
        let
            relevantUsages : List LetInsertPosition
            relevantUsages =
                branch.branches
                    |> RangeDict.values
                    |> List.concatMap (canBeMovedToCloserLocation False name)
        in
        -- TODO Avoid looking at other branches if we already found 2 that use "name"
        if List.length relevantUsages > 1 then
            emptyIfTrue isRoot [ branch.insertionLocation ]

        else
            relevantUsages


emptyIfTrue : Bool -> List a -> List a
emptyIfTrue bool list =
    if bool then
        []

    else
        list


createError : Context -> Declared -> LetInsertPosition -> Rule.Error {}
createError context declared letInsertPosition =
    let
        letInsertLine : Int
        letInsertLine =
            case letInsertPosition of
                InsertNewLet insertLocation ->
                    insertLocation.row

                InsertExistingLet insertLocation ->
                    insertLocation.row
    in
    Rule.errorWithFix
        { message = "Let value was declared prematurely"
        , details =
            [ "This value is only used in some code paths, and it can therefore be computed unnecessarily."
            , "Try moving it closer to where it is needed, I recommend to move it to line " ++ String.fromInt letInsertLine ++ "."
            ]
        }
        declared.reportRange
        (fix context declared letInsertPosition)


fix : Context -> Declared -> LetInsertPosition -> List Fix
fix context declared letInsertPosition =
    if declared.introducesVariablesInImplementation then
        []

    else
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
                    |> insertInLet declared.reportRange.start.column insertLocation.column
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


insertInLet : Int -> Int -> String -> String
insertInLet initialPosition column source =
    case source |> String.trim |> String.lines of
        [] ->
            ""

        firstLine :: restOfLines ->
            ((firstLine :: List.map (\line -> String.repeat (column - initialPosition) " " ++ line) restOfLines)
                |> String.join "\n"
            )
                ++ "\n"
                ++ String.repeat (column - 1) " "


getLastListItem : List a -> Maybe a
getLastListItem =
    List.reverse >> List.head
