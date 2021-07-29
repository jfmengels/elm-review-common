module NoPrematureLetComputation exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import RangeDict exposing (RangeDict)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)


{-| Reports let declarations that are computed earlier than needed.

This rule is useful to prevent unnecessary computations and to group related code together.

    config =
        [ NoPrematureLetComputation.rule
        ]

🔧 Running with `--fix` will automatically fix almost all of the reported errors.


## Fail

In this example, we compute `value` earlier than needed, and we end up not using it in of the branches.

    someFunction n =
        let
            value =
                expensiveComputation n
        in
        if needToCompute then
            value + 1

        else
            0


## Success

If we take the example from above, this would be the suggested (and automatic) fix:

    someFunction n =
        if needToCompute then
            let
                value =
                    expensiveComputation n
            in
            value + 1

        else
            0

A declaration will not be reported if it's used in multiple branches at the same level.
The rule will try to move the declaration as close as possible to the usages.

    someFunction n =
        let
            value =
                expensiveComputation n
        in
        if condition then
            value + 1

        else
            value - 1

Sometimes, when a computation is somewhat expensive, it is done once in a let declaration and then
reference in an anonymous function. This rule does not want to worsen the performance, and therefore
declarations will not be moved to inside a lambda.

    someFunction items n =
        let
            value =
                expensiveComputation n
        in
        List.map
            (\item ->
                if condition item then
                    value + item.value

                else
                    0
            )
            items


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoPrematureLetComputation
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoPrematureLetComputation" initialContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { extractSourceCode : Range -> String
    , branch : Scope
    , branching : Branching
    }


type alias Branching =
    { full : List Range
    , last : Maybe Range
    }


type Scope
    = Branch BranchData
    | LetScope BranchData
    | Lambda BranchData


type ScopeType
    = Branch_
    | LetScope_
    | Lambda_


type alias BranchData =
    { type_ : ScopeType
    , letDeclarations : List Declared
    , used : List String
    , insertionLocation : LetInsertPosition
    , branches : RangeDict Scope
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


newBranch : LetInsertPosition -> Scope
newBranch insertionLocation =
    Branch
        { type_ = Branch_
        , letDeclarations = []
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


updateCurrentBranch : (BranchData -> BranchData) -> List Range -> Scope -> Scope
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


updateAllSegmentsOfCurrentBranch : (BranchData -> BranchData) -> List Range -> Scope -> Scope
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
                                    (updateAllSegmentsOfCurrentBranch updateFn restOfSegments)
                                    branch.branches
                        }
                )
                segment


updateBranch : (BranchData -> BranchData) -> Scope -> Scope
updateBranch updateFn segment =
    case segment of
        Branch branch ->
            Branch (updateFn branch)

        LetScope branch ->
            LetScope (updateFn branch)

        Lambda branch ->
            Lambda (updateFn branch)


getCurrentBranch : List Range -> Scope -> Maybe Scope
getCurrentBranch currentBranching branch =
    case currentBranching of
        [] ->
            Just branch

        range :: restOfBranching ->
            RangeDict.get range (getBranches branch)
                |> Maybe.andThen (getCurrentBranch restOfBranching)


getBranches : Scope -> RangeDict Scope
getBranches =
    getBranchData >> .branches


getBranchData : Scope -> BranchData
getBranchData branch =
    case branch of
        Branch b ->
            b

        LetScope b ->
            b

        Lambda b ->
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
                branch : Scope
                branch =
                    updateCurrentBranch
                        (\b -> { b | used = name :: b.used })
                        context.branching.full
                        context.branch
            in
            { context | branch = branch }

        Expression.RecordUpdateExpression name _ ->
            let
                branch : Scope
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

                newScope : Scope
                newScope =
                    LetScope
                        { type_ = LetScope_
                        , letDeclarations = letDeclarations
                        , used = []
                        , insertionLocation = figureOutInsertionLocation node
                        , branches = RangeDict.empty
                        }

                contextWithDeclarationsMarked : Context
                contextWithDeclarationsMarked =
                    { context | branch = markLetDeclarationsAsIntroducingVariables (Node.range node) context }

                branch : Scope
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
                        contextWithDeclarationsMarked.branching.full
                        contextWithDeclarationsMarked.branch
            in
            { contextWithDeclarationsMarked
                | branch = branch
                , branching = addBranching (Node.range node) contextWithDeclarationsMarked.branching
            }

        Expression.IfBlock _ then_ else_ ->
            addBranches [ then_, else_ ] context

        Expression.CaseExpression { cases } ->
            let
                contextWithDeclarationsMarked : Context
                contextWithDeclarationsMarked =
                    if List.any (Tuple.first >> patternIntroducesVariable) cases then
                        { context | branch = markLetDeclarationsAsIntroducingVariables (Node.range node) context }

                    else
                        context

                branchNodes : List (Node Expression)
                branchNodes =
                    List.map (\( _, exprNode ) -> exprNode) cases
            in
            addBranches branchNodes contextWithDeclarationsMarked

        Expression.LambdaExpression { args } ->
            let
                branch : Scope
                branch =
                    if List.any patternIntroducesVariable args then
                        markLetDeclarationsAsIntroducingVariables (Node.range node) context

                    else
                        context.branch

                newScope : Scope
                newScope =
                    Lambda
                        { type_ = Lambda_
                        , letDeclarations = []
                        , used = []
                        , insertionLocation =
                            -- Will not be used
                            InsertNewLet { row = 0, column = 0 }
                        , branches = RangeDict.empty
                        }

                branchWithAddedScope : Scope
                branchWithAddedScope =
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
                        branch
            in
            { context
                | branch = branchWithAddedScope
                , branching = addBranching (Node.range node) context.branching
            }

        _ ->
            context


variablesInPattern : Node Pattern -> List (Node String)
variablesInPattern node =
    case Node.value node of
        Pattern.ListPattern patterns ->
            List.concatMap variablesInPattern patterns

        Pattern.TuplePattern patterns ->
            List.concatMap variablesInPattern patterns

        Pattern.RecordPattern fields ->
            fields

        Pattern.UnConsPattern left right ->
            List.concatMap variablesInPattern [ left, right ]

        Pattern.VarPattern name ->
            [ Node (Node.range node) name ]

        Pattern.NamedPattern _ patterns ->
            List.concatMap variablesInPattern patterns

        Pattern.AsPattern pattern name ->
            name :: variablesInPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            variablesInPattern pattern

        _ ->
            []


patternIntroducesVariable : Node Pattern -> Bool
patternIntroducesVariable node =
    case Node.value node of
        Pattern.ListPattern patterns ->
            List.any patternIntroducesVariable patterns

        Pattern.TuplePattern patterns ->
            List.any patternIntroducesVariable patterns

        Pattern.RecordPattern _ ->
            True

        Pattern.UnConsPattern left right ->
            patternIntroducesVariable left
                || patternIntroducesVariable right

        Pattern.VarPattern _ ->
            True

        Pattern.NamedPattern _ patterns ->
            List.any patternIntroducesVariable patterns

        Pattern.AsPattern _ _ ->
            True

        Pattern.ParenthesizedPattern pattern ->
            patternIntroducesVariable pattern

        _ ->
            False


markLetDeclarationsAsIntroducingVariables : Range -> Context -> Scope
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
        branch : Scope
        branch =
            updateCurrentBranch
                (\b -> { b | branches = insertNewBranches nodes b.branches })
                context.branching.full
                context.branch
    in
    { context | branch = branch }


insertNewBranches : List (Node Expression) -> RangeDict Scope -> RangeDict Scope
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
        Expression.LetExpression _ ->
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
            if List.isEmpty declaration.arguments then
                [ ( declaration.name
                  , Node.range declaration.expression
                  , node
                  )
                ]

            else
                []

        Expression.LetDestructuring pattern expression ->
            case variablesInPattern pattern of
                [ name ] ->
                    [ ( name
                      , Node.range expression
                      , node
                      )
                    ]

                _ ->
                    []


canBeMovedToCloserLocation : Bool -> String -> Scope -> List LetInsertPosition
canBeMovedToCloserLocation isRoot name segment =
    case segment of
        Branch branchData ->
            canBeMovedToCloserLocationForBranchData isRoot name branchData

        LetScope branchData ->
            canBeMovedToCloserLocationForBranchData isRoot name branchData

        Lambda branchData ->
            let
                closestLocation : List LetInsertPosition
                closestLocation =
                    canBeMovedToCloserLocationForBranchData isRoot name branchData
            in
            -- Duplicating so that the parent has to use its insert location,
            -- and we don't insert inside the let
            closestLocation ++ closestLocation


canBeMovedToCloserLocationForBranchData : Bool -> String -> BranchData -> List LetInsertPosition
canBeMovedToCloserLocationForBranchData isRoot name branchData =
    if List.member name branchData.used then
        emptyIfTrue isRoot [ branchData.insertionLocation ]

    else
        let
            relevantUsages : List LetInsertPosition
            relevantUsages =
                findRelevantUsages name (RangeDict.values branchData.branches) []
        in
        if List.length relevantUsages > 1 then
            emptyIfTrue isRoot [ branchData.insertionLocation ]

        else
            relevantUsages


findRelevantUsages : String -> List Scope -> List LetInsertPosition -> List LetInsertPosition
findRelevantUsages name branches result =
    if List.length result > 1 then
        -- If we have already found 2 branches with relevant usages, then we don't need to continue
        result

    else
        case branches of
            [] ->
                result

            first :: rest ->
                findRelevantUsages name rest (canBeMovedToCloserLocation False name first ++ result)


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
