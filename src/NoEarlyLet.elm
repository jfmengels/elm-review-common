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
    , currentBranching : List Range
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\extractSourceCode () ->
            { extractSourceCode = extractSourceCode
            , branch = newBranch
            , currentBranching = []
            }
        )
        |> Rule.withSourceCodeExtractor


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
        Declaration.FunctionDeclaration _ ->
            ( []
            , { extractSourceCode = context.extractSourceCode
              , branch = newBranch
              , currentBranching = []
              }
            )

        _ ->
            ( [], context )


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
                            (\( nameNode, declaration ) ->
                                { name = Node.value nameNode
                                , reportRange = Node.range nameNode
                                , declarationRange = fullLines (Node.range declaration)
                                , removeRange =
                                    if isDeclarationAlone then
                                        { start = (Node.range node).start
                                        , end = (Node.range expression).start
                                        }

                                    else
                                        Node.range declaration
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


fullLines : Range -> Range
fullLines range =
    { start = { row = range.start.row, column = 1 }
    , end = range.end
    }


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
                                    |> Maybe.map (createError context declaration)
                        )
                        branch.letDeclarations

                Nothing ->
                    []

        _ ->
            []


collectDeclarations : Node Expression.LetDeclaration -> List ( Node String, Node Expression.LetDeclaration )
collectDeclarations node =
    case Node.value node of
        Expression.LetFunction { declaration } ->
            -- TODO Add support for let functions? but need to check name clashes...
            if List.isEmpty (Node.value declaration).arguments then
                [ ( (Node.value declaration).name, node ) ]

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


createError : Context -> Declared -> LetInsertPosition -> Rule.Error {}
createError context declared (InsertNewLet insertLocation) =
    Rule.errorWithFix
        { message = "REPLACEME"
        , details = [ "REPLACEME" ]
        }
        declared.reportRange
        [ Fix.removeRange declared.removeRange
        , context.extractSourceCode declared.declarationRange
            |> wrapInLet insertLocation.column
            |> Fix.insertAt insertLocation
        ]


wrapInLet : Int -> String -> String
wrapInLet column source =
    let
        padding : String
        padding =
            String.repeat (column - 1) " "
    in
    [ [ "let" ]
    , stripSharedPadding source
        |> List.map (\line -> padding ++ line)
    , [ padding ++ "in", padding ]
    ]
        |> List.concat
        |> String.join "\n"


stripSharedPadding : String -> List String
stripSharedPadding source =
    let
        lines : List String
        lines =
            String.lines source

        sharedPadding : Int
        sharedPadding =
            lines
                |> List.map (String.toList >> countBlanks 0)
                |> List.minimum
                |> Maybe.withDefault 4
    in
    List.map (String.dropLeft sharedPadding) lines


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
