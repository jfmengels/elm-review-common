module NoUnnecessaryTrailingUnderscore exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports... REPLACEME

    config =
        [ NoUnnecessaryTrailingUnderscore.rule
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
elm-review --template jfmengels/elm-review-common/example --rules NoUnnecessaryTrailingUnderscore
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnnecessaryTrailingUnderscore" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scopes : Scopes
    , scopesToAdd : Dict RangeLike (Set String)
    }


type alias Scopes =
    ( Set String, List (Set String) )


type alias RangeLike =
    List Int


initialContext : Context
initialContext =
    { scopes = ( Set.empty, [] )
    , scopesToAdd = Dict.empty
    }


declarationListVisitor : List (Node Declaration) -> Context -> ( List (Rule.Error {}), Context )
declarationListVisitor declarations emptyContext =
    List.foldl
        (\node ( errors, context ) ->
            case Node.value node of
                Declaration.FunctionDeclaration function ->
                    let
                        functionName : Node String
                        functionName =
                            function.declaration
                                |> Node.value
                                |> .name
                    in
                    ( if String.endsWith "_" (Node.value functionName) && not (Set.member (Node.value functionName) reservedElmKeywords) then
                        Rule.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            }
                            (Node.range functionName)
                            :: errors

                      else
                        errors
                    , { context | scopes = Tuple.mapFirst (Set.insert (Node.value functionName)) context.scopes }
                    )

                _ ->
                    ( [], context )
        )
        ( [], emptyContext )
        declarations


declarationVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                arguments : List (Node Pattern.Pattern)
                arguments =
                    function.declaration
                        |> Node.value
                        |> .arguments

                body : Node Expression
                body =
                    function.declaration
                        |> Node.value
                        |> .expression
            in
            report
                [ ( arguments, body ) ]
                context

        _ ->
            ( [], context )


type alias ScopeNames =
    { name : String
    , range : Range
    , origin : NameOrigin
    }


type NameOrigin
    = FromRecord
    | NotFromRecord


getDeclaredVariableNames : Node Pattern.Pattern -> List ScopeNames
getDeclaredVariableNames pattern =
    case Node.value pattern of
        Pattern.VarPattern name ->
            [ { name = name, range = Node.range pattern, origin = NotFromRecord } ]

        Pattern.ParenthesizedPattern subPattern ->
            getDeclaredVariableNames subPattern

        Pattern.AsPattern subPattern name ->
            { name = Node.value name, range = Node.range name, origin = NotFromRecord } :: getDeclaredVariableNames subPattern

        Pattern.TuplePattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.UnConsPattern left right ->
            List.concatMap getDeclaredVariableNames [ left, right ]

        Pattern.ListPattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.RecordPattern fields ->
            List.map (\field -> { name = Node.value field, range = Node.range field, origin = FromRecord }) fields

        _ ->
            []


reservedElmKeywords : Set String
reservedElmKeywords =
    Set.fromList
        [ "if_"
        , "then_"
        , "else_"
        , "case_"
        , "of_"
        , "let_"
        , "in_"
        , "type_"
        , "module_"
        , "where_"
        , "import_"
        , "exposing_"
        , "as_"
        , "port_"
        ]


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    let
        newContext : Context
        newContext =
            case Dict.get (Node.range node |> rangeToRangeLike) context.scopesToAdd of
                Just scopeToAdd ->
                    { context | scopes = addNewScope scopeToAdd context.scopes }

                Nothing ->
                    context
    in
    expressionVisitorHelp node newContext


expressionExitVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionExitVisitor node context =
    let
        newContext : Context
        newContext =
            if Dict.member (Node.range node |> rangeToRangeLike) context.scopesToAdd then
                { context | scopes = popScope context.scopes }

            else
                context
    in
    ( [], newContext )


expressionVisitorHelp : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitorHelp node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            report
                (List.map (Tuple.mapFirst List.singleton) cases)
                context

        _ ->
            ( [], context )


report : List ( List (Node Pattern.Pattern), Node a ) -> Context -> ( List (Rule.Error {}), Context )
report patternsAndBody context =
    let
        scopesToAdd : List { errors : List (Rule.Error {}), scopesToAdd : ( RangeLike, Set String ) }
        scopesToAdd =
            List.map
                (\( patterns, expression ) ->
                    let
                        declaredVariables : List ScopeNames
                        declaredVariables =
                            List.concatMap getDeclaredVariableNames patterns

                        names : Set String
                        names =
                            declaredVariables
                                |> List.map .name
                                |> Set.fromList
                    in
                    { errors = List.filterMap (error (addNewScope names context.scopes)) declaredVariables
                    , scopesToAdd =
                        ( rangeToRangeLike (Node.range expression)
                        , names
                        )
                    }
                )
                patternsAndBody
    in
    ( List.concatMap .errors scopesToAdd
    , { context
        | scopesToAdd =
            Dict.union
                (scopesToAdd |> List.map .scopesToAdd |> Dict.fromList)
                context.scopesToAdd
      }
    )


addNewScope : Set String -> Scopes -> Scopes
addNewScope set ( head, tail ) =
    ( set, head :: tail )


popScope : Scopes -> Scopes
popScope ( head, tail ) =
    case tail of
        [] ->
            ( head, tail )

        newHead :: newTail ->
            ( newHead, newTail )


error : Scopes -> ScopeNames -> Maybe (Rule.Error {})
error scopes { range, name, origin } =
    if
        String.endsWith "_" name
            && not (isDefinedInScope scopes (String.dropRight 1 name))
            && not (Set.member name reservedElmKeywords)
            && shouldNameBeReported origin
    then
        Just
            (Rule.error
                { message = "REPLACEME"
                , details = [ "REPLACEME" ]
                }
                range
            )

    else
        Nothing


shouldNameBeReported : NameOrigin -> Bool
shouldNameBeReported origin =
    case origin of
        FromRecord ->
            False

        NotFromRecord ->
            True


isDefinedInScope : Scopes -> String -> Bool
isDefinedInScope ( top, rest ) name =
    List.any (Set.member name) (top :: rest)


rangeToRangeLike : Range -> RangeLike
rangeToRangeLike range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
