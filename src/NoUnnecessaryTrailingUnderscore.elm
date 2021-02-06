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

                argNames : List ( Range, String )
                argNames =
                    List.concatMap getDeclaredVariableNames arguments

                argNamesInScope : Set String
                argNamesInScope =
                    argNames
                        |> List.map Tuple.second
                        |> Set.fromList

                newScopes : Scopes
                newScopes =
                    Tuple.mapFirst (Set.union argNamesInScope) context.scopes
            in
            ( List.filterMap (error newScopes) argNames
            , { context | scopes = newScopes }
            )

        _ ->
            ( [], context )


getDeclaredVariableNames : Node Pattern.Pattern -> List ( Range, String )
getDeclaredVariableNames pattern =
    case Node.value pattern of
        Pattern.VarPattern name ->
            [ ( Node.range pattern, name ) ]

        Pattern.ParenthesizedPattern subPattern ->
            getDeclaredVariableNames subPattern

        Pattern.AsPattern subPattern name ->
            ( Node.range name, Node.value name ) :: getDeclaredVariableNames subPattern

        Pattern.TuplePattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.UnConsPattern left right ->
            List.concatMap getDeclaredVariableNames [ left, right ]

        Pattern.ListPattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap getDeclaredVariableNames patterns

        _ ->
            -- We're ignoring record pattern because this is not where the name has really been assigned.
            -- This might create some false negatives, but that's fine.
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


expressionVisitorHelp : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitorHelp node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            let
                names : List ( Range, String )
                names =
                    List.concatMap
                        (\( pattern, _ ) ->
                            getDeclaredVariableNames pattern
                        )
                        cases

                newScopes : Scopes
                newScopes =
                    Tuple.mapFirst (Set.union (names |> List.map Tuple.second |> Set.fromList)) context.scopes
            in
            ( List.filterMap (error newScopes) names
            , { context | scopes = newScopes }
            )

        _ ->
            ( [], context )


addNewScope : Set String -> Scopes -> Scopes
addNewScope set ( head, tail ) =
    ( set, head :: tail )


error : Scopes -> ( Range, String ) -> Maybe (Rule.Error {})
error scopes ( range, name ) =
    if
        String.endsWith "_" name
            && not (isDefinedInScope scopes (String.dropRight 1 name))
            && not (Set.member name reservedElmKeywords)
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


isDefinedInScope : Scopes -> String -> Bool
isDefinedInScope ( top, rest ) name =
    List.any (Set.member name) (top :: rest)


rangeToRangeLike : Range -> List Int
rangeToRangeLike range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
