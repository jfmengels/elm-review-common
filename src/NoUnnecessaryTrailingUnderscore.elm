module NoUnnecessaryTrailingUnderscore exposing (rule)

{-|

@docs rule

-}

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
    Rule.newModuleRuleSchema "NoUnnecessaryTrailingUnderscore" ( Set.empty, [] )
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    Stack NamesInScope


type alias Stack comparable =
    ( Set comparable, List (Set comparable) )


type alias NamesInScope =
    Set String


declarationVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                argErrors : List (Rule.Error {})
                argErrors =
                    argumentErrors
                        (function.declaration
                            |> Node.value
                            |> .arguments
                        )

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
                    :: argErrors

              else
                argErrors
            , context
            )

        _ ->
            ( [], context )


argumentErrors : List (Node Pattern.Pattern) -> List (Rule.Error {})
argumentErrors arguments =
    let
        argNames : List ( Range, String )
        argNames =
            List.concatMap getDeclaredVariableNames arguments

        argNamesInScope : Set String
        argNamesInScope =
            argNames
                |> List.map Tuple.second
                |> Set.fromList
    in
    List.filterMap (error argNamesInScope) argNames


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
    case Node.value node of
        Expression.CaseExpression { cases } ->
            let
                errors : List (Rule.Error {})
                errors =
                    List.concatMap
                        (\( pattern, _ ) ->
                            pattern
                                |> getDeclaredVariableNames
                                |> List.filterMap (error {- TODO -} Set.empty)
                        )
                        cases
            in
            ( errors, context )

        _ ->
            ( [], context )


error : Set String -> ( Range, String ) -> Maybe (Rule.Error {})
error namesInScope ( range, name ) =
    if
        String.endsWith "_" name
            && not (Set.member (String.dropRight 1 name) namesInScope)
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
