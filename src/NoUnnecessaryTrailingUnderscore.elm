module NoUnnecessaryTrailingUnderscore exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration
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
    Rule.newModuleRuleSchema "NoUnnecessaryTrailingUnderscore" ()
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


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

                functionName : Range
                functionName =
                    function.declaration |> Node.value |> .name |> Node.range
            in
            ( if True then
                Rule.error
                    { message = "REPLACEME"
                    , details = [ "REPLACEME" ]
                    }
                    functionName
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
            List.filterMap
                (\arg ->
                    case Node.value arg of
                        Pattern.VarPattern name ->
                            Just ( Node.range arg, name )

                        _ ->
                            Nothing
                )
                arguments

        argNamesInScope : Set String
        argNamesInScope =
            argNames
                |> List.map Tuple.second
                |> Set.fromList
    in
    argNames
        |> List.filter
            (\( _, name ) ->
                String.endsWith "_" name
                    && not (Set.member (String.dropRight 1 name) argNamesInScope)
                    && not (Set.member (String.dropRight 1 name) reservedElmKeywords)
            )
        |> List.map
            (\( range, name ) ->
                Rule.error
                    { message = "REPLACEME"
                    , details = [ "REPLACEME" ]
                    }
                    range
            )


reservedElmKeywords : Set String
reservedElmKeywords =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]


expressionVisitor node context =
    ( [], context )
