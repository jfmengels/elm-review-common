module NoMissingTypeAnnotationInLetIn exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports `let in` declarations that do not have a type annotation.

Type annotations help you understand what happens in the code, and it will help the compiler give better error messages.

    config =
        [ NoMissingTypeAnnotationInLetIn.rule
        ]

This rule does not report top-level declarations without a type annotation inside a `let in`.
For that, enable [`NoMissingTypeAnnotation`](./NoMissingTypeAnnotation).


## Fail

    a : number
    a =
        let
            -- Missing annotation
            b =
                2
        in
        b


## Success

    -- Top-level annotation is not necessary, but good to have!
    a : number
    a =
        let
            b : number
            b =
                2
        in
        b


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoMissingTypeAnnotationInLetIn
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoMissingTypeAnnotationInLetIn" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , knownTypes : Dict String (List String)
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , knownTypes = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor expression context =
    case Node.value expression of
        Expression.LetExpression { declarations } ->
            ( List.filterMap
                (\declaration ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            reportFunctionWithoutSignature context function

                        _ ->
                            Nothing
                )
                declarations
            , context
            )

        _ ->
            ( [], context )


reportFunctionWithoutSignature : Context -> Expression.Function -> Maybe (Error {})
reportFunctionWithoutSignature context function =
    case function.signature of
        Just _ ->
            Nothing

        Nothing ->
            let
                name : Node String
                name =
                    function.declaration
                        |> Node.value
                        |> .name

                maybeType : Maybe String
                maybeType =
                    inferType context (function.declaration |> Node.value |> .expression)
            in
            Rule.errorWithFix
                { message = "Missing type annotation for `" ++ Node.value name ++ "`"
                , details =
                    [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
                    ]
                }
                (Node.range name)
                (createFix name maybeType)
                |> Just


createFix : Node String -> Maybe String -> List Fix
createFix functionNameNode maybeType =
    case maybeType of
        Just type_ ->
            let
                functionName : String
                functionName =
                    Node.value functionNameNode

                position : { row : Int, column : Int }
                position =
                    (Node.range functionNameNode).start
            in
            [ Fix.insertAt position (functionName ++ " : " ++ type_ ++ "\n" ++ String.repeat (position.column - 1) " ") ]

        Nothing ->
            []


inferType : Context -> Node Expression -> Maybe String
inferType context node =
    case Node.value node of
        Expression.Literal _ ->
            Just "String"

        Expression.Integer _ ->
            Just "number"

        Expression.Floatable _ ->
            Just "Float"

        Expression.UnitExpr ->
            Just "()"

        Expression.FunctionOrValue _ name ->
            case ( ModuleNameLookupTable.moduleNameFor context.lookupTable node, name ) of
                ( Just [ "Basics" ], "True" ) ->
                    Just "Bool"

                ( Just [ "Basics" ], "False" ) ->
                    Just "Bool"

                ( Just [], _ ) ->
                    Dict.get name context.knownTypes
                        |> Maybe.map (String.join " -> ")

                _ ->
                    Nothing

        _ ->
            -- TODO Handle other cases
            Nothing



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor nodes context =
    let
        knownTypes : Dict String (List String)
        knownTypes =
            nodes
                |> List.filterMap typeOfDeclaration
                |> Dict.fromList
    in
    ( [], { context | knownTypes = knownTypes } )


typeOfDeclaration : Node Declaration -> Maybe ( String, List String )
typeOfDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                functionName : String
                functionName =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            function.signature
                |> Maybe.andThen (Node.value >> .typeAnnotation >> typeAnnotationAsString)
                |> Maybe.map (Tuple.pair functionName)

        _ ->
            Nothing


typeAnnotationAsString : Node TypeAnnotation -> Maybe (List String)
typeAnnotationAsString node =
    case Node.value node of
        TypeAnnotation.Typed (Node _ ( moduleName, name )) arguments ->
            -- TODO use arguments
            if List.isEmpty arguments then
                Just [ String.join "." (moduleName ++ [ name ]) ]

            else
                Nothing

        TypeAnnotation.Unit ->
            Just [ "()" ]

        TypeAnnotation.GenericType genericType ->
            Just [ genericType ]

        TypeAnnotation.FunctionTypeAnnotation input output ->
            Maybe.map2 (++)
                (typeAnnotationAsString input)
                (typeAnnotationAsString output)

        _ ->
            -- TODO Handle other cases
            Nothing
