module NoMissingTypeAnnotationInLetIn exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


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
    , knownTypes : Dict String Elm.Type.Type
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
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration

                maybeType : Maybe String
                maybeType =
                    if List.isEmpty declaration.arguments then
                        inferType context declaration.expression
                            |> Maybe.map typeAsString

                    else
                        Nothing
            in
            Rule.errorWithFix
                { message = "Missing type annotation for `" ++ Node.value declaration.name ++ "`"
                , details =
                    [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
                    ]
                }
                (Node.range declaration.name)
                (createFix declaration.name maybeType)
                |> Just


createFix : Node String -> Maybe String -> List Fix
createFix functionNameNode maybeInferredType =
    case maybeInferredType of
        Nothing ->
            []

        Just inferredType ->
            let
                functionName : String
                functionName =
                    Node.value functionNameNode

                position : { row : Int, column : Int }
                position =
                    (Node.range functionNameNode).start
            in
            [ Fix.insertAt position (functionName ++ " : " ++ inferredType ++ "\n" ++ String.repeat (position.column - 1) " ") ]


typeAsString : Elm.Type.Type -> String
typeAsString type_ =
    (typeAsStringWithParensMaybe type_).value


typeAsStringWithParensMaybe : Elm.Type.Type -> { value : String, mayNeedParens : Bool }
typeAsStringWithParensMaybe type_ =
    case type_ of
        Elm.Type.Var string ->
            { value = string
            , mayNeedParens = False
            }

        Elm.Type.Lambda input output ->
            { value = typeAsStringWrappedInParens input ++ " -> " ++ typeAsString output
            , mayNeedParens = True
            }

        Elm.Type.Tuple types ->
            { value =
                if List.isEmpty types then
                    "()"

                else
                    "( " ++ String.join ", " (List.map typeAsString types) ++ " )"
            , mayNeedParens = False
            }

        Elm.Type.Type string types ->
            -- TODO Handle aliasing correctly
            -- TODO Add imports if necessary
            { value = String.join " " (string :: List.map typeAsStringWrappedInParens types)
            , mayNeedParens = not (List.isEmpty types)
            }

        Elm.Type.Record fields maybeExtensibleValue ->
            let
                extensibleValueAsString : String
                extensibleValueAsString =
                    case maybeExtensibleValue of
                        Just extensibleValue ->
                            extensibleValue ++ " | "

                        Nothing ->
                            ""
            in
            { value =
                if List.isEmpty fields then
                    "{}"

                else
                    "{ " ++ extensibleValueAsString ++ String.join ", " (List.map recordFieldAsString fields) ++ " }"
            , mayNeedParens = False
            }


typeAsStringWrappedInParens : Elm.Type.Type -> String
typeAsStringWrappedInParens type_ =
    let
        { value, mayNeedParens } =
            typeAsStringWithParensMaybe type_
    in
    if mayNeedParens then
        "(" ++ value ++ ")"

    else
        value


recordFieldAsString : ( String, Elm.Type.Type ) -> String
recordFieldAsString ( fieldName, fieldType ) =
    fieldName ++ " : " ++ typeAsString fieldType


inferType : Context -> Node Expression -> Maybe Elm.Type.Type
inferType context node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            inferType context expr

        Expression.Literal _ ->
            -- TODO Re-add "String." but remove it at stringification time
            Just (Elm.Type.Type "String" [])

        Expression.CharLiteral _ ->
            -- TODO Re-add "Char." but remove it at stringification time
            Just (Elm.Type.Type "Char" [])

        Expression.Integer _ ->
            Just (Elm.Type.Var "number")

        Expression.Hex _ ->
            Just (Elm.Type.Var "number")

        Expression.Floatable _ ->
            -- TODO Re-add "Basics." but remove it at stringification time
            Just (Elm.Type.Type "Float" [])

        Expression.UnitExpr ->
            Just (Elm.Type.Tuple [])

        Expression.FunctionOrValue _ name ->
            case ( ModuleNameLookupTable.moduleNameFor context.lookupTable node, name ) of
                ( Just [ "Basics" ], "True" ) ->
                    -- TODO Re-add "Basics." but remove it at stringification time
                    Just (Elm.Type.Type "Bool" [])

                ( Just [ "Basics" ], "False" ) ->
                    -- TODO Re-add "Basics." but remove it at stringification time
                    Just (Elm.Type.Type "Bool" [])

                ( Just [], _ ) ->
                    Dict.get name context.knownTypes

                _ ->
                    Nothing

        Expression.Application elements ->
            case elements of
                [] ->
                    Nothing

                function :: arguments ->
                    inferType context function
                        |> Maybe.andThen (applyArguments context arguments)

        Expression.TupledExpression nodes ->
            let
                inferredTypes : List Elm.Type.Type
                inferredTypes =
                    List.filterMap (inferType context) nodes
            in
            if List.length inferredTypes == List.length nodes then
                Just (Elm.Type.Tuple inferredTypes)

            else
                Nothing

        Expression.ListExpr nodes ->
            inferTypeForList context nodes

        Expression.RecordExpr fields ->
            let
                inferredFields : List ( String, Elm.Type.Type )
                inferredFields =
                    List.filterMap
                        (Node.value
                            >> (\( fieldName, fieldValue ) ->
                                    Maybe.map
                                        (Tuple.pair (Node.value fieldName))
                                        (inferType context fieldValue)
                               )
                        )
                        fields
            in
            if List.length inferredFields == List.length fields then
                Just (Elm.Type.Record inferredFields Nothing)

            else
                Nothing

        Expression.RecordAccess expression (Node _ fieldName) ->
            case inferType context expression of
                Just (Elm.Type.Record fields _) ->
                    find (\( name, _ ) -> fieldName == name) fields
                        |> Maybe.map Tuple.second

                _ ->
                    Nothing

        Expression.RecordAccessFunction fieldName ->
            Just <|
                Elm.Type.Lambda
                    (Elm.Type.Record [ ( String.dropLeft 1 fieldName, Elm.Type.Var "a" ) ] (Just "b"))
                    (Elm.Type.Var "a")

        Expression.OperatorApplication _ _ _ _ ->
            -- TODO Handle this case
            Nothing

        Expression.IfBlock _ ifTrue ifFalse ->
            case inferType context ifTrue of
                Just trueType_ ->
                    if Set.isEmpty (findTypeVariables trueType_) then
                        Just trueType_

                    else
                        case inferType context ifFalse of
                            Just falseType_ ->
                                Just (refineInferredType trueType_ falseType_)

                            Nothing ->
                                Just trueType_

                Nothing ->
                    inferType context ifFalse

        Expression.PrefixOperator _ ->
            -- TODO Handle this case
            Nothing

        Expression.Operator _ ->
            -- Never occurs
            Nothing

        Expression.Negation _ ->
            -- TODO Handle this case
            Nothing

        Expression.LetExpression _ ->
            -- TODO Handle this case
            Nothing

        Expression.CaseExpression _ ->
            -- TODO Handle this case
            Nothing

        Expression.LambdaExpression _ ->
            -- TODO Handle this case
            Nothing

        Expression.RecordUpdateExpression _ _ ->
            -- TODO Handle this case
            Nothing

        Expression.GLSLExpression _ ->
            -- TODO Handle this case
            Nothing


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if predicate head then
                Just head

            else
                find predicate tail


inferTypeForList : Context -> List (Node Expression) -> Maybe Elm.Type.Type
inferTypeForList context nodes =
    if List.isEmpty nodes then
        Just (Elm.Type.Type "List" [ Elm.Type.Var "nothing" ])

    else
        inferTypeForNonEmptyList context nodes Nothing


inferTypeForNonEmptyList : Context -> List (Node Expression) -> Maybe Elm.Type.Type -> Maybe Elm.Type.Type
inferTypeForNonEmptyList context nodes maybeCurrentlyInferredType =
    case nodes of
        [] ->
            case maybeCurrentlyInferredType of
                Just currentlyInferredType ->
                    Just (Elm.Type.Type "List" [ currentlyInferredType ])

                Nothing ->
                    Nothing

        head :: tail ->
            case inferType context head of
                Just inferredType ->
                    if Set.isEmpty (findTypeVariables inferredType) then
                        Just (Elm.Type.Type "List" [ inferredType ])

                    else
                        let
                            newInferredType : Maybe Elm.Type.Type
                            newInferredType =
                                case maybeCurrentlyInferredType of
                                    Just currentlyInferredType ->
                                        Just (refineInferredType currentlyInferredType inferredType)

                                    Nothing ->
                                        Just inferredType
                        in
                        inferTypeForNonEmptyList context tail newInferredType

                Nothing ->
                    inferTypeForNonEmptyList context tail maybeCurrentlyInferredType


refineInferredType : Elm.Type.Type -> Elm.Type.Type -> Elm.Type.Type
refineInferredType typeA typeB =
    typeB


applyArguments : Context -> List (Node Expression) -> Elm.Type.Type -> Maybe Elm.Type.Type
applyArguments context arguments type_ =
    applyArgumentsInternal context arguments Set.empty type_


applyArgumentsInternal : Context -> List (Node Expression) -> Set String -> Elm.Type.Type -> Maybe Elm.Type.Type
applyArgumentsInternal context arguments previousTypeVariables type_ =
    case arguments of
        [] ->
            if Set.intersect (findTypeVariables type_) previousTypeVariables |> Set.isEmpty then
                Just type_

            else
                Nothing

        _ :: restOfArguments ->
            case type_ of
                Elm.Type.Lambda input output ->
                    let
                        typeVariables : Set String
                        typeVariables =
                            Set.union
                                (findTypeVariables input)
                                previousTypeVariables
                    in
                    applyArgumentsInternal context restOfArguments typeVariables output

                _ ->
                    Nothing


findTypeVariables : Elm.Type.Type -> Set String
findTypeVariables type_ =
    case type_ of
        Elm.Type.Var string ->
            Set.singleton string

        Elm.Type.Lambda input output ->
            Set.union
                (findTypeVariables input)
                (findTypeVariables output)

        Elm.Type.Tuple types ->
            types
                |> List.map findTypeVariables
                |> List.foldl Set.union Set.empty

        Elm.Type.Type _ types ->
            types
                |> List.map findTypeVariables
                |> List.foldl Set.union Set.empty

        Elm.Type.Record fields _ ->
            fields
                |> List.map (Tuple.second >> findTypeVariables)
                |> List.foldl Set.union Set.empty


typeAnnotationToElmType : Node TypeAnnotation -> Elm.Type.Type
typeAnnotationToElmType node =
    case Node.value node of
        TypeAnnotation.GenericType var ->
            Elm.Type.Var var

        TypeAnnotation.Typed (Node _ ( moduleName, name )) nodes ->
            Elm.Type.Type (String.join "." (moduleName ++ [ name ])) (List.map typeAnnotationToElmType nodes)

        TypeAnnotation.Unit ->
            Elm.Type.Tuple []

        TypeAnnotation.Tupled nodes ->
            Elm.Type.Tuple (List.map typeAnnotationToElmType nodes)

        TypeAnnotation.Record recordDefinition ->
            Elm.Type.Record
                (List.map
                    (Node.value >> (\( fieldName, fieldType ) -> ( Node.value fieldName, typeAnnotationToElmType fieldType )))
                    recordDefinition
                )
                Nothing

        TypeAnnotation.GenericRecord genericVar recordDefinition ->
            Elm.Type.Record
                (List.map
                    (Node.value >> (\( fieldName, fieldType ) -> ( Node.value fieldName, typeAnnotationToElmType fieldType )))
                    (Node.value recordDefinition)
                )
                (Just (Node.value genericVar))

        TypeAnnotation.FunctionTypeAnnotation input output ->
            Elm.Type.Lambda (typeAnnotationToElmType input) (typeAnnotationToElmType output)



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor nodes context =
    let
        knownTypes : Dict String Elm.Type.Type
        knownTypes =
            nodes
                |> List.concatMap typeOfDeclaration
                |> Dict.fromList
    in
    ( [], { context | knownTypes = knownTypes } )


typeOfDeclaration : Node Declaration -> List ( String, Elm.Type.Type )
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
            case function.signature of
                Just signature ->
                    [ ( functionName
                      , signature
                            |> Node.value
                            |> .typeAnnotation
                            |> typeAnnotationToElmType
                      )
                    ]

                Nothing ->
                    []

        Declaration.CustomTypeDeclaration type_ ->
            let
                customTypeType : Elm.Type.Type
                customTypeType =
                    Elm.Type.Type
                        (Node.value type_.name)
                        (List.map (Node.value >> Elm.Type.Var) type_.generics)
            in
            List.map
                (\(Node _ { name, arguments }) ->
                    let
                        functionType : Elm.Type.Type
                        functionType =
                            List.foldr
                                (\input output ->
                                    Elm.Type.Lambda
                                        (typeAnnotationToElmType input)
                                        output
                                )
                                customTypeType
                                arguments
                    in
                    ( Node.value name, functionType )
                )
                type_.constructors

        Declaration.AliasDeclaration typeAlias ->
            let
                aliasType : Elm.Type.Type
                aliasType =
                    Elm.Type.Type
                        (Node.value typeAlias.name)
                        (List.map (Node.value >> Elm.Type.Var) typeAlias.generics)
            in
            case typeAnnotationToElmType typeAlias.typeAnnotation of
                Elm.Type.Record fields _ ->
                    let
                        functionType : Elm.Type.Type
                        functionType =
                            List.foldr
                                (\( _, type_ ) output -> Elm.Type.Lambda type_ output)
                                aliasType
                                fields
                    in
                    [ ( Node.value typeAlias.name, functionType ) ]

                _ ->
                    []

        Declaration.PortDeclaration { name, typeAnnotation } ->
            [ ( Node.value name, typeAnnotationToElmType typeAnnotation ) ]

        Declaration.InfixDeclaration _ ->
            -- TODO Handle
            []

        Declaration.Destructuring _ _ ->
            -- Can't occur
            []
