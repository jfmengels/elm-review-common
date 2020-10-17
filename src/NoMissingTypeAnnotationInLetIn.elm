module NoMissingTypeAnnotationInLetIn exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import TypeInference.Infer exposing (inferType)
import TypeInference.TypeByNameLookup as TypeByNameLookup exposing (TypeByNameLookup)


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
    { moduleNameLookupTable : ModuleNameLookupTable
    , typeByNameLookup : TypeByNameLookup
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { moduleNameLookupTable = lookupTable
            , typeByNameLookup = TypeByNameLookup.empty
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


addTypeFromPatternToContext : Node Pattern -> Context -> Context
addTypeFromPatternToContext pattern context =
    case Node.value pattern of
        Pattern.AllPattern ->
            context

        Pattern.UnitPattern ->
            context

        Pattern.CharPattern _ ->
            context

        Pattern.StringPattern _ ->
            context

        Pattern.IntPattern _ ->
            context

        Pattern.HexPattern _ ->
            context

        Pattern.FloatPattern _ ->
            context

        Pattern.TuplePattern patterns ->
            --List.foldl addTypeFromPatternToContext context patterns
            context

        Pattern.RecordPattern _ ->
            context

        Pattern.UnConsPattern _ _ ->
            context

        Pattern.ListPattern _ ->
            context

        Pattern.VarPattern _ ->
            context

        Pattern.NamedPattern { name } argumentPatterns ->
            case TypeByNameLookup.byName context.typeByNameLookup name of
                Just type_ ->
                    let
                        typeVariablesInType : Set String
                        typeVariablesInType =
                            findTypeVariables type_
                    in
                    { context
                        | typeByNameLookup =
                            TypeByNameLookup.addType (assignTypesToPatterns typeVariablesInType type_ argumentPatterns) context.typeByNameLookup
                    }

                Nothing ->
                    context

        Pattern.AsPattern _ _ ->
            context

        Pattern.ParenthesizedPattern _ ->
            context


assignTypesToPatterns : Set String -> Elm.Type.Type -> List (Node Pattern) -> List ( String, Elm.Type.Type )
assignTypesToPatterns typeVariables type_ patterns =
    case patterns of
        [] ->
            []

        head :: rest ->
            case type_ of
                Elm.Type.Lambda input output ->
                    (assignTypeToPattern input head
                        |> List.filter
                            (\( _, typeForPattern ) ->
                                Set.isEmpty <|
                                    Set.intersect
                                        typeVariables
                                        (findTypeVariables typeForPattern)
                            )
                    )
                        ++ assignTypesToPatterns typeVariables output rest

                _ ->
                    []


assignTypeToPattern : Elm.Type.Type -> Node Pattern -> List ( String, Elm.Type.Type )
assignTypeToPattern type_ node =
    case ( Node.value node, type_ ) of
        ( Pattern.VarPattern name, _ ) ->
            [ ( name, type_ ) ]

        ( Pattern.TuplePattern subPatterns, Elm.Type.Tuple tuples ) ->
            List.map2 assignTypeToPattern
                tuples
                subPatterns
                |> List.concat

        ( Pattern.RecordPattern patternFieldNames, Elm.Type.Record typeFields _ ) ->
            List.filterMap
                (Node.value
                    >> (\patternFieldName ->
                            find
                                (\( typeFieldName, _ ) ->
                                    typeFieldName == patternFieldName
                                )
                                typeFields
                       )
                )
                patternFieldNames

        _ ->
            []


inferTypeFromPattern : Node Pattern -> Maybe Elm.Type.Type
inferTypeFromPattern node =
    case Node.value node of
        Pattern.VarPattern _ ->
            Nothing

        Pattern.AllPattern ->
            Nothing

        Pattern.UnitPattern ->
            Just (Elm.Type.Tuple [])

        Pattern.CharPattern _ ->
            Nothing

        Pattern.StringPattern _ ->
            Nothing

        Pattern.IntPattern _ ->
            Nothing

        Pattern.HexPattern _ ->
            Nothing

        Pattern.FloatPattern _ ->
            Nothing

        Pattern.TuplePattern _ ->
            Nothing

        Pattern.RecordPattern _ ->
            Nothing

        Pattern.UnConsPattern _ _ ->
            Nothing

        Pattern.ListPattern _ ->
            Nothing

        Pattern.NamedPattern _ _ ->
            Nothing

        Pattern.AsPattern _ _ ->
            Nothing

        Pattern.ParenthesizedPattern _ ->
            Nothing


inferTypeFromCombinationOf : List (() -> ( Context, Node Expression )) -> Maybe Elm.Type.Type
inferTypeFromCombinationOf expressions =
    inferTypeFromCombinationOfInternal
        { hasUnknowns = False, maybeInferred = Nothing, typeVariablesList = [] }
        expressions


inferTypeFromCombinationOfInternal :
    { hasUnknowns : Bool
    , maybeInferred : Maybe Elm.Type.Type
    , typeVariablesList : List (Set String)
    }
    -> List (() -> ( Context, Node Expression ))
    -> Maybe Elm.Type.Type
inferTypeFromCombinationOfInternal previousItemsResult expressions =
    case expressions of
        [] ->
            if previousItemsResult.hasUnknowns then
                Nothing

            else
                case previousItemsResult.typeVariablesList of
                    [] ->
                        -- Should not happen?
                        Nothing

                    head :: tail ->
                        if List.all ((==) head) tail then
                            previousItemsResult.maybeInferred

                        else
                            Nothing

        head :: tail ->
            let
                ( context, node ) =
                    head ()
            in
            case inferType context node of
                Just inferredType ->
                    let
                        typeVariables : Set String
                        typeVariables =
                            findTypeVariables inferredType

                        refinedType_ : Elm.Type.Type
                        refinedType_ =
                            case previousItemsResult.maybeInferred of
                                Just previouslyInferred ->
                                    refineInferredType previouslyInferred inferredType

                                Nothing ->
                                    inferredType
                    in
                    if Set.isEmpty typeVariables then
                        Just inferredType

                    else
                        inferTypeFromCombinationOfInternal
                            { previousItemsResult
                                | maybeInferred = Just refinedType_
                                , typeVariablesList = typeVariables :: previousItemsResult.typeVariablesList
                            }
                            tail

                Nothing ->
                    inferTypeFromCombinationOfInternal
                        { previousItemsResult | hasUnknowns = True }
                        tail


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


refineInferredType : Elm.Type.Type -> Elm.Type.Type -> Elm.Type.Type
refineInferredType _ typeB =
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

        Elm.Type.Record fields maybeGeneric ->
            let
                startSet : Set String
                startSet =
                    case maybeGeneric of
                        Just generic ->
                            Set.singleton generic

                        Nothing ->
                            Set.empty
            in
            fields
                |> List.map (Tuple.second >> findTypeVariables)
                |> List.foldl Set.union startSet


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
    ( []
    , { context
        | typeByNameLookup =
            TypeByNameLookup.addType
                (List.concatMap typeOfDeclaration nodes)
                context.typeByNameLookup
      }
    )


typeOfLetDeclaration : Context -> Node Expression.LetDeclaration -> List ( String, Elm.Type.Type )
typeOfLetDeclaration context node =
    case Node.value node of
        Expression.LetFunction function ->
            typeOfFunctionDeclaration context function

        Expression.LetDestructuring _ _ ->
            []


typeOfFunctionDeclaration : Context -> Expression.Function -> List ( String, Elm.Type.Type )
typeOfFunctionDeclaration context function =
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
            case inferType context (function.declaration |> Node.value |> .expression) of
                Just inferredType ->
                    [ ( functionName, inferredType ) ]

                Nothing ->
                    []


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
            []

        Declaration.Destructuring _ _ ->
            -- Can't occur
            []
