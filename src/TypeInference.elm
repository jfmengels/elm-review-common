module TypeInference exposing
    ( ModuleContext
    , ProjectContext
    , addProjectVisitors
    , fromProjectToModule
    , inferType
    , initialProjectContext
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency
import Review.Rule as Rule
import Set exposing (Set)
import TypeInference.TypeByNameLookup as TypeByNameLookup exposing (TypeByNameLookup)


type ProjectContext
    = ProjectContext InternalProjectContext


type alias InternalProjectContext =
    { dependencies : Dict ModuleName Review.Project.Dependency.Dependency
    }


unwrapProject : ProjectContext -> InternalProjectContext
unwrapProject (ProjectContext p) =
    p


wrapProject : InternalProjectContext -> ProjectContext
wrapProject =
    ProjectContext


type alias OuterModuleContext a =
    { a
        | moduleNameLookupTable : ModuleNameLookupTable
        , typeByNameLookup : TypeByNameLookup
        , typeInference : ModuleContext
    }


type alias ModuleContext =
    { dependencies : Dict ModuleName Review.Project.Dependency.Dependency
    , operatorsInScope : Dict String Elm.Type.Type
    }


initialProjectContext : ProjectContext
initialProjectContext =
    ProjectContext
        { dependencies = Dict.empty
        }


fromProjectToModule : { projectContext | typeInference : ProjectContext } -> ModuleContext
fromProjectToModule { typeInference } =
    let
        projectContext : InternalProjectContext
        projectContext =
            unwrapProject typeInference

        modules : Dict (List String) Elm.Docs.Module
        modules =
            projectContext.dependencies
                |> Dict.values
                |> List.concatMap Review.Project.Dependency.modules
                |> List.map (\module_ -> ( String.split "." module_.name, module_ ))
                |> Dict.fromList
    in
    { dependencies = projectContext.dependencies
    , operatorsInScope =
        List.concatMap
            (\import_ ->
                Dict.get (Node.value import_.moduleName) modules
                    |> Maybe.map (\{ binops } -> List.map (\binop -> ( binop.name, binop.tipe )) binops)
                    |> Maybe.withDefault []
            )
            elmCorePrelude
            |> Dict.fromList
    }


addProjectVisitors :
    Rule.ProjectRuleSchema
        { projectSchemaState | canAddModuleVisitor : () }
        { projectContext | typeInference : ProjectContext }
        (OuterModuleContext a)
    ->
        Rule.ProjectRuleSchema
            { projectSchemaState | canAddModuleVisitor : (), hasAtLeastOneVisitor : (), withModuleContext : Rule.Required }
            { projectContext | typeInference : ProjectContext }
            (OuterModuleContext a)
addProjectVisitors schema =
    schema
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor


moduleVisitor : Rule.ModuleRuleSchema schemaState (OuterModuleContext a) -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } (OuterModuleContext a)
moduleVisitor schema =
    schema |> Rule.withDeclarationListVisitor declarationListVisitor



-- DEPENDENCIES VISITOR


dependenciesVisitor : Dict String Review.Project.Dependency.Dependency -> { projectContext | typeInference : ProjectContext } -> ( List nothing, { projectContext | typeInference : ProjectContext } )
dependenciesVisitor rawDependencies context =
    let
        projectContext : InternalProjectContext
        projectContext =
            unwrapProject context.typeInference

        dependencies : Dict (List String) Review.Project.Dependency.Dependency
        dependencies =
            rawDependencies
                |> Dict.toList
                |> List.map (\( key, value ) -> ( String.split "." key, value ))
                |> Dict.fromList
    in
    ( []
    , { context | typeInference = ProjectContext { projectContext | dependencies = dependencies } }
    )



-- PRELUDE IMPORT


elmCorePrelude : List Import
elmCorePrelude =
    let
        explicit : List Exposing.TopLevelExpose -> Maybe Exposing
        explicit exposed =
            exposed
                |> List.map (Node Range.emptyRange)
                |> Exposing.Explicit
                |> Just
    in
    -- These are the default imports implicitly added by the Elm compiler
    -- https://package.elm-lang.org/packages/elm/core/latest
    [ createFakeImport
        { moduleName = [ "Basics" ]
        , moduleAlias = Nothing
        , exposingList = Just <| Exposing.All Range.emptyRange
        }
    , createFakeImport
        { moduleName = [ "List" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "List", open = Nothing }
                , Exposing.InfixExpose "::"
                ]
        }
    , createFakeImport
        { moduleName = [ "Maybe" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Maybe", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "Result" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Result", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "String" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Char", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Char" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Tuple" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Debug" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Platform" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Program", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Cmd" ]
        , moduleAlias = Just "Cmd"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Cmd", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Sub" ]
        , moduleAlias = Just "Sub"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Sub", open = Nothing }
                ]
        }
    ]


createFakeImport : { moduleName : List String, exposingList : Maybe Exposing, moduleAlias : Maybe String } -> Import
createFakeImport { moduleName, moduleAlias, exposingList } =
    { moduleName = Node Range.emptyRange moduleName
    , moduleAlias = moduleAlias |> Maybe.map (List.singleton >> Node Range.emptyRange)
    , exposingList = exposingList |> Maybe.map (Node Range.emptyRange)
    }



-- IMPORT VISITOR


importVisitor : Node Import -> OuterModuleContext a -> OuterModuleContext a
importVisitor node context =
    let
        newContext : OuterModuleContext a
        newContext =
            context
    in
    newContext



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> OuterModuleContext a -> ( List nothing, OuterModuleContext a )
declarationListVisitor nodes context =
    ( []
    , { context
        | typeByNameLookup =
            TypeByNameLookup.addType
                (List.concatMap typeOfDeclaration nodes)
                context.typeByNameLookup
      }
    )


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



-- TYPE INFERENCE


inferType : OuterModuleContext a -> Node Expression -> Maybe Elm.Type.Type
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
            case ( ModuleNameLookupTable.moduleNameFor context.moduleNameLookupTable node, name ) of
                ( Just [ "Basics" ], "True" ) ->
                    -- TODO Re-add "Basics." but remove it at stringification time
                    Just (Elm.Type.Type "Bool" [])

                ( Just [ "Basics" ], "False" ) ->
                    -- TODO Re-add "Basics." but remove it at stringification time
                    Just (Elm.Type.Type "Bool" [])

                ( Just [], _ ) ->
                    TypeByNameLookup.byName context.typeByNameLookup name

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
            if List.isEmpty nodes then
                Just (Elm.Type.Type "List" [ Elm.Type.Var "nothing" ])

            else
                inferTypeFromCombinationOf (List.map (\nodeInList () -> ( context, nodeInList )) nodes)
                    |> Maybe.map (\type_ -> Elm.Type.Type "List" [ type_ ])

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
            -- Needs lookup to prefix operator
            Nothing

        Expression.IfBlock _ ifTrue ifFalse ->
            inferTypeFromCombinationOf (List.map (\branchNode () -> ( context, branchNode )) [ ifTrue, ifFalse ])

        Expression.PrefixOperator operator ->
            Dict.get operator context.typeInference.operatorsInScope

        Expression.Operator _ ->
            -- Never occurs
            Nothing

        Expression.Negation expr ->
            inferType context expr

        Expression.LetExpression { declarations, expression } ->
            case inferType context expression of
                Just inferredType ->
                    Just inferredType

                Nothing ->
                    let
                        newContext : OuterModuleContext a
                        newContext =
                            { context
                                | typeByNameLookup =
                                    context.typeByNameLookup
                                        |> TypeByNameLookup.addNewScope
                                        |> TypeByNameLookup.addType (List.concatMap (typeOfLetDeclaration context) declarations)
                            }
                    in
                    inferType newContext expression

        Expression.CaseExpression { expression, cases } ->
            let
                inferredTypeForEvaluatedExpression : Maybe Elm.Type.Type
                inferredTypeForEvaluatedExpression =
                    inferType context expression
            in
            cases
                |> List.map
                    (\( pattern, expr ) () ->
                        let
                            typeByNameLookup : TypeByNameLookup
                            typeByNameLookup =
                                case inferredTypeForEvaluatedExpression of
                                    Just inferred ->
                                        TypeByNameLookup.addType (assignTypeToPattern inferred pattern) context.typeByNameLookup

                                    Nothing ->
                                        case ( Node.value expression, inferTypeFromPattern pattern ) of
                                            ( Expression.FunctionOrValue [] name, Just inferred ) ->
                                                TypeByNameLookup.addType [ ( name, inferred ) ] context.typeByNameLookup

                                            _ ->
                                                context.typeByNameLookup

                            contextToUse : OuterModuleContext a
                            contextToUse =
                                { context | typeByNameLookup = typeByNameLookup }
                        in
                        ( addTypeFromPatternToContext pattern contextToUse
                        , expr
                        )
                    )
                |> inferTypeFromCombinationOf

        Expression.LambdaExpression _ ->
            -- TODO Handle this case
            -- Needs inferring of arguments
            Nothing

        Expression.RecordUpdateExpression name _ ->
            TypeByNameLookup.byName context.typeByNameLookup (Node.value name)

        Expression.GLSLExpression _ ->
            -- TODO Handle this case
            Nothing


addTypeFromPatternToContext : Node Pattern -> OuterModuleContext a -> OuterModuleContext a
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

        Pattern.TuplePattern _ ->
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


inferTypeFromCombinationOf : List (() -> ( OuterModuleContext a, Node Expression )) -> Maybe Elm.Type.Type
inferTypeFromCombinationOf expressions =
    inferTypeFromCombinationOfInternal
        { hasUnknowns = False, maybeInferred = Nothing, typeVariablesList = [] }
        expressions


inferTypeFromCombinationOfInternal :
    { hasUnknowns : Bool
    , maybeInferred : Maybe Elm.Type.Type
    , typeVariablesList : List (Set String)
    }
    -> List (() -> ( OuterModuleContext a, Node Expression ))
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


applyArguments : OuterModuleContext a -> List (Node Expression) -> Elm.Type.Type -> Maybe Elm.Type.Type
applyArguments context arguments type_ =
    applyArgumentsInternal context arguments Set.empty type_


applyArgumentsInternal : OuterModuleContext a -> List (Node Expression) -> Set String -> Elm.Type.Type -> Maybe Elm.Type.Type
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


typeOfLetDeclaration : OuterModuleContext a -> Node Expression.LetDeclaration -> List ( String, Elm.Type.Type )
typeOfLetDeclaration context node =
    case Node.value node of
        Expression.LetFunction function ->
            typeOfFunctionDeclaration context function

        Expression.LetDestructuring _ _ ->
            []


typeOfFunctionDeclaration : OuterModuleContext a -> Expression.Function -> List ( String, Elm.Type.Type )
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