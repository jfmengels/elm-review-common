module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbids importing everything from a module.

When you import everything from a module, it becomes harder to know where a function
or a type comes from. The official guide even
[recommends against importing everything](https://guide.elm-lang.org/webapps/modules.html#using-modules).

    config =
        [ NoImportingEverything.rule []
        ]

Teams often have an agreement on the list of imports from which it is okay to expose everything, so
you can configure a list of exceptions.

    config =
        [ NoImportingEverything.rule [ "Html", "Some.Module" ]
        ]


## Fail

    import A exposing (..)
    import A as B exposing (..)


## Success

    import A as B exposing (B(..), C, d)

    -- If configured with `[ "Html" ]`
    import Html exposing (..)


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoImportingEverything
```

-}
rule : List String -> Rule
rule exceptions =
    Rule.newProjectRuleSchema "NoImportingEverything" initialContext
        |> Rule.withModuleVisitor (moduleVisitor exceptions)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { constructorToType : Dict ModuleName (Dict String String)
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , importsExposingAll : Dict ModuleName ImportExposingAll
    , exposedTypes : ExposedTypes
    , constructorToType : Dict ModuleName (Dict String String)
    , localConstructorToType : Dict String String
    }


type ExposedTypes
    = ExposesAll
    | ExposesConstructorsOf (Set String)


type alias ImportExposingAll =
    { node : Node Import
    , exposingRange : Range
    , values : Set String
    }


initialContext : ProjectContext
initialContext =
    { constructorToType = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            , importsExposingAll = Dict.empty
            , exposedTypes = ExposesAll
            , constructorToType = projectContext.constructorToType
            , localConstructorToType = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName moduleContext ->
            { constructorToType =
                if Dict.isEmpty moduleContext.localConstructorToType then
                    Dict.empty

                else
                    Dict.singleton moduleName moduleContext.localConstructorToType
            }
        )
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { constructorToType = Dict.union newContext.constructorToType previousContext.constructorToType
    }


moduleVisitor :
    List String
    -> Rule.ModuleRuleSchema schemaState ModuleContext
    -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor exceptions schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List empty, ModuleContext )
moduleDefinitionVisitor node context =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            ( [], { context | exposedTypes = ExposesAll } )

        Exposing.Explicit list ->
            let
                constructors : Set String
                constructors =
                    List.foldl
                        (\(Node _ item) acc ->
                            case item of
                                Exposing.TypeExpose { name } ->
                                    Set.insert name acc

                                _ ->
                                    acc
                        )
                        Set.empty
                        list
            in
            ( [], { context | exposedTypes = ExposesConstructorsOf constructors } )


exceptionsToSet : List String -> Set ModuleName
exceptionsToSet exceptions =
    List.foldl
        (\moduleName acc -> Set.insert (String.split "." moduleName) acc)
        Set.empty
        exceptions


importVisitor : Set ModuleName -> Node Import -> ModuleContext -> ( List (Error nothing), ModuleContext )
importVisitor exceptions node context =
    let
        moduleName : ModuleName
        moduleName =
            importModuleName node
    in
    if Set.member moduleName exceptions then
        ( [], context )

    else
        case
            Node.value node
                |> .exposingList
                |> Maybe.map Node.value
        of
            Just (Exposing.All allRange) ->
                ( []
                , { context
                    | importsExposingAll =
                        Dict.insert moduleName
                            { node = node
                            , exposingRange = allRange
                            , values = Set.empty
                            }
                            context.importsExposingAll
                  }
                )

            _ ->
                ( [], context )


declarationVisitor : Node Declaration -> ModuleContext -> ( List empty, ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { signature } ->
            -- TODO Get constructors from patterns
            case signature of
                Just (Node _ { typeAnnotation }) ->
                    ( [], visitTypeAnnotation [ typeAnnotation ] context )

                Nothing ->
                    ( [], context )

        Declaration.CustomTypeDeclaration type_ ->
            let
                typeName : String
                typeName =
                    Node.value type_.name

                isConstructorsExposed_ : Bool
                isConstructorsExposed_ =
                    isConstructorsExposed typeName context

                newContext : ModuleContext
                newContext =
                    List.foldl
                        (\(Node _ constructor) ctx ->
                            visitTypeAnnotation constructor.arguments
                                (if isConstructorsExposed_ then
                                    { ctx | localConstructorToType = Dict.insert (Node.value constructor.name) typeName ctx.localConstructorToType }

                                 else
                                    ctx
                                )
                        )
                        context
                        type_.constructors
            in
            ( [], newContext )

        _ ->
            ( [], context )


visitTypeAnnotation : List (Node TypeAnnotation) -> ModuleContext -> ModuleContext
visitTypeAnnotation typeAnnotations context =
    case typeAnnotations of
        [] ->
            context

        typeAnnotation :: rest ->
            case Node.value typeAnnotation of
                TypeAnnotation.Typed (Node range ( [], name )) subTypes ->
                    let
                        newContext : ModuleContext
                        newContext =
                            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                                Just moduleName ->
                                    useImportedType moduleName name context

                                Nothing ->
                                    context
                    in
                    visitTypeAnnotation (subTypes ++ rest) newContext

                TypeAnnotation.Typed _ subTypes ->
                    visitTypeAnnotation (subTypes ++ rest) context

                TypeAnnotation.Tupled subTypes ->
                    visitTypeAnnotation (subTypes ++ rest) context

                TypeAnnotation.Record fields ->
                    visitTypeAnnotation (List.map (Node.value >> Tuple.second) fields ++ rest) context

                TypeAnnotation.GenericRecord _ (Node _ fields) ->
                    visitTypeAnnotation (List.map (Node.value >> Tuple.second) fields ++ rest) context

                TypeAnnotation.FunctionTypeAnnotation fn arg ->
                    visitTypeAnnotation (fn :: arg :: rest) context

                _ ->
                    visitTypeAnnotation rest context


isConstructorsExposed : String -> { context | exposedTypes : ExposedTypes } -> Bool
isConstructorsExposed name context =
    case context.exposedTypes of
        ExposesAll ->
            True

        ExposesConstructorsOf set ->
            Set.member name set


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error nothing), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                Just moduleName ->
                    ( []
                    , useImportedValue moduleName name context
                    )

                Nothing ->
                    ( [], context )

        Expression.LetExpression { declarations } ->
            List.foldl
                (\(Node _ declaration) ctx ->
                    case declaration of
                        Expression.LetFunction { signature } ->
                            case signature of
                                Just (Node _ { typeAnnotation }) ->
                                    visitTypeAnnotation [ typeAnnotation ] ctx

                                Nothing ->
                                    ctx

                        Expression.LetDestructuring _ _ ->
                            ctx
                )
                context
                declarations
                |> Tuple.pair []

        _ ->
            ( [], context )


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation context =
    context.importsExposingAll
        |> Dict.values
        |> List.map importError


importError : ImportExposingAll -> Error {}
importError ({ exposingRange } as importExposingAll) =
    Rule.errorWithFix
        { message = "Prefer listing what you wish to import and/or using qualified imports"
        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
        }
        { start = { row = exposingRange.start.row, column = exposingRange.start.column - 1 }
        , end = { row = exposingRange.end.row, column = exposingRange.end.column + 1 }
        }
        [ exposingFix importExposingAll ]


useImportedValue : ModuleName -> String -> ModuleContext -> ModuleContext
useImportedValue moduleName name context =
    case Dict.get moduleName context.importsExposingAll of
        Nothing ->
            context

        Just importExposingAll ->
            let
                insertionName : String
                insertionName =
                    case Dict.get moduleName context.constructorToType |> Maybe.andThen (Dict.get name) of
                        Just typeName ->
                            typeName ++ "(..)"

                        Nothing ->
                            name

                importsExposingAll : Dict ModuleName ImportExposingAll
                importsExposingAll =
                    Dict.insert
                        moduleName
                        { importExposingAll | values = Set.insert insertionName importExposingAll.values }
                        context.importsExposingAll
            in
            { context | importsExposingAll = importsExposingAll }


useImportedType : ModuleName -> String -> ModuleContext -> ModuleContext
useImportedType moduleName typeName context =
    case Dict.get moduleName context.importsExposingAll of
        Nothing ->
            context

        Just importExposingAll ->
            let
                importsExposingAll : Dict ModuleName ImportExposingAll
                importsExposingAll =
                    Dict.insert
                        moduleName
                        { importExposingAll | values = Set.insert typeName importExposingAll.values }
                        context.importsExposingAll
            in
            { context | importsExposingAll = importsExposingAll }


importModuleName : Node Import -> ModuleName
importModuleName node =
    node
        |> Node.value
        |> .moduleName
        |> Node.value


exposingFix : ImportExposingAll -> Fix
exposingFix { node, exposingRange, values } =
    case Set.toList values of
        [] ->
            removeExposingFix node

        list ->
            replaceExposingFix list exposingRange


removeExposingFix : Node Import -> Fix
removeExposingFix node =
    let
        startRange : Range
        startRange =
            case node |> Node.value |> .moduleAlias of
                Just aliasNode ->
                    aliasNode |> Node.range

                Nothing ->
                    node |> Node.value |> .moduleName |> Node.range
    in
    Fix.replaceRangeBy { start = startRange.end, end = (Node.range node).end } ""


replaceExposingFix : List String -> Range -> Fix
replaceExposingFix values range =
    Fix.replaceRangeBy range (String.join ", " values)
