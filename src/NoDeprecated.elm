module NoDeprecated exposing
    ( rule
    , Configuration, checkInName
    )

{-|

@docs rule

This rule is useful to stop the spread of the usage of deprecated functions and types.

This rule is recommended to be used with `elm-review`'s suppression system (see `elm-review suppress --help`).
That way, current uses of deprecated elements won't be reported, but the rule will report new usages, in practice
allowing you to stop the bleed.

An additional benefit is that the suppressed errors will make it easy to have an overview of the number of times
deprecated elements are used and where they are located. Looking at the error reports (using `elm-review --unsuppress`
for instance) will give you the more precise problems and locations.

@docs Configuration, checkInName

REPLACEME You can configure this rule to only trigger for a specific module, function or element, and create multiple of these. TODO Mention performance
REPLACEME TODO Would this require renaming the rule maybe?

TODO Also report `.thingDeprecated` or `a.thingDeprecated`


## Fail

    a =
        Button.view_DEPRECATED "Click me!" OnClick


## When (not) to enable this rule

If you do not have deprecated elements in your project, this rule won't be useful.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoDeprecated
```

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports usages of deprecated functions and types.

    config =
        [ NoDeprecated.rule NoDeprecated.defaults
        ]

-}
rule : Configuration -> Rule
rule configuration =
    Rule.newProjectRuleSchema "NoDeprecated" initialProjectContext
        |> Rule.withDependenciesProjectVisitor (\dict ctx -> ( [], dependenciesVisitor configuration dict ctx ))
        |> Rule.withModuleVisitor (moduleVisitor configuration)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule configuration
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


initialProjectContext : ProjectContext
initialProjectContext =
    { deprecatedModules = []
    , deprecatedValues = []
    , deprecatedTypes = []
    }


type alias ProjectContext =
    { deprecatedModules : List ModuleName
    , deprecatedValues : List ( ModuleName, String )
    , deprecatedTypes : List ( ModuleName, String )
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , currentModuleName : ModuleName
    , deprecatedModules : Set ModuleName
    , deprecatedValues : Set ( ModuleName, String )
    , deprecatedTypes : Set ( ModuleName, String )
    , isModuleDeprecated : Bool
    }


fromProjectToModule : Configuration -> Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule (Configuration configuration) =
    Rule.initContextCreator
        (\metadata lookupTable projectContext ->
            let
                moduleName : ModuleName
                moduleName =
                    Rule.moduleNameFromMetadata metadata
            in
            { lookupTable = lookupTable
            , currentModuleName = moduleName
            , deprecatedModules = Set.fromList projectContext.deprecatedModules
            , deprecatedValues = Set.fromList projectContext.deprecatedValues
            , deprecatedTypes = Set.fromList projectContext.deprecatedTypes
            , isModuleDeprecated = configuration.moduleNamePredicate moduleName
            }
        )
        |> Rule.withMetadata
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { deprecatedModules =
                if moduleContext.isModuleDeprecated then
                    [ Rule.moduleNameFromMetadata metadata ]

                else
                    []
            , deprecatedValues = []
            , deprecatedTypes = []
            }
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { deprecatedModules = List.append newContext.deprecatedModules previousContext.deprecatedModules
    , deprecatedValues = List.append newContext.deprecatedValues previousContext.deprecatedValues
    , deprecatedTypes = List.append newContext.deprecatedTypes previousContext.deprecatedTypes
    }


moduleVisitor : Configuration -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor configuration schema =
    schema
        |> Rule.withCommentsVisitor (\comments context -> ( [], commentsVisitor configuration comments context ))
        |> Rule.withDeclarationListVisitor (\nodes context -> ( [], declarationListVisitor configuration nodes context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( declarationVisitor configuration node context, context ))
        |> Rule.withExpressionEnterVisitor (\node context -> ( expressionVisitor configuration node context, context ))


{-| REPLACEME
-}
type Configuration
    = Configuration
        { moduleNamePredicate : ModuleName -> Bool
        , documentationPredicate : String -> Bool
        , elementPredicate : ModuleName -> String -> Bool
        , typePredicate : ModuleName -> String -> Bool
        , recordFieldPredicate : String -> Bool
        , parameterPredicate : String -> Bool
        }


{-| REPLACEME
-}
checkInName : Configuration
checkInName =
    let
        containsDeprecated : String -> Bool
        containsDeprecated name =
            name
                |> String.toLower
                |> String.contains "deprecated"
    in
    Configuration
        { moduleNamePredicate = String.join "." >> String.toLower >> containsDeprecated
        , documentationPredicate = containsDeprecated
        , elementPredicate = \_ name -> containsDeprecated name
        , typePredicate = \_ name -> containsDeprecated name
        , recordFieldPredicate = containsDeprecated
        , parameterPredicate = containsDeprecated
        }


dependenciesVisitor : Configuration -> Dict String Review.Project.Dependency.Dependency -> ProjectContext -> ProjectContext
dependenciesVisitor configuration dict projectContext =
    let
        modules : List Elm.Docs.Module
        modules =
            dict
                |> Dict.values
                |> List.concatMap Review.Project.Dependency.modules
    in
    List.foldl
        (registerDeprecatedThings configuration)
        projectContext
        modules


registerDeprecatedThings : Configuration -> Elm.Docs.Module -> ProjectContext -> ProjectContext
registerDeprecatedThings (Configuration configuration) module_ acc =
    let
        moduleName : ModuleName
        moduleName =
            String.split "." module_.name
    in
    if configuration.documentationPredicate module_.comment then
        { deprecatedModules = moduleName :: acc.deprecatedModules
        , deprecatedValues = acc.deprecatedValues
        , deprecatedTypes = acc.deprecatedTypes
        }

    else
        let
            deprecatedAliases : List Elm.Docs.Alias
            deprecatedAliases =
                module_.aliases
                    |> List.filter (.comment >> configuration.documentationPredicate)

            deprecatedUnions : List Elm.Docs.Union
            deprecatedUnions =
                module_.unions
                    |> List.filter (.comment >> configuration.documentationPredicate)

            newValues : List ( ModuleName, String )
            newValues =
                List.concat
                    [ module_.values
                        |> List.filter (.comment >> configuration.documentationPredicate)
                        |> List.map (\value -> ( moduleName, value.name ))
                    , deprecatedUnions
                        |> List.concatMap .tags
                        |> List.map (\( name, _ ) -> ( moduleName, name ))
                    , deprecatedAliases
                        |> List.filter isRecordTypeAlias
                        |> List.map (\{ name } -> ( moduleName, name ))
                    ]
        in
        { deprecatedModules = acc.deprecatedModules
        , deprecatedValues = List.append newValues acc.deprecatedValues
        , deprecatedTypes =
            List.append
                (List.map (\{ name } -> ( moduleName, name )) deprecatedUnions)
                (List.append
                    (List.map (\{ name } -> ( moduleName, name )) deprecatedAliases)
                    acc.deprecatedValues
                )
        }


isRecordTypeAlias : Elm.Docs.Alias -> Bool
isRecordTypeAlias alias =
    case alias.tipe of
        Elm.Type.Record _ Nothing ->
            True

        _ ->
            False


commentsVisitor : Configuration -> List (Node String) -> ModuleContext -> ModuleContext
commentsVisitor (Configuration configuration) comments moduleContext =
    if moduleContext.isModuleDeprecated then
        moduleContext

    else
        case find (\(Node _ comment) -> String.startsWith "{-|" comment) comments of
            Just (Node _ moduleDocumentation) ->
                { moduleContext | isModuleDeprecated = configuration.documentationPredicate moduleDocumentation }

            Nothing ->
                moduleContext


declarationListVisitor : Configuration -> List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor configuration nodes context =
    List.foldl (registerDeclaration configuration) context nodes


registerDeclaration : Configuration -> Node Declaration -> ModuleContext -> ModuleContext
registerDeclaration (Configuration configuration) node context =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            registerFunctionDeclaration (Configuration configuration) declaration context

        Declaration.AliasDeclaration type_ ->
            registerAliasDeclaration (Configuration configuration) type_ context

        Declaration.CustomTypeDeclaration type_ ->
            case type_.documentation of
                Just (Node _ str) ->
                    if configuration.documentationPredicate str then
                        { context
                            | deprecatedValues = List.foldl (\(Node _ constructor) -> Set.insert ( [], Node.value constructor.name )) context.deprecatedValues type_.constructors
                            , deprecatedTypes = Set.insert ( [], type_.name |> Node.value ) context.deprecatedValues
                        }

                    else
                        context

                Nothing ->
                    context

        _ ->
            context


registerFunctionDeclaration : Configuration -> Expression.Function -> ModuleContext -> ModuleContext
registerFunctionDeclaration (Configuration configuration) declaration context =
    let
        name : String
        name =
            declaration.declaration |> Node.value |> .name |> Node.value
    in
    if
        configuration.elementPredicate context.currentModuleName name
            || checkDocumentation configuration.documentationPredicate declaration.documentation
    then
        registerValue name context

    else
        context


registerAliasDeclaration : Configuration -> Elm.Syntax.TypeAlias.TypeAlias -> ModuleContext -> ModuleContext
registerAliasDeclaration (Configuration configuration) type_ context =
    let
        name : String
        name =
            Node.value type_.name

        isRecordAlias : Bool
        isRecordAlias =
            case Node.value type_.typeAnnotation of
                TypeAnnotation.Record _ ->
                    True

                _ ->
                    False

        register : ModuleContext -> ModuleContext
        register ctx =
            { ctx
                | deprecatedValues =
                    if isRecordAlias then
                        Set.insert ( [], name ) ctx.deprecatedValues

                    else
                        ctx.deprecatedValues
                , deprecatedTypes = Set.insert ( [], name ) ctx.deprecatedValues
            }
    in
    if
        configuration.typePredicate context.currentModuleName name
            || checkDocumentation configuration.documentationPredicate type_.documentation
    then
        register context

    else if configuration.elementPredicate context.currentModuleName name then
        case Node.value type_.typeAnnotation of
            TypeAnnotation.Record _ ->
                registerValue name context

            _ ->
                context

    else
        context


checkDocumentation : (String -> Bool) -> Maybe (Node String) -> Bool
checkDocumentation documentationPredicate documentationNode =
    case documentationNode of
        Just (Node _ str) ->
            documentationPredicate str

        Nothing ->
            False


registerValue : String -> ModuleContext -> ModuleContext
registerValue name context =
    { context | deprecatedValues = Set.insert ( [], name ) context.deprecatedValues }


declarationVisitor : Configuration -> Node Declaration -> ModuleContext -> List (Rule.Error {})
declarationVisitor configuration node context =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            let
                signatureErrors : List (Rule.Error {})
                signatureErrors =
                    case declaration.signature of
                        Just signature ->
                            reportTypes
                                configuration
                                context
                                [ (Node.value signature).typeAnnotation ]
                                []

                        Nothing ->
                            []

                destructuringErrors : List (Rule.Error {})
                destructuringErrors =
                    reportPatterns
                        configuration
                        context
                        (declaration.declaration |> Node.value |> .arguments)
                        []
            in
            List.append destructuringErrors signatureErrors

        Declaration.CustomTypeDeclaration type_ ->
            reportTypes
                configuration
                context
                (List.concatMap (Node.value >> .arguments) type_.constructors)
                []

        Declaration.AliasDeclaration type_ ->
            reportTypes
                configuration
                context
                [ type_.typeAnnotation ]
                []

        Declaration.PortDeclaration signature ->
            reportTypes
                configuration
                context
                [ signature.typeAnnotation ]
                []

        _ ->
            []


reportLetDeclaration : Configuration -> ModuleContext -> Node Expression.LetDeclaration -> List (Rule.Error {})
reportLetDeclaration configuration context letDeclaration =
    case Node.value letDeclaration of
        Expression.LetFunction function ->
            let
                signatureErrors : List (Rule.Error {})
                signatureErrors =
                    case function.signature of
                        Just signature ->
                            reportTypes
                                configuration
                                context
                                [ (Node.value signature).typeAnnotation ]
                                []

                        Nothing ->
                            []

                destructuringErrors : List (Rule.Error {})
                destructuringErrors =
                    reportPatterns
                        configuration
                        context
                        (function.declaration |> Node.value |> .arguments)
                        []
            in
            List.append destructuringErrors signatureErrors

        Expression.LetDestructuring pattern _ ->
            reportPatterns
                configuration
                context
                [ pattern ]
                []


reportTypes : Configuration -> ModuleContext -> List (Node TypeAnnotation) -> List (Rule.Error {}) -> List (Rule.Error {})
reportTypes configuration context nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.Typed (Node range ( _, name )) args ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            case reportType configuration context range name of
                                Just err ->
                                    err :: acc

                                Nothing ->
                                    acc
                    in
                    reportTypes
                        configuration
                        context
                        (List.append args restOfNodes)
                        newAcc

                TypeAnnotation.Tupled nodesToLookAt ->
                    reportTypes configuration context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.Record recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (Node.value >> Tuple.second) recordDefinition
                    in
                    reportTypes configuration context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.GenericRecord _ recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (Node.value >> Tuple.second) (Node.value recordDefinition)
                    in
                    reportTypes configuration context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.FunctionTypeAnnotation left right ->
                    reportTypes configuration context (left :: right :: restOfNodes) acc

                _ ->
                    reportTypes configuration context restOfNodes acc


reportPatterns : Configuration -> ModuleContext -> List (Node Pattern) -> List (Rule.Error {}) -> List (Rule.Error {})
reportPatterns configuration context nodes acc =
    case nodes of
        [] ->
            acc

        pattern :: restOfNodes ->
            case Node.value pattern of
                Pattern.ParenthesizedPattern subPattern ->
                    reportPatterns
                        configuration
                        context
                        (subPattern :: restOfNodes)
                        acc

                Pattern.TuplePattern subPatterns ->
                    reportPatterns configuration context (List.append subPatterns restOfNodes) acc

                Pattern.RecordPattern fields ->
                    reportPatterns configuration
                        context
                        restOfNodes
                        (List.append (List.filterMap (reportField configuration) fields) acc)

                Pattern.UnConsPattern left right ->
                    reportPatterns configuration context (left :: right :: restOfNodes) acc

                Pattern.ListPattern subPatterns ->
                    reportPatterns configuration context (List.append subPatterns restOfNodes) acc

                Pattern.VarPattern name ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            case reportParameter configuration (Node.range pattern) name of
                                Just err ->
                                    err :: acc

                                Nothing ->
                                    acc
                    in
                    reportPatterns
                        configuration
                        context
                        restOfNodes
                        newAcc

                Pattern.NamedPattern qualifiedNameRef subPatterns ->
                    let
                        errors : List (Rule.Error {})
                        errors =
                            reportValue
                                configuration
                                context
                                (Node.range pattern)
                                (\() -> rangeForNamedPattern pattern qualifiedNameRef)
                                qualifiedNameRef.name
                    in
                    reportPatterns
                        configuration
                        context
                        (List.append subPatterns restOfNodes)
                        (List.append errors acc)

                Pattern.AsPattern subPattern name ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            case reportParameter configuration (Node.range name) (Node.value name) of
                                Just err ->
                                    err :: acc

                                Nothing ->
                                    acc
                    in
                    reportPatterns configuration context (subPattern :: restOfNodes) newAcc

                _ ->
                    reportPatterns configuration context restOfNodes acc


rangeForNamedPattern : Node a -> Pattern.QualifiedNameRef -> Range
rangeForNamedPattern (Node parentRange _) { moduleName, name } =
    let
        lengthForName : Int
        lengthForName =
            (moduleName ++ [ name ])
                |> String.join "."
                |> String.length

        patternStart : Range.Location
        patternStart =
            parentRange.start
    in
    { start = patternStart
    , end = { row = patternStart.row, column = patternStart.column + lengthForName }
    }


reportField : Configuration -> Node String -> Maybe (Rule.Error {})
reportField (Configuration configuration) field =
    if configuration.recordFieldPredicate (Node.value field) then
        Just (error (Node.range field))

    else
        Nothing


expressionVisitor : Configuration -> Node Expression -> ModuleContext -> List (Rule.Error {})
expressionVisitor configuration (Node nodeRange node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            reportValue
                configuration
                context
                nodeRange
                (always nodeRange)
                name

        Expression.LetExpression letBlock ->
            List.concatMap
                (reportLetDeclaration configuration context)
                letBlock.declarations

        Expression.CaseExpression { cases } ->
            reportPatterns
                configuration
                context
                (List.map Tuple.first cases)
                []

        Expression.RecordUpdateExpression (Node range name) _ ->
            -- TODO report deprecated fields
            reportValue
                configuration
                context
                range
                (always range)
                name

        Expression.RecordExpr _ ->
            -- TODO report deprecated fields
            []

        Expression.RecordAccess _ _ ->
            -- TODO report deprecated fields
            []

        Expression.RecordAccessFunction _ ->
            -- TODO report deprecated fields
            []

        _ ->
            []


reportValue : Configuration -> ModuleContext -> Range -> (() -> Range) -> String -> List (Rule.Error {})
reportValue (Configuration configuration) context rangeForLookupTable rangeForReport name =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable rangeForLookupTable of
        Just moduleName ->
            if
                configuration.elementPredicate moduleName name
                    || Set.member moduleName context.deprecatedModules
                    || Set.member ( moduleName, name ) context.deprecatedValues
            then
                [ error (rangeForReport ()) ]

            else
                []

        Nothing ->
            []


reportParameter : Configuration -> Range -> String -> Maybe (Rule.Error {})
reportParameter (Configuration configuration) range name =
    if configuration.parameterPredicate name then
        Just (error range)

    else
        Nothing


reportType : Configuration -> ModuleContext -> Range -> String -> Maybe (Rule.Error {})
reportType (Configuration configuration) context range name =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
        Just moduleName ->
            if
                configuration.typePredicate moduleName name
                    || Set.member moduleName context.deprecatedModules
                    || Set.member ( moduleName, name ) context.deprecatedTypes
            then
                Just (error range)

            else
                Nothing

        Nothing ->
            Nothing


error : Range -> Rule.Error {}
error range =
    Rule.error
        { message = "Found new usage of deprecated element"
        , details = [ "REPLACEME" ]
        }
        range


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.
find (\\num -> num > 5) [2, 4, 6, 8] == Just 6
-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest
