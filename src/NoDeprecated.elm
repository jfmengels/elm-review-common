module NoDeprecated exposing
    ( rule
    , Configuration, checkInName
    , deprecateUsageOfPackages
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
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
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
        |> Rule.withDependenciesProjectVisitor (dependenciesVisitor configuration)
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
    , deprecatedElements = []
    }


type alias ProjectContext =
    { deprecatedModules : List ModuleName
    , deprecatedElements : List ( ModuleName, String )
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , currentModuleName : ModuleName
    , deprecatedModules : Set ModuleName
    , deprecatedElements : Set ( ModuleName, String )
    , isModuleDeprecated : Bool
    , localDeprecatedElements : List ( ModuleName, String )
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
            , deprecatedElements = Set.fromList projectContext.deprecatedElements
            , isModuleDeprecated = configuration.moduleNamePredicate moduleName
            , localDeprecatedElements = []
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
            , deprecatedElements = moduleContext.localDeprecatedElements
            }
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { deprecatedModules = List.append newContext.deprecatedModules previousContext.deprecatedModules
    , deprecatedElements = List.append newContext.deprecatedElements previousContext.deprecatedElements
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
        , recordFieldPredicate : String -> Bool
        , parameterPredicate : String -> Bool
        , deprecatedDependencies : List String
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
        , recordFieldPredicate = containsDeprecated
        , parameterPredicate = containsDeprecated
        , deprecatedDependencies = []
        }


deprecateUsageOfPackages : List String -> Configuration -> Configuration
deprecateUsageOfPackages dependencyNames (Configuration configuration) =
    Configuration { configuration | deprecatedDependencies = List.append configuration.deprecatedDependencies dependencyNames }


dependenciesVisitor : Configuration -> Dict String Review.Project.Dependency.Dependency -> ProjectContext -> ( List (Rule.Error global), ProjectContext )
dependenciesVisitor (Configuration configuration) dict projectContext =
    let
        newContext : ProjectContext
        newContext =
            List.foldl
                (\( packageName, dependency ) acc ->
                    let
                        modules : List Elm.Docs.Module
                        modules =
                            Review.Project.Dependency.modules dependency
                    in
                    if List.member packageName configuration.deprecatedDependencies then
                        { acc | deprecatedModules = List.append (List.map (.name >> String.split ".") modules) acc.deprecatedModules }

                    else
                        List.foldl
                            (registerDeprecatedThings (Configuration configuration))
                            acc
                            modules
                )
                projectContext
                (Dict.toList dict)

        unknownDependenciesErrors : List (Rule.Error global)
        unknownDependenciesErrors =
            configuration.deprecatedDependencies
                |> List.filter (\name -> not (Dict.member name dict))
                |> List.map
                    (\name ->
                        Rule.globalError
                            { message = "Could not find package `" ++ name ++ "`"
                            , details =
                                [ "You marked this package as deprecated, but I can't find it in your dependencies."
                                , "It could be a typo, or maybe you've successfully removed it from your project?"
                                ]
                            }
                    )
    in
    ( unknownDependenciesErrors, newContext )


registerDeprecatedThings : Configuration -> Elm.Docs.Module -> ProjectContext -> ProjectContext
registerDeprecatedThings (Configuration configuration) module_ acc =
    let
        moduleName : ModuleName
        moduleName =
            String.split "." module_.name
    in
    if configuration.documentationPredicate module_.comment then
        { deprecatedModules = moduleName :: acc.deprecatedModules
        , deprecatedElements = acc.deprecatedElements
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
                        |> List.map (\{ name } -> ( moduleName, name ))
                    , deprecatedUnions
                        |> List.concatMap .tags
                        |> List.map (\( name, _ ) -> ( moduleName, name ))
                    , deprecatedAliases
                        |> List.map (\{ name } -> ( moduleName, name ))
                    ]
        in
        { deprecatedModules = acc.deprecatedModules
        , deprecatedElements = List.append newValues acc.deprecatedElements
        }


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
registerDeclaration configuration node context =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            registerFunctionDeclaration configuration declaration context

        Declaration.AliasDeclaration type_ ->
            registerAliasDeclaration configuration type_ context

        Declaration.CustomTypeDeclaration type_ ->
            registerCustomTypeDeclaration configuration type_ context

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
        registerElement name context

    else
        context


registerAliasDeclaration : Configuration -> Elm.Syntax.TypeAlias.TypeAlias -> ModuleContext -> ModuleContext
registerAliasDeclaration (Configuration configuration) type_ context =
    let
        name : String
        name =
            Node.value type_.name
    in
    if
        configuration.elementPredicate context.currentModuleName name
            || checkDocumentation configuration.documentationPredicate type_.documentation
    then
        registerElement name context

    else
        context


registerCustomTypeDeclaration : Configuration -> Elm.Syntax.Type.Type -> ModuleContext -> ModuleContext
registerCustomTypeDeclaration (Configuration configuration) type_ context =
    let
        name : String
        name =
            Node.value type_.name

        register : ModuleContext -> ModuleContext
        register ctx =
            List.foldl
                (\(Node _ constructor) -> registerElement (Node.value constructor.name))
                (registerElement name ctx)
                type_.constructors
    in
    if
        configuration.elementPredicate context.currentModuleName name
            || checkDocumentation configuration.documentationPredicate type_.documentation
    then
        register context

    else
        List.foldl
            (\(Node _ constructor) ctx ->
                if configuration.elementPredicate ctx.currentModuleName (Node.value constructor.name) then
                    registerElement (Node.value constructor.name) ctx

                else
                    ctx
            )
            context
            type_.constructors


checkDocumentation : (String -> Bool) -> Maybe (Node String) -> Bool
checkDocumentation documentationPredicate documentationNode =
    case documentationNode of
        Just (Node _ str) ->
            documentationPredicate str

        Nothing ->
            False


registerElement : String -> ModuleContext -> ModuleContext
registerElement name context =
    { context
        | deprecatedElements = Set.insert ( [], name ) context.deprecatedElements
        , localDeprecatedElements = ( context.currentModuleName, name ) :: context.localDeprecatedElements
    }


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
                context
                (List.concatMap (Node.value >> .arguments) type_.constructors)
                []

        Declaration.AliasDeclaration type_ ->
            reportTypes
                context
                [ type_.typeAnnotation ]
                []

        Declaration.PortDeclaration signature ->
            reportTypes
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


reportTypes : ModuleContext -> List (Node TypeAnnotation) -> List (Rule.Error {}) -> List (Rule.Error {})
reportTypes context nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.Typed (Node range ( _, name )) args ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            case reportElementAsMaybe context range name of
                                Just err ->
                                    err :: acc

                                Nothing ->
                                    acc
                    in
                    reportTypes
                        context
                        (List.append args restOfNodes)
                        newAcc

                TypeAnnotation.Tupled nodesToLookAt ->
                    reportTypes context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.Record recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (Node.value >> Tuple.second) recordDefinition
                    in
                    reportTypes context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.GenericRecord _ recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (Node.value >> Tuple.second) (Node.value recordDefinition)
                    in
                    reportTypes context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.FunctionTypeAnnotation left right ->
                    reportTypes context (left :: right :: restOfNodes) acc

                _ ->
                    reportTypes context restOfNodes acc


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
                            reportElementAsList
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
            reportElementAsList
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
            reportElementAsList
                context
                range
                (always range)
                name

        Expression.RecordAccess _ field ->
            case reportField configuration field of
                Just err ->
                    [ err ]

                Nothing ->
                    []

        Expression.RecordAccessFunction fieldName ->
            case reportField configuration (Node nodeRange fieldName) of
                Just err ->
                    [ err ]

                Nothing ->
                    []

        _ ->
            []


reportElementAsList : ModuleContext -> Range -> (() -> Range) -> String -> List (Rule.Error {})
reportElementAsList context rangeForLookupTable rangeForReport name =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable rangeForLookupTable of
        Just moduleName ->
            if
                Set.member moduleName context.deprecatedModules
                    || Set.member ( moduleName, name ) context.deprecatedElements
            then
                [ error (rangeForReport ()) ]

            else
                []

        Nothing ->
            []


reportElementAsMaybe : ModuleContext -> Range -> String -> Maybe (Rule.Error {})
reportElementAsMaybe context range name =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
        Just moduleName ->
            if
                Set.member moduleName context.deprecatedModules
                    || Set.member ( moduleName, name ) context.deprecatedElements
            then
                Just (error range)

            else
                Nothing

        Nothing ->
            Nothing


reportParameter : Configuration -> Range -> String -> Maybe (Rule.Error {})
reportParameter (Configuration configuration) range name =
    if configuration.parameterPredicate name then
        Just (error range)

    else
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
