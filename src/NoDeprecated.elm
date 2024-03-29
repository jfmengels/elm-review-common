module NoDeprecated exposing
    ( rule
    , Configuration, defaults, dependencies, withExceptionsForElements
    )

{-|

@docs rule

This rule is useful to stop the spread of the usage of deprecated values and types.

This rule is recommended to be used with `elm-review`'s suppression system (see `elm-review suppress --help`).
That way, current uses of deprecated elements won't be reported, but the rule will report new usages, in practice
allowing you to stop the bleed.


## Fail

    import DeprecatedModule

    a =
        DeprecatedModule.view "..."

    b =
        Button.view_DEPRECATED "Click me!" OnClick


## Tagging recommendations

I recommend making it extra explicit when deprecating elements in your application code, for instance by renaming
them to include "deprecated" in their name, or in their module name for modules.

That way, it will be very clear for you and your teammates when you're using something that is deprecated, even in
Git diffs.

For packages, renaming something is a breaking change so that is not a viable option (if it is, remove the function and
release a new major version). Instead, what you can do is to start a line in your module/value/type's documentation
with `@deprecated`. There is no official nor conventional approach around deprecation in the Elm community, but this may
be a good start. But definitely pitch in the discussion around making a standard!
(I'll put a link here soon. If I haven't, please remind me!)

For both application and packages, when you deprecate something, I highly recommend documenting (in the most appropriate
location) **why it is deprecated** but especially **what alternatives should be used** or explored. It can be frustrating to
learn that something is deprecated without an explanation or any guidance on what to use instead.

It is absolutely fine to suppress **more** things. While it's normal to want to suppress as few problems as possible,
identifying technical debt is important, and that is exactly what this rule can help with. Your efforts on reducing the
number of deprecated usages should not be a hard goal, and it should be done in parallel of identifying technical debt.


## Tackling reported issues

As mentioned before, this rule is recommended to be used with `elm-review`'s suppression system. One of its benefits,
is that the number of usages will be tallied per file in the `<review>/suppressed/NoDeprecated.json` file. You can look
at it, and decide to tackle one file over another based on how many problems are in the file and how you prefer tackling
these issues.

While tackling issues file by file might work for some cases, sometimes it is nicer to organize work based on how often
a deprecated function/type is used. For instance, you might want to look at the functions that are used only once or twice,
or look at the functions that are the most widespread in your codebase.

To get this point of view, you can run this rule as an insight rule:

```bash
elm-review --report=json --extract --rules NoDeprecated | jq -r '.extracts.NoDeprecated'
```

which will yield a result like the following:

```json
{
  "Some.Deprecated.Module": {
    "total": 28,
    "isModuleDeprecated": true,
    "usages": {
      "someFunction": 20,
      "someType": 8
    }
  },
  "Some.Module": {
    "total": 1,
    "isModuleDeprecated": false,
    "usages": {
      "someDeprecatedFunction": 1
    }
  }
}
```


## Configure

@docs Configuration, defaults, dependencies, withExceptionsForElements


## When (not) to enable this rule

If you do not have deprecated elements in your project, this rule won't be useful.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoDeprecated
```

-}

{-
   TODO Report when using deprecated module aliases
   TODO Add an exception for the rule itself
   TODO Report when using exceptions that could not be found
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Encode as Encode
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports usages of deprecated values and types.

    config =
        [ NoDeprecated.rule NoDeprecated.defaults
        ]

-}
rule : Configuration -> Rule
rule configuration =
    case createElementPredicate configuration of
        Ok elementPredicate ->
            let
                stableConfiguration : StableConfiguration
                stableConfiguration =
                    userConfigurationToStableConfiguration configuration elementPredicate
            in
            Rule.newProjectRuleSchema "NoDeprecated" initialProjectContext
                |> Rule.withDirectDependenciesProjectVisitor (dependenciesVisitor stableConfiguration)
                |> Rule.withModuleVisitor (moduleVisitor stableConfiguration)
                |> Rule.withModuleContextUsingContextCreator
                    { fromProjectToModule = fromProjectToModule stableConfiguration
                    , fromModuleToProject = fromModuleToProject
                    , foldProjectContexts = foldProjectContexts
                    }
                |> Rule.withContextFromImportedModules
                |> Rule.withDataExtractor dataExtractor
                |> Rule.fromProjectRuleSchema

        Err faultyNames ->
            Rule.configurationError "NoDeprecated"
                { message = "Invalid exceptions provided in the configuration"
                , details =
                    [ "The names provided to the withExceptionsForElements function should look like 'Some.Module.value' or 'MyModule.Type', which wasn't the case for the following types:"
                    , faultyNames
                        |> List.map (\str -> " - " ++ str)
                        |> String.join "\n"
                    ]
                }


type alias ProjectContext =
    { deprecatedModules : Dict ModuleName DeprecationReason
    , deprecatedElements : List ( ModuleName, String )
    , usages : Dict ( ModuleName, String ) Int
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { deprecatedModules = Dict.empty
    , deprecatedElements = []
    , usages = Dict.empty
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , currentModuleName : ModuleName
    , deprecatedModules : Dict ModuleName DeprecationReason
    , deprecatedElements : Set ( ModuleName, String )
    , isModuleDeprecated : Bool
    , localDeprecatedElements : List ( ModuleName, String )
    , usages : List DeprecatedElementUsage
    }


type DeprecationReason
    = DeprecatedModule
    | DeprecatedDependency


fromProjectToModule : StableConfiguration -> Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule (StableConfiguration configuration) =
    Rule.initContextCreator
        (\metadata lookupTable projectContext ->
            let
                moduleName : ModuleName
                moduleName =
                    Rule.moduleNameFromMetadata metadata
            in
            { lookupTable = lookupTable
            , currentModuleName = moduleName
            , deprecatedModules = projectContext.deprecatedModules
            , deprecatedElements = Set.fromList projectContext.deprecatedElements
            , isModuleDeprecated = configuration.moduleNamePredicate moduleName
            , localDeprecatedElements = []
            , usages = []
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
                    Dict.singleton (Rule.moduleNameFromMetadata metadata) DeprecatedModule

                else
                    Dict.empty
            , deprecatedElements = moduleContext.localDeprecatedElements
            , usages =
                List.foldl
                    (\{ moduleName, name } acc ->
                        let
                            key : ( ModuleName, String )
                            key =
                                ( moduleName, name )

                            count : Int
                            count =
                                Dict.get key acc |> Maybe.withDefault 0
                        in
                        Dict.insert key (count + 1) acc
                    )
                    Dict.empty
                    moduleContext.usages
            }
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { deprecatedModules = Dict.union newContext.deprecatedModules previousContext.deprecatedModules
    , deprecatedElements = newContext.deprecatedElements ++ previousContext.deprecatedElements
    , usages =
        Dict.foldl
            (\key countForNew acc ->
                let
                    count : Int
                    count =
                        Dict.get key previousContext.usages |> Maybe.withDefault 0
                in
                Dict.insert key (countForNew + count) acc
            )
            previousContext.usages
            newContext.usages
    }


moduleVisitor : StableConfiguration -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor configuration schema =
    schema
        |> Rule.withModuleDocumentationVisitor (\moduleDocumentation context -> ( [], moduleDocumentationVisitor configuration moduleDocumentation context ))
        |> Rule.withDeclarationListVisitor (\nodes context -> ( [], declarationListVisitor configuration nodes context ))
        |> Rule.withDeclarationEnterVisitor (declarationVisitor configuration)
        |> Rule.withExpressionEnterVisitor (expressionVisitor configuration)


{-| Configuration for the rule.

Create one using [`defaults`](#defaults), then change it using functions like [`dependencies`](#dependencies) and
[`withExceptionsForElements`](#withExceptionsForElements).

-}
type Configuration
    = Configuration
        { moduleNamePredicate : ModuleName -> Bool
        , documentationPredicate : String -> Bool
        , elementPredicate : ModuleName -> String -> Bool
        , exceptionsForElements : List String
        , recordFieldPredicate : String -> Bool
        , parameterPredicate : String -> Bool
        , deprecatedDependencies : List String
        }


type StableConfiguration
    = StableConfiguration
        { moduleNamePredicate : ModuleName -> Bool
        , documentationPredicate : String -> Bool
        , elementPredicate : ModuleName -> String -> Bool
        , recordFieldPredicate : String -> Bool
        , parameterPredicate : String -> Bool
        , deprecatedDependencies : List String
        }


userConfigurationToStableConfiguration : Configuration -> (ModuleName -> String -> Bool) -> StableConfiguration
userConfigurationToStableConfiguration (Configuration configuration) elementPredicate =
    StableConfiguration
        { moduleNamePredicate = configuration.moduleNamePredicate
        , documentationPredicate = configuration.documentationPredicate
        , elementPredicate = elementPredicate
        , recordFieldPredicate = configuration.recordFieldPredicate
        , parameterPredicate = configuration.parameterPredicate
        , deprecatedDependencies = configuration.deprecatedDependencies
        }


createElementPredicate : Configuration -> Result (List String) (ModuleName -> String -> Bool)
createElementPredicate (Configuration configuration) =
    if List.isEmpty configuration.exceptionsForElements then
        Ok
            (\moduleName name ->
                configuration.elementPredicate moduleName name
            )

    else
        case parseNames configuration.exceptionsForElements of
            Ok exceptionsForElements ->
                Ok
                    (\moduleName name ->
                        configuration.elementPredicate moduleName name
                            && not (Set.member ( moduleName, name ) exceptionsForElements)
                    )

            Err faultyNames ->
                Err faultyNames


parseNames : List String -> Result (List String) (Set ( ModuleName, String ))
parseNames strings =
    let
        parsedNames : List (Result String ( ModuleName, String ))
        parsedNames =
            List.map isValidName strings

        invalidNames : List String
        invalidNames =
            List.filterMap
                (\result ->
                    case result of
                        Err typeName ->
                            Just typeName

                        Ok _ ->
                            Nothing
                )
                parsedNames
    in
    if List.isEmpty invalidNames then
        parsedNames
            |> List.filterMap Result.toMaybe
            |> Set.fromList
            |> Ok

    else
        Err invalidNames


isValidName : String -> Result String ( ModuleName, String )
isValidName name =
    case List.reverse <| String.split "." name of
        functionName :: moduleName :: restOfModuleName ->
            Ok ( List.reverse (moduleName :: restOfModuleName), functionName )

        _ ->
            Err name


{-| Default configuration.

By default are considered as deprecated:

  - Values / types / modules that contain "deprecated" (case insensitive) in their name.
  - Values / types / modules whose documentation comment has a line starting with "@deprecated" or (for better visibility) "\*\*@deprecated"
  - Values / types from modules that are considered as deprecated

Configure this further using functions like [`dependencies`](#dependencies) and
[`withExceptionsForElements`](#withExceptionsForElements).

-}
defaults : Configuration
defaults =
    let
        containsDeprecated : String -> Bool
        containsDeprecated name =
            name
                |> String.toLower
                |> String.contains "deprecated"

        documentationPredicate : String -> Bool
        documentationPredicate doc =
            doc
                |> String.dropLeft 3
                |> String.lines
                |> List.any
                    (\rawLine ->
                        let
                            line : String
                            line =
                                String.trimLeft rawLine
                        in
                        String.startsWith "@deprecated" line
                            || String.startsWith "**@deprecated" line
                    )
    in
    Configuration
        { moduleNamePredicate = \moduleName -> containsDeprecated (String.join "." moduleName)
        , documentationPredicate = documentationPredicate
        , elementPredicate = \_ name -> containsDeprecated name
        , exceptionsForElements = []
        , recordFieldPredicate = containsDeprecated
        , parameterPredicate = containsDeprecated
        , deprecatedDependencies = []
        }


{-| Mark one or more dependencies as deprecated.

    config =
        [ NoDeprecated.defaults
            |> NoDeprecated.dependencies [ "jfmengels/some-deprecated-dependency" ]
            |> NoDeprecated.rule
        ]

Every usage of something defined in that dependency in the project's code wil be reported.

-}
dependencies : List String -> Configuration -> Configuration
dependencies dependencyNames (Configuration configuration) =
    Configuration { configuration | deprecatedDependencies = configuration.deprecatedDependencies ++ dependencyNames }


{-| Add exceptions for the reporting elements. This can for instance be used for values and that
contain "deprecated" in their name without actually being deprecated.

    config =
        [ NoDeprecated.defaults
            |> NoDeprecated.withExceptionsForElements [ "SomeModule.listOfDeprecatedFunctions" ]
            |> NoDeprecated.rule
        ]

-}
withExceptionsForElements : List String -> Configuration -> Configuration
withExceptionsForElements exceptionsForElements (Configuration configuration) =
    Configuration { configuration | exceptionsForElements = exceptionsForElements ++ configuration.exceptionsForElements }


type alias DeprecatedElementUsage =
    { moduleName : ModuleName
    , name : String
    , origin : Origin
    , range : Range
    }


toError : DeprecatedElementUsage -> Rule.Error {}
toError deprecatedElementUsage =
    error deprecatedElementUsage.origin deprecatedElementUsage.range


dependenciesVisitor : StableConfiguration -> Dict String Review.Project.Dependency.Dependency -> ProjectContext -> ( List (Rule.Error global), ProjectContext )
dependenciesVisitor (StableConfiguration configuration) dict projectContext =
    let
        newContext : ProjectContext
        newContext =
            Dict.foldl
                (\packageName dependency acc ->
                    let
                        modules : List Elm.Docs.Module
                        modules =
                            Review.Project.Dependency.modules dependency
                    in
                    if List.member packageName configuration.deprecatedDependencies then
                        { acc
                            | deprecatedModules =
                                List.foldl
                                    (\{ name } subAcc -> Dict.insert (String.split "." name) DeprecatedDependency subAcc)
                                    acc.deprecatedModules
                                    modules
                        }

                    else
                        List.foldl
                            (registerDeprecatedThings (StableConfiguration configuration))
                            acc
                            modules
                )
                projectContext
                dict

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


registerDeprecatedThings : StableConfiguration -> Elm.Docs.Module -> ProjectContext -> ProjectContext
registerDeprecatedThings (StableConfiguration configuration) module_ acc =
    let
        moduleName : ModuleName
        moduleName =
            String.split "." module_.name
    in
    if configuration.documentationPredicate module_.comment then
        { deprecatedModules = Dict.insert moduleName DeprecatedModule acc.deprecatedModules
        , deprecatedElements = acc.deprecatedElements
        , usages = acc.usages
        }

    else
        let
            commentIndicatesDeprecation : { a | comment : String } -> Bool
            commentIndicatesDeprecation { comment } =
                configuration.documentationPredicate comment

            deprecatedAliases : List Elm.Docs.Alias
            deprecatedAliases =
                module_.aliases
                    |> List.filter commentIndicatesDeprecation

            deprecatedUnions : List Elm.Docs.Union
            deprecatedUnions =
                module_.unions
                    |> List.filter commentIndicatesDeprecation

            newValues : List ( ModuleName, String )
            newValues =
                List.concat
                    [ module_.values
                        |> List.filter commentIndicatesDeprecation
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
        , deprecatedElements = newValues ++ acc.deprecatedElements
        , usages = acc.usages
        }


moduleDocumentationVisitor : StableConfiguration -> Maybe (Node String) -> ModuleContext -> ModuleContext
moduleDocumentationVisitor (StableConfiguration configuration) maybeModuleDocumentation moduleContext =
    if moduleContext.isModuleDeprecated then
        moduleContext

    else
        case maybeModuleDocumentation of
            Just (Node _ moduleDocumentation) ->
                { moduleContext | isModuleDeprecated = configuration.documentationPredicate moduleDocumentation }

            Nothing ->
                moduleContext


declarationListVisitor : StableConfiguration -> List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor configuration nodes context =
    if context.isModuleDeprecated then
        context

    else
        List.foldl (registerDeclaration configuration) context nodes


registerDeclaration : StableConfiguration -> Node Declaration -> ModuleContext -> ModuleContext
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


registerFunctionDeclaration : StableConfiguration -> Expression.Function -> ModuleContext -> ModuleContext
registerFunctionDeclaration (StableConfiguration configuration) declaration context =
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


registerAliasDeclaration : StableConfiguration -> Elm.Syntax.TypeAlias.TypeAlias -> ModuleContext -> ModuleContext
registerAliasDeclaration (StableConfiguration configuration) type_ context =
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


registerCustomTypeDeclaration : StableConfiguration -> Elm.Syntax.Type.Type -> ModuleContext -> ModuleContext
registerCustomTypeDeclaration (StableConfiguration configuration) type_ context =
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
        | deprecatedElements = Set.insert ( context.currentModuleName, name ) context.deprecatedElements
        , localDeprecatedElements = ( context.currentModuleName, name ) :: context.localDeprecatedElements
    }


declarationVisitor : StableConfiguration -> Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor configuration node context =
    let
        usages : List DeprecatedElementUsage
        usages =
            declarationVisitorHelp configuration node context
    in
    ( List.map toError usages, { context | usages = usages ++ context.usages } )


declarationVisitorHelp : StableConfiguration -> Node Declaration -> ModuleContext -> List DeprecatedElementUsage
declarationVisitorHelp configuration node context =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            let
                signatureErrors : List DeprecatedElementUsage
                signatureErrors =
                    case declaration.signature of
                        Just signature ->
                            reportTypes
                                context
                                [ (Node.value signature).typeAnnotation ]
                                []

                        Nothing ->
                            []
            in
            reportPatterns
                configuration
                context
                (declaration.declaration |> Node.value |> .arguments)
                signatureErrors

        Declaration.CustomTypeDeclaration type_ ->
            reportTypes
                context
                (List.concatMap (\(Node _ { arguments }) -> arguments) type_.constructors)
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


reportLetDeclarations : StableConfiguration -> ModuleContext -> List (Node Expression.LetDeclaration) -> List DeprecatedElementUsage -> List DeprecatedElementUsage
reportLetDeclarations configuration context letDeclarations acc =
    case letDeclarations of
        [] ->
            acc

        letDeclaration :: rest ->
            reportLetDeclarations
                configuration
                context
                rest
                (reportLetDeclaration configuration context letDeclaration acc)


reportLetDeclaration : StableConfiguration -> ModuleContext -> Node Expression.LetDeclaration -> List DeprecatedElementUsage -> List DeprecatedElementUsage
reportLetDeclaration configuration context letDeclaration acc =
    case Node.value letDeclaration of
        Expression.LetFunction function ->
            let
                signatureErrors : List DeprecatedElementUsage
                signatureErrors =
                    case function.signature of
                        Just signature ->
                            reportTypes
                                context
                                [ (Node.value signature).typeAnnotation ]
                                acc

                        Nothing ->
                            acc
            in
            reportPatterns
                configuration
                context
                (function.declaration |> Node.value |> .arguments)
                signatureErrors

        Expression.LetDestructuring pattern _ ->
            reportPatterns
                configuration
                context
                [ pattern ]
                acc


reportTypes : ModuleContext -> List (Node TypeAnnotation) -> List DeprecatedElementUsage -> List DeprecatedElementUsage
reportTypes context nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.Typed (Node range ( _, name )) args ->
                    reportTypes
                        context
                        (args ++ restOfNodes)
                        (maybeCons (reportElementAsMaybe context range name) acc)

                TypeAnnotation.Tupled nodesToLookAt ->
                    reportTypes context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.Record recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (\(Node _ ( _, fieldValue )) -> fieldValue) recordDefinition
                    in
                    reportTypes context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.GenericRecord _ recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (\(Node _ ( _, fieldValue )) -> fieldValue) (Node.value recordDefinition)
                    in
                    reportTypes context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.FunctionTypeAnnotation left right ->
                    reportTypes context (left :: right :: restOfNodes) acc

                _ ->
                    reportTypes context restOfNodes acc


reportPatterns : StableConfiguration -> ModuleContext -> List (Node Pattern) -> List DeprecatedElementUsage -> List DeprecatedElementUsage
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
                    reportPatterns configuration context (subPatterns ++ restOfNodes) acc

                Pattern.RecordPattern fields ->
                    reportPatterns configuration
                        context
                        restOfNodes
                        (List.filterMap (reportField configuration context.lookupTable context.currentModuleName) fields ++ acc)

                Pattern.UnConsPattern left right ->
                    reportPatterns configuration context (left :: right :: restOfNodes) acc

                Pattern.ListPattern subPatterns ->
                    reportPatterns configuration context (subPatterns ++ restOfNodes) acc

                Pattern.VarPattern name ->
                    reportPatterns
                        configuration
                        context
                        restOfNodes
                        (maybeCons (reportParameter configuration context.currentModuleName (Node.range pattern) name) acc)

                Pattern.NamedPattern qualifiedNameRef subPatterns ->
                    let
                        errors : List DeprecatedElementUsage
                        errors =
                            reportElementAsList
                                context
                                (Node.range pattern)
                                (\() -> rangeForNamedPattern pattern qualifiedNameRef)
                                qualifiedNameRef.name
                                acc
                    in
                    reportPatterns
                        configuration
                        context
                        (subPatterns ++ restOfNodes)
                        errors

                Pattern.AsPattern subPattern name ->
                    reportPatterns
                        configuration
                        context
                        (subPattern :: restOfNodes)
                        (maybeCons (reportParameter configuration context.currentModuleName (Node.range name) (Node.value name)) acc)

                _ ->
                    reportPatterns configuration context restOfNodes acc


rangeForNamedPattern : Node a -> Pattern.QualifiedNameRef -> Range
rangeForNamedPattern (Node { start } _) { moduleName, name } =
    let
        lengthForName : Int
        lengthForName =
            if List.isEmpty moduleName then
                String.length name

            else
                (String.join "." moduleName ++ "." ++ name)
                    |> String.length
    in
    { start = start
    , end = { row = start.row, column = start.column + lengthForName }
    }


reportField : StableConfiguration -> ModuleNameLookupTable -> ModuleName -> Node String -> Maybe DeprecatedElementUsage
reportField (StableConfiguration configuration) lookupTable currentModuleName field =
    if configuration.recordFieldPredicate (Node.value field) then
        let
            moduleName : ModuleName
            moduleName =
                ModuleNameLookupTable.fullModuleNameFor lookupTable field
                    |> Maybe.withDefault currentModuleName
        in
        Just (usageOfDeprecatedElement moduleName (Node.value field) Field (Node.range field))

    else
        Nothing


expressionVisitor : StableConfiguration -> Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor configuration node context =
    let
        usages : List DeprecatedElementUsage
        usages =
            expressionVisitorHelp configuration node context
    in
    ( List.map toError usages, { context | usages = usages ++ context.usages } )


expressionVisitorHelp : StableConfiguration -> Node Expression -> ModuleContext -> List DeprecatedElementUsage
expressionVisitorHelp configuration (Node nodeRange node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            reportElementAsList
                context
                nodeRange
                (always nodeRange)
                name
                []

        Expression.LetExpression letBlock ->
            reportLetDeclarations configuration context letBlock.declarations []

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
                []

        Expression.RecordAccess _ field ->
            case reportField configuration context.lookupTable context.currentModuleName field of
                Just err ->
                    [ err ]

                Nothing ->
                    []

        Expression.RecordAccessFunction fieldName ->
            case reportField configuration context.lookupTable context.currentModuleName (Node nodeRange fieldName) of
                Just err ->
                    [ err ]

                Nothing ->
                    []

        _ ->
            []


reportElementAsList : ModuleContext -> Range -> (() -> Range) -> String -> List DeprecatedElementUsage -> List DeprecatedElementUsage
reportElementAsList context rangeForLookupTable rangeForReport name acc =
    case ModuleNameLookupTable.fullModuleNameAt context.lookupTable rangeForLookupTable of
        Just moduleName ->
            case Dict.get moduleName context.deprecatedModules of
                Just DeprecatedModule ->
                    usageOfDeprecatedElement moduleName name Module (rangeForReport ()) :: acc

                Just DeprecatedDependency ->
                    usageOfDeprecatedElement moduleName name Dependency (rangeForReport ()) :: acc

                Nothing ->
                    if Set.member ( moduleName, name ) context.deprecatedElements then
                        usageOfDeprecatedElement moduleName name Element (rangeForReport ()) :: acc

                    else
                        acc

        Nothing ->
            acc


reportElementAsMaybe : ModuleContext -> Range -> String -> Maybe DeprecatedElementUsage
reportElementAsMaybe context range name =
    case ModuleNameLookupTable.fullModuleNameAt context.lookupTable range of
        Just moduleName ->
            case Dict.get moduleName context.deprecatedModules of
                Just DeprecatedModule ->
                    Just (usageOfDeprecatedElement moduleName name Module range)

                Just DeprecatedDependency ->
                    Just (usageOfDeprecatedElement moduleName name Dependency range)

                Nothing ->
                    if Set.member ( moduleName, name ) context.deprecatedElements then
                        Just (usageOfDeprecatedElement moduleName name Element range)

                    else
                        Nothing

        Nothing ->
            Nothing


reportParameter : StableConfiguration -> ModuleName -> Range -> String -> Maybe DeprecatedElementUsage
reportParameter (StableConfiguration configuration) currentModuleName range name =
    if configuration.parameterPredicate name then
        Just (usageOfDeprecatedElement currentModuleName name Parameter range)

    else
        Nothing


type Origin
    = Element
    | Module
    | Dependency
    | Field
    | Parameter


usageOfDeprecatedElement : ModuleName -> String -> Origin -> Range -> DeprecatedElementUsage
usageOfDeprecatedElement =
    DeprecatedElementUsage


error : Origin -> Range -> Rule.Error {}
error origin range =
    let
        details : List String
        details =
            case origin of
                Element ->
                    [ "This element was marked as deprecated and should not be used anymore."
                    , "Please check its documentation to know the alternative solutions."
                    ]

                Module ->
                    [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                    , "Please check its documentation to know the alternative solutions."
                    ]

                Dependency ->
                    [ "The dependency where this element is defined was marked as deprecated and should not be used anymore."
                    , "Please check its documentation or your review configuration to know the alternative solutions."
                    ]

                Field ->
                    [ "This element was marked as deprecated and should not be used anymore."
                    , "Please check its documentation to know the alternative solutions."
                    ]

                Parameter ->
                    [ "This element was marked as deprecated and should not be used anymore."
                    ]
    in
    Rule.error
        { message = "Found new usage of deprecated element"
        , details = details
        }
        range


dataExtractor : ProjectContext -> Encode.Value
dataExtractor projectContext =
    let
        deprecatedModules : Set String
        deprecatedModules =
            Dict.foldl
                (\key _ acc ->
                    Set.insert (String.join "." key) acc
                )
                Set.empty
                projectContext.deprecatedModules
    in
    projectContext.usages
        |> Dict.foldl
            (\( moduleName, name ) count acc ->
                Dict.update
                    (String.join "." moduleName)
                    (Maybe.withDefault Dict.empty >> Dict.insert name count >> Just)
                    acc
            )
            Dict.empty
        |> Dict.toList
        |> List.map (\( moduleName, dict ) -> ( moduleName, encodeCountDict (Set.member moduleName deprecatedModules) dict ))
        |> List.sortBy (\( _, ( _, count ) ) -> -count)
        |> List.map (\( moduleName, ( dict, _ ) ) -> ( moduleName, dict ))
        |> Encode.object


encodeCountDict : Bool -> Dict String Int -> ( Encode.Value, Int )
encodeCountDict isModuleDeprecated dict =
    let
        ( fields, totalCount ) =
            Dict.foldl
                (\name count ( accList, accCount ) ->
                    ( ( name, count ) :: accList
                    , accCount + count
                    )
                )
                ( [], 0 )
                dict
    in
    ( Encode.object
        [ ( "total", Encode.int totalCount )
        , ( "isModuleDeprecated", Encode.bool isModuleDeprecated )
        , ( "usages"
          , Encode.object
                (fields |> List.sortBy (Tuple.second >> negate) |> List.map (Tuple.mapSecond Encode.int))
          )
        ]
    , totalCount
    )


maybeCons : Maybe a -> List a -> List a
maybeCons maybe list =
    case maybe of
        Just a ->
            a :: list

        Nothing ->
            list
