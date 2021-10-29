module NoDeprecated exposing
    ( rule
    , checkInName
    )

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookuTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports... REPLACEME

    config =
        [ NoDeprecated.rule
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
elm-review --template jfmengels/elm-review-common/example --rules NoDeprecated
```

-}
rule : Configuration -> Rule
rule configuration =
    Rule.newModuleRuleSchemaUsingContextCreator "NoDeprecated" initialContext
        |> Rule.withDeclarationEnterVisitor (\node context -> ( declarationVisitor configuration node context, context ))
        |> Rule.withExpressionEnterVisitor (\node context -> ( expressionVisitor configuration node context, context ))
        |> Rule.fromModuleRuleSchema


type Configuration
    = Configuration
        { moduleNamePredicate : ModuleName -> Bool
        , elementPredicate : ModuleName -> String -> Bool
        , typePredicate : ModuleName -> String -> Bool
        }


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
        , elementPredicate = \_ name -> containsDeprecated name
        , typePredicate = \_ name -> containsDeprecated name
        }


type alias Context =
    { lookupTable : ModuleNameLookupTable
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> { lookupTable = lookupTable })
        |> Rule.withModuleNameLookupTable


declarationVisitor : Configuration -> Node Declaration -> Context -> List (Rule.Error {})
declarationVisitor configuration node context =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            case declaration.signature of
                Just signature ->
                    reportTypes
                        configuration
                        context.lookupTable
                        [ (Node.value signature).typeAnnotation ]
                        []

                Nothing ->
                    []

        _ ->
            []


reportTypes : Configuration -> ModuleNameLookupTable -> List (Node TypeAnnotation) -> List (Rule.Error {}) -> List (Rule.Error {})
reportTypes configuration lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.Typed subType args ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            acc
                    in
                    reportTypes
                        configuration
                        lookupTable
                        (List.append args restOfNodes)
                        newAcc

                TypeAnnotation.Tupled nodesToLookAt ->
                    reportTypes configuration lookupTable (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.Record recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (Node.value >> Tuple.second) recordDefinition
                    in
                    reportTypes configuration lookupTable (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.GenericRecord _ recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (Node.value >> Tuple.second) (Node.value recordDefinition)
                    in
                    reportTypes configuration lookupTable (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.FunctionTypeAnnotation left right ->
                    reportTypes configuration lookupTable (left :: right :: restOfNodes) acc

                _ ->
                    reportTypes configuration lookupTable restOfNodes acc


expressionVisitor : Configuration -> Node Expression -> Context -> List (Rule.Error {})
expressionVisitor configuration (Node nodeRange node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            reportValue
                configuration
                context.lookupTable
                nodeRange
                name

        Expression.RecordUpdateExpression name _ ->
            reportValue
                configuration
                context.lookupTable
                (Node.range name)
                (Node.value name)

        _ ->
            []


reportValue : Configuration -> ModuleNameLookupTable -> Range -> String -> List (Rule.Error {})
reportValue (Configuration configuration) lookupTable range name =
    case ModuleNameLookuTable.moduleNameAt lookupTable range of
        Just moduleName ->
            if
                configuration.elementPredicate moduleName name
                    || configuration.moduleNamePredicate moduleName
            then
                [ error range ]

            else
                []

        Nothing ->
            []


reportType : Configuration -> ModuleNameLookupTable -> Range -> String -> List (Rule.Error {})
reportType (Configuration configuration) lookupTable range name =
    case ModuleNameLookuTable.moduleNameAt lookupTable range of
        Just moduleName ->
            if
                configuration.typePredicate moduleName name
                    || configuration.moduleNamePredicate moduleName
            then
                [ error range ]

            else
                []

        Nothing ->
            []


error : Range -> Rule.Error {}
error range =
    Rule.error
        { message = "Found new usage of deprecated element"
        , details = [ "REPLACEME" ]
        }
        range
