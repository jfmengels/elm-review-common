module NoDeprecated exposing
    ( rule
    , checkInName
    )

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookuTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)


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
        , recordFieldPredicate : String -> Bool
        , parameterPredicate : String -> Bool
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
        , recordFieldPredicate = containsDeprecated
        , parameterPredicate = containsDeprecated
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
            let
                signatureErrors : List (Rule.Error {})
                signatureErrors =
                    case declaration.signature of
                        Just signature ->
                            reportTypes
                                configuration
                                context.lookupTable
                                [ (Node.value signature).typeAnnotation ]
                                []

                        Nothing ->
                            []

                destructuringErrors : List (Rule.Error {})
                destructuringErrors =
                    reportPatterns
                        configuration
                        context.lookupTable
                        (declaration.declaration |> Node.value |> .arguments)
                        []
            in
            List.append destructuringErrors signatureErrors

        _ ->
            []


reportTypes : Configuration -> ModuleNameLookupTable -> List (Node TypeAnnotation) -> List (Rule.Error {}) -> List (Rule.Error {})
reportTypes configuration lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.Typed (Node range ( _, name )) args ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            case reportType configuration lookupTable range name of
                                Just err ->
                                    err :: acc

                                Nothing ->
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


reportPatterns : Configuration -> ModuleNameLookupTable -> List (Node Pattern) -> List (Rule.Error {}) -> List (Rule.Error {})
reportPatterns configuration lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        pattern :: restOfNodes ->
            case Node.value pattern of
                Pattern.ParenthesizedPattern subPattern ->
                    reportPatterns
                        configuration
                        lookupTable
                        (subPattern :: restOfNodes)
                        acc

                Pattern.TuplePattern subPatterns ->
                    reportPatterns configuration lookupTable (List.append subPatterns restOfNodes) acc

                Pattern.RecordPattern fields ->
                    reportPatterns configuration
                        lookupTable
                        restOfNodes
                        (List.append (List.filterMap (reportField configuration) fields) acc)

                Pattern.UnConsPattern left right ->
                    reportPatterns configuration lookupTable (left :: right :: restOfNodes) acc

                Pattern.ListPattern subPatterns ->
                    reportPatterns configuration lookupTable (List.append subPatterns restOfNodes) acc

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
                        lookupTable
                        restOfNodes
                        newAcc

                Pattern.NamedPattern qualifiedNameRef subPatterns ->
                    let
                        lengthForName : Int
                        lengthForName =
                            (qualifiedNameRef.moduleName ++ [ qualifiedNameRef.name ])
                                |> String.join "."
                                |> String.length

                        patternStart : Range.Location
                        patternStart =
                            (Node.range pattern).start

                        rangeForNamedPattern : Range
                        rangeForNamedPattern =
                            { start = patternStart
                            , end = { row = patternStart.row, column = patternStart.column + lengthForName }
                            }

                        errors : List (Rule.Error {})
                        errors =
                            reportValue configuration
                                lookupTable
                                (Node.range pattern)
                                rangeForNamedPattern
                                qualifiedNameRef.name
                    in
                    reportPatterns
                        configuration
                        lookupTable
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
                    reportPatterns configuration lookupTable (subPattern :: restOfNodes) newAcc

                _ ->
                    reportPatterns configuration lookupTable restOfNodes acc


reportField : Configuration -> Node String -> Maybe (Rule.Error {})
reportField (Configuration configuration) field =
    if configuration.recordFieldPredicate (Node.value field) then
        Just (error (Node.range field))

    else
        Nothing


expressionVisitor : Configuration -> Node Expression -> Context -> List (Rule.Error {})
expressionVisitor configuration (Node nodeRange node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            reportValue
                configuration
                context.lookupTable
                nodeRange
                nodeRange
                name

        Expression.RecordUpdateExpression (Node range name) _ ->
            reportValue
                configuration
                context.lookupTable
                range
                range
                name

        _ ->
            []


reportValue : Configuration -> ModuleNameLookupTable -> Range -> Range -> String -> List (Rule.Error {})
reportValue (Configuration configuration) lookupTable rangeForLookupTable rangeForReport name =
    case ModuleNameLookuTable.moduleNameAt lookupTable rangeForLookupTable of
        Just moduleName ->
            if
                configuration.elementPredicate moduleName name
                    || configuration.moduleNamePredicate moduleName
            then
                [ error rangeForReport ]

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


reportType : Configuration -> ModuleNameLookupTable -> Range -> String -> Maybe (Rule.Error {})
reportType (Configuration configuration) lookupTable range name =
    case ModuleNameLookuTable.moduleNameAt lookupTable range of
        Just moduleName ->
            if
                configuration.typePredicate moduleName name
                    || configuration.moduleNamePredicate moduleName
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
