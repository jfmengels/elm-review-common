module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range
import Review.Fix as Fix exposing (Fix)
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
    Rule.newModuleRuleSchema "NoImportingEverything" ()
        |> Rule.withSimpleImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> Rule.fromModuleRuleSchema


exceptionsToSet : List String -> Set (List String)
exceptionsToSet exceptions =
    exceptions
        |> List.map (String.split ".")
        |> Set.fromList


importVisitor : Set (List String) -> Node Import -> List (Error {})
importVisitor exceptions node =
    if Set.member (moduleName node) exceptions then
        []

    else
        case
            Node.value node
                |> .exposingList
                |> Maybe.map Node.value
        of
            Just (Exposing.All range) ->
                [ Rule.errorWithFix
                    { message = "Prefer listing what you wish to import and/or using qualified imports"
                    , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                    }
                    { start = { row = range.start.row, column = range.start.column - 1 }
                    , end = { row = range.end.row, column = range.end.column + 1 }
                    }
                    [ removeExposingFix node ]
                ]

            _ ->
                []


moduleName : Node Import -> List String
moduleName node =
    node
        |> Node.value
        |> .moduleName
        |> Node.value


removeExposingFix : Node Import -> Fix
removeExposingFix node =
    case node |> Node.value |> .moduleAlias of
        Just aliasNode ->
            let
                endOfAliasName : Range.Location
                endOfAliasName =
                    aliasNode |> Node.range |> .end

                endOfImport : Range.Location
                endOfImport =
                    Node.range node |> .end
            in
            Fix.replaceRangeBy { start = endOfAliasName, end = endOfImport } ""

        Nothing ->
            let
                endOfModuleName : Range.Location
                endOfModuleName =
                    node |> Node.value |> .moduleName |> Node.range |> .end

                endOfImport : Range.Location
                endOfImport =
                    Node.range node |> .end
            in
            Fix.replaceRangeBy { start = endOfModuleName, end = endOfImport } ""
