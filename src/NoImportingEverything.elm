module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbids importing everything from a module.


## Fail

    import A exposing (..)
    import A as B exposing (..)


## Success

    import A
    import A as B
    import A exposing (B(..), C, d)

-}
rule : Rule
rule =
    Rule.newSchema "NoImportingEverything"
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromSchema


importVisitor : Node Import -> List Error
importVisitor moduleNode =
    case
        Node.value moduleNode
            |> .exposingList
            |> Maybe.map Node.value
    of
        Just (Exposing.All range) ->
            [ Rule.error
                { message = "TODO"
                , details = [ "TODO" ]
                }
                { start = { row = range.start.row, column = range.start.column - 1 }
                , end = { row = range.end.row, column = range.end.column + 1 }
                }
            ]

        _ ->
            []
