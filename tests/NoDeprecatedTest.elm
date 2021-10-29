module NoDeprecatedTest exposing (all)

import NoDeprecated exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoDeprecated"
        [ test "should not report an error when using a non-deprecated element" <|
            \() ->
                [ """module A exposing (..)
import Other
a = Other.normalFunction
""", """module Other exposing (..)
normalFunction = 1
""" ]
                    |> Review.Test.runOnModules (rule NoDeprecated.checkInName)
                    |> Review.Test.expectNoErrors
        , test "should report an error when referencing a local function whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
somethingDeprecated = 1

a = somethingDeprecated
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "somethingDeprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 24 } }
                        ]
        , test "should report an error when referencing a function from a module whose name contains 'deprecated' (qualified import)" <|
            \() ->
                """module A exposing (..)
import Some.DeprecatedModule
a = Some.DeprecatedModule.something
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Some.DeprecatedModule.something"
                            }
                        ]
        , test "should report an error when referencing a function from a module whose name contains 'deprecated' (unqualifed import)" <|
            \() ->
                """module A exposing (..)
import Some.DeprecatedModule as S
a = S.something
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "S.something"
                            }
                        ]
        , test "should report an error when referencing a function from a module whose name contains 'deprecated' (record update)" <|
            \() ->
                """module A exposing (..)
import Some.DeprecatedModule exposing (something)
a = { something | b = 1 }
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "something"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                        ]
        , test "should report an error when referencing a custom type constructor whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated
a = Deprecated
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report an error when referencing a type alias constructor whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
type alias Deprecated = {}
a = Deprecated
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (top-level declaration annotation)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Int
a : Deprecated
a = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated Int
a (Deprecated value) = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 4 }, end = { row = 3, column = 14 } }
                        ]
        , test "should report an error when having a parameter whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a thingDeprecated = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "thingDeprecated"
                            }
                        ]
        , test "should report an error when destructuring a field whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a ({deprecated}) = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "deprecated"
                            }
                        ]
        , test "should report an error when using a parameter alias whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a (( x, y ) as deprecated) = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "deprecated"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (let declaration annotation)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Int
a =
    let
        b : Deprecated
        b = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 13 }, end = { row = 5, column = 23 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (let declaration)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated Int
a =
    let
        b (Deprecated value) = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 12 }, end = { row = 5, column = 22 } }
                        ]
        , test "should report an error when having a parameter whose name contains 'deprecated' (let declaration)" <|
            \() ->
                """module A exposing (..)
a =
    let
        b thingDeprecated = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "thingDeprecated"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (let destructuring)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated Int
a =
    let
        (Deprecated b) = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 10 }, end = { row = 5, column = 20 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (custom type declaration)" <|
            \() ->
                """module A exposing (..)
type A = Thing ( A, { b : Deprecated } )
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (type alias declaration)" <|
            \() ->
                """module A exposing (..)
type alias A = Thing{ b : Deprecated }
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                        ]
        ]
