module NoExposingEverythingTest exposing (all)

import Expect
import Fuzz
import NoExposingEverything exposing (rule)
import Random
import Review.Test
import Shrink
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoExposingEverything"
        [ test "should not report anything when a module exposes a limited set of things" <|
            \() ->
                """
module A exposing (B(..), C, d)
type B = B
d = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , Test.fuzz2 genName genName "should report when a module exposes everything" <|
            \nameModule nameImport ->
                ("""
module """ ++ nameModule ++ """ exposing (..)
import """ ++ nameImport ++ """ exposing (..)
""")
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module exposes everything implicitly \"(..)\""
                            , details =
                                [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access it's internal details. Therefore, the API should be explicitly defined and ideally as small as possible."
                                ]
                            , under = "(..)"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = String.length nameModule + 18 }, end = { row = 2, column = String.length nameModule + 22 } }
                        ]
        ]


upperCase : Random.Generator Char
upperCase =
    Random.int 0 25
        |> Random.map (\n -> Char.fromCode (n + 65))


lowerCase : Random.Generator Char
lowerCase =
    Random.int 0 25
        |> Random.map (\n -> Char.fromCode (n + 97))


digit : Random.Generator Char
digit =
    Random.int 0 9
        |> Random.map (\n -> Char.fromCode (n + 48))


anyValidNameCharacter : Random.Generator Char
anyValidNameCharacter =
    Random.weighted
        ( 1, Random.constant '_' )
        [ ( 26, upperCase )
        , ( 26, lowerCase )
        , ( 10, digit )
        ]
        |> Random.andThen identity


genName : Fuzz.Fuzzer String
genName =
    let
        generator : Random.Generator String
        generator =
            Random.map2 (\first rest -> String.fromList (first :: rest))
                upperCase
                (Random.int 5 10 |> Random.andThen (\n -> Random.list n anyValidNameCharacter))
    in
    Fuzz.custom generator Shrink.noShrink
