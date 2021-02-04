module NoSimpleLetBodyTest exposing (all)

import NoSimpleLetBody exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoSimpleLetBody"
        [ test "should report an error when let body is a simple function or value" <|
            \() ->
                """module A exposing (..)
a = let b = 1
    in b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "b"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 14 } }
                        ]
        ]
