module NoUnnecessaryTrailingUnderscoreTest exposing (all)

import NoUnnecessaryTrailingUnderscore exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnnecessaryTrailingUnderscore"
        [ test "should report an error when argument has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a value_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value_"
                            }
                        ]
        ]
