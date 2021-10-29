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
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when referencing a local function whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
somethingDeprecated = 1

a = somethingDeprecated
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated `somethingDeprecated`"
                            , details = [ "REPLACEME" ]
                            , under = "somethingDeprecated"
                            }
                        ]
        ]
