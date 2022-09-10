module NoConfusingPrefixOperatorTest exposing (all)

import NoConfusingPrefixOperator exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoConfusingPrefixOperator"
        [ test "should not report fully-applied operators" <|
            \() ->
                """module A exposing (..)
a = a < 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report operators not used in an application" <|
            \() ->
                """module A exposing (..)
a = (<)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when using a confusing operator with only a single argument" <|
            \() ->
                """module A exposing (..)
a = (<) 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "(<)"
                            }
                        ]
        ]
