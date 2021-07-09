module NoEarlyLetTest exposing (all)

import NoEarlyLet exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)



-- TODO Keep computations done outside of lambdas there. It might be an optimization.


all : Test
all =
    describe "NoEarlyLet"
        [ test "should report a let declaration that could be computed in a branch" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  if b then
    z
  else
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "z"
                            }
                        ]
        , test "should not report a let declaration is used in multiple branches" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  if b then
    z
  else
    z + 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
