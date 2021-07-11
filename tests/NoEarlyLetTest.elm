module NoEarlyLetTest exposing (all)

import NoEarlyLet exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)



-- TODO Keep computations done outside of lambdas there. It might be an optimization.
-- TODO Look at variables used in the same branch


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
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
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
        , test "should not report a let declaration is used next to where it was declared" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
    y = z * 2
  in
  if b then
    y
  else
    y + 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
