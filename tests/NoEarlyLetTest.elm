module NoEarlyLetTest exposing (all)

import NoEarlyLet exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)



-- TODO Keep computations done outside of lambdas there. It might be an optimization.


all : Test
all =
    describe "NoEarlyLet"
        [ test "should report a let declaration that could be computed in a if branch" <|
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
                            |> Review.Test.whenFixed ("""module A exposing (..)
a b c d =
  if b then
    let
        z = 1
     $
    in
    z
  else
    1
""" |> String.replace "$" " ")
                        ]
        , test "should report a let declaration that could be computed in a if branch (referenced by record update expression)" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = {a = 1}
  in
  if b then
    {z | a = 2}
  else
    {a = 3}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a b c d =
  if b then
    let
        z = {a = 1}
     $
    in
    {z | a = 2}
  else
    {a = 3}
""" |> String.replace "$" " ")
                        ]
        , test "should add to an existing let block instead of inserting a new let block" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = {a = 1}
  in
  if b then
    let
      y = 1
    in
    {z | a = 2}
  else
    {a = 3}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a b c d =
  if b then
    let
        z = {a = 1}
     $
      y = 1
    in
    {z | a = 2}
  else
    {a = 3}
""" |> String.replace "$" " ")
                        ]
        , test "should not report let functions" <|
            \() ->
                -- TODO later?
                """module A exposing (..)
a b c d =
  let
    z n = 1
  in
  if b then
    z 1
  else
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report a let declaration is used in multiple if branches" <|
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
        , test "should not report a let declaration if it's unused" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  if b then
    1
  else
    2
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
        , test "should not report a let declaration without branches" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a let declaration that could be computed in a case branch" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  case b of
    A ->
        1
    B ->
        z
    C ->
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
                            |> Review.Test.whenFixed ("""module A exposing (..)
a b c d =
  case b of
    A ->
        1
    B ->
        let
            z = 1
         $
        in
        z
    C ->
        1
""" |> String.replace "$" " ")
                        ]
        , test "should not report a let declaration is used in multiple case branches" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  case b of
    A ->
        z
    B ->
        z
    C ->
        1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should only remove the moved let declaration when there are other let declarations there" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    y = 1
    z = {a = 1}
  in
  if b then
    {z | a = 2}
  else
    {a = 3}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a b c d =
  let
    y = 1
    in
  if b then
    let
        z = {a = 1}
     $
    in
    {z | a = 2}
  else
    {a = 3}
""" |> String.replace "$" " ")
                        ]
        , test "should not be confused by what happens in other declarations" <|
            \() ->
                """module A exposing (..)
first =
  case A of
    A -> z

second =
    let
        z = let x = 1 in x
        y = z
    in
    y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
