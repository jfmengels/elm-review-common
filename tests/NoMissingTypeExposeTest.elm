module NoMissingTypeExposeTest exposing (all)

import Elm.Project
import Json.Decode as Decode
import NoMissingTypeExpose exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMissingTypeExpose"
        [ describe "in exposed functions" functionTests
        , describe "in exposed types" typeTests
        , describe "in package projects" packageTests
        ]


functionTests : List Test
functionTests =
    [ test "passes when everything is exposed" <|
        \() ->
            """
module Happiness exposing (..)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Happiness -> String
toString howHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "passes when an exposed function uses an exposed type" <|
        \() ->
            """
module Happiness exposing (Happiness, toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Happiness -> String
toString howHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "reports when an exposed function uses a private type" <|
        \() ->
            """
module Happiness exposing (toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Happiness -> String
toString howHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 12 }, end = { row = 11, column = 21 } }
                    ]
    , test "reports when an exposed function returns a private type" <|
        \() ->
            """
module Happiness exposing (ecstatic)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


ecstatic : Happiness
ecstatic =
    Ecstatic
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 12 }, end = { row = 11, column = 21 } }
                    ]
    , test "reports when an exposed function uses a typed private type" <|
        \() ->
            """
module Happiness exposing (toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Maybe Happiness -> String
toString maybeHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 18 }, end = { row = 11, column = 27 } }
                    ]
    , test "reports when an exposed function uses an external typed private type" <|
        \() ->
            """
module Happiness exposing (toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Maybe.Maybe Happiness -> String
toString maybeHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 24 }, end = { row = 11, column = 33 } }
                    ]
    , test "reports when an exposed function uses a private type in a tuple" <|
        \() ->
            """
module Happiness exposing (equal)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


equal : ( Happiness, Happiness ) -> Bool
equal ( a, b ) =
    a == b
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 11 }, end = { row = 11, column = 20 } }
                    , Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 22 }, end = { row = 11, column = 31 } }
                    ]
    , test "reports when an exposed function uses a private type in a record" <|
        \() ->
            """
module Happiness exposing (rank)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


rank : { happiness : Happiness } -> Int
rank { happiness } =
    0
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 22 }, end = { row = 11, column = 31 } }
                    ]
    , test "reports when an exposed function uses a private type in a generic record" <|
        \() ->
            """
module Happiness exposing (rank)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


rank : { a | happiness : Happiness } -> Int
rank { happiness } =
    0
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 26 }, end = { row = 11, column = 35 } }
                    ]
    ]


typeTests : List Test
typeTests =
    [ test "passes when an exposed type uses another exposed type" <|
        \() ->
            """
module Happiness exposing (Happiness, Mood(..))


type Mood
    = Happy Happiness


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "passes when an exposed opaque type uses a private type" <|
        \() ->
            """
module Happiness exposing (Mood)


type Mood
    = Happy Happiness


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "reports when an exposed open type uses a private type" <|
        \() ->
            """
module Happiness exposing (Mood(..))


type Mood
    = Happy Happiness


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 6, column = 13 }, end = { row = 6, column = 22 } }
                    ]
    ]


packageTests : List Test
packageTests =
    [ test "reports a function exposed by a package module using a type from an internal module" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import Mood


toString : Mood.Happiness -> String
toString happiness =
    "Happy"
""", """
module Mood exposing (Happiness)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectErrorsForModules
                    [ ( "Exposed"
                      , [ Review.Test.error
                            { message = "Private type `Mood.Happiness` used by exposed function"
                            , details =
                                [ "Type `Mood.Happiness` is not exposed but is used by an exposed function."
                                ]
                            , under = "Mood.Happiness"
                            }
                        ]
                      )
                    ]
    , test "does not report an exposed function using a type from an exposed module" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import ExposedMood


toString : ExposedMood.Happiness -> String
toString happiness =
    "Happy"
""", """
module ExposedMood exposing (Happiness)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectNoErrors
    , test "reports an exposed function using an internal type from an aliased module" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import Mood as M


toString : M.Happiness -> String
toString happiness =
    "Happy"
""", """
module Mood exposing (Happiness)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectErrorsForModules
                    [ ( "Exposed"
                      , [ Review.Test.error
                            { message = "Private type `M.Happiness` used by exposed function"
                            , details =
                                [ "Type `M.Happiness` is not exposed but is used by an exposed function."
                                ]
                            , under = "M.Happiness"
                            }
                        ]
                      )
                    ]
    , test "reports an exposed function using an exposed type from an aliased module" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import ExposedMood as M


toString : M.Happiness -> String
toString happiness =
    "Happy"
""", """
module ExposedMood exposing (Happiness)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectNoErrors
    , test "reports an exposed function using an internal type imported from a module" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import Mood exposing (Happiness)


toString : Happiness -> String
toString happiness =
    "Happy"
""", """
module Mood exposing (Happiness)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectErrorsForModules
                    [ ( "Exposed"
                      , [ Review.Test.error
                            { message = "Private type `Happiness` used by exposed function"
                            , details =
                                [ "Type `Happiness` is not exposed but is used by an exposed function."
                                ]
                            , under = "Happiness"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 12 }, end = { row = 7, column = 21 } }
                        ]
                      )
                    ]
    , test "does not report an exposed function using an exposed imported type" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import ExposedMood exposing (Happiness)


toString : Happiness -> String
toString happiness =
    "Happy"
""", """
module ExposedMood exposing (Happiness)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectNoErrors
    ]


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = project
            }

        Err error ->
            Debug.todo ("[elm.json]: " ++ Debug.toString error)


packageElmJson : String
packageElmJson =
    """{
    "type": "package",
    "name": "jfmengels/review-common-tests",
    "summary": "A test package",
    "license": "MIT",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed",
        "ExposedMood"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.2 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""
