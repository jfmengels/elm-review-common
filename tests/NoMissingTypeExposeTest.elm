module NoMissingTypeExposeTest exposing (all)

import Elm.Docs
import Elm.Project
import Json.Decode as Decode
import NoMissingTypeExpose exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMissingTypeExpose"
        [ describe "in exposed functions" functionTests
        , describe "in exposed types" typeTests
        , describe "in exposed type aliases" typeAliasTests
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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 12 }, end = { row = 11, column = 21 } }
                        |> Review.Test.whenFixed
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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 12 }, end = { row = 11, column = 21 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, ecstatic)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


ecstatic : Happiness
ecstatic =
    Ecstatic
"""
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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 18 }, end = { row = 11, column = 27 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Maybe Happiness -> String
toString maybeHappy =
    "Very"
"""
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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 24 }, end = { row = 11, column = 33 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Maybe.Maybe Happiness -> String
toString maybeHappy =
    "Very"
"""
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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 11 }, end = { row = 11, column = 20 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, equal)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


equal : ( Happiness, Happiness ) -> Bool
equal ( a, b ) =
    a == b
"""
                    , Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 22 }, end = { row = 11, column = 31 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, equal)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


equal : ( Happiness, Happiness ) -> Bool
equal ( a, b ) =
    a == b
"""
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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 22 }, end = { row = 11, column = 31 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, rank)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


rank : { happiness : Happiness } -> Int
rank { happiness } =
    0
"""
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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 11, column = 26 }, end = { row = 11, column = 35 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, rank)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


rank : { a | happiness : Happiness } -> Int
rank { happiness } =
    0
"""
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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 6, column = 13 }, end = { row = 6, column = 22 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, Mood(..))


type Mood
    = Happy Happiness


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
"""
                    ]
    , test "reports when an exposed typed type alias uses a private type" <|
        \() ->
            """
module Happiness exposing (Mood)


type alias Mood =
    Maybe Happiness


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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 6, column = 11 }, end = { row = 6, column = 20 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, Mood)


type alias Mood =
    Maybe Happiness


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
"""
                    ]
    , test "reports when an exposed record type alias uses a private type" <|
        \() ->
            """
module Happiness exposing (Mood)


type alias Mood =
    { happiness : Happiness }


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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 6, column = 19 }, end = { row = 6, column = 28 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, Mood)


type alias Mood =
    { happiness : Happiness }


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
"""
                    ]
    , test "reports when an exposed extended record type alias uses a private type" <|
        \() ->
            """
module Happiness exposing (Mood)


type alias Mood =
    { a | happiness : Happiness }


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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 6, column = 23 }, end = { row = 6, column = 32 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, Mood)


type alias Mood =
    { a | happiness : Happiness }


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
"""
                    ]
    , test "reports when an exposed tuple type alias uses a private type" <|
        \() ->
            """
module Happiness exposing (Mood)


type alias Mood =
    ( Happiness, Int )


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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 6, column = 7 }, end = { row = 6, column = 16 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, Mood)


type alias Mood =
    ( Happiness, Int )


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
"""
                    ]
    , test "reports when an exposed function type alias uses a private type" <|
        \() ->
            """
module Happiness exposing (Mood)


type alias Mood =
    Happiness -> Int


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
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } }
                        |> Review.Test.whenFixed
                            """
module Happiness exposing (Happiness, Mood)


type alias Mood =
    Happiness -> Int


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
"""
                    ]
    ]


typeAliasTests : List Test
typeAliasTests =
    [ test "passes when everything is exposed" <|
        \() ->
            """
module Happiness exposing (..)


type alias Happiness =
    { happiness : Int }


toString : Happiness -> String
toString howHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "does not report a function using an exposed type alias of a private type" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (Happiness, toString)

import Mood


type alias Happiness =
    Mood.Happiness


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
                |> Review.Test.expectNoErrors
    , test "reports a function using a private type alias" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            """
module Exposed exposing (toString)


type alias Happiness =
    { happiness : Int }


toString : Happiness -> String
toString happiness =
    "Happy"
"""
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is not exposed but is used by an exposed function."
                            , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 9, column = 12 }, end = { row = 9, column = 21 } }
                        |> Review.Test.whenFixed
                            """
module Exposed exposing (Happiness, toString)


type alias Happiness =
    { happiness : Int }


toString : Happiness -> String
toString happiness =
    "Happy"
"""
                    ]
    , test "reports a function using an internal type alias" <|
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


type alias Happiness =
    { howHappy : Int }
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectErrorsForModules
                    [ ( "Exposed"
                      , [ Review.Test.error
                            { message = "Private type `Mood.Happiness` used by exposed function"
                            , details =
                                [ "Type `Mood.Happiness` is not exposed but is used by an exposed function."
                                , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                                ]
                            , under = "Mood.Happiness"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 12 }, end = { row = 7, column = 26 } }
                        ]
                      )
                    ]
    , test "reports a function using an internal type alias exposed via (..)" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import Mood exposing (..)


toString : Happiness -> String
toString happiness =
    "Happy"
""", """
module Mood exposing (..)


type alias Happiness =
    { howHappy : Int }
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectErrorsForModules
                    [ ( "Exposed"
                      , [ Review.Test.error
                            { message = "Private type `Mood.Happiness` used by exposed function"
                            , details =
                                [ "Type `Mood.Happiness` is not exposed but is used by an exposed function."
                                , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                                ]
                            , under = "Happiness"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 12 }, end = { row = 7, column = 21 } }
                        ]
                      )
                    ]
    , test "reports a function using an internal type alias exposed explicitly and imported via (..)" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import Mood exposing (..)


toString : Happiness -> String
toString happiness =
    "Happy"
""", """
module Mood exposing (Happiness)


type alias Happiness =
    { howHappy : Int }
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectErrorsForModules
                    [ ( "Exposed"
                      , [ Review.Test.error
                            { message = "Private type `Mood.Happiness` used by exposed function"
                            , details =
                                [ "Type `Mood.Happiness` is not exposed but is used by an exposed function."
                                , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                                ]
                            , under = "Happiness"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 12 }, end = { row = 7, column = 21 } }
                        ]
                      )
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
                                , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
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
                                , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
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
                            { message = "Private type `Mood.Happiness` used by exposed function"
                            , details =
                                [ "Type `Mood.Happiness` is not exposed but is used by an exposed function."
                                , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                                ]
                            , under = "Happiness"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 12 }, end = { row = 7, column = 21 } }
                        ]
                      )
                    ]
    , test "reports an exposed function using an internal type imported exposing all" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toRating)

import Mood exposing (..)
import Rating exposing (..)


toRating : Happiness -> Rating
toRating happiness =
    Rating.five
""", """
module Mood exposing (Happiness)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
""", """
module Rating exposing (Rating, five)


type Rating =
    Rating Int


five : Rating
five =
    Rating 5
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectErrorsForModules
                    [ ( "Exposed"
                      , [ Review.Test.error
                            { message = "Private type `Mood.Happiness` used by exposed function"
                            , details =
                                [ "Type `Mood.Happiness` is not exposed but is used by an exposed function."
                                , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                                ]
                            , under = "Happiness"
                            }
                        , Review.Test.error
                            { message = "Private type `Rating.Rating` used by exposed function"
                            , details =
                                [ "Type `Rating.Rating` is not exposed but is used by an exposed function."
                                , "Callers of this function will not be able to annotate other functions or variables that use this type outside of the module. You should expose this type or an alias of this type."
                                ]
                            , under = "Rating"
                            }
                            |> Review.Test.atExactly { start = { row = 8, column = 25 }, end = { row = 8, column = 31 } }
                        ]
                      )
                    ]
    , test "does not report an internal function using a type from another internal module" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Mood exposing (Happiness, toRating)

import Rating exposing (Rating)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toRating : Happiness -> Rating
toRating happiness =
    Rating.five
""", """
module Rating exposing (Rating, five)


type Rating =
    Rating Int


five : Rating
five =
    Rating 5
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectNoErrors
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
    , test "does not report an exposed function using an exposed type imported all" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import ExposedMood exposing (..)


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
    , test "does not report an exposed function using an type exposed all and imported all" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
            in
            [ """
module Exposed exposing (toString)

import ExposedMood exposing (..)


toString : Happiness -> String
toString happiness =
    "Happy"
""", """
module ExposedMood exposing (..)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectNoErrors
    , test "does not report an exposed function using a dependency's type" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
                        |> Project.addDependency dependency
            in
            """
module Exposed exposing (toString)

import ExternalMood exposing (Happiness)


toString : Happiness -> String
toString happiness =
    "Happy"
"""
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectNoErrors
    , test "does not report an exposed function using a dependency's type exposing all" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
                        |> Project.addDependency dependency
            in
            """
module Exposed exposing (toString)

import ExternalMood exposing (..)


toString : Happiness -> String
toString happiness =
    "Happy"
"""
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectNoErrors
    , test "does not report an exposed function importing a dependency's type" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
                        |> Project.addDependency dependency
            in
            """
module Exposed exposing (toString)

import ExternalMood


toString : ExternalMood.Happiness -> String
toString happiness =
    "Happy"
"""
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectNoErrors
    , test "does not report an exposed function importing a dependency's type with alias" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson (createElmJson packageElmJson)
                        |> Project.addDependency dependency
            in
            """
module Exposed exposing (toString)

import ExternalMood as M


toString : M.Happiness -> String
toString happiness =
    "Happy"
"""
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectNoErrors
    ]


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    { path = "elm.json"
    , raw = rawElmJson
    , project = createElmJsonProject rawElmJson
    }


createElmJsonProject : String -> Elm.Project.Project
createElmJsonProject rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            project

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


dependency : Dependency
dependency =
    Dependency.create
        "sparksp/external-mood"
        (createElmJsonProject dependencyElmJson)
        dependencyModules


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "ExternalMood"
      , comment = ""
      , unions =
            [ { name = "Happiness"
              , comment = ""
              , args = []
              , tags =
                    [ ( "Ecstatic", [] )
                    , ( "FineIGuess", [] )
                    , ( "Unhappy", [] )
                    ]
              }
            ]
      , aliases = []
      , values = []
      , binops = []
      }
    ]


dependencyElmJson : String
dependencyElmJson =
    """{
    "type": "package",
    "name": "sparksp/external-mood",
    "summary": "Moods for tests",
    "license": "MIT",
    "version": "1.0.0",
    "exposed-modules": [
        "ExternalMood"
    ],
    "elm-version": "0.19.0 <= v <= 0.20.0",
    "dependencies": {},
    "test-dependencies": {}
}"""
