module NoImportingEverythingWithFixTest exposing (all)

import NoImportingEverythingWithFix exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoImportingEverythingWithFix"
        [ noErrorTests
        , errorTests
        , externalModuleFixTests
        , internalModuleFixTests
        , mixedScenarioTests
        ]


noErrorTests : Test
noErrorTests =
    describe "should not report an error"
        [ test "when importing without exposing clause" <|
            \() ->
                """module A exposing (..)
import Html
import Html as H
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "when importing with explicit exposing list" <|
            \() ->
                """module A exposing (..)
import Html exposing (div, text)
import Html.Attributes exposing (class)
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "when importing with type constructors" <|
            \() ->
                """module A exposing (..)
import Html exposing (Html(..), div)
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "when module is in exceptions list" <|
            \() ->
                """module A exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
"""
                    |> Review.Test.run (rule [ "Html", "Html.Attributes" ])
                    |> Review.Test.expectNoErrors
        ]


errorTests : Test
errorTests =
    describe "should report an error"
        [ test "when importing everything from an external module" <|
            \() ->
                """module A exposing (..)
import Html exposing (..)
"""
                    |> Review.Test.runWithProjectData projectWithHtmlDependencies (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 22 }, end = { row = 2, column = 26 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html exposing (Attribute, Html, a, abbr, address, article, aside, audio, b, bdi, bdo, blockquote, br, button, canvas, caption, cite, code, col, colgroup, datalist, dd, del, details, dfn, div, dl, dt, em, embed, fieldset, figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, header, hr, i, iframe, img, input, ins, kbd, label, legend, li, main_, map, mark, math, menu, menuitem, meter, nav, node, object, ol, optgroup, option, output, p, param, pre, progress, q, rp, rt, ruby, s, samp, section, select, small, source, span, strong, sub, summary, sup, table, tbody, td, text, textarea, tfoot, th, thead, time, tr, track, u, ul, var, video, wbr)
"""
                        ]
        , test "when importing everything from an internal module" <|
            \() ->
                [ """module Internal.Types exposing (CustomType, MyRecord, Status(..))

type CustomType = Custom
type alias MyRecord = { name : String }  
type Status = Active | Inactive
"""
                , """module A exposing (..)
import Internal.Types exposing (..)

value : CustomType
value = Custom
"""
                ]
                    |> Review.Test.runOnModules (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Prefer listing what you wish to import and/or using qualified imports"
                                , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                                , under = "(..)"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 32 }, end = { row = 2, column = 36 } }
                                |> Review.Test.whenFixed """module A exposing (..)
import Internal.Types exposing (CustomType, MyRecord, Status(..))

value : CustomType
value = Custom
"""
                            ]
                          )
                        ]
        , test "when importing everything from an unknown module" <|
            \() ->
                """module A exposing (..)
import UnknownModule exposing (..)
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 31 }, end = { row = 2, column = 35 } }
                        ]
        ]


projectWithHtmlDependencies : Project
projectWithHtmlDependencies =
    Review.Test.Dependencies.projectWithElmCore
        |> Project.addDependency Review.Test.Dependencies.elmHtml


externalModuleFixTests : Test
externalModuleFixTests =
    describe "should provide fixes for external modules"
        [ test "when importing Html" <|
            \() ->
                """module A exposing (..)
import Html exposing (..)
"""
                    |> Review.Test.runWithProjectData projectWithHtmlDependencies (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 22 }, end = { row = 2, column = 26 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html exposing (Attribute, Html, a, abbr, address, article, aside, audio, b, bdi, bdo, blockquote, br, button, canvas, caption, cite, code, col, colgroup, datalist, dd, del, details, dfn, div, dl, dt, em, embed, fieldset, figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, header, hr, i, iframe, img, input, ins, kbd, label, legend, li, main_, map, mark, math, menu, menuitem, meter, nav, node, object, ol, optgroup, option, output, p, param, pre, progress, q, rp, rt, ruby, s, samp, section, select, small, source, span, strong, sub, summary, sup, table, tbody, td, text, textarea, tfoot, th, thead, time, tr, track, u, ul, var, video, wbr)
"""
                        ]
        , test "when importing Html.Attributes" <|
            \() ->
                """module A exposing (..)
import Html.Attributes exposing (..)
"""
                    |> Review.Test.runWithProjectData projectWithHtmlDependencies (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 33 }, end = { row = 2, column = 37 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes exposing (accept, acceptCharset, accesskey, action, align, alt, attribute, autocomplete, autofocus, autoplay, checked, cite, class, classList, cols, colspan, contenteditable, contextmenu, controls, coords, datetime, default, dir, disabled, download, draggable, dropzone, enctype, for, form, headers, height, hidden, href, hreflang, id, ismap, itemprop, kind, lang, list, loop, manifest, map, max, maxlength, media, method, min, minlength, multiple, name, novalidate, pattern, ping, placeholder, poster, preload, property, pubdate, readonly, rel, required, reversed, rows, rowspan, sandbox, scope, selected, shape, size, spellcheck, src, srcdoc, srclang, start, step, style, tabindex, target, title, type_, usemap, value, width, wrap)
"""
                        ]
        , test "should not provide fix for unknown external modules" <|
            \() ->
                """module A exposing (..)
import UnknownModule exposing (..)
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 31 }, end = { row = 2, column = 35 } }
                        ]
        ]


internalModuleFixTests : Test
internalModuleFixTests =
    describe "should provide fixes for internal modules"
        [ test "when importing from module with explicit exports" <|
            \() ->
                [ """module Internal.Types exposing (CustomType, MyRecord, Status(..))

type CustomType = Custom
type alias MyRecord = { name : String }  
type Status = Active | Inactive
"""
                , """module A exposing (..)
import Internal.Types exposing (..)

value : CustomType
value = Custom
"""
                ]
                    |> Review.Test.runOnModules (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Prefer listing what you wish to import and/or using qualified imports"
                                , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                                , under = "(..)"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 32 }, end = { row = 2, column = 36 } }
                                |> Review.Test.whenFixed """module A exposing (..)
import Internal.Types exposing (CustomType, MyRecord, Status(..))

value : CustomType
value = Custom
"""
                            ]
                          )
                        ]
        , test "when importing from module with type constructors" <|
            \() ->
                [ """module Internal.Data exposing (Result(..), Status, process)

type Result a = Success a | Error String
type Status = Loading | Complete  
process : String -> Result String
process s = Success s
"""
                , """module B exposing (..)
import Internal.Data exposing (..)

handleResult : Result String -> String
handleResult result =
    case result of
        Success s -> s
        Error e -> e
"""
                ]
                    |> Review.Test.runOnModules (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ Review.Test.error
                                { message = "Prefer listing what you wish to import and/or using qualified imports"
                                , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                                , under = "(..)"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 31 }, end = { row = 2, column = 35 } }
                                |> Review.Test.whenFixed """module B exposing (..)
import Internal.Data exposing (Result(..), Status, process)

handleResult : Result String -> String
handleResult result =
    case result of
        Success s -> s
        Error e -> e
"""
                            ]
                          )
                        ]
        ]


mixedScenarioTests : Test
mixedScenarioTests =
    describe "should handle mixed internal/external scenarios"
        [ test "when file has both internal and external (..) imports" <|
            \() ->
                [ """module Utils.Helper exposing (formatString, parseNumber)

formatString : String -> String
formatString s = String.trim s

parseNumber : String -> Maybe Int  
parseNumber s = String.toInt s
"""
                , """module Mixed exposing (..)
import Html exposing (..)
import Utils.Helper exposing (..)

view : String -> Html msg
view text = 
    div [] [ Html.text (formatString text) ]
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData projectWithHtmlDependencies (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "Mixed"
                          , [ Review.Test.error
                                { message = "Prefer listing what you wish to import and/or using qualified imports"
                                , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                                , under = "(..)"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 22 }, end = { row = 2, column = 26 } }
                                |> Review.Test.whenFixed """module Mixed exposing (..)
import Html exposing (Attribute, Html, a, abbr, address, article, aside, audio, b, bdi, bdo, blockquote, br, button, canvas, caption, cite, code, col, colgroup, datalist, dd, del, details, dfn, div, dl, dt, em, embed, fieldset, figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, header, hr, i, iframe, img, input, ins, kbd, label, legend, li, main_, map, mark, math, menu, menuitem, meter, nav, node, object, ol, optgroup, option, output, p, param, pre, progress, q, rp, rt, ruby, s, samp, section, select, small, source, span, strong, sub, summary, sup, table, tbody, td, text, textarea, tfoot, th, thead, time, tr, track, u, ul, var, video, wbr)
import Utils.Helper exposing (..)

view : String -> Html msg
view text = 
    div [] [ Html.text (formatString text) ]
"""
                            , Review.Test.error
                                { message = "Prefer listing what you wish to import and/or using qualified imports"
                                , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                                , under = "(..)"
                                }
                                |> Review.Test.atExactly { start = { row = 3, column = 30 }, end = { row = 3, column = 34 } }
                                |> Review.Test.whenFixed """module Mixed exposing (..)
import Html exposing (..)
import Utils.Helper exposing (formatString, parseNumber)

view : String -> Html msg
view text = 
    div [] [ Html.text (formatString text) ]
"""
                            ]
                          )
                        ]
        ]
