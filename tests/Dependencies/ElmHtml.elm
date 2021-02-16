module Dependencies.ElmHtml exposing (dependency)

import Elm.Docs
import Elm.Project
import Json.Decode as Decode
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create
        "elm/html"
        (createElmJsonProject elmJson)
        dependencyModules


createElmJsonProject : String -> Elm.Project.Project
createElmJsonProject rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            project

        Err error ->
            Debug.todo ("[elm.json]: " ++ Debug.toString error)


dependencyModules : List Elm.Docs.Module
dependencyModules =
    case Decode.decodeString (Decode.list Elm.Docs.decoder) docsJson of
        Ok modules ->
            modules

        Err error ->
            Debug.todo ("[docs.json]: " ++ Debug.toString error)


elmJson : String
elmJson =
    """{
    "type": "package",
    "name": "elm/html",
    "summary": "Fast HTML, rendered with virtual DOM diffing",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": {
        "HTML": [
            "Html",
            "Html.Attributes",
            "Html.Events"
        ],
        "Optimize": [
            "Html.Keyed",
            "Html.Lazy"
        ]
    },
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0",
        "elm/json": "1.0.0 <= v < 2.0.0",
        "elm/virtual-dom": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}
"""


docsJson : String
docsJson =
    """[{
  "name": "Html",
  "comment": " This file is organized roughly in order of popularity. The tags which you'd\\nexpect to use frequently will be closer to the top.\\n\\n# Primitives\\n@docs Html, Attribute, text, node, map\\n\\n# Tags\\n\\n## Headers\\n@docs h1, h2, h3, h4, h5, h6\\n\\n## Grouping Content\\n@docs div, p, hr, pre, blockquote\\n\\n## Text\\n@docs span, a, code, em, strong, i, b, u, sub, sup, br\\n\\n## Lists\\n@docs ol, ul, li, dl, dt, dd\\n\\n## Embedded Content\\n@docs img, iframe, canvas, math\\n\\n## Inputs\\n@docs form, input, textarea, button, select, option\\n\\n## Sections\\n@docs section, nav, article, aside, header, footer, address, main_\\n\\n## Figures\\n@docs figure, figcaption\\n\\n## Tables\\n@docs table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th\\n\\n\\n## Less Common Elements\\n\\n### Less Common Inputs\\n@docs fieldset, legend, label, datalist, optgroup, output, progress, meter\\n\\n### Audio and Video\\n@docs audio, video, source, track\\n\\n### Embedded Objects\\n@docs embed, object, param\\n\\n### Text Edits\\n@docs ins, del\\n\\n### Semantic Text\\n@docs small, cite, dfn, abbr, time, var, samp, kbd, s, q\\n\\n### Less Common Text Tags\\n@docs mark, ruby, rt, rp, bdi, bdo, wbr\\n\\n## Interactive Elements\\n@docs details, summary, menuitem, menu\\n\\n",
  "unions": [],
  "aliases": [{
    "name": "Attribute",
    "comment": " Set attributes on your `Html`. Learn more in the\\n[`Html.Attributes`](Html-Attributes) module.\\n",
    "args": ["msg"],
    "type": "VirtualDom.Attribute msg"
  }, {
    "name": "Html",
    "comment": " The core building block used to build up HTML. Here we create an `Html`\\nvalue with no attributes and one child:\\n\\n    hello : Html msg\\n    hello =\\n      div [] [ text \\"Hello!\\" ]\\n",
    "args": ["msg"],
    "type": "VirtualDom.Node msg"
  }],
  "values": [{
    "name": "a",
    "comment": " Represents a hyperlink, linking to another resource. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "abbr",
    "comment": " Represents an abbreviation or an acronym; the expansion of the\\nabbreviation can be represented in the title attribute.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "address",
    "comment": " Defines a section containing contact information. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "article",
    "comment": " Defines self-contained content that could exist independently of the rest\\nof the content.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "aside",
    "comment": " Defines some content loosely related to the page content. If it is removed,\\nthe remaining content still makes sense.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "audio",
    "comment": " Represents a sound or audio stream. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "b",
    "comment": " Represents a text which to which attention is drawn for utilitarian\\npurposes. It doesn't convey extra importance and doesn't imply an alternate\\nvoice.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "bdi",
    "comment": " Represents text that must be isolated from its surrounding for\\nbidirectional text formatting. It allows embedding a span of text with a\\ndifferent, or unknown, directionality.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "bdo",
    "comment": " Represents the directionality of its children, in order to explicitly\\noverride the Unicode bidirectional algorithm.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "blockquote",
    "comment": " Represents a content that is quoted from another source. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "br",
    "comment": " Represents a line break. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "button",
    "comment": " Represents a button. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "canvas",
    "comment": " Represents a bitmap area for graphics rendering. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "caption",
    "comment": " Represents the title of a table. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "cite",
    "comment": " Represents the title of a work. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "code",
    "comment": " Represents computer code. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "col",
    "comment": " Represents a column of a table. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "colgroup",
    "comment": " Represents a set of one or more columns of a table. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "datalist",
    "comment": " Represents a set of predefined options for other controls. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "dd",
    "comment": " Represents the definition of the terms immediately listed before it. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "del",
    "comment": " Defines a removal from the document. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "details",
    "comment": " Represents a widget from which the user can obtain additional information\\nor controls.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "dfn",
    "comment": " Represents a term whose definition is contained in its nearest ancestor\\ncontent.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "div",
    "comment": " Represents a generic container with no special meaning. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "dl",
    "comment": " Defines a definition list, that is, a list of terms and their associated\\ndefinitions.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "dt",
    "comment": " Represents a term defined by the next `dd`. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "em",
    "comment": " Represents emphasized text, like a stress accent. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "embed",
    "comment": " Represents a integration point for an external, often non-HTML,\\napplication or interactive content.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "fieldset",
    "comment": " Represents a set of controls. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "figcaption",
    "comment": " Represents the legend of a figure. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "figure",
    "comment": " Represents a figure illustrated as part of the document. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "footer",
    "comment": " Defines the footer for a page or section. It often contains a copyright\\nnotice, some links to legal information, or addresses to give feedback.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "form",
    "comment": " Represents a form, consisting of controls, that can be submitted to a\\nserver for processing.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "h1",
    "comment": "",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "h2",
    "comment": "",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "h3",
    "comment": "",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "h4",
    "comment": "",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "h5",
    "comment": "",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "h6",
    "comment": "",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "header",
    "comment": " Defines the header of a page or section. It often contains a logo, the\\ntitle of the web site, and a navigational table of content.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "hr",
    "comment": " Represents a thematic break between paragraphs of a section or article or\\nany longer content.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "i",
    "comment": " Represents some text in an alternate voice or mood, or at least of\\ndifferent quality, such as a taxonomic designation, a technical term, an\\nidiomatic phrase, a thought, or a ship name.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "iframe",
    "comment": " Embedded an HTML document. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "img",
    "comment": " Represents an image. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "input",
    "comment": " Represents a typed data field allowing the user to edit the data. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "ins",
    "comment": " Defines an addition to the document. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "kbd",
    "comment": " Represents user input, often from the keyboard, but not necessarily; it\\nmay represent other input, like transcribed voice commands.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "label",
    "comment": " Represents the caption of a form control. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "legend",
    "comment": " Represents the caption for a `fieldset`. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "li",
    "comment": " Defines a item of an enumeration list. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "main_",
    "comment": " Defines the main or important content in the document. There is only one\\n`main` element in the document.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "map",
    "comment": " Transform the messages produced by some `Html`. In the following example,\\nwe have `viewButton` that produces `()` messages, and we transform those values\\ninto `Msg` values in `view`.\\n\\n    type Msg = Left | Right\\n\\n    view : model -> Html Msg\\n    view model =\\n      div []\\n        [ map (\\\\_ -> Left) (viewButton \\"Left\\")\\n        , map (\\\\_ -> Right) (viewButton \\"Right\\")\\n        ]\\n\\n    viewButton : String -> Html ()\\n    viewButton name =\\n      button [ onClick () ] [ text name ]\\n\\nThis should not come in handy too often. Definitely read [this][reuse] before\\ndeciding if this is what you want.\\n\\n[reuse]: https://guide.elm-lang.org/reuse/\\n",
    "type": "(a -> msg) -> Html.Html a -> Html.Html msg"
  }, {
    "name": "mark",
    "comment": " Represents text highlighted for reference purposes, that is for its\\nrelevance in another context.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "math",
    "comment": " Defines a mathematical formula. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "menu",
    "comment": " Represents a list of commands. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "menuitem",
    "comment": " Represents a command that the user can invoke. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "meter",
    "comment": " Represents a scalar measurement (or a fractional value), within a known\\nrange.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "nav",
    "comment": " Defines a section that contains only navigation links.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "node",
    "comment": " General way to create HTML nodes. It is used to define all of the helper\\nfunctions in this library.\\n\\n    div : List (Attribute msg) -> List (Html msg) -> Html msg\\n    div attributes children =\\n        node \\"div\\" attributes children\\n\\nYou can use this to create custom nodes if you need to create something that\\nis not covered by the helper functions in this library.\\n",
    "type": "String.String -> List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "object",
    "comment": " Represents an external resource, which is treated as an image, an HTML\\nsub-document, or an external resource to be processed by a plug-in.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "ol",
    "comment": " Defines an ordered list of items. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "optgroup",
    "comment": " Represents a set of options, logically grouped. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "option",
    "comment": " Represents an option in a `select` element or a suggestion of a `datalist`\\nelement.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "output",
    "comment": " Represents the result of a calculation. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "p",
    "comment": " Defines a portion that should be displayed as a paragraph. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "param",
    "comment": " Defines parameters for use by plug-ins invoked by `object` elements. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "pre",
    "comment": " Indicates that its content is preformatted and that this format must be\\npreserved.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "progress",
    "comment": " Represents the completion progress of a task. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "q",
    "comment": " Represents an inline quotation. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "rp",
    "comment": " Represents parenthesis around a ruby annotation, used to display the\\nannotation in an alternate way by browsers not supporting the standard display\\nfor annotations.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "rt",
    "comment": " Represents the text of a ruby annotation. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "ruby",
    "comment": " Represents content to be marked with ruby annotations, short runs of text\\npresented alongside the text. This is often used in conjunction with East Asian\\nlanguage where the annotations act as a guide for pronunciation, like the\\nJapanese furigana.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "s",
    "comment": " Represents content that is no longer accurate or relevant. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "samp",
    "comment": " Represents the output of a program or a computer. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "section",
    "comment": " Defines a section in a document.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "select",
    "comment": " Represents a control allowing selection among a set of options. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "small",
    "comment": " Represents a side comment, that is, text like a disclaimer or a\\ncopyright, which is not essential to the comprehension of the document.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "source",
    "comment": " Allows authors to specify alternative media resources for media elements\\nlike `video` or `audio`.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "span",
    "comment": " Represents text with no specific meaning. This has to be used when no other\\ntext-semantic element conveys an adequate meaning, which, in this case, is\\noften brought by global attributes like `class`, `lang`, or `dir`.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "strong",
    "comment": " Represents especially important text. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "sub",
    "comment": " Represent a subscript. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "summary",
    "comment": " Represents a summary, caption, or legend for a given `details`. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "sup",
    "comment": " Represent a superscript. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "table",
    "comment": " Represents data with more than one dimension. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "tbody",
    "comment": " Represents the block of rows that describes the concrete data of a table.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "td",
    "comment": " Represents a data cell in a table. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "text",
    "comment": " Just put plain text in the DOM. It will escape the string so that it appears\\nexactly as you specify.\\n\\n    text \\"Hello World!\\"\\n",
    "type": "String.String -> Html.Html msg"
  }, {
    "name": "textarea",
    "comment": " Represents a multiline text edit control. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "tfoot",
    "comment": " Represents the block of rows that describes the column summaries of a table.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "th",
    "comment": " Represents a header cell in a table. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "thead",
    "comment": " Represents the block of rows that describes the column labels of a table.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "time",
    "comment": " Represents a date and time value; the machine-readable equivalent can be\\nrepresented in the datetime attribute.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "tr",
    "comment": " Represents a row of cells in a table. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "track",
    "comment": " Allows authors to specify timed text track for media elements like `video`\\nor `audio`.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "u",
    "comment": " Represents a non-textual annotation for which the conventional\\npresentation is underlining, such labeling the text as being misspelt or\\nlabeling a proper name in Chinese text.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "ul",
    "comment": " Defines an unordered list of items. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "var",
    "comment": " Represents a variable. Specific cases where it should be used include an\\nactual mathematical expression or programming context, an identifier\\nrepresenting a constant, a symbol identifying a physical quantity, a function\\nparameter, or a mere placeholder in prose.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "video",
    "comment": " Represents a video, the associated audio and captions, and controls. ",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }, {
    "name": "wbr",
    "comment": " Represents a line break opportunity, that is a suggested point for\\nwrapping text in order to improve readability of text split on several lines.\\n",
    "type": "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
  }],
  "binops": []
}, {
  "name": "Html.Attributes",
  "comment": " Helper functions for HTML attributes. They are organized roughly by\\ncategory. Each attribute is labeled with the HTML tags it can be used with, so\\njust search the page for `video` if you want video stuff.\\n\\n# Primitives\\n@docs style, property, attribute, map\\n\\n# Super Common Attributes\\n@docs class, classList, id, title, hidden\\n\\n# Inputs\\n@docs type_, value, checked, placeholder, selected\\n\\n## Input Helpers\\n@docs accept, acceptCharset, action, autocomplete, autofocus,\\n    disabled, enctype, list, maxlength, minlength, method, multiple,\\n    name, novalidate, pattern, readonly, required, size, for, form\\n\\n## Input Ranges\\n@docs max, min, step\\n\\n## Input Text Areas\\n@docs cols, rows, wrap\\n\\n\\n# Links and Areas\\n@docs href, target, download, hreflang, media, ping, rel\\n\\n## Maps\\n@docs ismap, usemap, shape, coords\\n\\n\\n# Embedded Content\\n@docs src, height, width, alt\\n\\n## Audio and Video\\n@docs autoplay, controls, loop, preload, poster, default, kind, srclang\\n\\n## iframes\\n@docs sandbox, srcdoc\\n\\n# Ordered Lists\\n@docs reversed, start\\n\\n# Tables\\n@docs align, colspan, rowspan, headers, scope\\n\\n# Less Common Global Attributes\\nAttributes that can be attached to any HTML tag but are less commonly used.\\n@docs accesskey, contenteditable, contextmenu, dir, draggable, dropzone,\\n      itemprop, lang, spellcheck, tabindex\\n\\n# Miscellaneous\\n@docs cite, datetime, pubdate, manifest\\n\\n",
  "unions": [],
  "aliases": [],
  "values": [{
    "name": "accept",
    "comment": " List of types the server accepts, typically a file type.\\nFor `form` and `input`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "acceptCharset",
    "comment": " List of supported charsets in a `form`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "accesskey",
    "comment": " Defines a keyboard shortcut to activate or add focus to the element. ",
    "type": "Char.Char -> Html.Attribute msg"
  }, {
    "name": "action",
    "comment": " The URI of a program that processes the information submitted via a `form`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "align",
    "comment": " Specifies the horizontal alignment of a `caption`, `col`, `colgroup`,\\n`hr`, `iframe`, `img`, `table`, `tbody`,  `td`,  `tfoot`, `th`, `thead`, or\\n`tr`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "alt",
    "comment": " Alternative text in case an image can't be displayed. Works with `img`,\\n`area`, and `input`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "attribute",
    "comment": " Create *attributes*, like saying `domNode.setAttribute('class', 'greeting')`\\nin JavaScript.\\n\\n    class : String -> Attribute msg\\n    class name =\\n      attribute \\"class\\" name\\n\\nRead more about the difference between properties and attributes [here][].\\n\\n[here]: https://github.com/elm/html/blob/master/properties-vs-attributes.md\\n",
    "type": "String.String -> String.String -> Html.Attribute msg"
  }, {
    "name": "autocomplete",
    "comment": " Indicates whether a `form` or an `input` can have their values automatically\\ncompleted by the browser.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "autofocus",
    "comment": " The element should be automatically focused after the page loaded.\\nFor `button`, `input`, `select`, and `textarea`.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "autoplay",
    "comment": " The `audio` or `video` should play as soon as possible. ",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "checked",
    "comment": " Indicates whether an `input` of type checkbox is checked. ",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "cite",
    "comment": " Contains a URI which points to the source of the quote or change in a\\n`blockquote`, `del`, `ins`, or `q`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "class",
    "comment": " Often used with CSS to style elements with common properties.\\n\\n**Note:** You can have as many `class` and `classList` attributes as you want.\\nThey all get applied, so if you say `[ class \\"notice\\", class \\"notice-seen\\" ]`\\nyou will get both classes!\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "classList",
    "comment": " This function makes it easier to build a space-separated class attribute.\\nEach class can easily be added and removed depending on the boolean value it\\nis paired with. For example, maybe we want a way to view notices:\\n\\n    viewNotice : Notice -> Html msg\\n    viewNotice notice =\\n      div\\n        [ classList\\n            [ (\\"notice\\", True)\\n            , (\\"notice-important\\", notice.isImportant)\\n            , (\\"notice-seen\\", notice.isSeen)\\n            ]\\n        ]\\n        [ text notice.content ]\\n\\n**Note:** You can have as many `class` and `classList` attributes as you want.\\nThey all get applied, so if you say `[ class \\"notice\\", class \\"notice-seen\\" ]`\\nyou will get both classes!\\n",
    "type": "List.List ( String.String, Basics.Bool ) -> Html.Attribute msg"
  }, {
    "name": "cols",
    "comment": " Defines the number of columns in a `textarea`. ",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "colspan",
    "comment": " The colspan attribute defines the number of columns a cell should span.\\nFor `td` and `th`.\\n",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "contenteditable",
    "comment": " Indicates whether the element's content is editable. ",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "contextmenu",
    "comment": " Defines the ID of a `menu` element which will serve as the element's\\ncontext menu.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "controls",
    "comment": " Indicates whether the browser should show playback controls for the `audio`\\nor `video`.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "coords",
    "comment": " A set of values specifying the coordinates of the hot-spot region in an\\n`area`. Needs to be paired with a `shape` attribute to be meaningful.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "datetime",
    "comment": " Indicates the date and time associated with the element.\\nFor `del`, `ins`, `time`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "default",
    "comment": " Indicates that the `track` should be enabled unless the user's preferences\\nindicate something different.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "dir",
    "comment": " Defines the text direction. Allowed values are ltr (Left-To-Right) or rtl\\n(Right-To-Left).\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "disabled",
    "comment": " Indicates whether the user can interact with a `button`, `fieldset`,\\n`input`, `optgroup`, `option`, `select` or `textarea`.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "download",
    "comment": " Indicates that clicking an `a` and `area` will download the resource\\ndirectly. The `String` argument determins the name of the downloaded file.\\nSay the file you are serving is named `hats.json`.\\n\\n    download \\"\\"               -- hats.json\\n    download \\"my-hats.json\\"   -- my-hats.json\\n    download \\"snakes.json\\"    -- snakes.json\\n\\nThe empty `String` says to just name it whatever it was called on the server.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "draggable",
    "comment": " Defines whether the element can be dragged. ",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "dropzone",
    "comment": " Indicates that the element accept the dropping of content on it. ",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "enctype",
    "comment": " How `form` data should be encoded when submitted with the POST method.\\nOptions include: application/x-www-form-urlencoded, multipart/form-data, and\\ntext/plain.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "for",
    "comment": " The element ID described by this `label` or the element IDs that are used\\nfor an `output`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "form",
    "comment": " Indicates the element ID of the `form` that owns this particular `button`,\\n`fieldset`, `input`, `label`, `meter`, `object`, `output`, `progress`,\\n`select`, or `textarea`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "headers",
    "comment": " A space separated list of element IDs indicating which `th` elements are\\nheaders for this cell. For `td` and `th`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "height",
    "comment": " Declare the height of a `canvas`, `embed`, `iframe`, `img`, `input`,\\n`object`, or `video`.\\n",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "hidden",
    "comment": " Indicates the relevance of an element. ",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "href",
    "comment": " The URL of a linked resource, such as `a`, `area`, `base`, or `link`. ",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "hreflang",
    "comment": " Two-letter language code of the linked resource of an `a`, `area`, or `link`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "id",
    "comment": " Often used with CSS to style a specific element. The value of this\\nattribute must be unique.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "ismap",
    "comment": " When an `img` is a descendant of an `a` tag, the `ismap` attribute\\nindicates that the click location should be added to the parent `a`'s href as\\na query string.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "itemprop",
    "comment": "",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "kind",
    "comment": " Specifies the kind of text `track`. ",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "lang",
    "comment": " Defines the language used in the element. ",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "list",
    "comment": " Associates an `input` with a `datalist` tag. The datalist gives some\\npre-defined options to suggest to the user as they interact with an input.\\nThe value of the list attribute must match the id of a `datalist` node.\\nFor `input`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "loop",
    "comment": " Indicates whether the `audio` or `video` should start playing from the\\nstart when it's finished.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "manifest",
    "comment": " Specifies the URL of the cache manifest for an `html` tag. ",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "map",
    "comment": " Transform the messages produced by an `Attribute`.\\n",
    "type": "(a -> msg) -> Html.Attribute a -> Html.Attribute msg"
  }, {
    "name": "max",
    "comment": " Indicates the maximum value allowed. When using an input of type number or\\ndate, the max value must be a number or date. For `input`, `meter`, and `progress`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "maxlength",
    "comment": " Defines the maximum number of characters allowed in an `input` or\\n`textarea`.\\n",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "media",
    "comment": " Specifies a hint of the target media of a `a`, `area`, `link`, `source`,\\nor `style`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "method",
    "comment": " Defines which HTTP method to use when submitting a `form`. Can be GET\\n(default) or POST.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "min",
    "comment": " Indicates the minimum value allowed. When using an input of type number or\\ndate, the min value must be a number or date. For `input` and `meter`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "minlength",
    "comment": " Defines the minimum number of characters allowed in an `input` or\\n`textarea`.\\n",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "multiple",
    "comment": " Indicates whether multiple values can be entered in an `input` of type\\nemail or file. Can also indicate that you can `select` many options.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "name",
    "comment": " Name of the element. For example used by the server to identify the fields\\nin form submits. For `button`, `form`, `fieldset`, `iframe`, `input`,\\n`object`, `output`, `select`, `textarea`, `map`, `meta`, and `param`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "novalidate",
    "comment": " This attribute indicates that a `form` shouldn't be validated when\\nsubmitted.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "pattern",
    "comment": " Defines a regular expression which an `input`'s value will be validated\\nagainst.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "ping",
    "comment": " Specify a URL to send a short POST request to when the user clicks on an\\n`a` or `area`. Useful for monitoring and tracking.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "placeholder",
    "comment": " Provides a hint to the user of what can be entered into an `input` or\\n`textarea`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "poster",
    "comment": " A URL indicating a poster frame to show until the user plays or seeks the\\n`video`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "preload",
    "comment": " Control how much of an `audio` or `video` resource should be preloaded. ",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "property",
    "comment": " Create *properties*, like saying `domNode.className = 'greeting'` in\\nJavaScript.\\n\\n    import Json.Encode as Encode\\n\\n    class : String -> Attribute msg\\n    class name =\\n      property \\"className\\" (Encode.string name)\\n\\nRead more about the difference between properties and attributes [here][].\\n\\n[here]: https://github.com/elm/html/blob/master/properties-vs-attributes.md\\n",
    "type": "String.String -> Json.Encode.Value -> Html.Attribute msg"
  }, {
    "name": "pubdate",
    "comment": " Indicates whether this date and time is the date of the nearest `article`\\nancestor element. For `time`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "readonly",
    "comment": " Indicates whether an `input` or `textarea` can be edited. ",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "rel",
    "comment": " Specifies the relationship of the target object to the link object.\\nFor `a`, `area`, `link`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "required",
    "comment": " Indicates whether this element is required to fill out or not.\\nFor `input`, `select`, and `textarea`.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "reversed",
    "comment": " Indicates whether an ordered list `ol` should be displayed in a descending\\norder instead of a ascending.\\n",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "rows",
    "comment": " Defines the number of rows in a `textarea`. ",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "rowspan",
    "comment": " Defines the number of rows a table cell should span over.\\nFor `td` and `th`.\\n",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "sandbox",
    "comment": " A space separated list of security restrictions you'd like to lift for an\\n`iframe`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "scope",
    "comment": " Specifies the scope of a header cell `th`. Possible values are: col, row,\\ncolgroup, rowgroup.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "selected",
    "comment": " Defines which `option` will be selected on page load. ",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "shape",
    "comment": " Declare the shape of the clickable area in an `a` or `area`. Valid values\\ninclude: default, rect, circle, poly. This attribute can be paired with\\n`coords` to create more particular shapes.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "size",
    "comment": " For `input` specifies the width of an input in characters.\\n\\nFor `select` specifies the number of visible options in a drop-down list.\\n",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "spellcheck",
    "comment": " Indicates whether spell checking is allowed for the element. ",
    "type": "Basics.Bool -> Html.Attribute msg"
  }, {
    "name": "src",
    "comment": " The URL of the embeddable content. For `audio`, `embed`, `iframe`, `img`,\\n`input`, `script`, `source`, `track`, and `video`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "srcdoc",
    "comment": " An HTML document that will be displayed as the body of an `iframe`. It will\\noverride the content of the `src` attribute if it has been specified.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "srclang",
    "comment": " A two letter language code indicating the language of the `track` text data.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "start",
    "comment": " Defines the first number of an ordered list if you want it to be something\\nbesides 1.\\n",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "step",
    "comment": " Add a step size to an `input`. Use `step \\"any\\"` to allow any floating-point\\nnumber to be used in the input.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "style",
    "comment": " Specify a style.\\n\\n    greeting : Node msg\\n    greeting =\\n      div\\n        [ style \\"background-color\\" \\"red\\"\\n        , style \\"height\\" \\"90px\\"\\n        , style \\"width\\" \\"100%\\"\\n        ]\\n        [ text \\"Hello!\\"\\n        ]\\n\\nThere is no `Html.Styles` module because best practices for working with HTML\\nsuggest that this should primarily be specified in CSS files. So the general\\nrecommendation is to use this function lightly.\\n",
    "type": "String.String -> String.String -> Html.Attribute msg"
  }, {
    "name": "tabindex",
    "comment": " Overrides the browser's default tab order and follows the one specified\\ninstead.\\n",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "target",
    "comment": " Specify where the results of clicking an `a`, `area`, `base`, or `form`\\nshould appear. Possible special values include:\\n\\n  * _blank &mdash; a new window or tab\\n  * _self &mdash; the same frame (this is default)\\n  * _parent &mdash; the parent frame\\n  * _top &mdash; the full body of the window\\n\\nYou can also give the name of any `frame` you have created.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "title",
    "comment": " Text to be displayed in a tooltip when hovering over the element. ",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "type_",
    "comment": " Defines the type of a `button`, `input`, `embed`, `object`, `script`,\\n`source`, `style`, or `menu`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "usemap",
    "comment": " Specify the hash name reference of a `map` that should be used for an `img`\\nor `object`. A hash name reference is a hash symbol followed by the element's name or id.\\nE.g. `\\"#planet-map\\"`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "value",
    "comment": " Defines a default value which will be displayed in a `button`, `option`,\\n`input`, `li`, `meter`, `progress`, or `param`.\\n",
    "type": "String.String -> Html.Attribute msg"
  }, {
    "name": "width",
    "comment": " Declare the width of a `canvas`, `embed`, `iframe`, `img`, `input`,\\n`object`, or `video`.\\n",
    "type": "Basics.Int -> Html.Attribute msg"
  }, {
    "name": "wrap",
    "comment": " Indicates whether the text should be wrapped in a `textarea`. Possible\\nvalues are \\"hard\\" and \\"soft\\".\\n",
    "type": "String.String -> Html.Attribute msg"
  }],
  "binops": []
}, {
  "name": "Html.Events",
  "comment": "\\nIt is often helpful to create an [Union Type][] so you can have many different kinds\\nof events as seen in the [TodoMVC][] example.\\n\\n[Union Type]: https://elm-lang.org/learn/Union-Types.elm\\n[TodoMVC]: https://github.com/evancz/elm-todomvc/blob/master/Todo.elm\\n\\n# Mouse\\n@docs onClick, onDoubleClick,\\n      onMouseDown, onMouseUp,\\n      onMouseEnter, onMouseLeave,\\n      onMouseOver, onMouseOut\\n\\n# Forms\\n@docs onInput, onCheck, onSubmit\\n\\n# Focus\\n@docs onBlur, onFocus\\n\\n# Custom\\n@docs on, stopPropagationOn, preventDefaultOn, custom\\n\\n## Custom Decoders\\n@docs targetValue, targetChecked, keyCode\\n",
  "unions": [],
  "aliases": [],
  "values": [{
    "name": "custom",
    "comment": " Create an event listener that may [`stopPropagation`][stop] or\\n[`preventDefault`][prevent].\\n\\n[stop]: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation\\n[prevent]: https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault\\n\\n**Note:** If you need something even more custom (like capture phase) check\\nout the lower-level event API in `elm/virtual-dom`.\\n",
    "type": "String.String -> Json.Decode.Decoder { message : msg, stopPropagation : Basics.Bool, preventDefault : Basics.Bool } -> Html.Attribute msg"
  }, {
    "name": "keyCode",
    "comment": " A `Json.Decoder` for grabbing `event.keyCode`. This helps you define\\nkeyboard listeners like this:\\n\\n    import Json.Decode as Json\\n\\n    onKeyUp : (Int -> msg) -> Attribute msg\\n    onKeyUp tagger =\\n      on \\"keyup\\" (Json.map tagger keyCode)\\n\\n**Note:** It looks like the spec is moving away from `event.keyCode` and\\ntowards `event.key`. Once this is supported in more browsers, we may add\\nhelpers here for `onKeyUp`, `onKeyDown`, `onKeyPress`, etc.\\n",
    "type": "Json.Decode.Decoder Basics.Int"
  }, {
    "name": "on",
    "comment": " Create a custom event listener. Normally this will not be necessary, but\\nyou have the power! Here is how `onClick` is defined for example:\\n\\n    import Json.Decode as Decode\\n\\n    onClick : msg -> Attribute msg\\n    onClick message =\\n      on \\"click\\" (Decode.succeed message)\\n\\nThe first argument is the event name in the same format as with JavaScript's\\n[`addEventListener`][aEL] function.\\n\\nThe second argument is a JSON decoder. Read more about these [here][decoder].\\nWhen an event occurs, the decoder tries to turn the event object into an Elm\\nvalue. If successful, the value is routed to your `update` function. In the\\ncase of `onClick` we always just succeed with the given `message`.\\n\\nIf this is confusing, work through the [Elm Architecture Tutorial][tutorial].\\nIt really helps!\\n\\n[aEL]: https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener\\n[decoder]: /packages/elm/json/latest/Json-Decode\\n[tutorial]: https://github.com/evancz/elm-architecture-tutorial/\\n\\n**Note:** This creates a [passive][] event listener, enabling optimizations for\\ntouch, scroll, and wheel events in some browsers.\\n\\n[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md\\n",
    "type": "String.String -> Json.Decode.Decoder msg -> Html.Attribute msg"
  }, {
    "name": "onBlur",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onCheck",
    "comment": " Detect [change](https://developer.mozilla.org/en-US/docs/Web/Events/change)\\nevents on checkboxes. It will grab the boolean value from `event.target.checked`\\non any input event.\\n\\nCheck out [`targetChecked`](#targetChecked) for more details on how this works.\\n",
    "type": "(Basics.Bool -> msg) -> Html.Attribute msg"
  }, {
    "name": "onClick",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onDoubleClick",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onFocus",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onInput",
    "comment": " Detect [input](https://developer.mozilla.org/en-US/docs/Web/Events/input)\\nevents for things like text fields or text areas.\\n\\nFor more details on how `onInput` works, check out [`targetValue`](#targetValue).\\n\\n**Note 1:** It grabs the **string** value at `event.target.value`, so it will\\nnot work if you need some other information. For example, if you want to track\\ninputs on a range slider, make a custom handler with [`on`](#on).\\n\\n**Note 2:** It uses `stopPropagationOn` internally to always stop propagation\\nof the event. This is important for complicated reasons explained [here][1] and\\n[here][2].\\n\\n[1]: /packages/elm/virtual-dom/latest/VirtualDom#Handler\\n[2]: https://github.com/elm/virtual-dom/issues/125\\n",
    "type": "(String.String -> msg) -> Html.Attribute msg"
  }, {
    "name": "onMouseDown",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onMouseEnter",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onMouseLeave",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onMouseOut",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onMouseOver",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onMouseUp",
    "comment": "",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "onSubmit",
    "comment": " Detect a [submit](https://developer.mozilla.org/en-US/docs/Web/Events/submit)\\nevent with [`preventDefault`](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault)\\nin order to prevent the form from changing the page’s location. If you need\\ndifferent behavior, create a custom event handler.\\n",
    "type": "msg -> Html.Attribute msg"
  }, {
    "name": "preventDefaultOn",
    "comment": " Create an event listener that may [`preventDefault`][prevent]. Your decoder\\nmust produce a message and a `Bool` that decides if `preventDefault` should\\nbe called.\\n\\nFor example, the `onSubmit` function in this library *always* prevents the\\ndefault behavior:\\n\\n[prevent]: https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault\\n\\n    onSubmit : msg -> Attribute msg\\n    onSubmit msg =\\n      preventDefaultOn \\"submit\\" (Json.map alwaysPreventDefault (Json.succeed msg))\\n\\n    alwaysPreventDefault : msg -> ( msg, Bool )\\n    alwaysPreventDefault msg =\\n      ( msg, True )\\n",
    "type": "String.String -> Json.Decode.Decoder ( msg, Basics.Bool ) -> Html.Attribute msg"
  }, {
    "name": "stopPropagationOn",
    "comment": " Create an event listener that may [`stopPropagation`][stop]. Your decoder\\nmust produce a message and a `Bool` that decides if `stopPropagation` should\\nbe called.\\n\\n[stop]: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation\\n\\n**Note:** This creates a [passive][] event listener, enabling optimizations for\\ntouch, scroll, and wheel events in some browsers.\\n\\n[passive]: https://github.com/WICG/EventListenerOptions/blob/gh-pages/explainer.md\\n",
    "type": "String.String -> Json.Decode.Decoder ( msg, Basics.Bool ) -> Html.Attribute msg"
  }, {
    "name": "targetChecked",
    "comment": " A `Json.Decoder` for grabbing `event.target.checked`. We use this to define\\n`onCheck` as follows:\\n\\n    import Json.Decode as Json\\n\\n    onCheck : (Bool -> msg) -> Attribute msg\\n    onCheck tagger =\\n      on \\"input\\" (Json.map tagger targetChecked)\\n",
    "type": "Json.Decode.Decoder Basics.Bool"
  }, {
    "name": "targetValue",
    "comment": " A `Json.Decoder` for grabbing `event.target.value`. We use this to define\\n`onInput` as follows:\\n\\n    import Json.Decode as Json\\n\\n    onInput : (String -> msg) -> Attribute msg\\n    onInput tagger =\\n      stopPropagationOn \\"input\\" <|\\n        Json.map alwaysStop (Json.map tagger targetValue)\\n\\n    alwaysStop : a -> (a, Bool)\\n    alwaysStop x =\\n      (x, True)\\n\\nYou probably will never need this, but hopefully it gives some insights into\\nhow to make custom event handlers.\\n",
    "type": "Json.Decode.Decoder String.String"
  }],
  "binops": []
}, {
  "name": "Html.Keyed",
  "comment": " A keyed node helps optimize cases where children are getting added, moved,\\nremoved, etc. Common examples include:\\n\\n  - The user can delete items from a list.\\n  - The user can create new items in a list.\\n  - You can sort a list based on name or date or whatever.\\n\\nWhen you use a keyed node, every child is paired with a string identifier. This\\nmakes it possible for the underlying diffing algorithm to reuse nodes more\\nefficiently.\\n\\n# Keyed Nodes\\n@docs node\\n\\n# Commonly Keyed Nodes\\n@docs ol, ul\\n",
  "unions": [],
  "aliases": [],
  "values": [{
    "name": "node",
    "comment": " Works just like `Html.node`, but you add a unique identifier to each child\\nnode. You want this when you have a list of nodes that is changing: adding\\nnodes, removing nodes, etc. In these cases, the unique identifiers help make\\nthe DOM modifications more efficient.\\n",
    "type": "String.String -> List.List (Html.Attribute msg) -> List.List ( String.String, Html.Html msg ) -> Html.Html msg"
  }, {
    "name": "ol",
    "comment": "",
    "type": "List.List (Html.Attribute msg) -> List.List ( String.String, Html.Html msg ) -> Html.Html msg"
  }, {
    "name": "ul",
    "comment": "",
    "type": "List.List (Html.Attribute msg) -> List.List ( String.String, Html.Html msg ) -> Html.Html msg"
  }],
  "binops": []
}, {
  "name": "Html.Lazy",
  "comment": " Since all Elm functions are pure we have a guarantee that the same input\\nwill always result in the same output. This module gives us tools to be lazy\\nabout building `Html` that utilize this fact.\\n\\nRather than immediately applying functions to their arguments, the `lazy`\\nfunctions just bundle the function and arguments up for later. When diffing\\nthe old and new virtual DOM, it checks to see if all the arguments are equal\\nby reference. If so, it skips calling the function!\\n\\nThis is a really cheap test and often makes things a lot faster, but definitely\\nbenchmark to be sure!\\n\\n@docs lazy, lazy2, lazy3, lazy4, lazy5, lazy6, lazy7, lazy8\\n\\n",
  "unions": [],
  "aliases": [],
  "values": [{
    "name": "lazy",
    "comment": " A performance optimization that delays the building of virtual DOM nodes.\\n\\nCalling `(view model)` will definitely build some virtual DOM, perhaps a lot of\\nit. Calling `(lazy view model)` delays the call until later. During diffing, we\\ncan check to see if `model` is referentially equal to the previous value used,\\nand if so, we just stop. No need to build up the tree structure and diff it,\\nwe know if the input to `view` is the same, the output must be the same!\\n",
    "type": "(a -> Html.Html msg) -> a -> Html.Html msg"
  }, {
    "name": "lazy2",
    "comment": " Same as `lazy` but checks on two arguments.\\n",
    "type": "(a -> b -> Html.Html msg) -> a -> b -> Html.Html msg"
  }, {
    "name": "lazy3",
    "comment": " Same as `lazy` but checks on three arguments.\\n",
    "type": "(a -> b -> c -> Html.Html msg) -> a -> b -> c -> Html.Html msg"
  }, {
    "name": "lazy4",
    "comment": " Same as `lazy` but checks on four arguments.\\n",
    "type": "(a -> b -> c -> d -> Html.Html msg) -> a -> b -> c -> d -> Html.Html msg"
  }, {
    "name": "lazy5",
    "comment": " Same as `lazy` but checks on five arguments.\\n",
    "type": "(a -> b -> c -> d -> e -> Html.Html msg) -> a -> b -> c -> d -> e -> Html.Html msg"
  }, {
    "name": "lazy6",
    "comment": " Same as `lazy` but checks on six arguments.\\n",
    "type": "(a -> b -> c -> d -> e -> f -> Html.Html msg) -> a -> b -> c -> d -> e -> f -> Html.Html msg"
  }, {
    "name": "lazy7",
    "comment": " Same as `lazy` but checks on seven arguments.\\n",
    "type": "(a -> b -> c -> d -> e -> f -> g -> Html.Html msg) -> a -> b -> c -> d -> e -> f -> g -> Html.Html msg"
  }, {
    "name": "lazy8",
    "comment": " Same as `lazy` but checks on eight arguments.\\n",
    "type": "(a -> b -> c -> d -> e -> f -> g -> h -> Html.Html msg) -> a -> b -> c -> d -> e -> f -> g -> h -> Html.Html msg"
  }],
  "binops": []
}]
"""
