# review-common

Provides common linting rules for [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/).


## Provided rules

- [`NoExposingEverything`](https://package.elm-lang.org/packages/jfmengels/review-common/1.0.0/NoExposingEverything) - Forbids exporting everything from a module.
- [`NoImportingEverything`](https://package.elm-lang.org/packages/jfmengels/review-common/1.0.0/NoImportingEverything) - Forbids importing everything from a module.
- [`NoMissingTypeAnnotation`](https://package.elm-lang.org/packages/jfmengels/review-common/1.0.0/NoMissingTypeAnnotation) - Reports top-level declarations that do not have a type annotation.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    ]
```
