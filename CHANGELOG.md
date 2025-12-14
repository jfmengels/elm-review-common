# Changelog

## [Unreleased]

- [`NoPrematureLetComputation`] can now move let declarations whose pattern introduces multiple values.
In the example below, only (1) could get moved, but with this update (2) can get moved as well.
```elm
-- (1)
let
  { x } = value
in
some code

-- (2)
let
  { x, y } = value
in
some code
```
- [`NoPrematureLetComputation`] can now move let declarations that have names in their scope.
This was previously purposefully reported but not automatically fixed, as moving a declaration could introduce compiler errors about shadowing.
```elm
let
  fn a =
    a
in
case value of
  Just a ->
    fn a
  Nothing ->
    Nothing
-->
case value of
  Just a ->
    let
      fn a = -- `a` is already defined two lines above
        a
    in
    fn a
  Nothing ->
    Nothing
```
We now detect whether such a compiler error would occur and only prevent the fix in that case.
- Corrected an automatic fix for [`NoPrematureLetComputation`] that resulted in incorrect syntax.

## [1.3.4] - 2025-11-21

Added an automatic fix for [`NoImportingEverything`]. Thanks to [@sparksp](https://github.com/sparksp) and [@jackhp95](https://github.com/jackhp95) for their help!

## [1.3.3] - 2023-05-05

Turn [`NoDeprecated`] into an insight rule to help tackle deprecated usages ([#20](https://github.com/jfmengels/elm-review-common/pull/20)).

## [1.3.2] - 2022-11-08

Add better support for `jfmengels/elm-review` v2.10.0.

## [1.3.1] - 2022-09-13

This makes the [`NoMissingTypeExpose`] rule faster.

## [1.3.0] - 2022-09-13

Adds new rule [`NoConfusingPrefixOperator`] ([#13](https://github.com/jfmengels/elm-review/pull/13))


## Missing changelog

Help would be appreciated to fill the blanks!

[Unreleased]: https://github.com/jfmengels/elm-review-common/compare/1.3.4...HEAD
[1.3.4]: https://github.com/jfmengels/elm-review-common/releases/tag/1.3.4
[1.3.3]: https://github.com/jfmengels/elm-review-common/releases/tag/1.3.3
[1.3.2]: https://github.com/jfmengels/elm-review-common/releases/tag/1.3.2
[1.3.1]: https://github.com/jfmengels/elm-review-common/releases/tag/1.3.1
[1.3.0]: https://github.com/jfmengels/elm-review-common/releases/tag/1.3.0

[`NoConfusingPrefixOperator`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-common/latest/NoConfusingPrefixOperator)
[`NoDeprecated`]: (https://package.lang.org/packages/jfmengels/elm-review-common/latest/NoDeprecated)
[`NoImportingEverything`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-common/latest/NoImportingEverything)
[`NoMissingTypeExpose`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-common/latest/NoMissingTypeExpose)
[`NoPrematureLetComputation`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-common/latest/NoPrematureLetComputation)
