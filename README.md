# levenshtein
[![Build Status of the package by GitHub actions](https://github.com/hapytex/levenshtein/actions/workflows/build-ci.yml/badge.svg)](https://github.com/hapytex/levenshtein/actions/workflows/build-ci.yml)
[![Build Status of the package by Hackage](https://matrix.hackage.haskell.org/api/v2/packages/levenshtein/badge)](https://matrix.hackage.haskell.org/#/package/levenshtein)
[![Hackage version badge](https://img.shields.io/hackage/v/levenshtein.svg)](https://hackage.haskell.org/package/levenshtein)

## Usage

The module `Data.Foldable.Levenshtein` exports a data type `Edit` that
represent the possible ways to edit a list by `Add`ing an element, `Rem`oving
an element, `Copy`ing (do nothing with the element), and `Swap` with a new value.

One can apply such edits to a list with the `applyEdits` function, for example:

```haskell
Prelude> applyEdits [Copy 1,Swap 3 4,Swap 0 2,Swap 2 5] [1,3,0,2]
Just [1,4,2,5]
```

We can also calculate the minimal list of edits necessary to turn one list into another one,
for example:

```haskell
Prelude> levenshtein [1,3,0,2] [1,4,2,5]
(3,[Copy 1,Swap 3 4,Swap 0 2,Swap 2 5])
```

here it means that the smallest edit distance is three, and that in order to transform
`[1,3,0,2]` to `[1,4,2,5]` we copy `1` change `3` for `4`, change `0` for `2`, and change `2` for `5`.

## Package structure

The package contains one module: **`Data.Foldable.Levenshtein`**.
This module provides functions to determine the edit distance and
a list of edits to turn one `Foldable` of items to another `Foldable`
of items. The foldables are first converted to a list, so the edits
always eventually produce a *list* of edits, even if (one of) the `Foldable`s
is for example a `Tree`, `Maybe`, etc.

Besides the `Edit` object, the module exports three types of functions:

 1. functions that return the edit distance together with a list of *reversed* edits;
 2. functions that return the edit distance with a list of edits (not reversed); and
 3. functions that only calculate the edit distance, not the edits itself.

The third type is more an optimized version of the first two types since it will
take less memory and finish slightly faster.

## `levenshtein` is *safe* Haskell

The `levenshtein` package does not work with arrays, vectors,
etc. but only vanilla lists, making this a safe package.

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/levenshtein).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).
