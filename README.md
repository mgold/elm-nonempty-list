# List.Nonempty for Elm

[![Build Status](https://travis-ci.org/mgold/elm-nonempty-list.svg?branch=master)](https://travis-ci.org/mgold/elm-nonempty-list)

A list that is known, at compile-time, to be nonempty. This means `head` and
`tail` are guaranteed to succeed and you don't have to carry Maybes throughout
your program.

```elm
import List.Nonempty exposing (..)

one : Nonempty Int
one = fromElement 2

two : Nonempty Int
two = cons 4 one

toList two --> [4, 2]

head two --> 4

tail two --> [2]

toList (reverse two) --> [2, 4]

toList (dropTail two) --> [4]

member 4 two --> True

foldl1 (+) two --> 6
```

For actual usage, I recommend `import List.Nonempty as NE exposing (Nonempty)`
to just import the type.

## Testing

```
npm install -g elm-test
npm install -g elm-verify-examples
elm verify-examples && elm test # From project root. Will require downloading packages on the first run.
```

## Upgrading

4.0.0, for Elm 0.19, removed the following functions: `(:::)`, `scanl`, and `scanl1`. `sample` uses the new [PRNG](https://en.m.wikipedia.org/wiki/Pseudorandom_number_generator) found in `elm-lang/random`.

The only breaking change from 2.x to 3.0.0 is that `andMap` has arguments
reversed to allow chaining with `|>` instead of backticks.

The only breaking change from 1.x to 2.0.0 is that `sample` returns a generator
rather than managing seeds.
