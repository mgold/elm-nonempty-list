# List.Nonempty for Elm

A list that is known, at compile-time, to be nonempty. This means `head` and `tail` are guaranteed to succeed and you don't have to carry Maybes throughout your program.

````elm
import List.Nonempty exposing (..)
one = fromElement 2
two = 4 ::: one
toList two == [4, 2]
head two == 4
tail two == [2]
toList (reverse two) == [2, 4]
toList (dropTail two) == [4]
member 4 two == True
foldl1 (+) two == 6
````

For actual usage, I recommend `import List.Nonempty as NE exposing (Nonempty, (:::))` to import the type and infix cons.

## Testing
`cd test; sh run-tests.sh`

Thanks to all the work that went into property-based testing, even if it's overkill for this tiny library.

## Upgrading

The only breaking change from 1.x to 2.0.0 is that `sample` returns a generator rather than managing seeds.
