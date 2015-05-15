# List.Nonempty for Elm

A list that is known, at compile-time, to be nonempty. This means `head` and `tail` are guaranteed to succeed.

````elm
one = fromElement 2
two = 4 ::: one
toList two == [4, 2]
toList (dropTail) two == [4]
head two == 4
tail two == [2]
````

## Testing
Run `elm reactor` in the test directory. Thanks to all the work that went into property-based testing, even if it's overkill for this tiny library.
