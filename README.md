# List.Nonempty for Elm

A list that is known, at compile-time, to be nonempty. This means `head` and `tail` are guaranteed to succeed.

## Testing
Rune `elm reactor` in the test directory. Thanks to all the work that went into property-based testing, even if it's overkill for this tiny library.
