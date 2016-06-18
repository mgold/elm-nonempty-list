module AssertExtra exposing (..)

import Assert exposing (pass, fail, Assertion)


true : Bool -> Assertion
true b =
    if b then
        pass
    else
        fail "expected true but got false"


false : Bool -> Assertion
false b =
    if b then
        fail "expected false but got true"
    else
        pass
