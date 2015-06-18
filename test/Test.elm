module Test where
{-| -}

import Graphics.Element exposing (above)
import Html
import Check exposing (..)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (..)
import ElmTest.Test exposing (equals)
import ElmTest.Runner.Element exposing (runDisplay)
import List.Nonempty as NE exposing ((:::))

nonemptylist elem = tuple (elem, list elem)

testSuite =
    suite "Nonempty List Test Suite"
    [ claim
        "dropping tail makes singleton"
      `true`
        (\(x,xs) -> NE.isSingleton (NE.dropTail (NE.Nonempty x xs)))
      `for`
        nonemptylist int
    , claim
        "converting to and from a normal list is the identity"
      `that`
        (\(x,xs) -> NE.Nonempty x xs |> NE.toList |> NE.fromList)
      `is`
        (\(x,xs) -> Just (NE.Nonempty x xs))
      `for`
        nonemptylist int
    , claim
        "length is 1 more than `length tail`"
     `that`
        (\(x,xs) -> NE.Nonempty x xs |> NE.length)
     `is`
        (\(x,xs) -> List.length xs + 1)
      `for`
        nonemptylist int
    , claim
        "cons works"
     `that`
        (\(y, (x,xs)) -> y ::: (NE.Nonempty x xs) |> NE.toList)
     `is`
        (\(y, (x,xs)) -> y :: x :: xs)
      `for`
        tuple (int, nonemptylist int)
    , claim
        "fromElement results in a singleton"
     `true`
       (\x -> NE.isSingleton (NE.fromElement x))
     `for`
       int
    , claim
        "fromList fails only for the empty List"
     `true`
       (\xs -> case NE.fromList xs of
                Just _ -> not (List.isEmpty xs)
                Nothing -> List.isEmpty xs
                )
     `for`
       list int
    , claim
        "map then toList == List.map"
     `that`
        (\(x,xs) -> NE.Nonempty x xs |> NE.map ((*) 2) |> NE.toList)
     `is`
        (\(x,xs) -> x::xs |> List.map ((*) 2))
      `for`
        nonemptylist int
    , claim
        "equality works"
     `true`
        (\(x,xs) -> NE.Nonempty x xs == NE.map identity (NE.Nonempty x xs))
      `for`
        nonemptylist int
    , claim
        "popping reduces the length by 1 except for singleton lists"
     `true`
        (\(x,xs) -> let ys = NE.Nonempty x xs
                        lengthReduced = (NE.length ys) - 1 == NE.length (NE.pop ys)
                    in lengthReduced || NE.isSingleton ys)
      `for`
        nonemptylist int
    , claim
        "pop xs == tail xs except for singleton lists"
     `true`
        (\(x,xs) -> let ys = NE.Nonempty x xs
                        tailEquals = NE.toList (NE.pop ys) == xs
                    in tailEquals || NE.isSingleton ys)
      `for`
        nonemptylist int
    , claim
        "reversing twice is the identity"
     `that`
        (\(x,xs) -> let ys = NE.Nonempty x xs
                    in NE.reverse (NE.reverse ys))
      `is`
        (\(x,xs) -> NE.Nonempty x xs)
      `for`
        nonemptylist int
    , claim
        "reversing is equal to the ordinary list reverse"
     `that`
        (\(x,xs) -> let ys = NE.Nonempty x xs
                    in NE.reverse ys |> NE.toList)
      `is`
        (\(x,xs) -> List.reverse (x::xs))
      `for`
        nonemptylist int
    , claim
        "replaceTail is equal to doing so with an ordinary list"
     `that`
        (\(ys, x, xs) -> let zs = NE.Nonempty x xs
                    in NE.replaceTail ys zs |> NE.toList)
      `is`
        (\(ys, x, xs) -> x::ys)
      `for`
        tuple3 (list int, int, list int)
    , claim
        "concat is equal to doing so with an ordinary list"
     `that`
        (\((x, xs), ys) ->
            let zs : NE.Nonempty (NE.Nonempty Int)
                zs = NE.Nonempty (NE.Nonempty x xs) (List.map (uncurry NE.Nonempty) ys)
            in NE.concat zs |> NE.toList)
      `is`
        (\((x, xs), ys) -> let ys' = List.map (uncurry (::)) ys
                           in List.concat ((x::xs)::ys'))
      `for`
        nonemptylist (nonemptylist int)
    ]

dedupeSuite =
    let mk x xs = NE.Nonempty x xs |> NE.dedup |> NE.toList
    in ElmTest.Test.suite "deduplication"
        [ [1] `equals` mk 1 []
        , [1, 2] `equals` mk 1 [2]
        , [1, 2] `equals` mk 1 [2, 2]
        , [1, 2] `equals` mk 1 [1, 2]
        , [1, 2, 1] `equals` mk 1 [1, 2, 2, 1]
        , [1, 2, 1] `equals` mk 1 [1, 2, 2, 2, 2, 2, 1]
        , [1, 2, 3, 4, 5] `equals` mk 1 [1, 2, 2, 3, 4, 4, 5]
        , [1, 2, 3, 2, 1] `equals` mk 1 [1, 2, 2, 3, 2, 2, 1, 1]
        , [1..4] `equals` mk 1 [1..4]
        , [3, 1, 2, 3] `equals` mk 3 [1..3]
        ]
uniqSuite =
    let mk x xs = NE.Nonempty x xs |> NE.uniq |> NE.toList
    in ElmTest.Test.suite "uniq"
        [ [1] `equals` mk 1 []
        , [1, 2] `equals` mk 1 [2]
        , [1, 2] `equals` mk 1 [2, 2]
        , [1, 2] `equals` mk 1 [1, 2]
        , [1, 2] `equals` mk 1 [1, 2, 2, 1]
        , [1, 2] `equals` mk 1 [1, 2, 2, 2, 2, 2, 1]
        , [1, 2, 3, 4, 5] `equals` mk 1 [1, 2, 2, 3, 4, 4, 5]
        , [1, 2, 3] `equals` mk 1 [1, 2, 2, 3, 2, 2, 1, 1]
        , [1..4] `equals` mk 1 [1..4]
        , [3, 1, 2] `equals` mk 3 [1..3]
        ]

unitSuite = ElmTest.Test.suite "all unit tests"
    [dedupeSuite, uniqSuite]

result = quickCheck testSuite

main = Html.div [] [display result, Html.fromElement (runDisplay unitSuite)]
