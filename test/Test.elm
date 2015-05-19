module Test where
{-| -}

import Check exposing (..)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (..)
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
    ]

result = quickCheck testSuite

main = display result
