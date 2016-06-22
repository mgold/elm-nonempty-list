module Testing exposing (..)

import String
import Random
import Task exposing (Task)
import Test exposing (test, fuzz, fuzz2, fuzz3, describe)
import Test.Runner.Html
import Assert
import Fuzz exposing (tuple, tuple3, char, int, list, string)
import List.Nonempty as NE exposing ((:::))


nonemptylist elem =
    tuple ( elem, list elem )


isEven n =
    n % 2 == 0


f x =
    x * 2


testSuite =
    describe "fuzz tests"
        [ fuzz (nonemptylist int) "dropping tail makes singleton"
            <| \( x, xs ) ->
                NE.Nonempty x xs |> NE.dropTail |> NE.isSingleton |> Assert.true "dropped tail not a singleton"
        , fuzz (nonemptylist int) "converting to and from a normal list is the identity"
            <| \( x, xs ) ->
                NE.Nonempty x xs |> NE.toList |> NE.fromList |> Assert.equal (Just (NE.Nonempty x xs))
        , fuzz (nonemptylist int) "length is 1 more than `length tail`"
            <| \( x, xs ) -> NE.Nonempty x xs |> NE.length |> Assert.equal (List.length xs + 1)
        , fuzz (tuple ( int, nonemptylist int )) "cons works"
            <| \( y, ( x, xs ) ) ->
                y ::: (NE.Nonempty x xs) |> NE.toList |> Assert.equal (y :: x :: xs)
        , fuzz int "fromElement results in a singleton"
            <| \x -> NE.fromElement x |> NE.isSingleton |> Assert.true "fromElement x not a singleton"
        , fuzz (tuple ( nonemptylist int, nonemptylist int )) "append works"
            <| \( ( x, xs ), ( y, ys ) ) ->
                NE.append (NE.Nonempty x xs) (NE.Nonempty y ys) |> NE.toList |> Assert.equal (x :: xs ++ y :: ys)
        , fuzz (tuple ( nonemptylist int, nonemptylist int )) "append never results in a singleton"
            <| \( ( x, xs ), ( y, ys ) ) ->
                NE.append (NE.Nonempty x xs) (NE.Nonempty y ys) |> NE.isSingleton |> Assert.false "got a singleton"
        , fuzz (nonemptylist int) "get 0 == head"
            <| \( x, xs ) ->
                NE.Nonempty x xs |> NE.get 0 |> Assert.equal x
        , fuzz2 int int "getting any index from singleton produces the value"
            <| \x i -> NE.fromElement x |> NE.get i |> Assert.equal x
        , fuzz int "sample will eventually produce every element"
            <| \i ->
                let
                    gen =
                        NE.sample (NE.Nonempty 1 [ 2, 3, 4, 5, 6 ]) |> Random.list 50
                in
                    Random.step gen (Random.initialSeed i)
                        |> fst
                        |> NE.fromList
                        |> Maybe.map NE.uniq
                        |> Maybe.map (\ne -> NE.length ne == 6)
                        |> Assert.equal (Just True)
        , fuzz (list int) "fromList fails only for the empty List"
            <| \xs ->
                case NE.fromList xs of
                    Just _ ->
                        List.isEmpty xs |> Assert.false "fromList made Just x from an empty list"

                    Nothing ->
                        List.isEmpty xs |> Assert.true "fromList made Nothing from a nonempty list"
        , fuzz (nonemptylist int) "map then toList == List.map"
            <| \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.map f
                    |> NE.toList
                    |> Assert.equal (List.map f (x :: xs))
        , fuzz (tuple ( nonemptylist int, nonemptylist string )) "length (map2 (,) xs ys) == min (length xs) (length ys)"
            <| \( ( x, xs ), ( y, ys ) ) ->
                NE.length (NE.map2 (,) (NE.Nonempty x xs) (NE.Nonempty y ys))
                    |> Assert.equal (1 + min (List.length xs) (List.length ys))
        , fuzz (tuple ( nonemptylist int, nonemptylist string ))
            "map2 (,) xs ys == map (,) xs `andMap` ys "
            <| \( ( x, xs ), ( y, ys ) ) ->
                let
                    expected =
                        NE.map2 (,) (NE.Nonempty x xs) (NE.Nonempty y ys)

                    actual =
                        NE.map (,) (NE.Nonempty x xs) `NE.andMap` (NE.Nonempty y ys)
                in
                    Assert.equal expected actual
        , fuzz3 (nonemptylist int) (nonemptylist string) (nonemptylist char) "head (map (,,) xs `andMap` ys `andMap` zs) == (head xs, head ys, head zs)"
            <| \( x, xs ) ( y, ys ) ( z, zs ) ->
                NE.map (,,) (NE.Nonempty x xs)
                    `NE.andMap` (NE.Nonempty y ys)
                    `NE.andMap` (NE.Nonempty z
                                    zs
                                )
                    |> NE.head
                    |> Assert.equal ( x, y, z )
        , fuzz (nonemptylist int) "concatMap works the same as for a list"
            <| \( x, xs ) ->
                NE.concatMap (\x -> NE.Nonempty x [ f x ]) (NE.Nonempty x xs)
                    |> NE.toList
                    |> Assert.equal (List.concatMap (\x -> [ x, f x ]) (x :: xs))
        , fuzz (nonemptylist int) "indexedMap works the same as for a list"
            <| \( x, xs ) ->
                NE.indexedMap (,) (NE.Nonempty x xs)
                    |> NE.toList
                    |> Assert.equal (List.indexedMap (,) (x :: xs))
        , fuzz (nonemptylist int) "filter works"
            <| \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.filter isEven -99
                    |> NE.toList
                    |> Assert.equal
                        (let
                            filtered =
                                List.filter isEven (x :: xs)
                         in
                            if List.isEmpty filtered then
                                [ -99 ]
                            else
                                filtered
                        )
        , fuzz2 (nonemptylist int) int "Filtering everything out results in the default value"
            <| \( x, xs ) d -> NE.Nonempty x xs |> NE.filter (always False) d |> NE.toList |> Assert.equal [ d ]
        , fuzz2 (nonemptylist int) int "Filtering nothing out is the identity"
            <| \( x, xs ) d -> NE.Nonempty x xs |> NE.filter (always True) d |> Assert.equal (NE.Nonempty x xs)
        , fuzz (nonemptylist int) "Equal lists equate true"
            <| \( x, xs ) -> NE.Nonempty x xs |> Assert.equal (NE.map identity (NE.Nonempty x xs))
        , fuzz2 (nonemptylist int) int "Lists of nonequal length equate false"
            <| \( x, xs ) d ->
                d
                    ::: NE.Nonempty x xs
                    |> Assert.notEqual (NE.Nonempty x xs)
        , fuzz (nonemptylist int) "Lists with unequal heads equate false"
            <| \( x, xs ) -> NE.Nonempty x xs == NE.Nonempty (x + 1) xs |> Assert.false "lists were equal"
        , fuzz (nonemptylist int) "popping reduces the length by 1 except for singleton lists"
            <| \( x, xs ) ->
                let
                    ys =
                        NE.Nonempty x xs

                    lengthReduced =
                        (NE.length ys) - 1 == NE.length (NE.pop ys)
                in
                    Assert.true "popping not working correctly" <| lengthReduced `xor` NE.isSingleton ys
        , fuzz (nonemptylist int) "pop xs == tail xs except for singleton lists"
            <| \( x, xs ) ->
                let
                    ys =
                        NE.Nonempty x xs

                    tailEquals =
                        NE.toList (NE.pop ys) == xs
                in
                    Assert.true "popping not working correctly" <| tailEquals || NE.isSingleton ys
        , fuzz (nonemptylist int) "reversing twice is the identity"
            <| \( x, xs ) ->
                let
                    ys =
                        NE.Nonempty x xs
                in
                    NE.reverse (NE.reverse ys) |> Assert.equal ys
        , fuzz (nonemptylist int) "reversing is equal to the ordinary list reverse"
            <| \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.reverse
                    |> NE.toList
                    |> Assert.equal (List.reverse (x :: xs))
        , fuzz3 (list int) int (list int) "replaceTail is equal to doing so with an ordinary list"
            <| \ys x xs ->
                NE.Nonempty x xs
                    |> NE.replaceTail ys
                    |> NE.toList
                    |> Assert.equal (x :: ys)
        , fuzz (nonemptylist (nonemptylist int))
            "concat is equal to doing so with an ordinary list"
            <| \( ( x, xs ), ys ) ->
                let
                    zs : NE.Nonempty (NE.Nonempty Int)
                    zs =
                        NE.Nonempty (NE.Nonempty x xs) (List.map (uncurry NE.Nonempty) ys)

                    ys' =
                        List.map (uncurry (::)) ys

                    expected =
                        List.concat ((x :: xs) :: ys')
                in
                    NE.concat zs |> NE.toList |> Assert.equal expected
        , fuzz3 int (list int) int "member checks the head and the tail"
            <| \x xs y ->
                let
                    expected =
                        x == y || List.member y xs
                in
                    NE.Nonempty x xs |> NE.member y |> Assert.equal expected
        , fuzz (nonemptylist string) "foldl is the same as for a list"
            <| \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.foldl (++) ""
                    |> Assert.equal (List.foldl (++) "" (x :: xs))
        , fuzz (nonemptylist string) "foldl1 is the same as for a list"
            <| \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.foldl1 (++)
                    |> Assert.equal (List.foldl (++) "" (x :: xs))
        , fuzz (nonemptylist string) "sort is the same as for a list"
            <| \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.sort
                    |> NE.toList
                    |> Assert.equal (List.sort (x :: xs))
        , fuzz (nonemptylist string) "sortBy is the same as for a list"
            <| \( x, xs ) ->
                let
                    expected =
                        List.map (\s -> { name = s }) (x :: xs) |> List.sortBy .name
                in
                    NE.Nonempty x xs
                        |> NE.map (\s -> { name = s })
                        |> NE.sortBy .name
                        |> NE.toList
                        |> Assert.equal expected
        , fuzz (nonemptylist string) "sortWith is the same as for a list"
            <| \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.sortWith compare
                    |> NE.toList
                    |> Assert.equal (List.sortWith compare (x :: xs))
        , describe "scanning"
            [ fuzz (nonemptylist string) "scanl is the same as for a list"
                <| \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl (++) ""
                        |> NE.toList
                        |> Assert.equal (List.scanl (++) "" (x :: xs))
            , fuzz (nonemptylist string) "The head of the result of scanl is the base case"
                <| \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl (++) ""
                        |> NE.head
                        |> Assert.equal ""
            , fuzz (nonemptylist string) "The tail of the result of scanl is the result of scanl1"
                <| \( x, xs ) ->
                    let
                        ys =
                            NE.Nonempty x xs

                        scanned =
                            NE.scanl (++) "" ys

                        scanned1 =
                            NE.scanl1 (++) ys
                    in
                        NE.tail scanned
                            |> Assert.equal (NE.toList scanned1)
            , fuzz (nonemptylist int) "scanl adds 1 to the length"
                <| \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl (+) 0
                        |> NE.length
                        |> Assert.equal (2 + List.length xs)
            , fuzz (nonemptylist int) "scanl1 does not change the length"
                <| \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl1 (+)
                        |> NE.length
                        |> Assert.equal (1 + List.length xs)
            , fuzz (nonemptylist string) "scanl with string concatenation never decreases the length"
                <| \( x, xs ) ->
                    let
                        counts =
                            NE.Nonempty x xs
                                |> NE.scanl1 (++)
                                |> NE.map String.length
                    in
                        List.map2 (,) (NE.toList counts) (NE.tail counts)
                            |> List.map (\( a, b ) -> a <= b)
                            |> List.all identity
                            |> Assert.true "length decreased at least once"
            , fuzz (nonemptylist string) "scanl1 does not change the head"
                <| \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl1 (++)
                        |> NE.head
                        |> Assert.equal x
            ]
        ]


dedupeSuite =
    let
        mk x xs =
            NE.Nonempty x xs |> NE.dedup |> NE.toList
    in
        describe "deduplication"
            [ test "" <| \_ -> mk 1 [] |> Assert.equal [ 1 ]
            , test "" <| \_ -> mk 1 [ 2 ] |> Assert.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 2, 2 ] |> Assert.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2 ] |> Assert.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 1 ] |> Assert.equal [ 1, 2, 1 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 2, 2, 2, 1 ] |> Assert.equal [ 1, 2, 1 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 3, 4, 4, 5 ] |> Assert.equal [ 1, 2, 3, 4, 5 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 3, 2, 2, 1, 1 ] |> Assert.equal [ 1, 2, 3, 2, 1 ]
            , test "" <| \_ -> mk 1 [1..4] |> Assert.equal [1..4]
            , test "" <| \_ -> mk 3 [1..3] |> Assert.equal [ 3, 1, 2, 3 ]
            ]


uniqSuite =
    let
        mk x xs =
            NE.Nonempty x xs |> NE.uniq |> NE.toList
    in
        describe "uniq"
            [ test "" <| \_ -> mk 1 [] |> Assert.equal [ 1 ]
            , test "" <| \_ -> mk 1 [ 2 ] |> Assert.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 2, 2 ] |> Assert.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2 ] |> Assert.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 1 ] |> Assert.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 2, 2, 2, 1 ] |> Assert.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 3, 4, 4, 5 ] |> Assert.equal [ 1, 2, 3, 4, 5 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 3, 2, 2, 1, 1 ] |> Assert.equal [ 1, 2, 3 ]
            , test "" <| \_ -> mk 1 [1..4] |> Assert.equal [1..4]
            , test "" <| \_ -> mk 3 [1..3] |> Assert.equal [ 3, 1, 2 ]
            ]


getSuite =
    let
        xs =
            NE.Nonempty 10 [ 11, 12 ]
    in
        describe "get"
            [ test "" <| \_ -> NE.get -4 xs |> Assert.equal 12
            , test "" <| \_ -> NE.get -3 xs |> Assert.equal 10
            , test "" <| \_ -> NE.get -2 xs |> Assert.equal 11
            , test "" <| \_ -> NE.get -1 xs |> Assert.equal 12
            , test "" <| \_ -> NE.get 0 xs |> Assert.equal 10
            , test "" <| \_ -> NE.get 1 xs |> Assert.equal 11
            , test "" <| \_ -> NE.get 2 xs |> Assert.equal 12
            , test "" <| \_ -> NE.get 3 xs |> Assert.equal 10
            ]


main =
    Test.Runner.Html.run
        <| describe "all tests"
            [ testSuite, getSuite, dedupeSuite, uniqSuite ]
