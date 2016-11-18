module Tests exposing (..)

import String
import Random
import Task exposing (Task)
import Test exposing (Test, test, fuzz, fuzz2, fuzz3, describe)
import Expect
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
        [ fuzz (nonemptylist int) "dropping tail makes singleton" <|
            \( x, xs ) ->
                NE.Nonempty x xs |> NE.dropTail |> NE.isSingleton |> Expect.true "dropped tail not a singleton"
        , fuzz (nonemptylist int) "converting to and from a normal list is the identity" <|
            \( x, xs ) ->
                NE.Nonempty x xs |> NE.toList |> NE.fromList |> Expect.equal (Just (NE.Nonempty x xs))
        , fuzz (nonemptylist int) "length is 1 more than `length tail`" <|
            \( x, xs ) -> NE.Nonempty x xs |> NE.length |> Expect.equal (List.length xs + 1)
        , fuzz2 int (nonemptylist int) "cons works" <|
            \y ( x, xs ) ->
                y ::: (NE.Nonempty x xs) |> NE.toList |> Expect.equal (y :: x :: xs)
        , fuzz int "fromElement results in a singleton" <|
            \x -> NE.fromElement x |> NE.isSingleton |> Expect.true "fromElement x not a singleton"
        , fuzz (tuple ( nonemptylist int, nonemptylist int )) "append works" <|
            \( ( x, xs ), ( y, ys ) ) ->
                NE.append (NE.Nonempty x xs) (NE.Nonempty y ys) |> NE.toList |> Expect.equal (x :: xs ++ y :: ys)
        , fuzz (tuple ( nonemptylist int, nonemptylist int )) "append never results in a singleton" <|
            \( ( x, xs ), ( y, ys ) ) ->
                NE.append (NE.Nonempty x xs) (NE.Nonempty y ys) |> NE.isSingleton |> Expect.false "got a singleton"
        , fuzz (nonemptylist int) "get 0 == head" <|
            \( x, xs ) ->
                NE.Nonempty x xs |> NE.get 0 |> Expect.equal x
        , fuzz2 int int "getting any index from singleton produces the value" <|
            \x i -> NE.fromElement x |> NE.get i |> Expect.equal x
        , fuzz int "sample will eventually produce every element" <|
            \i ->
                let
                    gen =
                        NE.sample (NE.Nonempty 1 [ 2, 3, 4, 5, 6 ]) |> Random.list 80
                in
                    Random.step gen (Random.initialSeed i)
                        |> Tuple.first
                        |> NE.fromList
                        |> Maybe.map NE.uniq
                        |> Maybe.map (\ne -> NE.length ne == 6)
                        |> Expect.equal (Just True)
        , fuzz (list int) "fromList fails only for the empty List" <|
            \xs ->
                case NE.fromList xs of
                    Just _ ->
                        List.isEmpty xs |> Expect.false "fromList made Just x from an empty list"

                    Nothing ->
                        List.isEmpty xs |> Expect.true "fromList made Nothing from a nonempty list"
        , fuzz (nonemptylist int) "map then toList == List.map" <|
            \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.map f
                    |> NE.toList
                    |> Expect.equal (List.map f (x :: xs))
        , fuzz (tuple ( nonemptylist int, nonemptylist string )) "length (map2 (,) xs ys) == min (length xs) (length ys)" <|
            \( ( x, xs ), ( y, ys ) ) ->
                NE.length (NE.map2 (,) (NE.Nonempty x xs) (NE.Nonempty y ys))
                    |> Expect.equal (1 + min (List.length xs) (List.length ys))
        , fuzz (tuple ( nonemptylist int, nonemptylist string ))
            "map2 (,) xs ys == map (,) xs  |> andMap ys"
          <|
            \( ( x, xs ), ( y, ys ) ) ->
                let
                    expected =
                        NE.map2 (,) (NE.Nonempty x xs) (NE.Nonempty y ys)

                    actual =
                        NE.map (,) (NE.Nonempty x xs) |> NE.andMap (NE.Nonempty y ys)
                in
                    Expect.equal expected actual
        , fuzz3 (nonemptylist int) (nonemptylist string) (nonemptylist char) "head (map (,,) xs |> andMap ys |> andMap zs) == (head xs, head ys, head zs)" <|
            \( x, xs ) ( y, ys ) ( z, zs ) ->
                NE.map (,,) (NE.Nonempty x xs)
                    |> NE.andMap (NE.Nonempty y ys)
                    |> NE.andMap (NE.Nonempty z zs)
                    |> NE.head
                    |> Expect.equal ( x, y, z )
        , fuzz (nonemptylist int) "concatMap works the same as for a list" <|
            \( x, xs ) ->
                NE.concatMap (\x -> NE.Nonempty x [ f x ]) (NE.Nonempty x xs)
                    |> NE.toList
                    |> Expect.equal (List.concatMap (\x -> [ x, f x ]) (x :: xs))
        , fuzz (nonemptylist int) "indexedMap works the same as for a list" <|
            \( x, xs ) ->
                NE.indexedMap (,) (NE.Nonempty x xs)
                    |> NE.toList
                    |> Expect.equal (List.indexedMap (,) (x :: xs))
        , fuzz (nonemptylist int) "filter works" <|
            \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.filter isEven -99
                    |> NE.toList
                    |> Expect.equal
                        (let
                            filtered =
                                List.filter isEven (x :: xs)
                         in
                            if List.isEmpty filtered then
                                [ -99 ]
                            else
                                filtered
                        )
        , fuzz2 (nonemptylist int) int "Filtering everything out results in the default value" <|
            \( x, xs ) d -> NE.Nonempty x xs |> NE.filter (always False) d |> NE.toList |> Expect.equal [ d ]
        , fuzz2 (nonemptylist int) int "Filtering nothing out is the identity" <|
            \( x, xs ) d -> NE.Nonempty x xs |> NE.filter (always True) d |> Expect.equal (NE.Nonempty x xs)
        , fuzz (nonemptylist int) "Equal lists equate true" <|
            \( x, xs ) -> NE.Nonempty x xs |> Expect.equal (NE.map identity (NE.Nonempty x xs))
        , fuzz2 (nonemptylist int) int "Lists of nonequal length equate false" <|
            \( x, xs ) d ->
                NE.Nonempty d (x :: xs)
                    |> Expect.notEqual (NE.Nonempty x xs)
        , fuzz (nonemptylist int) "Lists with unequal heads equate false" <|
            \( x, xs ) -> NE.Nonempty x xs == NE.Nonempty (x + 1) xs |> Expect.false "lists were equal"
        , fuzz (nonemptylist int) "popping reduces the length by 1 except for singleton lists" <|
            \( x, xs ) ->
                let
                    ys =
                        NE.Nonempty x xs

                    lengthReduced =
                        (NE.length ys) - 1 == NE.length (NE.pop ys)
                in
                    Expect.true "popping not working correctly" <| xor lengthReduced (NE.isSingleton ys)
        , fuzz (nonemptylist int) "pop xs == tail xs except for singleton lists" <|
            \( x, xs ) ->
                let
                    ys =
                        NE.Nonempty x xs

                    tailEquals =
                        NE.toList (NE.pop ys) == xs
                in
                    Expect.true "popping not working correctly" <| tailEquals || NE.isSingleton ys
        , fuzz (nonemptylist int) "reversing twice is the identity" <|
            \( x, xs ) ->
                let
                    ys =
                        NE.Nonempty x xs
                in
                    NE.reverse (NE.reverse ys) |> Expect.equal ys
        , fuzz (nonemptylist int) "reversing is equal to the ordinary list reverse" <|
            \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.reverse
                    |> NE.toList
                    |> Expect.equal (List.reverse (x :: xs))
        , fuzz3 (list int) int (list int) "replaceTail is equal to doing so with an ordinary list" <|
            \ys x xs ->
                NE.Nonempty x xs
                    |> NE.replaceTail ys
                    |> NE.toList
                    |> Expect.equal (x :: ys)
        , fuzz (nonemptylist (nonemptylist int))
            "concat is equal to doing so with an ordinary list"
          <|
            \( ( x, xs ), ys ) ->
                let
                    zs : NE.Nonempty (NE.Nonempty Int)
                    zs =
                        NE.Nonempty (NE.Nonempty x xs) (List.map (uncurry NE.Nonempty) ys)

                    ys_ =
                        List.map (uncurry (::)) ys

                    expected =
                        List.concat ((x :: xs) :: ys_)
                in
                    NE.concat zs |> NE.toList |> Expect.equal expected
        , fuzz3 int (list int) int "member checks the head and the tail" <|
            \x xs y ->
                let
                    expected =
                        x == y || List.member y xs
                in
                    NE.Nonempty x xs |> NE.member y |> Expect.equal expected
        , fuzz (nonemptylist string) "foldl is the same as for a list" <|
            \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.foldl (++) ""
                    |> Expect.equal (List.foldl (++) "" (x :: xs))
        , fuzz (nonemptylist string) "foldl1 is the same as for a list" <|
            \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.foldl1 (++)
                    |> Expect.equal (List.foldl (++) "" (x :: xs))
        , fuzz (nonemptylist string) "sort is the same as for a list" <|
            \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.sort
                    |> NE.toList
                    |> Expect.equal (List.sort (x :: xs))
        , fuzz (nonemptylist string) "sortBy is the same as for a list" <|
            \( x, xs ) ->
                let
                    expected =
                        List.map (\s -> { name = s }) (x :: xs) |> List.sortBy .name
                in
                    NE.Nonempty x xs
                        |> NE.map (\s -> { name = s })
                        |> NE.sortBy .name
                        |> NE.toList
                        |> Expect.equal expected
        , fuzz (nonemptylist string) "sortWith is the same as for a list" <|
            \( x, xs ) ->
                NE.Nonempty x xs
                    |> NE.sortWith compare
                    |> NE.toList
                    |> Expect.equal (List.sortWith compare (x :: xs))
        , describe "scanning"
            [ fuzz (nonemptylist string) "scanl is the same as for a list" <|
                \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl (++) ""
                        |> NE.toList
                        |> Expect.equal (List.scanl (++) "" (x :: xs))
            , fuzz (nonemptylist string) "The head of the result of scanl is the base case" <|
                \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl (++) ""
                        |> NE.head
                        |> Expect.equal ""
            , fuzz (nonemptylist string) "The tail of the result of scanl is the result of scanl1" <|
                \( x, xs ) ->
                    let
                        ys =
                            NE.Nonempty x xs

                        scanned =
                            NE.scanl (++) "" ys

                        scanned1 =
                            NE.scanl1 (++) ys
                    in
                        NE.tail scanned
                            |> Expect.equal (NE.toList scanned1)
            , fuzz (nonemptylist int) "scanl adds 1 to the length" <|
                \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl (+) 0
                        |> NE.length
                        |> Expect.equal (2 + List.length xs)
            , fuzz (nonemptylist int) "scanl1 does not change the length" <|
                \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl1 (+)
                        |> NE.length
                        |> Expect.equal (1 + List.length xs)
            , fuzz (nonemptylist string) "scanl with string concatenation never decreases the length" <|
                \( x, xs ) ->
                    let
                        counts =
                            NE.Nonempty x xs
                                |> NE.scanl1 (++)
                                |> NE.map String.length
                    in
                        List.map2 (,) (NE.toList counts) (NE.tail counts)
                            |> List.map (\( a, b ) -> a <= b)
                            |> List.all identity
                            |> Expect.true "length decreased at least once"
            , fuzz (nonemptylist string) "scanl1 does not change the head" <|
                \( x, xs ) ->
                    NE.Nonempty x xs
                        |> NE.scanl1 (++)
                        |> NE.head
                        |> Expect.equal x
            ]
        ]


dedupeSuite =
    let
        mk x xs =
            NE.Nonempty x xs |> NE.dedup |> NE.toList
    in
        describe "deduplication"
            [ test "" <| \_ -> mk 1 [] |> Expect.equal [ 1 ]
            , test "" <| \_ -> mk 1 [ 2 ] |> Expect.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 2, 2 ] |> Expect.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2 ] |> Expect.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 1 ] |> Expect.equal [ 1, 2, 1 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 2, 2, 2, 1 ] |> Expect.equal [ 1, 2, 1 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 3, 4, 4, 5 ] |> Expect.equal [ 1, 2, 3, 4, 5 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 3, 2, 2, 1, 1 ] |> Expect.equal [ 1, 2, 3, 2, 1 ]
            , test "" <| \_ -> mk 1 (List.range 1 4) |> Expect.equal (List.range 1 4)
            , test "" <| \_ -> mk 3 (List.range 1 3) |> Expect.equal [ 3, 1, 2, 3 ]
            ]


uniqSuite =
    let
        mk x xs =
            NE.Nonempty x xs |> NE.uniq |> NE.toList
    in
        describe "uniq"
            [ test "" <| \_ -> mk 1 [] |> Expect.equal [ 1 ]
            , test "" <| \_ -> mk 1 [ 2 ] |> Expect.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 2, 2 ] |> Expect.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2 ] |> Expect.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 1 ] |> Expect.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 2, 2, 2, 1 ] |> Expect.equal [ 1, 2 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 3, 4, 4, 5 ] |> Expect.equal [ 1, 2, 3, 4, 5 ]
            , test "" <| \_ -> mk 1 [ 1, 2, 2, 3, 2, 2, 1, 1 ] |> Expect.equal [ 1, 2, 3 ]
            , test "" <| \_ -> mk 1 (List.range 1 4) |> Expect.equal (List.range 1 4)
            , test "" <| \_ -> mk 3 (List.range 1 3) |> Expect.equal [ 3, 1, 2 ]
            ]


getSuite =
    let
        xs =
            NE.Nonempty 10 [ 11, 12 ]
    in
        describe "get"
            [ test "" <| \_ -> NE.get -4 xs |> Expect.equal 12
            , test "" <| \_ -> NE.get -3 xs |> Expect.equal 10
            , test "" <| \_ -> NE.get -2 xs |> Expect.equal 11
            , test "" <| \_ -> NE.get -1 xs |> Expect.equal 12
            , test "" <| \_ -> NE.get 0 xs |> Expect.equal 10
            , test "" <| \_ -> NE.get 1 xs |> Expect.equal 11
            , test "" <| \_ -> NE.get 2 xs |> Expect.equal 12
            , test "" <| \_ -> NE.get 3 xs |> Expect.equal 10
            ]


all : Test
all =
    describe "All Nonempty List tests"
        [ testSuite, getSuite, dedupeSuite, uniqSuite ]
