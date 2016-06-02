module List.Nonempty exposing (..)

{-| A list that cannot be empty. The head and tail can be accessed without Maybes. Most other list functions are
available.

# Definition
@docs Nonempty

# Create
@docs fromElement, fromList

# Access
@docs head, tail, toList, get, sample

# Inspect
Nonempty lists support equality with the usual `(==)` operator (provided their contents also support equality).
@docs isSingleton, length, member, all, any

# Convert
@docs cons, (:::), append, pop, reverse, concat

# Swap
@docs replaceHead, replaceTail, dropTail

# Map
@docs map, indexedMap, map2, andMap, concatMap

# Filter
@docs filter

# Fold
To fold or scan from the right, reverse the list first.
@docs foldl, foldl1, scanl, scanl1

# Sort
@docs sort, sortBy, sortWith

# Deduplicate
The nonempty list's elements must support equality (e.g. not functions). Otherwise  you will get a runtime error.
@docs dedup, uniq

-}

import Random


{-| The Nonempty type. If you have both a head and tail, you can construct a
nonempty list directly. Otherwise use the helpers below instead.
-}
type Nonempty a
    = Nonempty a (List a)


{-| Create a singleton list with the given element.
-}
fromElement : a -> Nonempty a
fromElement x =
    Nonempty x []


{-| Create a nonempty list from an ordinary list, failing on the empty list.
-}
fromList : List a -> Maybe (Nonempty a)
fromList ys =
    case ys of
        x :: xs ->
            Just (Nonempty x xs)

        _ ->
            Nothing


{-| Return the head of the list.
-}
head : Nonempty a -> a
head (Nonempty x xs) =
    x


{-| Return the tail of the list.
-}
tail : Nonempty a -> List a
tail (Nonempty x xs) =
    xs


{-| Convert to an ordinary list.
-}
toList : Nonempty a -> List a
toList (Nonempty x xs) =
    x :: xs


{-| Get the item at the specified index. The head has index 0. Indices are modulused by the length so out-of-range
errors can't happen. This means that negative indices are supported, e.g. -1 to get the last element.
-}
get : Int -> Nonempty a -> a
get i ((Nonempty x xs) as ne) =
    let
        j =
            i % (length ne)

        find k ys =
            case ys of
                [] ->
                    Debug.crash "This can't happen: attempted to take value at safe index from empty list"

                z :: zs ->
                    if k == 0 then
                        z
                    else
                        find (k - 1) zs
    in
        if j == 0 then
            x
        else
            find (j - 1) xs


{-| Create a random generator that returns a value of the nonempty list chosen uniformly at random.
-}
sample : Nonempty a -> Random.Generator a
sample nonempty =
    Random.int 0 (length nonempty - 1)
        |> Random.map (\i -> get i nonempty)


{-| Add another element as the head of the list, pushing the previous head to the tail.
-}
cons : a -> Nonempty a -> Nonempty a
cons y (Nonempty x xs) =
    Nonempty y (x :: xs)


{-| Infix cons. Note that you cannot use this for pattern matching. Be sure to add `exposing ((:::))` to your import.

    4 ::: Nonempty 3 [2,1] == Nonempty 4 [3,2,1]

Note: the infix associativity and precedence are not set for this operator, because doing so can cause
[conflicts](https://github.com/elm-lang/elm-compiler/issues/1096) that are hard to work around. In most cases this
shouldn't matter.

-}
(:::) : a -> Nonempty a -> Nonempty a
(:::) =
    cons



--infixr 5 :::


{-| Append two nonempty lists together. `(++)` is _not_ supported.
-}
append : Nonempty a -> Nonempty a -> Nonempty a
append (Nonempty x xs) (Nonempty y ys) =
    Nonempty x (xs ++ y :: ys)


{-| Pop and discard the head, or do nothing for a singleton list. Useful if you
want to exhaust a list but hang on to the last item indefinitely.

    pop (Nonempty 3 [2,1]) == Nonempty 2 [1]
    pop (Nonempty 1 []) == Nonempty 1 []
-}
pop : Nonempty a -> Nonempty a
pop (Nonempty x xs) =
    case xs of
        [] ->
            Nonempty x xs

        y :: ys ->
            Nonempty y ys


{-| Reverse a nonempty list.
-}
reverse : Nonempty a -> Nonempty a
reverse (Nonempty x xs) =
    let
        revapp : ( List a, a, List a ) -> Nonempty a
        revapp ( ls, c, rs ) =
            case rs of
                [] ->
                    Nonempty c ls

                r :: rs' ->
                    revapp ( c :: ls, r, rs' )
    in
        revapp ( [], x, xs )


{-| Flatten a nonempty list of nonempty lists into a single nonempty list.
-}
concat : Nonempty (Nonempty a) -> Nonempty a
concat (Nonempty xs xss) =
    let
        hd =
            head xs

        tl =
            tail xs ++ List.concat (List.map toList xss)
    in
        Nonempty hd tl


{-| Exchange the head element while leaving the tail alone.
-}
replaceHead : a -> Nonempty a -> Nonempty a
replaceHead y (Nonempty x xs) =
    Nonempty y xs


{-| Exchange the tail for another while leaving the head alone.
-}
replaceTail : List a -> Nonempty a -> Nonempty a
replaceTail ys (Nonempty x xs) =
    Nonempty x ys


{-| Replace the tail with the empty list while leaving the head alone.
-}
dropTail : Nonempty a -> Nonempty a
dropTail (Nonempty x xs) =
    Nonempty x []


{-| Map a function over a nonempty list.
-}
map : (a -> b) -> Nonempty a -> Nonempty b
map f (Nonempty x xs) =
    Nonempty (f x) (List.map f xs)


{-| Map a function over two nonempty lists.
-}
map2 : (a -> b -> c) -> Nonempty a -> Nonempty b -> Nonempty c
map2 f (Nonempty x xs) (Nonempty y ys) =
    Nonempty (f x y) (List.map2 f xs ys)


{-| Map over an arbitrary number of nonempty lists.

    map2 (,) xs ys == map (,) xs `andMap` ys
    head (map (,,) xs `andMap` ys `andMap` zs) == (head xs, head ys, head zs)
-}
andMap : Nonempty (a -> b) -> Nonempty a -> Nonempty b
andMap =
    map2 (<|)


{-| Map a given function onto a nonempty list and flatten the resulting nonempty lists. If you're chaining, you can
define `andThen = flip concatMap`.
-}
concatMap : (a -> Nonempty b) -> Nonempty a -> Nonempty b
concatMap f xs =
    concat (map f xs)


{-| Same as `map` but the function is also applied to the index of each element (starting at zero).
-}
indexedMap : (Int -> a -> b) -> Nonempty a -> Nonempty b
indexedMap f (Nonempty x xs) =
    let
        wrapped i d =
            f (i + 1) d
    in
        Nonempty (f 0 x) (List.indexedMap wrapped xs)


{-| Determine if the nonempty list has exactly one element.
-}
isSingleton : Nonempty a -> Bool
isSingleton (Nonempty x xs) =
    List.isEmpty xs


{-| Find the length of the nonempty list.
-}
length : Nonempty a -> Int
length (Nonempty x xs) =
    List.length xs + 1


{-| Determine if an element is present in the nonempty list.
-}
member : a -> Nonempty a -> Bool
member y (Nonempty x xs) =
    x == y || List.member y xs


{-| Determine if all elements satisfy the predicate.
-}
all : (a -> Bool) -> Nonempty a -> Bool
all f (Nonempty x xs) =
    f x && List.all f xs


{-| Determine if any elements satisfy the predicate.
-}
any : (a -> Bool) -> Nonempty a -> Bool
any f (Nonempty x xs) =
    f x || List.any f xs


{-| Filter a nonempty list. If all values are filtered out, return the singleton list containing the default value
provided. If any value is retained, the default value is not used. If you want to deal with a Maybe instead, use
`toList >> List.filter yourPredicate >> fromList`.

    filter isEven 0 (Nonempty 7 [2, 5]) == fromElement 2
    filter isEven 0 (Nonempty 7 []) == fromElement 0

-}
filter : (a -> Bool) -> a -> Nonempty a -> Nonempty a
filter p d (Nonempty x xs) =
    if p x then
        Nonempty x (List.filter p xs)
    else
        case (List.filter p xs) of
            [] ->
                Nonempty d []

            y :: ys ->
                Nonempty y ys


{-| Sort a nonempty list of comparable things, lowest to highest.
-}
sort : Nonempty comparable -> Nonempty comparable
sort (Nonempty x xs) =
    case List.sort (x :: xs) of
        y :: ys ->
            Nonempty y ys

        [] ->
            Debug.crash "This can't happen: sorting a nonempty list returned an empty list"


{-| Sort a nonempty list of things by a derived property.
-}
sortBy : (a -> comparable) -> Nonempty a -> Nonempty a
sortBy f (Nonempty x xs) =
    case List.sortBy f (x :: xs) of
        y :: ys ->
            Nonempty y ys

        [] ->
            Debug.crash "This can't happen: sortBying a nonempty list returned an empty list"


{-| Sort a nonempty list of things by a custom comparison function.
-}
sortWith : (a -> a -> Order) -> Nonempty a -> Nonempty a
sortWith f (Nonempty x xs) =
    case List.sortWith f (x :: xs) of
        y :: ys ->
            Nonempty y ys

        [] ->
            Debug.crash "This can't happen: sortWithing a nonempty list returned an empty list"


{-| Remove _adjacent_ duplicate elements from the nonempty list.

    dedup (Nonempty 1 [2, 2, 1]) == Nonempty 1 [2, 1]
-}
dedup : Nonempty a -> Nonempty a
dedup (Nonempty x xs) =
    let
        dedupe : a -> List a -> List a -> Nonempty a
        dedupe prev done next =
            case next of
                [] ->
                    Nonempty prev done

                y :: ys ->
                    if y == prev then
                        dedupe prev done ys
                    else
                        dedupe y (prev :: done) ys
    in
        reverse <| dedupe x [] xs


{-| Remove _all_ duplicate elements from the nonempty list, with the remaining elements ordered by first occurrence.

    uniq (Nonempty 1 [2, 2, 1]) == Nonempty 1 [2]
-}
uniq : Nonempty a -> Nonempty a
uniq (Nonempty x xs) =
    let
        unique : List a -> Nonempty a -> List a -> Nonempty a
        unique seen done next =
            case next of
                [] ->
                    done

                y :: ys ->
                    if List.member y seen then
                        unique seen done ys
                    else
                        unique (y :: seen) (y ::: done) ys
    in
        reverse <| unique [ x ] (Nonempty x []) xs


{-| Reduce a nonempty list from the left with a base case.

    foldl (++) "" (Nonempty "a" ["b", "c"]) == "cba"
-}
foldl : (a -> b -> b) -> b -> Nonempty a -> b
foldl f b (Nonempty x xs) =
    List.foldl f b (x :: xs)


{-| Reduce a nonempty list from the left _without_ a base case. As per Elm convention, the first argument is the current
element and the second argument is the accumulated value. The function is first invoked on the _second_ element, using
the first element as the accumulated value, except for singleton lists in which has the head is returned.

    foldl1 (++) (Nonempty "a" ["b", "c"]) == "cba"
    foldl1 (++) (fromElement "a") == "a"

    findMe = 42
    minimizeMe n = abs (n-findMe)
    nearest = foldl1 (\a b -> if minimizeMe a < minimizeMe b then a else b) (Nonempty 10 [20,30,40,50,60])
    nearest == 40
-}
foldl1 : (a -> a -> a) -> Nonempty a -> a
foldl1 f (Nonempty x xs) =
    List.foldl f x xs


{-| Like `foldl`, but keep each intermediate value. For example, scan addition to create the cumulative sum up to each
element. The head of the new nonempty list is always the base case provided, and the length increases by 1.

    scanl (++) "" (Nonempty "a" ["b", "c"]) == Nonempty "" ["a","ba","cba"]
-}
scanl : (a -> b -> b) -> b -> Nonempty a -> Nonempty b
scanl f b (Nonempty x xs) =
    Nonempty b <| List.scanl f (f x b) xs


{-| Like `foldl1`, but keep each intermediate value. The head and length are not changed.

This example starts with the number of ways to roll exactly index _i_ on two six-sided dice (the probability density
function), and turns it into the number of ways to roll at least _i_ (the cumulative density function).

    dicePDF = Nonempty 0 [0,1,2,3,4,5,6,5,4,3,2,1]
    diceCDF = scanl1 (+) dicePDF
    diceCDF == Nonempty 0 [0,1,3,6,10,15,21,26,30,33,35,36]
-}
scanl1 : (a -> a -> a) -> Nonempty a -> Nonempty a
scanl1 f (Nonempty x xs) =
    case xs of
        [] ->
            Nonempty x []

        y :: ys ->
            Nonempty x (List.scanl f (f y x) ys)
