module List.Nonempty where

{-| A list that cannot be empty. The head and tail can be accessed without Maybes.

# Definition
@docs Nonempty

# Create
@docs fromElement, fromList

# Access
@docs head, tail, toList

# Inspect
Nonempty lists support equality with the usual `(==)` operator.
@docs isSingleton, length

# Convert
@docs cons, pop, reverse, concat

# Swap
@docs replaceHead, replaceTail, dropTail

# Map
@docs map, map2

# Deduplicate
The elements must support equality or you will get a runtime error.
@docs dedup, uniq

-}

{-| The Nonempty type. If you have both a head and tail, you can construct a
nonempty list directly. Usually you'll use one of the many helpers below instead.
-}
type Nonempty a = Nonempty a (List a)

{-| Create a singleton list with the given element.
-}
fromElement : a -> Nonempty a
fromElement x = Nonempty x []

{-| Create a nonempty list from an ordinary list, failing on the empty list.
-}
fromList : List a -> Maybe (Nonempty a)
fromList ys = case ys of
    (x::xs) -> Just (Nonempty x xs)
    _ -> Nothing

{-| Return the head of the list.
-}
head : Nonempty a -> a
head (Nonempty x xs) = x

{-| Return the tail of the list.
-}
tail : Nonempty a -> List a
tail (Nonempty x xs) = xs

{-| Convert to an ordinary list.
-}
toList : Nonempty a -> List a
toList (Nonempty x xs) = x::xs

{-| Add another element as the head of the list. Also available infix as
`(:::)`; be sure to add `exposing ((:::))` to your import. -}
cons : a -> Nonempty a -> Nonempty a
cons y (Nonempty x xs) = Nonempty y (x::xs)

(:::) = cons
infixr 5 :::

{-| Pop and discard the head, or do nothing for a singleton list. Useful if you
want to exhaust a list but hang on to the last item indefinitely. -}
pop : Nonempty a -> Nonempty a
pop (Nonempty x xs) = case xs of
    [] -> Nonempty x xs
    y::ys -> Nonempty y ys

{-| Reverse a nonempty list.
-}
reverse : Nonempty a -> Nonempty a
reverse (Nonempty x xs) =
    let revapp : (List a, a, List a) -> Nonempty a
        revapp (ls, c, rs) = case rs of
            [] -> Nonempty c ls
            r::rs' -> revapp (c::ls, r, rs')
    in revapp ([], x, xs)

{-| Flatten a nonempty list of nonempty lists into a single nonempty list.
-}
concat : Nonempty (Nonempty a) -> Nonempty a
concat (Nonempty xs xss) =
    let hd = head xs
        tl = tail xs ++ List.concat (List.map toList xss)
    in Nonempty hd tl

{-| Exchange the head element while leaving the tail alone.
-}
replaceHead : a -> Nonempty a -> Nonempty a
replaceHead y (Nonempty x xs) = Nonempty y xs

{-| Exchange the tail for another while leaving the head alone.
-}
replaceTail : List a -> Nonempty a -> Nonempty a
replaceTail ys (Nonempty x xs) = Nonempty x ys

{-| Replace the tail with the empty list while leaving the head alone.
-}
dropTail : Nonempty a -> Nonempty a
dropTail (Nonempty x xs) = Nonempty x []

{-| Map a function over a nonempty list.
-}
map : (a -> b) -> Nonempty a -> Nonempty b
map f (Nonempty x xs) = Nonempty (f x) (List.map f xs)

{-| Map a function over two nonempty lists.
-}
map2 : (a -> b -> c) -> Nonempty a -> Nonempty b -> Nonempty c
map2 f (Nonempty x xs) (Nonempty y ys) = Nonempty (f x y) (List.map2 f xs ys)

{-| Determine if the nonempty list has exactly one element.
-}
isSingleton : Nonempty a -> Bool
isSingleton (Nonempty x xs) = List.isEmpty xs

{-| Find the length of the nonempty list.
-}
length : Nonempty a -> Int
length (Nonempty x xs) = List.length xs + 1

{-| Remove _adjacent_ duplicate elements from the nonempty list. For example, `1,2,2,1` becomes `1,2,1`.
-}
dedup : Nonempty a -> Nonempty a
dedup (Nonempty x xs) =
    let dedupe : a -> List a -> List a -> Nonempty a
        dedupe prev done next = case next of
            [] -> Nonempty prev done
            y::ys -> if y == prev
                     then dedupe prev done ys
                     else dedupe y (prev::done) ys
    in reverse <| dedupe x [] xs

{-| Remove _all_ duplicate elements from the nonempty list, with the remaining elements ordered by first occurrence. For example, `1,2,2,1` becomes `1,2`.
-}
uniq : Nonempty a -> Nonempty a
uniq (Nonempty x xs) =
    let unique : List a -> Nonempty a -> List a -> Nonempty a
        unique seen done next = case next of
            [] -> done
            y::ys -> if List.member y seen
                     then unique seen done ys
                     else unique (y::seen) (y:::done) ys
    in reverse <| unique [x] (Nonempty x []) xs
