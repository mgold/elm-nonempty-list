module List.Nonempty where

{-| A list that cannot be empty.

# Definition
@docs Nonempty

# Create
@docs fromElement, fromList

# Retrieve values
@docs head, tail, toList

# Inspect
Nonempty lists support equality with the usual `(==)` operator.
@docs isSingleton, length

# Convert
@docs cons, replaceHead, dropTail, map

-}

{-| The Nonempty type. Unless you have both a head and tail, you'll normally use the provided interface.
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

{-| Add another element to the head of the list. Also available infix as `(:::)`; be sure to add `exposing ((:::))` to
your import.
-}
cons : a -> Nonempty a -> Nonempty a
cons y (Nonempty x xs) = Nonempty y (x::xs)

(:::) = cons
infixr 5 :::

{-| Exchange the head element while leaving the tail alone.
-}
replaceHead : a -> Nonempty a -> Nonempty a
replaceHead y (Nonempty x xs) = Nonempty y xs

{-| Replace the tail with the empty list while leaving the head alone.
-}
dropTail : Nonempty a -> Nonempty a
dropTail (Nonempty x xs) = Nonempty x []

{-| Map a function over the nonempty list.
-}
map : (a -> b) -> Nonempty a -> Nonempty b
map f (Nonempty x xs) = Nonempty (f x) (List.map f xs)

{-| Determine if the nonempty list has exactly one element.
-}
isSingleton : Nonempty a -> Bool
isSingleton (Nonempty x xs) = List.isEmpty xs

{-| Find the length of the nonempty list.
-}
length : Nonempty a -> Int
length (Nonempty x xs) = List.length xs + 1

