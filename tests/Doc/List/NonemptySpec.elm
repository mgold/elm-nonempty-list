module Doc.List.NonemptySpec exposing (spec)

import Test
import Expect
import List.Nonempty exposing(..)


spec : Test.Test
spec =
    Test.describe "List.Nonempty" <|
        [ Test.describe "#pop" <|
            [ Test.test "Example: 1 -- `pop (Nonempty 3 [2,1]) --> Nonempty 2...`" <|
                \() ->
                    Expect.equal
                        (
                            pop (Nonempty 3 [2,1])
                        )
                        (
                            Nonempty 2 [1]
                        )
            , Test.test "Example: 2 -- `pop (Nonempty 1 []) --> Nonempty 1 []`" <|
                \() ->
                    Expect.equal
                        (
                            pop (Nonempty 1 [])
                        )
                        (
                            Nonempty 1 []
                        )
            ]
        , Test.describe "#filter" <|
            let
                isEven : Int -> Bool
                isEven n = n % 2 == 0
            in
            [ Test.test "Example: 1 -- `filter isEven 0 (Nonempty 7 [2, 5]) -...`" <|
                \() ->
                    Expect.equal
                        (
                            filter isEven 0 (Nonempty 7 [2, 5])
                        )
                        (
                            fromElement 2
                        )
            , Test.test "Example: 2 -- `filter isEven 0 (Nonempty 7 []) --> f...`" <|
                \() ->
                    Expect.equal
                        (
                            filter isEven 0 (Nonempty 7 [])
                        )
                        (
                            fromElement 0
                        )
            ]
        , Test.describe "#dedup" <|
            [ Test.test "Example: 1 -- `dedup (Nonempty 1 [2, 2, 1]) --> None...`" <|
                \() ->
                    Expect.equal
                        (
                            dedup (Nonempty 1 [2, 2, 1])
                        )
                        (
                            Nonempty 1 [2, 1]
                        )
            ]
        , Test.describe "#uniq" <|
            [ Test.test "Example: 1 -- `uniq (Nonempty 1 [2, 2, 1]) --> Nonem...`" <|
                \() ->
                    Expect.equal
                        (
                            uniq (Nonempty 1 [2, 2, 1])
                        )
                        (
                            Nonempty 1 [2]
                        )
            ]
        , Test.describe "#foldl" <|
            [ Test.test "Example: 1 -- `foldl (++) \"\" (Nonempty \"a\" [\"b\", \"c\"...`" <|
                \() ->
                    Expect.equal
                        (
                            foldl (++) "" (Nonempty "a" ["b", "c"])
                        )
                        (
                            "cba"
                        )
            ]
        , Test.describe "#foldl1" <|
            [ Test.test "Example: 1 -- `foldl1 (++) (Nonempty \"a\" [\"b\", \"c\"])...`" <|
                \() ->
                    Expect.equal
                        (
                            foldl1 (++) (Nonempty "a" ["b", "c"])
                        )
                        (
                            "cba"
                            foldl1 (++) (fromElement "a")
                        )
            ]
        , Test.describe "#scanl" <|
            [ Test.test "Example: 1 -- `scanl (++) \"\" (Nonempty \"a\" [\"b\", \"c\"...`" <|
                \() ->
                    Expect.equal
                        (
                            scanl (++) "" (Nonempty "a" ["b", "c"])
                        )
                        (
                            Nonempty "" ["a","ba","cba"]
                        )
            ]
        , Test.describe "#scanl1" <|
            let
                dicePDF : Nonempty Int
                dicePDF = Nonempty 0 [0,1,2,3,4,5,6,5,4,3,2,1]
            in
            [ Test.test "Example: 1 -- `scanl1 (+) dicePDF --> Nonempty 0 [0,...`" <|
                \() ->
                    Expect.equal
                        (
                            scanl1 (+) dicePDF
                        )
                        (
                            Nonempty 0 [0,1,3,6,10,15,21,26,30,33,35,36]
                        )
            ]
    ]