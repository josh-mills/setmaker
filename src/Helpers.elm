module Helpers exposing (compareLists, orderToInitialNothing, rotationalArrays, sublistsFromHead, wrapList)

import List.Extra


{-| Compare two lists by comparing each successive pair of items using the
given function f until a non-equal ordering is established or until the lists
are exhausted.

  - compareLists compare [5, 4, 2][5, 4, 1, 0] == GT
  - compareLists compare [4, 2] == EQ
  - compareLists compare [][7] == LT

-}
compareLists : (a -> a -> Order) -> List a -> List a -> Order
compareLists f a b =
    case ( a, b ) of
        ( h1 :: t1, h2 :: t2 ) ->
            case f h1 h2 of
                GT ->
                    GT

                LT ->
                    LT

                EQ ->
                    compareLists f t1 t2

        ( [], [] ) ->
            EQ

        ( _ :: _, [] ) ->
            GT

        ( [], _ :: _ ) ->
            LT


{-| Create a list of all rotations of a list.

rotationalArrays [1,2,3,4] == [ [1,2,3,4], [2,3,4,1], [3,4,1,2], [4,1,2,3] ]

-}
rotationalArrays : List a -> List (List a)
rotationalArrays list =
    let
        rotateN : List a -> Int -> List a
        rotateN l n =
            let
                front =
                    List.take n l

                back =
                    List.drop n l
            in
            back ++ front
    in
    List.range 0 (List.length list - 1)
        |> List.map (rotateN list)


{-| Wrap a list so the head is also the last element.

wrapList [1, 2, 3] -> [1, 2, 3, 1]

wrapList [1] -> [1, 1]

wrapList [] -> []

-}
wrapList : List a -> List a
wrapList l =
    case l of
        h :: _ ->
            l ++ [ h ]

        [] ->
            []


orderToInitialNothing : List (Maybe a) -> List (Maybe a)
orderToInitialNothing l =
    let
        isNothing : Maybe a -> Bool
        isNothing a =
            case a of
                Just _ ->
                    False

                Nothing ->
                    True
    in
    List.Extra.break isNothing l
        |> (\( a, b ) -> b ++ a)


{-| Take a list and return a list of sublists from the first element
to each other element.

[0, 1, 2, 3] -> [ [0], [0, 1], [0, 1, 2], [0, 1, 2, 3] ]

["foo"] -> [ ["foo"] ]

[] -> []

-}
sublistsFromHead : List a -> List (List a)
sublistsFromHead list =
    let
        go : List (List a) -> List a -> List (List a)
        go acc remaining =
            case ( acc, remaining ) of
                ( [], next :: rest ) ->
                    go [ [ next ] ] rest

                ( _, [] ) ->
                    acc

                ( h :: _, next :: rest ) ->
                    go ((next :: h) :: acc) rest
    in
    go [] list
        |> List.map List.reverse
        |> List.reverse
