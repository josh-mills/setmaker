module ForteNumbers exposing (forteNum, primeFormForForteNumber, zRelatedNum)

import Array exposing (Array)
import Bitwise exposing (shiftRightBy)
import Dict exposing (Dict)
import Html exposing (i)
import PcInt exposing (pcInt)
import PcSetBasics exposing (PcSet(..), primeForm)
import Set exposing (Set)


{-| In every row, the first number is what Aleck Brinkman called the "bit vector."
That’s the base-10 number that translates into the prime form of the set if you represent it as base 2 (binary).
"1" in a place means that "pc" is in the set.
So, the number 11 = binary 1011 (8+2+1) stands for [013] because that’s what it looks like.
The second number is the Forte catalog number.
The third number is the Forte number of its Z mate (if it has one).
The fourth number is the Forte number of its M-relation (M5 or M7).
-}
setTable : Dict Int ( Int, Int, Int )
setTable =
    Dict.fromList <|
        List.foldl List.append
            []
            [ emptySet
            , monochord
            , dyads
            , trichords
            , tetrachords
            , pentachords
            , hexachords
            , heptachords
            , octochords
            , enneachords
            , decachords
            , hendecachord
            , dodecachord
            ]


setTableComponents : Array (Dict Int Int)
setTableComponents =
    List.map makeForteNumberToBitVectorDict
        [ emptySet
        , monochord
        , dyads
        , trichords
        , tetrachords
        , pentachords
        , hexachords
        , heptachords
        , octochords
        , enneachords
        , decachords
        , hendecachord
        , dodecachord
        ]
        |> Array.fromList


emptySet : List ( Int, ( Int, Int, Int ) )
emptySet =
    [ ( 0, ( 0, 0, 0 ) ) ]


monochord : List (Int, (Int, Int, Int))
monochord =
    [ ( 1, ( 1, 0, 1 ) ) ]


dyads : List (Int, (Int, Int, Int))
dyads =
    [ {- [ 2-note sets]    [  2] -} ( 3, ( 1, 0, 5 ) )
    , {- [  3] -} ( 5, ( 2, 0, 2 ) )
    , {- [  4] -} ( 9, ( 3, 0, 3 ) )
    , {- [  5] -} ( 17, ( 4, 0, 4 ) )
    , {- [  6] -} ( 33, ( 5, 0, 1 ) )
    , {- [  7] -} ( 65, ( 6, 0, 6 ) )
    ]


trichords : List (Int, (Int, Int, Int))
trichords =
    [ {- [3-note sets] [ 8] -} ( 7, ( 1, 0, 9 ) )
    , ( 11, ( 2, 0, 7 ) )
    , ( 19, ( 3, 0, 11 ) )
    , ( 21, ( 6, 0, 6 ) )
    , ( 35, ( 4, 0, 4 ) )
    , ( 37, ( 7, 0, 2 ) )
    , ( 67, ( 5, 0, 5 ) )
    , ( 69, ( 8, 0, 8 ) )
    , ( 73, ( 10, 0, 10 ) )
    , ( 133, ( 9, 0, 1 ) )
    , ( 137, ( 11, 0, 3 ) )
    , {- [ 19] -} ( 273, ( 12, 0, 12 ) )
    ]


tetrachords : List (Int, (Int, Int, Int))
tetrachords =
    [ {- [ 4-note sets] [ 20] -} ( 15, ( 1, 0, 23 ) )
    , ( 23, ( 2, 0, 22 ) )
    , ( 27, ( 3, 0, 26 ) )
    , ( 39, ( 4, 0, 14 ) )
    , ( 43, ( 11, 0, 11 ) )
    , ( 45, ( 10, 0, 10 ) )
    , ( 51, ( 7, 0, 20 ) )
    , ( 71, ( 5, 0, 16 ) )
    , ( 75, ( 13, 0, 13 ) )
    , ( 77, ( 12, 0, 27 ) )
    , ( 83, ( 15, 29, 29 ) )
    , ( 85, ( 21, 0, 21 ) )
    , ( 99, ( 8, 0, 8 ) )
    , ( 135, ( 6, 0, 6 ) )
    , ( 139, ( 29, 15, 15 ) )
    , ( 141, ( 14, 0, 4 ) )
    , ( 147, ( 18, 0, 18 ) )
    , ( 149, ( 22, 0, 2 ) )
    , ( 153, ( 17, 0, 17 ) )
    , ( 163, ( 16, 0, 5 ) )
    , ( 165, ( 23, 0, 1 ) )
    , ( 195, ( 9, 0, 9 ) )
    , ( 275, ( 19, 0, 19 ) )
    , ( 277, ( 24, 0, 24 ) )
    , ( 291, ( 20, 0, 7 ) )
    , ( 293, ( 27, 0, 12 ) )
    , ( 297, ( 26, 0, 3 ) )
    , ( 325, ( 25, 0, 25 ) )
    , {- [ 48] -} ( 585, ( 28, 0, 28 ) )
    ]


pentachords : List (Int, (Int, Int, Int))
pentachords =
    [ {- [ 5-note sets] [ 49] -} ( 31, ( 1, 0, 35 ) )
    , ( 47, ( 2, 0, 23 ) )
    , ( 55, ( 3, 0, 27 ) )
    , ( 79, ( 4, 0, 29 ) )
    , ( 87, ( 9, 0, 24 ) )
    , ( 91, ( 10, 0, 25 ) )
    , ( 93, ( 8, 0, 34 ) )
    , ( 103, ( 6, 0, 20 ) )
    , ( 107, ( 12, 36, 12 ) )
    , ( 143, ( 5, 0, 14 ) )
    , ( 151, ( 36, 12, 36 ) )
    , ( 155, ( 16, 0, 32 ) )
    , ( 157, ( 11, 0, 11 ) )
    , ( 167, ( 14, 0, 5 ) )
    , ( 171, ( 24, 0, 9 ) )
    , ( 173, ( 23, 0, 2 ) )
    , ( 179, ( 18, 38, 38 ) )
    , ( 199, ( 7, 0, 7 ) )
    , ( 203, ( 19, 0, 19 ) )
    , ( 279, ( 13, 0, 30 ) )
    , ( 283, ( 17, 37, 37 ) )
    , ( 295, ( 38, 18, 18 ) )
    , ( 299, ( 27, 0, 3 ) )
    , ( 301, ( 25, 0, 10 ) )
    , ( 307, ( 21, 0, 21 ) )
    , ( 309, ( 26, 0, 26 ) )
    , ( 313, ( 37, 17, 17 ) )
    , ( 327, ( 15, 0, 15 ) )
    , ( 331, ( 29, 0, 4 ) )
    , ( 333, ( 28, 0, 28 ) )
    , ( 339, ( 30, 0, 13 ) )
    , ( 341, ( 33, 0, 33 ) )
    , {- [Rahn's  5-20] -} ( 355, ( 20, 0, 6 ) )
    , ( 403, ( 22, 0, 22 ) )
    , ( 587, ( 31, 0, 31 ) )
    , ( 595, ( 32, 0, 16 ) )
    , ( 597, ( 34, 0, 8 ) )
    , {- [ 86] -} ( 661, ( 35, 0, 1 ) )
    ]


hexachords : List (Int, (Int, Int, Int))
hexachords =
    [ {- [ 6-note sets] [ 87] -} ( 63, ( 1, 0, 32 ) )
    , ( 95, ( 2, 0, 33 ) )
    , ( 111, ( 3, 36, 25 ) )
    , ( 119, ( 4, 37, 26 ) )
    , ( 159, ( 36, 3, 47 ) )
    , ( 175, ( 9, 0, 9 ) )
    , ( 183, ( 11, 40, 40 ) )
    , ( 187, ( 10, 39, 46 ) )
    , ( 189, ( 8, 0, 8 ) )
    , ( 207, ( 5, 0, 18 ) )
    , ( 215, ( 12, 41, 12 ) )
    , ( 219, ( 13, 42, 50 ) )
    , ( 231, ( 6, 38, 38 ) )
    , ( 287, ( 37, 4, 48 ) )
    , ( 303, ( 40, 11, 11 ) )
    , ( 311, ( 15, 0, 31 ) )
    , ( 315, ( 14, 0, 14 ) )
    , ( 317, ( 39, 10, 24 ) )
    , ( 335, ( 41, 12, 41 ) )
    , ( 343, ( 22, 0, 22 ) )
    , ( 347, ( 24, 46, 39 ) )
    , ( 349, ( 21, 0, 34 ) )
    , ( 359, ( 43, 17, 43 ) )
    , ( 363, ( 25, 47, 3 ) )
    , ( 365, ( 23, 45, 23 ) )
    , ( 371, ( 16, 0, 16 ) )
    , ( 399, ( 38, 6, 6 ) )
    , ( 407, ( 17, 43, 17 ) )
    , ( 411, ( 19, 44, 44 ) )
    , ( 423, ( 18, 0, 5 ) )
    , ( 427, ( 26, 48, 4 ) )
    , ( 455, ( 7, 0, 7 ) )
    , ( 591, ( 42, 13, 29 ) )
    , ( 599, ( 46, 24, 10 ) )
    , ( 603, ( 27, 0, 27 ) )
    , ( 605, ( 45, 23, 45 ) )
    , ( 615, ( 44, 19, 19 ) )
    , ( 619, ( 28, 49, 28 ) )
    , ( 663, ( 47, 25, 36 ) )
    , ( 667, ( 49, 28, 49 ) )
    , ( 679, ( 48, 26, 37 ) )
    , ( 683, ( 34, 0, 21 ) )
    , ( 685, ( 33, 0, 2 ) )
    , {- [Rahn's  6-31] -} ( 691, ( 31, 0, 15 ) )
    , ( 693, ( 32, 0, 1 ) )
    , ( 715, ( 30, 0, 30 ) )
    , {- [Rahn's  6-29] -} ( 717, ( 29, 50, 42 ) )
    , ( 723, ( 50, 29, 13 ) )
    , ( 819, ( 20, 0, 20 ) )
    , {- [136] -} ( 1365, ( 35, 0, 35 ) )
    ]


heptachords : List (Int, (Int, Int, Int))
heptachords =
    [ {- [ 7-note sets] [137] -} ( 127, ( 1, 0, 35 ) )
    , ( 191, ( 2, 0, 23 ) )
    , ( 223, ( 4, 0, 29 ) )
    , ( 239, ( 5, 0, 14 ) )
    , ( 319, ( 3, 0, 27 ) )
    , ( 351, ( 9, 0, 24 ) )
    , ( 367, ( 36, 12, 36 ) )
    , ( 375, ( 13, 0, 30 ) )
    , ( 379, ( 11, 0, 11 ) )
    , ( 381, ( 8, 0, 34 ) )
    , ( 415, ( 6, 0, 20 ) )
    , ( 431, ( 14, 0, 5 ) )
    , ( 439, ( 38, 18, 18 ) )
    , ( 443, ( 37, 17, 17 ) )
    , ( 463, ( 7, 0, 7 ) )
    , ( 471, ( 15, 0, 15 ) )
    , ( 607, ( 10, 0, 25 ) )
    , ( 623, ( 16, 0, 32 ) )
    , ( 631, ( 17, 37, 37 ) )
    , ( 671, ( 12, 36, 12 ) )
    , ( 687, ( 24, 0, 9 ) )
    , ( 695, ( 27, 0, 3 ) )
    , ( 699, ( 26, 0, 26 ) )
    , ( 701, ( 23, 0, 2 ) )
    , ( 719, ( 19, 0, 19 ) )
    , ( 727, ( 29, 0, 4 ) )
    , ( 731, ( 31, 0, 31 ) )
    , ( 733, ( 25, 0, 10 ) )
    , {- [Rahn's  7-20] -} ( 743, ( 20, 0, 6 ) )
    , ( 747, ( 28, 0, 28 ) )
    , {- [Rahn's  7-18] -} ( 755, ( 18, 38, 38 ) )
    , ( 823, ( 21, 0, 21 ) )
    , ( 855, ( 30, 0, 13 ) )
    , ( 859, ( 32, 0, 16 ) )
    , ( 871, ( 22, 0, 22 ) )
    , ( 1367, ( 33, 0, 33 ) )
    , ( 1371, ( 34, 0, 8 ) )
    , {- [174] -} ( 1387, ( 35, 0, 1 ) )
    ]


octochords : List (Int, (Int, Int, Int))
octochords =
    [ {- [ 8-note sets] [175] -} ( 255, ( 1, 0, 23 ) )
    , ( 383, ( 2, 0, 22 ) )
    , ( 447, ( 4, 0, 14 ) )
    , ( 479, ( 5, 0, 16 ) )
    , ( 495, ( 6, 0, 6 ) )
    , ( 639, ( 3, 0, 26 ) )
    , ( 703, ( 11, 0, 11 ) )
    , ( 735, ( 13, 0, 13 ) )
    , ( 751, ( 29, 15, 15 ) )
    , ( 759, ( 14, 0, 4 ) )
    , ( 763, ( 12, 0, 27 ) )
    , ( 765, ( 10, 0, 10 ) )
    , ( 831, ( 7, 0, 20 ) )
    , ( 863, ( 15, 29, 29 ) )
    , ( 879, ( 18, 0, 18 ) )
    , ( 887, ( 19, 0, 19 ) )
    , ( 891, ( 17, 0, 17 ) )
    , ( 927, ( 8, 0, 8 ) )
    , ( 943, ( 16, 0, 5 ) )
    , ( 951, ( 20, 0, 7 ) )
    , ( 975, ( 9, 0, 9 ) )
    , ( 1375, ( 21, 0, 21 ) )
    , ( 1391, ( 22, 0, 2 ) )
    , ( 1399, ( 24, 0, 24 ) )
    , ( 1455, ( 23, 0, 1 ) )
    , ( 1463, ( 27, 0, 12 ) )
    , {- [Rahn's  8-26] -} ( 1467, ( 26, 0, 3 ) )
    , ( 1495, ( 25, 0, 25 ) )
    , {- [203] -} ( 1755, ( 28, 0, 28 ) )
    ]


enneachords : List (Int, (Int, Int, Int))
enneachords =
    [ {- [ 9-note sets] [204] -} ( 511, ( 1, 0, 9 ) )
    , ( 767, ( 2, 0, 7 ) )
    , ( 895, ( 3, 0, 11 ) )
    , ( 959, ( 4, 0, 4 ) )
    , ( 991, ( 5, 0, 5 ) )
    , ( 1407, ( 6, 0, 6 ) )
    , ( 1471, ( 7, 0, 2 ) )
    , ( 1503, ( 8, 0, 8 ) )
    , ( 1519, ( 9, 0, 1 ) )
    , ( 1759, ( 10, 0, 10 ) )
    , ( 1775, ( 11, 0, 3 ) )
    , {- [215] -} ( 1911, ( 12, 0, 12 ) )
    ]


decachords : List (Int, (Int, Int, Int))
decachords =
    [ {- [10-note sets] [216] -} ( 1023, ( 1, 0, 5 ) )
    , ( 1535, ( 2, 0, 2 ) )
    , ( 1791, ( 3, 0, 3 ) )
    , ( 1919, ( 4, 0, 4 ) )
    , ( 1983, ( 5, 0, 1 ) )
    , {- [221] -} ( 2015, ( 6, 0, 6 ) )
    ]


hendecachord : List (Int, (Int, Int, Int))
hendecachord =
    [ {- [11-note sets] [222] -} ( 2047, ( 1, 0, 1 ) ) ]


dodecachord : List (Int, (Int, Int, Int))
dodecachord =
    [ {- [12-note sets] [223] -} ( 4095, ( 1, 0, 1 ) )
    ]


{-| This is well-defined only for the 12edo world. Forte numbers are silly enough
in that space as it is; no sense in trying to generalize this.
-}
pcSetFromBitVectorIndex : Int -> PcSet
pcSetFromBitVectorIndex bitVec =
    let
        edo =
            PcInt.edoFromInt 12
    in
    Array.initialize 12 (\i -> modBy 2 <| shiftRightBy i bitVec)
        |> Array.indexedMap
            (\i p ->
                if p == 1 then
                    Just (pcInt edo i)

                else
                    Nothing
            )
        |> Array.toList
        |> List.filterMap identity
        |> PcSet edo


primeFormForForteNumber : Int -> Int -> Maybe PcSet
primeFormForForteNumber cardinality bitVec =
    Array.get cardinality setTableComponents
        |> Maybe.andThen (Dict.get bitVec)
        |> Maybe.map pcSetFromBitVectorIndex


makeForteNumberToBitVectorDict : List ( Int, ( Int, Int, Int ) ) -> Dict Int Int
makeForteNumberToBitVectorDict sets =
    Dict.fromList <|
        List.map (\( bitVec, ( frtNum, _, _ ) ) -> ( frtNum, bitVec )) sets


getForteNumber : Int -> String
getForteNumber bitVecIndex =
    Dict.get bitVecIndex setTable
        |> Maybe.map
            (\( n, z, _ ) ->
                if z > 0 then
                    "z" ++ String.fromInt n

                else
                    String.fromInt n
            )
        |> Maybe.withDefault " Error: cannot find Forte number"


getZrelatedNumber : Int -> Maybe String
getZrelatedNumber bitVecIndex =
    Dict.get bitVecIndex setTable
        |> Maybe.map (\( _, z, _ ) -> z)
        |> Maybe.andThen
            (\i ->
                if i > 0 then
                    Just (String.fromInt i)

                else
                    Nothing
            )


isSet12edo : PcSet -> Bool
isSet12edo (PcSet edo _) =
    PcInt.edoToInt edo == 12


{-| Returns a string of the Forte number (e.g., "3–11", "4–z15", etc.) for the given set for 12edo PcSets,
or Nothing otherwise.
-}
forteNum : PcSet -> Maybe String
forteNum set =
    if isSet12edo set then
        Just
            ((String.fromInt <| PcSetBasics.cardinality set)
                ++ "–"
                ++ (getForteNumber <| bitVectorIndex <| primeForm set)
            )

    else
        Nothing


zRelatedNum : PcSet -> Maybe String
zRelatedNum set =
    let
        zNum =
            if isSet12edo set then
                getZrelatedNumber <| bitVectorIndex <| primeForm set

            else
                Nothing
    in
    Maybe.map
        (\s ->
            (String.fromInt <| PcSetBasics.cardinality set)
                ++ "–z"
                ++ s
        )
        zNum


{-| Aleck Brinkman's "bit vector" is the base-10 number that translates into the prime form of the set if you represent it as base 2.
So, the number 11 = binary 1011 (8+2+1) stands for [013] because that’s what it looks like.
[0146] translates to 1010011 = 1+2+16+64 = 83.
-}
bitVectorIndex : PcSet -> Int
bitVectorIndex (PcSet edo pcs) =
    let
        set : Set Int
        set =
            List.map PcInt.pcIntToInt pcs
                |> Set.fromList

        f : Int -> Int
        f i =
            if Set.member i set then
                2 ^ i

            else
                0
    in
    Array.initialize (PcInt.edoToInt edo) f
        |> Array.foldl (+) 0
