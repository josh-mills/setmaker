module PcSetBasics exposing (PcSet(..), aggregate, cardinality, comparePcInts, complement, icCount, icVector, invertSet, normalForm, primeForm, setInts, setModulus, setToString, transposeSet, transposeToZero)

import Array exposing (Array)
import Dict exposing (Dict)
import Helpers exposing (compareLists, rotationalArrays)
import List.Extra
import PcInt exposing (Edo, PcInt, edoToInt, intervalClass, pcIntToInt)
import Set exposing (Set)


{-| Basic analysis and transformations of pitch class sets


# Analysis

@docs normalForm, primeForm, primeFormForte


# Transformations

@docs ???

-}
type PcSet
    = PcSet Edo (List PcInt)


{-| Set formatted as a comma-separated string in brackets: "{2, 6, 9}"
-}
setToString : PcSet -> String
setToString (PcSet _ pcs) =
    "{" ++ (String.join ", " <| List.map PcInt.toString pcs) ++ "}"


cardinality : PcSet -> Int
cardinality (PcSet _ pcs) =
    List.length pcs


setModulus : PcSet -> Int
setModulus (PcSet edo _) =
    edoToInt edo


setInts : PcSet -> Set Int
setInts (PcSet _ pcs) =
    List.map PcInt.pcIntToInt pcs
        |> Set.fromList



{- Ordering function to sort the pitch classes into their most compact form.
   This will enable easy comparisons between different sets to identify possible transformations.
   Uses the Rahn algorithm.
-}


normalForm : PcSet -> PcSet
normalForm (PcSet edo pcs) =
    let
        rotations : List ( Int, List PcInt )
        rotations =
            rotationalArrays pcs
                |> List.map
                    (\l ->
                        ( List.head l |> Maybe.map PcInt.pcIntToInt |> Maybe.withDefault 0
                        , l
                        )
                    )
                |> List.map (Tuple.mapSecond <| transposeToZero edo)
                |> List.map (Tuple.mapSecond List.reverse)

        --will need extra finagling at this point to case on Forte/Rahn:
        --if Forte, reverse the tail. (We'll need to undo this.)
    in
    case mostCompact rotations of
        Nothing ->
            PcSet edo []

        Just ( i, list ) ->
            List.reverse list
                |> List.map (PcInt.transposePcInt edo i)
                |> PcSet edo


{-| Determine the most compact List PcInt from the list of options.
-}
mostCompact : List ( Int, List PcInt ) -> Maybe ( Int, List PcInt )
mostCompact list =
    let
        compTuples : ( Int, List PcInt ) -> ( Int, List PcInt ) -> Order
        compTuples ( _, l1 ) ( _, l2 ) =
            compareLists comparePcInts l1 l2
    in
    List.Extra.minimumWith compTuples list


comparePcInts : PcInt -> PcInt -> Order
comparePcInts pc1 pc2 =
    compare (pcIntToInt pc1) (pcIntToInt pc2)


{-| Prime form according to the Rahn algorithm
-}
primeForm : PcSet -> PcSet
primeForm set =
    let
        edo : Edo
        edo =
            case set of
                PcSet e _ ->
                    e

        original : List PcInt
        original =
            normalForm set
                |> (\s ->
                        case s of
                            PcSet _ pcs ->
                                pcs
                   )
                |> transposeToZero edo

        inverted : List PcInt
        inverted =
            invertSet 0 set
                |> normalForm
                |> (\s ->
                        case s of
                            PcSet _ pcs ->
                                pcs
                   )
                |> transposeToZero edo
    in
    case compareLists comparePcInts original inverted of
        GT ->
            PcSet edo inverted

        LT ->
            PcSet edo original

        EQ ->
            PcSet edo original


{-| Positional array representing the count of each interval class between all
notes in the set (index 0 represents total occurences of IC 1, index 1 represents
total occurences of IC 2, and so on).
-}
icVector : PcSet -> Array Int
icVector (PcSet edo pcs) =
    let
        intervalCounts : Dict Int Int
        intervalCounts =
            List.Extra.uniquePairs pcs
                |> List.map (\( a, b ) -> intervalClass edo a b)
                |> List.Extra.gatherEquals
                |> List.map (\( a, aas ) -> ( a, List.length aas + 1 ))
                |> Dict.fromList

        vectorLength =
            PcInt.edoToInt edo // 2
    in
    Array.initialize vectorLength (\i -> Dict.get (i + 1) intervalCounts)
        |> Array.map (Maybe.withDefault 0)


{-| Count of the number of occurences of the specified interval class in the set.
This is more efficient than looking up the interval in an IC vector if you only
need a single ic rather than the whole vector.
-}
icCount : Int -> PcSet -> Int
icCount ic (PcSet modulus pcs) =
    List.Extra.uniquePairs pcs
        |> List.map (\( a, b ) -> intervalClass modulus a b)
        |> List.filter (\i -> i == ic)
        |> List.length


transposeToZero : Edo -> List PcInt -> List PcInt
transposeToZero edo pcs =
    let
        int : Int
        int =
            List.head pcs
                |> Maybe.map pcIntToInt
                |> Maybe.withDefault 0
                |> (*) -1
    in
    List.map (PcInt.transposePcInt edo int) pcs


transposeSet : Int -> PcSet -> PcSet
transposeSet index (PcSet edo pcs) =
    List.map (PcInt.transposePcInt edo index) pcs
        |> PcSet edo


invertSet : Int -> PcSet -> PcSet
invertSet index (PcSet edo pcs) =
    List.map (PcInt.invertPcInt edo index) pcs
        |> List.reverse
        |> PcSet edo


aggregate : Edo -> PcSet
aggregate edo =
    edoToInt edo
        - 1
        |> List.range 0
        |> List.map (PcInt.pcInt edo)
        |> PcSet edo


aggregateInts : Edo -> Set Int
aggregateInts edo =
    edoToInt edo
        - 1
        |> List.range 0
        |> Set.fromList


complement : PcSet -> PcSet
complement (PcSet edo pcs) =
    List.map PcInt.pcIntToInt pcs
        |> Set.fromList
        |> Set.diff (aggregateInts edo)
        |> Set.toList
        |> List.map (PcInt.pcInt edo)
        |> PcSet edo
