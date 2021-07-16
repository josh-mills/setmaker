module IntervalCycles exposing (..)

import Arithmetic exposing (isEven, isOdd)
import Array exposing (Array)
import Helpers
import Html exposing (a)
import List.Extra
import PcInt exposing (Edo, PcInt, edoToInt, pcInt)
import PcSetBasics exposing (PcSet(..), cardinality)
import Round
import Set exposing (Set(..))


type alias IntervalCycleSet =
    List (List (Maybe Int))


genericIntervalCycle : Int -> Int -> IntervalCycleSet
genericIntervalCycle modulus interval =
    let
        gcd =
            Arithmetic.gcd interval modulus

        numCycles =
            modulus // gcd
    in
    List.range 0 (gcd - 1)
        |> List.map
            (\start ->
                Array.initialize numCycles (\i -> modBy modulus <| start + i * interval)
            )
        |> List.map Array.toList
        |> List.map (List.map (\i -> Just i))


print : IntervalCycleSet -> String
print cycles =
    let
        formatCycle : List (Maybe Int) -> String
        formatCycle c =
            List.map (Maybe.map String.fromInt) c
                |> List.map (Maybe.withDefault "_")
                |> String.join ", "
                |> (\s -> "[" ++ s ++ "]")
    in
    List.map formatCycle cycles
        |> String.join " "


intervalCycleFragmentations : PcSet -> List IntervalCycleSet
intervalCycleFragmentations pcSet =
    let
        modulus =
            PcSetBasics.setModulus pcSet

        set : Set Int
        set =
            PcSetBasics.setInts pcSet
    in
    List.range 1 (modulus // 2)
        |> List.map (genericIntervalCycle modulus)
        |> List.map
            (List.map <|
                List.map
                    (\i ->
                        case i of
                            Just n ->
                                if Set.member n set then
                                    Just n

                                else
                                    Nothing

                            Nothing ->
                                Nothing
                    )
            )



cycleSeg : IntervalCycleSet -> List Int
cycleSeg icSet =
    let
        isJust : Maybe a -> Bool
        isJust a =
            case a of
                Just _ ->
                    True

                Nothing ->
                    False

        wrapIfNeeded : List Bool -> List Bool
        wrapIfNeeded l =
            let
                onlyTrues =
                    (List.length <| List.filter (\b -> not b) l) == 0
            in
            if onlyTrues && List.length l > 2 then
                Helpers.wrapList l

            else
                l
    in
    List.map (\l -> Helpers.orderToInitialNothing l) icSet
        |> List.map (\l -> List.map isJust l)
        |> List.map wrapIfNeeded
        -- : List (List Bool)
        |> List.map List.Extra.group
        -- : List (List ( Bool, List Bool ))
        |> List.map (List.filter (\( b, _ ) -> b))
        -- : List (List ( Bool, List Bool ))
        |> List.map (List.map (\( _, l ) -> List.length l))
        -- : List (List Int)
        |> List.map (List.filter (\i -> i > 0))
        |> List.map List.sort
        |> List.map List.reverse
        |> List.foldl List.append []


iccv : PcSet -> List (List Int)
iccv pcSet =
    intervalCycleFragmentations pcSet
        |> List.map cycleSeg


iccvString : PcSet -> String
iccvString pcSet =
    iccv pcSet
        |> List.map (List.map String.fromInt)
        |> List.map (String.join ", ")
        |> List.map (\s -> "{" ++ s ++ "}")
        |> String.join ", "
        |> (\s -> "<" ++ s ++ ">")


weightRecursive : Float -> Int -> Float
weightRecursive k n =
    if n < 1 then
        0

    else
        k * (1 + weightRecursive k (n - 1))


weight : Float -> Int -> Float
weight k n =
    if n < 1 then
        0

    else
        k / (k - 1) * (k ^ (toFloat n) - 1)


wiccv : Float -> PcSet -> List Float
wiccv k pcSet =
    iccv pcSet
        |> List.map (List.map (weight k))
        |> List.map List.sum


wiccvString : Float -> PcSet -> String
wiccvString k set =
    wiccv k set
        |> List.map (Round.round 2)
        |> String.join ", "
        |> (\s -> "<" ++ s ++ ">")


orderToMaximizeIC : Int -> Int -> List Int
orderToMaximizeIC modulus ic =
    genericIntervalCycle modulus ic
        |> List.foldr List.append []
        |> List.map (Maybe.withDefault 0)



{- THIS IS NOT USEFUL.
   It will minimize the IC, but not the interval cycle _clustering_.
-}


orderToMinimizeIC : Int -> Int -> List Int
orderToMinimizeIC modulus ic =
    let
        gic : IntervalCycleSet
        gic =
            genericIntervalCycle modulus ic

        f : List (List a) -> List a
        f l =
            List.Extra.transpose l
                |> List.foldr List.append []

        evens : List Int
        evens =
            gic
                |> List.map (List.Extra.removeIfIndex isOdd)
                |> f
                |> List.map (Maybe.withDefault 0)

        odds : List Int
        odds =
            gic
                |> List.map (List.Extra.removeIfIndex isEven)
                |> f
                |> List.map (Maybe.withDefault 0)
    in
    evens ++ odds


minIcsForCardinality : Edo -> Int -> Int -> Int
minIcsForCardinality modulus ic cardinality =
    let
        m =
            edoToInt modulus

        set : PcSet
        set =
            orderToMinimizeIC m ic
                |> List.take cardinality
                |> List.map (PcInt.pcInt modulus)
                |> PcSet modulus
    in
    PcSetBasics.icVector set
        |> Array.get (ic - 1)
        |> Maybe.withDefault 42


maxIcsForCardinality : Edo -> Int -> Int -> Int
maxIcsForCardinality modulus ic cardinality =
    let
        m =
            edoToInt modulus

        set =
            orderToMaximizeIC m ic
                |> List.take cardinality
                |> List.map (PcInt.pcInt modulus)
                |> PcSet modulus
    in
    PcSetBasics.icVector set
        |> Array.get (ic - 1)
        |> Maybe.withDefault 42


{-| To minimize the number of occurences of an IC, all we need is a general
ordering to minimize that IC and a number of PCs to take. But to maximally
break up interval cycles, we also need to know the cardinality of the set.
-}
minCycleSaturationForCardinality : Edo -> Int -> Int -> PcSet
minCycleSaturationForCardinality modulus ic cardinality =
    let
        gic : List (List Int)
        gic =
            genericIntervalCycle (edoToInt modulus) ic
                |> List.map (List.map <| Maybe.withDefault 0)

        numCycles : Int
        numCycles =
            List.length gic

        numGaps : Int
        numGaps =
            edoToInt modulus - cardinality

        gapsToDistribute : List Int
        gapsToDistribute =
            List.repeat numCycles (numGaps // numCycles)
                |> List.indexedMap
                    (\i a ->
                        if i < modBy numCycles numGaps then
                            a + 1

                        else
                            a
                    )
    in
    List.Extra.zip gic gapsToDistribute
        |> List.map (\( cycle, gapsNeeded ) -> Helpers.distributeGaps cycle gapsNeeded)
        |> List.foldl List.append []
        |> List.map (pcInt modulus)
        |> PcSet modulus


maxCycleSaturationForCardinality : Edo -> Int -> Int -> PcSet
maxCycleSaturationForCardinality modulus ic cardinality =
    orderToMaximizeIC (edoToInt modulus) ic
        |> List.take cardinality
        |> List.map (pcInt modulus)
        |> PcSet modulus


{-| List of minimally saturated sets for a given interval class.
Index position corresponds to the cardinality of the set, beginning with 0
(empty set) and continuing through the the full aggregate.
-}
minimallySaturatedSets : Edo -> Int -> List PcSet
minimallySaturatedSets modulus intervalClass =
    saturatedSets orderToMinimizeIC modulus intervalClass


{-| List of maximally saturated sets for a given interval class.
Index position corresponds to the cardinality of the set, beginning with 0
(empty set) and continuing through the the full aggregate.
-}
maximallySaturatedSets : Edo -> Int -> List PcSet
maximallySaturatedSets modulus intervalClass =
    saturatedSets orderToMaximizeIC modulus intervalClass


saturatedSets : (Int -> Int -> List Int) -> Edo -> Int -> List PcSet
saturatedSets pcOrderingFunction modulus intervalClass =
    let
        m =
            edoToInt modulus

        pcOrder : List PcInt
        pcOrder =
            pcOrderingFunction m intervalClass |> List.map (PcInt.pcInt modulus)
    in
    Helpers.sublistsFromHead pcOrder
        |> List.map (PcSet modulus)


{-| Array of float arrays.
The outer array is the position of the integer class (beware off-by-one errors).
The inner array is the cardinality of the set (index 0 is the empty set).
-}
minimumWICCs : Edo -> Float -> Array (Array Float)
minimumWICCs edo weightingConstant =
    let
        maxIC =
            edoToInt edo // 2
    in
    List.range 1 maxIC
        |> List.map (minimumWICCsForIC edo weightingConstant)
        |> Array.fromList


{-| position in the array indicates cardinality
-}
minimumWICCsForIC : Edo -> Float -> Int -> Array Float
minimumWICCsForIC edo weightingConstant ic =
    Array.initialize (edoToInt edo + 1) (minCycleSaturationForCardinality edo ic)
        |> Array.map (wiccv weightingConstant)
        |> Array.map (List.Extra.getAt (ic - 1))
        |> Array.map (Maybe.withDefault 0)


maximumWICCs : Edo -> Float -> Array (Array Float)
maximumWICCs edo weightingConstant =
    let
        maxIC =
            edoToInt edo // 2
    in
    List.range 0 maxIC
        |> List.map (maximumWICCsForIC edo weightingConstant)
        |> Array.fromList


maximumWICCsForIC : Edo -> Float -> Int -> Array Float
maximumWICCsForIC edo weightingConstant ic =
    maximallySaturatedSets edo ic
        |> List.map (wiccv weightingConstant)
        |> List.map (List.Extra.getAt (ic - 1))
        |> List.map (Maybe.withDefault 0)
        |> Array.fromList


type alias CyclicProportionSaturation =
    { minimum : Float
    , maximum : Float
    , value : Float
    }


setEdo : PcSet -> Edo
setEdo (PcSet edo _) =
    edo



{- Cyclic Proportion Saturation Vector -}


makeCPSATV : Float -> PcSet -> List CyclicProportionSaturation
makeCPSATV weightingConstant set =
    let
        cardinality =
            PcSetBasics.cardinality set

        edo : Edo
        edo =
            (\(PcSet e _) -> e) set

        maxIc =
            edoToInt edo

        wiccVals : (Edo -> Int -> Int -> PcSet) -> List Float
        wiccVals cycleSaturationForCardFunction =
            List.range 1 maxIc
                |> List.map (\ic -> cycleSaturationForCardFunction edo ic cardinality)
                |> List.map (wiccv weightingConstant)
                |> List.indexedMap (\i -> List.Extra.getAt i)
                |> List.map (Maybe.withDefault 0)

        minWiccVals : List Float
        minWiccVals =
            wiccVals minCycleSaturationForCardinality

        maxWiccVals : List Float
        maxWiccVals =
            wiccVals maxCycleSaturationForCardinality
    in
    List.map3 CyclicProportionSaturation minWiccVals maxWiccVals (wiccv weightingConstant set)
