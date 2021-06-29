module IntervalCycles exposing (..)

import Arithmetic exposing (isEven, isOdd)
import Array exposing (Array)
import Helpers
import Html exposing (a)
import List.Extra
import PcInt exposing (Edo, PcInt, edoToInt)
import PcSetBasics exposing (PcSet(..), cardinality)
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



{--Okay, this isn't working. I need another algorithm.
cycleSegNotWorking : IntervalCycleSet -> List Int
cycleSegNotWorking icSet =
    let
        wrapIfNeeded : List Bool -> List Bool
        wrapIfNeeded l =
            if List.length l > 2 then
                Helpers.wrapList l
            else
                l
    in
    
    List.map
        (List.map
            (\m ->
                case m of
                    Just _ ->
                        True

                    Nothing ->
                        False
            )
        )
        icSet
        -- : List (List Bool)
        |> List.map wrapIfNeeded
        -- : List (List Bool)
        |> List.map List.Extra.group
        -- : List (List ( Bool, List Bool ))
        |> List.map (List.filter (\( b, _ ) -> b))
        -- : List (List ( Bool, List Bool ))
        |> List.map (List.map (\( _, l ) -> List.length l))
        -- : List (List Int)
        |> List.map (List.filter (\i -> i > 0) ) 
        |> List.map List.sort
        |> List.map List.reverse
        |> List.foldl List.append []
-}


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


wiccv : Float -> PcSet -> List Float
wiccv k pcSet =
    iccv pcSet
        |> List.map (List.map (weightRecursive k))
        |> List.map List.sum


wiccvString : Float -> PcSet -> String
wiccvString k set =
    let
        round2 n =
            n * 100 |> round |> toFloat |> (\x -> x / 100)
    in
    wiccv k set
        |> List.map round2
        |> List.map String.fromFloat
        |> String.join ", "
        |> (\s -> "<" ++ s ++ ">")


orderToMaximizeIC : Int -> Int -> List Int
orderToMaximizeIC modulus ic =
    genericIntervalCycle modulus ic
        |> List.foldr List.append []
        |> List.map (Maybe.withDefault 0)


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


{-| Array of minimally saturated sets for a given interval class.
Index position corresponds to the cardinality of the set, beginning with 0
(empty set) and continuing through the the full aggregate.
-}
minimallySaturatedSets : Edo -> Int -> List PcSet
minimallySaturatedSets modulus intervalClass =
    saturatedSets orderToMinimizeIC modulus intervalClass


{-| Array of maximally saturated sets for a given interval class.
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
