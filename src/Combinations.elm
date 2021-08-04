module Combinations exposing (..)

import List.Extra
import PcInt exposing (Edo, edoToInt)
import PcSetBasics exposing (PcSet(..))
import Set exposing (Set)
import Transformations exposing (Transformation(..), transform)


type alias Combination =
    { a : PcSet
    , b : PcSet
    , t : Transformation
    , a_plus_tb : PcSet
    }


combinatorialPossibilities : PcSet -> PcSet -> List Combination
combinatorialPossibilities (PcSet edo pcsA) setB =
    let
        s : Set Int
        s =
            Set.fromList <| List.map PcInt.pcIntToInt pcsA

        isNonIntersectingWithA (PcSet _ pcs) =
            List.map PcInt.pcIntToInt pcs
                |> List.all (\i -> not <| Set.member i s)

        transformTuple set transformation =
            ( transformation, transform set transformation )
    in
    allTransformations edo
        |> List.map (transformTuple setB)
        |> List.filter (\( _, t_b ) -> isNonIntersectingWithA t_b)
        |> List.map (\( t, t_b ) -> Combination (PcSet edo pcsA) setB t (combineSets t_b <| PcSet edo pcsA))


combineSets : PcSet -> PcSet -> PcSet
combineSets (PcSet edo pcsA) (PcSet _ pcsB) =
    List.append pcsA pcsB
        |> List.Extra.uniqueBy PcInt.pcIntToInt
        |> List.sortBy PcInt.pcIntToInt
        |> PcSet edo
        |> PcSetBasics.normalForm


allTransformations : Edo -> List Transformation
allTransformations edo =
    let
        list =
            List.range 0 <| edoToInt edo - 1

        transpositions =
            List.map Transposition list

        inversions =
            List.map Inversion list
    in
    List.append transpositions inversions
