module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h2, h3, h4, input, li, ol, p, span, text, ul)
import Html.Attributes as Attr exposing (id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import IntervalCycles exposing (iccvString, maximallySaturatedSets, minimallySaturatedSets, maximumWICCs, minimumWICCs, wiccvString)
import PcInt exposing (Edo, PcInt, edoFromInt, edoToInt, invertPcInt, listFromInput, pcInt, toString, transposePcInt)
import PcSetBasics exposing (PcSet(..), icCount, icVector, normalForm, setToString)
import PitchClass exposing (PitchClass, listFromInput, toInt)
import Transformations exposing (Transformation(..), possibleTransformations, transformationToString)
import PcSetBasics exposing (cardinality)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { userInput : String
    , pitchClasses : List PitchClass
    , pcSet : PcSet
    , edo : Edo
    , weightingConstant : Float
    , icToMinimize : Int
    }


init : Model
init =
    { userInput = ""
    , pitchClasses = []
    , pcSet = PcSet (edoFromInt 12) []
    , edo = edoFromInt 12
    , weightingConstant = 1.2
    , icToMinimize = 1
    }



-- UPDATE


type Msg
    = Typing String
    | UpdateEdo String
    | Calculate
    | Reset
    | UpdateIc String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Typing input ->
            let
                edo12 : Bool
                edo12 =
                    edoToInt model.edo == 12

                pcs : List PitchClass
                pcs =
                    if edo12 then
                        PitchClass.listFromInput input

                    else
                        []

                set : PcSet
                set =
                    if edo12 then
                        pcs
                            |> List.map PitchClass.toInt
                            |> List.map (PcInt.pcInt model.edo)
                            |> PcSet model.edo

                    else
                        PcInt.listFromInput model.edo input
                            |> PcSet model.edo
            in
            { model | userInput = input, pitchClasses = pcs, pcSet = set }

        UpdateEdo input ->
            { model | edo = edoFromInt <| Maybe.withDefault 12 <| String.toInt input }

        Calculate ->
            model

        Reset ->
            { model | userInput = "", pcSet = PcSet model.edo [], pitchClasses = [] }

        UpdateIc ic ->
            { model | icToMinimize = Maybe.withDefault 1 <| String.toInt ic }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewUI model
        , viewIntervalCycles model.weightingConstant model.pcSet
        , viewCycleOrders model
        , viewMinIcOccurences model.edo
        , viewMaxIcOccurences model.edo
        , viewMinWICCs model.edo model.weightingConstant
        , viewMaxWICCs model.edo model.weightingConstant
        , viewSetFacts model
        , viewComplement model.pcSet
        ]


viewUI : Model -> Html Msg
viewUI model =
    div []
        [ h1 [] [ text "Setmaker, v. 6.0" ]
        , p []
            [ text "Modulus: "
            , input [ type_ "number", value (edoToInt model.edo |> String.fromInt), onInput UpdateEdo ] []
            ]
        , p []
            [ text "Pitch classes: "
            , input [ placeholder "type set...", value model.userInput, onInput Typing ] []
            ]
        , p []
            [ text (String.join ", " (List.map PitchClass.toString model.pitchClasses))
            , text (" " ++ setToString model.pcSet)
            ]
        , p []
            [ text "Cardinality: "
            , text (cardinality model.pcSet |> String.fromInt)]
        , button [ onClick Reset ] [ text "reset" ]
        ]


viewSetFacts : Model -> Html Msg
viewSetFacts model =
    div [ id "set-facts" ]
        [ h3 [] [ text "Basic Set Properties" ]
        , p []
            [ text (setToString <| normalForm model.pcSet)
            , text " is "
            , listTransformationsOfPrimeForm model.pcSet
            , text " of "
            , text (printPrimeForm <| PcSetBasics.primeForm model.pcSet)
            ]
        , p []
            [ text "IC vector: "
            , text (printIcVector model)
            ]
        ]


viewIntervalCycles : Float -> PcSet -> Html Msg
viewIntervalCycles weightingConstant set =
    let
        viewAllCycles : Int -> List String
        viewAllCycles modulus =
            List.range 1 (modulus // 2)
                |> List.map (IntervalCycles.genericIntervalCycle modulus)
                |> List.map IntervalCycles.print

        viewCycleFragmentations : PcSet -> List String
        viewCycleFragmentations pcset =
            IntervalCycles.intervalCycleFragmentations pcset
                -- List (List (List (Maybe Int)))
                -- |> List.map (Array.map Array.toList)
                |> List.map (List.map <| List.map <| Maybe.map String.fromInt)
                |> List.map (List.map <| List.map <| Maybe.withDefault "_")
                |> List.map (List.map <| String.join ", ")
                |> List.map (List.map (\s -> "[" ++ s ++ "]"))
                -- |> List.map Array.toList
                |> List.map (String.join " ")
    in
    div [ id "intervac-cycles" ]
        [ h3 [] [ text "Interval Cycles" ]
        , p [] [ text "Interval Cycles:" ]
        , ol []
            (viewAllCycles (PcSetBasics.setModulus set)
                |> List.map (\s -> li [] [ text s ])
            )
        , p [] [ text "Interval Cycle Fragmentation:" ]
        , ol []
            (viewCycleFragmentations set
                |> List.map (\s -> li [] [ text s ])
            )
        , p []
            [ text "ICCV: "
            , text (iccvString set)
            ]
        , p []
            [ text "WICCV: "
            , text (wiccvString weightingConstant set)
            ]
        ]


viewCycleOrders : Model -> Html Msg
viewCycleOrders model =
    let
        m =
            edoToInt model.edo

        maxIc =
            m // 2
    in
    div []
        [ h3 [] [ text "Possible Ordering to Maximize Interval Classes" ]
        , ol []
            (List.range 1 maxIc
                |> List.map (IntervalCycles.orderToMaximizeIC m)
                |> List.map (List.map String.fromInt)
                |> List.map (String.join ", ")
                |> List.map (\s -> li [] [ text s ])
            )
        , h3 [] [ text "Possible Ordering to Minimize Interval Classes (Without Regard for IC Cycles)" ]
        , ol []
            (List.range 1 maxIc
                |> List.map (IntervalCycles.orderToMinimizeIC m)
                |> List.map (List.map String.fromInt)
                |> List.map (String.join ", ")
                |> List.map (\s -> li [] [ text s ])
            )
        , h3 [] [text "Possible Ordering to Minimize Interval Class Cyles for a Given Cardinality"]
        , p []
            [ text "IC to minimize: "
            , input 
                [ type_ "number"
                , Attr.min "1"
                , Attr.max (String.fromInt <| (\i -> i // 2) <| edoToInt model.edo)
                , value (String.fromInt model.icToMinimize)
                , onInput UpdateIc 
                ]
                []
            ]
        , Html.ol []
            (List.range 1 (edoToInt model.edo)
                |> List.map (\i -> IntervalCycles.minCycleSaturationForCardinality model.edo model.icToMinimize i)
                |> List.map PcSetBasics.setToString
                |> List.map (\s -> li [] [text s])
            )
        ]


viewMinIcOccurences : Edo -> Html Msg
viewMinIcOccurences modulus =
    viewMinOrMaxIcOccurences "Minimum Occurences of IC for a given cardinality" IntervalCycles.minimallySaturatedSets modulus


viewMaxIcOccurences : Edo -> Html Msg
viewMaxIcOccurences modulus =
    viewMinOrMaxIcOccurences "Maximum Occurences of IC for a given cardinality" IntervalCycles.maximallySaturatedSets modulus


viewMinOrMaxIcOccurences : String -> (Edo -> Int -> List PcSet) -> Edo -> Html Msg
viewMinOrMaxIcOccurences heading setsGenerator modulus =
    let
        m =
            edoToInt modulus

        maxIc =
            m // 2

        makeRow : Int -> Html Msg
        makeRow ic =
            let
                sets : List PcSet
                sets =
                    setsGenerator modulus ic
            in
            Html.tr []
                (Html.td [] [ text (String.fromInt ic) ]
                    :: (List.map (icCount ic) sets
                            |> List.map String.fromInt
                            |> List.map (\s -> Html.td [] [ text s ])
                       )
                )
    in
    div []
        [ h3 [] [ text heading ]
        , Html.table []
            ((Html.th [] [ text "IC" ]
                :: (List.range 1 m
                        |> List.map String.fromInt
                        |> List.map (\s -> Html.th [] [ text s ])
                   )
             )
                ++ (List.range 1 maxIc
                        |> List.map makeRow
                   )
            )
        ]


viewMinWICCs : Edo -> Float -> Html Msg
viewMinWICCs edo weightingConstant =
    let
        round2 n =
            n * 100 |> round |> toFloat |> (\x -> x / 100)

        makeTR : List Float -> Html Msg
        makeTR vals =
            Html.tr []
                (List.map (\x -> String.fromFloat <| round2 x) vals
                    |> List.map (\s -> Html.td [] [text s])
                )

        makeTRs : Array (Array Float) -> List (Html Msg)
        makeTRs a =
            Array.map (Array.toList) a
                |> Array.map makeTR
                |> Array.toList
    in
    div []
        [ h3 [] [text "minimum WICC values"]
        , Html.table []
            [ Html.thead [] 
                (List.range 0 (edoToInt edo)
                    |> List.map String.fromInt
                    |> List.map (\s -> Html.th [] [text s])
                )
            , Html.tbody []
                ( minimumWICCs edo weightingConstant
                        |> makeTRs
                    )
            ]
        ]

viewMaxWICCs edo weightingConstant =
    let
        round2 n =
            n * 100 |> round |> toFloat |> (\x -> x / 100)

        makeTR : List Float -> Html Msg
        makeTR vals =
            Html.tr []
                (List.map (\x -> String.fromFloat <| round2 x) vals
                    |> List.map (\s -> Html.td [] [text s])
                )

        makeTRs : Array (Array Float) -> List (Html Msg)
        makeTRs a =
            Array.map (Array.toList) a
                |> Array.map makeTR
                |> Array.toList
    in
    div []
        [ h3 [] [text "maximum WICC values"]
        , Html.table []
            [ Html.thead [] []
            , Html.tbody []
                ( maximumWICCs edo weightingConstant
                        |> makeTRs
                    )
            ]
        ]


viewComplement : PcSet -> Html Msg
viewComplement pcSet =
    let
        c =
            PcSetBasics.complement pcSet

        cpf =
            PcSetBasics.primeForm c

    in
    div [ id "complement" ]
        [ h3 [] [ text "Related Sets / Set Classes" ]
        , p []
            [ text "Complement: "
            , text (setToString <| c)
            , text " = "
            , span []
                (Transformations.possibleTransformations c cpf
                    |> List.map printTransformation
                    |> List.intersperse (span [] [text " / "])
                )
            , text " of "
            , text (PcSetBasics.setToString cpf)
            ]
        ]


printIcVector : Model -> String
printIcVector model =
    "<"
        ++ (icVector model.pcSet
                |> Array.toList
                |> List.map String.fromInt
                |> String.join ", "
           )
        ++ ">"


printPrimeForm : PcSet -> String
printPrimeForm (PcSet _ pcs) =
    "(" ++ String.join " " (List.map PcInt.toString pcs) ++ ")"


listTransformationsOfPrimeForm : PcSet -> Html Msg
listTransformationsOfPrimeForm set =
    span []
        (possibleTransformations (PcSetBasics.primeForm set) set
            |> List.map printTransformation
            |> List.intersperse (span [] [text " / "])
        )


printTransformation : Transformation -> Html Msg
printTransformation t =
    case t of
        Transposition i ->
            span [] [ text " T", Html.sub [] [ text (String.fromInt i) ] ]

        Inversion i ->
            span [] [ text " I", Html.sub [] [ text (String.fromInt i) ] ]
