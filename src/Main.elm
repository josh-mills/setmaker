module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h2, h3, h4, input, li, ol, p, span, text, ul)
import Html.Attributes exposing (id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import IntervalCycles exposing (iccvString, maximallySaturatedSets, minimallySaturatedSets, wiccvString)
import PcInt exposing (Edo, PcInt, edoFromInt, edoToInt, invertPcInt, listFromInput, pcInt, toString, transposePcInt)
import PcSetBasics exposing (PcSet(..), icCount, icVector, normalForm, setToString)
import PitchClass exposing (PitchClass, listFromInput, toInt)
import Transformations exposing (Transformation(..), possibleTransformations, transformationToString)



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
    }


init : Model
init =
    { userInput = ""
    , pitchClasses = []
    , pcSet = PcSet (edoFromInt 12) []
    , edo = edoFromInt 12
    , weightingConstant = 1.2
    }



-- UPDATE


type Msg
    = Typing String
    | UpdateEdo String
    | Calculate
    | Reset


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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewUI model
        , viewCycleOrders model.edo
        , viewMinIcOccurences model.edo
        , viewMaxIcOccurences model.edo
        , viewSetFacts model
        , viewIntervalCycles model.weightingConstant model.pcSet
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


viewCycleOrders : Edo -> Html Msg
viewCycleOrders modulus =
    let
        m =
            edoToInt modulus

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
        , h3 [] [ text "Possible Ordering to Minimize Interval Classes" ]
        , ol []
            (List.range 1 maxIc
                |> List.map (IntervalCycles.orderToMinimizeIC m)
                |> List.map (List.map String.fromInt)
                |> List.map (String.join ", ")
                |> List.map (\s -> li [] [ text s ])
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


viewComplement : PcSet -> Html Msg
viewComplement pcSet =
    let
        c =
            PcSetBasics.complement pcSet

        cpf =
            PcSetBasics.primeForm c

        transformations =
            Transformations.possibleTransformations c cpf
    in
    div [ id "complement" ]
        [ h3 [] [ text "Related Sets / Set Classes" ]
        , p []
            [ text "Complement: "
            , text (setToString <| c)
            , text " = "
            , text
                (Transformations.possibleTransformations c cpf
                    |> List.map Transformations.transformationToString
                    |> String.join ", "
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
        )


printTransformation : Transformation -> Html Msg
printTransformation t =
    case t of
        Transposition i ->
            span [] [ text " T", Html.sub [] [ text (String.fromInt i) ] ]

        Inversion i ->
            span [] [ text " I", Html.sub [] [ text (String.fromInt i) ] ]
