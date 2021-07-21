module Main exposing (main)

import Arithmetic
import Array exposing (Array)
import Browser
import ForteNumbers
import Helpers
import Html exposing (Html, button, div, h1, h2, h3, h4, input, li, ol, p, span, text, ul)
import Html.Attributes as Attr exposing (id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import IntervalCycles exposing (CyclicProportionSaturation, iccvString, maximallySaturatedSets, maximumWICCs, minimallySaturatedSets, minimumWICCs, wiccvString)
import PcInt exposing (Edo, PcInt, edoFromInt, edoToInt, invertPcInt, listFromInput, pcInt, toString, transposePcInt)
import PcSetBasics exposing (PcSet(..), cardinality, icCount, icVector, normalForm, primeForm, setToString)
import PitchClass exposing (PitchClass, listFromInput, toInt)
import Regex exposing (Match, Regex)
import Round
import Set
import Transformations exposing (Transformation(..), possibleTransformations, transformationToString)
import IntervalCycles exposing (weight)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { userInput : String
    , pitchClasses : List PitchClass
    , pcSet : PcSet
    , edo : Edo
    , weightingConstant : Float
    , icToOptimize : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { userInput = ""
      , pitchClasses = []
      , pcSet = PcSet (edoFromInt 12) []
      , edo = edoFromInt 12
      , weightingConstant = 1.3
      , icToOptimize = 1
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Typing String
    | UpdateEdo String
    | Calculate
    | Reset
    | UpdateIc String
    | ClickSetLink String
    | UpdateWeightingConstant String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickSetLink set ->
            update (Typing set) { model | userInput = set }

        Typing input ->
            let
                edo12 : Bool
                edo12 =
                    edoToInt model.edo == 12

                forteNumberRegex : Regex
                forteNumberRegex =
                    Maybe.withDefault Regex.never <|
                        Regex.fromString "^(\\d{1,2})[-–][zZ]?(\\d{1,2})[zZ]?$"

                isForteNumber : Bool
                isForteNumber =
                    edo12 && Regex.contains forteNumberRegex input

                cleanup : String -> String
                cleanup s =
                    let
                        charsRegex =
                            Maybe.withDefault Regex.never <|
                                Regex.fromString "[{}()\\[\\]]"
                    in
                    Regex.replace charsRegex (\_ -> "") s

                pcs : List PitchClass
                pcs =
                    if edo12 then
                        PitchClass.listFromInput <| cleanup input

                    else
                        []

                set : PcSet
                set =
                    if isForteNumber then
                        let
                            matches : List Int
                            matches =
                                Regex.find forteNumberRegex input
                                    |> List.head
                                    |> Maybe.map .submatches
                                    |> Maybe.map (List.filterMap identity)
                                    |> Maybe.map (List.filterMap String.toInt)
                                    |> Maybe.map (List.take 2)
                                    |> Maybe.withDefault []

                            card : Int
                            card =
                                List.head matches
                                    |> Maybe.withDefault 0

                            catNum : Int
                            catNum =
                                List.drop 1 matches
                                    |> List.head
                                    |> Maybe.withDefault 0
                        in
                        Maybe.withDefault (PcSet model.edo []) <|
                            ForteNumbers.primeFormForForteNumber card catNum

                    else if edo12 then
                        pcs
                            |> List.map PitchClass.toInt
                            |> List.map (PcInt.pcInt model.edo)
                            |> PcSet model.edo

                    else
                        PcInt.listFromInput model.edo (cleanup input)
                            |> PcSet model.edo
            in
            ( { model | userInput = input, pitchClasses = pcs, pcSet = set }
            , Cmd.none
            )

        UpdateEdo input ->
            let
                newEdo : Edo
                newEdo =
                    edoFromInt <| Maybe.withDefault 12 <| String.toInt input

                s : List PcInt
                s =
                    (\(PcSet _ pcs) -> pcs) model.pcSet
            in
            ( { model | edo = newEdo, pcSet = PcSet newEdo s }
            , Cmd.none
            )

        Calculate ->
            ( model
            , Cmd.none
            )

        Reset ->
            ( { model | userInput = "", pcSet = PcSet model.edo [], pitchClasses = [] }
            , Cmd.none
            )

        UpdateIc ic ->
            ( { model | icToOptimize = Maybe.withDefault 1 <| String.toInt ic }
            , Cmd.none
            )

        UpdateWeightingConstant k ->
            ( { model | weightingConstant = Maybe.withDefault 1.2 <| String.toFloat k} 
            , Cmd.none
            )



subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewUI model
        , viewSetFacts model
        , viewRelatedSets model.pcSet
        , viewIntervalCycles model.weightingConstant model.pcSet
        , viewCycleOrders model

        -- , viewMinIcOccurences model.edo
        -- , viewMaxIcOccurences model.edo
        -- , viewMinWICCs model.edo model.weightingConstant
        -- , viewMaxWICCs model.edo model.weightingConstant
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
            [ if edoToInt model.edo == 12 then
                text "Pitch classes or Forte number: "

              else
                text "Pitch classes: "
            , input [ placeholder "type set...", value model.userInput, onInput Typing ] []
            ]
        , p []
            [ text (String.join ", " (List.map PitchClass.toString model.pitchClasses))
            , text (" " ++ setToString model.pcSet)
            ]
        , button [ onClick Reset ] [ text "reset" ]
        , viewWeightingOptions model
        ]


viewWeightingOptions : Model -> Html Msg
viewWeightingOptions model =
    let
        w : Float -> Int -> String
        w c n =
            (if c == 1 then
                ""
            else
                (String.fromFloat c)
            )
            ++ "w(" ++ (String.fromInt n) ++ ") = " 
            ++ (Round.round 2 <| (*) c <| weight model.weightingConstant n)

        e : Int
        e = edoToInt model.edo

        weight1 = (weight model.weightingConstant (e // 2)) + 2 * (weight model.weightingConstant (e // 4 - 2))

        weight2 = 2 * (weight model.weightingConstant (e // 2 - 2)) 

        gtlt = 
            if weight1 >= weight2 then
                " >= "
            else 
                " < "

    in
    div [ id "weighting-options"] 
        [ p [] 
            [ text "Weighting Constant: "
            , input [ type_ "text", value (model.weightingConstant |> String.fromFloat), onInput UpdateWeightingConstant ] []
            ]
        , p [] 
            [ text <| (w 1 <| e // 2) ++ "; "
            , text <| (w 2 <| e // 4 - 2) ++ "; "
            , text <| (w 2 <| e // 2 - 2) ++ "; "
            , Html.br [] []
            , text <| "w(" ++ (String.fromInt <| e // 2) ++ ") + 2w(" ++ (String.fromInt <| e // 4 - 2) ++ ")"
            , text gtlt
            , text <| "2w(" ++ (String.fromInt <| e//2 - 2) ++ ") "
            , text <| "(" ++ (Round.round 2 weight1) ++ gtlt ++ (Round.round 2 weight2) ++ ")"
            , Html.br [] []
            , text <| String.fromFloat model.weightingConstant
            , if IntervalCycles.validWeightingConstant model.weightingConstant (edoToInt model.edo) then
                    text " is a valid weighting constant for n = "
                else 
                    text " is NOT a valid weighting constant for n = "
            , text <| String.fromInt <| edoToInt model.edo
            , text "."
            ]
        ]

viewSetFacts : Model -> Html Msg
viewSetFacts model =
    let
        forteNum : Maybe String
        forteNum =
            ForteNumbers.forteNum model.pcSet
    in
    div [ id "set-facts" ]
        [ h3 [] [ text "Basic Set Properties" ]
        , p []
            [ text (setToString <| normalForm model.pcSet)
            , text " is "
            , listTransformationsOfPrimeForm model.pcSet
            , text " of "
            , text (printPrimeForm <| PcSetBasics.primeForm model.pcSet)
            , text
                (case forteNum of
                    Just s ->
                        ". Forte number: " ++ s

                    Nothing ->
                        ""
                )
            ]
        , p []
            [ text "Set Cardinality: "
            , text (cardinality model.pcSet |> String.fromInt)
            ]
        , p []
            [ text "Interval Class vector: "
            , text (printIcVector model)
            ]
        ]


viewIntervalCycles : Float -> PcSet -> Html Msg
viewIntervalCycles weightingConstant set =
    let
        viewCycleFragmentations : PcSet -> List String
        viewCycleFragmentations pcset =
            IntervalCycles.intervalCycleFragmentations pcset
                |> List.map (List.map <| List.map <| Maybe.map String.fromInt)
                |> List.map (List.map <| List.map <| Maybe.withDefault "_")
                |> List.map (List.map <| String.join ", ")
                |> List.map (List.map (\s -> "[" ++ s ++ "]"))
                |> List.map (String.join " ")
    in
    div [ id "interval-cycles" ]
        [ h3 [] [ text "Interval Cycles" ]
        , p []
            [ text "Interval Cycle Fragmentation of "
            , text (setToString set)
            ]
        , ol []
            (viewCycleFragmentations set
                |> List.map (\s -> li [] [ text s ])
            )
        , p []
            [ text "IC Cycle Vector: "
            , text (iccvString set)
            ]
        , p []
            [ text "Weighted IC Cycle Vector: "
            , text (wiccvString weightingConstant set)
            ]
        , viewCPSATV weightingConstant set

        -- , viewCycleSaturationDetails weightingConstant set
        ]


viewCPSATV : Float -> PcSet -> Html Msg
viewCPSATV weightingConstant set =
    let
        cpsList : List CyclicProportionSaturation
        cpsList =
            IntervalCycles.makeCPSATV weightingConstant set

        cpsatv : String
        cpsatv =
            cpsList
                |> List.map (\cps -> Helpers.scale cps.minimum cps.maximum cps.value)
                |> List.map (Round.round 2)
                |> String.join ", "
                |> (\s -> "Cyclic Proportion Saturation Vector: <" ++ s ++ ">")

        csatvA : String
        csatvA =
            cpsList
                |> List.map
                    (\cps ->
                        if Helpers.scale cps.minimum cps.maximum cps.value >= 0.5 then
                            "max-" ++ (Round.round 2 <| cps.maximum - cps.value)

                        else
                            "min+" ++ (Round.round 2 <| cps.value - cps.minimum)
                    )
                |> String.join ", "
                |> (\s -> "<" ++ s ++ ">")

        csatvB : String
        csatvB =
            cpsList
                |> List.map
                    (\cps ->
                        if Helpers.scale cps.minimum cps.maximum cps.value < 0.5 then
                            "max-" ++ (Round.round 2 <| cps.maximum - cps.value)

                        else
                            "min+" ++ (Round.round 2 <| cps.value - cps.minimum)
                    )
                |> String.join ", "
                |> (\s -> "<" ++ s ++ ">")
    in
    div []
        [ p [] [ text cpsatv ]
        , p []
            [ span []
                [ text "CycSatVec"
                , Html.sub [] [ text "A" ]
                , text ": "
                ]
            , text csatvA
            ]
        , p []
            [ span []
                [ text "CycSatVec"
                , Html.sub [] [ text "B" ]
                , text ": "
                ]
            , text csatvB
            ]
        ]


viewGenericIntervalCycles : Int -> Html Msg
viewGenericIntervalCycles modulus =
    div []
        [ p [] [ text "Generic Interval Cycles:" ]
        , ol []
            (List.range 1 (modulus // 2)
                |> List.map (IntervalCycles.genericIntervalCycle modulus)
                |> List.map IntervalCycles.print
                |> List.map (\s -> li [] [ text s ])
            )
        ]



{- I don't think we need this -}


viewCycleSaturationDetails : Float -> PcSet -> Html Msg
viewCycleSaturationDetails weightingConstant set =
    let
        round2 n =
            n * 100 |> round |> toFloat |> (\x -> x / 100)

        makeLi : CyclicProportionSaturation -> Html Msg
        makeLi cps =
            li []
                [ text "min: "
                , text (String.fromFloat <| round2 cps.minimum)
                , text "; max: "
                , text (String.fromFloat <| round2 cps.maximum)
                , text "; val:"
                , text (String.fromFloat <| round2 cps.value)
                ]
    in
    div [ id "cycle-saturation" ]
        [ ol []
            (IntervalCycles.makeCPSATV weightingConstant set
                |> List.map makeLi
            )
        ]


viewCycleOrders : Model -> Html Msg
viewCycleOrders model =
    let
        prettifySet : PcSet -> PcSet
        prettifySet (PcSet edo pcs) =
            List.sortBy PcInt.pcIntToInt pcs
                |> PcSetBasics.transposeToZero edo
                |> PcSet edo

        maxOrdering : List PcInt
        maxOrdering =
            IntervalCycles.orderToMaximizeIC (edoToInt model.edo) model.icToOptimize
                |> List.map (pcInt model.edo)

        ic =
            String.fromInt model.icToOptimize
    in
    div []
        [ h3 [] [ text "Possible Sets to Minimize or Maximize Unbroken Interval Class Cycles" ]
        , viewGenericIntervalCycles <| edoToInt model.edo
        , p []
            [ text "IC to minimize/maximize: "
            , input
                [ type_ "number"
                , Attr.min "1"
                , Attr.max (String.fromInt <| (\i -> i // 2) <| edoToInt model.edo)
                , value ic
                , onInput UpdateIc
                ]
                []
            ]
        , h4 [] [ text ("Possible Sets to Minimize IC-" ++ ic ++ " Cyles for a Given Cardinality") ]
        , Html.ol []
            (List.range 1 (edoToInt model.edo)
                |> List.map (\i -> IntervalCycles.minCycleSaturationForCardinality model.edo model.icToOptimize i)
                |> List.map prettifySet
                |> List.map PcSetBasics.setToString
                |> List.map
                    (\s ->
                        li []
                            [ Html.a [ Attr.href "#", onClick (ClickSetLink s) ]
                                [ text s ]
                            ]
                    )
            )
        , h4 [] [ text ("Possible Sets to Maximize IC-" ++ ic ++ " Cyles for a Given Cardinality") ]
        , Html.ol []
            (List.range 1 (edoToInt model.edo)
                |> List.map (\i -> List.take i maxOrdering)
                |> List.map (PcSet model.edo)
                |> List.map prettifySet
                |> List.map PcSetBasics.setToString
                |> List.map
                    (\s ->
                        li []
                            [ Html.a [ Attr.href "#", onClick (ClickSetLink s) ]
                                [ text s ]
                            ]
                    )
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
                    |> List.map (\s -> Html.td [] [ text s ])
                )

        makeTRs : Array (Array Float) -> List (Html Msg)
        makeTRs a =
            Array.map Array.toList a
                |> Array.map makeTR
                |> Array.toList
    in
    div []
        [ h3 [] [ text "minimum WICC values" ]
        , Html.table []
            [ Html.thead []
                (List.range 0 (edoToInt edo)
                    |> List.map String.fromInt
                    |> List.map (\s -> Html.th [] [ text s ])
                )
            , Html.tbody []
                (minimumWICCs edo weightingConstant
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
                    |> List.map (\s -> Html.td [] [ text s ])
                )

        makeTRs : Array (Array Float) -> List (Html Msg)
        makeTRs a =
            Array.map Array.toList a
                |> Array.map makeTR
                |> Array.toList
    in
    div []
        [ h3 [] [ text "maximum WICC values" ]
        , Html.table []
            [ Html.thead [] []
            , Html.tbody []
                (maximumWICCs edo weightingConstant
                    |> makeTRs
                )
            ]
        ]


viewRelatedSets : PcSet -> Html Msg
viewRelatedSets pcSet =
    div []
        [ h3 [] [ text "Related Sets" ]
        , viewComplement pcSet
        , viewZMate pcSet
        , viewMSets pcSet
        ]


viewComplement : PcSet -> Html Msg
viewComplement pcSet =
    let
        complement =
            PcSetBasics.complement pcSet

        complementString =
            setToString complement

        cpf =
            PcSetBasics.primeForm complement
    in
    div [ id "complement" ]
        [ p []
            [ text "Complement: "
            , Html.a [ Attr.href "#", onClick (ClickSetLink complementString) ]
                [ text complementString ]
            , text " = "
            , span []
                (Transformations.possibleTransformations complement cpf
                    |> List.map printTransformation
                    |> List.intersperse (span [] [ text " / " ])
                )
            , text " of "
            , text (printPrimeForm cpf)
            ]
        ]


viewZMate : PcSet -> Html Msg
viewZMate pcSet =
    let
        zMate =
            ForteNumbers.zRelatedNum pcSet
    in
    case zMate of
        Just z ->
            div [ id "z-related-mate" ]
                [ p []
                    [ text "Z-related pair: "
                    , Html.a [ Attr.href "#", onClick (ClickSetLink z) ] [ text z ]
                    ]
                ]

        Nothing ->
            div [ id "z-related-mate" ] []


viewMSets : PcSet -> Html Msg
viewMSets pcSet =
    let
        n = 
            PcSetBasics.setModulus pcSet

        coprimes = 
            List.range 2 (n-1)
            |> List.filter (Arithmetic.isCoprimeTo n)

        makeLi : Int -> Html Msg
        makeLi i =
            let
                mSet = PcSetBasics.multiplySet i pcSet

                set = PcSetBasics.setToString mSet
                
                mSetPF = PcSetBasics.primeForm mSet
            in
            li []
                [ span [] [text "M", Html.sub [] [text (String.fromInt i)], text ": "]
                , Html.a [ Attr.href "#", onClick (ClickSetLink set)] [text set]
                , text " = "
                , span []
                    (Transformations.possibleTransformations mSet mSetPF
                        |> List.map printTransformation
                        |> List.intersperse (span [] [ text " / " ])
                    )
                , text " of "
                , text (printPrimeForm mSetPF)
                ]

    in
    div [ id "M-related-sets" ]
        [ p [] [ text "M-related sets:"]
        , ul []
            (List.map makeLi coprimes)
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
            |> List.intersperse (span [] [ text " / " ])
        )


printTransformation : Transformation -> Html Msg
printTransformation t =
    case t of
        Transposition i ->
            span [] [ text " T", Html.sub [] [ text (String.fromInt i) ] ]

        Inversion i ->
            span [] [ text " I", Html.sub [] [ text (String.fromInt i) ] ]
