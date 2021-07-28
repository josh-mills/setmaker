module Main exposing (main)

import Arithmetic
import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import ForteNumbers
import Helpers
import Html exposing (Html)
import Html.Attributes as Attr exposing (id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import IntervalCycles exposing (CyclicProportionSaturation, iccvString, maximallySaturatedSets, maximumWICCs, minimallySaturatedSets, minimumWICCs, weight, weightingConstantForEdo, wiccvString)
import PcInt exposing (Edo, PcInt, edoFromInt, edoToInt, invertPcInt, listFromInput, pcInt, toString, transposePcInt)
import PcSetBasics exposing (PcSet(..), cardinality, icCount, icVector, normalForm, primeForm, setToString)
import PitchClass exposing (PitchClass, listFromInput, toInt)
import Regex exposing (Match, Regex)
import Round
import Set
import Transformations exposing (Transformation(..), possibleTransformations, transformationToString)
import Html exposing (a)
import String exposing (trim)



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
    { userInputA : String
    , userInputB : String
    , pitchClassesA : List PitchClass
    , pitchClassesB : List PitchClass
    , pcSetA : PcSet
    , pcSetB : PcSet
    , edo : Edo
    , weightingConstant : Float
    , icToOptimize : Int
    , option : Option
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        edo =
            edoFromInt 12
    in
    ( { userInputA = ""
      , userInputB = ""
      , pitchClassesA = []
      , pitchClassesB = []
      , pcSetA = PcSet edo []
      , pcSetB = PcSet edo []
      , edo = edo
      , weightingConstant = 1.2
      , icToOptimize = 1
      , option = SingleSet
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TypingA String
    | TypingB String
    | UpdateEdo Int
    | Reset
    | UpdateIc String
    | ClickSetLink String
    | UpdateWeightingConstant Float
    | ClickOption Option


type Option
    = About
    | CombineSets
    | Settings
    | SingleSet
    | TransformationalPossibilities


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickOption option ->
            ( { model | option = option}
            , Cmd.none )

        ClickSetLink set ->
            update (TypingA set) { model | userInputA = set }

        TypingA input ->
            let
                (pcs, set) = parseInput model.edo input
            in
            ( { model | userInputA = input, pitchClassesA = pcs, pcSetA = set }
            , Cmd.none
            )

        TypingB input ->
            let
                (pcs, set) = parseInput model.edo input
            in
            ( { model | userInputB = input, pitchClassesB = pcs, pcSetB = set }
            , Cmd.none
            )

        UpdateEdo input ->
            let
                newEdo : Edo
                newEdo =
                    edoFromInt input
            in
            ( { model | edo = newEdo, weightingConstant = weightingConstantForEdo newEdo } |> clearInputs
            , Cmd.none
            )

        Reset ->
            ( clearInputs model
            , Cmd.none
            )

        UpdateIc ic ->
            ( { model | icToOptimize = Maybe.withDefault 1 <| String.toInt ic }
            , Cmd.none
            )

        UpdateWeightingConstant k ->
            ( { model | weightingConstant = k }
            , Cmd.none
            )


clearInputs : Model -> Model
clearInputs model =
    { model | userInputA = "", userInputB = "", pitchClassesA = [], pitchClassesB = [], pcSetA = PcSet model.edo [], pcSetB = PcSet model.edo []}


cleanup : String -> String
cleanup s =
    let
        charsRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "[{}()\\[\\]]"
    in
    Regex.replace charsRegex (\_ -> "") s |> trim


parseInput : Edo -> String -> (List PitchClass, PcSet)
parseInput edo input =
    let
        edo12 : Bool
        edo12 =
            edoToInt edo == 12

        forteNumberRegex : Regex
        forteNumberRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^(\\d{1,2})[-–][zZ]?(\\d{1,2})[zZ]?$"

        isForteNumber : Bool
        isForteNumber =
            edo12 && Regex.contains forteNumberRegex input

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
                Maybe.withDefault (PcSet edo []) <|
                    ForteNumbers.primeFormForForteNumber card catNum

            else if edo12 then
                pcs
                    |> List.map PitchClass.toInt
                    |> List.map (PcInt.pcInt edo)
                    |> PcSet edo

            else
                PcInt.listFromInput edo (cleanup input)
                    |> PcSet edo
    in
    (pcs, set)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW



view : Model -> Html Msg
view model =
    layout
        [ Font.size 18
        , Font.family
            [ Font.typeface "Courier"
            , Font.monospace
            ]
        -- , explain Debug.todo
        ]
        ( column [ width fill, paddingXY 8 8 ]
            [ topBar
            , mainContent model
            ] )


topBar : Element Msg
topBar =
    wrappedRow [ Region.navigation, width fill, Font.size 36, spacing 12, paddingXY 8 8 ]
        [ text "Setmaker!"
        , menuButton "Set Properties" SingleSet 
        , menuButton "Transformations" TransformationalPossibilities
        , menuButton "Combine Sets" CombineSets
        , menuButton "Settings" Settings
        , menuButton "About" About
        ]


menuButton : String -> Option -> Element Msg
menuButton txt option =
    el 
        [ paddingXY 14 8
        , Font.size 18
        , pointer
        , Border.solid
        , Border.color <| rgb 0.7 0.7 0.7
        , Border.width 2
        , Border.rounded 8
        , Events.onClick (ClickOption option)
        ] 
        (text txt)


sectionHeading : String -> Element Msg
sectionHeading txt =
    el 
        [ Region.heading 1
        , Font.size 28
        , padding 6
        ]
        ( text txt )


secondHeading : String -> Element Msg
secondHeading txt =
    el 
        [ Region.heading 2
        , Font.size 24
        , paddingEach { top = 12, left = 0, right = 0, bottom = 8}
        ]
        ( text txt )


mainContent : Model -> Element Msg
mainContent model =
    el [ Region.mainContent, paddingXY 20 20 ]
        (case model.option of
            Settings ->
                viewSettings model

            SingleSet ->
                viewSetFacts model

            About ->
                viewAbout

            _ ->
                sectionHeading "not implemented. :("
        )


viewSettings : Model -> Element Msg
viewSettings model =
    column [ width <| px 500, Font.size 18 ]
        [ sectionHeading "Settings"
        , Input.slider
            sliderBackground
            { onChange = \x -> UpdateEdo (truncate x)
            , label = Input.labelLeft [] (text <| "modulus (" ++ (String.fromInt <| edoToInt model.edo) ++ "): ")
            , min = 1
            , max = 48
            , value = toFloat <| edoToInt model.edo
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        , Input.slider
            sliderBackground
            { onChange = UpdateWeightingConstant
            , label = Input.labelLeft [] (text <| "weighting constant (" ++ Round.round 2 model.weightingConstant ++ "): ")
            , min = 1
            , max = 2
            , value = model.weightingConstant
            , thumb = Input.defaultThumb
            , step = Just 0.001
            }
        ]

sliderBackground : List (Attribute Msg)
sliderBackground =
    [ Element.height (Element.px 20)
    , Element.behindContent
        (Element.el
            [ Element.width Element.fill
            , Element.height (Element.px 2)
            , Element.centerY
            , Background.color <| rgba 0.5 0.5 0.5 0.5
            , Border.rounded 2
            ]
            Element.none
        )
    ]


viewSetFacts : Model -> Element Msg
viewSetFacts model =
    column [ spacing 8 ]
        [ sectionHeading "View the Properties of a Single Set"
        , row [spacing 18]
            [ Input.text [width <| px 300, paddingXY 12 6, spacing 12]
            { onChange = TypingA
            , text = model.userInputA
            , placeholder = Just <| Input.placeholder [] 
                (if edoToInt model.edo == 12 then
                    text "input set or Forte number"
                else
                    text "input set")
            , label = Input.labelLeft [] (text "input set:")
            }
            , Input.button 
                [ Background.color <| rgba 0.25 0.25 0.25 0.3
                , Border.solid
                , Border.rounded 5
                , paddingXY 9 4
                ] 
                { onPress = Just Reset
                , label = text "Clear"
                }
            ]
        , paragraph [] 
            [ text (String.join ", " (List.map PitchClass.toString model.pitchClassesA))
            , text (" " ++ setToString model.pcSetA)
            
            ]
        , secondHeading "Basic Set Properties"
        , paragraph [] 
            [ text (setToString <| normalForm model.pcSetA)
            , text " is "
            , listTransformationsOfPrimeForm model.pcSetA
            , text " of "
            , text (printPrimeForm <| PcSetBasics.primeForm model.pcSetA)
            ]
        , case ForteNumbers.forteNum model.pcSetA of
            Just s ->
                paragraph [] [text <| "Forte number: " ++ s]
            Nothing ->
                none
        , paragraph []
            [ text "Set Cardinality: "
            , text (cardinality model.pcSetA |> String.fromInt)
            ]
        , paragraph []
            [ text "Interval Class vector: "
            , text (printIcVector model.pcSetA)
            ]
        , secondHeading "Related Sets"
        , viewComplement model.pcSetA
        , viewZMate model.pcSetA
        , viewMSets model.pcSetA
        , viewIntervalCycles model.weightingConstant model.pcSetA
        ]


viewAbout : Element Msg
viewAbout =
    textColumn [] 
        [ sectionHeading "About Setmaker"
        , paragraph [] [ text """Michael Buchler's Setmaker is nifty. Probably not terribly useful, but a fun little toy all the same."""]
        ]
    





viewIntervalCycles : Float -> PcSet -> Element Msg
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
    column []
        [ secondHeading "Interval Cycles" 
        , paragraph []
            [ text "Interval Cycle Fragmentation of "
            , text (setToString set)
            ]
        , html <| Html.ol []
            (viewCycleFragmentations set
                |> List.map (\s -> Html.li [] [ Html.text s ])
            )
        , paragraph []
            [ text "IC Cycle Vector: "
            , text (iccvString set)
            ]
        , paragraph []
            [ text "Weighted IC Cycle Vector: "
            , text (wiccvString weightingConstant set)
            ]
        , html <| viewCPSATV weightingConstant set

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
    Html.div []
        [ Html.p [] [ Html.text cpsatv ]
        , Html.p []
            [ Html.span []
                [ Html.text "CycSatVec"
                , Html.sub [] [ Html.text "A" ]
                , Html.text ": "
                ]
            , Html.text csatvA
            ]
        , Html.p []
            [ Html.span []
                [ Html.text "CycSatVec"
                , Html.sub [] [ Html.text "B" ]
                , Html.text ": "
                ]
            , Html.text csatvB
            ]
        ]

{- I don't think we need this -}

{-

viewGenericIntervalCycles : Int -> Html Msg
viewGenericIntervalCycles modulus =
    Html.div []
        [ Html.p [] [ Html.text "Generic Interval Cycles:" ]
        , Html.ol []
            (List.range 1 (modulus // 2)
                |> List.map (IntervalCycles.genericIntervalCycle modulus)
                |> List.map IntervalCycles.print
                |> List.map (\s -> Html.li [] [ Html.text s ])
            )
        ]



viewCycleSaturationDetails : Float -> PcSet -> Html Msg
viewCycleSaturationDetails weightingConstant set =
    let
        round2 n =
            n * 100 |> round |> toFloat |> (\x -> x / 100)

        makeLi : CyclicProportionSaturation -> Html Msg
        makeLi cps =
            Html.li []
                [ Html.text "min: "
                , Html.text (String.fromFloat <| round2 cps.minimum)
                , Html.text "; max: "
                , Html.text (String.fromFloat <| round2 cps.maximum)
                , Html.text "; val:"
                , Html.text (String.fromFloat <| round2 cps.value)
                ]
    in
    Html.div [ id "cycle-saturation" ]
        [ Html.ol []
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
    Html.div []
        [ Html.h3 [] [ Html.text "Possible Sets to Minimize or Maximize Unbroken Interval Class Cycles" ]
        , viewGenericIntervalCycles <| edoToInt model.edo
        , Html.p []
            [ Html.text "IC to minimize/maximize: "
            , Html.input
                [ type_ "number"
                , Attr.min "1"
                , Attr.max (String.fromInt <| (\i -> i // 2) <| edoToInt model.edo)
                , value ic
                , onInput UpdateIc
                ]
                []
            ]
        , Html.h4 [] [ Html.text ("Possible Sets to Minimize IC-" ++ ic ++ " Cyles for a Given Cardinality") ]
        , Html.ol []
            (List.range 1 (edoToInt model.edo)
                |> List.map (\i -> IntervalCycles.minCycleSaturationForCardinality model.edo model.icToOptimize i)
                |> List.map prettifySet
                |> List.map PcSetBasics.setToString
                |> List.map
                    (\s ->
                        Html.li []
                            [ Html.a [ Attr.href "#", onClick (ClickSetLink s) ]
                                [ Html.text s ]
                            ]
                    )
            )
        , Html.h4 [] [ Html.text ("Possible Sets to Maximize IC-" ++ ic ++ " Cyles for a Given Cardinality") ]
        , Html.ol []
            (List.range 1 (edoToInt model.edo)
                |> List.map (\i -> List.take i maxOrdering)
                |> List.map (PcSet model.edo)
                |> List.map prettifySet
                |> List.map PcSetBasics.setToString
                |> List.map
                    (\s ->
                        Html.li []
                            [ Html.a [ Attr.href "#", onClick (ClickSetLink s) ]
                                [ Html.text s ]
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
                (Html.td [] [ Html.text (String.fromInt ic) ]
                    :: (List.map (icCount ic) sets
                            |> List.map String.fromInt
                            |> List.map (\s -> Html.td [] [ Html.text s ])
                       )
                )
    in
    Html.div []
        [ Html.h3 [] [ Html.text heading ]
        , Html.table []
            ((Html.th [] [ Html.text "IC" ]
                :: (List.range 1 m
                        |> List.map String.fromInt
                        |> List.map (\s -> Html.th [] [ Html.text s ])
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
                    |> List.map (\s -> Html.td [] [ Html.text s ])
                )

        makeTRs : Array (Array Float) -> List (Html Msg)
        makeTRs a =
            Array.map Array.toList a
                |> Array.map makeTR
                |> Array.toList
    in
    Html.div []
        [ Html.h3 [] [ Html.text "minimum WICC values" ]
        , Html.table []
            [ Html.thead []
                (List.range 0 (edoToInt edo)
                    |> List.map String.fromInt
                    |> List.map (\s -> Html.th [] [ Html.text s ])
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
                    |> List.map (\s -> Html.td [] [ Html.text s ])
                )

        makeTRs : Array (Array Float) -> List (Html Msg)
        makeTRs a =
            Array.map Array.toList a
                |> Array.map makeTR
                |> Array.toList
    in
    Html.div []
        [ Html.h3 [] [ Html.text "maximum WICC values" ]
        , Html.table []
            [ Html.thead [] []
            , Html.tbody []
                (maximumWICCs edo weightingConstant
                    |> makeTRs
                )
            ]
        ]
-}


viewComplement : PcSet -> Element Msg
viewComplement pcSet =
    let
        complement =
            PcSetBasics.complement pcSet

        complementString =
            setToString complement

        cpf =
            PcSetBasics.primeForm complement
    in
    paragraph []
        [ text "Complement: "
        , html <| Html.a [ Attr.href "#", onClick (ClickSetLink <| cleanup complementString) ]
                [ Html.text complementString ]
        , text " = "
        , row []
                (Transformations.possibleTransformations complement cpf
                    |> List.map printTransformation
                    |> List.intersperse (el [] (text " / "))
                )
        , text " of "
        , text <| printPrimeForm cpf
        ]


viewZMate : PcSet -> Element Msg
viewZMate pcSet =
    let
        zMate =
            ForteNumbers.zRelatedNum pcSet
    in
    case zMate of
        Just z ->
            paragraph []
                [ text "Z-related pair: "
                , html <| Html.a [ Attr.href "#", onClick (ClickSetLink z) ] [ Html.text z ]
                ]
        Nothing ->
            none

viewMSets : PcSet -> Element Msg
viewMSets pcSet =
    let
        n =
            PcSetBasics.setModulus pcSet

        coprimes =
            List.range 2 (n - 1)
                |> List.filter (Arithmetic.isCoprimeTo n)

        data : List M_relation
        data =
            List.map (makeMrel pcSet) coprimes
    in
    column []
        [ paragraph [] [text "M-related sets"]
        , table 
            []
            { data = data
            , columns = 
                [ { header = text "M Factor"
                  , width = fill
                  , view = 
                        \mrel ->
                            html <| Html.span [] [Html.text "M", Html.sub [] [Html.text <| String.fromInt mrel.factor]]
                    }
                , { header = text "Set"
                  , width = fill
                  , view = 
                        \mrel ->
                            text (PcSetBasics.setToString mrel.set)
                    }
                , { header = text "Transformation of Prime Form"
                  , width = fill
                  , view = 
                        \mrel ->
                            let
                                mSetPF =
                                    PcSetBasics.primeForm mrel.set
                                
                                transformations =
                                    Transformations.possibleTransformations mrel.set mSetPF
                                        |> List.map printTransformation
                                        |> List.intersperse (el [] (text " / "))
                                        |> row []
                            in
                            row [] 
                                [ transformations
                                , text " of "
                                , text <| printPrimeForm mSetPF
                                ]
                  }
                ]
            }
        ]

type alias M_relation =
    { factor : Int
    , set : PcSet
    , setPF : PcSet
    , transformationsOfPF : List Transformation
    }

makeMrel : PcSet -> Int -> M_relation
makeMrel set factor =
    let
        s = 
            PcSetBasics.multiplySet factor set
        pf = 
            PcSetBasics.primeForm s
    in
    M_relation factor s pf (possibleTransformations s pf)


printIcVector : PcSet -> String
printIcVector set =
    "<"
        ++ (icVector set
                |> Array.toList
                |> List.map String.fromInt
                |> String.join ", "
           )
        ++ ">"


printPrimeForm : PcSet -> String
printPrimeForm (PcSet _ pcs) =
    "(" ++ String.join " " (List.map PcInt.toString pcs) ++ ")"


listTransformationsOfPrimeForm : PcSet -> Element Msg
listTransformationsOfPrimeForm set =
    row []
        (possibleTransformations (PcSetBasics.primeForm set) set
            |> List.map printTransformation
            |> List.intersperse (el [] (text " / ")) 
        )


printTransformation : Transformation -> Element Msg
printTransformation t =
    case t of
        Transposition i ->
            html <| Html.span [] [ Html.text "T", Html.sub [] [ Html.text (String.fromInt i) ] ]

        Inversion i ->
            html <| Html.span [] [ Html.text "I", Html.sub [] [ Html.text (String.fromInt i) ] ]
