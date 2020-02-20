module Balancing exposing (main)

import Browser
import Element exposing (centerX, centerY, column, row, spacing, text, width)
import Element.Background as Background
import Element.Input as Input
import Round exposing (round)


ourWeight : Float
ourWeight =
    -- in kg
    58


maxBarLength : Float
maxBarLength =
    -- in meters
    1.450975


minBarLength : Float
minBarLength =
    -- in meters
    0.0381


type alias Model =
    { hisWeight : Float
    , rightPosition : Float
    , leftPosition : Float
    , lastChanged : LastChanged
    }


init : Model
init =
    Model 0 0 0 Left


type LastChanged
    = Left
    | Right


type Msg
    = HisWeight String
    | PositionRight Float
    | PositionLeft Float


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view >> Element.layout [] }


update : Msg -> Model -> Model
update msg model =
    let
        ownPositionLeft : Float
        ownPositionLeft =
            (0 - model.rightPosition) * ourWeight / model.hisWeight

        ownPositionRight : Float
        ownPositionRight =
            (0 - model.leftPosition) * model.hisWeight / ourWeight
    in
    case msg of
        PositionRight position ->
            { model
                | rightPosition = position
                , leftPosition = ownPositionLeft
                , lastChanged = Right
            }

        PositionLeft position ->
            { model
                | leftPosition = position
                , rightPosition = ownPositionRight
                , lastChanged = Left
            }

        HisWeight hisWeight ->
            let
                stringToFloat : String -> Float
                stringToFloat =
                    Maybe.withDefault 0 << String.toFloat
            in
            case model.lastChanged of
                Left ->
                    { model
                        | hisWeight = stringToFloat hisWeight
                        , rightPosition = ownPositionRight
                    }

                Right ->
                    { model
                        | hisWeight = stringToFloat hisWeight
                        , leftPosition = ownPositionLeft
                    }


view : Model -> Element.Element Msg
view model =
    column
        [ centerX
        , centerY
        , spacing 30
        ]
        [ row [ spacing 610 ]
            [ model.leftPosition |> Round.round 3 >> text
            , model.rightPosition |> Round.round 3 >> text
            ]
        , row
            [ spacing 50
            , centerX
            , centerY
            ]
            [ slider PositionLeft model.leftPosition True <| Just 0.1
            , slider PositionRight model.rightPosition False Nothing
            ]
        , row [ centerX, spacing 100, centerY ]
            [ textInput model.hisWeight HisWeight
            , text <| "our weight: " ++ String.fromFloat ourWeight
            ]
        ]


slider : (Float -> Msg) -> Float -> Bool -> Maybe Float -> Element.Element Msg
slider msg model isSwitched step =
    let
        maxMin : ( Float, Float )
        maxMin =
            if isSwitched then
                ( -maxBarLength, -minBarLength )

            else
                ( minBarLength, maxBarLength )
    in
    Input.slider
        [ Element.centerY
        , Background.color <| Element.rgb255 0 0 200
        , width (Element.px 300)
        ]
        { onChange = msg
        , label = Input.labelHidden ""
        , min = Tuple.first maxMin
        , max = Tuple.second maxMin
        , value = model
        , thumb = Input.defaultThumb
        , step = step
        }


textInput : Float -> (String -> Msg) -> Element.Element Msg
textInput model msg =
    Input.text []
        { onChange = msg
        , placeholder = Nothing
        , label = Input.labelBelow [] <| Element.text "his weight"
        , text = String.fromFloat model
        }
