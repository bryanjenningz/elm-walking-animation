module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Process
import Task


steps : List Step
steps =
    [ { x = 0, y = 0, dir = South }
    , { x = 1, y = 0, dir = East }
    , { x = 1, y = 1, dir = South }
    , { x = 1, y = 2, dir = South }
    , { x = 1, y = 3, dir = South }
    , { x = 2, y = 3, dir = East }
    , { x = 2, y = 2, dir = North }
    , { x = 3, y = 2, dir = East }
    , { x = 3, y = 1, dir = North }
    , { x = 3, y = 0, dir = North }
    , { x = 2, y = 0, dir = West }
    , { x = 1, y = 0, dir = West }
    , { x = 0, y = 0, dir = West }
    ]


lastStep : Step
lastStep =
    steps
        |> List.reverse
        |> List.head
        |> Maybe.withDefault { x = 0, y = 0, dir = South }


type alias Model =
    { step : Int
    , showMove : Bool
    }


type Direction
    = North
    | South
    | West
    | East


type alias Step =
    { x : Int, y : Int, dir : Direction }


type Msg
    = Play
    | NextStep


init : ( Model, Cmd Msg )
init =
    ( Model 0 True, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model) ]
        , button [ onClick Play ] [ text "Play" ]
        , div [ style [ ( "position", "relative" ) ] ] [ viewBlock model ]
        ]


viewBlock : Model -> Html msg
viewBlock model =
    let
        position : Step
        position =
            steps
                |> List.drop model.step
                |> List.head
                |> Maybe.withDefault lastStep
    in
        div [ style (styleBlock model.showMove position) ]
            [ div [ style (styleBlockDir position.dir) ] [] ]


styleBlock : Bool -> Step -> List ( String, String )
styleBlock showMove { x, y } =
    [ ( "width", "50px" )
    , ( "height", "50px" )
    , ( "position", "absolute" )
    , ( "left", toString (50 * x) ++ "px" )
    , ( "top", toString (50 * y) ++ "px" )
    , ( "background-color", "black" )
    , ( "transition"
      , if showMove then
            "left "
                ++ toString sleep
                ++ "ms linear, top "
                ++ toString sleep
                ++ "ms linear"
        else
            ""
      )
    ]


styleBlockDir : Direction -> List ( String, String )
styleBlockDir dir =
    let
        styleDir =
            case dir of
                North ->
                    [ ( "left", "40%" ), ( "top", "10%" ) ]

                South ->
                    [ ( "left", "40%" ), ( "top", "70%" ) ]

                West ->
                    [ ( "left", "10%" ), ( "top", "40%" ) ]

                East ->
                    [ ( "left", "70%" ), ( "top", "40%" ) ]

        styles =
            [ ( "background-color", "red" )
            , ( "position", "absolute" )
            , ( "width", "20%" )
            , ( "height", "20%" )
            ]
    in
        styles ++ styleDir


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            ( { model | step = 0, showMove = False }, nextStep )

        NextStep ->
            let
                step =
                    model.step + 1
            in
                if step > List.length steps then
                    ( model, Cmd.none )
                else
                    ( { model | step = model.step + 1, showMove = True }
                    , nextStep
                    )


sleep : Float
sleep =
    500


nextStep : Cmd Msg
nextStep =
    delay sleep NextStep


delay : Float -> msg -> Cmd msg
delay ms msg =
    Process.sleep ms
        |> Task.andThen (\_ -> Task.succeed msg)
        |> Task.perform identity


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
