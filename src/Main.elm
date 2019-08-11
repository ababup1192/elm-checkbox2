module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { checkedValueList : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { checkedValueList = [] }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = UpdateCheck ChangeWithValue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCheck { isChecked, value } ->
            let
                newCheckedValueList =
                    if isChecked then
                        value :: model.checkedValueList

                    else
                        List.filter (\cv -> not <| cv == value) model.checkedValueList
            in
            ( { model | checkedValueList = newCheckedValueList }, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Browser.Document Msg
view model =
    { title = "Elm 0.19 starter"
    , body =
        [ div [ class "sample-elm-ui-components" ] <|
            List.map
                (\v ->
                    label []
                        [ input
                            [ type_ "checkbox"
                            , class "checkbox"
                            , value v
                            , checked <| List.member v model.checkedValueList
                            , onChangeWithValue UpdateCheck
                            ]
                            []
                        , span [ class "checkbox-icon" ] [ text v ]
                        ]
                )
                foods
        , span [] [ text <| String.join ", " model.checkedValueList ]
        ]
    }


foods =
    [ "お肉", "お魚", "おにぎり" ]


type alias ChangeWithValue =
    { isChecked : Bool, value : String }


onChangeWithValue : (ChangeWithValue -> msg) -> Attribute msg
onChangeWithValue onCheckedAction =
    on "change" <|
        JD.map2
            (\value isChecked ->
                onCheckedAction { value = value, isChecked = isChecked }
            )
            targetValue
            targetChecked



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
