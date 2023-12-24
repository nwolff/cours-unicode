module Main exposing (Msg(..), main)

import Browser exposing (Document)
import Hex
import Html exposing (div, h2, input, label, text)
import Html.Attributes exposing (class, for, id, value)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { hex : String
    , string : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "", Cmd.none )


type Msg
    = GotHex String
    | GotString String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotHex hex ->
            let
                hexWithoutSpaces =
                    String.replace " " "" hex

                string =
                    if String.isEmpty hexWithoutSpaces then
                        ""

                    else
                        case Hex.fromString (String.toLower hexWithoutSpaces) of
                            Ok characterNumber ->
                                String.fromChar (Char.fromCode characterNumber)

                            Err error ->
                                error
            in
            ( Model hex string, Cmd.none )

        GotString string ->
            let
                hex =
                    case String.uncons string of
                        Nothing ->
                            ""

                        Just ( char, _ ) ->
                            String.toUpper (Hex.toString (Char.toCode char))
            in
            ( Model hex string, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    Document "Convertisseur unicode"
        [ div [ class "container" ]
            [ h2 [ class "mt-2 mb-4" ] [ text "Convertisseur unicode" ]
            , div [ class "my-3" ]
                [ label [ for "hex", class "form-label" ]
                    [ text "Numéro (hexadécimal)" ]
                , div []
                    [ input [ id "hex", value model.hex, onInput GotHex, class "form-control form-control-lg" ] [] ]
                ]
            , div [ class "my-3" ]
                [ label [ for "string", class "form-label" ]
                    [ text "Caractère" ]
                , div []
                    [ input [ id "string", value model.string, onInput GotString, class "form-control form-control-lg" ] [] ]
                ]
            ]
        ]
