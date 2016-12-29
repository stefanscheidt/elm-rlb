module Login exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Navigation


-- model


url : String
url =
    "http://localhost:5000/authenticate"


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    , target : String
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    , error = Nothing
    , target = "/#"
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = UsernameInput String
    | PasswordInput String
    | Submit
    | LoginResponse (Result Http.Error String)
    | Error String


update : Msg -> Model -> String -> ( Model, Maybe String, Cmd Msg )
update msg model target =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Nothing, Cmd.none )

        PasswordInput password ->
            ( { model | password = password }, Nothing, Cmd.none )

        Submit ->
            let
                body =
                    [ ( "username", JE.string model.username )
                    , ( "password", JE.string model.password )
                    ]
                        |> JE.object
                        |> JE.encode 0
                        |> Http.stringBody "application/json"

                decoder =
                    field "token" JD.string

                request =
                    Http.post url body decoder

                cmd =
                    Http.send LoginResponse request
            in
                ( { model | target = target }, Nothing, cmd )

        LoginResponse (Ok token) ->
            ( initModel, Just token, Navigation.newUrl model.target )

        LoginResponse (Err err) ->
            let
                errorMessage =
                    case err of
                        Http.BadStatus resp ->
                            case resp.status.code of
                                401 ->
                                    resp.body

                                _ ->
                                    resp.status.message

                        _ ->
                            "Login Error!"
            in
                ( { model | error = Just errorMessage }, Nothing, Cmd.none )

        Error error ->
            ( { model | error = Just error }, Nothing, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , loginForm model
        ]


loginForm : Model -> Html Msg
loginForm model =
    Html.form [ class "add-runner", onSubmit Submit ]
        [ fieldset []
            [ legend [] [ text "Login" ]
            , div []
                [ label [] [ text "User Name" ]
                , input
                    [ type_ "text"
                    , value model.username
                    , onInput UsernameInput
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Password" ]
                , input
                    [ type_ "password"
                    , value model.password
                    , onInput PasswordInput
                    ]
                    []
                ]
            , div []
                [ label [] []
                , button [ type_ "submit" ] [ text "Login" ]
                ]
            ]
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
