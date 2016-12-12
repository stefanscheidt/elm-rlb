module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { username : String
    , password : String
    }


type Msg
    = UsernameInput String
    | PasswordInput String


initialModel : Model
initialModel =
    { username = "", password = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UsernameInput username ->
            { model | username = username }

        PasswordInput password ->
            { model | password = password }


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", onInput UsernameInput, value model.username, placeholder "Username" ] []
        , input [ type_ "password", onInput PasswordInput, value model.password, placeholder "Password" ] []
        ]
