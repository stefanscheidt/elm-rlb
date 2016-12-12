module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LeaderBoard
import Login


type Page
    = LoginPage
    | LeaderBoardPage


type alias Model =
    { page : Page
    , loginModel : Login.Model
    , leaderBoardModel : LeaderBoard.Model
    }


type Msg
    = ChangePage Page
    | LoginMessage Login.Msg
    | LeaderBoardMessage LeaderBoard.Msg


initialModel : Model
initialModel =
    { page = LeaderBoardPage
    , loginModel = Login.initialModel
    , leaderBoardModel = LeaderBoard.initialModel
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangePage page ->
            { model | page = page }

        LoginMessage loginMessage ->
            { model | loginModel = (Login.update loginMessage model.loginModel) }

        LeaderBoardMessage leaderBoardMessage ->
            { model | leaderBoardModel = (LeaderBoard.update leaderBoardMessage model.leaderBoardModel) }


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                LeaderBoardPage ->
                    Html.map LeaderBoardMessage
                        (LeaderBoard.view model.leaderBoardModel)

                LoginPage ->
                    Html.map LoginMessage
                        (Login.view model.loginModel)
    in
        div []
            [ div []
                [ a
                    [ href "#"
                    , onClick (ChangePage LeaderBoardPage)
                    ]
                    [ text "LeaderBoard" ]
                , span [] [ text " | " ]
                , a
                    [ href "#"
                    , onClick (ChangePage LoginPage)
                    ]
                    [ text "Login" ]
                ]
            , hr [] []
            , page
            ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
