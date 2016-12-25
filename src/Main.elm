port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import LeaderBoard
import Login
import Runner


-- model


type alias Model =
    { page : Page
    , leaderBoard : LeaderBoard.Model
    , login : Login.Model
    , runner : Runner.Model
    }


type Page
    = NotFound
    | LeaderBoardPage
    | LoginPage
    | RunnerPage


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            hashToPage location.hash

        ( lbInitModel, lbInitCmd ) =
            LeaderBoard.init

        ( loginInitModel, loginInitCmd ) =
            Login.init

        ( runnerInitModel, runnerInitCmd ) =
            Runner.init

        initModel =
            { page = page
            , leaderBoard = lbInitModel
            , login = loginInitModel
            , runner = runnerInitModel
            }

        initCmd =
            Cmd.batch
                [ Cmd.map LeaderBoardMsg lbInitCmd
                , Cmd.map LoginMsg loginInitCmd
                , Cmd.map RunnerMsg runnerInitCmd
                ]
    in
        ( initModel, initCmd )



-- update


type Msg
    = Navigate Page
    | ChangePage Page
    | LeaderBoardMsg LeaderBoard.Msg
    | LoginMsg Login.Msg
    | RunnerMsg Runner.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( model, page |> pageToHash |> Navigation.newUrl )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        LeaderBoardMsg msg ->
            let
                ( lbModel, lbCmd ) =
                    LeaderBoard.update msg model.leaderBoard

                newModel =
                    { model | leaderBoard = lbModel }

                newCmd =
                    Cmd.map LeaderBoardMsg lbCmd
            in
                ( newModel, newCmd )

        LoginMsg msg ->
            let
                ( loginModel, loginCmd ) =
                    Login.update msg model.login

                newModel =
                    { model | login = loginModel }

                newCmd =
                    Cmd.map LoginMsg loginCmd
            in
                ( newModel, newCmd )

        RunnerMsg msg ->
            let
                ( runnerModel, runnerCmd ) =
                    Runner.update msg model.runner

                newModel =
                    { model | runner = runnerModel }

                newCmd =
                    Cmd.map RunnerMsg runnerCmd
            in
                ( newModel, newCmd )



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                LeaderBoardPage ->
                    model.leaderBoard |> LeaderBoard.view |> Html.map LeaderBoardMsg

                LoginPage ->
                    model.login |> Login.view |> Html.map LoginMsg

                RunnerPage ->
                    model.runner |> Runner.view |> Html.map RunnerMsg

                NotFound ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]
    in
        div []
            [ pageHeader model
            , page
            ]


pageHeader : Model -> Html Msg
pageHeader model =
    header []
        [ a [ onClick (Navigate LeaderBoardPage) ] [ text "Race Results" ]
        , ul []
            [ li []
                [ a [ onClick (Navigate RunnerPage) ] [ text "Add Runner" ]
                ]
            ]
        , ul []
            [ li []
                [ a [ onClick (Navigate LoginPage) ] [ text "Login" ]
                ]
            ]
        ]



-- navigation


hashToPage : String -> Page
hashToPage hash =
    case hash of
        "" ->
            LeaderBoardPage

        "#/" ->
            LeaderBoardPage

        "#/login" ->
            LoginPage

        "#/add" ->
            RunnerPage

        _ ->
            NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        LeaderBoardPage ->
            "#/"

        LoginPage ->
            "#/login"

        RunnerPage ->
            "#/add"

        NotFound ->
            "#notfound"


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location.hash |> hashToPage |> ChangePage



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        lbSubscriptons =
            LeaderBoard.subscriptions model.leaderBoard

        loginSubscriptions =
            Login.subscriptions model.login

        runnerSubscriptions =
            Runner.subscriptions model.runner
    in
        Sub.batch
            [ Sub.map LeaderBoardMsg lbSubscriptons
            , Sub.map LoginMsg loginSubscriptions
            , Sub.map RunnerMsg runnerSubscriptions
            ]


main : Program Never Model Msg
main =
    Navigation.program locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
