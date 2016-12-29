port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import LeaderBoard
import Login
import Runner


-- model


type Page
    = NotFound
    | LeaderBoardPage
    | LoginPage
    | RunnerPage


securePages : List Page
securePages =
    [ RunnerPage ]


loginTarget : Page -> String -> String
loginTarget page target =
    case page of
        LoginPage ->
            target

        _ ->
            pageToHash page


pageOrLoginPage : Page -> String -> Bool -> ( Page, String, Cmd Msg )
pageOrLoginPage page target loggedIn =
    if loggedIn || not (List.member page securePages) then
        ( page
        , loginTarget page target
        , Cmd.none
        )
    else
        ( LoginPage
        , loginTarget page target
        , pageToCmd LoginPage
        )


type alias Model =
    { page : Page
    , target : String
    , leaderBoard : LeaderBoard.Model
    , login : Login.Model
    , runner : Runner.Model
    , token : Maybe String
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        target =
            location.hash

        page =
            hashToPage target

        ( securePage, secureTarget, secureCmd ) =
            pageOrLoginPage page target (flags.token /= Nothing)

        ( lbInitModel, lbInitCmd ) =
            LeaderBoard.init

        ( loginInitModel, loginInitCmd ) =
            Login.init

        ( runnerInitModel, runnerInitCmd ) =
            Runner.init

        initModel =
            { page = securePage
            , target = secureTarget
            , leaderBoard = lbInitModel
            , login = loginInitModel
            , runner = runnerInitModel
            , token = flags.token
            }

        initCmd =
            Cmd.batch
                [ Cmd.map LeaderBoardMsg lbInitCmd
                , Cmd.map LoginMsg loginInitCmd
                , Cmd.map RunnerMsg runnerInitCmd
                , secureCmd
                ]
    in
        ( initModel, initCmd )


loggedIn : Model -> Bool
loggedIn model =
    model.token /= Nothing



-- update


type Msg
    = Navigate Page
    | ChangePage Page
    | LeaderBoardMsg LeaderBoard.Msg
    | LoginMsg Login.Msg
    | RunnerMsg Runner.Msg
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( model, pageToCmd page )

        ChangePage page ->
            let
                ( securePage, secureTarget, secureCmd ) =
                    pageOrLoginPage page model.target (loggedIn model)
            in
                ( { model | page = securePage, target = secureTarget }, secureCmd )

        LeaderBoardMsg msg ->
            let
                ( lbModel, lbCmd ) =
                    LeaderBoard.update msg model.leaderBoard
            in
                ( { model | leaderBoard = lbModel }
                , Cmd.map LeaderBoardMsg lbCmd
                )

        LoginMsg msg ->
            let
                ( loginModel, loginToken, loginCmd ) =
                    Login.update msg model.login model.target

                saveTokenCmd =
                    case loginToken of
                        Just jwt ->
                            saveToken jwt

                        Nothing ->
                            Cmd.none
            in
                ( { model
                    | login = loginModel
                    , token = loginToken
                  }
                , Cmd.batch
                    [ Cmd.map LoginMsg loginCmd
                    , saveTokenCmd
                    ]
                )

        RunnerMsg msg ->
            let
                ( runnerModel, runnerCmd ) =
                    Runner.update msg model.runner
            in
                ( { model | runner = runnerModel }
                , Cmd.map RunnerMsg runnerCmd
                )

        Logout ->
            ( { model | token = Nothing }
            , Cmd.batch
                [ deleteToken ()
                , pageToCmd LeaderBoardPage
                ]
            )



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
                [ addRunnerLinkView model ]
            ]
        , ul []
            [ li []
                [ loginLinkView model ]
            ]
        ]


addRunnerLinkView : Model -> Html Msg
addRunnerLinkView model =
    if loggedIn model then
        a [ onClick (Navigate RunnerPage) ] [ text "Add Runner" ]
    else
        text ""


loginLinkView : Model -> Html Msg
loginLinkView model =
    if loggedIn model then
        a [ onClick Logout ] [ text "Logout" ]
    else
        a [ onClick (Navigate LoginPage) ] [ text "Login" ]



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


pageToCmd : Page -> Cmd Msg
pageToCmd page =
    page |> pageToHash |> Navigation.modifyUrl


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location.hash |> hashToPage |> ChangePage



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ model.leaderBoard |> LeaderBoard.subscriptions |> Sub.map LeaderBoardMsg
        , model.login |> Login.subscriptions |> Sub.map LoginMsg
        , model.runner |> Runner.subscriptions |> Sub.map RunnerMsg
        ]



-- ports


port saveToken : String -> Cmd msg


port deleteToken : () -> Cmd msg



-- main


type alias Flags =
    { token : Maybe String }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
