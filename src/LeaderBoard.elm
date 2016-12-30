module LeaderBoard exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import WebSocket as WS


-- model


url : String
url =
    "ws://localhost:5000/runners"


type alias Model =
    { error : Maybe String
    , query : String
    , runners : List Runner
    , active : Bool
    }


type alias Runner =
    { id : String
    , name : String
    , location : String
    , age : Int
    , bib : Int
    , estimatedDistance : Float
    , lastMarkerDistance : Float
    , lastMarkerTime : Float
    , pace : Float
    }


initModel : Model
initModel =
    { error = Nothing
    , query = ""
    , runners = []
    , active = False
    }


initCmd : Cmd Msg
initCmd =
    WS.send url (encodeWsMsg "listen runners" JE.null)


init : ( Model, Cmd Msg )
init =
    ( initModel, initCmd )


encodeWsMsg : String -> JE.Value -> String
encodeWsMsg name data =
    JE.encode 0
        (JE.object
            [ ( "name", JE.string name )
            , ( "data", data )
            ]
        )



-- update


type Msg
    = SearchInput String
    | Search
    | WsMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none )

        Search ->
            ( model, Cmd.none )

        WsMessage wsMsg ->
            handleWsMsg wsMsg model


handleWsMsg : String -> Model -> ( Model, Cmd Msg )
handleWsMsg wsMsg model =
    case JD.decodeString wsMsgDecoder wsMsg of
        Ok { name, runner } ->
            case name of
                "new runner" ->
                    ( { model | runners = runner :: model.runners }
                    , Cmd.none
                    )

                _ ->
                    ( { model | error = Just ("Unknown message " ++ name) }
                    , Cmd.none
                    )

        Err err ->
            ( { model | error = Just err }, Cmd.none )


type alias RunnerWsMsg =
    { name : String
    , runner : Runner
    }


wsMsgDecoder : JD.Decoder RunnerWsMsg
wsMsgDecoder =
    JDP.decode RunnerWsMsg
        |> JDP.required "name" JD.string
        |> JDP.required "data" runnerDecoder


runnerDecoder : JD.Decoder Runner
runnerDecoder =
    JDP.decode Runner
        |> JDP.required "_id" JD.string
        |> JDP.required "name" JD.string
        |> JDP.required "location" JD.string
        |> JDP.required "age" JD.int
        |> JDP.required "bib" JD.int
        |> JDP.hardcoded 0
        |> JDP.required "lastMarkerDistance" JD.float
        |> JDP.required "lastMarkerTime" JD.float
        |> JDP.required "pace" JD.float



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , searchForm model.query
        , runners model
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg
                , button [ type_ "button" ] [ text "×" ]
                ]


searchForm : String -> Html Msg
searchForm query =
    Html.form [ onSubmit Search ]
        [ input
            [ type_ "text"
            , placeholder "Search for runner..."
            , value query
            , onInput SearchInput
            ]
            []
        , button [ type_ "submit" ] [ text "Search" ]
        ]


runners : Model -> Html Msg
runners { query, runners } =
    runners
        |> List.map runner
        |> tbody []
        |> (\r -> runnersHeader :: [ r ])
        |> table []


runner : Runner -> Html Msg
runner { name, location, age, bib, estimatedDistance } =
    tr []
        [ td [] [ text name ]
        , td [] [ text location ]
        , td [] [ text (toString age) ]
        , td [] [ text (toString bib) ]
        , td []
            [ text "1 mi @ 08:30AM (TODO)"
            ]
        , td [] [ text (toString estimatedDistance) ]
        ]


runnersHeader : Html Msg
runnersHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "From" ]
            , th [] [ text "Age" ]
            , th [] [ text "Bib #" ]
            , th [] [ text "Last Marker" ]
            , th [] [ text "Est. Miles" ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    WS.listen url WsMessage
