module LeaderBoard exposing (..)

import Date
import Date.Extra.Format as DateFormat
import Date.Extra.Config.Config_en_us as DateConfig
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import String
import Time
import WebSocket as WS


-- model


url : String
url =
    "ws://localhost:5000/runners"


type alias Model =
    { error : Maybe String
    , query : String
    , searchTerm : Maybe String
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


advanceDistance : Time.Time -> Runner -> Runner
advanceDistance time runner =
    let
        elapsedMinutes =
            (time - runner.lastMarkerTime) / 1000 / 60
    in
        if runner.lastMarkerTime > 0 then
            { runner | estimatedDistance = runner.lastMarkerDistance + (elapsedMinutes * runner.pace) }
        else
            runner


descComparison : Runner -> Runner -> Order
descComparison a b =
    case compare a.estimatedDistance b.estimatedDistance of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


initModel : Model
initModel =
    { error = Nothing
    , query = ""
    , searchTerm = Nothing
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
    | Tick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none )

        Search ->
            let
                searchTerm =
                    if String.isEmpty model.query then
                        Nothing
                    else
                        Just model.query
            in
                ( { model | searchTerm = searchTerm }, Cmd.none )

        WsMessage wsMsg ->
            handleWsMsg wsMsg model

        Tick time ->
            ( tick model time, Cmd.none )


handleWsMsg : String -> Model -> ( Model, Cmd Msg )
handleWsMsg wsMsg model =
    case JD.decodeString wsMsgDecoder wsMsg of
        Ok { name, runner } ->
            case name of
                "new runner" ->
                    ( { model | runners = runner :: model.runners }
                    , Cmd.none
                    )

                "update runner" ->
                    let
                        updatedRunners =
                            model.runners
                                |> List.map
                                    (\r ->
                                        if r.id == runner.id then
                                            runner
                                        else
                                            r
                                    )
                    in
                        ( { model | runners = updatedRunners }
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


tick : Model -> Time.Time -> Model
tick model time =
    { model | runners = List.map (advanceDistance time) model.runners }



-- view


formatDistance : Float -> String
formatDistance distance =
    if distance <= 0 then
        ""
    else
        toString ((toFloat (round (distance * 100))) / 100)


formatTime : Time.Time -> String
formatTime time =
    if time > 0 then
        time
            |> Date.fromTime
            |> DateFormat.format DateConfig.config "%H:%M:%S %P"
    else
        ""


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
runners { runners, searchTerm } =
    runners
        |> List.filter (showRunner searchTerm)
        |> List.sortWith descComparison
        |> List.map runner
        |> tbody []
        |> (\r -> runnersHeader :: [ r ])
        |> table []


showRunner : Maybe String -> Runner -> Bool
showRunner searchTerm runner =
    searchTerm
        |> Maybe.map (\term -> String.contains term runner.name)
        |> Maybe.withDefault True


runner : Runner -> Html Msg
runner runner =
    tr []
        [ td [] [ text runner.name ]
        , td [] [ text runner.location ]
        , td [] [ text (toString runner.age) ]
        , td [] [ text (toString runner.bib) ]
        , td []
            [ lastMarker runner
            ]
        , td [] [ text (formatDistance runner.estimatedDistance) ]
        ]


lastMarker : Runner -> Html Msg
lastMarker runner =
    if runner.lastMarkerDistance > 0 then
        text
            ((formatDistance runner.lastMarkerDistance)
                ++ " mi @ "
                ++ (formatTime runner.lastMarkerTime)
            )
    else
        text ""


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
    Sub.batch
        [ WS.listen url WsMessage
        , Time.every Time.second Tick
        ]
