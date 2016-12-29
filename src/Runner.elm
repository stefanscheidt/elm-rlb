module Runner exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import String


-- model


type Status
    = Saving String
    | Saved String
    | NotSaved


type alias Model =
    { id : String
    , name : String
    , nameError : Maybe String
    , location : String
    , locationError : Maybe String
    , age : String
    , ageError : Maybe String
    , bib : String
    , bibError : Maybe String
    , error : Maybe String
    , status : Status
    }


initModel : Model
initModel =
    { id = ""
    , name = ""
    , nameError = Nothing
    , location = ""
    , locationError = Nothing
    , age = ""
    , ageError = Nothing
    , bib = ""
    , bibError = Nothing
    , error = Nothing
    , status = NotSaved
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


validate : Model -> Model
validate model =
    model
        |> validateName
        |> validateLocation
        |> validateAge
        |> validateBib


validateName : Model -> Model
validateName model =
    if String.isEmpty model.name then
        { model | nameError = Just "Name is required" }
    else
        { model | nameError = Nothing }


validateLocation : Model -> Model
validateLocation model =
    if String.isEmpty model.location then
        { model | locationError = Just "Location is required" }
    else
        { model | locationError = Nothing }


validateAge : Model -> Model
validateAge model =
    let
        ageAsInt =
            Result.withDefault 0 (String.toInt model.age)
    in
        if ageAsInt <= 0 then
            { model | ageError = Just "Age must be a positve number" }
        else
            { model | ageError = Nothing }


validateBib : Model -> Model
validateBib model =
    let
        bibAsInt =
            Result.withDefault 0 (String.toInt model.bib)
    in
        if bibAsInt <= 0 then
            { model | bibError = Just "Bib must be a positve number" }
        else
            { model | bibError = Nothing }


isValid : Model -> Bool
isValid model =
    model.nameError
        == Nothing
        && model.locationError
        == Nothing
        && model.ageError
        == Nothing
        && model.bibError
        == Nothing



-- update


type Msg
    = NameInput String
    | LocationInput String
    | AgeInput String
    | BibInput String
    | Save
    | SaveResponse (Result Http.Error String)


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        NameInput name ->
            ( { model
                | name = name
                , nameError = Nothing
              }
            , Cmd.none
            )

        LocationInput location ->
            ( { model
                | location = location
                , locationError = Nothing
              }
            , Cmd.none
            )

        AgeInput age ->
            ageInput model age

        BibInput bib ->
            bibInput model bib

        Save ->
            let
                updatedModel =
                    validate model
            in
                if isValid updatedModel then
                    save model token
                else
                    ( updatedModel, Cmd.none )

        SaveResponse (Ok _) ->
            ( { initModel | status = Saved "Runner saved." }, Cmd.none )

        SaveResponse (Err err) ->
            let
                errorMessage =
                    case err of
                        Http.BadStatus resp ->
                            resp.body

                        _ ->
                            "Error saving runner!"
            in
                ( { model | status = NotSaved, error = Just errorMessage }, Cmd.none )


ageInput : Model -> String -> ( Model, Cmd Msg )
ageInput model age =
    let
        ageInt =
            age
                |> String.toInt
                |> Result.withDefault 0

        ageError =
            if ageInt <= 0 then
                Just "Must Enter a Positive Number"
            else
                Nothing
    in
        ( { model | age = age, ageError = ageError }, Cmd.none )


bibInput : Model -> String -> ( Model, Cmd Msg )
bibInput model bib =
    let
        bibInt =
            bib
                |> String.toInt
                |> Result.withDefault 0

        bibError =
            if bibInt <= 0 then
                Just "Must Enter a Positive Number"
            else
                Nothing
    in
        ( { model | bib = bib, bibError = bibError }, Cmd.none )


save : Model -> String -> ( Model, Cmd Msg )
save model token =
    let
        url =
            "http://localhost:5000/runner"

        headers =
            [ Http.header "Authorization" ("Bearer " ++ token) ]

        json =
            JE.object
                [ ( "name", JE.string model.name )
                , ( "location", JE.string model.location )
                , ( "age", JE.int (asInt model.age) )
                , ( "bib", JE.int (asInt model.bib) )
                ]

        body =
            Http.jsonBody json

        decoder =
            JD.field "_id" JD.string

        request =
            postWithHeaders url headers body decoder

        cmd =
            Http.send SaveResponse request
    in
        ( { model | status = Saving "Saving runner..." }, cmd )


postWithHeaders : String -> List Http.Header -> Http.Body -> JD.Decoder a -> Http.Request a
postWithHeaders url headers body decoder =
    Http.request
        { method = "POST"
        , url = url
        , headers = headers
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


asInt : String -> Int
asInt string =
    Result.withDefault 0 (String.toInt string)



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , viewForm model
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg
                ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ class "add-runner", onSubmit Save ]
        [ fieldset []
            [ legend [] [ text "Add / Edit Runner" ]
            , div []
                [ label [] [ text "Name" ]
                , input
                    [ type_ "text"
                    , value model.name
                    , onInput NameInput
                    ]
                    []
                , span [] [ text <| Maybe.withDefault "" model.nameError ]
                ]
            , div []
                [ label [] [ text "Location" ]
                , input
                    [ type_ "text"
                    , value model.location
                    , onInput LocationInput
                    ]
                    []
                , span [] [ text <| Maybe.withDefault "" model.locationError ]
                ]
            , div []
                [ label [] [ text "Age" ]
                , input
                    [ type_ "text"
                    , value model.age
                    , onInput AgeInput
                    ]
                    []
                , span [] [ text <| Maybe.withDefault "" model.ageError ]
                ]
            , div []
                [ label [] [ text "Bib #" ]
                , input
                    [ type_ "text"
                    , value model.bib
                    , onInput BibInput
                    ]
                    []
                , span [] [ text <| Maybe.withDefault "" model.bibError ]
                ]
            , div []
                [ label [] []
                , button [ type_ "submit" ] [ text "Save" ]
                , span []
                    [ text <|
                        case model.status of
                            Saving savingMsg ->
                                savingMsg

                            Saved savedMsg ->
                                savedMsg

                            NotSaved ->
                                ""
                    ]
                ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
