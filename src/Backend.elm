module Backend exposing (app)

import Dict
import Lamdera exposing (ClientId, SessionId)
import Types exposing (AdminData, BackendModel, BackendMsg(..), CurrentQuestion(..), ToBackend(..), ToFrontend(..))


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Lamdera.onConnect UserConnected
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { howAreYou = Dict.empty
      , howExperiencedAreYouWithElm = Dict.empty
      , howExperiencedAreYouWithProgramming = Dict.empty
      , whatCountryAreYouFrom = Dict.empty
      , currentQuestion = HowAreYou_
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        UserConnected _ clientId ->
            ( model, Lamdera.sendToFrontend clientId (SetCurrentQuestion model.currentQuestion) )


convertModelToAdminUpdate : BackendModel -> AdminData
convertModelToAdminUpdate model =
    { howAreYou = Dict.values model.howAreYou
    , howExperiencedAreYouWithElm = Dict.values model.howExperiencedAreYouWithElm
    , howExperiencedAreYouWithProgramming = Dict.values model.howExperiencedAreYouWithProgramming
    , whatCountryAreYouFrom = Dict.values model.whatCountryAreYouFrom
    }


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId _ msg model =
    case msg of
        ChoseHowAreYou happiness ->
            let
                newModel : BackendModel
                newModel =
                    { model | howAreYou = Dict.insert sessionId happiness model.howAreYou }
            in
            ( newModel, convertModelToAdminUpdate newModel |> UpdateAdmin |> Lamdera.broadcast )

        ChoseHowExperiencedAreYouWithElm experienceLevel ->
            let
                newModel : BackendModel
                newModel =
                    { model | howExperiencedAreYouWithElm = Dict.insert sessionId experienceLevel model.howExperiencedAreYouWithElm }
            in
            ( newModel, convertModelToAdminUpdate newModel |> UpdateAdmin |> Lamdera.broadcast )

        ChoseHowExperiencedAreYouWithProgramming experienceLevel ->
            let
                newModel : BackendModel
                newModel =
                    { model
                        | howExperiencedAreYouWithProgramming =
                            Dict.insert sessionId experienceLevel model.howExperiencedAreYouWithProgramming
                    }
            in
            ( newModel, convertModelToAdminUpdate newModel |> UpdateAdmin |> Lamdera.broadcast )

        ChoseWhatCountryAreYouFrom country ->
            let
                newModel : BackendModel
                newModel =
                    { model | whatCountryAreYouFrom = Dict.insert sessionId country model.whatCountryAreYouFrom }
            in
            ( newModel, convertModelToAdminUpdate newModel |> UpdateAdmin |> Lamdera.broadcast )

        AdminRequestNextQuestion ->
            ( { model | currentQuestion = nextCurrentQuestion model.currentQuestion }
            , Lamdera.broadcast (SetCurrentQuestion (nextCurrentQuestion model.currentQuestion))
            )

        AdminRequestReset ->
            ( { model
                | howAreYou = Dict.empty
                , howExperiencedAreYouWithElm = Dict.empty
                , howExperiencedAreYouWithProgramming = Dict.empty
                , whatCountryAreYouFrom = Dict.empty
                , currentQuestion = HowAreYou_
              }
            , Lamdera.broadcast (SetCurrentQuestion HowAreYou_)
            )


nextCurrentQuestion : CurrentQuestion -> CurrentQuestion
nextCurrentQuestion currentQuestion =
    case currentQuestion of
        HowAreYou_ ->
            HowExperiencedAreYouWithElm_

        HowExperiencedAreYouWithElm_ ->
            HowExperiencedAreYouWithProgramming_

        HowExperiencedAreYouWithProgramming_ ->
            WhatCountryAreYouFrom_

        WhatCountryAreYouFrom_ ->
            WhatCountryAreYouFrom_
