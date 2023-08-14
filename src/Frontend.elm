module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Countries exposing (Country)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Env
import Lamdera
import List.Extra as List
import Task
import Types exposing (AdminData, CurrentQuestion(..), ExperienceLevel(..), FrontendModel(..), FrontendMsg(..), Happiness(..), Question(..), QuestionData, Screen, ToBackend(..), ToFrontend(..))
import Url
import Url.Parser
import Url.Parser.Query


app :
    { init : Lamdera.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url.Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


decodeUrl : Url.Parser.Parser (Maybe String -> a) a
decodeUrl =
    Url.Parser.query (Url.Parser.Query.string "secret")


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        defaultScreen : Screen
        defaultScreen =
            { width = 1024
            , height = 768
            }
    in
    ( case Url.Parser.parse decodeUrl url of
        Just (Just secret) ->
            if secret == Env.secret then
                IsAdmin
                    HowAreYou_
                    { howAreYou = []
                    , howExperiencedAreYouWithElm = []
                    , howExperiencedAreYouWithProgramming = []
                    , whatCountryAreYouFrom = []
                    }

            else
                IsUser defaultScreen (HowAreYou Nothing)

        _ ->
            IsUser defaultScreen (HowAreYou Nothing)
    , Cmd.batch
        [ Browser.Navigation.replaceUrl key (Url.toString { url | query = Nothing })
        , Browser.Dom.getViewport
            |> Task.perform
                (\{ viewport } ->
                    ScreenSize
                        (floor viewport.width)
                        (floor viewport.height)
                )
        ]
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal _ ->
                    ( model, Cmd.none )

                External url ->
                    ( model, Browser.Navigation.load url )

        UrlChanged _ ->
            ( model, Cmd.none )

        PressedHowAreYou happiness ->
            case model of
                IsAdmin _ _ ->
                    ( model, Cmd.none )

                IsUser screen question ->
                    case question of
                        HowAreYou _ ->
                            ( Just happiness |> HowAreYou |> IsUser screen
                            , Lamdera.sendToBackend (ChoseHowAreYou happiness)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedHowExperiencedAreYouWithElm experienceLevel ->
            case model of
                IsAdmin _ _ ->
                    ( model, Cmd.none )

                IsUser screen question ->
                    case question of
                        HowExperiencedAreYouWithElm _ ->
                            ( Just experienceLevel |> HowExperiencedAreYouWithElm |> IsUser screen
                            , Lamdera.sendToBackend (ChoseHowExperiencedAreYouWithElm experienceLevel)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedHowExperiencedAreYouWithProgramming experienceLevel ->
            case model of
                IsAdmin _ _ ->
                    ( model, Cmd.none )

                IsUser screen question ->
                    case question of
                        HowExperiencedAreYouWithProgramming _ ->
                            ( Just experienceLevel |> HowExperiencedAreYouWithProgramming |> IsUser screen
                            , Lamdera.sendToBackend (ChoseHowExperiencedAreYouWithProgramming experienceLevel)
                            )

                        _ ->
                            ( model, Cmd.none )

        PressedWhatCountryAreYouFrom country ->
            case model of
                IsAdmin _ _ ->
                    ( model, Cmd.none )

                IsUser screen question ->
                    case question of
                        WhatCountryAreYouFrom _ ->
                            ( Just country |> WhatCountryAreYouFrom |> IsUser screen
                            , Lamdera.sendToBackend (ChoseWhatCountryAreYouFrom country)
                            )

                        _ ->
                            ( model, Cmd.none )

        AdminPressedNextQuestion ->
            case model of
                IsAdmin _ _ ->
                    ( model, Lamdera.sendToBackend AdminRequestNextQuestion )

                IsUser _ _ ->
                    ( model, Cmd.none )

        AdminPressedReset ->
            case model of
                IsAdmin _ _ ->
                    ( model, Lamdera.sendToBackend AdminRequestReset )

                IsUser _ _ ->
                    ( model, Cmd.none )

        ScreenSize width height ->
            case model of
                IsAdmin _ _ ->
                    ( model, Cmd.none )

                IsUser _ question ->
                    ( IsUser { width = width, height = height } question, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UpdateAdmin answerData ->
            case model of
                IsAdmin currentQuestion _ ->
                    ( IsAdmin currentQuestion answerData, Cmd.none )

                IsUser _ _ ->
                    ( model, Cmd.none )

        SetCurrentQuestion question ->
            case model of
                IsAdmin _ adminData ->
                    ( IsAdmin question adminData, Cmd.none )

                IsUser screen _ ->
                    ( currentQuestionToQuestion question |> IsUser screen, Cmd.none )


currentQuestionToQuestion : CurrentQuestion -> Question
currentQuestionToQuestion currentQuestion =
    case currentQuestion of
        HowAreYou_ ->
            HowAreYou Nothing

        HowExperiencedAreYouWithElm_ ->
            HowExperiencedAreYouWithElm Nothing

        HowExperiencedAreYouWithProgramming_ ->
            HowExperiencedAreYouWithProgramming Nothing

        WhatCountryAreYouFrom_ ->
            WhatCountryAreYouFrom Nothing


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Elm Online Survey!"
    , body =
        [ Element.layout
            [ Element.padding 16 ]
            (case model of
                IsAdmin currentQuestion answerData ->
                    Element.column
                        [ Element.width Element.fill, Element.height Element.fill, Element.spacing 8 ]
                        [ adminQuestionView currentQuestion answerData
                        , Element.Input.button
                            [ Element.padding 8
                            , Element.Background.color <| Element.rgb 0.9 0.9 0.9
                            , Element.Border.width 1
                            , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                            ]
                            { onPress = Just AdminPressedNextQuestion
                            , label = Element.text "Next Question"
                            }
                        , Element.Input.button
                            [ Element.padding 8
                            , Element.Background.color <| Element.rgb 0.9 0.9 0.9
                            , Element.Border.width 1
                            , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                            ]
                            { onPress = Just AdminPressedReset
                            , label = Element.text "Reset Questions"
                            }
                        ]

                IsUser screen question ->
                    questionView screen question
            )
        ]
    }


adminQuestionView : CurrentQuestion -> AdminData -> Element FrontendMsg
adminQuestionView currentQuestion adminData =
    let
        inner : QuestionData answer -> List answer -> Element msg
        inner question data =
            questionContainer
                question.title
                (adminAnswers question data)
    in
    case currentQuestion of
        HowAreYou_ ->
            inner happinessQuestion adminData.howAreYou

        HowExperiencedAreYouWithElm_ ->
            inner elmExperienceQuestion adminData.howExperiencedAreYouWithElm

        HowExperiencedAreYouWithProgramming_ ->
            inner programmingExperienceQuestion adminData.howExperiencedAreYouWithProgramming

        WhatCountryAreYouFrom_ ->
            inner countryQuestion adminData.whatCountryAreYouFrom


adminAnswers : QuestionData answer -> List answer -> Element msg
adminAnswers { toString, possibleAnswers } answers_ =
    List.filterMap
        (\answer ->
            let
                count : Int
                count =
                    List.count ((==) answer) answers_
            in
            if count == 0 then
                Nothing

            else
                toString answer
                    ++ " "
                    ++ String.fromInt count
                    |> Element.text
                    |> Element.el
                        [ Element.Background.color <| Element.rgb 0.9 0.9 0.9
                        , Element.Border.width 1
                        , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                        , Element.padding 16
                        ]
                    |> Just
        )
        possibleAnswers
        |> Element.wrappedRow [ Element.spacing 8, Element.centerX ]


questionView : Screen -> Question -> Element FrontendMsg
questionView screen question =
    let
        inner :
            QuestionData option
            ->
                { onPress : option -> msg
                , selected : Maybe option
                , columns : Maybe Int
                }
            -> Element msg
        inner questionData options =
            questionContainer
                questionData.title
                (answers questionData options)
    in
    case question of
        HowAreYou maybeHappiness ->
            inner happinessQuestion
                { onPress = PressedHowAreYou
                , selected = maybeHappiness
                , columns = Nothing
                }

        HowExperiencedAreYouWithElm maybeExperienceLevel ->
            inner elmExperienceQuestion
                { onPress = PressedHowExperiencedAreYouWithElm
                , selected = maybeExperienceLevel
                , columns = Nothing
                }

        HowExperiencedAreYouWithProgramming maybeExperienceLevel ->
            inner programmingExperienceQuestion
                { onPress = PressedHowExperiencedAreYouWithProgramming
                , selected = maybeExperienceLevel
                , columns = Nothing
                }

        WhatCountryAreYouFrom maybeCountry ->
            inner countryQuestion
                { onPress = PressedWhatCountryAreYouFrom
                , selected = maybeCountry

                -- The maximum width of a button is 301, intra-column spacing is 8
                , columns = Just <| max 1 <| (screen.width + 8) // (301 + 8)
                }


questionContainer : String -> Element msg -> Element msg
questionContainer title answers_ =
    Element.column
        [ Element.spacing 16, Element.centerX, Element.centerY ]
        [ Element.paragraph [ Element.Font.center ] [ Element.text title ], answers_ ]


answers :
    QuestionData option
    ->
        { onPress : option -> msg
        , selected : Maybe option
        , columns : Maybe Int
        }
    -> Element msg
answers { toString, possibleAnswers } { onPress, selected, columns } =
    let
        elements : List (Element msg)
        elements =
            List.map
                (\option ->
                    let
                        text : String
                        text =
                            toString option
                    in
                    Element.Input.button
                        [ Element.Background.color
                            (if selected == Just option then
                                Element.rgb 0.7 0.8 0.9

                             else
                                Element.rgb 0.9 0.9 0.9
                            )
                        , Element.height Element.fill
                        , Element.Border.width 1
                        , Element.Border.color <| Element.rgb 0.1 0.1 0.1
                        , Element.padding 16
                        , if String.length text > 30 then
                            Element.Font.size 12

                          else if String.length text > 20 then
                            Element.Font.size 16

                          else
                            Element.Font.size 20
                        , if columns == Nothing then
                            Element.width Element.shrink

                          else
                            Element.width Element.fill

                        -- Prevent different font sizes from changing the height
                        , Element.height <| Element.px 54
                        ]
                        { onPress = Just (onPress option)
                        , label = Element.text text
                        }
                )
                possibleAnswers
    in
    case columns of
        Nothing ->
            Element.wrappedRow
                [ Element.spacing 8
                , Element.centerX
                , Element.width Element.fill
                ]
                elements

        Just columnCount ->
            elements
                |> List.greedyGroupsOf columnCount
                |> transpose
                |> List.map
                    (Element.column
                        [ Element.spacing 8
                        , Element.alignTop
                        , Element.width Element.fill
                        ]
                    )
                |> Element.row
                    [ Element.spacing 8
                    , Element.centerX
                    , Element.width Element.fill
                    ]


transpose : List (List a) -> List (List a)
transpose lists =
    transposeHelp [] lists


transposeHelp : List (List a) -> List (List a) -> List (List a)
transposeHelp acc lists =
    case lists of
        [] ->
            List.reverse acc

        _ ->
            let
                ( heads, tails ) =
                    lists
                        |> List.filterMap List.uncons
                        |> List.unzip
            in
            transposeHelp (heads :: acc) (List.filterNot List.isEmpty tails)


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Browser.Events.onResize ScreenSize


elmExperienceQuestion : QuestionData ExperienceLevel
elmExperienceQuestion =
    { title = "How good are you with Elm?"
    , possibleAnswers = experienceLevelAnswers
    , toString = experienceLevelToString
    }


programmingExperienceQuestion : QuestionData ExperienceLevel
programmingExperienceQuestion =
    { title = "How good are you at programming in general?"
    , possibleAnswers = experienceLevelAnswers
    , toString = experienceLevelToString
    }


experienceLevelAnswers : List ExperienceLevel
experienceLevelAnswers =
    [ Expert, Intermediate, Beginner ]


experienceLevelToString : ExperienceLevel -> String
experienceLevelToString experienceLevel =
    case experienceLevel of
        Expert ->
            "Expert"

        Intermediate ->
            "Intermediate"

        Beginner ->
            "Beginner"


countryQuestion : QuestionData Country
countryQuestion =
    { title = "What country do you live in?"
    , possibleAnswers =
        Countries.all
            |> List.map
                (\country ->
                    case country.code of
                        "TW" ->
                            { country | name = "Taiwan" }

                        "GB" ->
                            { country | name = "United Kingdom" }

                        _ ->
                            country
                )
            |> List.sortBy .name
    , toString = \country -> country.flag ++ " " ++ country.name
    }


happinessQuestion : QuestionData Happiness
happinessQuestion =
    { title = "How are you doing?"
    , possibleAnswers = [ Good, NotGood ]
    , toString =
        \howAreYou ->
            case howAreYou of
                Good ->
                    "Good"

                NotGood ->
                    "Not good"
    }
