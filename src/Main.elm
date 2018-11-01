module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Process
import Task exposing (Task)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, map, oneOf, s, top)



-- STYLES


colors =
    { white = rgb255 255 255 255
    , coral = rgb255 255 100 80
    , grays =
        { lighter = rgb255 240 240 240
        , light = rgb255 200 200 200
        , dark = rgb255 50 50 50
        }
    }


fonts =
    { heading =
        Font.external
            { url = "https://fonts.googleapis.com/css?family=Source+Sans+Pro:600"
            , name = "Source Sans Pro"
            }
    , body =
        Font.external
            { url = "https://fonts.googleapis.com/css?family=Nunito:400,700"
            , name = "Nunito"
            }
    }


icons =
    { menu = icon "menu"
    , user = icon "person"
    }


icon : String -> Element msg
icon name =
    html (Html.span [ Html.Attributes.class ("icon ion-md-" ++ name) ] [])


setAlpha : Float -> Element.Color -> Element.Color
setAlpha alpha =
    toRgb >> (\color -> { color | alpha = alpha }) >> fromRgb


shadows =
    { card =
        { offset = ( 0, 4 )
        , size = 0
        , blur = 8
        , color = setAlpha 0.125 colors.grays.dark
        }
    }


classes =
    { input =
        [ width fill
        , paddingXY 0 2
        , Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
        , Border.color colors.grays.light
        ]
    , card =
        [ width fill
        , paddingXY 24 36
        , spacing 48
        , Background.color colors.white
        , Border.rounded 6
        , Border.solid
        , Border.width 1
        , Border.color colors.grays.light
        , Border.shadow shadows.card
        , Font.color colors.grays.dark
        ]
    , button =
        [ paddingXY 24 8
        , Border.rounded 4
        , Border.shadow shadows.card
        , Background.color colors.coral
        , Font.color colors.white
        , Font.bold
        , Font.size 18
        , mouseOver [ alpha 0.8 ]
        ]
    }


fade : Float -> Attribute msg
fade ms =
    htmlAttribute
        (Html.Attributes.style "transition" <| "opacity " ++ String.fromFloat ms ++ "ms ease-in-out")



-- MAIN


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }



-- MODEL


type alias Model =
    { url : Url
    , key : Nav.Key
    , page : PageStatus
    }


type PageStatus
    = Loading
    | Leaving Page
    | Showing Page


type Page
    = SignIn
    | SignUp
    | Dashboard
    | NotFound


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model url key Loading
    , delay 300 (NavigateTo Dashboard)
    )


delay : Float -> Msg -> Cmd Msg
delay ms msg =
    Process.sleep ms
        |> Task.perform (always msg)


call : Msg -> Cmd Msg
call =
    delay 0



-- VIEW


view : Model -> Document Msg
view model =
    { title = title model
    , body =
        [ Element.layout
            [ Font.size 16
            , Font.family [ fonts.body ]
            , height fill
            , Background.color colors.grays.lighter
            ]
            (viewLayout model)
        ]
    }


title : Model -> String
title model =
    "Jangle"


viewLayout : Model -> Element Msg
viewLayout model =
    case model.page of
        Loading ->
            el [ width fill, height fill, fade 300, alpha 0 ] (text "")

        Leaving page ->
            el [ width fill, height fill, fade 300, alpha 0 ] (viewPage page)

        Showing page ->
            el [ width fill, height fill, fade 300, alpha 1 ] (viewPage page)


viewPage : Page -> Element Msg
viewPage page =
    case page of
        SignIn ->
            signInPage

        SignUp ->
            text "Sign up"

        NotFound ->
            text "Not found"

        Dashboard ->
            dashboardPage


signInPage : Element Msg
signInPage =
    el
        [ padding 16
        , centerX
        , centerY
        , width (fill |> maximum 320)
        ]
        signInForm


signInForm : Element Msg
signInForm =
    column classes.card
        [ el
            [ Font.size 56
            , Font.family [ fonts.heading ]
            , centerX
            ]
            (text "Jangle")
        , column [ spacing 24, width fill ]
            [ Input.email
                classes.input
                { onChange = always NoOp
                , text = ""
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] (text "Email")
                }
            , Input.currentPassword
                classes.input
                { onChange = always NoOp
                , text = ""
                , placeholder = Nothing
                , show = False
                , label = Input.labelAbove [ Font.bold ] (text "Password")
                }
            , Input.button (classes.button ++ [ alignRight ])
                { onPress = Just (NavigateTo Dashboard)
                , label = text "Sign in"
                }
            ]
        ]


dashboardPage : Element Msg
dashboardPage =
    column
        [ width fill, height fill ]
        [ navbar
        , column
            [ padding 16
            , spacing 16
            , width (fill |> maximum 720)
            , centerX
            ]
            (List.map (always cardExample) (List.range 1 5))
        ]


navbar : Element Msg
navbar =
    el
        [ paddingXY 0 16
        , width fill
        , Background.color colors.white
        , Border.shadow shadows.card
        , Font.size 24
        ]
    <|
        row
            [ paddingXY 16 0
            , width (fill |> maximum 720)
            , centerX
            ]
            [ el [ alignLeft, paddingEach { top = 8, left = 8, right = 8, bottom = 4 } ] icons.menu
            , el
                [ centerX
                , Font.family [ fonts.heading ]
                , Font.size 32
                ]
                (text "Jangle")
            , Input.button [ alignRight, paddingXY 8 4 ]
                { onPress = Just (NavigateTo SignIn)
                , label = icons.user
                }
            ]


cardExample : Element Msg
cardExample =
    column
        [ width fill
        , padding 16
        , spacing 4
        , Background.color colors.white
        , Border.solid
        , Border.width 1
        , Border.color colors.grays.light
        , Border.shadow shadows.card
        , Font.color colors.grays.dark
        ]
        [ el
            [ Font.size 24
            , Font.bold
            ]
            (text "Authors")
        , el [ Font.size 14, alpha 0.5 ] (text "123 items")
        ]



-- UPDATE


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | NavigateTo Page
    | FadeIn Page
    | ShowPage Page
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        NavigateTo page ->
            ( model
            , if urlOf page model.url /= model.url then
                Nav.pushUrl model.key (Url.toString (urlOf page model.url))

              else
                Nav.replaceUrl model.key (Url.toString (urlOf page model.url))
            )

        OnUrlRequest request ->
            case request of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        OnUrlChange url ->
            ( { model | url = url }
            , call (FadeIn (pageFrom url))
            )

        FadeIn page ->
            ( { model | page = fadeOut model.page }
            , delay 300 (ShowPage page)
            )

        ShowPage page ->
            ( { model | page = Showing page }
            , Cmd.none
            )


fadeOut : PageStatus -> PageStatus
fadeOut pageStatus =
    case pageStatus of
        Loading ->
            Loading

        Leaving page ->
            Leaving page

        Showing page ->
            Leaving page


urlOf : Page -> Url -> Url
urlOf page url =
    { url | path = pathOf page }


pathOf : Page -> String
pathOf page =
    "/"
        ++ (case page of
                Dashboard ->
                    ""

                SignIn ->
                    "sign-in"

                SignUp ->
                    "sign-up"

                NotFound ->
                    "not-found"
           )


pageFrom : Url -> Page
pageFrom url =
    case Parser.parse route url of
        Just page ->
            page

        Nothing ->
            NotFound


route : Parser (Page -> a) a
route =
    oneOf
        [ map Dashboard top
        , map SignIn (s "sign-in")
        , map SignUp (s "sign-up")
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
