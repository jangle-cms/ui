module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events as Events
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Html
import Html.Attributes
import Process
import Task exposing (Task)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s, top)



-- STYLES


colors =
    { transparent = rgba 0 0 0 0
    , white = rgb255 255 255 255
    , coral = rgb255 255 100 80
    , lightCoral = rgba255 255 100 80 0.15
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
    , search = icon "search"
    , signout = icon "log-out"
    , list = icon "list"
    , users = icon "people"
    , media = icon "images"
    }


icon : String -> Element msg
icon name =
    html (Html.span [ Html.Attributes.class ("icon ion-ios-" ++ name) ] [])


setAlpha : Float -> Element.Color -> Element.Color
setAlpha alpha =
    toRgb >> (\color -> { color | alpha = alpha }) >> fromRgb


shadowWithOffset offset =
    { offset = offset
    , size = 0
    , blur = 8
    , color = setAlpha 0.125 colors.grays.dark
    }


shadows =
    { card = shadowWithOffset ( 0, 4 )
    }


classes =
    { input =
        [ width fill
        , paddingXY 0 2
        , Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
        , Border.color colors.grays.light
        , Background.color colors.transparent
        ]
    , card =
        [ width fill
        , paddingEach { top = 36, left = 24, right = 24, bottom = 24 }
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
        [ paddingXY 24 12
        , Border.rounded 4
        , Border.shadow shadows.card
        , Background.color colors.coral
        , Font.color colors.white
        , Font.bold
        , Font.size 18
        , mouseOver [ alpha 0.8 ]
        ]
    , tinyButton =
        [ paddingXY 20 10
        , Border.rounded 4
        , Border.shadow shadows.card
        , Background.color colors.coral
        , Font.color colors.white
        , Font.bold
        , Font.size 14
        , mouseOver [ alpha 0.8 ]
        ]
    }


type TransitionableProp
    = Transform
    | Opacity


nameOfProp : TransitionableProp -> String
nameOfProp prop =
    case prop of
        Transform ->
            "transform"

        Opacity ->
            "opacity"


transition : TransitionableProp -> Float -> Attribute msg
transition prop ms =
    [ nameOfProp prop, String.fromFloat ms ++ "ms", "ease-in-out" ]
        |> String.join " "
        |> Html.Attributes.style "transition"
        |> htmlAttribute



-- MAIN


type alias Flags =
    { windowSize :
        { width : Int
        , height : Int
        }
    }


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
    , device : Device
    , page : PageStatus
    , isSidenavRevealed : Bool
    }


type PageStatus
    = Loading
    | Leaving Page
    | Showing Page


type Page
    = SignIn
    | SignUp
    | Dashboard
    | Item
    | NotFound


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model url
        key
        (classifyDevice flags.windowSize)
        Loading
        False
    , delay 300 (NavigateTo SignIn)
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
            ([ Font.size 16
             , Font.family [ fonts.body ]
             , height fill
             , Background.color colors.grays.lighter
             ]
                ++ (if model.device.class == Phone then
                        [ inFront (navbar (isNavVisible model.page))
                        , inFront (footer model.device (isNavVisible model.page))
                        ]

                    else
                        [ inFront (footer model.device (isNavVisible model.page)), inFront (sidenav (isNavVisible model.page)) ]
                   )
                ++ [ clipY
                   ]
            )
            (viewLayout model)
        ]
    }


isNavVisible : PageStatus -> Bool
isNavVisible =
    pageFromStatus
        >> Maybe.map shouldShowNavigation
        >> Maybe.withDefault False


pageFromStatus : PageStatus -> Maybe Page
pageFromStatus pageStatus =
    case pageStatus of
        Loading ->
            Nothing

        Leaving page ->
            Just page

        Showing page ->
            Just page


shouldShowNavigation : Page -> Bool
shouldShowNavigation page =
    case page of
        SignIn ->
            False

        SignUp ->
            False

        NotFound ->
            False

        Dashboard ->
            True

        Item ->
            True


title : Model -> String
title model =
    "Jangle"


viewLayout : Model -> Element Msg
viewLayout model =
    case model.page of
        Loading ->
            el [ width fill, height fill, transition Opacity 300, alpha 0 ] (text "")

        Leaving page ->
            el [ width fill, height fill, transition Opacity 300, alpha 0 ] (viewPage model.device page)

        Showing page ->
            el [ width fill, height fill, transition Opacity 300, alpha 1 ] (viewPage model.device page)


viewPage : Device -> Page -> Element Msg
viewPage device page =
    case page of
        SignIn ->
            signInPage

        SignUp ->
            text "Sign up"

        NotFound ->
            text "Not found"

        Item ->
            pageWithNavigation device itemPage

        Dashboard ->
            pageWithNavigation device dashboardPage


pageWithNavigation : Device -> Element msg -> Element msg
pageWithNavigation device =
    if device.class == Phone then
        el
            [ scrollbarY
            , paddingEach { top = 64, left = 0, right = 0, bottom = 72 }
            , width fill
            , height fill
            , Background.color colors.grays.lighter
            ]

    else
        el
            [ scrollbarY
            , paddingEach
                { top = 0
                , left = 240
                , right = 0
                , bottom = 72
                }
            , width fill
            , height fill
            , Background.color colors.grays.lighter
            ]


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
        [ width fill
        , height fill
        , spacing 0
        ]
        [ hero "Dashboard" "Welcome back, Ryan."
        , column
            [ width (fill |> maximum 640)
            , centerX
            , paddingXY 16 0
            ]
            [ el [ width fill, moveUp 30 ] searchbar
            , column
                [ width fill
                , height fill
                , spacing 12
                ]
                (List.map cardExample
                    [ ( "People", "10 people" )
                    , ( "Blog Posts", "52 posts" )
                    , ( "Offices", "4 locations" )
                    ]
                )
            ]
        ]


type Field
    = SingleLine
    | Html
    | Email
    | Relationship
    | Group NestedFields
    | Repeater NestedFields


type NestedFields
    = NestedFields Form


type alias Form =
    List ( String, Field )


form : List ( String, Field )
form =
    [ ( "Name"
      , Group
            (NestedFields
                [ ( "First Name", SingleLine )
                , ( "Middle Name", SingleLine )
                , ( "Last Name", SingleLine )
                ]
            )
      )
    , ( "Contact Information"
      , Group
            (NestedFields
                [ ( "Office", Relationship )
                , ( "Phone", SingleLine )
                , ( "Email", Email )
                ]
            )
      )
    , ( "Sections"
      , Repeater (NestedFields [ ( "Section", Html ) ])
      )
    ]


itemForm : Form -> Element Msg
itemForm form_ =
    column
        [ paddingXY 16 32
        , spacing 32
        , Font.size 18
        , width (fill |> maximum 640)
        , centerX
        ]
        (List.map viewField form)


viewField : ( String, Field ) -> Element Msg
viewField ( label, field ) =
    case field of
        SingleLine ->
            textField label

        Email ->
            Input.email classes.input
                { onChange = always NoOp
                , text = ""
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] (text label)
                }

        Html ->
            Input.multiline []
                { onChange = always NoOp
                , text = ""
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] (text label)
                , spellcheck = False
                }

        Relationship ->
            Input.search classes.input
                { onChange = always NoOp
                , text = ""
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] (text label)
                }

        Group (NestedFields fields) ->
            column [ spacing 24, width fill ] <|
                [ el [ Font.size 28, Font.bold ] (text label)
                , column [ spacing 16, width fill ]
                    (List.map (Lazy.lazy viewField) fields)
                ]

        Repeater (NestedFields fields) ->
            column [ spacing 24, width fill ] <|
                [ el [ Font.size 28, Font.bold ] (text label)
                , column [ spacing 16, width fill ]
                    (List.map (Lazy.lazy viewField) fields)
                , Input.button (classes.tinyButton ++ [ alignRight ]) { label = text "Add row", onPress = Nothing }
                ]


textField label =
    Input.text
        classes.input
        { onChange = always NoOp
        , text = ""
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.bold ] (text label)
        }


itemPage : Element Msg
itemPage =
    column
        [ width fill
        , height fill
        ]
        [ hero "New Person" "Created by Ryan."
        , itemForm form
        ]


hero : String -> String -> Element Msg
hero title_ caption =
    column
        [ Background.color colors.coral
        , Font.color colors.white
        , paddingXY 16 64
        , width fill
        , spacing 12
        ]
        [ el
            [ width fill
            , Font.center
            , Font.size 36
            , Font.bold
            ]
            (text title_)
        , el
            [ width fill
            , Font.center
            ]
            (text caption)
        ]


searchbar : Element Msg
searchbar =
    row
        [ width fill
        , Background.color colors.white
        , padding 12
        , Border.shadow shadows.card
        , spacing 8
        ]
        [ Input.search
            [ paddingXY 2 4
            , Border.width 0
            ]
            { label = Input.labelHidden "Search"
            , text = ""
            , onChange = always NoOp
            , placeholder = Just (Input.placeholder [ moveDown 4 ] (text "Search"))
            }
        , el [ alignRight, Font.size 24, padding 4 ] icons.search
        ]


sidenav : Bool -> Element Msg
sidenav isVisible =
    column
        [ width (px 240)
        , height fill
        , Background.color colors.white
        , Border.shadow shadows.card
        , moveLeft
            (if isVisible then
                0

             else
                240
            )
        , transition Transform 300
        ]
        [ Input.button [ width fill, paddingXY 16 24 ] { label = el [ centerX ] logo, onPress = Just (NavigateTo Dashboard) }
        , el [ height (fillPortion 1) ] (text "")
        , column [ width fill ]
            [ el [ width fill, Font.size 20, Font.bold, Font.color colors.coral, Background.color colors.lightCoral ] <|
                el [ spacing 8, width fill, paddingXY 16 12, Font.center ] (text "Content")
            , el [ width fill, Font.size 20, Font.bold, Font.color colors.grays.dark, Background.color colors.white ] <|
                el [ spacing 8, width fill, paddingXY 16 12, Font.center ] (text "Media")
            , el [ width fill, Font.size 20, Font.bold, Font.color colors.grays.dark, Background.color colors.white ] <|
                el [ spacing 8, width fill, paddingXY 16 12, Font.center ] (text "Users")
            ]
        , el [ height (fillPortion 2) ] (text "")
        , Input.button [ width fill, paddingXY 16 24 ]
            { label =
                row [ spacing 6, centerX ]
                    [ el [] icons.signout
                    , text "Sign out"
                    ]
            , onPress = Just (NavigateTo SignIn)
            }
        ]


navbar : Bool -> Element Msg
navbar isVisible =
    el
        [ paddingXY 0 16
        , width fill
        , Background.color colors.white
        , Border.shadow shadows.card
        , Font.size 16
        , moveUp
            (if isVisible then
                0

             else
                64
            )
        , transition Transform 300
        ]
    <|
        row
            [ paddingXY 16 0
            , width (fill |> maximum 720)
            , centerX
            ]
            [ Input.button [ Font.size 24, alignLeft ] { label = icons.menu, onPress = Just RevealSidenav }
            , Input.button [ centerX ] { label = logo, onPress = Just (NavigateTo Dashboard) }
            , Input.button [ Font.size 24, alignRight ] { label = icons.signout, onPress = Just (NavigateTo SignIn) }
            ]


logo : Element Msg
logo =
    el
        [ centerX
        , Font.family [ fonts.heading ]
        , Font.size 30
        ]
        (text "Jangle")


footer : Device -> Bool -> Element Msg
footer device isVisible =
    el
        [ alignBottom
        , width fill
        , Background.color colors.white
        , Border.shadow (shadowWithOffset ( 0, -4 ))
        , Font.size 16
        , paddingEach
            { top = 0
            , left =
                if device.class /= Phone then
                    240

                else
                    0
            , right = 0
            , bottom = 0
            }
        , moveDown
            (if isVisible then
                0

             else
                42 + 24
            )
        , transition Transform 300
        ]
    <|
        row
            [ paddingXY 16 0
            , width (fill |> maximum 720)
            , centerX
            ]
            [ button Item "Create"
            ]


button : Page -> String -> Element Msg
button page label =
    Input.button (classes.button ++ [ centerX, moveUp 20 ])
        { onPress = Just (NavigateTo page)
        , label = text label
        }


cardExample : ( String, String ) -> Element Msg
cardExample ( label, caption ) =
    column
        [ width fill
        , padding 16
        , spacing 4
        , Background.color colors.white
        , Border.solid
        , Border.color colors.grays.light
        , Border.shadow shadows.card
        , Font.color colors.grays.dark
        ]
        [ el
            [ Font.size 24
            , Font.bold
            ]
            (text label)
        , el [ Font.size 14, alpha 0.5 ] (text caption)
        ]



-- UPDATE


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | NavigateTo Page
    | FadeIn Page
    | ShowPage Page
    | WindowResize Int Int
    | RevealSidenav
    | HideSidenav
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        RevealSidenav ->
            ( { model | isSidenavRevealed = True }
            , Cmd.none
            )

        HideSidenav ->
            ( { model | isSidenavRevealed = False }
            , Cmd.none
            )

        WindowResize width height ->
            ( { model | device = classifyDevice { width = width, height = height } }
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

                Item ->
                    "lists/people/new"

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
        , map Item (s "lists" </> s "people" </> s "new")
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onResize WindowResize
