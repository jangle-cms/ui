module Main exposing (main)

import Application
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
import Pages.SignIn as SignIn
import Process
import Route exposing (Route)
import Styles exposing (..)
import Task exposing (Task)
import Tuple
import Url exposing (Url)



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
    = SignIn SignIn.Model
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
    , delay 300 (NavigateTo Route.SignIn)
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
                ++ viewLayoutOverlays model
                ++ [ clipY
                   ]
            )
            (viewLayout model)
        ]
    }


viewLayoutOverlays : Model -> List (Attribute Msg)
viewLayoutOverlays model =
    if model.device.class == Phone then
        [ inFront (navbar (isNavVisible model.page))
        , inFront (footer model.device (isNavVisible model.page))
        ]

    else
        [ inFront (footer model.device (isNavVisible model.page))
        , inFront (sidenav (isNavVisible model.page))
        ]


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
        SignIn _ ->
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
            el
                [ width fill
                , height fill
                , transition Opacity 300
                , alpha 0
                ]
                (text "")

        Leaving page ->
            el
                [ width fill
                , height fill
                , transition Opacity 300
                , alpha 0
                ]
                (viewPage model.device page)

        Showing page ->
            el
                [ width fill
                , height fill
                , transition Opacity 300
                , alpha 1
                ]
                (viewPage model.device page)


viewPage : Device -> Page -> Element Msg
viewPage device page =
    case page of
        SignIn model ->
            SignIn.view model
                |> Element.map (SignInMsg model)

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


dashboardPage : Element Msg
dashboardPage =
    column
        [ width fill
        , height fill
        , spacing 0
        ]
        [ hero "Dashboard" "Welcome back."
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
                (List.map listCard
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
      , Group <|
            NestedFields
                [ ( "First Name", SingleLine )
                , ( "Middle Name", SingleLine )
                , ( "Last Name", SingleLine )
                ]
      )
    , ( "Contact Information"
      , Group <|
            NestedFields
                [ ( "Office", Relationship )
                , ( "Phone", SingleLine )
                , ( "Email", Email )
                ]
      )
    , ( "Bio"
      , Group <|
            NestedFields
                [ ( "Main Bio", Html )
                , ( "Additional Bio", Html )
                ]
      )
    , ( "Sections"
      , Repeater <| NestedFields [ ( "Section", Html ) ]
      )
    ]


itemForm : Form -> Element Msg
itemForm form_ =
    column
        [ paddingXY 48 32
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
            Input.multiline (classes.input ++ [ height (px 180) ])
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
            viewGroupFields label fields

        Repeater (NestedFields fields) ->
            column [ spacing 24, width fill ] <|
                [ el [ Font.size 28, Font.bold ] (text label)
                , column [ spacing 16, width fill ]
                    (List.map
                        viewRepeatedField
                        fields
                    )
                , Input.button (classes.tinyButton ++ [ alignRight ])
                    { label = text "Add row", onPress = Nothing }
                ]


viewRepeatedField : ( String, Field ) -> Element Msg
viewRepeatedField pair =
    el
        [ width fill
        , spacing 12
        , onLeft
            (el
                [ height fill
                , paddingEach { top = 0, left = 0, right = 8, bottom = 0 }
                ]
                (Input.button
                    [ Font.size 24
                    , Font.color colors.grays.light
                    , mouseOver
                        [ Background.color colors.lightCoral
                        , Font.color colors.coral
                        ]
                    , transitions [ FontColor, BackgroundColor ] 300
                    , height fill
                    , paddingXY 8 0
                    ]
                    { label = el [ centerY ] icons.reorder
                    , onPress = Nothing
                    }
                )
            )
        ]
        (Lazy.lazy viewField pair)


viewGroupFields : String -> Form -> Element Msg
viewGroupFields label fields =
    column [ spacing 24, width fill ] <|
        [ el [ Font.size 28, Font.bold ] (text label)
        , column [ spacing 16, width fill ]
            (List.map (Lazy.lazy viewField) fields)
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
            , placeholder =
                Just (Input.placeholder [ moveDown 4 ] (text "Search"))
            }
        , el
            [ alignRight
            , Font.size 24
            , padding 4
            , Font.color colors.grays.light
            ]
            icons.search
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
        [ Input.button
            [ width fill, paddingXY 16 24 ]
            { label = el [ centerX ] logo
            , onPress = Just (NavigateTo Route.Dashboard)
            }
        , el [ height (fillPortion 1) ] (text "")
        , column [ width fill ]
            [ sidenavLink True "Content"
            , sidenavLink False "Media"
            , sidenavLink False "Users"
            ]
        , el [ height (fillPortion 2) ] (text "")
        , Input.button
            [ width fill
            , paddingXY 16 24
            , mouseOver [ Font.color colors.coral ]
            , transition FontColor 300
            ]
            { label =
                row [ spacing 6, centerX ]
                    [ el [] icons.signout
                    , text "Sign out"
                    ]
            , onPress = Just (NavigateTo Route.SignIn)
            }
        ]


sidenavLink : Bool -> String -> Element Msg
sidenavLink isActive label =
    Input.button
        [ width fill
        , Font.size 20
        , Font.bold
        , Font.color <|
            if isActive then
                colors.coral

            else
                colors.grays.dark
        , Background.color <|
            if isActive then
                colors.lightCoral

            else
                colors.white
        , mouseOver
            [ Font.color colors.coral
            ]
        , transitions [ FontColor, BackgroundColor ] 300
        ]
        { label =
            el
                [ spacing 8
                , width fill
                , paddingXY 16 12
                , Font.center
                ]
                (text label)
        , onPress = Nothing
        }


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
            [ Input.button
                [ Font.size 24, alignLeft ]
                { label = icons.menu, onPress = Just RevealSidenav }
            , Input.button
                [ centerX ]
                { label = logo, onPress = Just (NavigateTo Route.Dashboard) }
            , Input.button
                [ Font.size 24, alignRight ]
                { label = icons.signout, onPress = Just (NavigateTo Route.SignIn) }
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
            [ button (Route.NewItem "people") "Create"
            ]


button : Route -> String -> Element Msg
button route label =
    Input.button (classes.button ++ [ centerX, moveUp 20 ])
        { onPress = Just (NavigateTo route)
        , label = text label
        }


listCard : ( String, String ) -> Element Msg
listCard ( label, caption ) =
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
        , el
            [ Font.size 14
            , alpha 0.5
            ]
            (text caption)
        ]


itemCard : ( String, String ) -> Element Msg
itemCard ( label, caption ) =
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
        , el
            [ Font.size 14
            , alpha 0.5
            , Background.color colors.coral
            , Font.color colors.white
            , Border.rounded 12
            , paddingXY 10 4
            ]
            (text caption)
        ]



-- UPDATE


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | NavigateTo Route.Route
    | FadeIn Page
    | ShowPage Page
    | WindowResize Int Int
    | RevealSidenav
    | HideSidenav
    | NoOp
    | SignInMsg SignIn.Model SignIn.Msg
    | AppMsg Application.Msg


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
            ( { model
                | device =
                    classifyDevice
                        { width = width, height = height }
              }
            , Cmd.none
            )

        NavigateTo page ->
            ( model
            , if Route.urlOf page model.url /= model.url then
                Nav.pushUrl model.key (Url.toString (Route.urlOf page model.url))

              else
                Nav.replaceUrl model.key (Url.toString (Route.urlOf page model.url))
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
            let
                route =
                    Route.pageFrom url
            in
            ( { model | url = url }
            , call (FadeIn (initPage route))
            )

        FadeIn page ->
            ( { model | page = fadeOut model.page }
            , delay 300 (ShowPage page)
            )

        ShowPage page ->
            ( { model | page = Showing page }
            , Cmd.none
            )

        SignInMsg pageModel pageMsg ->
            SignIn.update pageMsg pageModel
                |> pageTime SignIn model

        AppMsg appMsg ->
            case appMsg of
                Application.NavigateTo route ->
                    ( model
                    , invoke (NavigateTo route)
                    )


pageTime :
    (pageModel -> Page)
    -> Model
    -> ( pageModel, Maybe Application.Msg )
    -> ( Model, Cmd Msg )
pageTime toPage model =
    Tuple.mapBoth
        (\pageModel -> { model | page = Showing (toPage pageModel) })
        appCommand


appCommand : Maybe Application.Msg -> Cmd Msg
appCommand appMsg =
    case appMsg of
        Just msg_ ->
            invoke (AppMsg msg_)

        Nothing ->
            Cmd.none


invoke : msg -> Cmd msg
invoke msg =
    Task.perform (always msg) (Task.succeed msg)


initPage : Route -> Page
initPage route =
    case route of
        Route.SignIn ->
            SignIn SignIn.init

        Route.Dashboard ->
            Dashboard

        Route.NewItem slug ->
            Item

        Route.NotFound ->
            NotFound


fadeOut : PageStatus -> PageStatus
fadeOut pageStatus =
    case pageStatus of
        Loading ->
            Loading

        Leaving page ->
            Leaving page

        Showing page ->
            Leaving page



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onResize WindowResize
