module Pages.SignIn exposing (Model, Msg, init, update, view)

import Application
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Route
import Styles exposing (..)



-- MODEL


type alias Model =
    { email : String
    , password : String
    }


init : Model
init =
    Model "" ""



-- UPDATE


type Msg
    = UpdateField Field String
    | SignIn


type Field
    = Email
    | Password


update : Msg -> Model -> ( Model, Maybe Application.Msg )
update msg model =
    case msg of
        UpdateField Email email ->
            ( { model | email = email }
            , Nothing
            )

        UpdateField Password password ->
            ( { model | password = password }
            , Nothing
            )

        SignIn ->
            ( model
            , Just (Application.NavigateTo Route.Dashboard)
            )



-- VIEW


view : Model -> Element Msg
view model =
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
                { onChange = UpdateField Email
                , text = ""
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] (text "Email")
                }
            , Input.currentPassword
                classes.input
                { onChange = UpdateField Password
                , text = ""
                , placeholder = Nothing
                , show = False
                , label = Input.labelAbove [ Font.bold ] (text "Password")
                }
            , Input.button (classes.button ++ [ alignRight ])
                { onPress = Just SignIn
                , label = text "Sign in"
                }
            ]
        ]
