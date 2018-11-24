module Styles exposing
    ( Colors
    , Fonts
    , Icons
    , Shadows
    , TransitionableProp(..)
    , classes
    , colors
    , fonts
    , icons
    , setAlpha
    , shadowWithOffset
    , shadows
    , transition
    , transitions
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes



-- COLORS


type alias Colors =
    { transparent : Color
    , white : Color
    , coral : Color
    , lightCoral : Color
    , grays :
        { lighter : Color
        , light : Color
        , dark : Color
        }
    }


colors : Colors
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


setAlpha : Float -> Element.Color -> Element.Color
setAlpha alpha =
    toRgb >> (\color -> { color | alpha = alpha }) >> fromRgb



-- FONTS


type alias Fonts =
    { heading : Font.Font
    , body : Font.Font
    }


fonts : Fonts
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



-- ICONS


type alias Icons msg =
    { menu : Element msg
    , user : Element msg
    , search : Element msg
    , signout : Element msg
    , list : Element msg
    , users : Element msg
    , media : Element msg
    , reorder : Element msg
    }


icons : Icons msg
icons =
    { menu = icon "menu"
    , user = icon "person"
    , search = icon "search"
    , signout = icon "log-out"
    , list = icon "list"
    , users = icon "people"
    , media = icon "images"
    , reorder = icon "reorder"
    }


icon : String -> Element msg
icon name =
    html (Html.span [ Html.Attributes.class ("icon ion-md-" ++ name) ] [])



-- SHADOWS


type alias Shadow =
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Element.Color
    }


type alias Shadows =
    { card : Shadow
    }


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
        , paddingXY 4 4
        , Border.width 1
        , spacing 8
        , Border.color colors.grays.light
        , Background.color colors.white
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
        , transition Opacity 200
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
        , transition Opacity 200
        ]
    }



-- STYLES


type TransitionableProp
    = Transform
    | Opacity
    | FontColor
    | BackgroundColor


nameOfProp : TransitionableProp -> String
nameOfProp prop =
    case prop of
        Transform ->
            "transform"

        Opacity ->
            "opacity"

        FontColor ->
            "color"

        BackgroundColor ->
            "background-color"


transition : TransitionableProp -> Float -> Attribute msg
transition prop ms =
    [ nameOfProp prop, String.fromFloat ms ++ "ms", "ease-in-out" ]
        |> String.join " "
        |> Html.Attributes.style "transition"
        |> htmlAttribute


transitions : List TransitionableProp -> Float -> Attribute msg
transitions props ms =
    props
        |> List.map nameOfProp
        |> List.map
            (\prop -> [ prop, String.fromFloat ms ++ "ms", "ease-in-out" ])
        |> List.map (String.join " ")
        |> String.join ", "
        |> Html.Attributes.style "transition"
        |> htmlAttribute
