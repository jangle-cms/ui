module Application exposing (Model, Msg(..), PageMsg(..))

import Route exposing (Route)


type alias Model =
    { user : Maybe User
    }


type alias User =
    { name : String
    }


type Msg
    = NavigateTo Route


type PageMsg pageMsg
    = Local pageMsg
    | Global Msg
