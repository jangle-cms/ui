module Route exposing (Route(..), pageFrom, route, urlOf)

import Url exposing (Url)
import Url.Parser as Parser
    exposing
        ( (</>)
        , Parser
        , map
        , oneOf
        , s
        , string
        , top
        )


type alias ListSlug =
    String


type Route
    = SignIn
    | Dashboard
    | NewItem ListSlug
    | NotFound


urlOf : Route -> Url -> Url
urlOf route_ url =
    { url | path = pathOf route_ }


pathOf : Route -> String
pathOf route_ =
    "/"
        ++ String.join "/"
            (case route_ of
                Dashboard ->
                    []

                NewItem listSlug ->
                    [ "lists", listSlug, "new" ]

                SignIn ->
                    [ "sign-in" ]

                NotFound ->
                    [ "not-found" ]
            )


pageFrom : Url -> Route
pageFrom url =
    case Parser.parse route url of
        Just route_ ->
            route_

        Nothing ->
            NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ map Dashboard top
        , map SignIn (s "sign-in")
        , map NewItem (s "lists" </> string </> s "new")
        ]
