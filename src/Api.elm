module Api exposing (..)

import Amount exposing (Amount)
import Frequency exposing (Frequency)
import Http
import Url.Builder exposing (int, string)


baseUrl : String
baseUrl =
    "https://worker.elm-lang.org"


{-| Create a URL of the form
<https://worker.elm-lang.org/donate?amount=10&frequency=monthly>
<https://worker.elm-lang.org/donate?amount=25&frequency=onetime>
<https://worker.elm-lang.org/donate?amount=50&frequency=onetime>
-}
donateUrl : Amount -> Frequency -> String
donateUrl =
    UrlBuilder.crossOrigin baseUrl
        [ "donate" ]
        [ int (Amount.toInt amount)
        , string (Frequency.toString frequency)
        ]


getCheckoutId : msg -> Amount -> Frequency -> Cmd msg
getCheckoutId toMsg amount frequency =
    let
        url =
            donateUrl amount frequency
    in
    Http.get url
