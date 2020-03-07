module Api exposing (getCheckoutId)

import Amount exposing (Amount)
import Frequency exposing (Frequency)
import Http
import Url.Builder exposing (int, string)


baseUrl : String
baseUrl =
    "https://localhost:5000"


{-| Create a URL of the form
<https://worker.elm-lang.org/donate?amount=10&frequency=monthly>
<https://worker.elm-lang.org/donate?amount=25&frequency=onetime>
<https://worker.elm-lang.org/donate?amount=50&frequency=onetime>
-}
donateUrl : Amount -> Frequency -> String
donateUrl amount frequency =
    Url.Builder.crossOrigin baseUrl
        [ "donate" ]
        [ int "amount" (Amount.toInt amount)
        , string "frequency" (Frequency.toString frequency)
        ]


getCheckoutId : (Result Http.Error String -> msg) -> Amount -> Frequency -> Cmd msg
getCheckoutId toMsg amount frequency =
    Http.get
        { url = donateUrl amount frequency
        , expect = Http.expectString toMsg
        }
