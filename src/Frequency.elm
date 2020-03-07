module Frequency exposing (Frequency(..), toString)


type Frequency
    = Monthly
    | OneTime


toString : Frequency -> String
toString frequency =
    case frequency of
        Monthly ->
            "monthly"

        OneTime ->
            "onetime"
