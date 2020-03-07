module Frequency exposing (Frequency(..), fromString, toString)


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


fromString : String -> Maybe Frequency
fromString str =
    case str of
        "monthly" ->
            Just Monthly

        "onetime" ->
            Just OneTime

        _ ->
            Nothing
