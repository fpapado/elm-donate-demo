port module Donate exposing (..)

import Amount exposing (Amount)
import Api
import Browser
import Frequency exposing (Frequency)
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Http



-- Ports to communicate with the host page


port checkout : String -> Cmd msg


port failure : (String -> msg) -> Sub msg



-- Main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> failure GotStripeCheckoutFailure
        }



-- Init


init : () -> ( Model, Cmd Msg )
init () =
    ( { selectedAmount = Nothing
      , selectedFrequency = Nothing
      , checkout = FillingDetails
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { selectedFrequency : Maybe Frequency
    , selectedAmount : Maybe Amount
    , checkout : CheckoutState
    }


type SelectedAmount
    = None
    | Prefilled Amount
    | CustomAmount String


type CheckoutState
    = FillingDetails
    | FailedValidation (List ValidationError)
    | Submitting
    | FailedToGetId Http.Error
    | FailedToRedirect String
    | Redirecting



-- Msg


type Msg
    = ChangedDonationFrequency Frequency
    | ChangedDonationAmount Amount
    | UserPressedSubmit
    | GotCheckoutIdResponse (Result Http.Error String)
    | GotStripeCheckoutFailure String



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedDonationFrequency frequency ->
            ( { model | selectedFrequency = Just frequency }, Cmd.none )

        ChangedDonationAmount amount ->
            ( { model | selectedAmount = Just amount }, Cmd.none )

        -- When the user presses sumbit, we validate their input
        -- and either expose an error or send to the API
        UserPressedSubmit ->
            case model.checkout of
                -- If the checkout is in process, do not allow the user to submit again
                -- (we could also do this in the onSubmit handler directly)
                Submitting ->
                    ( model, Cmd.none )

                _ ->
                    let
                        validationResult =
                            validateOptions { amount = model.selectedAmount, frequency = model.selectedFrequency }
                    in
                    case validationResult of
                        Err errors ->
                            ( { model | checkout = FailedValidation errors }
                            , Cmd.none
                            )

                        Ok { amount, frequency } ->
                            ( { model | checkout = Submitting }
                            , Api.getCheckoutId GotCheckoutIdResponse amount frequency
                            )

        GotCheckoutIdResponse res ->
            case res of
                -- If we got the checkout id, send it through the port
                Ok checkoutId ->
                    ( { model
                        | checkout = Redirecting
                      }
                    , checkout checkoutId
                    )

                -- If we failed to get the id, set the state with the error
                Err err ->
                    ( { model
                        | checkout = FailedToGetId err
                      }
                    , Cmd.none
                    )

        GotStripeCheckoutFailure err ->
            ( { model
                | checkout = FailedToRedirect err
              }
            , Cmd.none
            )



-- Form Validation


type alias ValidationResult =
    Result (List ValidationError) { amount : Amount, frequency : Frequency }


type ValidationError
    = MissingAmount
    | MissingFrequency


validateOptions : { amount : Maybe Amount, frequency : Maybe Frequency } -> ValidationResult
validateOptions rawInput =
    let
        parsedFrequency : Result ValidationError Frequency
        parsedFrequency =
            rawInput.frequency
                |> Result.fromMaybe MissingFrequency

        parsedAmount : Result ValidationError Amount
        parsedAmount =
            rawInput.amount
                |> Result.fromMaybe MissingAmount
    in
    -- There might be nicer ways to write this, perhaps with extra tagging
    -- but I'm not sure if that would guarantee both amount and frequency
    -- being present. A tuple, perhaps? Maybe not critical atm.
    case ( parsedAmount, parsedFrequency ) of
        ( Err x, Err y ) ->
            Err [ x, y ]

        ( Err x, Ok _ ) ->
            Err [ x ]

        ( Ok _, Err y ) ->
            Err [ y ]

        ( Ok amount, Ok frequency ) ->
            Ok { amount = amount, frequency = frequency }



-- View


frequencyOptions : List (RadioOption Frequency)
frequencyOptions =
    [ { label = "One time only", value = Frequency.OneTime }
    , { label = "Monthly", value = Frequency.Monthly }
    ]


amountOptions : List (RadioOption Amount)
amountOptions =
    [ { label = "25", value = Amount.fromInt 25 }
    , { label = "50", value = Amount.fromInt 50 }
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Donate"
    , body =
        [ main_ []
            [ h1 [] [ text "Donate" ]
            , viewForm model
            ]
        ]
    }


viewForm : Model -> Html Msg
viewForm model =
    form [ HE.onSubmit UserPressedSubmit ]
        -- Fieldsets help group radio buttons, and need legend to be exposed
        -- as a region to Assistive Technologies
        -- Using a heading (h2) in them is another affordance, if people want
        -- to jump to them
        [ case model.checkout of
            FailedValidation errors ->
                div []
                    [ h2 []
                        [ text "Error" ]
                    , ul
                        []
                        (List.map
                            (\error ->
                                li []
                                    [ text <|
                                        case error of
                                            MissingAmount ->
                                                "Please select an amount."

                                            MissingFrequency ->
                                                "Please select a frequency."
                                    ]
                            )
                            errors
                        )
                    ]
            _ -> text ""
        , fieldset []
            [ legend []
                [ h2 []
                    [ text "Frequency of donation"
                    ]
                ]
            , viewRadioGroup
                { name = "frequency", onItemSelect = ChangedDonationFrequency }
                frequencyOptions
            ]
        , fieldset []
            [ legend []
                [ h2 []
                    [ text "Amount"
                    ]
                ]
            , viewRadioGroup
                { name = "amount", onItemSelect = ChangedDonationAmount }
                amountOptions
            ]
        , button [] [ text "Go to checkout" ]
        ]


type alias RadioOption v =
    { label : String, value : v }


{-| Show a group of radio buttons, with correct ids and labels
-}
viewRadioGroup : { name : String, onItemSelect : value -> msg } -> List (RadioOption value) -> Html msg
viewRadioGroup { name, onItemSelect } items =
    div []
        (List.indexedMap
            (\index { label, value } ->
                viewRadioButton
                    { name = name
                    , id = name ++ "_" ++ String.fromInt index
                    , onClick = onItemSelect value
                    }
                    label
            )
            items
        )


viewRadioButton : { name : String, id : String, onClick : msg } -> String -> Html msg
viewRadioButton { name, id, onClick } content =
    div []
        [ input
            [ HA.type_ "radio"
            , HE.onClick onClick
            , HA.name name
            , HA.id id
            ]
            []
        , label [ HA.for id ] [ text content ]
        ]
