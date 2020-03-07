port module Donate exposing (..)

import Amount exposing (Amount)
import Api
import Browser
import Browser.Dom as Dom
import Frequency exposing (Frequency)
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Process
import Task



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
      , checkoutState = FillingDetails
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { selectedFrequency : Maybe Frequency
    , selectedAmount : Maybe Amount
    , checkoutState : CheckoutState
    }


type CheckoutState
    = FillingDetails
    | Submitting
    | Failed CheckoutError
    | Redirecting


type CheckoutError
    = FailedValidation (List ValidationError)
    | FailedToGetId Http.Error
    | FailedToRedirect String



-- Msg


type Msg
    = ChangedDonationFrequency Frequency
    | ChangedDonationAmount Amount
    | UserPressedSubmit
    | GotCheckoutIdResponse (Result Http.Error String)
    | GotStripeCheckoutFailure String
    | NoOp



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
            case model.checkoutState of
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
                            ( { model | checkoutState = Failed (FailedValidation errors) }
                            , focusErrorBox
                            )

                        Ok { amount, frequency } ->
                            ( { model | checkoutState = Submitting }
                            , Api.getCheckoutId GotCheckoutIdResponse amount frequency
                            )

        GotCheckoutIdResponse res ->
            case res of
                -- If we got the checkout id, send it through the port
                Ok checkoutId ->
                    ( { model | checkoutState = Redirecting }
                    , checkout checkoutId
                    )

                -- If we failed to get the id, set the state with the error
                Err err ->
                    ( { model | checkoutState = Failed (FailedToGetId err) }
                    , focusErrorBox
                    )

        GotStripeCheckoutFailure err ->
            ( { model | checkoutState = Failed (FailedToRedirect err) }
            , focusErrorBox
            )

        NoOp ->
            ( model, Cmd.none )


{-| Focus the form's error box, after a timeout.
The timeout is important to let a Screen Reader's virtual buffer catch up.
-}
focusErrorBox : Cmd Msg
focusErrorBox =
    Process.sleep 500
        |> Task.andThen (\_ -> Dom.focus errorBoxId)
        |> Task.attempt (\_ -> NoOp)



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


{-| We need a unique error box id to be able to focus it
on errors
-}
errorBoxId : String
errorBoxId =
    "checkout-error"


frequencyOptions : List (RadioOption Frequency)
frequencyOptions =
    [ { label = "One time only", value = Frequency.OneTime }
    , { label = "Monthly", value = Frequency.Monthly }
    ]


amountOptions : List (RadioOption Amount)
amountOptions =
    [ { label = "$25", value = Amount.fromInt 25 }
    , { label = "$50", value = Amount.fromInt 50 }
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
    let
        frequencyInputName =
            "frequency"

        amountInputName =
            "amount"
    in
    form [ HE.onSubmit UserPressedSubmit ]
        -- The error box should always be present (even if empty) in the DOM
        -- so that it is available when we focus it. This also allows Screen Reader
        -- announcements via ARIA Live Regions, though we do not use those at the moment.
        [ viewErrorBox
            { id = errorBoxId
            , frequencyInputName = frequencyInputName
            , amountInputName = amountInputName
            }
            model.checkoutState

        -- Fieldsets help group radio buttons, and need legend to be exposed
        -- as a region to Assistive Technologies
        -- Using a heading (h2) in them is another affordance, if people want
        -- to jump to them
        , fieldset []
            (legend []
                [ h2 []
                    [ text "Frequency of donation"
                    ]
                ]
                :: viewRadioGroup
                    { name = "frequency", onItemSelect = ChangedDonationFrequency }
                    frequencyOptions
            )
        , fieldset []
            (legend []
                [ h2 []
                    [ text "Amount"
                    ]
                ]
                :: viewRadioGroup
                    { name = "amount", onItemSelect = ChangedDonationAmount }
                    amountOptions
            )
        , button [] [ text "Go to checkout" ]
        ]


viewErrorBox : { id : String, frequencyInputName : String, amountInputName : String } -> CheckoutState -> Html msg
viewErrorBox { id, frequencyInputName, amountInputName } checkoutState =
    div [ HA.id id, HA.tabindex -1 ]
        (case checkoutState of
            Failed error ->
                [ div [ HA.class "error-box" ]
                    [ h2 []
                        [ text "Error" ]
                    , case error of
                        FailedValidation validationErrors ->
                            viewValidationErrors
                                { frequencyInputName = frequencyInputName
                                , amountInputName = amountInputName
                                }
                                validationErrors

                        FailedToGetId str ->
                            -- TODO: Add a more actionabl error here. Should the user retry?
                            p [] [ text "We could not get the checkout id. This happens because we do not have the API wired up." ]

                        FailedToRedirect str ->
                            p [] [ text str ]
                    ]
                ]

            _ ->
                []
        )


{-| Validation errors are a list of errors, with links to the respective field.
This allows users to jump to the relevant error, without guessing.
In the case of radios/checkboxes, we link to the first
The GDS has guidance on error summaries, such as this one.
@see <https://design-system.service.gov.uk/components/error-summary/>
-}
viewValidationErrors : { frequencyInputName : String, amountInputName : String } -> List ValidationError -> Html msg
viewValidationErrors { frequencyInputName, amountInputName } errors =
    let
        linkToError error =
            case error of
                MissingAmount ->
                    a [ HA.href ("#" ++ getInputId amountInputName 0) ]
                        [ text "Please select an amount." ]

                MissingFrequency ->
                    a [ HA.href ("#" ++ getInputId frequencyInputName 0) ]
                        [ text "Please select a frequency." ]
    in
    ul
        []
        (List.map
            (\error ->
                li [] [ linkToError error ]
            )
            errors
        )



-- Radio Groups


type alias RadioOption v =
    { label : String
    , value : v
    }


{-| Show a group of radio buttons, with correct ids and labels
-}
viewRadioGroup : { name : String, onItemSelect : value -> msg } -> List (RadioOption value) -> List (Html msg)
viewRadioGroup { name, onItemSelect } items =
    List.indexedMap
        (\index { label, value } ->
            viewRadioButton
                { name = name
                , id = getInputId name index
                , onClick = onItemSelect value
                }
                label
        )
        items


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


{-| Helper to get an input's id.
We want to standardise this, because we use names for linking to inputs from the error box!
-}
getInputId : String -> Int -> String
getInputId base index =
    base ++ "_" ++ String.fromInt index
