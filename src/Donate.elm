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
    , selectedAmount : Maybe SelectedAmount
    , checkoutState : CheckoutState
    }


{-| A selected amount can be either a known valid value,
or a String that we must validate on submit.
-}
type SelectedAmount
    = FixedAmount Amount
    | CustomAmount String


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
    | ChangedDonationAmount SelectedAmount
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
                            validateOptions
                                { amount = model.selectedAmount
                                , frequency = model.selectedFrequency
                                }
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
    | InvalidAmount
    | MissingFrequency


validateOptions : { amount : Maybe SelectedAmount, frequency : Maybe Frequency } -> ValidationResult
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
                |> Result.andThen
                    (\existingAmount ->
                        case existingAmount of
                            FixedAmount amount ->
                                Ok amount

                            CustomAmount rawAmount ->
                                rawAmount
                                    |> String.toInt
                                    |> Maybe.map Amount.fromInt
                                    |> Result.fromMaybe InvalidAmount
                    )
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


amountOptions : List (RadioOption SelectedAmount)
amountOptions =
    [ { label = "$10", value = FixedAmount (Amount.fromInt 25) }
    , { label = "$25", value = FixedAmount (Amount.fromInt 25) }
    , { label = "$50", value = FixedAmount (Amount.fromInt 50) }

    -- The "selecting will reval input box" is one way to deal with
    -- WCAG SC 3.2.2 (On Input), which deals with predictable behaviour
    -- when the user interacts with an input. Conditionally showing/hiding
    -- a text box might be unexpected, so we provide instructions to mitigate that.
    -- Alternatively, we could opt to always make the custom input box visible;
    -- maybe that's ok as well!
    , { label = "Custom (selecting will reveal input box)", value = CustomAmount "" }
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
        frequencyName =
            "frequency"

        amountName =
            "amount"

        frequencyFirstId =
            getInputId frequencyName 0

        amountFirstId =
            getInputId amountName 0

        customAmountId =
            "amount-custom"
    in
    form [ HE.onSubmit UserPressedSubmit, HA.autocomplete False ]
        -- The error box should always be present (even if empty) in the DOM
        -- so that it is available when we focus it. This also allows Screen Reader
        -- announcements via ARIA Live Regions, though we do not use those at the moment.
        [ viewErrorBox
            { id = errorBoxId
            , frequencyId = frequencyFirstId
            , amountId = amountFirstId
            , customAmountId = customAmountId
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
                    { name = frequencyName, onItemSelect = ChangedDonationFrequency }
                    frequencyOptions
            )
        , fieldset []
            ((legend []
                [ h2 []
                    [ text "Amount"
                    ]
                ]
                :: viewRadioGroup
                    { name = amountName, onItemSelect = ChangedDonationAmount }
                    amountOptions
             )
                -- If the user has opted for a custom amount, show an input box
                -- right after it
                ++ [ case model.selectedAmount of
                        Just (CustomAmount amount) ->
                            div [ HA.class "custom-amount-input" ]
                                [ label [ HA.for customAmountId ]
                                    [ text "Custom amount (dollars)" ]
                                , input
                                    [ -- We avoid type number, because we validate ourselves
                                      -- @see also https://technology.blog.gov.uk/2020/02/24/why-the-gov-uk-design-system-team-changed-the-input-type-for-numbers/
                                      HA.type_ "text"
                                    , HA.id customAmountId
                                    , HA.placeholder "10"
                                    , HA.value amount

                                    -- Bring up a number keyboard on mobile
                                    , HA.attribute "inputmode" "numeric"
                                    , HE.onInput (ChangedDonationAmount << CustomAmount)
                                    ]
                                    []
                                ]

                        _ ->
                            text ""
                   ]
            )
        , button [] [ text "Go to checkout" ]
        ]


viewErrorBox : { id : String, frequencyId : String, amountId : String, customAmountId : String } -> CheckoutState -> Html msg
viewErrorBox { id, frequencyId, amountId, customAmountId } checkoutState =
    div [ HA.id id, HA.tabindex -1 ]
        (case checkoutState of
            Failed error ->
                [ div [ HA.class "error-box" ]
                    [ h2 []
                        [ text "Error" ]
                    , case error of
                        FailedValidation validationErrors ->
                            viewValidationErrors
                                { frequencyId = frequencyId
                                , amountId = amountId
                                , customAmountId = customAmountId
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
viewValidationErrors : { frequencyId : String, amountId : String, customAmountId : String } -> List ValidationError -> Html msg
viewValidationErrors { frequencyId, amountId, customAmountId } errors =
    let
        linkToError error =
            case error of
                MissingFrequency ->
                    a [ HA.href ("#" ++ frequencyId) ]
                        [ text "Please select a frequency." ]

                MissingAmount ->
                    a [ HA.href ("#" ++ amountId) ]
                        [ text "Please select an amount." ]

                InvalidAmount ->
                    a [ HA.href ("#" ++ customAmountId) ]
                        [ text "Fill in the custom amount as an integer, such as 5 and 10, but not a fraction, such as 10,5." ]
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
