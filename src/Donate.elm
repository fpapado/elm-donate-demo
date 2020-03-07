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
    ( { selectedAmount = None
      , checkout = FillingDetails
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { selectedAmount : SelectedAmount
    , checkout : CheckoutState
    }


type SelectedAmount
    = None
    | Prefilled Amount
    | CustomAmount String


type CheckoutState
    = FillingDetails
    | FailedValidation String
    | Submitting
    | FailedToGetId Http.Error
    | FailedToRedirect String
    | Redirecting



-- Msg


type Msg
    = UserPressedSubmit
    | GotCheckoutIdResponse (Result Http.Error String)
    | GotStripeCheckoutFailure String



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- When the user presses sumbit, we validate their input
        -- and either expose an error or send to the API
        UserPressedSubmit ->
            case model.checkout of
                -- If the checkout is in process, do not allow the user to submit again
                -- (we could also do this in the onSubmit handler directly)
                Submitting ->
                    ( model, Cmd.none )

                _ ->
                    case model.selectedAmount of
                        None ->
                            ( { model | checkout = FailedValidation "Please select an amount first" }
                            , Cmd.none
                            )

                        Prefilled amount ->
                            ( { model | checkout = Submitting }
                            , Api.getCheckoutId GotCheckoutIdResponse amount Frequency.OneTime
                            )

                        CustomAmount int ->
                            Debug.todo "Handle custom amounts"

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



-- View


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
        [ fieldset []
            [ legend []
                [ h2 []
                    [ text "Amount"
                    ]
                ]
            , label [ HA.for "amount-0" ] [ text "25" ]
            , input [ HA.type_ "radio" ] []
            ]
        , button [] [ text "Go to checkout" ]
        ]
