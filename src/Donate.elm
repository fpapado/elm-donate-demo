port module Donate exposing (..)

import Amount
import Api
import Frequency
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE



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
        , subscriptions = failure GotStripeCheckoutFailure
        }



-- Init
-- Model


type alias Model =
    { selectedAmount : SelectedAmount
    , checkout : CheckoutState
    }


type SelectedAmount
    = Prefilled Amount
    | CustomAmount String


type CheckoutState
    = FillingDetails
    | FailedValidation String
    | Submitting
    | Failed String



-- Msg


type Msg
    = PressedSubmit
    | GotCheckoutIdResponse Result
    | GotStripeCheckoutFailure



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- When the user presses sumbit, we validate their input
        -- and either expose an error or send to the API
        PressedSubmit ->
            ( model, Cmd.none )

        GotCheckoutIdResponse res ->
            ( model, Cmd.none )

        GotStripeCheckoutFailure ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    main []
        [ h1 [] [ text "Donate" ]
        , viewForm
        ]


viewForm : Model -> Html Msg
viewForm =
    form []
        [ fieldset []
            [ legend []
                [ h2 []
                    [ text "Amount"
                    ]
                ]
            , label [ HA.for "amount-0" ] [ text "25" ]
            , input [ HA.type_ "radio" ] []
            ]
        ]
