module Amount exposing (Amount, fromInt, toInt)

-- NOTE: currently setting the Amount only as an integer, not fractionals
-- (though of course in JS it will be float anyway). This might be a bad
-- assumption


type Amount
    = Amount Int


fromInt : Int -> Amount
fromInt int =
    Amount int


toInt : Amount -> Int
toInt (Amount int) =
    int
