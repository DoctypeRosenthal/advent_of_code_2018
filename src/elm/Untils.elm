module Untils exposing (cycleLeft)

{-| Move the first element of a list to its end
-}


cycleLeft : List a -> List a
cycleLeft list =
    case list of
        head :: tail ->
            tail ++ [ head ]

        _ ->
            list
