module Day3 exposing (parseClaim)

import Array exposing (Array)


type alias Claim =
    { id : Int
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


defaultClaim : Claim
defaultClaim =
    { id = 0, x = 0, y = 0, width = 0, height = 0 }


withId : String -> Claim -> Claim
withId str claim =
    case String.toInt str of
        Just num ->
            { claim | id = num }

        _ ->
            claim


withCoordinates : String -> Claim -> Claim
withCoordinates str claim =
    case String.split "," str of
        [ x, y ] ->
            { claim | x = parseWithDefault x, y = parseWithDefault y }

        _ ->
            claim


withDimensions : String -> Claim -> Claim
withDimensions str claim =
    case String.split "x" str of
        [ width, height ] ->
            { claim | width = parseWithDefault width, height = parseWithDefault height }

        _ ->
            claim


parseWithDefault : String -> Int
parseWithDefault str =
    Maybe.withDefault 0 <| String.toInt str


parseClaim : String -> Claim
parseClaim str =
    case
        str
            |> String.replace "#" ""
            |> String.replace "@" ""
            |> String.replace ":" ""
            |> String.words
    of
        [ id, coordinates, dimensions ] ->
            defaultClaim
                |> withId id
                |> withCoordinates coordinates
                |> withDimensions dimensions

        _ ->
            defaultClaim
