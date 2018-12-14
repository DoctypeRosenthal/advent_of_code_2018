module Tests.Day2 exposing (suite)

import Day2
import Expect
import Fuzz exposing (string)
import Test exposing (Test, describe, fuzz, test)


testSet =
    [ ( "abcdef", ( False, False ) )
    , ( "bababc", ( True, True ) )
    , ( "abbcde", ( True, False ) )
    , ( "abcccd", ( False, True ) )
    , ( "aabcdd", ( True, False ) )
    , ( "abcdee", ( True, False ) )
    , ( "ababab", ( False, True ) )
    ]


suite : Test
suite =
    describe "Hashes of box ids"
        [ describe "Single box IDs and letter occurences"
            -- Nest as many descriptions as you like.
            (testSet
                |> List.map
                    (\( input, res ) ->
                        test input <|
                            \_ -> Expect.equal (Day2.hasMultiples input) res
                    )
            )
        , describe "The hash of all box ids"
            [ test "Hash from all" <|
                \_ -> Expect.equal (Day2.hash <| List.map Tuple.first testSet) 12
            ]
        ]
