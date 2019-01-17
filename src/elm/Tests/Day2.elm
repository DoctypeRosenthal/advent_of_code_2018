module Tests.Day2 exposing (suite)

import Day2


testSet =
    [ ( "abcdef", ( False, False ) )
    , ( "bababc", ( True, True ) )
    , ( "abbcde", ( True, False ) )
    , ( "abcccd", ( False, True ) )
    , ( "aabcdd", ( True, False ) )
    , ( "abcdee", ( True, False ) )
    , ( "ababab", ( False, True ) )
    ]


testStr =
    """abcde
      fghij
      klmno
      pqrst
      fguij
      axcye
      wvxyz"""


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
        , describe "Part 2"
            [ test "Corresponding box ids" <|
                \_ -> Expect.equal (Day2.strDiffIndices "fghij" "fguij") [ 2 ]
            , test "Non-corresponding box ids" <|
                \_ -> Expect.equal (Day2.strDiffIndices "abcde" "axcye") [ 3, 1 ]
            , test "Common letters between box ids" <|
                \_ -> Expect.equal (Day2.commonLettersBetween "fghij" "fguij") "fgij"
            , test "List of corresponding box ids" <|
                \_ -> Expect.equal (Day2.getCorrespondingIds testStr) [ ( "fguij", "fghij", 2 ) ]
            , test "List of intersections of corresponding box ids" <|
                \_ -> Expect.equal (Day2.intersectionsOfBoxIds testStr) [ "fgij" ]
            ]
        ]
