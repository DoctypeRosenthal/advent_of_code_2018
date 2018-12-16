module Tests.Day3 exposing (suite)

import Day3 exposing (claimId)
import Expect
import Set
import Test exposing (Test, describe, fuzz, test)


testClaims =
    """#1 @ 1,3: 4x4
       #2 @ 3,1: 4x4
       #3 @ 5,5: 2x2"""



{-
   ........
   ...2222.
   ...2222.
   .11XXXX3
   .11XXXX3
   .111X333
   .111X333
-}


testClaims2 =
    """#1 @ 1,3: 4x4
       #2 @ 3,1: 4x4
       #3 @ 4,3: 4x4
       #4 @ 7,3: 4x2"""


claimStr1 =
    "#1 @ 1,3: 4x4"


claim1 =
    { id = 1, x = 1, y = 3, width = 4, height = 4 }


claim2 =
    { id = 2, x = 3, y = 1, width = 4, height = 4 }


claimSet1 =
    Set.fromList
        [ ( 1, 3 )
        , ( 2, 3 )
        , ( 3, 3 )
        , ( 4, 3 )
        , ( 1, 4 )
        , ( 2, 4 )
        , ( 3, 4 )
        , ( 4, 4 )
        , ( 1, 5 )
        , ( 2, 5 )
        , ( 3, 5 )
        , ( 4, 5 )
        , ( 1, 6 )
        , ( 2, 6 )
        , ( 3, 6 )
        , ( 4, 6 )
        ]


suite : Test
suite =
    describe "Overlapping claims of the fabric for Santas coat"
        [ describe "Part1"
            [ test "Parses a claim correctly" <|
                \_ -> Expect.equal (Day3.parseClaim claimStr1) claim1
            , test "claim to set of points" <|
                \_ ->
                    Expect.equal (Day3.claimToPoints claim1) claimSet1
            , test "Intersection of claims" <|
                \_ ->
                    Expect.equal (Day3.claimsIntersect claim1 claim2) <| Set.fromList [ ( 3, 3 ), ( 4, 3 ), ( 3, 4 ), ( 4, 4 ) ]
            , test "Total number of intersection points across all claims" <|
                \_ ->
                    Expect.equal (Set.size <| Day3.overlapsBetweenClaims testClaims) 4
            , test "Total number of intersection points across different claims" <|
                \_ ->
                    Expect.equal (Set.size <| Day3.overlapsBetweenClaims testClaims2) 12
            ]
        , describe "Part2"
            [ test "Get Id of the first claim that has no intersections with the others" <|
                \_ ->
                    Expect.equal (Day3.getSeparateClaim testClaims |> claimId) (Just 3)
            ]
        ]
