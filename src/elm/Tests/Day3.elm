module Tests.Day3 exposing (suite)

import Day3
import Expect
import Test exposing (Test, describe, fuzz, test)


testClaims =
    """#1 @ 1,3: 4x4
       #2 @ 3,1: 4x4
       #3 @ 5,5: 2x2"""


aClaim =
    "#1 @ 1,3: 4x4"


suite : Test
suite =
    describe "Overlapping claims of the fabric for Santas coat"
        [ describe "Part1"
            [ test "Parses a claim correctly" <|
                \_ -> Expect.equal (Day3.parseClaim aClaim) { id = 1, x = 1, y = 3, width = 4, height = 4 }
            ]
        ]
