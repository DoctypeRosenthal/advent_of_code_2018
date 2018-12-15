module Day2 exposing (answer1, commonLettersBetween, getCorrespondingIds, hasMultiples, hash, intersectionsOfBoxIds, part1, part2, question, strDiffIndices)


type alias HasDoubles =
    Bool


type alias HasTriples =
    Bool


occurance : Char -> List Char -> Int
occurance needle haystack =
    haystack
        |> List.filter (\x -> x == needle)
        |> List.length


contains : Int -> String -> Bool
contains count haystack =
    let
        charList =
            String.toList haystack
    in
    charList
        |> List.any (\x -> occurance x charList == count)


hasMultiples : String -> ( HasDoubles, HasTriples )
hasMultiples inStr =
    let
        hasDouble =
            contains 2 inStr

        hasTriple =
            contains 3 inStr
    in
    ( hasDouble, hasTriple )


hash : List String -> Int
hash list =
    let
        countTrue =
            List.filter (\x -> x) >> List.length

        ( doubles, triples ) =
            list
                |> List.map hasMultiples
                |> List.unzip
                |> Tuple.mapBoth countTrue countTrue
    in
    doubles * triples


answer1 : String -> Int
answer1 str =
    hash <| String.split "\n" str


question : String
question =
    """icxjubroqtunzeyzpomfksahgw
        ibxjvbroqtunleyzjdmfksahow
        icxjvbroqtinleyzpdmflsahnw
        icxjvbnoqtunleyvpgmfksahgw
        wcxjvbroqrunleyzpdmfksahge
        icxjtbroqtjzleyzpdmfksahgw
        icxjvbrohtunleyzpdmfkbahsw
        xcxjvbroqcunleyzpdmfksaxgw
        ycxjvbroqtunleyzpowfksahgw
        icxfvbroqtunleyzpdmfksncgw
        ixxjvbuoqtunleyzpdvfksahgw
        icfjvbroqtunleyzpdmfksadgt
        icxjvbroqdunleyzpdafksqhgw
        icxjvbrobtunlelzpdmfkuahgs
        ujxjvbroqtunleyzpdmqksahgw
        icqjvsroqtunleyzpdmfksahuw
        icxjvbroptpnleyzpdmfksangw
        ipxjvbroqtunleyzpdmfesahgi
        icajvbroqtunltyzpdqfksahgw
        ickjvbroqtuzleyzpdmfgsahgw
        icxjvbroqtunledzpdmwksahgz
        icxjvlroqtsnleyzpdmfksvhgw
        icxjvbroqtunleyzpdsfysahvw
        icxjvbroqtunwnyzydmfksahgw
        ionjvbroqtunleyzpdmfksahgj
        icxjvwriqtunleyzpdmfksahgi
        ocxjvbroztunleyzpdmfksapgw
        icxjvbroqtmnlewzpumfksahgw
        ucxjvbroqtunleyzpdmzktahgw
        icxgvbroqtunleyztdmfktahgw
        icxhvbroqttnleybpdmfksahgw
        icxjvbroqtugleyzpdxfkeahgw
        acxjvbroqvunlexzpdmfksahgw
        icxjvbroqglnleyzpbmfksahgw
        icxjvbriqtnvleyzpdmfksahgw
        icxjvbreqtunlakzpdmfksahgw
        gcxjvbuoqtunleyzpdmfksawgw
        icxjvbroqtunleyzpddfkyzhgw
        icxjvbjoqtunleyzpdmfqsahhw
        icxjvjroqtunleyzpnmfksajgw
        ycxjvbroqtunmeyzcdmfksahgw
        irxkvbryqtunleyzpdmfksahgw
        isxjvbrlqtunleyzpdmsksahgw
        icxjvbcoqtunleyzpdfkksahgw
        ixnjvbroqtunleyzpdmfkqahgw
        wcxjvbroqhunleyzqdmfksahgw
        icljvurmqtunleyzpdmfksahgw
        ibxjvbroqtayleyzpdmfksahgw
        arxjvbroqiunleyzpdmfksahgw
        iuxjvbroqtunluyzpdmoksahgw
        icxjvbrmqtunleyzpdmfosahew
        isxjvbroqtunleyrpdmfksrhgw
        icxjvxrpqtunleyzpdmfkdahgw
        ichjvbrogtunllyzpdmfksahgw
        icxjvbeoqtunlryzpdmfksakgw
        icxjvbroqtcnemyzpdmfksahgw
        icxjvbroqtybledzpdmfksahgw
        icxjvbrqqtunleyzpdmfksgngw
        icgjvbroqtunleyzmdmfksabgw
        icxjtbroqtunleyzkdmfksahww
        icxjvbfoqtunleyzpamfqsahgw
        icxjvbroknuyleyzpdmfksahgw
        icxjvbroqtujleyzpdmaksaigw
        icxjvbroqtnnlmyzpdmflsahgw
        icxjvbroqtunlefzpdmfsfahgw
        icxjvdroqtusleyzpdzfksahgw
        icxjvbioqtunlsyzpdmfkshhgw
        icxbvbrodtunleyzpdmoksahgw
        icxjvbroqtuvleyzpdmfkbahmw
        iyxjvbroqtunljvzpdmfksahgw
        icxjvbroqtudleynddmfksahgw
        icxjvwroqtnnleyzpdmfksahgz
        ichjvbroqtunleyzjdmeksahgw
        icxjvbrostunluyrpdmfksahgw
        icfjvbroqtunleyxpdgfksahgw
        nhxjvbroqtunlerzpdmfksahgw
        icxjvbrothunlexzpdmfksahgw
        icxjvbrzltqnleyzpdmfksahgw
        icxjvbrhqtunleyzpdmfksajgy
        vcxjvjroqiunleyzpdmfksahgw
        icxjfbroltunleyzpdmqksahgw
        icxbvbroqtunleyzpdofasahgw
        icxjvbkoqtunveyzpdmfksaqgw
        icxsebroqtunleyzpdmuksahgw
        icxjvbroquunlpyrpdmfksahgw
        icxhvbroqtunjeyzpdmrksahgw
        icdjvbroqtunlzyzpdmfksangw
        jcxqvbroqtvnleyzpdmfksahgw
        icxjvxroqtunleyrpdmfxsahgw
        icxjvnroqtunleyzpdmfssyhgw
        icxjvbraptunleyzpdofksahgw
        icxjvbroatunleyjpdmfbsahgw
        icxjvbroytlnlryzpdmfksahgw
        iaxjvbroqkunleyzpdmfcsahgw
        ucxjvbroqtuileyzzdmfksahgw
        icxjqbroqtcnleyzpgmfksahgw
        icxjvbloqtunleyzadmfksaqgw
        icxjvbroqtunleyzkdmnksakgw
        icxjvbroqtunleyjpdxfksahvw
        iqxjvbroqtujleyzpdmfklahgw
        icgjvbroqtunleyzpdmfksbhgb
        icxjzbroqtunleyzpdmfkdahgg
        icxjvbrobtunloywpdmfksahgw
        icxavbroqtunleyfpdmfksahgd
        icxjvbroqtunleyophmfksahkw
        icxjndroqtunlyyzpdmfksahgw
        icxjvbroqtjnleyppdmvksahgw
        icxjvbroonfnleyzpdmfksahgw
        icxjvbrqqtlnljyzpdmfksahgw
        icxjvbrzqtunlelspdmfksahgw
        icxjvbooqtunleyztdmfkfahgw
        icajvbroltunlnyzpdmfksahgw
        icxjvbroqtunleyzidmdkschgw
        icxjvbroktupleyzpdmfksahyw
        icxjcbroyeunleyzpdmfksahgw
        icxjvbroqtunlezkpdmfksahsw
        icxjvbroqtunlejzpcmfksrhgw
        icxjvvroqtunlsyzkdmfksahgw
        icxjnbroqtunbeyzpdmfpsahgw
        itxjbbroqtunleyzpemfksahgw
        icxjvbroqtunlrkzpdmfksshgw
        rcxjvbroqtujlbyzpdmfksahgw
        icxjvmroqtugleazpdmfksahgw
        icxjvbfoqtunleylpdmfkeahgw
        icnjvoroktunleyzpdmfksahgw
        icxjvbroqtunlvyzpdmfkfahgr
        icxjvbroqtgnseyzpdmfxsahgw
        scxjvbroqtunleycpdmfksdhgw
        icxhvbxoqtunleuzpdmfksahgw
        icxjvbruqtunleyzpnmfksfhgw
        icdjvbroqtunleyzpdmfksahup
        ihxjvbroqtunleovpdmfksahgw
        icxjvbroqtunleyzxdmfksahzv
        ocxjvbioqtunleyzpdmfzsahgw
        idxjvbroqtunlyyzpdofksahgw
        izdjvbroqtunleyzpdtfksahgw
        icxjvbrnqtunleyzpdmfksbhgb
        icxjvbrjqtunleyhpdmrksahgw
        icxjvbroqtunleyzpdbflsahgg
        icxjvbmfqtunleyzpdmfkaahgw
        idxjvbroqtunlsyzpdffksahgw
        bcxjvbroqtunleyzpkmfkswhgw
        ivxjvbroqtdnleyzpdmbksahgw
        icxpvbboqtunleyzpdmfksahtw
        ibxjvbroqtunlehzpdmfkmahgw
        icxjvbboqtunleyzpdmfkaahgv
        icxjlaroqtuileyzpdmfksahgw
        icxjvbroftunleyzpdmfqsahew
        ichjvbroqtunleyzpdmiwsahgw
        icxrvbvoqtunleyzpdmiksahgw
        icxjvbroqtunldydpdmfksahgl
        icogvbroqtunleyzpdmfnsahgw
        icxjvbroqtunleszodmfkswhgw
        icxjvbrontunleyztemfksahgw
        icxjvbrovtunleyzpdvkksahgw
        icxjvbroqqucteyzpdmfksahgw
        icmovbroptunleyzpdmfksahgw
        icxjvbqoftunleyzvdmfksahgw
        icxjvbdoqtunleyzpdmfkadhgw
        icxjvbroqtunlgnzpdmfksaqgw
        icxjvbroqtunieygpdyfksahgw
        acdjvbroqtunleyzpdmfkwahgw
        icxjvbroqtunleyzpdmfkfahlj
        icxjvbgoqtunleyepdmvksahgw
        icxjvbpobbunleyzpdmfksahgw
        icxjvbroqtunleurpdmfktahgw
        ipxjvbzoqtunleyzpdmffsahgw
        icxjtbroqtunllyzpdmuksahgw
        icxjvbroqtunbsyzadmfksahgw
        ihxjvoroqtuqleyzpdmfksahgw
        idxjmbroqqunleyzpdmfksahgw
        wcxjvbdoqtunleyzpdmfksahgr
        icxjvbroqtunleygptmfksahgj
        ipxjvbrsqtunleyzpdmfksghgw
        ycxjvbroqtunluyzkdmfksahgw
        icxjvbroxtuulejzpdmfksahgw
        icqjvbroqtunlwyypdmfksahgw
        ioxjhbroqtunleyzphmfksahgw
        icxjvbgoqnunleyzpdmfksahaw
        mcxjvbroqtunleyzpdmfksihgh
        icxjsbroqtunlqyzpdmfksawgw
        icxjvbroqtuoleycpdmftsahgw
        icxjvbroqtunleyzgdifksahlw
        icxjvbmoqtunleyzjfmfksahgw
        icxjvbroqtunlezopdmfksahge
        icxjvbroqtbnlefzpdmfosahgw
        tcxjvbromtunlevzpdmfksahgw
        irxjgbroqtunleyzpdmfksthgw
        icxjvbrojtunleyxpdmoksahgw
        icxrvbroytpnleyzpdmfksahgw
        icxjvbroqtunfeyupdmfksasgw
        ihqjvbroqtunleyzpdmftsahgw
        icxjobroqkunleozpdmfksahgw
        icjjjbroqtualeyzpdmfksahgw
        icxjvbroqtunaeytpdmfksahvw
        icxjvbroqtunzeyzpdmfkshhxw
        icxqvbroqtucleyzxdmfksahgw
        icxjvbrogturleyzxdmfksahgw
        icxjvoqoqtunleyzpdcfksahgw
        iuxjvbroqtunleyzpdmfksopgw
        icxjveroqtunleyzptmfksalgw
        icxjvbroqtunleyzpdmfwcahhw
        iwxjvbroqtlnleyzpdmfksyhgw
        ectjvbroqtanleyzpdmfksahgw
        icxjvnroqtyhleyzpdmfksahgw
        icvjvhboqtunleyzpdmfksahgw
        icxjtbroqtuzleyupdmfksahgw
        icjjvproqtunleyzpsmfksahgw
        icdjvbroqtutleyzpdmiksahgw
        icxjvwroqtujleyzpdmfksahgc
        icxjxbroqtunleyzpdwhksahgw
        icxjvbqoqtunleyzpdmvfsahgw
        icajvbroqtusleyzpdmfksaagw
        icxjvbroqtunbtyzpdmfksmhgw
        kcxjvbroqtxnleyzpdmfkskhgw
        icxjvbqogfunleyzpdmfksahgw
        icxjvbroqtubleyzpdmfdswhgw
        icxjvprlqtunleyzpdmffsahgw
        icxjxbroqtucleyzpdmfksakgw
        dcxrvbroqtunleycpdmfksahgw
        icxjvbrobtunleyzpomfksahgu
        ocxrvbroqtunleyzpdmfssahgw
        icxjvbroktunlejzpdmfksahzw
        icxjvbrovtunleyzmdmfkhahgw
        icxjvbroqtudleygpdmfksfhgw
        bcxjvbroqtubllyzpdmfksahgw
        icxwvbrontunzeyzpdmfksahgw
        icxjvbroqtunleysjbmfksahgw
        icxjvvroztunleyzpdmfksjhgw
        ivxjxbroqtunleyzpdmfksahew
        icxjvbroqtunleyupqufksahgw
        icxjvmrcqtunleyzpdmxksahgw
        icxjvgroqtunleyzpdgfkuahgw
        icxjvbroqthnqeyfpdmfksahgw
        icxjsbuodtunleyzpdmfksahgw
        iuxjzbroqtunleyzpdrfksahgw
        icxjvbrobtunlelzpdmfksahgs
        icxjvbroqtzhljyzpdmfksahgw
        inxtvbroqtunleyzpdmeksahgw
        icgjvbroqtunleyztdmfksahgq
        icxjvagoqtugleyzpdmfksahgw
        icxuvbroqtunleyzpimfkyahgw
        icxzvbroqtfhleyzpdmfksahgw
        icxjjbroqtqnleyzpdmnksahgw
        icjrvbroqtunleszpdmfksahgw
        iexjvbroqtunlgyzpdmfksacgw
        rcxjvbkoqtuoleyzpdmfksahgw
        icxjvbroqgunlwyzpdmfksqhgw
        icxjvbroqtunleqzpsmfksqhgw
        icxjvbroqtubaeyzpdmfksaugw"""


part1 =
    answer1 question


strDiffIndices : String -> String -> List Int
strDiffIndices str1 str2 =
    List.map2 Tuple.pair (String.toList str1) (String.toList str2)
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( i, ( char1, char2 ) ) acc ->
                if char1 == char2 then
                    acc

                else
                    i :: acc
            )
            []


commonLettersBetween : String -> String -> String
commonLettersBetween str1 str2 =
    let
        diff =
            strDiffIndices str1 str2
    in
    String.toList str1
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( i, char ) acc ->
                if List.member i diff then
                    acc

                else
                    acc ++ [ char ]
            )
            []
        |> String.fromList


getIdWithOneDiff : List String -> String -> Maybe ( String, String, Int )
getIdWithOneDiff list str =
    list
        |> List.filterMap
            (\x ->
                case strDiffIndices x str of
                    [ oneDiff ] ->
                        Just ( x, str, oneDiff )

                    _ ->
                        Nothing
            )
        |> List.head


keepIdsWithOneDiff : List String -> List ( String, String, Int )
keepIdsWithOneDiff list =
    let
        singleDiffIds =
            getIdWithOneDiff list
    in
    list
        |> List.filterMap singleDiffIds
        |> List.foldl
            (\( a, b, diff ) acc ->
                if List.member ( a, b, diff ) acc || List.member ( b, a, diff ) acc then
                    acc

                else
                    ( a, b, diff ) :: acc
            )
            []


getCorrespondingIds : String -> List ( String, String, Int )
getCorrespondingIds str =
    String.split "\n" str
        |> List.map String.trim
        |> keepIdsWithOneDiff


intersectionsOfBoxIds : String -> List String
intersectionsOfBoxIds str =
    str
        |> getCorrespondingIds
        |> List.map (\( a, b, diff ) -> commonLettersBetween a b)


part2 =
    intersectionsOfBoxIds question
