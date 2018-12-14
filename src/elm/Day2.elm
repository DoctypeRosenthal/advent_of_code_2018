module Day2 exposing (answer, hasMultiples, hash, part1, question)


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


answer : String -> Int
answer str =
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
    answer question
