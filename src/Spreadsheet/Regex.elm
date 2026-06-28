module Spreadsheet.Regex exposing
    ( test
    , extract
    , replace
    )

{-| A small, dependency-free regular-expression engine — enough to back the spreadsheet's
`REGEXTEST` / `REGEXEXTRACT` / `REGEXREPLACE` functions without relying on a `Regex` kernel
that may not be bound on every backend.

The supported syntax is the common subset:

  - literals and `.` (any character)
  - quantifiers `*`, `+`, `?` (greedy, with backtracking)
  - character classes `[abc]`, ranges `[a-z]`, negation `[^...]`
  - escapes `\d \D \w \W \s \S` and `\.` etc. for literal metacharacters
  - anchors `^` and `$`
  - capturing groups `( … )` and alternation `a|b`

Matching is a straightforward backtracking interpreter over the parsed pattern. The three
entry points all compile the pattern once and then scan the input.

@docs test, extract, replace

-}

import Array exposing (Array)
import Dict exposing (Dict)



-- PUBLIC API -----------------------------------------------------------------


{-| Does the pattern match anywhere in the text? -}
test : Bool -> String -> String -> Bool
test ignoreCase pattern text =
    case compile pattern of
        Just re ->
            firstMatch ignoreCase re (toArr ignoreCase text) /= Nothing

        Nothing ->
            False


{-| The first match in the text, or the first **capturing group** of that match when the
pattern has one (matching Google Sheets' `REGEXEXTRACT`). `Nothing` if it doesn't match. -}
extract : Bool -> String -> String -> Maybe String
extract ignoreCase pattern text =
    case compile pattern of
        Just re ->
            let
                arr =
                    toArr ignoreCase text

                orig =
                    Array.fromList (String.toList text)
            in
            case firstMatch ignoreCase re arr of
                Just ( start, end, caps ) ->
                    case Dict.get 1 caps of
                        Just ( gs, ge ) ->
                            Just (sliceArr orig gs ge)

                        Nothing ->
                            Just (sliceArr orig start end)

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| Replace every (non-overlapping) match with `replacement`, in which `$0` is the whole
match and `$1`…`$9` are capturing groups. -}
replace : Bool -> String -> String -> String -> String
replace ignoreCase pattern replacement text =
    case compile pattern of
        Just re ->
            let
                arr =
                    toArr ignoreCase text

                orig =
                    Array.fromList (String.toList text)
            in
            replaceFrom ignoreCase re arr orig replacement 0 []

        Nothing ->
            text


toArr : Bool -> String -> Array Char
toArr ignoreCase text =
    Array.fromList
        (String.toList
            (if ignoreCase then
                String.toLower text

             else
                text
            )
        )


sliceArr : Array Char -> Int -> Int -> String
sliceArr arr from to =
    String.fromList (Array.toList (Array.slice from to arr))


replaceFrom : Bool -> Re -> Array Char -> Array Char -> String -> Int -> List String -> String
replaceFrom ignoreCase re arr orig replacement pos acc =
    if pos > Array.length arr then
        String.concat (List.reverse acc)

    else
        case matchAt ignoreCase re arr pos of
            Just ( end, caps ) ->
                let
                    rep =
                        expandReplacement replacement orig pos end caps

                    -- Always advance at least one character to avoid looping on a
                    -- zero-width match.
                    nextPos =
                        if end > pos then
                            end

                        else
                            pos + 1

                    skipped =
                        if end > pos then
                            ""

                        else
                            charStr orig pos
                in
                replaceFrom ignoreCase re arr orig replacement nextPos (skipped :: rep :: acc)

            Nothing ->
                replaceFrom ignoreCase re arr orig replacement (pos + 1) (charStr orig pos :: acc)


charStr : Array Char -> Int -> String
charStr arr i =
    case Array.get i arr of
        Just c ->
            String.fromChar c

        Nothing ->
            ""


expandReplacement : String -> Array Char -> Int -> Int -> Captures -> String
expandReplacement replacement orig start end caps =
    expandHelp (String.toList replacement) orig start end caps []


expandHelp : List Char -> Array Char -> Int -> Int -> Captures -> List String -> String
expandHelp chars orig start end caps acc =
    case chars of
        [] ->
            String.concat (List.reverse acc)

        '$' :: d :: rest ->
            if Char.isDigit d then
                let
                    n =
                        Char.toCode d - Char.toCode '0'

                    piece =
                        if n == 0 then
                            sliceArr orig start end

                        else
                            case Dict.get n caps of
                                Just ( gs, ge ) ->
                                    sliceArr orig gs ge

                                Nothing ->
                                    ""
                in
                expandHelp rest orig start end caps (piece :: acc)

            else
                expandHelp (d :: rest) orig start end caps (String.fromChar '$' :: acc)

        c :: rest ->
            expandHelp rest orig start end caps (String.fromChar c :: acc)



-- PATTERN AST ----------------------------------------------------------------


type alias Re =
    List Seq


type alias Seq =
    List Quant


type Quant
    = Quant Atom Rep


type Rep
    = One
    | Star
    | Plus
    | Opt


type Atom
    = Lit Char
    | Any
    | Cls Bool (List CItem)
    | Grp Int Re
    | AnchorStart
    | AnchorEnd


type CItem
    = CChar Char
    | CRange Char Char
    | CClass Char



-- PARSER ---------------------------------------------------------------------


type alias PState =
    { chars : List Char, group : Int }


compile : String -> Maybe Re
compile pattern =
    case parseRe { chars = String.toList pattern, group = 0 } of
        Just ( re, st ) ->
            if List.isEmpty st.chars then
                Just re

            else
                Nothing

        Nothing ->
            Nothing


parseRe : PState -> Maybe ( Re, PState )
parseRe st0 =
    case parseSeq st0 of
        Just ( seq, st1 ) ->
            case st1.chars of
                '|' :: rest ->
                    case parseRe { st1 | chars = rest } of
                        Just ( alts, st2 ) ->
                            Just ( seq :: alts, st2 )

                        Nothing ->
                            Nothing

                _ ->
                    Just ( [ seq ], st1 )

        Nothing ->
            Nothing


parseSeq : PState -> Maybe ( Seq, PState )
parseSeq st =
    case st.chars of
        [] ->
            Just ( [], st )

        '|' :: _ ->
            Just ( [], st )

        ')' :: _ ->
            Just ( [], st )

        _ ->
            case parseQuant st of
                Just ( q, st1 ) ->
                    case parseSeq st1 of
                        Just ( rest, st2 ) ->
                            Just ( q :: rest, st2 )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing


parseQuant : PState -> Maybe ( Quant, PState )
parseQuant st =
    case parseAtom st of
        Just ( atom, st1 ) ->
            case st1.chars of
                '*' :: rest ->
                    Just ( Quant atom Star, { st1 | chars = rest } )

                '+' :: rest ->
                    Just ( Quant atom Plus, { st1 | chars = rest } )

                '?' :: rest ->
                    Just ( Quant atom Opt, { st1 | chars = rest } )

                _ ->
                    Just ( Quant atom One, st1 )

        Nothing ->
            Nothing


parseAtom : PState -> Maybe ( Atom, PState )
parseAtom st =
    case st.chars of
        [] ->
            Nothing

        '(' :: rest ->
            let
                idx =
                    st.group + 1
            in
            case parseRe { chars = rest, group = idx } of
                Just ( re, st1 ) ->
                    case st1.chars of
                        ')' :: more ->
                            Just ( Grp idx re, { st1 | chars = more } )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        '[' :: rest ->
            parseClass rest st.group

        '.' :: rest ->
            Just ( Any, { st | chars = rest } )

        '^' :: rest ->
            Just ( AnchorStart, { st | chars = rest } )

        '$' :: rest ->
            Just ( AnchorEnd, { st | chars = rest } )

        '\\' :: c :: rest ->
            Just ( escapeAtom c, { st | chars = rest } )

        c :: rest ->
            if c == ')' || c == '|' || c == '*' || c == '+' || c == '?' then
                Nothing

            else
                Just ( Lit c, { st | chars = rest } )


escapeAtom : Char -> Atom
escapeAtom c =
    case classOfEscape c of
        Just _ ->
            Cls False [ CClass c ]

        Nothing ->
            Lit c


parseClass : List Char -> Int -> Maybe ( Atom, PState )
parseClass chars group =
    let
        ( negated, rest0 ) =
            case chars of
                '^' :: more ->
                    ( True, more )

                _ ->
                    ( False, chars )
    in
    parseClassItems rest0 [] negated group


parseClassItems : List Char -> List CItem -> Bool -> Int -> Maybe ( Atom, PState )
parseClassItems chars acc negated group =
    case chars of
        [] ->
            Nothing

        ']' :: rest ->
            Just ( Cls negated (List.reverse acc), { chars = rest, group = group } )

        '\\' :: c :: rest ->
            let
                item =
                    case classOfEscape c of
                        Just _ ->
                            CClass c

                        Nothing ->
                            CChar c
            in
            parseClassItems rest (item :: acc) negated group

        a :: '-' :: b :: rest ->
            if b /= ']' then
                parseClassItems rest (CRange a b :: acc) negated group

            else
                parseClassItems ('-' :: b :: rest) (CChar a :: acc) negated group

        c :: rest ->
            parseClassItems rest (CChar c :: acc) negated group


classOfEscape : Char -> Maybe (Char -> Bool)
classOfEscape c =
    case c of
        'd' ->
            Just Char.isDigit

        'D' ->
            Just (\x -> not (Char.isDigit x))

        'w' ->
            Just isWord

        'W' ->
            Just (\x -> not (isWord x))

        's' ->
            Just isSpace

        'S' ->
            Just (\x -> not (isSpace x))

        _ ->
            Nothing


isWord : Char -> Bool
isWord c =
    Char.isAlphaNum c || c == '_'


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}'



-- MATCHER (backtracking, CPS) ------------------------------------------------


type alias Captures =
    Dict Int ( Int, Int )


type alias St =
    { pos : Int, caps : Captures }


{-| Find the first match at or after position 0: `(start, end, captures)`. -}
firstMatch : Bool -> Re -> Array Char -> Maybe ( Int, Int, Captures )
firstMatch _ re arr =
    scanFrom re arr 0


scanFrom : Re -> Array Char -> Int -> Maybe ( Int, Int, Captures )
scanFrom re arr i =
    if i > Array.length arr then
        Nothing

    else
        case matchAt False re arr i of
            Just ( end, caps ) ->
                Just ( i, end, caps )

            Nothing ->
                scanFrom re arr (i + 1)


{-| Try to match the pattern anchored at position `i`; returns the end position and the
captures if it matches. -}
matchAt : Bool -> Re -> Array Char -> Int -> Maybe ( Int, Captures )
matchAt _ re arr i =
    case matchRe arr re { pos = i, caps = Dict.empty } (\st -> Just st) of
        Just st ->
            Just ( st.pos, st.caps )

        Nothing ->
            Nothing


matchRe : Array Char -> Re -> St -> (St -> Maybe St) -> Maybe St
matchRe arr alts st k =
    case alts of
        [] ->
            Nothing

        seq :: rest ->
            case matchSeq arr seq st k of
                Just done ->
                    Just done

                Nothing ->
                    matchRe arr rest st k


matchSeq : Array Char -> Seq -> St -> (St -> Maybe St) -> Maybe St
matchSeq arr seq st k =
    case seq of
        [] ->
            k st

        q :: rest ->
            matchQuant arr q st (\st2 -> matchSeq arr rest st2 k)


matchQuant : Array Char -> Quant -> St -> (St -> Maybe St) -> Maybe St
matchQuant arr (Quant atom rep) st k =
    case rep of
        One ->
            matchAtom arr atom st k

        Opt ->
            orElse (matchAtom arr atom st k) (\() -> k st)

        Star ->
            matchStar arr atom st k

        Plus ->
            matchAtom arr atom st (\st2 -> matchStar arr atom st2 k)


matchStar : Array Char -> Atom -> St -> (St -> Maybe St) -> Maybe St
matchStar arr atom st k =
    orElse
        (matchAtom arr
            atom
            st
            (\st2 ->
                if st2.pos > st.pos then
                    matchStar arr atom st2 k

                else
                    Nothing
            )
        )
        (\() -> k st)


matchAtom : Array Char -> Atom -> St -> (St -> Maybe St) -> Maybe St
matchAtom arr atom st k =
    case atom of
        Lit c ->
            if Array.get st.pos arr == Just c then
                k { st | pos = st.pos + 1 }

            else
                Nothing

        Any ->
            if st.pos < Array.length arr then
                k { st | pos = st.pos + 1 }

            else
                Nothing

        Cls negated items ->
            case Array.get st.pos arr of
                Just c ->
                    if classMatches items c /= negated then
                        k { st | pos = st.pos + 1 }

                    else
                        Nothing

                Nothing ->
                    Nothing

        AnchorStart ->
            if st.pos == 0 then
                k st

            else
                Nothing

        AnchorEnd ->
            if st.pos == Array.length arr then
                k st

            else
                Nothing

        Grp idx re ->
            matchRe arr re st (\st2 -> k { st2 | caps = Dict.insert idx ( st.pos, st2.pos ) st2.caps })


classMatches : List CItem -> Char -> Bool
classMatches items c =
    List.any (itemMatches c) items


itemMatches : Char -> CItem -> Bool
itemMatches c item =
    case item of
        CChar x ->
            x == c

        CRange lo hi ->
            c >= lo && c <= hi

        CClass e ->
            case classOfEscape e of
                Just f ->
                    f c

                Nothing ->
                    False


orElse : Maybe a -> (() -> Maybe a) -> Maybe a
orElse a b =
    case a of
        Just _ ->
            a

        Nothing ->
            b ()
