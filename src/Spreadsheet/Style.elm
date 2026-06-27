module Spreadsheet.Style exposing
    ( CellStyle
    , Align(..)
    , emptyStyle
    , mergeStyle
    , Rendered
    , render
    , alignClass
    , Condition(..)
    , matches
    , Rule
    , ColorScale
    , DataBar
    , lerpColor
    , dataBarPercent
    )

{-| Cell appearance: static styling, conditional-formatting rules, colour scales and
data bars.

The guiding rule (from the brief) is **classes, not inline styles, wherever possible** —
so the structural look (bold, italic, alignment, borders) is expressed as CSS classes
that the bundled stylesheet defines and a host page can override. Only genuinely
*data-driven* colour (a specific hex from a colour scale, a per-value bar width) is
emitted inline, because no fixed class could express a continuous value.

`render` turns a `CellStyle` into a `Rendered` record of `classes` + `inline`
declarations, which the view converts to attributes. Keeping it as plain data (not
`Html.Attribute`) makes styling unit-testable without a DOM.

@docs CellStyle, Align, emptyStyle, mergeStyle, Rendered, render, alignClass
@docs Condition, matches, Rule, ColorScale, DataBar, lerpColor, dataBarPercent

-}

import Spreadsheet.Functions as Functions
import Spreadsheet.Ref exposing (Range)
import Spreadsheet.Value as Value exposing (Value(..))


{-| The static style attached to a cell. `classes` are author-supplied custom classes;
the booleans map to bundled utility classes; `color`/`background` are escape hatches for
arbitrary colours (emitted inline). -}
type alias CellStyle =
    { classes : List String
    , bold : Bool
    , italic : Bool
    , underline : Bool
    , strikethrough : Bool
    , align : Maybe Align
    , color : Maybe String
    , background : Maybe String
    }


{-| Horizontal alignment. -}
type Align
    = AlignLeft
    | AlignCenter
    | AlignRight


{-| A cell with no styling. -}
emptyStyle : CellStyle
emptyStyle =
    { classes = []
    , bold = False
    , italic = False
    , underline = False
    , strikethrough = False
    , align = Nothing
    , color = Nothing
    , background = Nothing
    }


{-| Layer `top` over `base`: booleans OR together, `Maybe` fields prefer `top` when set,
class lists concatenate. Used to combine a cell's own style with the styles contributed
by any matching conditional-format rules. -}
mergeStyle : CellStyle -> CellStyle -> CellStyle
mergeStyle base top =
    { classes = base.classes ++ top.classes
    , bold = base.bold || top.bold
    , italic = base.italic || top.italic
    , underline = base.underline || top.underline
    , strikethrough = base.strikethrough || top.strikethrough
    , align = orElse base.align top.align
    , color = orElse base.color top.color
    , background = orElse base.background top.background
    }


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback m =
    case m of
        Just _ ->
            m

        Nothing ->
            fallback


{-| The render target: a set of CSS classes plus inline `(property, value)` declarations
(used only for data-driven colour). -}
type alias Rendered =
    { classes : List String
    , inline : List ( String, String )
    }


{-| Turn a `CellStyle` into classes + inline declarations, given the cell's value (so we
can pick a default alignment when none is set). -}
render : CellStyle -> Value -> Rendered
render style value =
    let
        alignmentClass =
            case style.align of
                Just a ->
                    alignClass a

                Nothing ->
                    defaultAlign value

        utilityClasses =
            List.filterMap identity
                [ justIf style.bold "ss-bold"
                , justIf style.italic "ss-italic"
                , justIf style.underline "ss-underline"
                , justIf style.strikethrough "ss-strike"
                ]

        inline =
            List.filterMap identity
                [ Maybe.map (\c -> ( "color", c )) style.color
                , Maybe.map (\c -> ( "background-color", c )) style.background
                ]
    in
    { classes = alignmentClass :: utilityClasses ++ style.classes
    , inline = inline
    }


justIf : Bool -> a -> Maybe a
justIf cond x =
    if cond then
        Just x

    else
        Nothing


{-| The CSS class for an explicit alignment. -}
alignClass : Align -> String
alignClass a =
    case a of
        AlignLeft ->
            "ss-align-left"

        AlignCenter ->
            "ss-align-center"

        AlignRight ->
            "ss-align-right"


defaultAlign : Value -> String
defaultAlign value =
    case value of
        VNumber _ ->
            "ss-align-right"

        VBool _ ->
            "ss-align-center"

        VError _ ->
            "ss-align-center"

        _ ->
            "ss-align-left"



-- CONDITIONAL FORMATTING -----------------------------------------------------


{-| A predicate over a single cell value, the test side of a conditional-format rule. -}
type Condition
    = GreaterThan Float
    | GreaterEqual Float
    | LessThan Float
    | LessEqual Float
    | EqualToNum Float
    | NotEqualNum Float
    | Between Float Float
    | TextContainsC String
    | TextEquals String
    | IsEmptyC
    | NotEmptyC
    | IsErrorC
    | CriteriaC String


{-| Does a value satisfy a condition? Numeric comparisons coerce; text comparisons are
case-insensitive; `CriteriaC` reuses the COUNTIF-style criterion grammar. -}
matches : Condition -> Value -> Bool
matches condition value =
    case condition of
        GreaterThan n ->
            numTest value (\x -> x > n)

        GreaterEqual n ->
            numTest value (\x -> x >= n)

        LessThan n ->
            numTest value (\x -> x < n)

        LessEqual n ->
            numTest value (\x -> x <= n)

        EqualToNum n ->
            numTest value (\x -> x == n)

        NotEqualNum n ->
            numTest value (\x -> x /= n)

        Between lo hi ->
            numTest value (\x -> x >= lo && x <= hi)

        TextContainsC sub ->
            String.contains (String.toLower sub) (String.toLower (Value.toText value))

        TextEquals s ->
            String.toLower (Value.toText value) == String.toLower s

        IsEmptyC ->
            value == VEmpty

        NotEmptyC ->
            value /= VEmpty

        IsErrorC ->
            Value.isError value

        CriteriaC crit ->
            Functions.matchCriteria crit value


numTest : Value -> (Float -> Bool) -> Bool
numTest value test =
    case Value.toNumber value of
        Ok n ->
            test n

        Err _ ->
            False


{-| A conditional-format rule: when a cell in `range` satisfies `condition`, layer
`style` on top of its own. -}
type alias Rule =
    { range : Range
    , condition : Condition
    , style : CellStyle
    }


{-| A two-colour scale across a range: cells are tinted between `low` (at the range's
minimum) and `high` (at the maximum). The actual per-cell colour is computed in the
sheet, which knows the range's extent. -}
type alias ColorScale =
    { range : Range
    , low : String
    , high : String
    }


{-| A data bar: a horizontal bar whose width is proportional to the cell value within the
range's extent. -}
type alias DataBar =
    { range : Range
    , color : String
    }


{-| Linear interpolation between two `#rrggbb` colours; `t` is clamped to `[0,1]`. -}
lerpColor : String -> String -> Float -> String
lerpColor lo hi t01 =
    let
        t =
            clamp 0 1 t01
    in
    case ( parseHex lo, parseHex hi ) of
        ( Just ( r1, g1, b1 ), Just ( r2, g2, b2 ) ) ->
            "rgb("
                ++ String.fromInt (lerpInt r1 r2 t)
                ++ ","
                ++ String.fromInt (lerpInt g1 g2 t)
                ++ ","
                ++ String.fromInt (lerpInt b1 b2 t)
                ++ ")"

        _ ->
            lo


lerpInt : Int -> Int -> Float -> Int
lerpInt a b t =
    round (toFloat a + (toFloat b - toFloat a) * t)


parseHex : String -> Maybe ( Int, Int, Int )
parseHex raw =
    let
        s =
            String.replace "#" "" (String.trim raw)
    in
    if String.length s == 6 then
        Maybe.map3 (\r g b -> ( r, g, b ))
            (hexByte (String.slice 0 2 s))
            (hexByte (String.slice 2 4 s))
            (hexByte (String.slice 4 6 s))

    else
        Nothing


hexByte : String -> Maybe Int
hexByte s =
    case String.toList (String.toLower s) of
        [ hi, lo ] ->
            Maybe.map2 (\h l -> h * 16 + l) (hexDigit hi) (hexDigit lo)

        _ ->
            Nothing


hexDigit : Char -> Maybe Int
hexDigit c =
    if Char.isDigit c then
        Just (Char.toCode c - Char.toCode '0')

    else if c >= 'a' && c <= 'f' then
        Just (Char.toCode c - Char.toCode 'a' + 10)

    else
        Nothing


{-| The fill percentage (0–100) for a data bar, given the value and the range's min/max. -}
dataBarPercent : Float -> Float -> Float -> Float
dataBarPercent minV maxV value =
    if maxV <= minV then
        if value > 0 then
            100

        else
            0

    else
        clamp 0 100 ((value - minV) / (maxV - minV) * 100)
