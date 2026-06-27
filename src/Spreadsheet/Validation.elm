module Spreadsheet.Validation exposing
    ( Rule(..)
    , check
    , options
    , describe
    )

{-| Data-validation rules attached to a range of cells.

A rule answers one question — *is this value allowed in the cell?* — via `check`. A blank
cell is always allowed except under `NotBlank` (validation fires on entry, not on emptiness).
`options` exposes the choice list of a dropdown (`OneOf`) rule so the view can render a
`<select>`, and `describe` gives a human message for a tooltip or rejection notice.

@docs Rule, check, options, describe

-}

import Spreadsheet.Value as Value exposing (Value(..))


{-| A validation rule. -}
type Rule
    = AnyValue
    | NotBlank
    | NumberBetween Float Float
    | NumberAtLeast Float
    | NumberAtMost Float
    | TextLengthMax Int
    | OneOf (List String)


{-| Does `value` satisfy the rule? A blank cell passes everything but `NotBlank`. -}
check : Rule -> Value -> Bool
check rule value =
    case value of
        VEmpty ->
            rule /= NotBlank

        _ ->
            checkNonEmpty rule value


checkNonEmpty : Rule -> Value -> Bool
checkNonEmpty rule value =
    case rule of
        AnyValue ->
            True

        NotBlank ->
            True

        NumberBetween lo hi ->
            withNumber (\n -> n >= lo && n <= hi) value

        NumberAtLeast lo ->
            withNumber (\n -> n >= lo) value

        NumberAtMost hi ->
            withNumber (\n -> n <= hi) value

        TextLengthMax n ->
            String.length (Value.toText value) <= n

        OneOf opts ->
            List.any (\o -> sameText o (Value.toText value)) opts


withNumber : (Float -> Bool) -> Value -> Bool
withNumber f value =
    case Value.toNumber value of
        Ok n ->
            f n

        Err _ ->
            False


sameText : String -> String -> Bool
sameText a b =
    String.toUpper (String.trim a) == String.toUpper (String.trim b)


{-| The choice list of a dropdown rule (for rendering a `<select>`). -}
options : Rule -> Maybe (List String)
options rule =
    case rule of
        OneOf opts ->
            Just opts

        _ ->
            Nothing


{-| A human description of the rule, for a tooltip or rejection message. -}
describe : Rule -> String
describe rule =
    case rule of
        AnyValue ->
            "Any value"

        NotBlank ->
            "Must not be blank"

        NumberBetween lo hi ->
            "A number from " ++ num lo ++ " to " ++ num hi

        NumberAtLeast lo ->
            "A number ≥ " ++ num lo

        NumberAtMost hi ->
            "A number ≤ " ++ num hi

        TextLengthMax n ->
            "At most " ++ String.fromInt n ++ " characters"

        OneOf opts ->
            "One of: " ++ String.join ", " opts


num : Float -> String
num x =
    Value.toText (VNumber x)
