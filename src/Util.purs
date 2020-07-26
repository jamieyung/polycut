module Util
    ( hsl_to_hex
    , Hsl_to_hex
    , hsl_to_rgb
    , Hsl_to_rgb
    , n_decimal_places_for_hue_and_lightness
    , render_range
    , Number_range
    ) where

import Prelude

import Math (abs)
import Slider (format_as_percentage)

foreign import data Hsl_to_hex :: Type
foreign import hsl_to_hex :: Hsl_to_hex

foreign import data Hsl_to_rgb :: Type
foreign import hsl_to_rgb :: Hsl_to_rgb

n_decimal_places_for_hue_and_lightness :: Number -> Int
n_decimal_places_for_hue_and_lightness n =
    let absn = abs n
    in if absn == 0.0 then 0
    else if absn < 0.1 then 3
    else if absn < 50.0 then 1
    else 0

render_range :: (Number -> Int) -> Number_range -> String
render_range n_decimal_places r =
    let format = format_as_percentage n_decimal_places
        u = format.to r.min
        v = format.to r.max
    in "(" <> u <> ", " <> v <> ")"

type Number_range = { min :: Number, max :: Number }
