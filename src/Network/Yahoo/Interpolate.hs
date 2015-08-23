module Network.Yahoo.Interpolate
    ( interpolate )
    where

import           Data.Monoid ((<>))
import           Text.Regex  (mkRegex, subRegex)

toMarker :: String -> String
toMarker value = "${" <> value <> "}"

replace :: String -> String -> String -> String
replace string marker replacement =
    subRegex (mkRegex marker) string replacement

-- | Simple string interpolation
--   interpolate "foo ${bar}" [("bar", "baz")] -> "foo baz"
interpolate :: String -> [(String, String)] -> String
interpolate input xs =
    foldl (\s v ->
        let (a,b) = v in replace s (toMarker a) b) input xs
