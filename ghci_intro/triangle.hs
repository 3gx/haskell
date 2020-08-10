-- triangle.hs

import Data.Function ((&))
import Data.List (intercalate)
import Data.Semigroup (stimes)

main = putStrLn triangle

triangle = intercalate "\n" lines
  where
    lines = [peak] <> map line [1 .. height]
    peak = stimes height " " <> "△"

line n = padding <> "╱" <> numbers <> "╲"
  where
    padding = stimes (height - n) " "
    numbers =
        cycle ["0", "1"]
            & drop (n `mod` 2)
            & take n
            & intercalate " "

height = 33
