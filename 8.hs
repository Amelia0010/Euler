{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Char
{-# HLINT ignore "Use guards" #-}
xAdjacentDigitsInYWithGreatestProduct x y =
    xAdjacentDigitsInYStringWithGreatestProduct x (show y)

productOfDigits arr =
    product (map digitToInt arr)

xAdjacentDigitsInYStringWithGreatestProduct x y =
    if length y < x then
        0
    else if length y == x then
        curProduct
    else
        max nextProduct curProduct
    where
        nextProduct = xAdjacentDigitsInYStringWithGreatestProduct x (drop 1 y)
        curProduct = productOfDigits (take x y)