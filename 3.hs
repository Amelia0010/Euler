import Data.Fixed (mod')

isXDivisorOfY x y =
   y `mod'` x == 0

isPrime n = null [ d | d <- [2..(sqrt n)], n `mod'` d == 0 ]

getSmallerDivisorOfXAboveY x y
  | y > x = 1
  | isXDivisorOfY y x && isPrime bigDivisor && isXDivisorOfY bigDivisor x = bigDivisor
  | otherwise = getSmallerDivisorOfXAboveY x (y + 2)
  where
      bigDivisor = x / y

highestPrimeFactor x =
    getSmallerDivisorOfXAboveY x 3