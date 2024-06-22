smallestMultipleOfTriangleNumberOfX x =
    check x x

isXDivisibleByAllNumbersUnderY x y
  | y == 1 = True
  | x `rem` y == 0 = isXDivisibleByAllNumbersUnderY x (y-1)
  | otherwise = False


check x currentValue =
    if isXDivisibleByAllNumbersUnderY currentValue (x-1) then
        currentValue
    else
        check x (currentValue + x)