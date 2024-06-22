floorDiv t n = floor(t / n)

sumOfDivisibleByXUnderY x y = 
    myHighestDivisible * (
        (
            myHighestDivisibleFrac / 2.0 -- Fractional, Fractional -> Fractional
        ) + 0.5 -- Fractional, Fractional -> Fractional
    ) * x
    where
        myHighestDivisible::Double = realToFrac (floorDiv (y-1) x) -- y // x -- Integral, Integral -> Integral
        myHighestDivisibleFrac::Double = realToFrac myHighestDivisible -- fromIntegral -- Integral -> Num
    

eulerOne = do 
    let threeResult = sumOfDivisibleByXUnderY 3 1_000
    let fiveResult = sumOfDivisibleByXUnderY 5 1_000
    let fifteenResult = sumOfDivisibleByXUnderY 15 1_000
    print (threeResult + fiveResult - fifteenResult)
