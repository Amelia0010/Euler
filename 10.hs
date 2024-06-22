sieve divisor arr
    | null arr = []
    | (curVal `mod` divisor) /= 0 = curVal : rest
    | otherwise = rest
    where
        curVal = head arr
        rest = sieve divisor (tail arr)

eratosthenes sumSoFar curValue remainingNumbers sieveCap
    | null remainingNumbers = sumSoFar
    | curValue > sieveCap = sum remainingNumbers + sumSoFar
    | otherwise = eratosthenes (sumSoFar + curValue) (head nextNumbers) nextNumbers sieveCap
    where
        nextNumbers = sieve curValue remainingNumbers

sumOfPrimesUpTo limit =
    eratosthenes 0 2 [3..limit] sieveCap
    where
        sieveCap = round (sqrt doubleCap)
        doubleCap::Double = fromIntegral limit