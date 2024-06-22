import Data.Fixed (mod')
isPrime n = null [ d | d <- [2..(sqrt n)], n `mod'` d == 0 ]

findNthPrime n =
    findXthPrime n 1 3

findXthPrime x primesSoFar curVal =
    if isPrime curVal then
        if incPrime == x then
            curVal
        else
            findXthPrime x incPrime nextVal
    else
        findXthPrime x primesSoFar nextVal
    where
        incPrime = primesSoFar + 1
        nextVal = curVal + 2