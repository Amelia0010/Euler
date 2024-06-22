{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
isPalindrome x =
    candidate == reverse candidate
    where candidate = show x

check left right numSoFar =
    -- incrementation rules
    if right == 0 then
        numSoFar
    else if left == 0 then
        check 999 (right-1) numSoFar
    else
        if isPalindrome(result) && result > numSoFar then
            check (left - 1) right result
        else
            check (left - 1) right numSoFar
        where
            result = left * right