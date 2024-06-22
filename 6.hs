-- t = (n*(n+1)) / 2
-- m = (n-1) * 0.66... + 1
-- d = t * t - (m * t)
sumSquareDifference x =
    round(t * t - m * t)
    where
        t = (x * (x+1)) / 2
        m = (x-1) * ((1/3) * 2) + 1