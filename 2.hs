fibonacciSumUnderX x a b
  | b > x = 0
  | even b = b + fibonacciSumUnderX x b (a+b)
  | otherwise = fibonacciSumUnderX x b (a+b)