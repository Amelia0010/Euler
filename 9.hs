getHypotenuse a b = sqrt ((a * a) + (b * b))

calculateTriplet a b = a + b + c where c = getHypotenuse a b

getTriplet a b
  | currentValue > 1000 = getTriplet (a + 1) (a + 2)
  | currentValue < 1000 = getTriplet a (b+1)
  | otherwise = a * b * (getHypotenuse a b)
  where
      currentValue = calculateTriplet a b