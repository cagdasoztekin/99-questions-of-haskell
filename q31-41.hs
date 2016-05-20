{-  Question 31

    Determine whether a given integer number is prime.

    Example in Haskell:
	P31> isPrime 7
	True
-}

isPrime :: (Integral a) => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isPrime2 n [2..(n-1)]
	where 
		isPrime2 a [] = True
		isPrime2 a (x:xs) = a `mod` x /= 0 && isPrime2 a xs
