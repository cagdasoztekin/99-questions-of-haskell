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

{-  Question 32

    Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

    Example in Haskell:
	[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
	[9,3,3]
-}

myGCD :: (Integral a) => a -> a -> a
myGCD 1 n = 1
myGCD n 1 = 1
myGCD n 0 = n
myGCD 0 n = n
myGCD m k 
    | m > k	= myGCD k (m `mod` k)
    | otherwise = myGCD m (k `mod` m)

{-  Question 33 

    Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

    Example:
	* (coprime 35 64)
	T
-}

coprime :: (Integral a) => a -> a -> Bool
coprime n k = myGCD n k == 1

{-  Question 34

    Calculate Euler's totient function phi(m).

    Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

    Example:
	* (totient-phi 10)
	4
-}

totient :: (Integral a) => a -> a
totient n = totient2 n [1..(n-1)] 0
	where
		totient2 n [] k = k
		totient2 n (x:xs) k 
			| coprime n x = totient2 n xs (k+1)
			| otherwise = totient2 n xs k

-- another implementation

totient3 n = length [x | x <- [1..(n-1)], coprime x n]

{-  Question 35 

    (**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

    Example:
    	* (prime-factors 315)
	(3 3 5 7)
-}

