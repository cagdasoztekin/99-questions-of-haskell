{-  Question 1
    Find the last element of a list.

    Example in Haskell:
        Prelude> myLast [1,2,3,4]
        4
        Prelude> myLast ['x','y','z']
        'z'
-}

myLast ::   [a] -> a
myLast []       =   error "No element in the list"
myLast [a]      =   a
myLast (_:xs)   =  myLast xs

{-  Question 2
    Find the last but one element of a list.

    Example in Haskell:
        Prelude> myButLast [1,2,3,4]
        3
        Prelude> myButLast ['a'..'z']
        'y'
-}

myButLast :: [a] -> a
myButLast []        =   error "No element in the list"
myButLast [a]       =   error "Just one element in the list"
myButLast [a,b]     =   a
myButLast (_:xs)    =   myButLast xs

{-  Question 3
    Find the K'th element of a list. The first element in the list is number 1.

    Example in Haskell:
        Prelude> elementAt [1,2,3] 2
        2
        Prelude> elementAt "haskell" 5
        'e'
-}

elementAt :: [a] -> Int -> a
elementAt [] k      = error "Given index greater than list size"
elementAt (x:_) 1   = x
elementAt (_:xs) k
    | k > 1     = elementAt xs (k-1)
    | otherwise = error "Given k is not positive"


{-  Question 4
    Find the number of elements of a list.

    Example in Haskell:
        Prelude> myLength [123, 456, 789]
        3
        Prelude> myLength "Hello, world!"
        13
-}

myLength :: [a] -> Int
myLength x = myLengthInc x 0
    where
        myLengthInc [] k = k
        myLengthInc (_:xs) k = myLengthInc xs (k+1)

{-  Question 5
    Reverse a list.

    Example in Haskell:
        Prelude> myReverse "A man, a plan, a canal, panama!"
        "!amanap ,lanac a ,nalp a ,nam A"
        Prelude> myReverse [1,2,3,4]
        [4,3,2,1]
-}

myReverse :: [a] -> [a]
myReverse l = myReverseAcc l []
    where
        myReverseAcc (x:xs) b = myReverseAcc xs (x:b)
        myReverseAcc [] b = b

{-  Question 6
    Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

    Example in Haskell:
    *Main> isPalindrome [1,2,3]
    False
    *Main> isPalindrome "madamimadam"
    True
    *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
    True
-}

isPalindrome :: (Eq x) => [x] -> Bool
isPalinrome []      = True
isPalindrome [_]    = True
isPalindrome xs = (head xs == last xs) && (isPalindrome (take (length (drop 1 xs) - 1) (drop 1 xs)))
