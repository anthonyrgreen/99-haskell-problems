myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

secondLast :: [a] -> a
secondLast [x, _] = x
secondLast (x:xs) = secondLast xs

kthElement :: (Integral a) => a -> [b] -> b
kthElement 0 (x:xs) = x
kthElement n (x:xs) = kthElement (n-1) xs

numElements :: [a] -> Int
numElements [] = 0
numElements (x:xs) = 1 + numElements xs

reverseList :: [a] -> [a]
reverseList xs = foldl (flip (:)) [] xs

listEquality :: (Eq a) => [a] -> [a] -> Bool
listEquality [] [] = True
listEquality _ [] = False
listEquality [] _ = False
listEquality (x:xs) (y:ys) = if x == y
	then listEquality xs ys else False

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = 
	let sx = reverseList xs
	in listEquality xs sx

flatten :: [[a]] -> [a]
flatten = reverse . foldr (++) []
--flatten xs = reverse . (foldr (\ys zs -> ys ++ zs) [] xs)
--1. needs to be reversed
--2. why doesn't the second version work?

--compressDuplicates

--packDuplicates

--lengthEncode
