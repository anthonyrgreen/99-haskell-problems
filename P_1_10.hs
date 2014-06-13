module P_1_10
( myLast
, secondLast
, kthElement
, numElements
, reverseList
, listEquality
, isPalindrome
, isPalindrome'
, flatten
, compressDuplicates
, compressDuplicates'
, compressDuplicates''
, compressDuplicates'''
, packDuplicates
, packDuplicates'
, lengthEncode
, lengthEncode'
) where

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

secondLast :: [a] -> a
secondLast [x, _] = x
secondLast (x:xs) = secondLast xs

kthElement :: (Integral a) => a -> [b] -> b
kthElement 0 (x:_) = x
kthElement n (_:xs) = kthElement (n-1) xs

numElements :: [a] -> Int
numElements [] = 0
numElements (_:xs) = 1 + numElements xs

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) [] 

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

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == sx
  where sx = reverseList xs

flatten :: [[a]] -> [a]
flatten = foldr (++) []

compressDuplicates :: (Eq a) => [a] -> [a]
compressDuplicates [] = []
compressDuplicates [x] = [x]
compressDuplicates (x:y:xs) = if x == y 
  then compressDuplicates (y:xs) else x : compressDuplicates (y:xs)

compressDuplicates' :: (Eq a) => [a] -> [a]
compressDuplicates' =
  let compressFold x xs = case xs of 
        [] -> [x]
        (y:ys) -> if x == y then (x:ys) else (x:y:ys)
  in foldr compressFold []

compressDuplicates'' :: (Eq a) => [a] -> [a]
compressDuplicates'' = foldr compressFold []
  where compressFold x [] = [x]
        compressFold x (y:ys)
          | x == y    = y:ys
          | otherwise = x:y:ys

compressDuplicates''' :: (Eq a) => [a] -> [a]
compressDuplicates''' [] = []
compressDuplicates''' (x:xs) = head headList : compressDuplicates''' tailList
  where (headList, tailList) = span (==x) (x:xs)

packDuplicates :: (Eq a) => [a] -> [[a]]
packDuplicates xs = 
  let packFold x xs = case xs of
        [] -> [[x]]
        ((y:ys):zs) -> if x == y then ((x:y:ys):zs) else ([x]:(y:ys):zs)
  in foldr packFold [] xs

packDuplicates' :: (Eq a) => [a] -> [[a]]
packDuplicates' [] = []
packDuplicates' (x:xs) = headList : packDuplicates' tailList
  where (headList, tailList) = span (==x) (x:xs)

lengthEncode :: (Eq a) => [a] -> [(Int, a)]
lengthEncode = 
  let lengthFold x xs = case xs of
        [] -> [(1, x)]
        ((num, y):ys) -> if x == y then ((num + 1, y):ys) else ((1, x):(num,y):ys)
  in foldr lengthFold []

lengthEncode' :: (Eq a) => [a] -> [(Int, a)]
lengthEncode' = map (\xs -> (length xs, head xs)) . packDuplicates' 
