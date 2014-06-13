import P_1_10
module P_11_20
( lengthEncode''
, duplicateElts
, replicateK
, dropEvery
, split
, slice
, rotate
, removeAt
) where

data Encode a = Single a | Multiple Int a deriving (Show)

lengthEncode'' :: (Eq a) => [a] -> [Encode a]
lengthEncode'' xs = map toEncode $ lengthEncode' xs
	where toEncode (x, y) 
					| x == 1 = Single y
					| otherwise = Multiple x y

duplicateElts :: [a] -> [a]
duplicateElts = flatten . map (\x -> [x, x])

replicateK :: Int -> [a] -> [a]
replicateK k = flatten . map (\x -> take k $ repeat x)

dropEvery :: (Integral a) => a -> [b] -> [b]
dropEvery k xs = dropCount k k xs
	where 
		dropCount _ _ []     = []
		dropCount k 0 (_:xs) = dropCount k k xs
		dropCount k i (x:xs) = x : dropCount k (i-1) xs

split :: Int -> [a] -> ([a], [a])
split k xs = (\(xs, ys) -> (reverse xs, ys)) $ splitHelp k ([], xs)
	where
		splitHelp _ (xs, []) = (xs, [])
		splitHelp 0 (xs, ys) = (xs, ys)
		splitHelp k (xs, y:ys) = splitHelp (k-1) (y:xs, ys)

slice :: Int -> Int -> [a] -> [a]
slice 0 0 _ = []
slice 0 k (x:xs) = x : slice 0 (k-1) xs
slice j k (x:xs) = slice (j-1) k xs

rotate :: Int -> [a] -> [a]
rotate k (x:xs)
	| k == 0 = (x:xs)
	| k > 0 = let (ys, zs) = split rotVal (x:xs) in zs ++ ys 
	| k < 0 = let (ys, zs) = split ((length (x:xs)) - rotVal) (x:xs) in zs ++ ys
	where	rotVal = k `mod` (length (x:xs))

removeAt :: Int -> [a] -> [a]
removeAt k = tupleToList . (split k)
	where
		tupleToList (xs, []) = xs
		tupleToList (xs, y:ys) = xs ++ ys
