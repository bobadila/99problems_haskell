-- 1
myLast :: [a] -> a
myLast [] = error "The list is empty, can't find the last element"
myLast (x : xs)
	| null xs 	= x
	| otherwise = myLast xs

-- 2
myLastButOne :: [a] -> a
myLastButOne [] = error "The list is empty"
myLastButOne [x] = error "The list contains only one element"
myLastButOne [x, _] = x
myLastButOne (x : xs)
	| length xs == 1 	= x
	| otherwise 		= myLastButOne xs

-- 3
elementAt :: [a] -> Int -> a
elementAt xs index
	| index <= 0 || null xs = error "Index is out of bounds"
	| index == 1 			= head xs
	| otherwise 			= elementAt (tail xs) (index - 1)

-- 4
myLength :: [a] -> Int
myLength = foldl (\len _ -> len + 1) 0

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = myReverse' xs []
	where myReverse' (x : xs) rxs
		| null xs 	= x : rxs
		| otherwise = myReverse' xs (x : rxs)

-- 5
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 6
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten xs = reverse (rec [] xs)
	where 
		rec rxs (List []) = rxs
		rec rxs (Elem x) = x : rxs
		rec rxs (List (x : xs)) = rec (rec rxs x) (List xs)

-- 8
compress :: (Eq a) => [a] -> [a]
compress xs = reverse (foldl accum [] xs)
	where
		accum [] x 	= [x]
		accum xs y 	= if (head xs) == y then xs else (y: xs)

-- 9
pack :: Eq a => [a] -> [[a]]
pack = foldr accum []
	where
		accum x [] = [[x]]
		accum x (lf : ll) = if x == (head lf) then ((x :lf):ll) else ([x]:lf:ll)

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map countDup (pack xs)
	where
		countDup :: [a] -> (Int, a)
		countDup dups = (length dups, head dups)