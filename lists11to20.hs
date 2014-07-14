-- 14
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x : xs) = x : (x : duplicate xs)

-- 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli xs i
	| i < 1 	= error "The number of times to replicate is invalid"
	| otherwise = foldr (\x acc -> (replicate i x) ++ acc) [] xs