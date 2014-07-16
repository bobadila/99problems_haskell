-- 11
data VariousSize a = Multiple Int a | Single a
                     deriving (Show)
 
encode_modified          :: (Eq a) => [a] -> [VariousSize a]
encode_modified []       = []
encode_modified (x : xs) = analyze ch_var x 0 xs
    where
      analyze choose x rep []            = [choose x (rep + 1)]
      analyze choose x rep rest@(y : ys) = if x == y
                                           then analyze choose x (rep + 1) ys
                                           else (choose x (rep + 1)) : analyze choose y 0 ys
      ch_var x rep                       = if rep == 1
                                           then (Single x)
                                           else (Multiple rep x)

-- 12
decode_modified :: (Eq a) => [VariousSize a] -> [a]
decode_modified = foldr decode' []
    where
      decode' (Multiple rep x) acc = replicate rep x ++ acc
      decode' (Single x) acc       = x : acc


-- 14
dupli          :: [a] -> [a]
dupli []       = []
dupli (x : xs) = x : x : (dupli xs)

-- 15
repli      :: [a] -> Int -> [a]
repli [] _ = []
repli xs n
    | n < 1     = error "Wrong number of times to replicate"
    | otherwise = foldr (\x acc -> prepend x acc n) [] xs
    where
      prepend x acc 1 = x : acc
      prepend x acc n = prepend x (x : acc) (n - 1)

-- 16
dropEvery      :: [a] -> Int -> [a]
dropEvery _ 1  = []
dropEvery xs n = dropEvery' xs n
    where
      dropEvery' all@(x : xs) k = if k == 1
                                  then dropEvery' xs n
                                  else (x : dropEvery' xs (k - 1))
      dropEvery' [] _           = []

-- 17
split      :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split all@(x : xs) n
    | n > 0     = (x : ls, rs)
    | otherwise = ([], all)
    where
      (ls, rs) = split xs (n - 1)

--18
slice        :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x : xs) i j
    | i > 1     = slice xs (i - 1) (j - 1)
    | j < 1     = []
    | otherwise = x : slice xs (i - 1) (j - 1)

--19
rotate      :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs n
    | n < 0     = let len = length xs
                      shift = (abs n) `rem` len
                  in rotate xs (len - shift)
    | otherwise = let len = length xs
                      shift = n `rem` len
                  in (slice xs (shift  + 1) len) ++ (slice xs 1 (shift))


-- 20
removeAt            :: Int -> [a] -> [a]
removeAt _ []       = []
removeAt i (x : xs) = if i == 1
                      then xs
                      else x : removeAt (i - 1) xs
