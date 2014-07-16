-- 20
insertAt        :: Int -> a -> [a] -> [a]
insertAt 1 x ys = x : ys
insertAt i x (y : ys)
    | i < 1     = error "Bad index"  
    | otherwise = y : (insertAt (i - 1) x ys)

-- 21
range     :: Integral a => a -> a -> [a]
range i j = if i < j
            then i : range (i + 1) j
            else []

-- 22

