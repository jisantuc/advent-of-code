module Debug where

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf 0 xs = [xs]
chunksOf n xs = [take n xs] ++ chunksOf n (drop n xs)
