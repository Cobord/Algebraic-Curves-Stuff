prime_factors :: Int -> [Int]
prime_factors n =
  case factors of
    [] -> [n]
    _  -> factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

-- should be the same as const True
check_factorization :: Int -> Bool
check_factorization x = (x==foldl (*) 1 (prime_factors x))

is_smooth_num :: Int -> Int -> Bool
is_smooth_num x y = all (<= y) (prime_factors x)

psi_helper :: Int -> Int -> [Int]
psi_helper x_max y = filter (\x -> is_smooth_num  x y) [1..x_max]

psi_count_smooth :: Int -> Int -> Int
psi_count_smooth = \x -> length . psi_helper x