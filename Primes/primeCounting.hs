-- \pi (x) - \pi (\sqrt{x}) = -1 + \sum_{S \subseteq Primes(\leq \sqrt{x})} (-1)^{|S|} \floor{ \frac{x}{\prod_{p \in S} p} }
-- Page 138 http://www.ams.org/journals/bull/2012-49-01/S0273-0979-2011-01359-3/S0273-0979-2011-01359-3.pdf

import Data.Maybe

primesToGT :: Int -> [Int]
primesToGT m
           | m<2 = []
           | otherwise = sieve [2..m]
           where
           sieve (p:xs) 
                  | p*p > m = p : xs
                  | True    = p : sieve [x | x <- xs, rem x p > 0]

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

summand :: [Int] -> Int -> Int
summand sSet x = (-1)^(length sSet) * floor ((fromIntegral x)/(fromIntegral $ foldl (*) 1 sSet))

piFunctionSlow :: Int -> Maybe Int
piFunctionSlow x
                 | x<=50000 = Just (length $ primesToGT x)
                 | otherwise = Nothing

-- there are $2^{\pi (\sqrt{x})}$ subsets to sum over so that grows fast, if x=42^2 that is already 8192 summands
piFunctionSlower :: Int -> Maybe Int
piFunctionSlower x
             | x <=100 = Just (length $ primesToGT x)
             | x <= 42^2 = Just (fromJust (piFunctionSlow sqrtX) - 1 + sum [summand sSet x | sSet <- (subsets $ primesToGT sqrtX)])
             | otherwise = Nothing
              where sqrtX = head $ reverse $ takeWhile (\p -> p^2 <= x) [1..x]