import Data.List
main = putStrLn $ show answer
  where
    number = 600851475143
    answer = last $ factors number

factors n = result
  where
    divisors = filter (\x -> x * ((n `div` x)) == n) [2..n]
    next = head divisors
    result | divisors == [] = []
           | otherwise = next : factors (n `div` next) 
