import Data.List
main = putStrLn $ show answer
  where
    number = 600851475143
    primes' = reverse $ primes number
    answer = find (\x -> x * ((number `div` x)) == number) primes'
    --answer = take 100 $ primes number

-- primes = [i | i <- [1..], not $ any (\x -> i `mod` x == 0) [2..(i-1)]]

-- From http://primes.utm.edu/prove/prove2_1.html
-- Make a list of all the integers less than or equal to n (greater than one) and
-- strike out the multiples of all primes less than or equal to the square root of n,
--  then the numbers that are left are the primes. (See also our glossary page.)
notMultiple x ys = [y | y<- ys,  x `mod` y == 0] == []
exceptMultiples ms xs = [x | x <- xs, notMultiple x ms]

primes n = 1 : 2 : primes' [2] n
primes' ps n | not st = []
             | otherwise = p : (primes' (p:ps) n)
  where
    ns = [last ps..n]
    st = head ns < squareRoot n 
    rst = exceptMultiples ps ns
    p = head rst


(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
 
-- From http://www.haskell.org/haskellwiki/Generic_number_type#squareRoot
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters
