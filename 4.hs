import Data.List
{- A palindromic number reads the same both ways. 
 - The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
 - Find the largest palindrome made from the product of two 3-digit numbers.
 - -}
main = putStrLn $ show answer
  where
    answer = head palindromes

palindromes = filter isPalindrome productsOf3Digits

productsOf3Digits = productsOf3Digits' 999 999
productsOf3Digits' a b | a == b = a * b : productsOf3Digits' 999 (b-1) 
                       | otherwise = a * b : productsOf3Digits' (a-1) b
{-
99 99
99 98
98 98
99 97
98 97
97 97
 -}

isPalindrome n = chars == reverse chars
  where
    chars = show n

