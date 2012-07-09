{- A palindromic number reads the same both ways. 
 - The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
 - Find the largest palindrome made from the product of two 3-digit numbers.
 - -}
main = putStrLn $ show answer
  where
    answer = head palindromes

palindromes = filter isPalindrome productsOf3Digits

productsOf3Digits = productsOf3Digits' 999 999
productsOf3Digits' a b = [i * b | i <- [999,998..a]] ++ rest
  where
    rest | a == 100 && b == 100 = []
         | a == b = productsOf3Digits' (a-1) (a-1) 
         | otherwise = productsOf3Digits' a (b-1)

isPalindrome n = chars == reverse chars
  where
    chars = show n
