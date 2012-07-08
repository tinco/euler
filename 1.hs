main = putStrLn $ show answer
  where
    answer = sum $ [i | i <- [1..999], i `mod` 3 == 0 || i `mod` 5 == 0 ]
