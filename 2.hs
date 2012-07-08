main = putStrLn $ show answer
  where
    answer = sum $ [i | i <- fibSeq, i `mod` 2 == 0  ]

fibSeq = takeWhile (\i -> i < 4000000) (fibSeq' 0 0)
  where
    fibSeq' 0 0 = 1 : 1 : fibSeq' 1 1
    fibSeq' a b = (a + b) : fibSeq' b (a + b)


