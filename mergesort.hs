half :: [Int] -> ([Int], [Int])
half xs = splitAt ((length xs) `div` 2) xs

left :: [Int] -> [Int]
left xs = fst (half xs)

right :: [Int] -> [Int]
right xs = snd (half xs)

merge :: [Int] -> [Int] -> [Int]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
  | x <= y  = x:(merge xs (y:ys))
  | otherwise = y:(merge (x:xs) ys)

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = merge (mergeSort (left x)) (mergeSort (right x))

