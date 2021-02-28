-- 1 задача
findSum :: Int -> Int -> Int -> Int 
findSum a b k = sumTheLastThree 0 0 0
    where 
        sumTheLastThree n result numsCnt
            |numsCnt == 3       = result
            |(n <(k - numsCnt)) = sumTheLastThree(n+1)(result + b*(2^n)) numsCnt
            |(n==(k - numsCnt)) = (a + result) + sumTheLastThree 0 0 (numsCnt + 1)

 -- 2 задачa
isSquareRoot :: Int -> Bool
isSquareRoot s = helper s 2 
    where 
        helper s m
         | s < 0     = error "No negative input allowed!"
         | s == 0    = True
         | s == 1    = True 
         | m < s     = if((m*m) == s) then True else helper s (m+1)
         | otherwise = False      
-- 3 задача
isSpecial :: Integer -> Int -> Bool
isSpecial n k 
    | numDigits n == k = isTheNumberPrime((theLastSearchedElements n k)) 
    | isTheNumberPrime((theLastSearchedElements n k))==False = False 
    | otherwise = isSpecial (n `div` 10) k 


theLastSearchedElements :: Integer -> Int -> Integer
theLastSearchedElements n k = n `mod` (10^k )

isTheNumberPrime :: Integer -> Bool
isTheNumberPrime 1 = False
isTheNumberPrime n = smthToHelp(n-1)
    where
        smthToHelp current
            | current == 1 = True
            | n `mod` current== 0 = False
            | otherwise = smthToHelp( current- 1)




numDigits :: Integer -> Int
numDigits n =  (round (logBase 10 (fromIntegral n))) + 1
    



    

main :: IO()
main = do
    --1

    --print(findSum 0 2 10)
    --print(findSum 5 3 5) 
    
    --2 

     --print(isSquareRoot 1)
     --print(isSquareRoot 2)
     --print(isSquareRoot 4)
     --print(isSquareRoot 17)
     --print(isSquareRoot 256)
     --print(isSquareRoot 2500)
     
     --3
    
     --print (isSpecial 131 2)
     --print(isSpecial 472 2)
     --print(isSpecial 17197 2)
     --print(isSpecial 12234 3)
     --print(isSpecial 10113 3)
     --print(isSpecial 353 2)
