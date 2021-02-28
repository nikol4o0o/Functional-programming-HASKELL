--1 задача
uptoNum:: Double ->Double ->Double
uptoNum kv denominator
    |denominator == 1 = 1.0
    |otherwise        =(1/(denominator ** kv)) + uptoNum kv (denominator - 1)


generate :: Double -> Int -> [Double]
generate k m = reverse (returnList k m)
    where
        returnList kv maxD =
            if(maxD == 1) then [1] else uptoNum kv (fromIntegral maxD) : returnList kv (maxD - 1)





--3 задача
type Point = (Double, Double)

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints _ _ [] = ([], [])
splitPoints centre radius pts = ([ip | ip <- pts, isItInTheCircle ip], [op | op <- pts, not(isItInTheCircle op)])
    where
        isItInTheCircle :: Point -> Bool
        isItInTheCircle pnt = ((fst pnt - fst centre)** 2 + (snd pnt - snd centre)**2) <= (radius ** 2)


--2 задача
isSquareRoot n = helper 1
    where
        helper k
            |k * k == n  = True
            |k * k > n   = False
            |otherwise   = helper (k + 1)
 

listSquares:: Integer ->Integer -> [(Integer, Integer)]
listSquares a b = [(n, divisorsSquaresNum n) | n <- [a..b], isSquareRoot n]
    where
        divisorsSquaresNum  n = sum[d * d | d <- [1..n], n `mod` d == 0]


--4 задача
type Account = (Int, Int, Double)
type Person = (Int, String, String)

ps :: [Person]
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
as :: [Account]
as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2), (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

returnAverage :: [Double] -> Double
returnAverage bal = sum bal / fromIntegral(length bal)

returnAverageBal :: ([Account], [Person]) -> (Person -> Bool) -> Double
returnAverageBal dBal q = returnAverage userBalances
    where
        searchedUserIds = map(\ (id, _, _) -> id) (filter q(snd dBal))
        userBalances = [balace | (_, userID, balace) <- (fst dBal), any (userID==) searchedUserIds]

main :: IO()
main = do 
 
--1 задача примери
--print(generate 1 3) 
--print(generate 0.1 5)-- → [1.0,1.93,2.83,3.70,4.55]

--2 задача примери
--print(listSquares 1 30) -- → [(1,1),(4,21),(9,91),(16,341),(25,651)]
--print(listSquares 250 300) -- → [(256,87381),(289,83811)

--3 задача примери
--print(splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)])  
--([1.0,2.0),(2.0,3.0),(-1.0,1.0)],[(10.0,15.0),(12.0,14.0)])
--print(splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) 
--([(10.0,15.0),(12.0,14.0)],[(1.0,2.0),(2.0,3.0),(-1.0,1.0)])

--4 задача примери
--print(returnAverageBal (as,ps) (\ (_,_,city) -> city == "Burgas")) 
-- →24.95
--print(returnAverageBal (as,ps) (\ (_,(n:_),_) -> n == 'P')) 
-- → 18.85
