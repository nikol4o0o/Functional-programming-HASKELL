import Data.List

--1 задача
removeEveryKth:: Int -> [a] -> [a]
removeEveryKth _ [] = []
removeEveryKth 0 xs = []
removeEveryKth k xs = take (k-1) xs ++ removeEveryKth k (drop k xs)

--2 задача 
factorize :: Int -> [Int]
factorize n 
  | n > 0 = if n == 1 then []else let helper2 = helper n 2 in helper2 : factorize (n `div` helper2)
  where helper k x
          | rem k x == 0 = x
          | x * x > k    = k
          | otherwise    = helper k (if odd x then x + 2 else x + 1)

--3 задача 
--poly:: [Int] -> (Int -> Int)

--4 задача не можах да я оправя да събира само най-дълбоките, иначе решавахме таква на упражнение :)

data BTree a = Empty | Node a (BTree a)  (BTree a)

sumTree :: Num a => BTree a -> a
sumTree Empty               = 0
sumTree (Node x left right) = x + sumTree left + sumTree right

sumLeaves :: Num a => BTree a -> a
sumLeaves Empty                = 0
sumLeaves (Node x Empty Empty) = x
sumLeaves (Node _ left  right) = sumLeaves left + sumLeaves right


t3 :: BTree Int
t3 = Node 1 (Node 2 (Node 4 (Node 7 Empty Empty)
 Empty) 
 (Node 5 Empty 
 Empty))
 (Node 3 Empty
 (Node 6 Empty 
 (Node 8 Empty Empty)))

t4 :: BTree Int
t4 = Node 1 (Node 2 (Node 4 Empty Empty) 
  Empty)
  (Node 3 Empty Empty) 


main::IO()
main= do
    print (removeEveryKth 2 [1,2,3,4,5,6])
    print (removeEveryKth 3 [1,2,3,4,5,6])
    print (factorize 152)
    print (factorize 13)
    print (sumLeaves t3)
    print (sumLeaves t4)