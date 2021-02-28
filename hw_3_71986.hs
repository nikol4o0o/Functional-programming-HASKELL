import Data.List


data BTree a = Empty | Node a (BTree a) (BTree a)

--1 задача // Задача 1 се реализира с помощта на зад 2, защото при зад 2 се извежда  списък с всички елементи, така че е достатъчно 
-- да потърсим само в списъка
containsWord :: BTree Char -> String-> Bool
containsWord a str = helper (genWords a) str
   where
      helper (x:xs) str = if (x == str) then True else (helper xs str)
      helper _ str      = False


-------------------------------------------------------------
t1 :: BTree Char 
t1 = Node 'a' (Node 'c' (Node 'f' Empty Empty) 
 (Node 'd' Empty Empty)) 
 (Node 'b' Empty 
 (Node 'e' Empty Empty)) 

t2 :: BTree Char                                 
t2 = Node 'a' (Node 'c' (Node 'd' Empty Empty)    
 Empty)                                            
 (Node 'b' Empty Empty)                           
                                                   

t3 :: NTree               
t3 = NNode 1 [NNode 3 [],   
 NNode 5 [],                 
 NNode 7 [],
 NNode 9 []]
                                        
t4 :: NTree                             
t4 = NNode 7 [NNode 9 [NNode 5 [],      
 NNode 2 []]]                         
                                        
-------------------------------------------------------------
--2 задача за идеята е използван учебен материал от интернет, задачата е реализирана със самостоятелен труд
genWords :: BTree Char -> [String]
genWords Empty                 = []
genWords (Node ch Empty Empty) = [[ch]] 
genWords (Node ch left right)  = map (ch:) path ++ path
   where path                  = genWords left ++ genWords right

--3 задача
allContain::[BTree Char] -> [String]
allContain xs = repeated[v| v <- helper(xs)]
   where 
         repeated [] = []
         repeated (x:xs)
            | x `elem` xs = x : repeated xs
            | otherwise   = repeated xs

helper::[BTree Char] -> [String]
helper []     = []
helper (x:xs) = (genWords x) ++ helper xs

-- 4 задача
data NTree = Nil | NNode Int [NTree]

isODD:: Int -> Bool
isODD k = k `mod` 2 /= 0

differentODDs :: [NTree] -> Int -> Bool
differentODDs [q@(NNode qv _)] x    = isODD((abs(qv - x)) + 1)
differentODDs (q@(NNode qv _):qs) x = isODD((abs(qv - x)) + 1) && (differentODDs qs x)

isGraceful:: NTree -> Bool
isGraceful Nil                              = True
isGraceful (NNode c [])                     = True
isGraceful (NNode c (v@(NNode c1 _ ) : vs)) = (differentODDs(v:vs) c) && isGraceful v

      










main::IO() 
main = do
   --1 задача примери
   print (containsWord t1 "acd") -- True
   print (containsWord t1 "cd")  -- True
   print (containsWord t1 "ac")  -- False
   --2 задача примери 
   print (genWords t1) -- ["acf","acd","af","ad","abe","ae","cf","cd","f","d","be","e"]
   print (genWords t2) -- ["acd","ad","ab","cd","d","b"]
   --3 задача примери
   print (allContain [t1, t2]) -- ["acd","ad","cd","d"]
   --4 задача примери 
   print (isGraceful t3) -- True (|3-1|=2, |5-1|=4, |7-1|=6, |9-1|=8)
   print (isGraceful t4) -- False (|9-7|=2, |5-9|=4, |2-9|=7)



        
