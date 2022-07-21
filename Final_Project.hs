items = ["item1", "item2", "item3", "item4", "item5", "item6"] 
--purchasesHistory = [("user1",[["item1","item2","item3"],["item1","item2","item4"]]),
--("user2",[["item2","item5"],["item4","item5"]]),
--("user3",[["item3","item2"]]),("user4",[ ])]


--randomZeroToX :: Int -> Int
--randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

--recommend :: String -> [String] -> String

--recommend u ((x,y):xs) = if u == x then recommend1 (concat y) else recommend u xs

--recommend1 ::  [String] -> String

--recommend1 x = x !! randomZeroToX length x
--listfreq :: [a]->[a]-> [a]
--freqListItems:: [a] -> [(a, b)]
--freqListItems a = findinpurch a purchasesHistory 
--(String, [[String]])]
--getAllUsersStats :: [(a, [[a]])] -> [(a, [(a, [a, b)])])]
--getAllUsersStats [a]




createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList (x:xs)=[(x,[])]++ createEmptyFreqList xs
createEmptyFreqList []=[]



findinpurch ::Eq a =>[a]-> [([a],b)]-> ([a],b)
findinpurch a ((x1,x2):xs)= if a==x1 then (x1,x2) else findinpurch a xs 
--findinpurch _ []=


itemfreq :: Eq a => a -> [a] -> Int 
itemfreq y (x:xs)= if y==x then 1 + (itemfreq y xs) else itemfreq y xs
itemfreq _ []=0

itemfreqlist :: Eq a=> a-> [a]-> [(a,Int)]
itemfreqlist y x = if (itemfreq y x) == 0 then [] else [(y,itemfreq y x)]



cont ::Eq a => a -> [[a]] -> [a]
cont a (x:xs) = if elem a x then x ++ cont a xs else cont a xs
cont _ [] = []

--appears :: [a]-> [a]-> [[(a,Int)]]
--appears (x:xs) xl= itemfreqlist x x1 ++ appears xs x1
--getAllUsersStats :: [(a, [[a]])] -> [(a, [(a, [(a, b)])])]
--getAllUsersStats [(a, [[a]])]=

--removed1 :: Eq a => a -> [a] -> [a]

--removed1 y (x:xs) = if x == y then removed1 y xs else if (x elem xs) then removed1 y xs else x ++ removed1 y xs
--removed1 _ [] = []
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = remDups list []

remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | (x `elem` list2) = remDups xs list2
    | otherwise = x : remDups xs (x:list2)