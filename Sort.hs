
import Data.Array
import Data.List
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Map as Map
import System.Random
import Debug.Trace
import Data.Char (digitToInt)
import Data.Maybe (isNothing)
import Test.QuickCheck

testData = [(1, ("a", 27)), (5, ("b", 10)), (3, ("n", 283)), (8, ("k", 1))]
randomData n = take n . randomRs (1, n) $ mkStdGen 29834938743
rData :: [Int]
rData = take 10 . randomRs (1, 100000) $ mkStdGen 29834938743
rData2 = [1, 2, 8, 2835, 29, 298934, 275, 27538, 2829, 5328, 298735, 293873, 28, 382, 3985, 295]
               
-- testData の (int, (string, "int")) ⇐ "" 部分を昇順にソート
sortAscTestData = sortBy (compare `on` (snd . snd)) $ testData

-- testData の (int, (string, "int")) ⇐ "" 部分を降順にソート
sortDescTestData = sortBy (flip compare `on` (snd . snd)) $ testData


testData2 = [("a",1),("b",5),("n",3),("k",8)]

-- testData2 の数値部分を昇順にソート
sortNumTestData2 = sortBy (compare `on` snd) testData2

-- testData2 の文字列部分を降順にソート
sortStrTestData2 = sortBy (flip compare `on` fst) testData2


-- 選択ソート : O(n^2)
selectionSort [] = []
selectionSort list =
    (fst minList):(selectionSort (snd minList))
        where minList = let min = minimum list in
                        (min, delete min list)

-- 挿入ソート : O (n^2)
insertSort :: (Ord a) => [a] -> [a]
insertSort = foldl (\accumList x -> insert x accumList) []

-- バブルソート : O (n^2)
bubbleSort [] = []
bubbleSort list =
    sort list ((length list) - 1)
        where
          bubble y [] = [y]
          bubble y (x:xs) =  if (x > y) then y:x:xs
                             else x:y:xs
          sort list index = if (index == 0) then list
                            else sort (foldr bubble [] list) (index - 1)

-- シェルソート : O (n^1.25 〜 2)
-- シェルソート用の数列としては 3n + 1 を採用する
columnNumber :: [a] -> Int
columnNumber list =
    if (max <= 1) then 1
    else last . takeWhile (<= max) $ sequences
    where sequences = 1:[n * 3 + 1 | n <- sequences]
          max = (flip div 9) . length $ list

encodeColumn :: (Ord a) => Int -> [a] -> [[a]]
encodeColumn k = transpose . takeWhile (not . null) . unfoldr (Just . splitAt k)

decodeColumn :: (Ord a) => [[a]] -> [a]
decodeColumn = concat . transpose

shellSort list =
    sort (columnNumber list) list
    where
      sort 1 xs = insertSort xs
      sort column xs = sort (column `div` 3) (decodeColumn . map insertSort . encodeColumn column $ xs)

-- マージソート : O (n*log(n))
merge list [] = list
merge [] list = list
merge a@(x:xs) b@(y:ys) =
    if (x <= y) then x:(merge xs b)
    else y:(merge a ys)

mergeSort [] = []
mergeSort [x] = [x]
mergeSort list =
    let (upper, lower) = splitAt ((flip div 2) . length $ list) list in
    merge (mergeSort upper) (mergeSort lower)

-- クイックソート : O (n*log(n))
quickSort [] = []
quickSort (pivot:xs) =
    (comparePivot (<)) ++ [pivot] ++ (comparePivot (>=))
        where comparePivot cmp = quickSort $ filter (`cmp` pivot) xs

-- ヒープソート : O (n*log(n))

-- ビンソート : O (n) 
binSort bounds xs =
    concat $ elems $ accumArray (flip (:)) [] bounds [(x, x) | x <- xs]

-- 基数ソート : O (n)
type Radix = (Maybe [Char], [Char])
type RadixIndex = (Int, Radix)

radixMatcher :: Radix -> RadixIndex
radixMatcher (Nothing, remain) = (0, (Nothing, remain))
radixMatcher (Just "", remain) = (0, (Nothing, remain))
radixMatcher (Just list, remain) = (digitToInt (last list), (Just (init list), (last list):remain))

radixSortMain list =
    concat . elems . fmap reverse $ accumArray (flip (:)) [] (0, 9) (map radixMatcher list)

stringToInt string =
    sum $ zipWith (*) (reverse numList) [10 ^ i | i <- [0..]]
        where numList = map digitToInt string

encodeRadix :: [Int] -> [Radix]
encodeRadix = map (\x -> (Just $ show x, ""))

decodeRadix :: [Radix] -> [Int]
decodeRadix = map (\(_, num) -> stringToInt num)

radixSort :: [Int] -> [Int]
radixSort =
    decodeRadix . sort . encodeRadix
        where sort list = let newList = radixSortMain list
                      in if (all (\(num, _) -> isNothing num) list) then newList
                         else sort newList

-- チェック数を増やす
args = Args {
         maxSize = 1100,
         maxSuccess = 1000,
         chatty = True,
         maxDiscardRatio=2,
         replay=Nothing
       }

prop_Id :: [Int] -> Bool
prop_Id list =
        (== radixSort absList) . radixSort . radixSort $ absList
            where absList = map abs list

prop_Ordered sort list =
    not (null list) ==> 
    ordered (sort absList)
    where
      absList = map abs list
      ordered [] = True
      ordered [x] = True
      ordered (x:y:remain) = (x <= y) && ordered (y:remain)


