{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}

import Data.List (foldl', intercalate)
import Control.Arrow ((***), first, app, (>>>))
import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Maybe (isNothing, catMaybes)

-- 半順序木 に対する様々な操作が行える
-- ヒープソート用に作成

-- 半順序木
data SemiHalfTree a = Empty
                    | Node {
                        nodeValue :: a
                      , nodeLeft :: SemiHalfTree a
                      , nodeRight :: SemiHalfTree a
                      } deriving (Show, Eq)

instance Functor SemiHalfTree where
    fmap f Empty = Empty
    fmap f tree = Node {
                    nodeValue = f (nodeValue tree)
                  , nodeLeft = fmap f (nodeLeft tree)
                  , nodeRight = fmap f (nodeRight tree)
                  }    

isEmpty :: (Ord a) => SemiHalfTree a -> Bool
isEmpty Empty = True
isEmpty _ = False

insertNode :: (Ord a) => a -> SemiHalfTree a -> SemiHalfTree a
insertNode value tree =
    mergeTree Node {nodeValue = value, nodeLeft = Empty, nodeRight = Empty} tree

-- 木から最小値を削除し、最小値を返す
deleteNode :: (Ord a) => SemiHalfTree a -> (a, SemiHalfTree a)
deleteNode tree = (nodeValue tree, mergeTree (nodeLeft tree) (nodeRight tree))

mergeTree Empty Empty = Empty
mergeTree x Empty = x
mergeTree Empty x = x
mergeTree a b
    | (nodeValue a) < (nodeValue b) = joinTree a b
    | otherwise                     = joinTree b a

joinTree a b = Node {
             nodeValue = (nodeValue a)
           , nodeLeft = (nodeRight a)
           , nodeRight = (mergeTree (nodeLeft a) b)
           }

fromList :: (Ord a) => [a] -> SemiHalfTree a
fromList [] = Empty
fromList list = foldl' (\tree x -> insertNode x tree) Empty list

toList (isEmpty -> True) = []
toList tree =
    if (isEmpty node)
       then [value]
       else value:(toList node)
    where (value, node) = deleteNode tree

-- 深さ優先探索を行う
dfsTree :: (Ord a) => SemiHalfTree a -> [a]
dfsTree Empty = []
dfsTree tree =
    (nodeValue tree):((dfsTree (nodeLeft tree)) ++ (dfsTree (nodeRight tree)))

-- 深さdepthまで再帰的に木を辿る
bfsWalk :: (Ord a) => SemiHalfTree a -> Int -> [a]
bfsWalk Empty _    = []
bfsWalk tree 0     = [(nodeValue tree)]
bfsWalk tree depth =
    (bfsWalk (nodeLeft tree) (depth - 1)) ++ (bfsWalk (nodeRight tree) (depth - 1))

-- 幅優先探索を行う
bfsTree :: (Ord a) => SemiHalfTree a -> [[a]]
bfsTree tree = takeWhile (not . null) $ [bfsWalk tree i | i <- [0..]]

-- bfsTreeAdv tree rest
--     | Empty _ = []
--     | tree nodeValue =  rest

-- 木の深さリスト
bfsShowTree :: (Ord a) => SemiHalfTree a -> [[Maybe a]]
bfsShowTree tree = takeWhile (not . all isNothing) [walk tree i | i <- [0..]]
    where
      walk tree 0 =
          case tree of
            Empty -> [Nothing]
            _     -> [Just (nodeValue tree)]
      walk tree depth =
          let (right, left) =
                  case tree of
                    Empty -> (Empty, Empty)
                    _     -> (nodeLeft tree, nodeRight tree)
          in (walk left (depth - 1)) ++ (walk right (depth - 1))

seqs :: [Int]
seqs = iterate (\x -> x * 2 + 1) 1

-- 木を表示させる
showTree =  reverse . procRow . zip seqs . reverse . bfsShowTree
    where
      procRow = map (\(index, row) -> intercalate " " $ map (proc index) row)
      proc space x =
          case x of
            Nothing -> rep (space * 2 + 1)
            Just x  -> (rep space) ++ show x ++ (rep space)
            where rep index = replicate index ' '

out :: [String] -> IO ()
out = mapM_ putStrLn

-- テスト用の木を生成する
infixr 6 +>>
(+>>) a b = (insertNode a b)
testTree = 3 +>> 4 +>> 8 +>> 1 +>> 12 +>> 2 +>> 5 +>> 28 +>> 7 +>> Empty

testData = [5, 3, 8, 10, 21, 23, 29, 4, 292, 8, 7, 5, 293]
testData2 = [1, 2, 8, 2835, 29, 298934, 275, 27538, 2829, 5328, 298735, 293873, 28, 382, 3985, 295]
-- testData3 = [17,12,19,22,21,23,19,24,8,7,10,16,9,17,3,19,5,14,2,17,30,27,4,6,10,18,9,5,26,30]
testData3 = [17,12,19,19,5,14,2,17,30,27,4,6,10,18,9,5,26,30]
tree = fromList testData
tree2 = fromList testData2
tree3 = fromList testData3
