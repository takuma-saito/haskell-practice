
import Data.List (unfoldr)

-- 数列のいろいろな表現方法
-- A(0) = 1
-- A(n + 1) = 3 * A(n) + 1 の無限数列を作る
-- 一番キレイなのはiterateを使って数列を表現する方法

-- unfoldr を使う方法
a = unfoldr (\x -> Just (x, (3 * x + 1))) 1

-- scanl を使う方法
b = scanl (\x _ -> x * 3 + 1) 1 [0..]

-- iterateを使う方法
c = iterate (\x -> x * 3 + 1) 1

-- リスト内包表記を使う方法
d = 1:[ 3 * x + 1 | x <- c]
    
