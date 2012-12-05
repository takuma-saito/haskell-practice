
import qualified Debug.Trace as Trace
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random
import Control.Monad (liftM)

-- 文字列の探索ライブラリ

textData = "This is hello world. program."
matchData = "ram"

-- blute force でパターンを探す -- 

-- 文字列を pos から length だけ切り取る
splitStr :: String -> Int -> Int -> String
splitStr text pos length = take length . drop pos $ text

-- パターンにマッチしているかどうかを探す
isTextMatch :: String -> String -> Int -> Bool
isTextMatch pattern text pos
    | pattern == (splitStr text pos (length pattern)) = True
    | otherwise                                       = False

-- blute force のメインルーチン
searchBluteForce :: String -> String -> Bool
searchBluteForce pattern text =
    search 0
        where
          search pos 
              | (pos > (length text))          = False
              | (isTextMatch pattern text pos) = True
              | otherwise                      = search (pos + 1)


-- BM 法でパターンを探す --

-- pattern の中に char が出現する位置文字の位置を返す
getCharPos :: Char -> String -> Maybe Int
getCharPos char string = lookup char $ zip string [0..]

-- BM 法のメインルーチン
searchBM :: String -> String -> Bool
searchBM pattern text = 
    search patternLen
        where
          patternLen = length pattern
          patternSize = patternLen - 1
          search pos
              | pos >= (length text) = False
              | (isMatch pos 0)      = True
              | otherwise            = search (pos + patternLen)
          isMatch pos size
              | size == patternLen = True
              | (pattern !! (patternSize - size)) == (text !! (pos - size)) =
                  isMatch pos (size + 1)
              | otherwise =
                  case getCharPos (text !! (pos - size)) pattern of
                    Just x -> search (pos + (patternSize - x))
                    Nothing -> False

-- 10文字以内のアスキー文字列
newtype Pattern = Pattern String deriving (Show)
newtype Text = Text String deriving (Show)

instance Arbitrary Pattern where
    arbitrary = do
       string <- vectorOf 5 $ elements (['A'..'Z'] ++ ['a'..'z'])
       return $ Pattern string

instance Arbitrary Text where
    arbitrary = oneof [liftM Text arbitrary]


-- チェック数を増やす
args = Args {
         maxSize = 1100,
         maxSuccess = 1000,
         chatty = True,
         maxDiscardRatio = 2,
         replay = Nothing
       }

prop_Id :: String -> String -> Property
prop_Id string1 string2 =
    not (null string1) && not (null string2) && (length string1) /= (length string2) ==>
            ((searchBluteForce pattern text) == (searchBM pattern text))
            where pattern = if (string1 > string2) then string2
                            else string1
                  text = if (string1 > string2) then string1
                         else string2
