
import Data.List (intercalate)
import Data.Char (digitToInt, isDigit)
import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs)
import Control.Monad (when)
import Control.Exception (throwIO)

-- パスカルの三角形を表示するプログラム

combination :: Integer -> Integer -> Integer
combination n k
    | n == k || n == 0 || k == 0 = 1
    | otherwise = (product [(k + 1)..n]) `div` (product [1..(n - k)])

pascalStream n = 
    [n `combination` i | i <- [0..n]]:(pascalStream (n + 1))

pascalTriangles n = take n $ pascalStream 0

toDigits :: Integer -> Integer
toDigits = fromIntegral . length . map digitToInt . show

toString :: Integer -> Integer -> [Integer] -> String
toString spaceNum maxLen list = spaces ++ line
    where spaces = replicate (div ((fromIntegral maxLen) - (length line)) 2) ' '
          line = intercalate (replicate (fromIntegral spaceNum) ' ') . map show $ list

out :: [String] -> IO ()
out = mapM_ putStrLn

pascalShow :: Int -> [String]
pascalShow n = reverse . map (toString spaceNum maxLen) . reverse $ pascals
    where pascals = pascalTriangles n
          spaceNum
              | space >= 5 = 3
              | space >= 2 = 2
              | otherwise = space
              where space = toDigits . maximum . map maximum $ pascals
          maxLen = (+ ((fromIntegral (length pascals)) * spaceNum)) . sum . map toDigits . last $ pascals

-- 終了処理
exit warning = do
  putStrLn warning
  exitWith (ExitFailure 1)

-- 引数からバリデート処理を行い、パスカル三角形の辺の数を取得
getNumber = do
  args <- getArgs
  when (length args /= 1) $ do
         exit "./Pascal [1-100] : show pascal triangle"
  let digits = head args
  when (not $ all isDigit digits) $ do 
         exit "Please input digits argument"
  let number = (read digits :: Int)
  when (0 > number || 100 < number) $ do
         exit "Please input 1 to 100 numbers in argument"
  return number
  
-- パスカルの三角形を表示させる
main = do
  number <- getNumber
  out (pascalShow number)
  
               
