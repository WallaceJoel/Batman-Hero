import Data.List
import Data.Char
import Debug.Trace
import System.IO
import System.Random

loopHang ::Bool-> String -> String ->Int-> IO()
loopHang True s x i = putStrLn(x)
loopHang False s x 0 = do
  putStrLn(x)
  guess <- getChar
  let reWord = bangMan s x guess 0
  loopHang (s == reWord) s reWord 1
loopHang False s x 1 = do
  guess <- getChar
  let reWord = bangMan s x guess 0
  loopHang (s == reWord) s reWord 0

lengthCheck :: Bool -> String -> String -> Char -> Int -> String
lengthCheck True s x c i = x
lengthCheck False s x c i = listWords ((head (drop i s) ) == (toUpper c) || (head (drop i s) ) == (toLower c) ) s x c i

listWords :: Bool -> String -> String ->Char-> Int -> String
listWords False s x c i = bangMan s x c (i + 1)
listWords True s x c i = bangMan s ( (take i x)++(c:[])++(drop (i+1) x) ) c (i+1)

bangMan :: String ->String-> Char ->Int -> String
bangMan s x c i  = lengthCheck (i==(length s)) s x c i

makeDash :: String -> String
makeDash [] = []
makeDash s = "-" ++ (makeDash (tail s))

main = do 
  number <- randomRIO (0,45407) :: IO Int
  fileWords <-(readFile "HangmanWords.txt") 
  let word =  (head (drop number(lines fileWords) ) ) 
  let dash = (makeDash (head (drop number(lines fileWords) ) ) )
  loopHang False word dash 0

  
