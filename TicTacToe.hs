import Data.List
import System.IO
import Data.Char

findInt :: Char->Int
findInt 'A'=1
findInt 'B'=3
findInt 'C'=5
findInt 'D'=9
findInt 'E'=11
findInt 'F'=13
findInt 'G'=17
findInt 'H'=19
findInt 'I'=21
findInt c = -1

threeCheck::Int->String -> Bool
threeCheck 1 c = ( (c!!1 == c!!3) && (c!!1 == c!!5) ) || ( (c!!1 == c!!11) && (c!!1 == c!!21) ) || ( (c!!1 == c!!9) && (c!!1 == c!!17) )
threeCheck 3 c = ( (c!!1 == c!!3) && (c!!1 == c!!5) ) || ( (c!!3 == c!!11) && (c!!3 == c!!19) )
threeCheck 5 c = ((c!!1 == c!!3) && (c!!1 == c!!5)) || ((c!!5 == c!!13) && (c!!5 == c!!21)) || ( (c!!5 == c!!11) && (c!!5 == c!!17) )
threeCheck 9 c = ( (c!!9 == c!!11) && (c!!9 == c!!13) ) || ( (c!!1 == c!!9) && (c!!1 == c!!17) )
threeCheck 11 c = ((c!!9 == c!!11) && (c!!9 == c!!13)) || ( (c!!3 == c!!11) && (c!!3 == c!!19) ) || ( (c!!1 == c!!11) && (c!!1 == c!!21) ) ||  ( (c!!5 == c!!11) && (c!!5 == c!!17) )
threeCheck 13 c = ((c!!9 == c!!11) &&(c!!9 == c!!13)) || ((c!!5 == c!!13) && (c!!5 == c!!21))
threeCheck 17 c = ((c!!17 == c!!19) && (c!!17 == c!!21)) || ( (c!!1 == c!!9) && (c!!1 == c!!17) ) || ( (c!!5 == c!!11) && (c!!5 == c!!17) )
threeCheck 19 c = ((c!!17 == c!!19)  && (c!!17 == c!!21) ) || ( (c!!3 == c!!11) && (c!!3 == c!!19) )
threeCheck 21 c = ((c!!17 == c!!19) && (c!!17 == c!!21) )||((c!!5 == c!!13) && (c!!5 == c!!21)) || ( (c!!1 == c!!11) && (c!!1 == c!!21) )
threeCheck i c = False
gameCheck :: Bool -> String->Char->Int->IO()
gameCheck True game 'X' i = loopTic True game 'O' 'j' i
gameCheck True game 'O' i = loopTic True game 'X' 'j' i
gameCheck False game c i = do
   putStrLn([c]++" pick A B C D E F G H or I")
   putStrLn game
   d<-getChar
   loopTic False game c d (i)

loopTic:: Bool -> String->Char->Char->Int->IO()
loopTic False game c p 9 = do
 putStrLn("TIE GAME")
 putStrLn(game)
loopTic True game c p i = do 
  putStrLn([c]++"!!! YOU HAVE WON THE GAME!!!!!!!")
  putStrLn(game)
loopTic False game 'X' p i= do 
  a<-getChar
  let newgame  = tictac game 'X' p 
  gameCheck (threeCheck (findInt p) newgame) newgame 'O' (i+1)
loopTic False game 'O' p i = do 
  a<-getChar
  let newgame  = tictac game 'O' p
  gameCheck (threeCheck (findInt p) newgame) newgame 'X' (i+1)

tictac :: String -> Char-> Char -> String
tictac game c p = (take (findInt p) game ) ++ [c]  ++ drop ((findInt p)+1) game 

main = do  
  putStrLn("LET'S PLAY TIC TAC TOE")
  let game = "|A|B|C|\n|D|E|F|\n|G|H|I|\n" 
  putStrLn(game)
  putStrLn("X pick A B C D E F G H or I")
  position <- getChar
  loopTic False game 'X' position 1
  
