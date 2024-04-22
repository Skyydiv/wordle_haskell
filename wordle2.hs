import System.Random
import Data.Char (ord)

main::IO ()
main = do
    --configuration
    x <- selectPrinting

    dic <- loadFile   --the dictionnary
    selected <-selectWord dic --the word selected

    doTurn selected x 1 dic

    putStrLn ("The word was: " ++ selected)
    

--Error Monade

data Erreur a = Err String| Value a

instance Functor Erreur where 
    fmap _ (Err s) = Err s
    fmap f (Value x) = Value (f x)

instance Applicative Erreur where 
    pure = Value

    (Err s) <*> _ = Err s
    (Value f) <*> something = fmap f something


instance Monad Erreur where
    return a = Value a

    (Err s) >>= g = Err s
    (Value x) >>= g = g x






--functions
asking::[String] -> IO (Erreur String)
asking dic = do
    putStrLn "Entréer un mot de 5 lettres"
    word <- getLine

    let inLen = verLength word
    let inWord = verAlpha inLen
    let verif = verDic inWord dic
    
    return verif --transform the Erreur String in a IO (String Erreur)


verLength::String -> Erreur String
verLength word = do 
    let l = length word
    if l > 5
        then (Err "word too long!")
    else if l< 5
        then (Err "word too short!")
    else (Value word)

verDic::Erreur String-> [String] -> Erreur String
verDic (Err e) _ = Err e
verDic (Value word) dic = do
    let res = foldl (\acc s -> (s==word) || acc) False dic
    if res
        then Value word
        else Err "Your word isn't in the dictionnary, it seems wrong"

verAlpha::Erreur String -> Erreur String
verAlpha (Err e) = (Err e)
verAlpha (Value w) = do
    -- [97,122] and [224,252] for [a,z] and [à,ü]
    let e = foldl (\acc c -> acc || ((ord c)<97) || (((ord c)>122) && (((ord c)<224) || ((ord c)>252)))) False w
    if e 
        then Err "Non alphabetic value in your word"
        else Value w

--Load a file and return a list of its list of his lines as a string list
loadFile::IO [String]
loadFile = do 
    content <- readFile "dictionaire.txt"
    return (lines content)

--Given a string list return a random string in it
selectWord:: [String] -> IO String
selectWord wordList = do
    generator <- newStdGen
    let (randomInt,other) = randomR (0,(length wordList)-1) generator ::(Int, StdGen)
    -- putStrLn (wordList !! randomInt)
    return (wordList !! randomInt)


-- check if a character is present in a word
checkLetter:: Char -> String -> Bool
checkLetter c w = foldl (\acc letter-> acc || (letter == c)) False w

--give the correct value for a letter
handleLetter::Char -> Int  -> String -> Int
handleLetter letter i word =

      if (word !! i) == letter --give the "word[i]" letter
        then 1
        else (
            if checkLetter letter word
                then 2
                else 0
        )

updateAcumulator::([Int],Int,Bool)-> Char -> String -> ([Int],Int,Bool)
updateAcumulator (list,count,found) letter ref=  do 
    --count : the possition of the current letter in the word that is checked
    --list : the begining of the int list representing the letter with the int associated
    let res = (handleLetter letter count ref)
    (list ++ [res],count+1,(found && (res == 1))) 


--Take 2 string, the first one to check and the second the reference one, return a int list representing a number for each letter folowing the rules of wordle game
checkword:: String -> String -> ([Int],Int,Bool)
checkword toCheck ref= 
    foldl (\acc letter -> updateAcumulator acc letter ref) ([],0,True) toCheck --aplis to each letter of toCheck the updateAculator methode wich appends to the accumulator with the correct value for the current letter of toCheck


doTurn :: String -> Int -> Int-> [String]-> IO ()
doTurn ref printM count dic  = do
    userWord <- asking dic --ask for a word
    case userWord of 
        Err e -> do
            putStrLn ("\nError: " ++ e)
            doTurn ref printM count dic


        Value word -> do
            let (valList,_,found) = checkword word ref --check the word
            printWord valList printM --print the corresponding response
            -- putStrLn ((show found) ++ " : " ++ (show count) )
            if found  
                then putStrLn "Well played you won!" 
            else if count < 5 
                then do 
                    putStrLn ("Error try again. " ++ (show (5-count)) ++ " Remaining\n")
                    doTurn ref printM (count+1) dic
                else
                    putStrLn "You lost, next time"



--printing functions using ANSI escape codes

printSquare :: IO ()
printSquare = putStr "\x25A0"

resetText :: IO ()
resetText = putStr "\x1b[0m"

redSquare :: IO ()
redSquare = do
    putStr "\x1b[31m "
    printSquare
    resetText

greenSquare :: IO ()
greenSquare = do
    putStr "\x1b[32m "
    printSquare
    resetText

yellowSquare :: IO ()
yellowSquare = do
    putStr "\x1b[2;33m "
    printSquare
    resetText




numToSquare :: Int -> IO ()
numToSquare x
    |x == 0 = redSquare
    |x == 1 = greenSquare
    |x == 2 = yellowSquare

numToLetter :: Int -> IO ()
numToLetter x
    |x == 0 = putStr "R|"
    |x == 1 = putStr "G|"
    |x == 2 = putStr "Y|"

listToLetter :: [Int] -> IO ()
listToLetter l = do
    putStrLn ""
    mapM_ numToLetter l
    putStrLn ""
    
 
listToSquares :: [Int] -> IO ()
listToSquares l= do
    putStrLn ""
    mapM_ numToSquare l
    putStrLn ""
    

selectPrinting :: IO Int
selectPrinting = do
    putStrLn "Select your display method: \n 1:Colored squares\n 2:Letters (Red/Green/Yellow)"
    x <- getLine
    putStrLn ""
    if (length x) == 0
        then return 1
    else if ((ord (x!!0)) /= (ord '2'))
        then return 1
        else return 2

printWord :: [Int]-> Int -> IO ()
printWord l x = 
    if (x == 2) then listToLetter l
    else listToSquares l
     