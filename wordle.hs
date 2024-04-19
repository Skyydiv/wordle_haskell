import System.Random

main::IO ()
main = do
    --configuration
    x <- selectPrinting

    words <- loadFile   --the dictionnary
    selected <-selectWord words --the word selected

    doTurn selected x 1

    putStrLn ("The word was: " ++ selected)
    



asking::IO String
asking = do
    putStrLn "Entréer un mot de 5 lettres"
    getLine

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


doTurn :: String -> Int -> Int-> IO ()
doTurn ref printM count = do
    userWord <- asking --ask for a word
    let (valList,_,found) = checkword userWord ref --check the word
    printWord valList printM --print the corresponding response
    -- putStrLn ((show found) ++ " : " ++ (show count) )
    if found  
        then putStrLn "Well played you won!" 
    else if count < 5 
        then do 
            putStrLn ("Error try again. " ++ (show (5-count)) ++ " Remaining\n")
            doTurn ref printM (count+1)
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
    mapM_ numToLetter l
    putStrLn ""
 
listToSquares :: [Int] -> IO ()
listToSquares l= do
    mapM_ numToSquare l
    putStrLn ""

selectPrinting :: IO Int
selectPrinting = do
    putStrLn "Select your display method: \n 1:Colored squares\n 2:Letters (Red/Green/Yellow)"
    x <- getLine
    return ( read x :: Int) 

printWord :: [Int]-> Int -> IO ()
printWord l x = 
    if (x == 2) then listToLetter l
    else listToSquares l
     