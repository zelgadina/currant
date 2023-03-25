import Control.Monad
import System.Environment
import System.Directory
import System.IO
import qualified Data.List as DL
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "delete" = delete
dispatch "bump" = bump
dispatch "help" = help
dispatch command = doesntExist command

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks
    putStrLn "My tasks:"
    mapM_ putStrLn numberedTasks

delete :: [String] -> IO ()
delete [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        newTodoItem = unlines $ DL.delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "./tasks" "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItem
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        bumpingString = todoTasks !! number
        newTodoItem = unlines $ swap todoTasks number (number - 1)
    bracketOnError (openTempFile "./tasks" "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItem
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)

help :: [String] -> IO ()
help _ = putStrLn "Commands:\nadd fileName todoItem\nview fileName\ndetele fileName todoNumber\nbump fileName todoNumber"

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "Command " ++ command ++ " doesn't exist."

swap :: [a] -> Int -> Int -> [a]
swap xs i j
    | i > j = swap xs j i
    | otherwise = take i xs ++ [xj] ++ drop (i + 1) (take j xs) ++ [xi] ++ drop (j + 1) xs
        where xi = xs !! i
              xj = xs !! j

