import System.Environment
import Data.Typeable
import System.IO
import Control.DeepSeq

helpText :: [String]
helpText = ["day_1 Solves Advent of Code 2015 Day 1",
            "Accepts arguments",
            "-h/--help: Display this help",
            "-i/--input-file: file path to the input file"]

getInputFileArgFoldFunc :: (Maybe Int, Int) -> String -> (Maybe Int, Int)
getInputFileArgFoldFunc (acc, idx) arg = if acc /= Nothing then
                                            (acc, idx)
                                         else
                                            if ("-i" == arg) || ("--input-file" == arg) then
                                                (Just idx, idx)
                                            else
                                                (Nothing, idx+1)

getInputFileArg :: [String] -> Maybe String
getInputFileArg args = let idx = fst $ foldl getInputFileArgFoldFunc (Nothing, 0) args in
                       case idx of
                           Nothing -> Nothing
                           Just idx_i -> Just (args !! (idx_i+1))

handleHelp :: IO ()
handleHelp = mapM_ putStrLn helpText

handleInput :: [String] -> IO ()
handleInput args = let input_result = getInputFileArg args in
                   case input_result of
                       Nothing -> putStrLn "No Input filepath!"
                       Just input_path -> handleFile input_path

specialFolder :: (Bool, Int, Int) -> Int -> (Bool, Int, Int)
specialFolder (True,idx,_) _= (True, idx, -1)
specialFolder (False,idx,acc) instruction = if acc+instruction == -1 then
                                              (True, idx+1, acc+instruction)
                                          else
                                              (False, idx+1, acc+instruction)

handleFile :: String -> IO ()
handleFile input_path = withFile input_path ReadMode (\handle -> do
                                                                 file_data <- hGetContents handle
                                                                 let int_list = foldr (\x acc -> if x == '(' then 1:acc else -1:acc) [] file_data
                                                                 putStrLn $ "Part 1: " ++ (show $ sum int_list)
                                                                 let (_, floor, _) = foldl specialFolder (False, 0, 0) int_list
                                                                 putStrLn $ "Part 2: " ++ (show floor))


main = do
    -- Get arguments
    args <- getArgs
    -- Primitive argument parsing
    if ("-h" `elem` args) || ("--help" `elem` args) then
        handleHelp
    else 
        handleInput args
