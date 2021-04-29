import System.Environment
import System.IO
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Library.ArgParser as AP

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

specialFolder :: (Bool, Int, Int) -> Int -> (Bool, Int, Int)
specialFolder (True,idx,_) _= (True, idx, -1)
specialFolder (False,idx,acc) instruction = if acc+instruction == -1
                                                then (True, idx+1, acc+instruction)
                                                else (False, idx+1, acc+instruction)

handleFile :: String -> IO ()
handleFile input_path = withFile input_path ReadMode (\handle -> do
                                                                 file_data <- hGetContents handle
                                                                 let int_list = foldr (\x acc -> if x == '(' then 1:acc else -1:acc) [] file_data
                                                                 putStrLn $ "Part 1: " ++ (show $ sum int_list)
                                                                 let (_, floor, _) = foldl specialFolder (False, 0, 0) int_list
                                                                 putStrLn $ "Part 2: " ++ (show floor))

argHelpHandler progArgs args = if AP.helpPresent progArgs args
                                   then AP.writeHelpText progArgs
                                   else let parse_result = AP.parseArguments progArgs args in
                                       case parse_result of
                                            Left msg -> putStrLn msg
                                            Right argMap -> case Map.lookup "input_filepath" argMap of
                                                                Just x -> let input_filepath = (reverse x !! 0) !! 0 in
                                                                          handleFile input_filepath
                                                                Nothing -> putStrLn "Couldn't get input_filepath"

main = do
       args <- getArgs
       let progArgCont = AP.buildProgramArguments "day_1 Solves Advent of Code 2015 Day 1" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
