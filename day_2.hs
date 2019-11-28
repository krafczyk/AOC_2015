import Data.List.Split
import System.Environment
import System.IO
import Data.Typeable
import qualified Data.Map as Map
import qualified Library.ArgParser as AP

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

part_1 problem_data = let area_map = map (\[l, w, h] -> [l*w, l*h, w*h]) problem_data
                          min_area = map (minimum) area_map
                          pres_area = map (\x -> 2*sum x) area_map in
                      sum $ map (\(min,pres)-> min+pres) $ zip min_area pres_area

handleFile :: String -> IO ()
handleFile input_filepath = withFile input_filepath ReadMode (\handle -> do
                                                                         file_data <- hGetContents handle
                                                                         let problem_data = map (\vals -> map (\x -> read x ::Int) vals) $ map (splitOn "x") $ lines file_data
                                                                         putStrLn $ show $ part_1 problem_data)

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
       let progArgCont = AP.buildProgramArguments "day_2 Solves Advent of Code 2015 Day 2" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
