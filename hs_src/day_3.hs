import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP

argDefinitions = [ ("input_filepath", ["-i", "--input-file"], "Filepath to use for input", 1) ]

type HLocation = (Int,Int)
type HMap = Map.Map (Int,Int) Int

move :: Char -> Maybe HLocation -> Maybe HLocation
move _ Nothing = Nothing
move dir (Just (x,y))
    | dir == '^' = Just (x, y+1)
    | dir == '>' = Just (x+1, y)
    | dir == '<' = Just (x-1, y)
    | dir == 'v' = Just (x, y-1)
    | otherwise = Nothing

updateMap :: Maybe HMap -> Maybe HLocation -> Maybe HMap
updateMap Nothing _ = Nothing
updateMap _ Nothing = Nothing
updateMap (Just hmap) (Just x) = case Map.lookup x hmap of
                              Nothing -> Just (Map.insert x 1 hmap)
                              Just y -> Just (Map.insert x (y+1) hmap)

mapFoldFunc :: (Maybe HLocation, Maybe HMap) -> Char -> (Maybe HLocation, Maybe HMap)
mapFoldFunc (curLoc, hmap) dir = let newLoc = move dir curLoc in
                                 (newLoc, updateMap hmap $ newLoc)

buildMap :: String -> (Maybe HLocation, Maybe HMap)
buildMap map_data = foldl mapFoldFunc (Just (0,0), Just (Map.fromList [((0,0),1)])) $ map_data

handleFile :: String -> IO ()
handleFile input_filepath = withFile input_filepath ReadMode (\handle -> do
                                                                         file_data <- hGetContents handle
                                                                         let problem_data = (lines file_data) !! 0
                                                                             (_, jhMap) = buildMap problem_data
                                                                         case jhMap of
                                                                             Nothing -> putStrLn "ERROR: Didn't build house map correctly!"
                                                                             Just hMap -> putStrLn $ "Task 1: " ++ (show $ Map.size hMap)
                                                                         let santa_data = map (fst) $ filter (\(c,idx) -> (idx `mod` 2) == 0) $ zip problem_data [0..]
                                                                             robo_data = map (fst) $ filter (\(c,idx) -> idx `mod` 2 == 1) $ zip problem_data [0..]
                                                                             (_, jSantaMap) = buildMap santa_data
                                                                             (_, jRoboMap) = buildMap robo_data
                                                                         case (jSantaMap, jRoboMap) of
                                                                             (Nothing, Nothing) -> putStrLn $ "ERROR: didn't build santa map correctly!"
                                                                             (Nothing, _) -> putStrLn $ "ERROR: didn't build santa map correctly!"
                                                                             (_, Nothing) -> putStrLn $ "ERROR: didn't build robo map correctly!"
                                                                             (Just santaMap, Just roboMap) -> let cMap = Map.unionWith (+) santaMap roboMap in
                                                                                                              putStrLn $ "Task 2: " ++ (show $ Map.size cMap))

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
       let progArgCont = AP.buildProgramArguments "day 3 solves Advent of Code 2015 Day 3" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
