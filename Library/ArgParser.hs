module Library.ArgParser where

import Data.List
import qualified Data.Map as Map

type ArgName = String
type ArgHandles = [String]
type ArgHelptext = String
type EasyArgDefinitions = (String, [String], String, Int)
data ArgDefinitions = ArgDefinitions { name :: ArgName, handles :: ArgHandles, helpText :: ArgHelptext, numArgs :: Int }
data ProgramArguments = ProgramArguments { progTitle :: String, argDefs :: [ArgDefinitions] }

helpArgDef :: ArgDefinitions
helpArgDef = ArgDefinitions { name="help",
                              handles=["-h", "--help"],
                              helpText="Print this help text",
                              numArgs=0 }

buildProgArgFunc :: EasyArgDefinitions -> ArgDefinitions
buildProgArgFunc (name, handles, helptext) = ArgDefinitions { name=name,
                                                              handles=handles,
                                                              helpText=helptext }

checkForRepeated :: [String] -> Maybe String
checkForRepeated list = let (test, val, _) = foldl (\(test, val, accl) x ->
                                                        if test then
                                                            (test, val, accl)
                                                        else
                                                            if x `elem` accl then
                                                                (True, x, x:accl)
                                                            else
                                                                (False, "", x:accl)) (False, "", []) list in
                        if test then
                            Just val
                        else
                            Nothing

argNameConflicts :: [EasyArgDefinitions] -> Maybe String
argNameConflicts easyargs = let arg_names = map (\(n, _, _) -> n) easyargs in
                            checkForRepeated arg_names

argHandleConflicts :: [EasyArgDefinitions] -> Maybe String
argHandleConflicts easyargs = let handle_names = foldr (\x acc -> x++acc) [] $ map (\(_, h, _) -> h) easyargs in
                              checkForRepeated handle_names

buildProgramArguments :: String -> [EasyArgDefinitions] -> Either String ProgramArguments
buildProgramArguments prog_title user_easyargs = case (argNameConflicts user_easyargs) of
                                                     Nothing -> case (argHandleConflicts user_easyargs) of
                                                                Nothing -> let user_argdefs = map buildProgArgFunc user_easyargs in
                                                                           Right ProgramArguments { progTitle=prog_title,
                                                                                                    argDefs=helpArgDef:user_argdefs }
                                                                Just x -> Left $ "ArgParser ERROR: Argument handle " ++ x ++ " conflicts."
                                                     Just x -> Left $ "ArgParser ERROR: Argument name " ++ x ++ " conflicts."

printArgDef :: ArgDefinitions -> IO ()
printArgDef argdef = let rep_str = intercalate "/" (handles argdef)
                         name_str = name argdef
                         help_str = helpText argdef in
                     putStrLn $ rep_str ++ ": stored as " ++ name_str ++ " = " ++ help_str

writeHelpText :: ProgramArguments -> IO ()
writeHelpText progArgs = do
                         putStrLn $ progTitle progArgs
                         putStrLn $ "Accepts arguments"
                         mapM_ (printArgDef) $ argDefs progArgs

getNamePositions :: String -> [String] -> [Int]
getNamePositions name args = fold (\x acc -> ) (0, []) args

argFoldFunc :: (Int, String, [String], Map String [[String]]) -> String -> (Int, String, [String], Map String [[String]])
argFoldFunc (State, 

parseArguments :: ProgramArguments -> [String] -> Either String (Map String [[String]])
parseArguments progArgs args = 
