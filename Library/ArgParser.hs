module Library.ArgParser where

import Data.List

type ArgName = String
type ArgHandles = [String]
type ArgHelptext = String
type EasyArgDefinitions = (String, [String], String)
data ArgDefinitions = ArgDefinitions { name :: ArgName, handles :: ArgHandles, helpText :: ArgHelptext }
data ProgramArguments = ProgramArguments { progTitle :: String, argDefs :: [ArgDefinitions] }

helpArgDef :: ArgDefinitions
helpArgDef = ArgDefinitions { name="help",
                              handles=["-h", "--help"],
                              helpText="Print this help text" }

buildProgArgFunc :: EasyArgDefinitions -> ArgDefinitions
buildProgArgFunc (name, handles, helptext) = ArgDefinitions { name=name,
                                                              handles=handles,
                                                              helpText=helptext }

buildProgramArguments :: String -> [EasyArgDefinitions] -> ProgramArguments
buildProgramArguments prog_title user_easyargs = let user_argdefs = map buildProgArgFunc user_easyargs in
                                                 ProgramArguments { progTitle=prog_title,
                                                                    argDefs=helpArgDef:user_argdefs }

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
