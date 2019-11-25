module ArgParser where

type ArgHandles = [String]
type ArgHelptext = String
data ArgDefinitions = ArgDefinitions { getHandles :: ArgHandles, getHelptext :: ArgHelptext }
data ProgramArguments = ProgramArguments { progTitle :: String, argDefs :: [ArgDefinitions] }
