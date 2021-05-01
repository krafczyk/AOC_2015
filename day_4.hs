import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Library.ArgParser as AP
import Data.Word
import Data.Typeable
import Text.Printf

argDefinitions = [ ("key", ["-k", "--key"], "Key to use", 1) ]

hexCharmap :: Map.Map Char Integer
hexCharmap = Map.fromList [('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6),
                           ('7', 7), ('8', 8), ('9', 9), ('a', 10), ('b', 11), ('c', 12), ('d', 13),
                           ('e', 14), ('f', 15)]

hexFoldFunc :: (Char, Integer) -> Maybe Integer -> Maybe Integer
hexFoldFunc (_,_) Nothing = Nothing
hexFoldFunc (c, b) (Just acc) = case Map.lookup c hexCharmap of
                                  Nothing -> Nothing
                                  Just cv -> Just ((cv*b)+acc)

fromHex :: String -> Maybe Integer
fromHex input = foldr hexFoldFunc (Just 0) $ zip (reverse input) (map (\x -> 16^x) [0..])


data ABCD = ABCD { a :: Word32, b :: Word32, c :: Word32, d :: Word32 } deriving (Show)

md5abcd_init :: Maybe ABCD
md5abcd_init = let jinit = (fromHex "67452301", fromHex "efcdab89", fromHex "98badcfe", fromHex "10325476") in
               case jinit of
                   (Nothing, _, _, _) -> Nothing
                   (_, Nothing, _, _) -> Nothing
                   (_, _, Nothing, _) -> Nothing
                   (_, _, _, Nothing) -> Nothing
                   (Just iA, Just iB, Just iC, Just iD) -> Just ABCD { a=fromIntegral iA, b=fromIntegral iB, c=fromIntegral iC, d=fromIntegral iD }

md5s_pieces :: [[Word32]]
md5s_pieces = [ [7, 12, 17, 22],
                [5, 9, 14, 20],
                [4, 11, 16, 23],
                [6, 10, 15, 21] ]

convertToWord32 = map (\x -> fromIntegral x :: Word32)

md5s = foldl (++) [] . foldl (++) [] . map (replicate 4) $ md5s_pieces
md5K = convertToWord32 . map (floor) . map ((2^32)*) . map (abs) . map (sin) . map (+1) $ take 64 [0..]

md5initInput input = let num_bytes = 512 `div` 8
                         bitlength = 8*(length input)
                         num_existing_blocks = bitlength `div` 512 in
                         --num_initial_bytes = bitlength `div` 8
                         --len_mod = bitlength `mod` 512
                         --byte_mod = num_initial_bytes `mod` num_bytes
                         --init_pad = 128 :: Word8
                         --num_zero_pad = 512 - ((num_initial_bytes+1+8) `mod` num_bytes) in
                     num_existing_blocks

build_complete :: String -> Int -> String
build_complete key n = key ++ show n

solveProblems :: String -> IO ()
solveProblems secret_key = do
                           putStrLn $ show $ md5initInput secret_key
                                         

argHelpHandler progArgs args = if AP.helpPresent progArgs args
                                   then AP.writeHelpText progArgs
                                   else let parse_result = AP.parseArguments progArgs args in
                                       case parse_result of
                                            Left msg -> putStrLn msg
                                            Right argMap -> case Map.lookup "key" argMap of
                                                                Just x -> let secret_key = (reverse x !! 0) !! 0 in
                                                                          solveProblems secret_key
                                                                Nothing -> putStrLn "Please pass the secret key with -k."

main = do
       args <- getArgs
       let progArgCont = AP.buildProgramArguments "Day template" argDefinitions
       case progArgCont of
           Left msg -> putStrLn msg
           Right progArgs -> argHelpHandler progArgs args
