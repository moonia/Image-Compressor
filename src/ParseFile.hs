module ParseFile ( handleCompression, printOutput, getPixelsList ) where
import Kmeans
import Text.Read (readMaybe)
import System.Exit
import Control.Exception (try, IOException)
import Pixel

getPixel :: [[Maybe Int]] -> Maybe Pixel
getPixel (((Just x):(Just y):[]):((Just r):(Just g):(Just b):[]):[]) |
    ((r >= 0 && r <= 255) && (g >= 0 && g <= 255) &&
    (b >= 0 && b <= 255)) = Just (Pixel ((Just x), (Just y))
    ((Just r), (Just g), (Just b)))
    | otherwise = Nothing
getPixel _ = Nothing

getValuesLine :: String -> [[Maybe Int]]
getValuesLine str = fmap (\x -> (fmap
                    (\y -> readMaybe y :: Maybe Int) x)) 
                    (fmap (words) (fmap (trimLine) (words str)))

getPixelsList :: [String] -> [Maybe Pixel]
getPixelsList [] = []
getPixelsList (x:xs) = ((getPixel (getValuesLine x)) :
                        getPixelsList xs)

trimLine :: String -> String
trimLine [] = []
trimLine ('(':[]) = (' ' : [])
trimLine (')':[]) = (' ' : [])
trimLine (',':[]) = (' ' : [])
trimLine ('(':xs) = (' ' : trimLine (xs))
trimLine (')':xs) = (' ' : trimLine (xs))
trimLine (',':xs) = (' ' : trimLine (xs))
trimLine (x:xs) = (x : trimLine (xs))

handleCompression :: Int -> Float -> String -> IO ()
handleCompression k l filename = do
    content <- try $ (readFile filename) :: IO (Either IOException String)
    case content of
        Left _ -> exitWith (ExitFailure 84)
        Right validContent -> checkValidPixelsList k l 
            (getPixelsList (lines validContent))

checkValidPixelsList :: Int -> Float -> [Maybe Pixel] -> IO ()
checkValidPixelsList k l tabPixels =
    case (checkPixelsList tabPixels) of
        False -> exitWith (ExitFailure 84)
        True -> initKmeans (getValidPixelsList tabPixels) k l

getValidPixelsList :: [Maybe Pixel] -> [Pixel]
getValidPixelsList [] = []
getValidPixelsList (Just (Pixel (Just x, Just y)
            (Just r, Just g, Just b)):xs) = ((Pixel (Just x, Just y)
            (Just r, Just g, Just b)): getValidPixelsList (xs))
getValidPixelsList _ = []

printOutput :: [Maybe Pixel] -> IO ()
printOutput [] = putStr ""
printOutput (x:xs) = print x >> printOutput xs

checkPixelsList :: [Maybe Pixel] -> Bool
checkPixelsList [] = True
checkPixelsList (Just (Pixel (Just _, Just _)
            (Just _, Just _, Just _)):xs) = checkPixelsList xs
checkPixelsList _ = False