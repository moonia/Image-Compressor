module Pixel ( Pixel (..) ) where

data Pixel = Pixel (Maybe Int, Maybe Int)
                   (Maybe Int, Maybe Int, Maybe Int)
            deriving (Eq)

instance Show Pixel where
    show (Pixel (Just x, Just y)
       (Just r, Just g, Just b)) = show (x, y) ++ " " ++ show (r, g, b)
    show (Pixel (_, _) (_, _, _)) = show ""