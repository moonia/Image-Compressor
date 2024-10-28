module GetOpts ( getOpts, defaultConf, Conf (..), checkOpts ) where

import Text.Read (readMaybe)

data Conf = Conf (Maybe Int) (Maybe Float) (Maybe String)
            deriving (Show, Eq)

defaultConf :: Conf
defaultConf = Conf (Nothing) (Nothing) (Nothing)

updateConf :: Maybe Conf -> String -> String -> Maybe Conf
updateConf (Just (Conf _ l f)) "-n" value =
      case (readMaybe value :: Maybe Int) of
            (Just n) -> if (n >= 0) then Just (Conf (Just n) l f)
                        else Nothing
            Nothing -> Nothing
updateConf (Just (Conf n _ f)) "-l" value =
      case (readMaybe value :: Maybe Float) of
            (Just l) ->  if (l >= 0) then Just (Conf n (Just l) f)
                        else Nothing
            Nothing -> Nothing
updateConf (Just (Conf n l _)) "-f" value = Just (Conf n l (Just value))
updateConf _ _ _ = Nothing

getOpts :: Maybe Conf -> [String] -> Maybe Conf
getOpts (Just conf) [] = Just conf
getOpts (Just conf) ("-n":y:xs) =
      getOpts (updateConf (Just conf) "-n" y) (xs)
getOpts (Just conf) ("-l":y:xs) = 
      getOpts (updateConf (Just conf) "-l" y) (xs)
getOpts (Just conf) ("-f":y:xs) =
      getOpts (updateConf (Just conf) "-f" y) (xs)
getOpts _ _ = Nothing

checkOpts :: Maybe Conf -> Maybe Conf
checkOpts (Just (Conf (Just n) (Just l) (Just f))) =
      (Just (Conf (Just n) (Just l) (Just f)))
checkOpts _ = Nothing