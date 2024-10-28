import System.Environment
import System.Exit
import GetOpts
import ParseFile

main :: IO ()
main = do
    options <- getArgs
    case (checkOpts (getOpts (Just defaultConf) options)) of
        (Just (Conf (Just n) (Just l) (Just f))) -> 
            (handleCompression n l f)
        _ -> exitWith (ExitFailure 84)