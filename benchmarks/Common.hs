module Common (commonMain) where

import Criterion.Main (Benchmark, defaultMain)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment (getArgs, withArgs)
import Text.Read (readEither)

-- We request and extra Int parameter 'mdMax' so that
-- it is possible to have per module default benchmark
-- data size.
commonMain :: Int -> (Int -> [Benchmark]) -> IO ()
commonMain mdMax bench = do
    (maybeNewMax, critArgs) <- parseArgs =<< getArgs
    
    withArgs critArgs $ defaultMain $ bench $ fromMaybe mdMax maybeNewMax

options :: [OptDescr (Either String Int)]
options =
    [
      Option ['i'] ["imax"] (ReqArg readEither "vmax")
      "Set benchmark maximum data size."
    ]

parseArgs :: [String] -> IO (Maybe Int, [String])
parseArgs args =
    case getOpt Permute options args of
        ([], rst, [])   -> return (Nothing, rst)
        ([ev], rst, []) -> either (usrErr prsErrMsg) (validate rst) ev
        (_, _, errs)    -> usrErr "" $ concat errs
    where
        usrErr ce = ioError . userError . (++) ce
        prsErrMsg = "-i requires a natural number >= 1 : "
        validate rst v
            | v >= 1 = return (Just v, rst)
            | otherwise = usrErr prsErrMsg (show v)
