module Common (commonMain) where

import Criterion.Main (Benchmark, runMode)
import Criterion.Main.Options as Criterion
import Data.Maybe (fromMaybe)
import Data.Monoid
import Options.Applicative

commonMain :: Int                    -- ^ default maximum data size
           -> (Int -> [Benchmark])   -- ^ the benchmarks to run
           -> IO ()
commonMain mdMax bench = do
    (maybeNewMax, critMode) <- execParser $ info (helper <*> options) mempty
    runMode critMode $ bench (fromMaybe mdMax maybeNewMax)

options :: Parser (Maybe Int, Criterion.Mode)
options =
    (,) <$> optional (option auto (help "benchmark maximum data size"
                                   <> metavar "N" <> short 'i'  <> long "imax"))
        <*> Criterion.parseWith Criterion.defaultConfig
