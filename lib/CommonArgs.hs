module CommonArgs where

import Data.Word (Word64)
import Options.Applicative

data CommonArgs = CommonArgs
  { learningRate :: Float,
    epoch :: Int,
    seed :: Maybe Word64,
    loadPath :: Maybe FilePath,
    savePath :: Maybe FilePath
  }

parseCommonArgs :: Parser CommonArgs
parseCommonArgs =
  CommonArgs
    <$> option auto (long "learning-rate" <> short 'l' <> help "Learning rate")
    <*> option auto (long "epoch" <> short 'e' <> help "Epoch" <> value 100)
    <*> optional (option auto (long "seed" <> short 's' <> help "Random seed"))
    <*> optional (strOption (long "load" <> help "Load model"))
    <*> optional (strOption (long "save" <> help "Save model"))

opts :: ParserInfo CommonArgs
opts =
  info
    (parseCommonArgs <**> helper)
    fullDesc

getArgs :: IO CommonArgs
getArgs = execParser opts
