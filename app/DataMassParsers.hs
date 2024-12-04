-- this module is only required to translate the challenge large input
module DataMassParsers (getFileLines) where

import qualified System.IO as SIO

getFileLines :: String -> IO [String]
getFileLines fileName = do
    dataMassFile <- SIO.openFile ("assets/" ++ fileName) SIO.ReadMode
    dataMass <- SIO.hGetContents dataMassFile
    return (lines dataMass)
