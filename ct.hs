{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Data.Conduit.Network
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = runTCPServer (serverSettings 4000 "*") $ \appData ->
    runConduit (appSource appData .| mapMC print .| sinkNull)
