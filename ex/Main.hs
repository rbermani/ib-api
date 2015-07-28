-- |
-- Module  : IB.Client
-- License : GPL3
-- Author : Robert Bermani <bobbermani@gmail.com>
-- Stability : experimental
-- |

-- Main Loop example

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Attoparsec.ByteString.Char8 hiding (try)
import Data.Bits
import qualified Network.Socket as S hiding (send, recv, sendTo, recvFrom) 
import Network.Socket.ByteString 
import Network.BSD
import Data.List
import System.Timeout
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import System.Posix.Time
import System.Posix.Types
import Foreign.C.Types
import Data.IORef

import IB.Client.Types
import IB.Client.Nums
import IB.Client.Request
import IB.Client.Exception
import IB.Client.Parser
import IB.Client


handleMsg :: MIB -> IBMessage -> IO ()
handleMsg msv (CurrentTime time) =
    do s <- readMVar msv
       putStrLn $ "Current Time Response Received " ++ show time
handleMsg msv (ManagedAccts acct) =
    do putStrLn $ "Managed Accts: " ++ acct
handleMsg msv msg = putStrLn $ "Catch-all handler called for " ++ show msg

{--
nothreadEx = do let cconf = defaultConf { cc_handler = Just handleMsg } 
                result <- connect cconf False False
                case result of 
                 Left err ->
                 Right msv -> do s <- readMVar msv
                                 unless (not s_connected s) 
                                    do checkMsg False 

threadEx = do let cconf = defaultConf { cc_handler = Just handleMsg } 
                  result <- connect cconf True False
                  case result of 
                   Left err ->
                   Right msv -> do s <- readMVar msv

                  unless (not s_connected s) checkMsg False 
--}
main :: IO ()
main = 
    do result <- connect defaultConf { cc_handler = Just handleMsg } False True
       case result of 
         Left err -> putStrLn "Unable to Connect"
         Right msv -> do businessLogic msv


businessLogic :: MIB -> IO ()
businessLogic msv =
    do s <- readMVar msv

       CTime ptime <- epochTime 
       when (s_connected s) $
            do checkMsg msv False
               when (((toInteger ptime) `mod` 120) == 0)  $
                 do putStrLn $ "Inside do block" ++ show (toInteger ptime)
                    request s CurrentTimeReq
       threadDelay (10^6)
       businessLogic msv
