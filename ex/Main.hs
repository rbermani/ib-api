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

-- Modificaton by Phage Ky
import Data.Time.Calendar(toGregorian)
import Data.Time.Clock(utctDay,UTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import System.Console.ANSI


epochToUTC :: Integral a => a -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral

epochToGregorian :: Integral a => a -> (Integer,Int,Int)
epochToGregorian = toGregorian . utctDay . epochToUTC


handleMsg :: MIB -> IBMessage -> IO ()
handleMsg msv (CurrentTime time) =
    do s <- readMVar msv
       putStrLn $ "Current Time Response Received " ++ show time
handleMsg msv (ManagedAccts acct) =
    do putStrLn $ "Managed Accts: " ++ acct
handleMsg msv msg = putStrLn $ "Catch-all handler called for " ++ show msg


main :: IO ()
main = 
    do clearScreen >> setCursorPosition 0 0
       result <- connect defaultConf { cc_handler = Just handleMsg } False True
       case result of 
         Left err -> putStrLn "Unable to Connect"
         Right msv -> do setSGR []
                         setSGR [SetColor Foreground Vivid Yellow]
                         putStrLn " *** Connection Sucessful ..."
                         putStrLn " *** API is now ON - commence trading ... "
                         checkMsg msv False
                         businessLogic msv

{-- Original Code
businessLogic :: MIB -> IO ()
businessLogic msv =
    do s <- readMVar msv

       CTime ptime <- epochTime 
       when (s_connected s) $
            do checkMsg msv False
               when (((toInteger ptime) `mod` 120) == 0)  $
                 do putStrLn $ "Inside do block: " ++ show (toInteger ptime)
                    putStrLn "Requesting..."
                    request s CurrentTimeReq
       threadDelay (10^6)
       businessLogic msv
--}


businessLogic :: MIB -> IO ()
businessLogic msv =
    do checkMsg msv False
       s <- readMVar msv

       CTime ptime <- epochTime 
       when (s_connected s) $
            do putStrLn $ "Inside do block: " ++ show (toInteger ptime)
               setSGR [SetColor Foreground Vivid Yellow]
               putStrLn " *** Sending Request to IB-TWS ..."
               request s CurrentTimeReq
               checkMsg msv False
       hClose (fromJust $ s_sock s)
       putStrLn "... IB-TWS Connection Close"
       -- threadDelay (10^8)
       -- businessLogic msv

