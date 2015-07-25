-- |
-- Module  : IB.Client
-- License : GPL3
-- Author : Robert Bermani <bobbermani@gmail.com>
-- Stability : experimental

-- IB Client

module IB.Client where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Attoparsec.ByteString.Char8 hiding (try)
import Data.Bits
import qualified Network.Socket as S hiding (send, recv, sendTo, recvFrom) 
import Network.Socket.ByteString 
import Network.BSD
import Data.List
import Data.Char
import System.Timeout
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.Maybe

import IB.Client.Types
import IB.Client.Nums
import IB.Client.Request
import IB.Client.Exception
import IB.Client.Parser

data ClientConfig = ClientConfig
    { cc_addr :: String
    , cc_port :: Int 
    , cc_clientId :: Int 
    , cc_extraAuth :: Bool
    , cc_handler :: Maybe HandlerFunc
    } 

disconnect :: MIB -> IO ()
disconnect smv = 
    do srv <- readMVar smv
       let h = fromJust (s_sock srv)
       hClose h
       putMVar smv srv { s_clientId = -1
                     , s_extraAuth = False
                     , s_version = 0
                     , s_connected = False
                     }

ascCodes :: B.ByteString -> String
ascCodes inp = unwords ( map (show . ord) (B.unpack inp) )
       
greetServer :: IBServer -> IO IBServer
greetServer server = 
    do write server $ appNull $ show' client_version
       wFlush server

       let h = fromJust (s_sock server )
           extraAuth = s_extraAuth server

       msg <- B.hGet h 25 
       prea <- parseWith (B.hGetNonBlocking h 25) pServerVersion msg

       case eitherResult prea of
         Left errMsg -> throwIO $ IBExc no_valid_id ParseError errMsg
         Right val   -> do let serv_ver =  pre_serverVersion val
                               twsTime = pre_twsTime val
                               sCo = server { s_connected = True
                                            , s_version = serv_ver
                                            , s_twsTime = twsTime 
                                            } 
                           case () of
                            _ | serv_ver < server_version -> throwIO $ IBExc no_valid_id UpdateTWS ""
                              | serv_ver >= 3 -> if (serv_ver < min_server_ver_linking)
                                                   then write sCo $ show' ( s_clientId sCo)
                                                   else if (not extraAuth)
                                                          then request sCo StartApi
                                                          else return ()
                              | otherwise -> return ()
                           wFlush sCo
                           return sCo 


checkMsg :: MIB -> Bool -> IO ()
checkMsg mvs loop =
    do s <- readMVar mvs
       let h = fromJust $ s_sock s
           handleMsg = s_handler s
           ver = s_version s
       eof <- timeout (s_timeoutInterval s) (hIsEOF h)

       case eof of
        Nothing -> putStrLn "EOF timeout encountered"
        Just True -> do putStrLn "EOF encountered on handle"
                        modifyMVar_ mvs (\serv -> return $ serv {s_sock = Nothing})
                        hClose h
        Just False -> do msg <- B.hGetNonBlocking h 1024
                         server <- takeMVar mvs
                         debugWrite server $ ">> " ++ B.unpack msg
                         putMVar mvs server
                         pResult <- parseWith (B.hGetNonBlocking h 1024) (pRecvMsg ver) msg 

                         case eitherResult pResult of 
                             Left errMsg -> throwIO $ IBExc no_valid_id ParseError errMsg
                             Right res -> handleMsg mvs $ rc_msgBody res 
                         
                         when loop $ checkMsg mvs loop

-- |Connects to a server
connect :: ClientConfig     -- ^ Configuration
           -> Bool          -- ^ Run in a new thread
           -> Bool          -- ^ Print debug messages
           -> IO (Either IOError MIB) -- ^ IB instance
 
connect cconf threaded debug = try $ do
    when debug $
        putStrLn $ "Connecting to " ++ cc_addr cconf ++ " on port " ++ show (cc_port cconf)

--    if (isConnected $ cc_socket cconf)
--             then throwIO IBExc no_valid_id AlreadyConnected ""
--             else return ()
    --(when ((cc_handler cconf) == Nothing) (throwIO $ IBExc no_valid_id MissingHandler ""))

    let hostStr = cc_addr cconf
        portStr = show $ cc_port cconf
        hostname | null hostStr = Nothing
                 | otherwise = Just hostStr 
    addrinfos <- S.getAddrInfo Nothing hostname (Just portStr)

    let serveraddr = head addrinfos
    s <- S.socket (S.addrFamily serveraddr) S.Stream defaultProtocol

    S.connect s (S.addrAddress serveraddr)
    h <- S.socketToHandle s ReadWriteMode 
    hSetBuffering h (BlockBuffering Nothing)
   
    let preServer = toServer cconf h debug

    -- initialize connection with server
    server <- greetServer preServer

    res <- newMVar server

    if threaded
        then do msgThreadId <- forkIO (checkMsg res True)
                modifyMVar_ res (\srv -> return $ srv { s_msgThread = Just msgThreadId 
                                                      } )
                return res
        else return res


toServer :: ClientConfig -> Handle -> Bool -> IBServer
toServer cc h debug = IBServer { s_addr = cc_addr cc
                               , s_port = cc_port cc
                               , s_clientId = cc_clientId cc
                               , s_extraAuth = cc_extraAuth cc
                               , s_handler = fromJust $ cc_handler cc
                               , s_debug = debug
                               , s_twsTime = ""
                               , s_msgThread = Nothing
                               , s_version = 0
                               , s_sock = Just h
                               , s_timeoutInterval = 100000
                               , s_connected = False
                               }

defaultConf :: ClientConfig 
defaultConf = ClientConfig { cc_addr = "127.0.0.1"
                           , cc_port = 7496
                           , cc_clientId = 0
                           , cc_extraAuth = False
                           , cc_handler = Just defHandler
                           } 

defHandler :: MIB -> IBMessage -> IO ()
defHandler _ _ = putStrLn "No Message Handler is defined" 

