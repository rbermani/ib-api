{-# LANGUAGE DeriveDataTypeable #-}

module IB.Client.Exception 
  (
    -- * Types
    IBExceptionType(..)
  , IBException(..)
  , CodeMsgPair
 
    -- * Functions
  , excToPair

    -- * Accessors
  , code
  , msg
  ) where
  
import Control.Exception
import Data.Typeable

type CodeMsgPair = (Int, String)

data IBExceptionType 
    = AlreadyConnected 
    | ConnectFail 
    | UpdateTWS 
    | NotConnected 
    | UnknownID 
    | ZeroByteRead 
    | NullStringRead 
    | NoBytesRead 
    | SocketException 
    | FailCreateSock 
    | FailConnectTWS 
    | FailSendFARequest 
    | FailSendFAReplace 
    | FailSendReqScanner 
    | FailSendCanScanner 
    | FailSendReqScannerParameters 
    | FailSendReqHistData 
    | FailSendCanHistData 
    | FailSendReqRtBars 
    | FailSendCanRTBars 
    | FailSendReqCurrTime 
    | MissingHandler
    | ParseError
    | SystemError
        deriving (Show)

data IBException = IBExc Int IBExceptionType String
    deriving (Show, Typeable)
 
instance Exception IBException


code :: CodeMsgPair -> Int
code = fst

msg :: CodeMsgPair -> String
msg = snd

excToPair :: IBExceptionType -> CodeMsgPair
excToPair AlreadyConnected = (501,	"Already connected.") 
excToPair ConnectFail = (502, "Couldn't connect to TWS.  Confirm that \"Enable ActiveX and Socket Clients\" is enabled on the TWS \"Configure->API\" menu.") 
excToPair UpdateTWS = (503, "The TWS is out of date and must be upgraded.")
excToPair NotConnected = (504, "Not connected")
excToPair UnknownID = (505, "Fatal Error: Unknown message id.")
excToPair ZeroByteRead = (506, "Unexplained zero bytes read.")
excToPair NullStringRead = (507, "Null string read when expecting integer")
excToPair NoBytesRead = (508, "Error: no bytes read or no null terminator found")
excToPair SocketException = (509, "Exception caught while reading socket - ")
excToPair FailCreateSock = (520, "Failed to create socket")
excToPair FailConnectTWS = (521, "Couldn't connect to TWS.")
excToPair FailSendFARequest = (522, "FA Information Request Sending Error - ")
excToPair FailSendFAReplace  = (523, "FA Information Replace Sending Error - ")
excToPair FailSendReqScanner = (524, "Request Scanner Subscription Sending Error - ")
excToPair FailSendCanScanner = (525, "Cancel Scanner Subscription Sending Error - ")
excToPair FailSendReqScannerParameters = (526, "Request Scanner Parameter Sending Error - ")
excToPair FailSendReqHistData = (527, "Request Historical Data Sending Error - ")
excToPair FailSendCanHistData= (528, "Cancel Historical Data Sending Error - ")
excToPair FailSendReqRtBars = (529, "Request Real-time Bar Data Sending Error - ")
excToPair FailSendCanRTBars =(530, "Cancel Real-time Bar Data Sending Error - ")
excToPair FailSendReqCurrTime = (531, "Request Current Time Sending Error - ")

