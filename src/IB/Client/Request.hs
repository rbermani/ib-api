{-# LANGUAGE NamedFieldPuns #-}

module IB.Client.Request where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when, unless)
import Text.Printf
import qualified System.IO as S
import qualified Data.ByteString.Char8 as B
import Data.Maybe

import IB.Client.Exception
import IB.Client.Nums
import IB.Client.Types

import System.Console.ANSI

data ReqHeader = 
    ReqHeader 
    { rqh_msgId :: Int 
    , rqh_proVer :: Int
    , rqh_errId :: Int
    , rqh_errMsg :: String
    , rqh_minVer :: Maybe Int
    , rqh_exAuth :: Maybe Bool
    }

defReqHeader = ReqHeader 1 1 no_valid_id "" Nothing Nothing

(<++>) :: B.ByteString -> B.ByteString -> B.ByteString
a <++> b =  a `B.append` nullch `B.append` b 

debugWrite :: IBServer -> String -> IO ()
debugWrite s msg =
    when (s_debug s) $ putStrLn msg

write :: IBServer -> B.ByteString -> IO ()
write s msg = do
    setSGR [SetColor Foreground Dull Cyan]
    putStrLn " *** Writing Msg to IB Server ..."
    debugWrite s $ " IB-server << " ++ B.unpack msg 
    B.hPutStr h msg 
    where h = fromJust $ s_sock s

writeLst :: IBServer -> [B.ByteString] -> IO ()
writeLst s bsl = 
    do let outbs = (B.intercalate nullch bsl) `B.append` nullch
       debugWrite s $ "<< " ++ B.unpack outbs
       B.hPutStr h outbs
       where h = fromJust $ s_sock s

wFlush :: IBServer -> IO ()
wFlush s = S.hFlush h
    where h = fromJust $ s_sock s

show' :: Show a => a -> B.ByteString
show' = B.pack . show

nullch :: B.ByteString
nullch = B.pack "\0"

appNull :: B.ByteString -> B.ByteString
appNull bin = bin `B.append` nullch

encodeDbl :: Double -> B.ByteString 
encodeDbl val = B.pack (printf "%.2f" val)

encodeIntMax :: Int -> B.ByteString 
encodeIntMax val 
    | val == int32max = B.pack "" 
    | otherwise = show' val

encodeDblMax :: Double -> B.ByteString 
encodeDblMax val
    | val == dblMaximum = B.pack "" 
    | otherwise = encodeDbl val

encodeExecutionFilter :: ExecutionFilter -> B.ByteString 
encodeExecutionFilter exf = appNull (show' ( exf_clientId exf)
                                    <++> B.pack (exf_acctCode exf)
                                    <++> B.pack ( exf_time exf)
                                    <++> B.pack ( exf_symbol exf)
                                    <++> B.pack ( exf_secType exf)
                                    <++> B.pack ( exf_exchange exf)
                                    <++> B.pack ( exf_side exf ))
                                    

encodeSubscription :: ScannerSubscription -> B.ByteString 
encodeSubscription subs = appNull $ encodeIntMax ( ssb_numberOfRows subs)
                             <++> B.pack ( ssb_instrument subs)
                             <++> B.pack ( ssb_locationCode subs)
                             <++> B.pack ( ssb_scanCode subs)
                             <++> encodeDblMax ( ssb_abovePrice subs)
                             <++> encodeDblMax ( ssb_belowPrice subs)
                             <++> encodeIntMax ( ssb_aboveVolume subs)
                             <++> encodeDblMax ( ssb_marketCapAbove subs)
                             <++> encodeDblMax ( ssb_marketCapBelow subs)
                             <++> B.pack ( ssb_moodyRatingAbove subs)
                             <++> B.pack ( ssb_moodyRatingBelow subs)
                             <++> B.pack ( ssb_spRatingAbove subs)
                             <++> B.pack ( ssb_spRatingBelow subs)
                             <++> B.pack ( ssb_maturityDateAbove subs)
                             <++> B.pack ( ssb_maturityDateBelow subs)
                             <++> encodeDblMax ( ssb_couponRateAbove subs)
                             <++> encodeDblMax ( ssb_couponRateBelow subs)
                             <++> encodeIntMax ( ssb_excludeConvertible subs)
                             <++> encodeIntMax ( ssb_averageOptionVolumeAbove subs)
                             <++> B.pack ( ssb_scannerSettingPairs subs)
                             <++> B.pack ( ssb_stockTypeFilter subs)
                             
                              
encodeTagValue :: TagValue -> B.ByteString 
encodeTagValue tv = B.pack $ tv_tag tv ++ "=" ++ tv_value tv ++ ";" 
 
encodeTagValueList :: [TagValue] -> B.ByteString
encodeTagValueList tvl = B.concat $ map encodeTagValue tvl

encodeUnderComp :: UnderComp -> B.ByteString 
encodeUnderComp uc = appNull $ show' 1 <++> show' ( uc_conId uc )
                        <++> show' ( uc_price uc)

encodeComboLeg :: ComboLeg -> B.ByteString 
encodeComboLeg cl =  appNull $ show' ( cl_conId cl)
                        <++> show' ( cl_ratio cl )
                        <++> B.pack ( cl_action cl )
                        <++> B.pack ( cl_exchange cl)

encodeComboLegList :: [ComboLeg] -> B.ByteString 
encodeComboLegList cll = appNull $ show' ( length cll) <++> B.concat ( map encodeComboLeg cll )

encodeContract :: IBServer -> Contract -> Bool -> IO B.ByteString 
encodeContract s con pexch = 
    do let serv_ver = s_version s 
           bs | serv_ver >= min_server_ver_trading_class = show' $ ct_conId con
              | otherwise = B.empty 
           out = bs <++> B.pack ( ct_symbol con)
                    <++> B.pack ( ct_secType con)
                    <++> B.pack ( ct_expiry con)
                    <++> show' ( ct_strike con)
                    <++> B.pack ( ct_right con)
                    <++> B.pack ( ct_multiplier con)
                    <++> B.pack ( ct_exchange con)
           out' | pexch = out <++>  B.pack ( ct_primaryExchange con )
                | otherwise = out
           out'' = out' <++> B.pack ( ct_currency con)
                        <++> B.pack ( ct_localSymbol con)

       if serv_ver >= min_server_ver_trading_class
            then return $ appNull $ out'' <++> B.pack ( ct_tradingClass con)
            else return $ appNull out'' 
 
getHeaderCon :: IBServer -> ReqHeader -> Contract -> IO B.ByteString 
getHeaderCon s rqh con = 
    do let hndle =  s_sock s 
           connected = s_connected s 
           serv_ver = s_version s 
           mExtraAuth = s_extraAuth s 
 
       case () of
        _ | not connected -> throwIO $ IBExc (rqh_errId rqh) NotConnected ""  
          | (rqh_minVer rqh) /= Nothing -> 
                when ((serv_ver < fromJust (rqh_minVer rqh)) && not ( null ( ct_tradingClass con)) || ct_conId con > 0)
                    $ throwIO $ IBExc (rqh_errId rqh) UpdateTWS (rqh_errMsg rqh)
          | otherwise -> return ()          

       return $ appNull $ show' ( rqh_msgId rqh) <++> show' ( rqh_proVer rqh)

getHeader :: IBServer -> ReqHeader -> IO B.ByteString 
getHeader s rqh = 
    do let hndle =  s_sock s 
           connected = s_connected s 
           serv_ver = s_version s 
           mExtraAuth = s_extraAuth s 
 
       case () of
        _ | not connected -> throwIO $ IBExc (rqh_errId rqh) NotConnected ""  
          | (rqh_minVer rqh) /= Nothing -> when (serv_ver < fromJust (rqh_minVer rqh)) $ throwIO $ IBExc (rqh_errId rqh) UpdateTWS (rqh_errMsg rqh)

          | (rqh_exAuth rqh) /= Nothing -> unless mExtraAuth $ throwIO $ IBExc no_valid_id UpdateTWS "  Intent to authenticate needs to be expressed during initial connect request."
          | otherwise -> return ()

       return $ show' ( rqh_msgId rqh) <++> show' ( rqh_proVer rqh)

request :: IBServer -> Request -> IO ()

request s inp @ (MktDataReq { }) = 
    do let hndle =  s_sock s 
           connected = s_connected s 
           serv_ver = s_version s 
           mExtraAuth = s_extraAuth s 
           ct = mdr_contract inp
           tickerId = rqp_tickerId inp
           version = 11

       case () of
        _ | not connected -> throwIO $ IBExc tickerId NotConnected ""  
          | serv_ver < min_server_ver_under_comp && (ct_underComp ct /= undefined) -> throwIO $ IBExc tickerId UpdateTWS "  It does not support fundamental data requests." 
          | serv_ver < min_server_ver_req_mkt_data_conid && (ct_conId ct > 0) -> throwIO $ IBExc tickerId UpdateTWS "  It does not support conId parameter."  
          | serv_ver < min_server_ver_trading_class && not ( null (ct_tradingClass ct)) -> throwIO $ IBExc tickerId UpdateTWS "  It does not support tradingClass parameter in reqMktData."   
          | otherwise -> return ()

       let bs = show' (reqToId inp)
                 <++> show' version
                 <++> show' tickerId
           conbs | serv_ver >= min_server_ver_req_mkt_data_conid = show' $ ct_conId ct 
                 | otherwise = B.empty

           bs' = bs <++> conbs
            <++> B.pack ( ct_symbol ct )
            <++> B.pack ( ct_secType ct)
            <++> B.pack ( ct_expiry ct)
            <++> show' ( ct_strike ct)
            <++> B.pack ( ct_right ct)
            <++> B.pack ( ct_multiplier ct )
            <++> B.pack ( ct_exchange ct)
            <++> B.pack ( ct_primaryExchange ct)
            <++> B.pack ( ct_currency ct)
            <++> B.pack ( ct_localSymbol ct)

           tclass | serv_ver >= min_server_ver_trading_class = (show' $ ct_tradingClass ct )
                  | otherwise = B.empty

           clist | compare (ct_secType ct) "BAG" == EQ = encodeComboLegList (ct_comboLegsList ct) 
                 | otherwise = B.empty
           ucomp | serv_ver >= min_server_ver_under_comp && ct_underComp ct /= undefined = encodeUnderComp (ct_underComp ct) 
                 | otherwise = show' 0
           bs'' = bs'  
                    <++> tclass 
                    <++> clist 
                    <++> ucomp 
                    <++> B.pack ( mdr_genericTicks inp )
                    <++> show' ( fromBool ( mdr_snapshot inp))

       if serv_ver >= min_server_ver_linking
           then write s $ appNull $ bs'' <++>  encodeTagValueList ( mdr_mktDataOptions inp)
           else write s $ appNull bs'' 

       wFlush s

request s rq @ (CancelMktData { rqp_tickerId = tid }) =
    do  hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                                        , rqh_proVer = 2
                                        , rqh_errId = tid
                                        }
        write s $ appNull $ hdr <++> show' tid
        wFlush s

request s rq @ (PlaceOrder { rqp_orderId = oid }) =
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                                       , rqh_proVer = 2
                                       , rqh_errId = oid
                                       }
       write s $ appNull $ hdr <++> show' oid
       wFlush s
-- TODO PlaceOrder Complete

request s rq @ (CancelOrder oid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                                       , rqh_errId = oid
                                       }
       write s $ appNull $ hdr <++> show' oid
       wFlush s

request s rq @ OpenOrdersReq = do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq } 
                                  write s hdr 
                                  wFlush s

request s rq @ (AccountUpdatesReq {aur_subscribe = subscribe, aur_acctCode = acctCode}) = 
     do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                                        , rqh_proVer = 2
                                        }
        write s $ appNull $ show' ( fromBool subscribe) <++> B.pack acctCode
        wFlush s

request s rq @ (ExecutionsReq req_id exc_filt) =
    do  hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                                      , rqh_proVer = 3
                                      }
        let serv_ver = s_version s 
            reqbs | serv_ver >= min_server_ver_execution_data_chain = show' req_id
                  | otherwise = B.empty

        write s $ appNull $ hdr <++> reqbs <++> encodeExecutionFilter exc_filt
        wFlush s

request s rq @ (IdsReq numIds) = do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq, rqh_errId = numIds }
                                    write s $ appNull $ hdr <++> show' numIds
                                    wFlush s
--TODO
--request s rq @ (ContractDetailsReq req_id contract) = 

request s rq @ (MktDepthReq { rqp_tickerId = tid
                            , mkr_contract = con
                            , mkr_numRows = numRows
                            , mkr_mktDepthOptions = mkDepthOpts
                            }) =  
    do let serv_ver = s_version s 

       hdr <- getHeaderCon s defReqHeader { rqh_msgId = reqToId rq
                                        , rqh_proVer = 5
                                        , rqh_minVer = Just min_server_ver_trading_class
                                        , rqh_errId = tid
                                        , rqh_errMsg = "  It does not support conId and tradingClass parameters in reqMktDepth."
                                        } con
       con' <- encodeContract s con False

       write s $ appNull $ hdr <++> show' tid <++> con' <++> show' numRows

       when (serv_ver >= min_server_ver_linking) $
           write s $ appNull $ encodeTagValueList mkDepthOpts

       wFlush s
 
request s rq @ (CancelMktDepth tid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq,
                                 rqh_errId = tid}
       write s $ appNull $ hdr <++> show' tid 
       wFlush s

request s rq @ (NewsBulletinsReq allMsgs) =
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq}
       write s $ appNull $ hdr <++> show' ( fromBool allMsgs)
       wFlush s

request s rq @ CancelNewsBulletins = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq } 
       write s $ appNull hdr

request s rq @ (SetServerLogLevel llvl) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq }
       write s $ appNull $ hdr <++> show' llvl
       wFlush s

request s rq @ (AutoOpenOrdersReq autoBind) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq }
       write s $ appNull $ hdr <++> show' (fromBool autoBind)
       wFlush s

request s rq @ AllOpenOrdersReq =
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq } 
       write s $ appNull hdr
       wFlush s

request s rq @ ManagedAcctsReq = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq }   
       write s $ appNull hdr
       wFlush s

request s rq @ (FAReq fad) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq}  
       write s $ appNull $ hdr <++> show' ( fromEnum' fad)

request s rq @ (FAReplaceReq fad cxml) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq}  
       write s $ appNull $ hdr <++> show' (fromEnum' fad) <++> B.pack cxml
       wFlush s

request s rq @ (HistoricalDataReq {rqp_tickerId = tid
                                    , hdr_contract = con
                                    , hdr_endDataTime = edt
                                    , hdr_durationStr = durStr
                                    , hdr_whatToShow = whatToShow
                                    , hdr_formatDate = formatDate
                                    , hdr_chartOptions = chartOptions
                                    , hdr_useRTH = useRTH
                                    , hdr_barSizeSetting = barSizeSetting
                                    }) =  
    do  let serv_ver = s_version s 
        hdr <-  getHeaderCon s defReqHeader { rqh_msgId = reqToId rq
                                   , rqh_proVer = 6
                                   , rqh_errId = tid
                                   , rqh_minVer = Just min_server_ver_trading_class
                                   , rqh_errMsg = "  It does not support conId and tradingClass parameters in reqHistoricalData."
                                   } con
        con' <- encodeContract s con True

        write s $ appNull $ (hdr <++> show' tid)
            <++> con'
            <++> show' ( fromBool $ ct_includeExpired con  )
            <++> B.pack edt
            <++> B.pack barSizeSetting
            <++> B.pack durStr
            <++> show' useRTH
            <++> B.pack whatToShow
            <++> show' formatDate

        when (compare (ct_secType con) "BAG" == EQ) $
            write s $ appNull $ encodeComboLegList (ct_comboLegsList con) 
      
        when (serv_ver >= min_server_ver_linking) $
            write s $ appNull $ encodeTagValueList chartOptions

        wFlush s
 
request s rq @ (ExerciseOptionsReq { rqp_tickerId = tid
                                   , eor_contract = con 
                                   , eor_exerciseAction = exerciseAction 
                                   , eor_exerciseQuantity = exerciseQty 
                                   , eor_account = account 
                                   , eor_override = override 
                                   }) =  
    do  let serv_ver = s_version s 
        hdr <-  getHeaderCon s defReqHeader { rqh_msgId = reqToId rq
                                         , rqh_proVer = 2
                                         , rqh_errId = tid
                                         , rqh_minVer = Just min_server_ver_trading_class
                                         , rqh_errMsg = "  It does not support conId and tradingClass parameters in reqHistoricalData."
                                         } con
        con' <- encodeContract s con False

        write s $ appNull $ hdr <++> show' tid
                <++> con'
                <++> show' exerciseAction
                <++> show' exerciseQty
                <++> B.pack account
                <++> show' override 
        wFlush s

request s rq @ (ScannerSubscriptionReq { rqp_tickerId = tid
                                       , ssr_subscription = subs
                                       , ssr_subscriptionOptions = subsOpts
                                       }) = 
    do  let serv_ver = s_version s
        hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq 
                                      , rqh_proVer = 4
                                      , rqh_errId = tid
                                      }
        write s $ appNull $ show' tid <++> encodeSubscription subs

        when (serv_ver >= min_server_ver_linking) $
            write s $ appNull $  encodeTagValueList subsOpts
            

        wFlush s

request s rq @ (CancelScannerSubscription tid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                                     , rqh_errId = tid 
                                     }
       write s $ appNull $ hdr <++> show' tid
       wFlush s

-- TODO: verify correctness
request s rq @ ScannerParametersReq = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq }
       write s $ appNull hdr
       wFlush s

request s rq @ (CancelHistoricalData tid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_errId = tid 
                               } 
       write s $ appNull $ hdr <++> show' tid
       wFlush s



request s rq @ CurrentTimeReq = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq } 
       write s $ appNull hdr
       wFlush s

-- TODO needs dev
--
request s rq @ (RealTimeBarsReq {}) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq } 
       write s $ appNull hdr
       wFlush s

request s rq @ (CancelRealTimeBars tid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_errId = tid } 
       write s $ appNull $ hdr <++> show' tid
       wFlush s


request s rq @ (CancelFundamentalData tid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                                     , rqh_errId = tid
                                     , rqh_minVer = Just min_server_ver_fundamental_data
                                     , rqh_errMsg = "  It does not support fundamental data requests." 
                                     }
       write s $ appNull $ hdr <++> show' tid
       wFlush s

--TODO
--request s rq @ (CalcOptionPriceReq {}) = 

request s rq @ (CancelCalcImpliedVolatility tid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                                     , rqh_errId = tid
                                     , rqh_minVer = Just min_server_ver_cancel_calc_implied_volat
                                     , rqh_errMsg = "  It does not support calculate implied volatility cancellation." 
                                     }
       write s $ appNull $ hdr <++> show' tid
       wFlush s

request s rq @ (CancelCalcOptionPrice tid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_errId = tid
                               , rqh_minVer = Just min_server_ver_cancel_calc_option_price
                               , rqh_errMsg = "  It does not support calculate option price cancellation." 
                               }
       write s $ appNull $ hdr <++> show' tid
       wFlush s

request s rq @ GlobalCancelReq = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq 
                                     , rqh_errMsg = "  It does not support globalCancel requests."
                                     , rqh_minVer = Just min_server_ver_req_global_cancel
                                     }  
       write s $ appNull hdr
       wFlush s

request s rq @ (MarketDataTypeReq tid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq}
       write s $ appNull $ hdr <++> show' tid
       wFlush s


request s rq @ PositionsReq = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq}
       write s $ appNull hdr 
       wFlush s
--TODO
--request s rq @ (AccountSummaryReq {}) = 

request s rq @ (CancelAccountSummary req_id) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                                     , rqh_minVer =  Just min_server_ver_account_summary
                                     , rqh_errMsg = "  It does not support account summary cancellation." 
                                     }
       write s $ appNull hdr
       wFlush s
                               
request s rq @ CancelPositions = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_errMsg = "  It does not support positions cancellation." 
                               , rqh_minVer = Just min_server_ver_positions
                               }
       write s $ appNull hdr
       wFlush s


request s rq @ (VerifyReq apiName apiVersion) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_minVer = Just min_server_ver_linking
                               , rqh_errMsg = "  It does not support verification message sending." 
                               , rqh_exAuth = Just True
                               } 
       write s $ appNull $ hdr <++> B.pack apiName <++> B.pack apiVersion
       wFlush s

request s rq @ (VerifyMessage apiData) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_minVer = Just min_server_ver_linking
                               , rqh_errMsg = "  It does not support verification message sending." 
                               } 
       write s $ appNull $ hdr <++> B.pack apiData
       wFlush s

request s rq @ (QueryDisplayGroups rid) =
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_minVer = Just min_server_ver_linking
                               , rqh_errMsg = "  It does not support queryDisplayGroups request." 
                               } 
       write s $ appNull $ hdr <++> show' rid 
       wFlush s
  
request s rq @ (SubscribeToGroupEvents reqId gid) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_minVer = Just min_server_ver_linking
                               , rqh_errMsg = "  It does not support subscribeToGroupEvents request." 
                               } 
       write s $ appNull $ hdr <++> show' reqId
                <++> show' gid
                
       wFlush s

request s rq @ (UpdateDisplayGroup reqId contractInfo) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_minVer = Just min_server_ver_linking
                               , rqh_errMsg = "  It does not support updateDisplayGroup request." 
                               } 
       write s $ appNull $ hdr <++> show' reqId
            <++> B.pack contractInfo
            
       wFlush s

request s rq @ (UnsubscribeFromGroupEvents reqId) = 
    do hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq
                               , rqh_minVer = Just min_server_ver_linking
                               , rqh_errMsg = "  It does not support unsubscribeFromGroupEvents request." 
                               } 
       write s $ appNull $ hdr <++> show' reqId 
       wFlush s

request s rq @ StartApi = 
    do  let clientId = s_clientId s
        hdr <- getHeader s defReqHeader { rqh_msgId = reqToId rq } 
        write s $ appNull $ hdr <++> show' clientId 
        wFlush s
