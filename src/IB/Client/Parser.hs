{-# LANGUAGE NamedFieldPuns #-}


module IB.Client.Parser 
  (
    -- * Types
    IBMessage(..)
  , RecvMsg(..) 

    -- * Functions
  , pRecvMsg
  , pServerVersion
  ) where

--import Data.ByteString hiding (elem, map, empty)
import qualified Data.ByteString.Char8 as C 
import Control.Monad 
import Control.Applicative
import Data.Attoparsec.ByteString.Char8

import IB.Client.Types
import IB.Client.Nums

pServerVersion :: Parser Preamble
pServerVersion = 
    do pre_serverVersion <- pStrInt
       let pre_twsTime = ""
           

--       when (pre_serverVersion >= 20) $
--        do pre_twsTime <- pStr
--           return ()
    
       --if (serv_ver >= 20)
       -- then do twsTime <- pStrInt
       -- else let twsTime = 0 
    
       return Preamble {pre_serverVersion, pre_twsTime }

pRecvMsg :: Int -> Parser RecvMsg
pRecvMsg sver = 
    do rc_msgId <- pStrInt
       rc_version <- pStrInt
       rc_msgBody <- pIBMsg rc_msgId sver rc_version
       return RecvMsg {rc_msgId, rc_version, rc_msgBody}

tickOptionDefault :: IBMessage
tickOptionDefault = TickOptionComputation { tickerId = 0
                                          , tickType = 0
                                          , impliedVol = dblMaximum
                                          , delta = dblMaximum 
                                          , optPrice = dblMaximum
                                          , pvDividend = dblMaximum
                                          , gamma = dblMaximum
                                          , vega = dblMaximum
                                          , theta = dblMaximum
                                          , undPrice = dblMaximum 
                                          }


pStr :: Parser String
pStr = do bs <- takeWhile1 (/= '\NUL')
          return (C.unpack bs)

pStrMaybe :: Parser (Maybe String)
pStrMaybe = do bs <- takeWhile1 (/= '\NUL')
               let str = C.unpack bs
               return (if null str then Nothing else Just str)

pStrIntMax :: Parser Int
pStrIntMax = do res <- pStrMaybe

                return $ case res of
                            Just x -> read x 
                            Nothing -> fromIntegral int32max

pStrDoubleMax :: Parser Double
pStrDoubleMax = do res <- pStrMaybe
                   return $ case res of
                                Just x -> read x
                                Nothing -> dblMaximum
 
pStrInt :: Parser Int
pStrInt = read <$> pStr

pStrDouble :: Parser Double
pStrDouble = read <$> pStr

pStrBool :: Parser Bool
pStrBool = do int <- pStrInt
              case () of 
               _ | int /= 0 -> return True
                 | otherwise -> return False

pTagValue :: Parser TagValue
pTagValue = TagValue <$>
         pStr
    <*>  pStr
    <?> "Tag Value"

 
pTickPrice :: Int -> Int -> Parser IBMessage
pTickPrice _ _ = TickPrice <$> 
        pStrInt 
    <*> pStrInt
    <*> pStrDouble 
    <*> pStrInt 
    <*> pStrInt
    <?> "Tick Price"

pTickSize :: Int -> Int -> Parser IBMessage
pTickSize _ _ = TickSize <$>
        pStrInt
    <*> pStrInt
    <*> pStrDouble
    <*> pStrInt
    <?> "Tick Size"

pTickEFP :: Int -> Int -> Parser IBMessage
pTickEFP _ _ = TickEFP <$>
        pStrInt
    <*> pStrInt
    <*> pStrDouble
    <*> pStr
    <*> pStrDouble
    <*> pStrInt
    <*> pStr
    <*> pStrDouble
    <*> pStrDouble
    <?> "Tick EFP"

pAcctValue :: Int -> Int -> Parser IBMessage
pAcctValue _ _ = AcctValue <$>
        pStr
    <*> pStr
    <*> pStr
    <*> pStr
    <?> "Acct Value"

pPortfolioValue :: Int -> Int -> Parser IBMessage
pPortfolioValue sver ver = 
     do contract <- pContractHead sver ver (7,8)

        position <- pStrInt
        marketPrice <- pStrDouble
        marketValue <- pStrDouble
        averageCost <- pStrDouble
        unrealizedPNL <- pStrDouble
        realizedPNL <- pStrDouble
        accountName <- pStr

        return PortfolioValue { contract
                              , position
                              , marketPrice
                              , averageCost
                              , unrealizedPNL
                              , realizedPNL
                              , accountName 
                              }

pAcctUpdateTime :: Int -> Int -> Parser IBMessage
pAcctUpdateTime _ _ = AcctUpdateTime <$> pStr <?> "Acct Update Time"

pNextValidId :: Int -> Int -> Parser IBMessage
pNextValidId _ _ = NextValidId <$> pStrInt <?> "Next Valid Id"

pManagedAccts :: Int -> Int -> Parser IBMessage
pManagedAccts _ _ = ManagedAccts <$>
        pStr
    <?> "Managed Accts"

pReceiveFA :: Int -> Int -> Parser IBMessage
pReceiveFA _ _ = ReceiveFA <$>
        pStrInt
    <*> pStr
    <?> "Receive FA"

pHistoricalData :: Int -> Int -> Parser IBMessage
pHistoricalData _ _ = HistoricalData <$>
        pStrInt
    <*> pStr
    <*> pStr
    <*> pBarDataCons
    <?> "Historical Data"

pTickString :: Int -> Int -> Parser IBMessage 
pTickString _ _ = TickString <$>
        pStrInt
    <*> pStrInt
    <*> pStr
    <?> "Tick String"

pScannerData :: Int -> Int -> Parser IBMessage
pScannerData _ _ = ScannerData <$>
        pStrInt
    <*> pScanDataList
    <?> "Scanner Data"

pScannerParameters :: Int -> Int -> Parser IBMessage
pScannerParameters _ _ = ScannerParameters <$>
        pStr
    <?> "Scanner Parameters"

pCurrentTime :: Int -> Int -> Parser IBMessage
pCurrentTime  _ _ = CurrentTime <$>
       pStrInt
    <?> "Current Time"

pRealTimeBars :: Int -> Int -> Parser IBMessage
pRealTimeBars _ _ = RealTimeBars <$>
        pStrInt
    <*> pStrInt
    <*> pStrDouble
    <*> pStrDouble
    <*> pStrDouble
    <*> pStrDouble
    <*> pStrInt
    <*> pStrDouble
    <*> pStrInt
    <?> "Real Time Bars"

pFundamentalData :: Int -> Int -> Parser IBMessage
pFundamentalData _ _ = FundamentalData <$>
        pStrInt
    <*> pStr
    <?> "Fundamental Data"

pPositionData :: Int -> Int -> Parser IBMessage
pPositionData sver ver = 
    do account <- pStr
       contract <- pContractHead sver ver (0,2) 
       position <- pStrInt
       let avgCost = dblMaximum

       when (ver >= 3) $
          do avgCost <- pStrDouble
             return ()

       return PositionData { account
                           , contract
                           , position
                           , avgCost
                           }
                           
pPositionEnd :: Int -> Int -> Parser IBMessage
pPositionEnd _ _ = return PositionEnd  

pAccountSummary :: Int -> Int -> Parser IBMessage 
pAccountSummary _ _ = AccountSummary <$> 
        pStrInt 
    <*> pStr
    <*> pStr
    <*> pStr
    <*> pStr
    <?> "Account Summary" 

pAccountSummaryEnd :: Int -> Int -> Parser IBMessage 
pAccountSummaryEnd _ _ = AccountSummaryEnd <$> pStrInt <?> "Account Summary End"

pVerifyMessageAPI :: Int -> Int -> Parser IBMessage  
pVerifyMessageAPI _ _ = VerifyMessageAPI <$>
        pStr
    <?> "Verify Message API"    

pVerifyCompleted :: Int -> Int -> Parser IBMessage   
pVerifyCompleted _ _ = VerifyCompleted <$>
        pStr
    <*> pStr
    <?> "Verify Completed"

pDisplayGroupList :: Int -> Int -> Parser IBMessage    
pDisplayGroupList _ _ = DisplayGroupList <$>
        pStrInt
    <*> pStr
    <?> "Display Group List"

        
pDisplayGroupUpdated :: Int -> Int -> Parser IBMessage     
pDisplayGroupUpdated _ _ = DisplayGroupUpdated <$>
        pStrInt
    <*> pStr
    <?> "Display Group Updated"
 
pCommissionReport :: Int -> Int -> Parser IBMessage
pCommissionReport _ _  = CommissionReport <$>
        pStr
    <*> pStrDouble
    <*> pStr
    <*> pStrDouble
    <*> pStrDouble
    <*> pStrInt
    <?> "Commission Report"

pContractDataEnd :: Int -> Int -> Parser IBMessage
pContractDataEnd _ _ = ContractDataEnd <$> pStrInt <?> "Contract Data End"  

pOpenOrderEnd :: Int -> Int -> Parser IBMessage
pOpenOrderEnd _ _ = return OpenOrderEnd
 
pAcctDownloadEnd :: Int -> Int -> Parser IBMessage
pAcctDownloadEnd _ _ = AcctDownloadEnd <$> pStr <?> "Acct Download End"

pExecutionDataEnd :: Int -> Int -> Parser IBMessage  
pExecutionDataEnd _ _ = ExecutionDataEnd <$> pStrInt <?> "Execution Data End"

pDeltaNeutralValidation :: Int -> Int -> Parser IBMessage   
pDeltaNeutralValidation _ _ =  DeltaNeutralValidation <$> pStrInt
    <*> pUnderComp' <?> "Delta Neutral Validation"

pTickSnapshotEnd :: Int -> Int -> Parser IBMessage
pTickSnapshotEnd _ _ = TickSnapshotEnd <$> pStrInt <?> "Tick Snapshot End"

pMarketDataType :: Int -> Int -> Parser IBMessage 
pMarketDataType _ _ = MarketDataType <$> pStrInt <*> pStrInt <?> "Market Data"

pOrderState :: Parser OrderState
pOrderState = OrderState <$> 
        pStr
    <*> pStr
    <*> pStr
    <*> pStr
    <*> pStrDoubleMax
    <*> pStrDoubleMax
    <*> pStrDoubleMax
    <*> pStr
    <*> pStr
    <?> "Order State"

--if (ver >= 6) 
--  then do cumQty <- pStrInt
--          avgPrice <- pStrDouble
--  else let cumQty = int32max
--           avgPrice = dblMaximum

--if (ver >= 8)
--  then do orderRef <- pStr
--  else return () 

--if (ver >= 9) 
--  then do evRule <- pStr
--          evMultiplier <- pStrDouble
--  else let evRule = ""
--           evMultiplier = dblMaximum

pExecution :: Int -> Int -> Parser Execution
pExecution _ ver = 
    do  ex_execId <- pStr
        ex_time <- pStr
        ex_acctNumber <- pStr
        ex_exchange <- pStr
        ex_side <- pStr
        ex_shares <- pStrInt
        ex_price <- pStrDouble
        ex_permId <- pStrInt
        ex_clientId <- pStrInt
        ex_liquidation <- pStrInt
    
        let ex_cumQty = int32max
            ex_avgPrice = dblMaximum
            ex_orderRef = ""
            ex_evRule = ""
            ex_evMultiplier = dblMaximum

        case () of
         _ | ver >= 9 -> do ex_cumQty <- pStrInt
                            ex_avgPrice <- pStrDouble
                            ex_orderRef <- pStr
                            ex_evRule <- pStr
                            ex_evMultiplier <- pStrDouble
                            return ()
           | ver >= 8 -> do ex_cumQty <- pStrInt
                            ex_avgPrice <- pStrDouble
                            ex_orderRef <- pStr
                            return ()
           | ver >= 6 -> do ex_cumQty <- pStrInt
                            ex_avgPrice <- pStrDouble
                            return ()

        return Execution 
              { ex_execId  
              , ex_time
              , ex_acctNumber
              , ex_exchange
              , ex_side
              , ex_shares
              , ex_price
              , ex_permId
              , ex_clientId
              , ex_liquidation
              , ex_cumQty
              , ex_avgPrice
              , ex_orderRef
              , ex_evRule
              , ex_evMultiplier 
              }

pContractHead :: Int -> Int -> (Int, Int) -> Parser Contract
pContractHead _ ver vercheck@(lwr, uppr) = 
    do ct_conId <- pStrInt
       ct_symbol <- pStr
       ct_secType <- pStr
       ct_expiry <- pStr
       ct_strike <- pStrDouble
       ct_right <- pStr
       
       let ct_multiplier = ""
           ct_tradingClass = ""
    
       when (ver >= lwr) $
           do ct_multiplier <- pStr
              return ()

       ct_exchange <- pStr
       ct_currency <- pStr
       ct_localSymbol <- pStr
       
       when (ver >= uppr) $
           do ct_tradingClass <- pStr
              return ()

       return Contract { ct_conId
                       , ct_symbol
                       , ct_secType
                       , ct_expiry
                       , ct_strike
                       , ct_right
                       , ct_multiplier
                       , ct_exchange
                       , ct_currency
                       , ct_localSymbol
                       , ct_tradingClass 
                       }

pExecutionData :: Int -> Int -> Parser IBMessage
pExecutionData sver ver = 
    do reqId <- pStrInt 
       orderId <- pStrInt 
       contract <- pContractHead sver ver (9,10)
       exec <- pExecution 0 ver
       return ExecutionData { reqId
                            , orderId
                            , contract
                            , exec 
                            } 

pMarketDepth :: Int -> Int -> Parser IBMessage
pMarketDepth _ _ = MarketDepth <$>
        pStrInt
    <*> pStrInt
    <*> pStrInt
    <*> pStrInt
    <*> pStrDouble
    <*> pStrInt
    <?> "Market Depth"

pMarketDepthL2 :: Int -> Int -> Parser IBMessage
pMarketDepthL2 _ _ = MarketDepthL2 <$>
        pStrInt
    <*> pStrInt
    <*> pStr
    <*> pStrInt
    <*> pStrInt
    <*> pStrDouble
    <*> pStrInt
    <?> "Market Depth L2"
 
pBondContractData :: Int -> Int -> Parser IBMessage
pBondContractData _ ver = 
    do  let reqId = -1 
        when (ver >= 3) $ 
            do reqId <- pStrInt
               return ()

        ct_symbol <- pStr
        ct_secType <- pStr
        ctd_cusip <- pStr
        ctd_coupon <- pStrDouble
        ctd_maturity <- pStr
        ctd_issueDate <- pStr
        ctd_ratings <- pStr
        ctd_bondType <- pStr
        ctd_couponType <- pStr
        ctd_convertible <- pStrBool
        ctd_callable <- pStrBool
        ctd_putable <- pStrBool
        ctd_descAppend <- pStr
        ct_exchange <- pStr
        ct_currency <- pStr
        ctd_marketName <- pStr
        ct_tradingClass <- pStr
        ct_conId <- pStrInt
        ctd_minTick <- pStrDouble
        ctd_orderTypes <- pStr
        ctd_validExchanges <- pStr
        ctd_nextOptionDate <- pStr
        ctd_nextOptionType <- pStr
        ctd_nextOptionPartial <- pStrBool
        ctd_notes <- pStr
        
        let ctd_summary = Contract { ct_symbol
                               , ct_secType
                               , ct_exchange
                               , ct_currency
                               , ct_tradingClass
                               , ct_conId
                               }
            ctd_secIdList = []
            ctd_evRule = ""
            ctd_longName = ""
            ctd_evMultiplier = dblMaximum

        case () of
         _ | ver >= 6 -> do ctd_longName <- pStr
                            ctd_evRule <- pStr
                            ctd_evMultiplier <- pStrDouble
                            ctd_secIdList <- pTagValueCons
                            return ()
           | ver >= 5 -> do ctd_longName <- pStr
                            ctd_secIdList <- pTagValueCons
                            return ()
           | ver >= 4 -> do ctd_longName <- pStr
                            return ()

        let ctd = ContractDetails { ctd_summary
                                  , ctd_marketName
                                  , ctd_minTick
                                  , ctd_orderTypes
                                  , ctd_validExchanges
                                  , ctd_longName
                                  , ctd_evRule
                                  , ctd_evMultiplier
                                  , ctd_secIdList
                                  , ctd_notes
                                  , ctd_nextOptionPartial
                                  , ctd_nextOptionType
                                  , ctd_nextOptionDate
                                  , ctd_descAppend
                                  , ctd_putable
                                  , ctd_callable
                                  , ctd_convertible
                                  , ctd_couponType
                                  , ctd_bondType
                                  , ctd_ratings
                                  , ctd_issueDate
                                  , ctd_maturity
                                  , ctd_coupon
                                  , ctd_cusip
                                  }
        return $ BondContractData ctd

pNewsBulletins :: Int -> Int -> Parser IBMessage  
pNewsBulletins _ _ = NewsBulletins <$>
        pStrInt
    <*> pStrInt
    <*> pStr
    <*> pStr
    <?> "News Bulletins"
 
pContractData :: Int -> Int -> Parser IBMessage
pContractData _ ver = do
    let reqId = -1
    when (ver >= 3) $ 
        do reqId <- pStrInt
           return ()
           
    ct_symbol <- pStr
    ct_secType <- pStr
    ct_expiry <- pStr
    ct_strike <- pStrDouble
    ct_right <- pStr
    ct_exchange <- pStr
    ct_currency <- pStr
    ct_localSymbol <- pStr
    ctd_marketName <- pStr
    ctd_minTick <- pStrDouble
    ct_multiplier <- pStr
    ctd_orderTypes <- pStr
    ctd_validExchanges <- pStr
    ctd_priceMagnifier <- pStrInt

    let ctd_underConId = int32max 
        ctd_longName = ""
        ct_primaryExchange = ""
        ctd_evRule = ""
        ctd_evMultiplier = dblMaximum
        ctd_secIdList = []
        ctd_contractMonth = ""
        ctd_industry = ""
        ctd_category = ""
        ctd_subcategory = ""
        ctd_timeZoneId = ""
        ctd_tradingHours = ""
        ctd_liquidHours = ""

    when (ver >= 4) $ do ctd_underConId <- pStrInt
                         return ()

    when (ver >= 5) $ do ctd_longName <- pStr
                         ct_primaryExchange <- pStr 
                         return ()
                      
    when (ver >= 6) $ do ctd_contractMonth <- pStr
                         ctd_industry <- pStr
                         ctd_category <- pStr
                         ctd_subcategory <- pStr
                         ctd_timeZoneId <- pStr
                         ctd_tradingHours <- pStr
                         ctd_liquidHours <- pStr
                         return ()
                   
    when (ver >= 8) $ do ctd_evRule <- pStr
                         ctd_evMultiplier <- pStrDouble
                         return ()
        

    when (ver >= 7) $ do ctd_secIdList <- pTagValueCons
                         return ()

                                       
    let ctd_summary = Contract { ct_symbol
                               , ct_secType
                               , ct_expiry
                               , ct_primaryExchange
                               , ct_strike
                               , ct_right
                               , ct_multiplier
                               , ct_exchange
                               , ct_currency
                               , ct_localSymbol 
                               }

        ctd = ContractDetails { ctd_summary
                              , ctd_marketName
                              , ctd_minTick
                              , ctd_orderTypes
                              , ctd_validExchanges
                              , ctd_priceMagnifier
                              , ctd_underConId
                              , ctd_longName
                              , ctd_contractMonth
                              , ctd_industry
                              , ctd_category
                              , ctd_subcategory
                              , ctd_timeZoneId
                              , ctd_tradingHours
                              , ctd_liquidHours
                              , ctd_evRule
                              , ctd_evMultiplier
                              , ctd_secIdList
                              }
    return $ ContractData ctd

pContract :: Int -> Int -> Parser Contract
pContract sver ver = pContractHead sver ver (32,32)

pOrder :: Int -> Int -> Parser Order
pOrder serv_ver ver = 
     do ord_action <- pStr
        ord_totalQuantity <- pStrInt
        ord_orderType <- pStr
        let ord_exemptCode = int32max
            ord_deltaNeutralConId = int32max
            ord_deltaNeutralSettlingFirm = ""
            ord_deltaNeutralClearingAccount = ""
            ord_deltaNeutralClearingIntent = ""
            ord_deltaNeutralOpenClose = ""
            ord_deltaNeutralShortSale = False
            ord_deltaNeutralShortSaleSlot = int32max
            ord_deltaNeutralDesignatedLocation = ""
            ord_trailingPercent = dblMaximum

        if ver < 29
           then do ord_lmtPrice <- pStrDouble
                   return ()
           else do ord_lmtPrice <- pStrDoubleMax
                   return ()

        if ver < 30
           then do ord_auxPrice <- pStrDouble
                   return ()
           else do ord_auxPrice <- pStrDoubleMax
                   return () 

        ord_tif <- pStr
        ord_ocaGroup <- pStr
        ord_account <- pStr
        ord_openClose <- pStr
        ord_origin' <- pStrInt
        ord_orderRef <- pStr
        ord_clientId <- pStrInt
        ord_permId <- pStrInt
        ord_outsideRth <- pStrBool
        ord_hidden <- pStrBool
        ord_discretionaryAmt <- pStrDouble
        ord_goodAfterTime <- pStr
        ord_pStr <- pStr -- deprecated sharesAllocation Field, unused
        ord_faGroup <- pStr
        ord_faMethod <- pStr
        ord_faPercentage <- pStr 
        ord_faProfile <- pStr
        ord_goodTillDate <- pStr
        ord_rule80A <- pStr
        ord_percentOffset <- pStrDoubleMax
        ord_settlingFirm <- pStr
        ord_shortSaleSlot <- pStrInt
        ord_designatedLocation <- pStr

        when (serv_ver == min_server_ver_sshortx_old) $
           do pStrInt
              return () 


        when (serv_ver /= min_server_ver_sshortx_old && ver >= 23) $
            do ord_exemptCode <- pStrInt
               return ()

        ord_auctionStrategy <- pStrInt
        ord_startingPrice <- pStrDoubleMax
        ord_stockRefPrice <- pStrDoubleMax
        ord_delta <- pStrDoubleMax
        ord_stockRangeLower <- pStrDoubleMax
        ord_stockRangeUpper <- pStrDoubleMax
        ord_displaySize <- pStrInt
        ord_blockOrder <-  pStrBool
        ord_sweepToFill <- pStrBool
        ord_allOrNone <- pStrBool
        ord_minQty <- pStrIntMax
        ord_ocaType <- pStrInt
        ord_eTradeOnly <- pStrBool
        ord_firmQuoteOnly <- pStrBool
        ord_nbboPriceCap <- pStrDoubleMax
        ord_parentId <- pStrInt
        ord_triggerMethod <- pStrInt
        ord_volatility <- pStrDoubleMax
        ord_volatilityType <- pStrInt
        ord_deltaNeutralOrderType <- pStr
        ord_deltaNeutralAuxPrice <- pStrDoubleMax

        if ver >= 27 && not (null ord_deltaNeutralOrderType)
            then do ord_deltaNeutralConId <- pStrInt 
                    ord_deltaNeutralSettlingFirm <- pStr
                    ord_deltaNeutralClearingAccount <- pStr
                    ord_deltaNeutralClearingIntent <- pStr
                    return () 
            else when (ver >= 31 && not  (null ord_deltaNeutralOrderType)) $
                 do ord_deltaNeutralOpenClose <- pStr
                    ord_deltaNetralShortSale <- pStrBool
                    ord_deltaNeutralShortSaleSlot <- pStrInt
                    ord_deltaNeutralDesignatedLocation <- pStr
                    return ()


        ord_continuousUpdate <- pStrBool
        ord_referencePriceType <- pStrInt
        ord_trailStopPrice <- pStrDoubleMax
        
        when (ver >= 30) $
            do ord_trailingPercent <- pStrDoubleMax
               return ()

        
        ord_basisPoints <- pStrDoubleMax
        ord_basisPointsType <- pStrIntMax
        let ord_origin = toEnum ord_origin'

        return Order { ord_action
                     , ord_totalQuantity
                     , ord_orderType
                     , ord_tif
                     , ord_ocaGroup
                     , ord_account
                     , ord_openClose
                     , ord_origin
                     , ord_orderRef
                     , ord_clientId
                     , ord_permId
                     , ord_outsideRth
                     , ord_hidden
                     , ord_discretionaryAmt
                     , ord_goodAfterTime
                     , ord_faGroup
                     , ord_faMethod
                     , ord_faPercentage
                     , ord_faProfile
                     , ord_goodTillDate
                     , ord_rule80A
                     , ord_percentOffset
                     , ord_settlingFirm
                     , ord_shortSaleSlot
                     , ord_designatedLocation
                     , ord_exemptCode
                     , ord_auctionStrategy
                     , ord_startingPrice
                     , ord_stockRefPrice
                     , ord_delta
                     , ord_stockRangeLower
                     , ord_stockRangeUpper
                     , ord_displaySize
                     , ord_blockOrder
                     , ord_sweepToFill
                     , ord_allOrNone
                     , ord_minQty
                     , ord_ocaType
                     , ord_eTradeOnly
                     , ord_firmQuoteOnly
                     , ord_nbboPriceCap
                     , ord_parentId
                     , ord_triggerMethod
                     , ord_volatility
                     , ord_volatilityType
                     , ord_deltaNeutralOrderType
                     , ord_deltaNeutralAuxPrice
                     , ord_deltaNeutralConId
                     , ord_deltaNeutralSettlingFirm
                     , ord_deltaNeutralClearingAccount
                     , ord_deltaNeutralClearingIntent
                     , ord_deltaNeutralOpenClose
                     , ord_deltaNeutralShortSale
                     , ord_deltaNeutralShortSaleSlot
                     , ord_deltaNeutralDesignatedLocation
                     , ord_continuousUpdate
                     , ord_referencePriceType
                     , ord_trailStopPrice
                     , ord_trailingPercent
                     , ord_basisPoints
                     , ord_basisPointsType
                     }

 
pOrderStatus :: Int -> Int -> Parser IBMessage
pOrderStatus _ _  = OrderStatus <$>
            pStrInt
        <*> pStr 
        <*> pStrInt
        <*> pStrInt
        <*> pStrDouble
        <*> pStrInt
        <*> pStrInt
        <*> pStrDouble
        <*> pStrInt
        <*> pStr
        <?> "Order Status"

pErr :: Int -> Int -> Parser IBMessage
pErr _ _ = Err <$> pStrInt <*> pStr <?> "Err Msg"

pComboLeg :: Parser ComboLeg
pComboLeg = ComboLeg <$> 
        pStrInt
    <*> pStrInt
    <*> pStr
    <*> pStr
    <*> pStrInt
    <*> pStrInt
    <*> pStr
    <*> pStrInt
    <?> "Combo Leg"

pTagValueCons :: Parser [TagValue]
pTagValueCons = 
    do listCount <- pStrInt
       if listCount > 0
        then replicateM listCount pTagValue
        else return []

pComboLegCons :: Int -> Int -> Parser [ComboLeg]
pComboLegCons _ ver = 
    if ver >= 29 then do 
                        comboLegsCount <- pStrInt 
                        if comboLegsCount > 0
                            then replicateM comboLegsCount pComboLeg 
                                    
                            else return []
    else return []

pOrderComboLeg :: Parser OrderComboLeg
pOrderComboLeg = pStrDoubleMax

pBarData :: Parser BarData
pBarData = BarData <$>
        pStr
    <*> pStrDouble
    <*> pStrDouble
    <*> pStrDouble
    <*> pStrDouble
    <*> pStrInt
    <*> pStrDouble
    <*> pStr
    <*> pStrInt
    <?> "Bar Data"

pScanData :: Parser ScanData
pScanData = ScanData <$>
        pStrInt
    <*> plocContractDetails
    <*> pStr
    <*> pStr
    <*> pStr
    <*> pStr
    <?> "Scan Data"
        where
          plocContractDetails = do ct_conId <- pStrInt
                                   ct_symbol <- pStr
                                   ct_secType <- pStr
                                   ct_expiry <- pStr
                                   ct_strike <- pStrDouble
                                   ct_right <- pStr
                                   ct_exchange <- pStr
                                   ct_currency <- pStr
                                   ct_localSymbol <- pStr
                                   ctd_marketName <- pStr
                                   ct_tradingClass <- pStr

                                   let ctd_summary = Contract { ct_conId
                                                          , ct_symbol
                                                          , ct_secType
                                                          , ct_expiry
                                                          , ct_strike
                                                          , ct_right
                                                          , ct_exchange
                                                          , ct_currency
                                                          , ct_localSymbol
                                                          , ct_tradingClass
                                                          }
                                   return ContractDetails { ctd_summary
                                                          , ctd_marketName}

pScanDataList :: Parser [ScanData]
pScanDataList = do numberOfElements <- pStrInt
                   replicateM numberOfElements pScanData
                   

pOrderComboLegCons :: Int -> Int -> Parser [OrderComboLeg]
pOrderComboLegCons _ ver 
    | ver >= 29 = do orderComboLegsCount <- pStrInt
                     case () of
                      _ | orderComboLegsCount > 0 -> replicateM orderComboLegsCount pOrderComboLeg
                        | otherwise -> return []
    | otherwise = return []

pBarDataCons :: Parser [BarData]
pBarDataCons  = do itemCount <- pStrInt
                   replicateM itemCount pBarData

pUnderComp' :: Parser UnderComp
pUnderComp' = UnderComp <$> pStrInt <*> pStrDouble <*> pStrDouble <?> "UnderComp"

pUnderComp :: Int -> Int -> Parser UnderComp
pUnderComp _ ver = 
    if ver >= 20
        then do underCompPresent <- pStrBool
                let uc_conId = int32max
                    uc_delta = dblMaximum
                    uc_price = dblMaximum 

                when underCompPresent $
                    do uc_conId <- pStrInt
                       uc_delta <- pStrDouble
                       uc_price <- pStrDouble
                       return ()

                return UnderComp {uc_conId, uc_delta, uc_price}
        else return UnderComp {}

-- TODO: Convert to pattern guard case structure
pAlgoStrategy :: Int -> Int -> Parser (String,[TagValue])
pAlgoStrategy _ ver = 
    if ver >= 21
        then do algoStrategy <- pStr
                if not (null algoStrategy)
                 then do algoParamsCount <- pStrInt
                         if algoParamsCount > 0
                           then do agl <- replicateM algoParamsCount pTagValue
                                   return (algoStrategy, agl)
                           else return (empty,[])
                         return (empty,[])
                 else return (empty,[] )
        else return (empty,[] )                               


pOpenOrder :: Int -> Int -> Parser IBMessage
pOpenOrder serv_ver ver = 
    do  let ord_smartComboRoutingParams = [] 
            ord_scaleSubsLevelSize = int32max
            ord_hedgeType = ""
            ord_hedgeParam = ""
            ord_optOutSmartRouting = False
            ord_notHeld = False
            ord_scalePriceAdjustValue = dblMaximum 
            ord_scalePriceAdjustInterval = int32max
            ord_scaleProfitOffset = dblMaximum
            ord_scaleAutoReset = False
            ord_scaleInitPosition = int32max
            ord_scaleInitFillQty = int32max
            ord_scaleRandomPercent = False
            ord_scaleInitLevelSize = int32max

        orderId <- pStrInt
        contract' <- pContract serv_ver ver
        order' <- pOrder serv_ver ver
        ct_comboLegsDescrip <- pStr
        comboLeg <- pComboLegCons serv_ver ver
        orderComboLegs <- pOrderComboLegCons serv_ver ver

        when (ver >= 26) $ do ord_smartComboRoutingParams <- pTagValueCons
                              return ()

        if ver >= 20
            then do ord_scaleInitLevelSize <- pStrIntMax
                    ord_scaleSubsLevelSize <- pStrIntMax
                    return ()
            else do pStr
                    ord_scaleInitLevelSize <- pStrIntMax
                    return () 

        ord_scalePriceIncrement <- pStrDoubleMax
        
        when (ver >= 28 && ord_scalePriceIncrement > 0.0 && ord_scalePriceIncrement /= dblMaximum) $ 
            do ord_scalePriceAdjustValue <- pStrDoubleMax
               ord_scalePriceAdjustInterval <- pStrIntMax
               ord_scaleProfitOffset <- pStrDoubleMax
               ord_scaleAutoReset <- pStrBool
               ord_scaleInitPosition <- pStrIntMax
               ord_scaleInitFillQty <- pStrIntMax
               ord_scaleRandomPercent <- pStrBool
               return ()

        when (ver >= 24) $ 
            do ord_hedgeType <- pStr
               unless (null ord_hedgeType) $
                 do ord_hedgeParam <- pStr
                    return ()


        when (ver >= 25) $ 
           do ord_optOutSmartRouting <- pStrBool
              return ()
              

        ord_clearingAccount <- pStr
        ord_clearingIntent <- pStr

        when (ver >= 22) $ 
            do notHeld <- pStrBool
               return ()

        ct_underComp <- pUnderComp serv_ver ver
        (ord_algoStrategy, ord_algoParams) <- pAlgoStrategy serv_ver ver
        ord_whatIf <- pStrBool
        orderState <- pOrderState

        let order = order' { ord_whatIf
                           , ord_notHeld
                           , ord_smartComboRoutingParams
                           , ord_algoStrategy
                           , ord_algoParams
                           , ord_clearingAccount
                           , ord_clearingIntent
                           , ord_optOutSmartRouting
                           , ord_hedgeType
                           , ord_hedgeParam
                           , ord_scaleInitLevelSize
                           , ord_scaleSubsLevelSize
                           , ord_scalePriceIncrement
                           , ord_scalePriceAdjustValue
                           , ord_scalePriceAdjustInterval
                           , ord_scaleProfitOffset
                           , ord_scaleAutoReset
                           , ord_scaleInitPosition
                           , ord_scaleInitFillQty
                           , ord_scaleRandomPercent
                           }

        let contract = contract' { ct_underComp
                                 , ct_comboLegsDescrip  
                                 } 

        return OpenOrder { order
                         , contract
                         , comboLeg
                         , orderComboLegs
                         , orderState
                         }

pTickOptionComputation :: Int -> Int -> Parser IBMessage
pTickOptionComputation _ ver = do 
        let gamma = dblMaximum
            vega = dblMaximum
            theta = dblMaximum 
            undPrice = dblMaximum 
            optPrice = dblMaximum
            pvDividend = dblMaximum 
        tickerId <- pStrInt
        tickType <- pStrInt
        impliedVol <- dblCheckNegative <$> pStrDouble
        delta <- pStrDouble

        when (ver >= 6 || tickType == fromEnum MODEL_OPTION) $
          do optPrice <- dblCheckNegative <$> pStrDouble
             pvDividend <- dblCheckNegative <$> pStrDouble  
             return ()


        when (ver >= 6) $
          do gamma <- dblDefaultCheck <$> pStrDouble
             vega <- dblDefaultCheck <$> pStrDouble
             theta <- dblDefaultCheck <$> pStrDouble
             undPrice <- dblDefaultCheck <$> pStrDouble
             return ()

        return $ TickOptionComputation tickerId tickType impliedVol delta optPrice pvDividend gamma vega theta undPrice

pTickGeneric :: Int -> Int -> Parser IBMessage
pTickGeneric _ _ = TickGeneric <$>
            pStrInt
        <*> pStrInt
        <*> pStrDouble
        <?> "Tick Generic"

pIBMsg :: Int -> Int -> Int -> Parser IBMessage
pIBMsg id sver ver = case idToMsg id of
    TickPrice {} -> pTickPrice sver ver
    TickSize {} -> pTickSize sver ver
    TickOptionComputation {}   -> pTickOptionComputation sver ver
    TickGeneric {}  -> pTickGeneric sver ver
    TickString {}  -> pTickString  sver ver
    TickEFP {}  -> pTickEFP sver ver
    OrderStatus {}  -> pOrderStatus sver ver
    Err {}  -> pErr sver ver
    OpenOrder {}   -> pOpenOrder sver ver
    AcctValue {}  -> pAcctValue sver ver
    PortfolioValue {} -> pPortfolioValue sver ver
    AcctUpdateTime {} -> pAcctUpdateTime sver ver
    NextValidId  {}   -> pNextValidId sver ver
    ContractData  {}  -> pContractData sver ver
    BondContractData  {}  -> pBondContractData sver ver
    ExecutionData  {}  -> pExecutionData sver ver
    MarketDepth  {}  -> pMarketDepth sver ver
    MarketDepthL2  {}  -> pMarketDepthL2 sver ver
    NewsBulletins  {}  -> pNewsBulletins sver ver
    ManagedAccts  {}  -> pManagedAccts sver ver
    ReceiveFA  {}  -> pReceiveFA sver ver
    HistoricalData  {}  -> pHistoricalData sver ver
    ScannerData  {}  -> pScannerData sver ver
    ScannerParameters {}   -> pScannerParameters sver ver
    CurrentTime  {}  -> pCurrentTime sver ver
    RealTimeBars  {}  -> pRealTimeBars sver ver
    FundamentalData  {}  -> pFundamentalData sver ver
    ContractDataEnd  {}  -> pContractDataEnd sver ver
    OpenOrderEnd  {}  -> pOpenOrderEnd  sver ver
    AcctDownloadEnd  {}  -> pAcctDownloadEnd sver ver
    ExecutionDataEnd  {}  -> pExecutionDataEnd sver ver
    DeltaNeutralValidation  {}  -> pDeltaNeutralValidation  sver ver
    TickSnapshotEnd  {}  -> pTickSnapshotEnd  sver ver
    MarketDataType {}   -> pMarketDataType sver ver
    CommissionReport  {}  -> pCommissionReport sver ver
    PositionData  {}  -> pPositionData sver ver
    PositionEnd {}   -> pPositionEnd sver ver
    AccountSummary  {}  -> pAccountSummary sver ver
    AccountSummaryEnd {}  -> pAccountSummaryEnd sver ver
    VerifyMessageAPI {}   -> pVerifyMessageAPI sver ver
    VerifyCompleted  {} -> pVerifyCompleted sver ver
    DisplayGroupList  {}  -> pDisplayGroupList sver ver
    DisplayGroupUpdated  {}  -> pDisplayGroupUpdated sver ver
    _         -> return IBUnknown  
