{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module IB.Client.Types where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Int
import System.IO
import Data.Data

data Request =  
    MktDataReq 
    { rqp_tickerId :: TickerId
    , mdr_contract :: Contract
    , mdr_genericTicks :: String
    , mdr_snapshot :: Bool
    , mdr_mktDataOptions :: [TagValue]
    }
    | CancelMktData { rqp_tickerId  :: TickerId }
    | PlaceOrder
    { rqp_orderId :: OrderId
    , por_contract :: Contract
    , por_order :: Order
    } 
    | CancelOrder { rqp_orderId :: OrderId }
    | OpenOrdersReq
    | AccountUpdatesReq { aur_subscribe :: Bool, aur_acctCode :: String }
    | ExecutionsReq
    { rqp_reqId :: ReqId
    , exe_filter :: ExecutionFilter
    } 
    | IdsReq Int 
    | ContractDetailsReq
    { rqp_reqId :: ReqId
    , cdr_contract :: Contract
    }
    |  MktDepthReq  
    { rqp_tickerId :: TickerId
    , mkr_contract :: Contract
    , mkr_numRows :: Int 
    , mkr_mktDepthOptions :: [TagValue]
    } 
    | CancelMktDepth TickerId
    | NewsBulletinsReq Bool
    | CancelNewsBulletins
    | SetServerLogLevel Int
    | AutoOpenOrdersReq Bool
    | AllOpenOrdersReq
    | ManagedAcctsReq
    | FAReq FaDataType
    | FAReplaceReq 
    { far_pFaDataType :: FaDataType 
    , far_cxml :: String
    }
    | HistoricalDataReq 
    { rqp_tickerId :: TickerId
    , hdr_contract :: Contract
    , hdr_endDataTime :: String
    , hdr_durationStr :: String
    , hdr_barSizeSetting :: String
    , hdr_whatToShow :: String
    , hdr_useRTH :: Int
    , hdr_formatDate :: Int
    , hdr_chartOptions :: [TagValue]
    }
    | ExerciseOptionsReq  
    { rqp_tickerId :: TickerId
    , eor_contract :: Contract
    , eor_exerciseAction :: Int
    , eor_exerciseQuantity :: Int
    , eor_account :: String
    , eor_override :: Int
    }  
    | ScannerSubscriptionReq
    { rqp_tickerId :: TickerId
    , ssr_subscription :: ScannerSubscription
    , ssr_subscriptionOptions :: [TagValue]
    }
    | CancelScannerSubscription TickerId
    | ScannerParametersReq  
    | CancelHistoricalData TickerId
    | CurrentTimeReq
    | RealTimeBarsReq   
    { rqp_tickerId :: TickerId
    , rtb_contract :: Contract
    , rtb_barSize :: Int 
    , rtb_whatToShow :: String
    , rtb_useRTH :: Bool
    , rtb_realTimeBarsOptions :: [TagValue]
    }
    | CancelRealTimeBars TickerId
    | FundamentalDataReq
    { rqp_reqId :: TickerId
    , fdr_contract :: Contract
    , fdr_reportType :: String
    }
    | CancelFundamentalData TickerId
    | ImpliedVolatilityReq
    { rqp_tickerId :: TickerId
    , ivr_contract :: Contract
    , ivr_optionPrice :: Double
    , ivr_underPrice :: Double
    }
    | CalcOptionPriceReq  
    { rqp_tickerId :: TickerId
    , opr_contract :: Contract
    , opr_volatility :: Double
    , opr_underPrice :: Double
    }
    | CancelCalcImpliedVolatility TickerId
    | CancelCalcOptionPrice TickerId
    | GlobalCancelReq
    | MarketDataTypeReq TickerId
    | UnusedReq
    | PositionsReq
    | AccountSummaryReq
    { rqp_reqId :: ReqId
    , asr_groupName :: String
    , asr_tags :: String
    }
    | CancelAccountSummary ReqId
    | CancelPositions
    | VerifyReq
    { vr_apiName :: String
    , vr_apiVer :: String
    } 
    | VerifyMessage String
    | QueryDisplayGroups ReqId
    | SubscribeToGroupEvents ReqId GroupId
    | UpdateDisplayGroup
    { rqp_reqId :: Int
    , udg_contractInfo :: String
    } 
    | UnsubscribeFromGroupEvents ReqId
    | StartApi
        deriving (Typeable, Data) 

data IBMessage 
    = TickPrice 
    { tickerId :: Int
    , tickType :: Int
    , price   :: Double
    , size    :: Int
    , canAutoExecute :: Int
    } 
    | TickSize 
    { tickerId :: Int
    , tickType :: Int
    , price   :: Double
    , size    :: Int
    } 
    | OrderStatus 
    { orderId :: Int
    , status :: String
    , filled :: Int
    , remaining :: Int
    , avgFillPrice :: Double
    , permId      :: Int
    , parentId    :: Int
    , lastFillPrice :: Double
    , clientId    :: Int
    , whyHeld :: String
    }
    | Err  
    { errorCode   :: Int
    , errorMsg :: String
    }
    | OpenOrder 
    { order   :: Order
    , contract :: Contract 
    , comboLeg :: [ComboLeg]
    , orderComboLegs :: [OrderComboLeg]
    , orderState :: OrderState
    }
    | AcctValue  
    { key     :: String
    , val     :: String
    , cur     :: String
    , accountName :: String 
    }
    | PortfolioValue
    { contract    :: Contract
    , position    :: Int
    , marketPrice :: Double
    , marketValue :: Double
    , averageCost :: Double
    , unrealizedPNL :: Double
    , realizedPNL :: Double
    , accountName :: String
    }
    | AcctUpdateTime String 
    | NextValidId Int  
    | ContractData ContractDetails  
    | ExecutionData  
    { reqId :: Int
    , contract :: Contract
    , exec    :: Execution
    }
    | MarketDepth 
    { id :: Int 
    , position :: Int 
    , operation :: Int 
    , side :: Int 
    , price :: Double 
    , size :: Int 
    }
    | MarketDepthL2 
    { id :: Int 
    , position :: Int 
    , marketMaker :: String 
    , operation :: Int 
    , side :: Int 
    , price :: Double 
    , size :: Int 
    }
    | NewsBulletins  
    { msgId :: Int 
    , msgType :: Int 
    , newsMessage :: String 
    , originatingExch :: String 
    }
    | ManagedAccts String  
    | ReceiveFA  
    { faDataTypeInt :: Int
    , cxml :: String    
    }
    | HistoricalData 
    { reqId :: Int
    , startDateStr :: String
    , endDateStr :: String
    , barDataList :: [BarData] 
    }
    | BondContractData ContractDetails  
    | ScannerParameters String  
    | ScannerData  
    { tickerId :: Int
    , scannerDataList :: [ScanData] 
    }
    | TickOptionComputation 
    { tickerId :: Int
    , tickType :: Int
    , impliedVol :: Double
    , delta       :: Double
    , optPrice    :: Double
    , pvDividend  :: Double
    , gamma       :: Double
    , vega        :: Double
    , theta       :: Double
    , undPrice    :: Double
    }
    | TickGeneric  
    { tickerId :: Int
    , tickType :: Int
    , tg_value :: Double
    }
    | TickString
    { tickerId :: Int
    , tickType :: Int
    , ts_value :: String
    }
    | TickEFP  
    { tickerId :: Int
    , tickType :: Int
    , basisPoints       :: Double
    , formattedBasisPoints      :: String
    , impliedFuturesPrice       :: Double
    , holdDays  :: Int
    , futureExpiry      :: String
    , dividendImpact    :: Double
    , dividendsToExpiry :: Double
    } 
    | UnusedMsg1
    | CurrentTime Int   
    | RealTimeBars  
    { reqId :: Int
    , time :: Int
    , open :: Double
    , high :: Double
    , low :: Double
    , close :: Double
    , volume :: Int
    , average :: Double
    , count :: Int
    }
    | FundamentalData 
    { reqId :: Int
    , fdata :: String
    }
    | ContractDataEnd ReqId  
    | OpenOrderEnd  
    | AcctDownloadEnd String  
    | ExecutionDataEnd ReqId  
    | DeltaNeutralValidation  
    { reqId :: Int
    , underComp :: UnderComp
    }
    | TickSnapshotEnd ReqId  
    | MarketDataType 
    { reqId :: Int
    , marketDataType :: Int
    } 
    | CommissionReport  
    { execId :: String 
    , commission :: Double
    , currency :: String
    , realizedPNL :: Double
    , yield :: Double
    , yieldRedemptionDate :: Int 
    }
    | UnusedMsg2
    | PositionData  
    { account :: String
    , contract :: Contract
    , position :: Int
    , avgCost :: Double
    }
    | PositionEnd  
    | AccountSummary  
    { reqId :: Int
    , account :: String
    , tag :: String
    , value :: String
    , currency :: String
    }
    | AccountSummaryEnd ReqId 
    | VerifyMessageAPI String   
    | VerifyCompleted  
    { isSuccessful :: String
    , errorText :: String
    }
    | DisplayGroupList  
    { reqId :: Int
    , groups :: String
    }
    | DisplayGroupUpdated  
    { reqId :: Int
    , contractInfo :: String
    }
    | IBUnknown 
        deriving (Typeable, Data, Show)

data RecvMsg = 
    RecvMsg
    { rc_msgId  ::  Int
    , rc_version :: Int
    , rc_msgBody :: IBMessage
    } 

type ReqId = Int
type OrderComboLeg = Double
type TickerId = Int
type OrderId  = Int
type GroupId  = Int
type MIB = MVar IBServer

type HandlerFunc = (MIB -> IBMessage -> IO ())

data IBServer = 
    IBServer
    { s_addr :: String
    , s_port :: Int
    , s_clientId :: Int
    , s_extraAuth :: Bool
    , s_version :: Int
    , s_connected :: Bool
    , s_twsTime :: String
    , s_debug :: Bool
    , s_sock :: Maybe Handle
    , s_msgThread :: Maybe ThreadId
    , s_timeoutInterval :: Int
    , s_handler :: HandlerFunc
    }

data TickType =
  BID_SIZE |
  BID      |
  ASK      |
  ASK_SIZE |
  LAST     |
  LAST_SIZE|
  HIGH     |
  LOW      |
  VOLUME   |
  CLOSE    |
  BID_OPTION_COMPUTATION |
  ASK_OPTION_COMPUTATION |
  LAST_OPTION_COMPUTATION|
  MODEL_OPTION |
  OPEN |
  LOW_13_WEEK |
  HIGH_13_WEEK |
  LOW_26_WEEK |
  HIGH_26_WEEK |
  LOW_52_WEEK |
  HIGH_52_WEEK |
  AVG_VOLUME |
  OPEN_INTEREST |
  OPTION_HISTORICAL_VOL |
  OPTION_IMPLIED_VOL |
  OPTION_BID_EXCH |
  OPTION_ASK_EXCH |
  OPTION_CALL_OPEN_INTEREST |
  OPTION_PUT_OPEN_INTEREST |
  OPTION_CALL_VOLUME |
  OPTION_PUT_VOLUME |
  INDEX_FUTURE_PREMIUM |
  BID_EXCH |
  ASK_EXCH |
  AUCTION_VOLUME
   deriving (Show, Read, Eq, Enum)

data FaDataType =
    GROUPS |
    PROFILES |
    ALIASES 
        deriving (Data, Typeable, Show, Read, Eq, Enum)

data Origin =
    CUSTOMER |
    FIRM     |
    UNKNOWN 
        deriving (Data, Typeable, Show, Eq, Read, Enum)

data NewsBulletin =
    NEWS_MSG | EXCHANGE_AVAIL_MSG | EXCHANGE_UNAVAIL_MSG
        deriving (Show, Read, Enum)


dblMaximum = encodeFloat 1 $ snd $ floatRange (0.0::Double)

int32max :: Int
int32max = fromIntegral (maxBound :: Int32) :: Int
int32min :: Int
int32min = fromIntegral (minBound :: Int32) :: Int

dblCheckNegative :: Double -> Double
dblCheckNegative inp
    | inp < 0 = dblMaximum
    | otherwise = inp

dblDefaultCheck :: Double -> Double
dblDefaultCheck = dblBoundsCheck 1.0 (-1.0)

dblBoundsCheck :: Double -> Double -> Double -> Double
dblBoundsCheck upperBounds lowerBounds inp
    | inp > upperBounds || inp < lowerBounds = dblMaximum
    | otherwise = inp

fromEnum' :: Enum a =>  a -> Int
fromEnum' a = fromEnum a +1

fromBool :: Num a => Bool -> a
fromBool False  = 0
fromBool True   = 1

isPrice :: TickType -> Bool
isPrice x = (x == BID) || (x == ASK) || (x == LAST) 

conToId :: Data a => a -> Int 
conToId = constrIndex . toConstr 

msgToId :: IBMessage -> Int
msgToId msg | (conToId msg) > 21 = conToId msg + 23
msgToId msg | otherwise = conToId msg

reqToId :: Request -> Int
reqToId rq | (conToId rq) > 25 = conToId rq + 23
reqToId rq | otherwise = conToId rq

idToMsg :: Int -> IBMessage
idToMsg id | id > 25 = fromConstr $ indexConstr (dataTypeOf IBUnknown) (id - 23)
idToMsg id | otherwise = fromConstr $ indexConstr (dataTypeOf IBUnknown) id 

data Preamble = 
    Preamble
    { pre_serverVersion :: Int
    , pre_twsTime :: String
    }

data Execution = 
    Execution 
    { ex_execId :: String
    , ex_time   :: String
    , ex_acctNumber     :: String
    , ex_exchange       :: String
    , ex_side   :: String
    , ex_shares :: Int
    , ex_price  :: Double
    , ex_permId :: Int
    , ex_clientId       :: Int
    , ex_liquidation    :: Int
    , ex_orderId        :: Int
    , ex_cumQty :: Int
    , ex_avgPrice       :: Double
    , ex_orderRef       :: String
    , ex_evRule :: String
    , ex_evMultiplier   :: Double
    } deriving (Data, Typeable, Show)

data ExecutionFilter =
    ExecutionFilter
    { exf_clientId :: Int
    , exf_acctCode :: String
    , exf_time :: String
    , exf_symbol :: String
    , exf_secType :: String
    , exf_exchange :: String
    , exf_side :: String
    } deriving (Data, Typeable)

data BarData = 
    BarData 
    { bar_date :: String
    , bar_open :: Double
    , bar_high :: Double
    , bar_low :: Double
    , bar_close :: Double
    , bar_volume :: Int
    , bar_average :: Double
    , bar_hasGaps :: String
    , bar_barCount :: Int
    } deriving (Typeable, Data, Show)

data ScanData  =
    ScanData
    { sd_rank :: Int
    , sd_contract :: ContractDetails
    , sd_distance :: String
    , sd_benchmark :: String
    , sd_projection :: String
    , sd_legsStr :: String
    } deriving (Typeable, Data, Show)

data OrderState =
    OrderState
    { os_status :: String
    , os_initMargin :: String
    , os_maintMargin :: String
    , os_equityWithLoan :: String
    , os_commission :: Double
    , os_minCommission :: Double
    , os_maxCommission :: Double
    , os_commissionCurrency :: String
    , os_warningText :: String
    } deriving (Typeable, Data, Show)

data TagValue = 
    TagValue 
    { tv_tag :: String
    , tv_value :: String
    } deriving (Typeable, Data, Show)


data ScannerSubscription = 
    ScannerSubscription
    { ssb_numberOfRows :: Int 
    , ssb_instrument :: String 
    , ssb_locationCode :: String 
    , ssb_scanCode :: String 
    , ssb_abovePrice :: Double 
    , ssb_belowPrice :: Double 
    , ssb_aboveVolume :: Int 
    , ssb_marketCapAbove :: Double 
    , ssb_marketCapBelow :: Double 
    , ssb_moodyRatingAbove :: String 
    , ssb_moodyRatingBelow :: String 
    , ssb_spRatingAbove :: String 
    , ssb_spRatingBelow :: String 
    , ssb_maturityDateAbove :: String 
    , ssb_maturityDateBelow :: String 
    , ssb_couponRateAbove :: Double 
    , ssb_couponRateBelow :: Double 
    , ssb_excludeConvertible :: Int 
    , ssb_averageOptionVolumeAbove :: Int 
    , ssb_scannerSettingPairs :: String 
    , ssb_stockTypeFilter :: String 
    } deriving (Data, Typeable)

defScannerSubscription = ScannerSubscription { ssb_numberOfRows = -1
                                             , ssb_instrument = ""
                                             , ssb_locationCode = ""
                                             , ssb_scanCode = ""
                                             , ssb_abovePrice = dblMaximum
                                             , ssb_belowPrice = dblMaximum
                                             , ssb_aboveVolume = int32max
                                             , ssb_marketCapAbove = dblMaximum
                                             , ssb_marketCapBelow = dblMaximum
                                             , ssb_moodyRatingAbove = ""
                                             , ssb_moodyRatingBelow = ""
                                             , ssb_spRatingAbove = ""
                                             , ssb_spRatingBelow = ""
                                             , ssb_maturityDateAbove = ""
                                             , ssb_maturityDateBelow = ""
                                             , ssb_couponRateAbove = dblMaximum
                                             , ssb_couponRateBelow = dblMaximum
                                             , ssb_excludeConvertible = 0
                                             , ssb_averageOptionVolumeAbove = 0
                                             , ssb_scannerSettingPairs = ""
                                             , ssb_stockTypeFilter = ""
                                             }
data ComboLeg = 
    ComboLeg 
    { cl_conId :: Int
    , cl_ratio :: Int
    , cl_action :: String --BUY/SELL/SSHORT
    , cl_exchange :: String
    , cl_openClose :: Int -- LegOpenClose enum values
    , cl_shortSaleSlot :: Int -- 1 = clearing broker, 2 = third party
    , cl_designatedLocation :: String
    , cl_exemptCode :: Int
    } deriving (Typeable, Data, Show)

data UnderComp = 
    UnderComp 
    { uc_conId  :: Int
    , uc_delta  :: Double
    , uc_price  :: Double
    } deriving (Typeable, Data, Eq, Show)

defUnderComp :: UnderComp
defUnderComp =
    UnderComp
    { uc_conId = 0
    , uc_delta = 0.0
    , uc_price = 0.0
    }

data Contract = 
    Contract
    { ct_conId :: Int
    , ct_symbol :: String
    , ct_secType :: String
    , ct_expiry :: String
    , ct_strike :: Double
    , ct_right :: String
    , ct_multiplier :: String
    , ct_exchange :: String
    , ct_primaryExchange :: String -- pick an actual (ie non-aggregate) exchange that the contract trades on.  DO NOT SET TO SMART.
    , ct_currency :: String
    , ct_localSymbol :: String
    , ct_tradingClass :: String
    , ct_includeExpired :: Bool
    , ct_secIdType :: String -- CUSIPSEDOLISINRIC
    , ct_secId :: String
     -- COMBOS
    , ct_comboLegsDescrip :: String -- received in open order 14 and up for all combos
    , ct_comboLegsList :: [ComboLeg] 
     -- combo legs
     --typedef std::vector<ComboLegSPtr> ComboLegList
     --typedef shared_ptr<ComboLegList> ComboLegListSPtr
     --ComboLegListSPtr comboLegs
     -- delta neutral
    , ct_underComp :: UnderComp
    } deriving (Typeable, Data, Show)

defContract :: Contract
defContract = 
    Contract
    { ct_conId = 0
    , ct_symbol = ""
    , ct_secType= ""
    , ct_expiry = ""
    , ct_strike = 0.0
    , ct_right = ""
    , ct_multiplier = ""
    , ct_exchange = ""
    , ct_primaryExchange= ""
    , ct_currency = ""
    , ct_localSymbol = ""
    , ct_tradingClass = ""
    , ct_includeExpired = False
    , ct_secIdType = ""
    , ct_secId= ""
    , ct_comboLegsDescrip= ""
    , ct_comboLegsList = []
    , ct_underComp = defUnderComp
    }

data ContractDetails = 
    ContractDetails
    { ctd_summary   :: Contract
    , ctd_marketName :: String
    , ctd_minTick :: Double
    , ctd_orderTypes :: String
    , ctd_validExchanges :: String
    , ctd_priceMagnifier  :: Int
    , ctd_underConId  :: Int
    , ctd_longName  :: String
    , ctd_contractMonth :: String
    , ctd_industry :: String
    , ctd_category      :: String
    , ctd_subcategory   :: String
    , ctd_timeZoneId    :: String
    , ctd_tradingHours  :: String
    , ctd_liquidHours   :: String
    , ctd_evRule        :: String
    , ctd_evMultiplier  :: Double
    , ctd_secIdList     :: [TagValue]
      --BOND	values
    , ctd_cusip :: String
    , ctd_ratings       :: String
    , ctd_descAppend    :: String
    , ctd_bondType      :: String
    , ctd_couponType    :: String
    , ctd_callable      :: Bool
    , ctd_putable       :: Bool
    , ctd_coupon        :: Double
    , ctd_convertible   :: Bool
    , ctd_maturity      :: String
    , ctd_issueDate     :: String
    , ctd_nextOptionDate        :: String
    , ctd_nextOptionType        :: String
    , ctd_nextOptionPartial     :: Bool
    , ctd_notes :: String
    } deriving (Data, Typeable, Show)

defContractDetails :: ContractDetails
defContractDetails = 
    ContractDetails 
    { ctd_summary = defContract
    , ctd_marketName = ""
    , ctd_minTick = 0.0
    , ctd_orderTypes = ""
    , ctd_validExchanges = ""
    , ctd_priceMagnifier = 0
    , ctd_underConId = int32max 
    , ctd_longName = ""
    , ctd_contractMonth = ""
    , ctd_industry = ""
    , ctd_category = ""
    , ctd_subcategory = ""
    , ctd_timeZoneId = ""
    , ctd_tradingHours = ""
    , ctd_liquidHours = ""
    , ctd_evRule = ""
    , ctd_evMultiplier = dblMaximum
    , ctd_secIdList = []
    , ctd_cusip  = ""
    , ctd_ratings = ""
    , ctd_descAppend = ""
    , ctd_bondType = ""
    , ctd_couponType     = ""
    , ctd_callable       = False
    , ctd_putable = False
    , ctd_coupon = 0.0
    , ctd_convertible = False
    , ctd_maturity       = ""
    , ctd_issueDate = ""
    , ctd_nextOptionDate = ""
    , ctd_nextOptionType = ""
    , ctd_nextOptionPartial = False
    , ctd_notes  = ""
    }

data Order = 
    Order 
    { ord_orderId       :: Int
    , ord_clientId      :: Int
    , ord_permId        :: Int

    , ord_action        :: String
    , ord_totalQuantity :: Int
    , ord_orderType     :: String
    , ord_lmtPrice      :: Double
    , ord_auxPrice      :: Double

    , ord_tif   :: String           -- "Time in Force" - DAY, GTC, etc.
    , ord_activeStartTime       :: String       -- for GTC orders
    , ord_activeStopTime        :: String       -- for GTC orders
    , ord_ocaGroup      :: String      -- one cancels all group name
    , ord_ocaType       :: Int       -- 1 = CANCEL_WITH_BLOCK, 2 = REDUCE_WITH_BLOCK, 3 = REDUCE_NON_BLOCK
    , ord_orderRef      :: String      -- order reference
    , ord_transmit      :: Bool      -- if false, order will be created but not transmited
    , ord_parentId      :: Int      -- Parent order Id, to associate Auto STP or TRAIL orders with the original order.
    , ord_blockOrder    :: Bool
    , ord_sweepToFill   :: Bool
    , ord_displaySize   :: Int
    , ord_triggerMethod :: Int -- 0=Default, 1=Double_Bid_Ask, 2=Last, 3=Double_Last, 4=Bid_Ask, 7=Last_or_Bid_Ask, 8=Mid-po:: Int
    , ord_outsideRth    :: Bool
    , ord_hidden        :: Bool
    , ord_goodAfterTime :: String    -- Format: 20060505 08:00:00 {time zone}
    , ord_goodTillDate  :: String     -- Format: 20060505 08:00:00 {time zone}
    , ord_rule80A       :: String -- Individual = 'I', Agency = 'A', AgentOtherMember = 'W', IndividualPTIA = 'J', AgencyPTIA = 'U', AgentOtherMemberPTIA = 'M', IndividualPT = 'K', AgencyPT = 'Y', AgentOtherMemberPT = 'N'
    , ord_allOrNone     :: Bool
    , ord_minQty        :: Int
    , ord_percentOffset :: Double -- REL orders only
    , ord_overridePercentageConstraints :: Bool
    , ord_trailStopPrice        :: Double -- TRAILLIMIT orders only
    , ord_trailingPercent       :: Double
        -- financial advisors only
    , ord_faGroup       :: String
    , ord_faProfile     :: String
    , ord_faMethod      :: String
    , ord_faPercentage  :: String
        -- institutional (ie non-cleared) only
    , ord_openClose     :: String -- O=Open, C=Close
    , ord_origin :: Origin    -- 0=Customer, 1=Firm
    , ord_shortSaleSlot :: Int -- 1 if you hold the shares, 2 if they will be delivered from elsewhere.  Only for Action="SSHORT
    , ord_designatedLocation    :: String -- set when slot=2 only.
    , ord_exemptCode    :: Int
        -- SMART routing only
    , ord_discretionaryAmt      :: Double
    , ord_eTradeOnly    :: Bool
    , ord_firmQuoteOnly :: Bool
    , ord_nbboPriceCap  :: Double
    , ord_optOutSmartRouting    :: Bool
        -- BOX exchange orders only
    , ord_auctionStrategy       :: Int -- AUCTION_MATCH, AUCTION_IMPROVEMENT, AUCTION_TRANSPARENT
    , ord_startingPrice :: Double
    , ord_stockRefPrice :: Double
    , ord_delta :: Double
      -- pegged to stock and VOL orders only
    , ord_stockRangeLower       :: Double
    , ord_stockRangeUpper       :: Double
      -- VOLATILITY ORDERS ONLY
    , ord_volatility    :: Double
    , ord_volatilityType        :: Int     -- 1=daily, 2=annual
    , ord_deltaNeutralOrderType :: String
    , ord_deltaNeutralAuxPrice  :: Double
    , ord_deltaNeutralConId     :: Int
    , ord_deltaNeutralSettlingFirm      :: String
    , ord_deltaNeutralClearingAccount   :: String
    , ord_deltaNeutralClearingIntent    :: String
    , ord_deltaNeutralOpenClose :: String
    , ord_deltaNeutralShortSale :: Bool
    , ord_deltaNeutralShortSaleSlot     :: Int
    , ord_deltaNeutralDesignatedLocation        :: String
    , ord_continuousUpdate      :: Bool
    , ord_referencePriceType    :: Int -- 1=Average, 2 = BidOrAsk
        -- COMBO ORDERS ONLY
    , ord_basisPoints   :: Double      -- EFP orders only
    , ord_basisPointsType       :: Int  -- EFP orders only
     -- SCALE ORDERS ONLY
    , ord_scaleInitLevelSize    :: Int
    , ord_scaleSubsLevelSize    :: Int
    , ord_scalePriceIncrement   :: Double
    , ord_scalePriceAdjustValue :: Double
    , ord_scalePriceAdjustInterval      :: Int
    , ord_scaleProfitOffset     :: Double
    , ord_scaleAutoReset        :: Bool
    , ord_scaleInitPosition     :: Int
    , ord_scaleInitFillQty      :: Int
    , ord_scaleRandomPercent    :: Bool
    , ord_scaleTable    :: String
      -- HEDGE ORDERS
    , ord_hedgeType     :: String  -- 'D' - delta, 'B' - beta, 'F' - FX, 'P' - pair
    , ord_hedgeParam    :: String -- 'beta=X' value for beta hedge, 'ratio=Y' for pair hedge
      -- Clearing info
    , ord_account       :: String -- IB account
    , ord_settlingFirm  :: String
    , ord_clearingAccount       :: String -- True beneficiary of the order
    , ord_clearingIntent        :: String -- "" (Default), "IB", "Away", "PTA" (PostTrade)
    -- ALGO ORDERS ONLY
    , ord_algoStrategy  :: String
    , ord_algoParams    :: [TagValue]
    , ord_smartComboRoutingParams   :: [TagValue]
      -- What-if
    , ord_whatIf        :: Bool
      -- Not Held
    , ord_notHeld       :: Bool
        
    -- order combo legs
    --typedef std::vector<OrderComboLegSPtr> OrderComboLegList
    --typedef shared_ptr<OrderComboLegList> OrderComboLegListSPtr
    --
    --orderComboLegs    OrderComboLegListSPtr
    --orderMiscOptions  TagValueListSPtr
    } deriving (Data, Typeable, Show)

defOrder :: Order
defOrder =
    Order
    { ord_orderId  = 0
    , ord_clientId = 0
    , ord_permId   = 0
    , ord_action = ""
    , ord_totalQuantity = 0
    , ord_orderType = ""
    , ord_lmtPrice      = dblMaximum
    , ord_auxPrice      = dblMaximum
    , ord_tif = ""
    , ord_activeStartTime = ""
    , ord_activeStopTime = ""
    , ord_ocaGroup = ""
    , ord_ocaType        = 0
    , ord_orderRef = ""
    , ord_transmit       = True
    , ord_parentId       = 0
    , ord_blockOrder     = False
    , ord_sweepToFill    = False
    , ord_displaySize    = 0
    , ord_triggerMethod  = 0
    , ord_outsideRth     = False
    , ord_hidden         = False
    , ord_goodAfterTime = ""
    , ord_goodTillDate = ""
    , ord_rule80A = ""
    , ord_allOrNone      = False
    , ord_minQty         = int32max
    , ord_percentOffset  = dblMaximum
    , ord_overridePercentageConstraints = False
    , ord_trailStopPrice = dblMaximum
    , ord_trailingPercent = dblMaximum
    , ord_faGroup = ""
    , ord_faProfile = ""
    , ord_faMethod = ""
    , ord_faPercentage = ""
    , ord_openClose     = "O"
    , ord_origin        = CUSTOMER
    , ord_shortSaleSlot = 0
    , ord_designatedLocation = ""
    , ord_exemptCode    = -1
    , ord_discretionaryAmt = 0
    , ord_eTradeOnly       = True
    , ord_firmQuoteOnly    = True
    , ord_nbboPriceCap     = dblMaximum
    , ord_optOutSmartRouting = False
    , ord_auctionStrategy = 0
    , ord_startingPrice   = dblMaximum
    , ord_stockRefPrice   = dblMaximum
    , ord_delta           = dblMaximum
    , ord_stockRangeLower = dblMaximum
    , ord_stockRangeUpper = dblMaximum
    , ord_volatility            = dblMaximum
    , ord_volatilityType        = int32max 
    , ord_deltaNeutralOrderType = ""
    , ord_deltaNeutralAuxPrice  = dblMaximum
    , ord_deltaNeutralConId     = 0
    , ord_deltaNeutralSettlingFirm = ""
    , ord_deltaNeutralClearingAccount = ""
    , ord_deltaNeutralClearingIntent = ""
    , ord_deltaNeutralOpenClose = ""
    , ord_deltaNeutralShortSale = False
    , ord_deltaNeutralShortSaleSlot = 0
    , ord_deltaNeutralDesignatedLocation = ""
    , ord_continuousUpdate      = False
    , ord_referencePriceType    = int32max
    , ord_basisPoints     = dblMaximum  
    , ord_basisPointsType = int32max 
    , ord_scaleInitLevelSize  = int32max
    , ord_scaleSubsLevelSize  = int32max
    , ord_scalePriceIncrement = dblMaximum
    , ord_scalePriceAdjustValue = dblMaximum
    , ord_scalePriceAdjustInterval = int32max
    , ord_scaleProfitOffset = dblMaximum
    , ord_scaleAutoReset = False
    , ord_scaleInitPosition = int32max
    , ord_scaleInitFillQty = int32max
    , ord_scaleRandomPercent = False
    , ord_scaleTable = ""
    , ord_hedgeType = ""
    , ord_hedgeParam = ""
    , ord_account = ""
    , ord_settlingFirm = "" 
    , ord_clearingAccount = ""
    , ord_clearingIntent = ""
    , ord_algoStrategy = ""
    , ord_algoParams = []
    , ord_smartComboRoutingParams = [] 
    , ord_whatIf = False
    , ord_notHeld = False
    }

