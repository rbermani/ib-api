# Interactive Brokers API
An API for Interactive Brokers Trader Workstation written in pure Haskell with minimal dependencies. It was written based on 9.71 of tge IB API.


## API Coverage

The following messages are supported.

### Requests
* MktDataReq 
* CancelMktData 
* PlaceOrder
*  CancelOrder
*  OpenOrdersReq
*  AccountUpdatesReq 
*  ExecutionsReq
*  IdsReq Int 
*  ContractDetailsReq
*  MktDepthReq  
*  CancelMktDepth TickerId
*  NewsBulletinsReq Bool
*  CancelNewsBulletins
*  SetServerLogLevel Int
*  AutoOpenOrdersReq Bool
*  AllOpenOrdersReq
*  ManagedAcctsReq
*  FAReq FaDataType
*  FAReplaceReq 
*  HistoricalDataReq 
*  ExerciseOptionsReq  
*  ScannerSubscriptionReq
*  CancelScannerSubscription TickerId
*  ScannerParametersReq  
*  CancelHistoricalData TickerId
*  CurrentTimeReq
*  RealTimeBarsReq   
*  CancelRealTimeBars TickerId
*  FundamentalDataReq
*  CancelFundamentalData TickerId
*  ImpliedVolatilityReq
*  CalcOptionPriceReq  
*  CancelCalcImpliedVolatility TickerId
*  CancelCalcOptionPrice TickerId
*  GlobalCancelReq
*  MarketDataTypeReq TickerId
*  PositionsReq
*  AccountSummaryReq
*  CancelAccountSummary ReqId
*  CancelPositions
*  VerifyReq
*  VerifyMessage String
*  QueryDisplayGroups ReqId
*  SubscribeToGroupEvents ReqId GroupId
*  UpdateDisplayGroup
*  UnsubscribeFromGroupEvents ReqId
*  StartApi

### Responses
*  TickPrice 
*  TickSize 
*  OrderStatus 
*  Err  
*  OpenOrder 
*  AcctValue  
*  PortfolioValue
*  AcctUpdateTime String 
*  NextValidId Int  
*  ContractData ContractDetails  
*  ExecutionData  
*  MarketDepth 
*  MarketDepthL2 
*  NewsBulletins  
*  ManagedAccts String  
*  ReceiveFA  
*  HistoricalData 
*  BondContractData ContractDetails  
*  ScannerParameters String  
*  ScannerData  
*  TickOptionComputation 
*  TickGeneric  
*  TickString
*  TickEFP  
*  CurrentTime Int   
*  RealTimeBars  
*  FundamentalData 
*  ContractDataEnd ReqId  
*  OpenOrderEnd  
*  AcctDownloadEnd String  
*  ExecutionDataEnd ReqId  
*  DeltaNeutralValidation  
*  TickSnapshotEnd ReqId  
*  MarketDataType 
*  CommissionReport  
*  PositionData  
*  PositionEnd  
*  AccountSummary  
*  AccountSummaryEnd ReqId 
*  VerifyMessageAPI String   
*  VerifyCompleted  
*  DisplayGroupList  
*  DisplayGroupUpdated  


