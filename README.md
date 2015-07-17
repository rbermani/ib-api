# Interactive Brokers API
An API for Interactive Brokers Trader Workstation written in pure Haskell with minimal dependencies. It was based on 9.71 of the IB API.

## Status
This library is completely experimental and untested. I have not even attempted to use it to connect to TWS yet. Please use at your own risk, and if you find bugs, feel free to commit fixes.

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
*  CancelMktDepth 
*  NewsBulletinsReq 
*  CancelNewsBulletins
*  SetServerLogLevel 
*  AutoOpenOrdersReq 
*  AllOpenOrdersReq
*  ManagedAcctsReq
*  FAReq FaDataType
*  FAReplaceReq 
*  HistoricalDataReq 
*  ExerciseOptionsReq  
*  ScannerSubscriptionReq
*  CancelScannerSubscription 
*  ScannerParametersReq  
*  CancelHistoricalData 
*  CurrentTimeReq
*  RealTimeBarsReq   
*  CancelRealTimeBars 
*  FundamentalDataReq
*  CancelFundamentalData 
*  ImpliedVolatilityReq
*  CalcOptionPriceReq  
*  CancelCalcImpliedVolatility 
*  CancelCalcOptionPrice 
*  GlobalCancelReq
*  MarketDataTypeReq 
*  PositionsReq
*  AccountSummaryReq
*  CancelAccountSummary 
*  CancelPositions
*  VerifyReq
*  VerifyMessage 
*  QueryDisplayGroups 
*  SubscribeToGroupEvents  
*  UpdateDisplayGroup
*  UnsubscribeFromGroupEvents 
*  StartApi

### Responses
*  TickPrice 
*  TickSize 
*  OrderStatus 
*  Err  
*  OpenOrder 
*  AcctValue  
*  PortfolioValue
*  AcctUpdateTime  
*  NextValidId   
*  ContractData   
*  ExecutionData  
*  MarketDepth 
*  MarketDepthL2 
*  NewsBulletins  
*  ManagedAccts   
*  ReceiveFA  
*  HistoricalData 
*  BondContractData   
*  ScannerParameters   
*  ScannerData  
*  TickOptionComputation 
*  TickGeneric  
*  TickString
*  TickEFP  
*  CurrentTime nt   
*  RealTimeBars  
*  FundamentalData 
*  ContractDataEnd   
*  OpenOrderEnd  
*  AcctDownloadEnd String  
*  ExecutionDataEnd   
*  DeltaNeutralValidation  
*  TickSnapshotEnd   
*  MarketDataType 
*  CommissionReport  
*  PositionData  
*  PositionEnd  
*  AccountSummary  
*  AccountSummaryEnd
*  VerifyMessageAPI 
*  VerifyCompleted  
*  DisplayGroupList  
*  DisplayGroupUpdated  
