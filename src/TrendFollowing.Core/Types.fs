module TrendFollowing.Types

open System

//-------------------------------------------------------------------------------------------------

type Quote =
    { Date       : DateTime
      Ticker     : string
      Hi         : decimal
      Lo         : decimal
      Close      : decimal
      Dividend   : decimal option
      SplitNew   : int option
      SplitOld   : int option }

type RecordsLog =
    { Date       : DateTime
      Ticker     : string
      Count      : int
      Hi         : decimal
      Lo         : decimal
      Close      : decimal
      Dividend   : decimal
      SplitNew   : int
      SplitOld   : int
      DeltaHi    : decimal
      DeltaLo    : decimal
      Shares     : int
      StopLoss   : decimal }

type ElementLog<'T> =
    { RecordsLog : RecordsLog
      MetricsLog : 'T }

type SummaryLog =
    { Date       : DateTime
      Cash       : decimal
      Equity     : decimal
      ExitValue  : decimal
      Peak       : decimal
      Drawdown   : decimal
      Leverage   : decimal }

type TradingLog =
    { Date       : DateTime
      Ticker     : string
      Shares     : int
      Price      : decimal }

type TakeOrder =
    { Ticker     : string
      Shares     : int }

type ExitOrder =
    { Ticker     : string
      Shares     : int
      StopLoss   : decimal }

type Order =
    | Take of TakeOrder
    | Exit of ExitOrder

//-------------------------------------------------------------------------------------------------

type System<'T> =
    { Principal         : decimal
      DateSequence      : DateTime seq
      GetQuotes         : DateTime -> Quote[]
      ComputeMetrics    : RecordsLog -> 'T option -> 'T
      ComputeTakeOrders : ElementLog<'T>[] -> SummaryLog -> TakeOrder[]
      CalculateStopLoss : ElementLog<'T> -> decimal
      EmitElementLog    : ElementLog<'T> -> unit
      EmitSummaryLog    : SummaryLog -> unit
      EmitTradingLog    : TradingLog -> unit }
