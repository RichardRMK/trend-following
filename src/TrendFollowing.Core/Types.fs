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
      SplitNew   : uint32 option
      SplitOld   : uint32 option }

type RecordsLog =
    { Date       : DateTime
      Ticker     : string
      Count      : uint32
      Hi         : decimal
      Lo         : decimal
      Close      : decimal
      Dividend   : decimal
      SplitNew   : uint32
      SplitOld   : uint32
      DeltaHi    : decimal
      DeltaLo    : decimal
      Shares     : uint32
      StopLoss   : decimal option }

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
      Shares     : uint32 }

type ExitOrder =
    { Ticker     : string
      Shares     : uint32
      StopLoss   : decimal }

type Order =
    | Take of TakeOrder
    | Exit of ExitOrder

//-------------------------------------------------------------------------------------------------

type System<'T> =
    { Principal         : decimal
      DateSequence      : DateTime seq
      GetQuotes         : DateTime -> Quote[]
      ComputeMetricsLog : RecordsLog -> 'T option -> 'T
      ComputeTakeOrders : ElementLog<'T>[] -> SummaryLog -> TakeOrder[]
      CalculateStopLoss : ElementLog<'T> -> decimal
      ReportElementLog  : ElementLog<'T> -> unit
      ReportSummaryLog  : SummaryLog -> unit
      ReportTradingLog  : TradingLog -> unit }
