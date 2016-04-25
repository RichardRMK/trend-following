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
      ExitStop   : decimal option }

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

//-------------------------------------------------------------------------------------------------

type JournalExecuteTake =
    { Shares     : uint32
      Price      : decimal }

type JournalExecuteExit =
    { Shares     : uint32
      Price      : decimal }

type JournalLiquidation =
    { Shares     : uint32
      Price      : decimal }

type JournalPayDividend =
    { Shares     : uint32
      Amount     : decimal }

type JournalSplitShares =
    { SharesNew  : uint32
      SharesOld  : uint32
      ExcessOld  : decimal
      Price      : decimal }

type JournalDetail =
    | ExecuteTake of JournalExecuteTake
    | ExecuteExit of JournalExecuteExit
    | Liquidation of JournalLiquidation
    | PayDividend of JournalPayDividend
    | SplitShares of JournalSplitShares

type JournalLog =
    { Date       : DateTime
      Ticker     : string
      Shares     : int
      Cash       : decimal
      Detail     : JournalDetail }

//-------------------------------------------------------------------------------------------------

type TakeOrder =
    { Ticker     : string
      Shares     : uint32 }

type ExitOrder =
    { Ticker     : string
      Shares     : uint32
      Stop       : decimal }

type Order =
    | Take of TakeOrder
    | Exit of ExitOrder

//-------------------------------------------------------------------------------------------------

type Model<'T> =
    { ComputeMetricsLog : RecordsLog -> ElementLog<'T> option -> 'T
      ComputeTakeOrders : ElementLog<'T>[] -> SummaryLog -> TakeOrder[]
      CalculateExitStop : ElementLog<'T> -> decimal }

type Simulation<'T> =
    { Principal         : decimal
      GetQuotes         : DateTime -> Quote[]
      Dates             : DateTime seq
      Model             : Model<'T>
      ReportElementLog  : ElementLog<'T> -> unit
      ReportSummaryLog  : SummaryLog -> unit
      ReportJournalLog  : JournalLog -> unit
      ReportCompletion  : unit -> unit }
