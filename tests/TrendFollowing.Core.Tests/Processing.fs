module TrendFollowing.Tests.Processing

open System
open NUnit.Framework
open FsUnit
open TrendFollowing.Types
open TrendFollowing.Processing

//-------------------------------------------------------------------------------------------------

let private unexpectedCall () = failwith "Unexpected call."

let private convertDetail journalLog (conversion : obj -> 'T) =
    match journalLog.Detail with
    | ExecuteTake detail -> detail |> conversion
    | ExecuteExit detail -> detail |> conversion
    | Liquidation detail -> detail |> conversion
    | PayDividend detail -> detail |> conversion
    | SplitShares detail -> detail |> conversion

let private (-->) journalLog (_ : 'T -> JournalDetail) =
    convertDetail journalLog (fun x -> x :?> 'T)

let private sieve (_ : 'T -> JournalDetail) journalLog =
    convertDetail journalLog (fun x -> x :? 'T)

let private toQuote ticker (date, hi, lo, close, dividend, splitNew, splitOld) : Quote =

    { Date = date
      Ticker = ticker
      Hi = hi
      Lo = lo
      Close = close
      Dividend = dividend
      SplitNew = splitNew
      SplitOld = splitOld }

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Baseline increment`` () =

    let date1 = DateTime(2000, 01, 01)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateExitStop = (fun _ -> unexpectedCall ()) }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state1

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 0
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 0

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date1
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 1u
    recordsLog.Hi          |> should equal 101.00m
    recordsLog.Lo          |> should equal 100.00m
    recordsLog.Close       |> should equal 100.50m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal 0m
    recordsLog.DeltaLo     |> should equal 0m
    recordsLog.Shares      |> should equal 0u
    recordsLog.ExitStop    |> should equal None

    summaryLog.Date        |> should equal date1
    summaryLog.Cash        |> should equal 1000000.00m
    summaryLog.Equity      |> should equal 1000000.00m
    summaryLog.ExitValue   |> should equal 1000000.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal 0m
    summaryLog.Leverage    |> should equal 0m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 107.00m, 105.00m, 106.00m, None, None, None
           date2, 108.00m, 106.00m, 107.00m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateExitStop = (fun _ -> unexpectedCall ()) }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state2

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date2
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 2u
    recordsLog.Hi          |> should equal 108.00m
    recordsLog.Lo          |> should equal 106.00m
    recordsLog.Close       |> should equal 107.00m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal ((108.00m / 107.00m) - 1m)
    recordsLog.DeltaLo     |> should equal ((106.00m / 105.00m) - 1m)
    recordsLog.Shares      |> should equal 0u
    recordsLog.ExitStop    |> should equal None

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, dividend 10%`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 107.00m, 105.00m, 106.00m, None       , None, None
           date2,  97.20m,  95.40m,  96.30m, Some 10.60m, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateExitStop = (fun _ -> unexpectedCall ()) }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state2

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date2
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 2u
    recordsLog.Hi          |> should equal 97.20m
    recordsLog.Lo          |> should equal 95.40m
    recordsLog.Close       |> should equal 96.30m
    recordsLog.Dividend    |> should equal 10.60m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal ((97.20m / (107.00m * (1m - (10.60m / 106.00m)))) - 1m)
    recordsLog.DeltaLo     |> should equal ((95.40m / (105.00m * (1m - (10.60m / 106.00m)))) - 1m)
    recordsLog.Shares      |> should equal 0u
    recordsLog.ExitStop    |> should equal None

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, dividend 50%`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 107.00m, 105.00m, 106.00m, None       , None, None
           date2,  54.00m,  53.00m,  53.50m, Some 53.00m, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateExitStop = (fun _ -> unexpectedCall ()) }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state2

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date2
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 2u
    recordsLog.Hi          |> should equal 54.00m
    recordsLog.Lo          |> should equal 53.00m
    recordsLog.Close       |> should equal 53.50m
    recordsLog.Dividend    |> should equal 53.00m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal ((54.00m / (107.00m * (1m - (53.00m / 106.00m)))) - 1m)
    recordsLog.DeltaLo     |> should equal ((53.00m / (105.00m * (1m - (53.00m / 106.00m)))) - 1m)
    recordsLog.Shares      |> should equal 0u
    recordsLog.ExitStop    |> should equal None

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, split 2:1`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 107.00m, 105.00m, 106.00m, None, None   , None
           date2,  54.00m,  53.00m,  53.50m, None, Some 2u, Some 1u |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateExitStop = (fun _ -> unexpectedCall ()) }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state2

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date2
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 2u
    recordsLog.Hi          |> should equal 54.00m
    recordsLog.Lo          |> should equal 53.00m
    recordsLog.Close       |> should equal 53.50m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 2u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal ((54.00m / (107.00m * (1m / 2m))) - 1m)
    recordsLog.DeltaLo     |> should equal ((53.00m / (105.00m * (1m / 2m))) - 1m)
    recordsLog.Shares      |> should equal 0u
    recordsLog.ExitStop    |> should equal None

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, split 2:3`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 107.00m, 105.00m, 106.00m, None, None   , None
           date2, 162.00m, 159.00m, 160.50m, None, Some 2u, Some 3u |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateExitStop = (fun _ -> unexpectedCall ()) }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state2

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date2
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 2u
    recordsLog.Hi          |> should equal 162.00m
    recordsLog.Lo          |> should equal 159.00m
    recordsLog.Close       |> should equal 160.50m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 2u
    recordsLog.SplitOld    |> should equal 3u
    recordsLog.DeltaHi     |> should equal ((162.00m / (107.00m * (3m / 2m))) - 1m)
    recordsLog.DeltaLo     |> should equal ((159.00m / (105.00m * (3m / 2m))) - 1m)
    recordsLog.Shares      |> should equal 0u
    recordsLog.ExitStop    |> should equal None

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, both dividend 10% and split 2:1`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 107.00m, 105.00m, 106.00m, None       , None   , None
           date2,  48.60m,  47.70m,  48.15m, Some 10.60m, Some 2u, Some 1u |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateExitStop = (fun _ -> unexpectedCall ()) }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state2

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date2
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 2u
    recordsLog.Hi          |> should equal 48.60m
    recordsLog.Lo          |> should equal 47.70m
    recordsLog.Close       |> should equal 48.15m
    recordsLog.Dividend    |> should equal 10.60m
    recordsLog.SplitNew    |> should equal 2u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal ((48.60m / (107.00m * (1m - (10.60m / 106.00m)) * (1m / 2m))) - 1m)
    recordsLog.DeltaLo     |> should equal ((47.70m / (105.00m * (1m - (10.60m / 106.00m)) * (1m / 2m))) - 1m)
    recordsLog.Shares      |> should equal 0u
    recordsLog.ExitStop    |> should equal None

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, take position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None, None
           date2, 102.00m, 101.00m, 101.50m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state2

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 1
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 1

    let journalLog = journalLogsExecuteTake.[0]
    let detail = journalLog --> ExecuteTake
    journalLog.Date        |> should equal date2
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal +100
    journalLog.Cash        |> should equal -10200.00m
    detail.Shares          |> should equal 100u
    detail.Price           |> should equal 102.00m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date2
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 2u
    recordsLog.Hi          |> should equal 102.00m
    recordsLog.Lo          |> should equal 101.00m
    recordsLog.Close       |> should equal 101.50m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal (1.00m / 101.00m)
    recordsLog.DeltaLo     |> should equal (1.00m / 100.00m)
    recordsLog.Shares      |> should equal 100u
    recordsLog.ExitStop    |> should equal (Some 100.10m)

    summaryLog.Date        |> should equal date2
    summaryLog.Cash        |> should equal  989800.00m
    summaryLog.Equity      |> should equal  999950.00m
    summaryLog.ExitValue   |> should equal  999810.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0001900m
    summaryLog.Leverage    |> should equal (1m - (989800.00m / 999810.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker   |> should equal "X"
    nextExitOrder.Shares   |> should equal 100u
    nextExitOrder.Stop     |> should equal 100.20m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, take position and exit position on the same day`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None, None
           date2, 102.00m, 101.00m, 101.50m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 101.10m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state2

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 1
    journalLogsExecuteExit |> Array.length |> should equal 1
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 0

    let journalLog = journalLogsExecuteTake.[0]
    let detail = journalLog --> ExecuteTake
    journalLog.Date        |> should equal date2
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal +100
    journalLog.Cash        |> should equal -10200.00m
    detail.Shares          |> should equal 100u
    detail.Price           |> should equal 102.00m

    let journalLog = journalLogsExecuteExit.[0]
    let detail = journalLog --> ExecuteExit
    journalLog.Date        |> should equal date2
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal -100
    journalLog.Cash        |> should equal +10100.00m
    detail.Shares          |> should equal 100u
    detail.Price           |> should equal 101.00m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date2
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 2u
    recordsLog.Hi          |> should equal 102.00m
    recordsLog.Lo          |> should equal 101.00m
    recordsLog.Close       |> should equal 101.50m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal (1.00m / 101.00m)
    recordsLog.DeltaLo     |> should equal (1.00m / 100.00m)
    recordsLog.Shares      |> should equal 0u
    recordsLog.ExitStop    |> should equal (Some 101.10m)

    summaryLog.Date        |> should equal date2
    summaryLog.Cash        |> should equal  999900.00m
    summaryLog.Equity      |> should equal  999900.00m
    summaryLog.ExitValue   |> should equal  999900.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0001000m
    summaryLog.Leverage    |> should equal 0m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, take position and then exit position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None, None
           date2, 102.00m, 101.00m, 101.50m, None, None, None
           date3, 103.00m, 102.00m, 102.50m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 102.20m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 0
    journalLogsExecuteExit |> Array.length |> should equal 1
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 0

    let journalLog = journalLogsExecuteExit.[0]
    let detail = journalLog --> ExecuteExit
    journalLog.Date        |> should equal date3
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal -100
    journalLog.Cash        |> should equal +10200.00m
    detail.Shares          |> should equal 100u
    detail.Price           |> should equal 102.00m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date3
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 3u
    recordsLog.Hi          |> should equal 103.00m
    recordsLog.Lo          |> should equal 102.00m
    recordsLog.Close       |> should equal 102.50m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal (1.00m / 102.00m)
    recordsLog.DeltaLo     |> should equal (1.00m / 101.00m)
    recordsLog.Shares      |> should equal 0u
    recordsLog.ExitStop    |> should equal (Some 102.20m)

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal 1000000.00m
    summaryLog.Equity      |> should equal 1000000.00m
    summaryLog.ExitValue   |> should equal 1000000.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal 0m
    summaryLog.Leverage    |> should equal 0m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, take position and then hold position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None, None
           date2, 102.00m, 101.00m, 101.50m, None, None, None
           date3, 103.00m, 102.00m, 102.50m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | "X", date when date = date3 -> 100.30m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 0
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 1

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date3
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 3u
    recordsLog.Hi          |> should equal 103.00m
    recordsLog.Lo          |> should equal 102.00m
    recordsLog.Close       |> should equal 102.50m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal (1.00m / 102.00m)
    recordsLog.DeltaLo     |> should equal (1.00m / 101.00m)
    recordsLog.Shares      |> should equal 100u
    recordsLog.ExitStop    |> should equal (Some 100.20m)

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal  989800.00m
    summaryLog.Equity      |> should equal 1000050.00m
    summaryLog.ExitValue   |> should equal  999820.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0001800m
    summaryLog.Leverage    |> should equal (1m - (989800.00m / 999820.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker   |> should equal "X"
    nextExitOrder.Shares   |> should equal 100u
    nextExitOrder.Stop     |> should equal 100.30m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, take position and then stack onto existing position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None, None
           date2, 102.00m, 101.00m, 101.50m, None, None, None
           date3, 103.00m, 102.00m, 102.50m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | date when date = date2 -> [| { Ticker = "X"; Shares = 150u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | "X", date when date = date3 -> 100.30m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 1
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 1

    let journalLog = journalLogsExecuteTake.[0]
    let detail = journalLog --> ExecuteTake
    journalLog.Date        |> should equal date3
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal +150
    journalLog.Cash        |> should equal -15450.00m
    detail.Shares          |> should equal 150u
    detail.Price           |> should equal 103.00m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date3
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 3u
    recordsLog.Hi          |> should equal 103.00m
    recordsLog.Lo          |> should equal 102.00m
    recordsLog.Close       |> should equal 102.50m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal (1.00m / 102.00m)
    recordsLog.DeltaLo     |> should equal (1.00m / 101.00m)
    recordsLog.Shares      |> should equal 250u
    recordsLog.ExitStop    |> should equal (Some 100.20m)

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal  974350.00m
    summaryLog.Equity      |> should equal  999975.00m
    summaryLog.ExitValue   |> should equal  999400.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0006000m
    summaryLog.Leverage    |> should equal (1m - (974350.00m / 999400.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker   |> should equal "X"
    nextExitOrder.Shares   |> should equal 250u
    nextExitOrder.Stop     |> should equal 100.30m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, with discontinued quote, ignore order to take position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state2

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 0
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 0
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 0

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, with discontinued quote, liquidate existing position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None, None
           date2, 102.00m, 101.00m, 101.50m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 0
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 1
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 0
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 0

    let journalLog = journalLogsLiquidation.[0]
    let detail = journalLog --> Liquidation
    journalLog.Date        |> should equal date3
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal -100
    journalLog.Cash        |> should equal +10150.00m
    detail.Shares          |> should equal 100u
    detail.Price           |> should equal 101.50m

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal  999950.00m
    summaryLog.Equity      |> should equal  999950.00m
    summaryLog.ExitValue   |> should equal  999950.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0000500m
    summaryLog.Leverage    |> should equal 0m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, with dividend 10%, existing position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None       , None, None
           date2, 102.00m, 101.00m, 101.50m, None       , None, None
           date3,  92.70m,  91.80m,  92.25m, Some 10.15m, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | "X", date when date = date3 ->  90.27m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 0
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 1
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 1

    let journalLog = journalLogsPayDividend.[0]
    let detail = journalLog --> PayDividend
    journalLog.Date        |> should equal date3
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal 0
    journalLog.Cash        |> should equal +1015.00m
    detail.Shares          |> should equal 100u
    detail.Amount          |> should equal 10.15m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date3
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 3u
    recordsLog.Hi          |> should equal 92.70m
    recordsLog.Lo          |> should equal 91.80m
    recordsLog.Close       |> should equal 92.25m
    recordsLog.Dividend    |> should equal 10.15m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal ((92.70m / (102.00m * (1m - (10.15m / 101.50m)))) - 1m)
    recordsLog.DeltaLo     |> should equal ((91.80m / (101.00m * (1m - (10.15m / 101.50m)))) - 1m)
    recordsLog.Shares      |> should equal 100u
    recordsLog.ExitStop    |> should equal (Some 90.18m)

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal  990815.00m
    summaryLog.Equity      |> should equal 1000040.00m
    summaryLog.ExitValue   |> should equal  999833.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0001670m
    summaryLog.Leverage    |> should equal (1m - (990815.00m / 999833.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker   |> should equal "X"
    nextExitOrder.Shares   |> should equal 100u
    nextExitOrder.Stop     |> should equal 90.27m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, with dividend 10%, take position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None       , None, None
           date2, 102.00m, 101.00m, 101.50m, None       , None, None
           date3,  92.70m,  91.80m,  92.25m, Some 10.15m, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date2 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | "X", date when date = date3 ->  90.27m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 1
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 1

    let journalLog = journalLogsExecuteTake.[0]
    let detail = journalLog --> ExecuteTake
    journalLog.Date        |> should equal date3
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal +100
    journalLog.Cash        |> should equal -9270.00m
    detail.Shares          |> should equal 100u
    detail.Price           |> should equal 92.70m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date3
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 3u
    recordsLog.Hi          |> should equal 92.70m
    recordsLog.Lo          |> should equal 91.80m
    recordsLog.Close       |> should equal 92.25m
    recordsLog.Dividend    |> should equal 10.15m
    recordsLog.SplitNew    |> should equal 1u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal ((92.70m / (102.00m * (1m - (10.15m / 101.50m)))) - 1m)
    recordsLog.DeltaLo     |> should equal ((91.80m / (101.00m * (1m - (10.15m / 101.50m)))) - 1m)
    recordsLog.Shares      |> should equal 100u
    recordsLog.ExitStop    |> should equal (Some 90.18m)

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal  990730.00m
    summaryLog.Equity      |> should equal  999955.00m
    summaryLog.ExitValue   |> should equal  999748.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0002520m
    summaryLog.Leverage    |> should equal (1m - (990730.00m / 999748.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker   |> should equal "X"
    nextExitOrder.Shares   |> should equal 100u
    nextExitOrder.Stop     |> should equal 90.27m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, with split 2:1, existing position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None   , None
           date2, 102.00m, 101.00m, 101.50m, None, None   , None
           date3,  51.50m,  51.00m,  51.25m, None, Some 2u, Some 1u |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | "X", date when date = date3 ->  50.15m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 0
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 1
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 1

    let journalLog = journalLogsSplitShares.[0]
    let detail = journalLog --> SplitShares
    journalLog.Date        |> should equal date3
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal +100
    journalLog.Cash        |> should equal 0m
    detail.SharesNew       |> should equal 200u
    detail.SharesOld       |> should equal 100u
    detail.ExcessOld       |> should equal 0m
    detail.Price           |> should equal 101.50m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date3
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 3u
    recordsLog.Hi          |> should equal 51.50m
    recordsLog.Lo          |> should equal 51.00m
    recordsLog.Close       |> should equal 51.25m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 2u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal ((51.50m / (102.00m * (1m / 2m))) - 1m)
    recordsLog.DeltaLo     |> should equal ((51.00m / (101.00m * (1m / 2m))) - 1m)
    recordsLog.Shares      |> should equal 200u
    recordsLog.ExitStop    |> should equal (Some 50.10m)

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal  989800.00m
    summaryLog.Equity      |> should equal 1000050.00m
    summaryLog.ExitValue   |> should equal  999820.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0001800m
    summaryLog.Leverage    |> should equal (1m - (989800.00m / 999820.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker   |> should equal "X"
    nextExitOrder.Shares   |> should equal 200u
    nextExitOrder.Stop     |> should equal 50.15m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, with split 2:1, take position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None   , None
           date2, 102.00m, 101.00m, 101.50m, None, None   , None
           date3,  51.50m,  51.00m,  51.25m, None, Some 2u, Some 1u |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date2 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | "X", date when date = date3 ->  50.15m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 1
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 1

    let journalLog = journalLogsExecuteTake.[0]
    let detail = journalLog --> ExecuteTake
    journalLog.Date        |> should equal date3
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal +200
    journalLog.Cash        |> should equal -10300.00m
    detail.Shares          |> should equal 200u
    detail.Price           |> should equal 51.50m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date3
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 3u
    recordsLog.Hi          |> should equal 51.50m
    recordsLog.Lo          |> should equal 51.00m
    recordsLog.Close       |> should equal 51.25m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 2u
    recordsLog.SplitOld    |> should equal 1u
    recordsLog.DeltaHi     |> should equal ((51.50m / (102.00m * (1m / 2m))) - 1m)
    recordsLog.DeltaLo     |> should equal ((51.00m / (101.00m * (1m / 2m))) - 1m)
    recordsLog.Shares      |> should equal 200u
    recordsLog.ExitStop    |> should equal (Some 50.10m)

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal  989700.00m
    summaryLog.Equity      |> should equal  999950.00m
    summaryLog.ExitValue   |> should equal  999720.00m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0002800m
    summaryLog.Leverage    |> should equal (1m - (989700.00m / 999720.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker   |> should equal "X"
    nextExitOrder.Shares   |> should equal 200u
    nextExitOrder.Stop     |> should equal 50.15m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, with split 2:3, existing position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None   , None
           date2, 102.00m, 101.00m, 101.50m, None, None   , None
           date3, 154.50m, 153.00m, 153.75m, None, Some 2u, Some 3u |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date1 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | "X", date when date = date3 -> 150.45m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 0
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 1
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 1

    let journalLog = journalLogsSplitShares.[0]
    let detail = journalLog --> SplitShares
    journalLog.Date        |> should equal date3
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal -34
    journalLog.Cash        |> should equal +101.50m
    detail.SharesNew       |> should equal 66u
    detail.SharesOld       |> should equal 100u
    detail.ExcessOld       |> should equal 1m
    detail.Price           |> should equal 101.50m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date3
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 3u
    recordsLog.Hi          |> should equal 154.50m
    recordsLog.Lo          |> should equal 153.00m
    recordsLog.Close       |> should equal 153.75m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 2u
    recordsLog.SplitOld    |> should equal 3u
    recordsLog.DeltaHi     |> should equal ((154.50m / (102.00m * (3m / 2m))) - 1m)
    recordsLog.DeltaLo     |> should equal ((153.00m / (101.00m * (3m / 2m))) - 1m)
    recordsLog.Shares      |> should equal 66u
    recordsLog.ExitStop    |> should equal (Some 150.30m)

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal  989901.50m
    summaryLog.Equity      |> should equal 1000049.00m
    summaryLog.ExitValue   |> should equal  999821.30m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0001787m
    summaryLog.Leverage    |> should equal (1m - (989901.50m / 999821.30m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker   |> should equal "X"
    nextExitOrder.Shares   |> should equal 66u
    nextExitOrder.Stop     |> should equal 150.45m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process transactions, with split 2:3, take position`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)
    let date3 = DateTime(2000, 01, 03)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None   , None
           date2, 102.00m, 101.00m, 101.50m, None, None   , None
           date3, 154.50m, 153.00m, 153.75m, None, Some 2u, Some 3u |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let computeTakeOrders elementLog (summaryLog : SummaryLog) : TakeOrder[] =
        match summaryLog.Date with
        | date when date = date2 -> [| { Ticker = "X"; Shares = 100u } |]
        | _ -> Array.empty

    let calculateExitStop elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.10m
        | "X", date when date = date2 -> 100.20m
        | "X", date when date = date3 -> 150.45m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateExitStop = calculateExitStop }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 1000000.00m
          Equity    = 1000000.00m
          ExitValue = 1000000.00m
          Peak      = 1000000.00m
          Drawdown  = 0m
          Leverage  = 0m }

    let state0 = (Array.empty, Array.empty, summaryLog, Array.empty)
    let state1 = date1 |> runIncrement model state0
    let state2 = date2 |> runIncrement model state1
    let state3 = date3 |> runIncrement model state2
    let (journalLogs, elementLogs, summaryLog, nextOrders) = state3

    let journalLogsExecuteTake = journalLogs |> Array.filter (sieve ExecuteTake)
    let journalLogsExecuteExit = journalLogs |> Array.filter (sieve ExecuteExit)
    let journalLogsLiquidation = journalLogs |> Array.filter (sieve Liquidation)
    let journalLogsPayDividend = journalLogs |> Array.filter (sieve PayDividend)
    let journalLogsSplitShares = journalLogs |> Array.filter (sieve SplitShares)
    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    journalLogsExecuteTake |> Array.length |> should equal 1
    journalLogsExecuteExit |> Array.length |> should equal 0
    journalLogsLiquidation |> Array.length |> should equal 0
    journalLogsPayDividend |> Array.length |> should equal 0
    journalLogsSplitShares |> Array.length |> should equal 0
    elementLogs            |> Array.length |> should equal 1
    nextTakeOrders         |> Array.length |> should equal 0
    nextExitOrders         |> Array.length |> should equal 1

    let journalLog = journalLogsExecuteTake.[0]
    let detail = journalLog --> ExecuteTake
    journalLog.Date        |> should equal date3
    journalLog.Ticker      |> should equal "X"
    journalLog.Shares      |> should equal +66
    journalLog.Cash        |> should equal -10197.00m
    detail.Shares          |> should equal 66u
    detail.Price           |> should equal 154.50m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date        |> should equal date3
    recordsLog.Ticker      |> should equal "X"
    recordsLog.Count       |> should equal 3u
    recordsLog.Hi          |> should equal 154.50m
    recordsLog.Lo          |> should equal 153.00m
    recordsLog.Close       |> should equal 153.75m
    recordsLog.Dividend    |> should equal 0m
    recordsLog.SplitNew    |> should equal 2u
    recordsLog.SplitOld    |> should equal 3u
    recordsLog.DeltaHi     |> should equal ((154.50m / (102.00m * (3m / 2m))) - 1m)
    recordsLog.DeltaLo     |> should equal ((153.00m / (101.00m * (3m / 2m))) - 1m)
    recordsLog.Shares      |> should equal 66u
    recordsLog.ExitStop    |> should equal (Some 150.30m)

    summaryLog.Date        |> should equal date3
    summaryLog.Cash        |> should equal  989803.00m
    summaryLog.Equity      |> should equal  999950.50m
    summaryLog.ExitValue   |> should equal  999722.80m
    summaryLog.Peak        |> should equal 1000000.00m
    summaryLog.Drawdown    |> should equal -0.0002772m
    summaryLog.Leverage    |> should equal (1m - (989803.00m / 999722.80m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker   |> should equal "X"
    nextExitOrder.Shares   |> should equal 66u
    nextExitOrder.Stop     |> should equal 150.45m
