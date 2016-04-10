module TrendFollowing.Tests.Processing

open System
open NUnit.Framework
open FsUnit
open TrendFollowing.Types
open TrendFollowing.Processing

//-------------------------------------------------------------------------------------------------

let private chooseTakeOrder = function Take order -> Some order | _ -> None
let private chooseExitOrder = function Exit order -> Some order | _ -> None

let private unexpectedCall () = failwith "Unexpected call."

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
let ``Baseline increment 1`` () =

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
          CalculateStopLoss = (fun _ -> unexpectedCall ()) }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state1

    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    tradingLogs    |> Array.length |> should equal 0
    elementLogs    |> Array.length |> should equal 1
    nextTakeOrders |> Array.length |> should equal 0
    nextExitOrders |> Array.length |> should equal 0

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date      |> should equal date1
    recordsLog.Ticker    |> should equal "X"
    recordsLog.Count     |> should equal 1u
    recordsLog.Hi        |> should equal 101.00m
    recordsLog.Lo        |> should equal 100.00m
    recordsLog.Close     |> should equal 100.50m
    recordsLog.Dividend  |> should equal 0m
    recordsLog.SplitNew  |> should equal 1u
    recordsLog.SplitOld  |> should equal 1u
    recordsLog.DeltaHi   |> should equal 0m
    recordsLog.DeltaLo   |> should equal 0m
    recordsLog.Shares    |> should equal 0u
    recordsLog.StopLoss  |> should equal None

    summaryLog.Date      |> should equal date1
    summaryLog.Cash      |> should equal 1000000.00m
    summaryLog.Equity    |> should equal 1000000.00m
    summaryLog.ExitValue |> should equal 1000000.00m
    summaryLog.Peak      |> should equal 1000000.00m
    summaryLog.Drawdown  |> should equal 0m
    summaryLog.Leverage  |> should equal 0m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Baseline increment 2`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 101.00m, 100.00m, 100.50m, None, None, None
           date2, 102.00m, 101.00m, 101.50m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateStopLoss = (fun _ -> unexpectedCall ()) }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state2

    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    tradingLogs    |> Array.length |> should equal 0
    elementLogs    |> Array.length |> should equal 1
    nextTakeOrders |> Array.length |> should equal 0
    nextExitOrders |> Array.length |> should equal 0

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date      |> should equal date2
    recordsLog.Ticker    |> should equal "X"
    recordsLog.Count     |> should equal 2u
    recordsLog.Hi        |> should equal 102.00m
    recordsLog.Lo        |> should equal 101.00m
    recordsLog.Close     |> should equal 101.50m
    recordsLog.Dividend  |> should equal 0m
    recordsLog.SplitNew  |> should equal 1u
    recordsLog.SplitOld  |> should equal 1u
    recordsLog.DeltaHi   |> should equal (1.00m / 101.00m)
    recordsLog.DeltaLo   |> should equal (1.00m / 100.00m)
    recordsLog.Shares    |> should equal 0u
    recordsLog.StopLoss  |> should equal None

    summaryLog.Date      |> should equal date2
    summaryLog.Cash      |> should equal 1000000.00m
    summaryLog.Equity    |> should equal 1000000.00m
    summaryLog.ExitValue |> should equal 1000000.00m
    summaryLog.Peak      |> should equal 1000000.00m
    summaryLog.Drawdown  |> should equal 0m
    summaryLog.Leverage  |> should equal 0m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move`` () =

    let date1 = DateTime(2000, 01, 01)
    let date2 = DateTime(2000, 01, 02)

    let quotes =
        [| date1, 102.00m, 101.00m, 101.50m, None, None, None
           date2, 107.10m, 103.02m, 105.06m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateStopLoss = (fun _ -> unexpectedCall ()) }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state2

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date      |> should equal date2
    recordsLog.Ticker    |> should equal "X"
    recordsLog.Count     |> should equal 2u
    recordsLog.Hi        |> should equal 107.10m
    recordsLog.Lo        |> should equal 103.02m
    recordsLog.Close     |> should equal 105.06m
    recordsLog.Dividend  |> should equal 0m
    recordsLog.SplitNew  |> should equal 1u
    recordsLog.SplitOld  |> should equal 1u
    recordsLog.DeltaHi   |> should equal 0.05m
    recordsLog.DeltaLo   |> should equal 0.02m
    recordsLog.Shares    |> should equal 0u
    recordsLog.StopLoss  |> should equal None

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, dividend 50%`` () =

    Assert.Fail()

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, dividend 10%`` () =

    Assert.Fail()

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, split 2:1`` () =

    Assert.Fail()

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, split 3:1`` () =

    Assert.Fail()

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Compute delta, price move, both dividend 10% and split 2:1`` () =

    Assert.Fail()

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process orders, take position`` () =

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

    let calculateStopLoss elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.01m
        | "X", date when date = date2 -> 100.02m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateStopLoss = calculateStopLoss }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state2

    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    tradingLogs    |> Array.length |> should equal 1
    elementLogs    |> Array.length |> should equal 1
    nextTakeOrders |> Array.length |> should equal 0
    nextExitOrders |> Array.length |> should equal 1

    let tradingLog = tradingLogs.[0]
    tradingLog.Date      |> should equal date2
    tradingLog.Ticker    |> should equal "X"
    tradingLog.Shares    |> should equal +100
    tradingLog.Price     |> should equal 102.00m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date      |> should equal date2
    recordsLog.Ticker    |> should equal "X"
    recordsLog.Count     |> should equal 2u
    recordsLog.Hi        |> should equal 102.00m
    recordsLog.Lo        |> should equal 101.00m
    recordsLog.Close     |> should equal 101.50m
    recordsLog.Dividend  |> should equal 0m
    recordsLog.SplitNew  |> should equal 1u
    recordsLog.SplitOld  |> should equal 1u
    recordsLog.DeltaHi   |> should equal (1.00m / 101.00m)
    recordsLog.DeltaLo   |> should equal (1.00m / 100.00m)
    recordsLog.Shares    |> should equal 100u
    recordsLog.StopLoss  |> should equal (Some 100.01m)

    summaryLog.Date      |> should equal date2
    summaryLog.Cash      |> should equal  989800.00m
    summaryLog.Equity    |> should equal  999950.00m
    summaryLog.ExitValue |> should equal  999801.00m
    summaryLog.Peak      |> should equal 1000000.00m
    summaryLog.Drawdown  |> should equal -0.0001990m
    summaryLog.Leverage  |> should equal (1m - (989800.00m / 999801.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker |> should equal "X"
    nextExitOrder.Shares |> should equal 100u
    nextExitOrder.StopLoss |> should equal 100.02m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process orders, take position and exit position on the same day`` () =

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

    let calculateStopLoss elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 101.01m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateStopLoss = calculateStopLoss }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state2

    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    tradingLogs    |> Array.length |> should equal 2
    elementLogs    |> Array.length |> should equal 1
    nextTakeOrders |> Array.length |> should equal 0
    nextExitOrders |> Array.length |> should equal 0

    let tradingLog = tradingLogs.[0]
    tradingLog.Date      |> should equal date2
    tradingLog.Ticker    |> should equal "X"
    tradingLog.Shares    |> should equal +100
    tradingLog.Price     |> should equal 102.00m

    let tradingLog = tradingLogs.[1]
    tradingLog.Date      |> should equal date2
    tradingLog.Ticker    |> should equal "X"
    tradingLog.Shares    |> should equal -100
    tradingLog.Price     |> should equal 101.01m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date      |> should equal date2
    recordsLog.Ticker    |> should equal "X"
    recordsLog.Count     |> should equal 2u
    recordsLog.Hi        |> should equal 102.00m
    recordsLog.Lo        |> should equal 101.00m
    recordsLog.Close     |> should equal 101.50m
    recordsLog.Dividend  |> should equal 0m
    recordsLog.SplitNew  |> should equal 1u
    recordsLog.SplitOld  |> should equal 1u
    recordsLog.DeltaHi   |> should equal (1.00m / 101.00m)
    recordsLog.DeltaLo   |> should equal (1.00m / 100.00m)
    recordsLog.Shares    |> should equal 0u
    recordsLog.StopLoss  |> should equal (Some 101.01m)

    summaryLog.Date      |> should equal date2
    summaryLog.Cash      |> should equal  999901.00m
    summaryLog.Equity    |> should equal  999901.00m
    summaryLog.ExitValue |> should equal  999901.00m
    summaryLog.Peak      |> should equal 1000000.00m
    summaryLog.Drawdown  |> should equal -0.0000990m
    summaryLog.Leverage  |> should equal 0m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process orders, take position order ignored for discontinued instrument`` () =

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

    let calculateStopLoss elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.01m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateStopLoss = calculateStopLoss }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state2

    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    tradingLogs    |> Array.length |> should equal 0
    elementLogs    |> Array.length |> should equal 0
    nextTakeOrders |> Array.length |> should equal 0
    nextExitOrders |> Array.length |> should equal 0

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process orders, take position, exit position`` () =

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

    let calculateStopLoss elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.01m
        | "X", date when date = date2 -> 102.02m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateStopLoss = calculateStopLoss }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state3

    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    tradingLogs    |> Array.length |> should equal 1
    elementLogs    |> Array.length |> should equal 1
    nextTakeOrders |> Array.length |> should equal 0
    nextExitOrders |> Array.length |> should equal 0

    let tradingLog = tradingLogs.[0]
    tradingLog.Date      |> should equal date3
    tradingLog.Ticker    |> should equal "X"
    tradingLog.Shares    |> should equal -100
    tradingLog.Price     |> should equal 102.02m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date      |> should equal date3
    recordsLog.Ticker    |> should equal "X"
    recordsLog.Count     |> should equal 3u
    recordsLog.Hi        |> should equal 103.00m
    recordsLog.Lo        |> should equal 102.00m
    recordsLog.Close     |> should equal 102.50m
    recordsLog.Dividend  |> should equal 0m
    recordsLog.SplitNew  |> should equal 1u
    recordsLog.SplitOld  |> should equal 1u
    recordsLog.DeltaHi   |> should equal (1.00m / 102.00m)
    recordsLog.DeltaLo   |> should equal (1.00m / 101.00m)
    recordsLog.Shares    |> should equal 0u
    recordsLog.StopLoss  |> should equal (Some 102.02m)

    summaryLog.Date      |> should equal date3
    summaryLog.Cash      |> should equal 1000002.00m
    summaryLog.Equity    |> should equal 1000002.00m
    summaryLog.ExitValue |> should equal 1000002.00m
    summaryLog.Peak      |> should equal 1000002.00m
    summaryLog.Drawdown  |> should equal 0m
    summaryLog.Leverage  |> should equal 0m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process orders, take position, hold position`` () =

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

    let calculateStopLoss elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.01m
        | "X", date when date = date2 -> 100.02m
        | "X", date when date = date3 -> 100.03m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateStopLoss = calculateStopLoss }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state3

    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    tradingLogs    |> Array.length |> should equal 0
    elementLogs    |> Array.length |> should equal 1
    nextTakeOrders |> Array.length |> should equal 0
    nextExitOrders |> Array.length |> should equal 1

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date      |> should equal date3
    recordsLog.Ticker    |> should equal "X"
    recordsLog.Count     |> should equal 3u
    recordsLog.Hi        |> should equal 103.00m
    recordsLog.Lo        |> should equal 102.00m
    recordsLog.Close     |> should equal 102.50m
    recordsLog.Dividend  |> should equal 0m
    recordsLog.SplitNew  |> should equal 1u
    recordsLog.SplitOld  |> should equal 1u
    recordsLog.DeltaHi   |> should equal (1.00m / 102.00m)
    recordsLog.DeltaLo   |> should equal (1.00m / 101.00m)
    recordsLog.Shares    |> should equal 100u
    recordsLog.StopLoss  |> should equal (Some 100.02m)

    summaryLog.Date      |> should equal date3
    summaryLog.Cash      |> should equal  989800.00m
    summaryLog.Equity    |> should equal 1000050.00m
    summaryLog.ExitValue |> should equal  999802.00m
    summaryLog.Peak      |> should equal 1000000.00m
    summaryLog.Drawdown  |> should equal -0.0001980m
    summaryLog.Leverage  |> should equal (1m - (989800.00m / 999802.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker |> should equal "X"
    nextExitOrder.Shares |> should equal 100u
    nextExitOrder.StopLoss |> should equal 100.03m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process orders, take position, stack onto existing position`` () =

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

    let calculateStopLoss elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.01m
        | "X", date when date = date2 -> 100.02m
        | "X", date when date = date3 -> 100.03m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateStopLoss = calculateStopLoss }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state3

    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    tradingLogs    |> Array.length |> should equal 1
    elementLogs    |> Array.length |> should equal 1
    nextTakeOrders |> Array.length |> should equal 0
    nextExitOrders |> Array.length |> should equal 1

    let tradingLog = tradingLogs.[0]
    tradingLog.Date      |> should equal date3
    tradingLog.Ticker    |> should equal "X"
    tradingLog.Shares    |> should equal +150
    tradingLog.Price     |> should equal 103.00m

    let recordsLog = elementLogs.[0].RecordsLog
    recordsLog.Date      |> should equal date3
    recordsLog.Ticker    |> should equal "X"
    recordsLog.Count     |> should equal 3u
    recordsLog.Hi        |> should equal 103.00m
    recordsLog.Lo        |> should equal 102.00m
    recordsLog.Close     |> should equal 102.50m
    recordsLog.Dividend  |> should equal 0m
    recordsLog.SplitNew  |> should equal 1u
    recordsLog.SplitOld  |> should equal 1u
    recordsLog.DeltaHi   |> should equal (1.00m / 102.00m)
    recordsLog.DeltaLo   |> should equal (1.00m / 101.00m)
    recordsLog.Shares    |> should equal 250u
    recordsLog.StopLoss  |> should equal (Some 100.02m)

    summaryLog.Date      |> should equal date3
    summaryLog.Cash      |> should equal  974350.00m
    summaryLog.Equity    |> should equal  999975.00m
    summaryLog.ExitValue |> should equal  999355.00m
    summaryLog.Peak      |> should equal 1000000.00m
    summaryLog.Drawdown  |> should equal -0.0006450m
    summaryLog.Leverage  |> should equal (1m - (974350.00m / 999355.00m))

    let nextExitOrder = nextExitOrders.[0]
    nextExitOrder.Ticker |> should equal "X"
    nextExitOrder.Shares |> should equal 250u
    nextExitOrder.StopLoss |> should equal 100.03m

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Process orders, take position, terminate position for discontinued instrument`` () =

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

    let calculateStopLoss elementLog =
        match (elementLog.RecordsLog.Ticker, elementLog.RecordsLog.Date) with
        | "X", date when date = date1 -> 100.01m
        | "X", date when date = date2 -> 100.02m
        | _ -> unexpectedCall ()

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = computeTakeOrders
          CalculateStopLoss = calculateStopLoss }

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
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = state3

    let nextTakeOrders = nextOrders |> Array.choose chooseTakeOrder
    let nextExitOrders = nextOrders |> Array.choose chooseExitOrder

    tradingLogs    |> Array.length |> should equal 1
    elementLogs    |> Array.length |> should equal 0
    nextTakeOrders |> Array.length |> should equal 0
    nextExitOrders |> Array.length |> should equal 0

    let tradingLog = tradingLogs.[0]
    tradingLog.Date      |> should equal date3
    tradingLog.Ticker    |> should equal "X"
    tradingLog.Shares    |> should equal -100
    tradingLog.Price     |> should equal 101.50m

    summaryLog.Date      |> should equal date3
    summaryLog.Cash      |> should equal  999950.00m
    summaryLog.Equity    |> should equal  999950.00m
    summaryLog.ExitValue |> should equal  999950.00m
    summaryLog.Peak      |> should equal 1000000.00m
    summaryLog.Drawdown  |> should equal -0.0000500m
    summaryLog.Leverage  |> should equal 0m
