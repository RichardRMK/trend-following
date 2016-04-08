module TrendFollowing.Tests.Processing

open System
open NUnit.Framework
open FsUnit
open TrendFollowing.Types
open TrendFollowing.Processing

//-------------------------------------------------------------------------------------------------

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
let ``Run increment`` () =

    let date = DateTime(2000, 01, 01)

    let quotes =
        [| date, 100.00m, 100.00m, 100.00m, None, None, None |]
        |> Array.map (toQuote "X")

    let getQuotes date =
        quotes
        |> Array.filter (fun x -> x.Date = date)

    let model =
        { GetQuotes         = getQuotes
          ComputeMetricsLog = (fun _ _ -> ())
          ComputeTakeOrders = (fun _ _ -> Array.empty)
          CalculateStopLoss = (fun _ -> 0m) }

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = 100000m
          Equity    = 100000m
          ExitValue = 100000m
          Peak      = 100000m
          Drawdown  = 0m
          Leverage  = 0m }

    let prevState = (Array.empty, Array.empty, summaryLog, Array.empty)
    let nextState = date |> runIncrement model prevState
    let (tradingLogs, elementLogs, summaryLog, nextOrders) = nextState

    tradingLogs |> Array.length |> should equal 0
    elementLogs |> Array.length |> should equal 1

    summaryLog.Date      |> should equal date
    summaryLog.Cash      |> should equal 100000m
    summaryLog.Equity    |> should equal 100000m
    summaryLog.ExitValue |> should equal 100000m
    summaryLog.Peak      |> should equal 100000m
    summaryLog.Drawdown  |> should equal 0m
    summaryLog.Leverage  |> should equal 0m

    nextOrders |> Array.length |> should equal 0
