module TrendFollowing.Processing

open System
open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

let inline private negate value = LanguagePrimitives.GenericZero - value

let chooseTakeOrder = function Take order -> Some order | _ -> None
let chooseExitOrder = function Exit order -> Some order | _ -> None

//-------------------------------------------------------------------------------------------------

let private computeOptional placeholder = function
    | None -> placeholder
    | Some value -> value

let private computeDelta dividend splitNew splitOld basis next prev =
    prev
    |> (*) (1m - (dividend / basis))
    |> (*) (decimal splitOld / decimal splitNew)
    |> (/) next
    |> (+) -1m

let private computeShares prevShares (journalLogs : JournalLog[]) =
    journalLogs
    |> Seq.sumBy (fun x -> x.Shares)
    |> (+) (int prevShares)
    |> Checked.uint32

let private computeExitStop (exitOrder : ExitOrder option) =
    exitOrder |> Option.map (fun x -> x.Stop)

//-------------------------------------------------------------------------------------------------

let private computeRecordsLogInit (quote : Quote) exitOrder journalLogs =

    let dividend = computeOptional 0m quote.Dividend
    let splitNew = computeOptional 1u quote.SplitNew
    let splitOld = computeOptional 1u quote.SplitOld

    let shares = journalLogs |> computeShares 0u
    let exitStop = exitOrder |> computeExitStop

    { Date     = quote.Date
      Ticker   = quote.Ticker
      Count    = 1u
      Hi       = quote.Hi
      Lo       = quote.Lo
      Close    = quote.Close
      Dividend = dividend
      SplitNew = splitNew
      SplitOld = splitOld
      DeltaHi  = 0m
      DeltaLo  = 0m
      Shares   = shares
      ExitStop = exitStop }

let private computeRecordsLogNext (quote : Quote) exitOrder journalLogs prevRecordsLog =

    let count = prevRecordsLog.Count + 1u

    let dividend = computeOptional 0m quote.Dividend
    let splitNew = computeOptional 1u quote.SplitNew
    let splitOld = computeOptional 1u quote.SplitOld

    let computeDelta = prevRecordsLog.Close |> computeDelta dividend splitNew splitOld
    let deltaHi = computeDelta quote.Hi prevRecordsLog.Hi
    let deltaLo = computeDelta quote.Lo prevRecordsLog.Lo

    let shares = journalLogs |> computeShares prevRecordsLog.Shares
    let exitStop = exitOrder |> computeExitStop

    { Date     = quote.Date
      Ticker   = quote.Ticker
      Count    = count
      Hi       = quote.Hi
      Lo       = quote.Lo
      Close    = quote.Close
      Dividend = dividend
      SplitNew = splitNew
      SplitOld = splitOld
      DeltaHi  = deltaHi
      DeltaLo  = deltaLo
      Shares   = shares
      ExitStop = exitStop }

let computeRecordsLog (quote : Quote) exitOrder journalLogs = function
    | None      -> computeRecordsLogInit quote exitOrder journalLogs
    | Some prev -> computeRecordsLogNext quote exitOrder journalLogs prev

//-------------------------------------------------------------------------------------------------

let computeElementLog model orders (journalLogs : JournalLog[]) prevElementLogs (quote : Quote) =

    let prevElementLog =
        prevElementLogs
        |> Array.tryFind (fun x -> quote.Ticker = x.RecordsLog.Ticker)

    let exitOrder =
        orders
        |> Array.choose chooseExitOrder
        |> Array.tryFind (fun x -> quote.Ticker = x.Ticker)

    let journalLogs =
        journalLogs
        |> Array.filter (fun x -> quote.Ticker = x.Ticker)

    let recordsLog =
        prevElementLog
        |> Option.map (fun x -> x.RecordsLog)
        |> computeRecordsLog quote exitOrder journalLogs

    let metricsLog =
        prevElementLog
        |> Option.map (fun x -> x.MetricsLog)
        |> model.ComputeMetricsLog recordsLog

    { RecordsLog = recordsLog
      MetricsLog = metricsLog }

//-------------------------------------------------------------------------------------------------

let computeSummaryLog date journalLogs elementLogs (prevSummaryLog : SummaryLog) =

    let cashValueAdjustment =
        journalLogs
        |> Seq.map (fun x -> x.Cash)
        |> Seq.sum

    let position elementLog =
        elementLog.RecordsLog.Shares <> 0u

    let positionValueEquity =
        elementLogs
        |> Seq.filter position
        |> Seq.map (fun x -> x.RecordsLog)
        |> Seq.map (fun x -> decimal x.Shares * x.Close)
        |> Seq.sum

    let positionValueAtExit =
        elementLogs
        |> Seq.filter position
        |> Seq.map (fun x -> x.RecordsLog)
        |> Seq.map (fun x -> decimal x.Shares * x.ExitStop.Value)
        |> Seq.sum

    let cash = prevSummaryLog.Cash + cashValueAdjustment
    let equity = cash + positionValueEquity
    let exitValue = cash + positionValueAtExit
    let peak = max prevSummaryLog.Peak exitValue
    let drawdown = (exitValue / peak) - 1m
    let leverage = 1m - (cash / exitValue)

    { Date      = date
      Cash      = cash
      Equity    = equity
      ExitValue = exitValue
      Peak      = peak
      Drawdown  = drawdown
      Leverage  = leverage }

//-------------------------------------------------------------------------------------------------

let computeTakeOrders model elementLogs summaryLog =
    model.ComputeTakeOrders elementLogs summaryLog

let computeExitOrders model elementLogs (takeOrders : TakeOrder[]) =

    let computeShares elementLog =
        takeOrders
        |> Array.tryFind (fun x -> x.Ticker = elementLog.RecordsLog.Ticker)
        |> function None -> 0u | Some takeOrder -> takeOrder.Shares
        |> (+) elementLog.RecordsLog.Shares

    let generateOrder (elementLog, shares) =
        { Ticker = elementLog.RecordsLog.Ticker
          Shares = shares
          Stop   = model.CalculateExitStop elementLog }

    elementLogs
    |> Array.map (fun elementLog -> elementLog, computeShares elementLog)
    |> Array.filter (fun (_, shares) -> shares <> 0u)
    |> Array.map generateOrder

//-------------------------------------------------------------------------------------------------

let processTransactionsExecuteTake (orders : Order[]) (quotes : Quote[]) =

    let generateJournalLog (order : TakeOrder) (quote : Quote) : JournalLog =

        let detail : JournalExecuteTake =
            { Shares = order.Shares
              Price  = quote.Hi }

        { Date   = quote.Date
          Ticker = order.Ticker
          Shares = detail.Shares |> (int)
          Cash   = detail.Shares |> (decimal >> negate) |> (*) detail.Price
          Detail = ExecuteTake detail }

    let executeTransaction (order : TakeOrder) =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = order.Ticker)
        |> Option.map (generateJournalLog order)

    orders
    |> Array.choose chooseTakeOrder
    |> Array.map executeTransaction
    |> Array.choose id

let processTransactionsExecuteExit (orders : Order[]) (quotes : Quote[]) =

    let generateJournalLog (order : ExitOrder) (quote : Quote) : JournalLog =

        let detail : JournalExecuteExit =
            { Shares = order.Shares
              Price  = order.Stop }

        { Date   = quote.Date
          Ticker = order.Ticker
          Shares = detail.Shares |> (int >> negate)
          Cash   = detail.Shares |> (decimal) |> (*) detail.Price
          Detail = ExecuteExit detail }

    let executeTransaction (order : ExitOrder) =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = order.Ticker && quote.Lo <= order.Stop)
        |> Option.map (generateJournalLog order)

    orders
    |> Array.choose chooseExitOrder
    |> Array.map executeTransaction
    |> Array.choose id

let processTransactionsLiquidation date prevElementLogs (quotes : Quote[]) =

    let wasDiscontinued prevElementLog =
        quotes
        |> Array.exists (fun quote -> quote.Ticker = prevElementLog.RecordsLog.Ticker)
        |> not

    let hadOpenPosition prevElementLog =
        prevElementLog.RecordsLog.Shares <> 0u

    let generateJournalLog prevElementLog : JournalLog =

        let detail : JournalLiquidation =
            { Shares = prevElementLog.RecordsLog.Shares
              Price  = prevElementLog.RecordsLog.Close }

        { Date   = date
          Ticker = prevElementLog.RecordsLog.Ticker
          Shares = detail.Shares |> (int >> negate)
          Cash   = detail.Shares |> (decimal) |> (*) detail.Price
          Detail = Liquidation detail }

    prevElementLogs
    |> Array.filter wasDiscontinued
    |> Array.filter hadOpenPosition
    |> Array.map generateJournalLog

//-------------------------------------------------------------------------------------------------

let runIncrement model (_, prevElementLogs, prevSummaryLog, orders) date =

    let quotes = model.GetQuotes date

    let journalLogsExecuteTake = quotes |> processTransactionsExecuteTake orders
    let journalLogsExecuteExit = quotes |> processTransactionsExecuteExit orders
    let journalLogsLiquidation = quotes |> processTransactionsLiquidation date prevElementLogs
    let journalLogs =
        Array.concat [ journalLogsExecuteTake; journalLogsExecuteExit; journalLogsLiquidation ]

    let elementLogs = quotes |> Array.map (computeElementLog model orders journalLogs prevElementLogs)
    let summaryLog = computeSummaryLog date journalLogs elementLogs prevSummaryLog

    let takeOrders = computeTakeOrders model elementLogs summaryLog
    let exitOrders = computeExitOrders model elementLogs takeOrders
    let nextOrders =
        Array.concat [ Array.map Take takeOrders; Array.map Exit exitOrders ]

    (journalLogs, elementLogs, summaryLog, nextOrders)

let runSimulation simulation =

    let reportResults (journalLogs, elementLogs, summaryLog, _) =
        journalLogs |> Array.iter simulation.ReportJournalLog
        elementLogs |> Array.iter simulation.ReportElementLog
        summaryLog |> simulation.ReportSummaryLog

    let summaryLog =
        { Date      = DateTime.MinValue
          Cash      = simulation.Principal
          Equity    = simulation.Principal
          ExitValue = simulation.Principal
          Peak      = simulation.Principal
          Drawdown  = 0m
          Leverage  = 0m }

    let journalLogs = Array.empty
    let elementLogs = Array.empty
    let nextOrders = Array.empty

    simulation.Dates
    |> Seq.scan (runIncrement simulation.Model) (journalLogs, elementLogs, summaryLog, nextOrders)
    |> Seq.iter reportResults
