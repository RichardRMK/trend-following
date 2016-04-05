module TrendFollowing.Processing

open System
open System.Collections.Generic
open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

let private chooseTakeOrder = function Take order -> Some order | _ -> None
let private chooseExitOrder = function Exit order -> Some order | _ -> None
let private positive = (+) 0
let private negative = (-) 0

//-------------------------------------------------------------------------------------------------

let private computeRecordsLogInit (quote : Quote) (exitOrder : ExitOrder option) (tradingLogs : TradingLog[]) =

    let shares = tradingLogs |> Seq.sumBy (fun x -> x.Shares)
    let stopLoss = if exitOrder.IsNone then 0m else exitOrder.Value.StopLoss

    { Date = quote.Date
      Ticker = quote.Ticker
      Count = 1
      Hi = quote.Hi
      Lo = quote.Lo
      Close = quote.Close
      Dividend = 0m
      SplitNew = 1
      SplitOld = 1
      DeltaHi = 0m
      DeltaLo = 0m
      Shares = shares
      StopLoss = stopLoss }

let private computeRecordsLogNext (quote : Quote) (exitOrder : ExitOrder option) (tradingLogs : TradingLog[]) prevRecordsLog =

    let count = prevRecordsLog.Count + 1
    let deltaHi = (quote.Hi / prevRecordsLog.Hi) - 1m
    let deltaLo = (quote.Lo / prevRecordsLog.Lo) - 1m

    let sharesAdjustment = tradingLogs |> Seq.sumBy (fun x -> x.Shares)
    let sharesPreviously = prevRecordsLog.Shares
    let shares = sharesPreviously + sharesAdjustment
    let stopLoss = if exitOrder.IsNone then 0m else exitOrder.Value.StopLoss

    { Date = quote.Date
      Ticker = quote.Ticker
      Count = count
      Hi = quote.Hi
      Lo = quote.Lo
      Close = quote.Close
      Dividend = 0m
      SplitNew = 1
      SplitOld = 1
      DeltaHi = deltaHi
      DeltaLo = deltaLo
      Shares = shares
      StopLoss = stopLoss }

let computeRecordsLog (quote : Quote) (exitOrder : ExitOrder option) (tradingLogs : TradingLog[]) = function
    | None      -> computeRecordsLogInit quote exitOrder tradingLogs
    | Some prev -> computeRecordsLogNext quote exitOrder tradingLogs prev

//-------------------------------------------------------------------------------------------------

let computeElementLog system orders (tradingLogs : TradingLog[]) prevElementLogs (quote : Quote) =

    let tradingLogs = tradingLogs |> Array.filter (fun x -> quote.Ticker = x.Ticker)
    let exitOrders = orders |> Array.choose chooseExitOrder
    let exitOrder = exitOrders |> Array.tryFind (fun x -> quote.Ticker = x.Ticker)

    let prevElementLog = prevElementLogs |> Array.tryFind (fun x -> quote.Ticker = x.RecordsLog.Ticker)
    let prevRecordsLog = prevElementLog |> Option.map (fun x -> x.RecordsLog)
    let prevMetricsLog = prevElementLog |> Option.map (fun x -> x.MetricsLog)

    let recordsLog = prevRecordsLog |> computeRecordsLog quote exitOrder tradingLogs
    let metricsLog = prevMetricsLog |> system.ComputeMetricsLog recordsLog

    { RecordsLog = recordsLog
      MetricsLog = metricsLog }

//-------------------------------------------------------------------------------------------------

let computeSummaryLog date (tradingLogs : TradingLog[]) elementLogs prevSummaryLog =

    let cashAdjustmentValue =
        tradingLogs
        |> Seq.map (fun x -> decimal x.Shares * x.Price)
        |> Seq.sum

    let positionValueEquity =
        elementLogs
        |> Seq.map (fun x -> x.RecordsLog)
        |> Seq.map (fun x -> decimal x.Shares * x.Close)
        |> Seq.sum

    let positionValueAtExit =
        elementLogs
        |> Seq.map (fun x -> x.RecordsLog)
        |> Seq.map (fun x -> decimal x.Shares * x.StopLoss)
        |> Seq.sum

    let cash = prevSummaryLog.Cash - cashAdjustmentValue
    let equity = cash + positionValueEquity
    let exitValue = cash + positionValueAtExit
    let peak = max prevSummaryLog.Peak exitValue
    let drawdown = (exitValue / peak) - 1m
    let leverage = 1m - (cash / exitValue)

    { Date = date
      Cash = cash
      Equity = equity
      ExitValue = exitValue
      Peak = peak
      Drawdown = drawdown
      Leverage = leverage }

//-------------------------------------------------------------------------------------------------

let processTakeOrders (orders : Order[]) (quotes : Quote[]) =

    let executeOrder (order : TakeOrder) (quote : Quote) =
        { Date   = quote.Date
          Ticker = order.Ticker
          Shares = order.Shares |> positive
          Price  = quote.Hi }

    let processOrder (order : TakeOrder) =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = order.Ticker)
        |> Option.map (executeOrder order)

    orders
    |> Array.choose chooseTakeOrder
    |> Array.map processOrder
    |> Array.choose id

let processExitOrders (orders : Order[]) (quotes : Quote[]) =

    let executeOrder (order : ExitOrder) (quote : Quote) =
        { Date   = quote.Date
          Ticker = order.Ticker
          Shares = order.Shares |> negative
          Price  = order.StopLoss }

    let processOrder (order : ExitOrder) =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = order.Ticker && quote.Lo <= order.StopLoss)
        |> Option.map (executeOrder order)

    orders
    |> Array.choose chooseExitOrder
    |> Array.map processOrder
    |> Array.choose id

let processTermTrades date prevElementLogs (quotes : Quote[]) =

    let isTerminated prevElementLog =
        quotes
        |> Array.exists (fun x -> x.Ticker = prevElementLog.RecordsLog.Ticker)
        |> not

    let openPosition prevElementLog =
        prevElementLog.RecordsLog.Shares <> 0

    let executeTrade prevElementLog =
        { Date   = date
          Ticker = prevElementLog.RecordsLog.Ticker
          Shares = prevElementLog.RecordsLog.Shares |> negative
          Price  = prevElementLog.RecordsLog.Close }

    prevElementLogs
    |> Array.filter isTerminated
    |> Array.filter openPosition
    |> Array.map executeTrade

//-------------------------------------------------------------------------------------------------

let computeTakeOrders system elementLogs summaryLog =
    system.ComputeTakeOrders elementLogs summaryLog

let computeExitOrders system elementLogs (takeOrders : TakeOrder[]) =

    let computeShares (elementLog : ElementLog<'T>) =
        takeOrders
        |> Array.tryFind (fun x -> x.Ticker = elementLog.RecordsLog.Ticker)
        |> function None -> 0 | Some takeOrder -> takeOrder.Shares
        |> (+) elementLog.RecordsLog.Shares

    let generateOrder (elementLog, shares) =
        { Ticker = elementLog.RecordsLog.Ticker
          Shares = shares
          StopLoss = system.CalculateStopLoss elementLog }

    elementLogs
    |> Array.map (fun elementLog -> elementLog, computeShares elementLog)
    |> Array.filter (fun (_, shares) -> shares <> 0)
    |> Array.map generateOrder

//-------------------------------------------------------------------------------------------------

let private emitResult system (tradingLogs, elementLogs, summaryLog, _) =

    tradingLogs |> Array.iter system.EmitTradingLog
    elementLogs |> Array.iter system.EmitElementLog
    summaryLog |> system.EmitSummaryLog

let runSingleDate system (_, prevElementLogs, prevSummaryLog, orders) date =

    let quotes = system.GetQuotes date

    let tradingLogsTake = quotes |> processTakeOrders orders
    let tradingLogsExit = quotes |> processExitOrders orders
    let tradingLogsTerm = quotes |> processTermTrades date prevElementLogs
    let tradingLogs =
        Array.concat [ tradingLogsTake; tradingLogsExit; tradingLogsTerm ]

    let elementLogs = quotes |> Array.map (computeElementLog system orders tradingLogs prevElementLogs)
    let summaryLog = computeSummaryLog date tradingLogs elementLogs prevSummaryLog

    let takeOrders = computeTakeOrders system elementLogs summaryLog
    let exitOrders = computeExitOrders system elementLogs takeOrders
    let nextOrders =
        Array.concat [ Array.map Take takeOrders; Array.map Exit exitOrders ]

    (tradingLogs, elementLogs, summaryLog, nextOrders)

let runSimulation system =

    let initializeSummaryLog principal =
        { Date      = DateTime.MinValue
          Cash      = principal
          Equity    = principal
          ExitValue = principal
          Peak      = principal
          Drawdown  = 0m
          Leverage  = 0m }

    let tradingLogs = Array.empty
    let elementLogs = Array.empty
    let summaryLog = initializeSummaryLog system.Principal
    let nextOrders = Array.empty

    system.DateSequence
    |> Seq.scan (system |> runSingleDate) (tradingLogs, elementLogs, summaryLog, nextOrders)
    |> Seq.iter (system |> emitResult)
