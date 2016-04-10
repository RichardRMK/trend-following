module TrendFollowing.Processing

open System
open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

let inline private negate value = LanguagePrimitives.GenericZero - value

let private forTakePosition = int
let private forExitPosition = int >> negate

let chooseTakeOrder = function Take order -> Some order | _ -> None
let chooseExitOrder = function Exit order -> Some order | _ -> None

//-------------------------------------------------------------------------------------------------

let private computeShares prevShares (tradingLogs : TradingLog[]) =
    tradingLogs
    |> Seq.sumBy (fun x -> x.Shares)
    |> (+) (int prevShares)
    |> Checked.uint32

let private computeExitStop (exitOrder : ExitOrder option) =
    exitOrder |> Option.map (fun x -> x.Stop)

//-------------------------------------------------------------------------------------------------

let private computeRecordsLogInit (quote : Quote) exitOrder tradingLogs =

    let shares = tradingLogs |> computeShares 0u
    let exitStop = exitOrder |> computeExitStop

    { Date     = quote.Date
      Ticker   = quote.Ticker
      Count    = 1u
      Hi       = quote.Hi
      Lo       = quote.Lo
      Close    = quote.Close
      Dividend = 0m
      SplitNew = 1u
      SplitOld = 1u
      DeltaHi  = 0m
      DeltaLo  = 0m
      Shares   = shares
      ExitStop = exitStop }

let private computeRecordsLogNext (quote : Quote) exitOrder tradingLogs prevRecordsLog =

    let count = prevRecordsLog.Count + 1u
    let deltaHi = (quote.Hi / prevRecordsLog.Hi) - 1m
    let deltaLo = (quote.Lo / prevRecordsLog.Lo) - 1m

    let shares = tradingLogs |> computeShares prevRecordsLog.Shares
    let exitStop = exitOrder |> computeExitStop

    { Date     = quote.Date
      Ticker   = quote.Ticker
      Count    = count
      Hi       = quote.Hi
      Lo       = quote.Lo
      Close    = quote.Close
      Dividend = 0m
      SplitNew = 1u
      SplitOld = 1u
      DeltaHi  = deltaHi
      DeltaLo  = deltaLo
      Shares   = shares
      ExitStop = exitStop }

let computeRecordsLog (quote : Quote) exitOrder tradingLogs = function
    | None      -> computeRecordsLogInit quote exitOrder tradingLogs
    | Some prev -> computeRecordsLogNext quote exitOrder tradingLogs prev

//-------------------------------------------------------------------------------------------------

let computeElementLog model orders (tradingLogs : TradingLog[]) prevElementLogs (quote : Quote) =

    let prevElementLog =
        prevElementLogs
        |> Array.tryFind (fun x -> quote.Ticker = x.RecordsLog.Ticker)

    let exitOrder =
        orders
        |> Array.choose chooseExitOrder
        |> Array.tryFind (fun x -> quote.Ticker = x.Ticker)

    let tradingLogs =
        tradingLogs
        |> Array.filter (fun x -> quote.Ticker = x.Ticker)

    let recordsLog =
        prevElementLog
        |> Option.map (fun x -> x.RecordsLog)
        |> computeRecordsLog quote exitOrder tradingLogs

    let metricsLog =
        prevElementLog
        |> Option.map (fun x -> x.MetricsLog)
        |> model.ComputeMetricsLog recordsLog

    { RecordsLog = recordsLog
      MetricsLog = metricsLog }

//-------------------------------------------------------------------------------------------------

let computeSummaryLog date (tradingLogs : TradingLog[]) elementLogs prevSummaryLog =

    let cashValueAdjustment =
        tradingLogs
        |> Seq.map (fun x -> decimal x.Shares * x.Price)
        |> Seq.sum
        |> negate

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

let processTakeOrders (orders : Order[]) (quotes : Quote[]) =

    let executeOrder (order : TakeOrder) (quote : Quote) =
        { Date   = quote.Date
          Ticker = order.Ticker
          Shares = order.Shares |> forTakePosition
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
          Shares = order.Shares |> forExitPosition
          Price  = order.Stop }

    let processOrder (order : ExitOrder) =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = order.Ticker && quote.Lo <= order.Stop)
        |> Option.map (executeOrder order)

    orders
    |> Array.choose chooseExitOrder
    |> Array.map processOrder
    |> Array.choose id

let processTermTrades date prevElementLogs (quotes : Quote[]) =

    let isTerminated prevElementLog =
        quotes
        |> Array.exists (fun quote -> quote.Ticker = prevElementLog.RecordsLog.Ticker)
        |> not

    let openPosition prevElementLog =
        prevElementLog.RecordsLog.Shares <> 0u

    let executeTrade prevElementLog =
        { Date   = date
          Ticker = prevElementLog.RecordsLog.Ticker
          Shares = prevElementLog.RecordsLog.Shares |> forExitPosition
          Price  = prevElementLog.RecordsLog.Close }

    prevElementLogs
    |> Array.filter isTerminated
    |> Array.filter openPosition
    |> Array.map executeTrade

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

let runIncrement model (_, prevElementLogs, prevSummaryLog, orders) date =

    let quotes = model.GetQuotes date

    let tradingLogsTake = quotes |> processTakeOrders orders
    let tradingLogsExit = quotes |> processExitOrders orders
    let tradingLogsTerm = quotes |> processTermTrades date prevElementLogs
    let tradingLogs =
        Array.concat [ tradingLogsTake; tradingLogsExit; tradingLogsTerm ]

    let elementLogs = quotes |> Array.map (computeElementLog model orders tradingLogs prevElementLogs)
    let summaryLog = computeSummaryLog date tradingLogs elementLogs prevSummaryLog

    let takeOrders = computeTakeOrders model elementLogs summaryLog
    let exitOrders = computeExitOrders model elementLogs takeOrders
    let nextOrders =
        Array.concat [ Array.map Take takeOrders; Array.map Exit exitOrders ]

    (tradingLogs, elementLogs, summaryLog, nextOrders)

let runSimulation simulation =

    let reportResults (tradingLogs, elementLogs, summaryLog, _) =
        tradingLogs |> Array.iter simulation.ReportTradingLog
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

    let tradingLogs = Array.empty
    let elementLogs = Array.empty
    let nextOrders = Array.empty

    simulation.Dates
    |> Seq.scan (runIncrement simulation.Model) (tradingLogs, elementLogs, summaryLog, nextOrders)
    |> Seq.iter reportResults
