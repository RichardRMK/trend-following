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

let private computeShares prevShares (tradingLogs : TradingLog[]) =
    tradingLogs
    |> Seq.sumBy (fun x -> x.Shares)
    |> (+) (int prevShares)
    |> Checked.uint32

let private computeExitStop (exitOrder : ExitOrder option) =
    exitOrder |> Option.map (fun x -> x.Stop)

//-------------------------------------------------------------------------------------------------

let private computeRecordsLogInit (quote : Quote) exitOrder tradingLogs =

    let dividend = computeOptional 0m quote.Dividend
    let splitNew = computeOptional 1u quote.SplitNew
    let splitOld = computeOptional 1u quote.SplitOld

    let shares = tradingLogs |> computeShares 0u
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

let private computeRecordsLogNext (quote : Quote) exitOrder tradingLogs prevRecordsLog =

    let count = prevRecordsLog.Count + 1u

    let dividend = computeOptional 0m quote.Dividend
    let splitNew = computeOptional 1u quote.SplitNew
    let splitOld = computeOptional 1u quote.SplitOld

    let computeDelta = prevRecordsLog.Close |> computeDelta dividend splitNew splitOld
    let deltaHi = computeDelta quote.Hi prevRecordsLog.Hi
    let deltaLo = computeDelta quote.Lo prevRecordsLog.Lo

    let shares = tradingLogs |> computeShares prevRecordsLog.Shares
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
        |> Seq.map (fun x -> x.Amount)
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

let processTransactionsTakePosition (orders : Order[]) (quotes : Quote[]) =

    let executeTransaction (order : TakeOrder) (quote : Quote) : TradingLog =

        let detail : JournalTakePosition =
            { Shares = order.Shares
              Price  = quote.Hi }

        { Date   = quote.Date
          Ticker = order.Ticker
          Shares = detail.Shares |> (int)
          Amount = detail.Shares |> (decimal >> negate) |> (*) detail.Price
          Detail = TakePosition detail }

    let processTransaction (order : TakeOrder) =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = order.Ticker)
        |> Option.map (executeTransaction order)

    orders
    |> Array.choose chooseTakeOrder
    |> Array.map processTransaction
    |> Array.choose id

let processTransactionsExitPosition (orders : Order[]) (quotes : Quote[]) =

    let executeTransaction (order : ExitOrder) (quote : Quote) : TradingLog =

        let detail : JournalExitPosition =
            { Shares = order.Shares
              Price  = order.Stop }

        { Date   = quote.Date
          Ticker = order.Ticker
          Shares = detail.Shares |> (int >> negate)
          Amount = detail.Shares |> (decimal) |> (*) detail.Price
          Detail = ExitPosition detail }

    let processTransaction (order : ExitOrder) =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = order.Ticker && quote.Lo <= order.Stop)
        |> Option.map (executeTransaction order)

    orders
    |> Array.choose chooseExitOrder
    |> Array.map processTransaction
    |> Array.choose id

let processTransactionsTermPosition date prevElementLogs (quotes : Quote[]) =

    let hasBeenTerminated prevElementLog =
        quotes
        |> Array.exists (fun quote -> quote.Ticker = prevElementLog.RecordsLog.Ticker)
        |> not

    let hasAnOpenPosition prevElementLog =
        prevElementLog.RecordsLog.Shares <> 0u

    let executeTransaction prevElementLog : TradingLog =

        let detail : JournalTermPosition =
            { Shares = prevElementLog.RecordsLog.Shares
              Price  = prevElementLog.RecordsLog.Close }

        { Date   = date
          Ticker = prevElementLog.RecordsLog.Ticker
          Shares = detail.Shares |> (int >> negate)
          Amount = detail.Shares |> (decimal) |> (*) detail.Price
          Detail = TermPosition detail }

    prevElementLogs
    |> Array.filter hasBeenTerminated
    |> Array.filter hasAnOpenPosition
    |> Array.map executeTransaction

//-------------------------------------------------------------------------------------------------

let runIncrement model (_, prevElementLogs, prevSummaryLog, orders) date =

    let quotes = model.GetQuotes date

    let tradingLogsTakePosition = quotes |> processTransactionsTakePosition orders
    let tradingLogsExitPosition = quotes |> processTransactionsExitPosition orders
    let tradingLogsTermPosition = quotes |> processTransactionsTermPosition date prevElementLogs
    let tradingLogs =
        Array.concat [ tradingLogsTakePosition; tradingLogsExitPosition; tradingLogsTermPosition ]

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
