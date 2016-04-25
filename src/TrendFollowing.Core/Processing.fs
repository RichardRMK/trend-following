module TrendFollowing.Processing

open System
open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

let inline private negate value = LanguagePrimitives.GenericZero - value

let chooseTakeOrder = function Take order -> Some order | _ -> None
let chooseExitOrder = function Exit order -> Some order | _ -> None

//-------------------------------------------------------------------------------------------------

let computeOptional normal =
    normal
    |> Option.fold (fun _ -> id)

let computeAdjustedAmount splitNew splitOld dividend basis amount =
    amount
    |> (*) (1m - (dividend / basis))
    |> (*) (decimal splitOld / decimal splitNew)

let computeAdjustedShares splitNew splitOld shares =
    shares
    |> decimal
    |> (*) (decimal splitNew / decimal splitOld)
    |> uint32

let computeAdjustedExcess splitNew splitOld shares =
    shares
    |> computeAdjustedShares splitNew splitOld
    |> decimal
    |> (*) (decimal splitOld / decimal splitNew)
    |> (-) (decimal shares)

//-------------------------------------------------------------------------------------------------

let private computeDelta splitNew splitOld dividend basis next prev =
    prev
    |> computeAdjustedAmount splitNew splitOld dividend basis
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

    let computeDelta = prevRecordsLog.Close |> computeDelta splitNew splitOld dividend
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

let adjustOrder prevElementLog order (quote : Quote) =

    let dividend = computeOptional 0m quote.Dividend
    let splitNew = computeOptional 1u quote.SplitNew
    let splitOld = computeOptional 1u quote.SplitOld
    let basis = prevElementLog.RecordsLog.Close

    let adjustTakeOrder (order : TakeOrder) =
        let shares = order.Shares |> computeAdjustedShares splitNew splitOld
        { order with Shares = shares }

    let adjustExitOrder (order : ExitOrder) =
        let shares = order.Shares |> computeAdjustedShares splitNew splitOld
        let stop   = order.Stop   |> computeAdjustedAmount splitNew splitOld dividend basis
        { order with Shares = shares; Stop = stop }

    match order with
    | Take order -> order |> adjustTakeOrder |> Take
    | Exit order -> order |> adjustExitOrder |> Exit

let adjustOrders prevElementLogs orders (quotes : Quote[]) =

    let getTicker = function
        | Take order -> order.Ticker
        | Exit order -> order.Ticker

    let chooseOrder order =
        let ticker = getTicker order
        let prevElementLog = prevElementLogs |> Array.find (fun x -> x.RecordsLog.Ticker = ticker)
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = ticker)
        |> Option.map (adjustOrder prevElementLog order)

    orders
    |> Array.choose chooseOrder

//-------------------------------------------------------------------------------------------------

let processTransactionsExecuteTake orders (quotes : Quote[]) =

    let generateJournalLog (order : TakeOrder) (quote : Quote) =

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
    |> Array.choose executeTransaction

let processTransactionsExecuteExit orders (quotes : Quote[]) =

    let generateJournalLog (order : ExitOrder) (quote : Quote) =

        let detail : JournalExecuteExit =
            { Shares = order.Shares
              Price  = quote.Lo }

        { Date   = quote.Date
          Ticker = order.Ticker
          Shares = detail.Shares |> (int >> negate)
          Cash   = detail.Shares |> (decimal) |> (*) detail.Price
          Detail = ExecuteExit detail }

    let executeTransaction (order : ExitOrder) =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = order.Ticker)
        |> Option.filter (fun quote -> quote.Lo <= order.Stop)
        |> Option.map (generateJournalLog order)

    orders
    |> Array.choose chooseExitOrder
    |> Array.choose executeTransaction

let processTransactionsLiquidation prevElementLogs date (quotes : Quote[]) =

    let generateJournalLog prevElementLog =

        let detail : JournalLiquidation =
            { Shares = prevElementLog.RecordsLog.Shares
              Price  = prevElementLog.RecordsLog.Close }

        { Date   = date
          Ticker = prevElementLog.RecordsLog.Ticker
          Shares = detail.Shares |> (int >> negate)
          Cash   = detail.Shares |> (decimal) |> (*) detail.Price
          Detail = Liquidation detail }

    let wasDiscontinued prevElementLog =
        quotes
        |> Array.exists (fun quote -> quote.Ticker = prevElementLog.RecordsLog.Ticker)
        |> not

    let hadOpenPosition prevElementLog =
        prevElementLog.RecordsLog.Shares <> 0u

    prevElementLogs
    |> Array.filter wasDiscontinued
    |> Array.filter hadOpenPosition
    |> Array.map generateJournalLog

let processTransactionsPayDividend prevElementLogs (quotes : Quote[]) =

    let generateJournalLog prevElementLog (quote : Quote) =

        let dividend = computeOptional 0m quote.Dividend

        let detail : JournalPayDividend =
            { Shares = prevElementLog.RecordsLog.Shares
              Amount = dividend }

        { Date   = quote.Date
          Ticker = prevElementLog.RecordsLog.Ticker
          Shares = 0
          Cash   = detail.Shares |> (decimal) |> (*) detail.Amount
          Detail = PayDividend detail }

    let executeTransaction prevElementLog =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = prevElementLog.RecordsLog.Ticker)
        |> Option.filter (fun quote -> prevElementLog.RecordsLog.Shares <> 0u)
        |> Option.filter (fun quote -> quote.Dividend.IsSome)
        |> Option.map (generateJournalLog prevElementLog)

    prevElementLogs
    |> Array.choose executeTransaction

let processTransactionsSplitShares prevElementLogs (quotes : Quote[]) =

    let generateJournalLog prevElementLog (quote : Quote) =

        let splitNew = computeOptional 1u quote.SplitNew
        let splitOld = computeOptional 1u quote.SplitOld

        let sharesOld = prevElementLog.RecordsLog.Shares
        let sharesNew = sharesOld |> computeAdjustedShares splitNew splitOld
        let excessOld = sharesOld |> computeAdjustedExcess splitNew splitOld

        let detail : JournalSplitShares =
            { SharesNew  = sharesNew
              SharesOld  = sharesOld
              ExcessOld  = excessOld
              Price      = prevElementLog.RecordsLog.Close }

        { Date   = quote.Date
          Ticker = prevElementLog.RecordsLog.Ticker
          Shares = int detail.SharesNew - int detail.SharesOld
          Cash   = detail.ExcessOld * detail.Price
          Detail = SplitShares detail }

    let executeTransaction prevElementLog =
        quotes
        |> Array.tryFind (fun quote -> quote.Ticker = prevElementLog.RecordsLog.Ticker)
        |> Option.filter (fun quote -> prevElementLog.RecordsLog.Shares <> 0u)
        |> Option.filter (fun quote -> quote.SplitNew.IsSome || quote.SplitOld.IsSome)
        |> Option.map (generateJournalLog prevElementLog)

    prevElementLogs
    |> Array.choose executeTransaction

//-------------------------------------------------------------------------------------------------

let runIncrement getQuotes model (_, prevElementLogs, prevSummaryLog, orders) date =

    let quotes = getQuotes date
    let orders = adjustOrders prevElementLogs orders quotes

    let journalLogsExecuteTake = quotes |> processTransactionsExecuteTake orders
    let journalLogsExecuteExit = quotes |> processTransactionsExecuteExit orders
    let journalLogsLiquidation = quotes |> processTransactionsLiquidation prevElementLogs date
    let journalLogsPayDividend = quotes |> processTransactionsPayDividend prevElementLogs
    let journalLogsSplitShares = quotes |> processTransactionsSplitShares prevElementLogs

    let journalLogs =
        [ journalLogsExecuteTake
          journalLogsExecuteExit
          journalLogsLiquidation
          journalLogsPayDividend
          journalLogsSplitShares ]

    let journalLogs = journalLogs |> Array.concat
    let elementLogs = quotes |> Array.map (computeElementLog model orders journalLogs prevElementLogs)
    let summaryLog = computeSummaryLog date journalLogs elementLogs prevSummaryLog

    let takeOrders = computeTakeOrders model elementLogs summaryLog
    let exitOrders = computeExitOrders model elementLogs takeOrders
    let nextOrders = [ Array.map Take takeOrders; Array.map Exit exitOrders ]
    let nextOrders = nextOrders |> Array.concat

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
    |> Seq.scan (runIncrement simulation.GetQuotes simulation.Model) (journalLogs, elementLogs, summaryLog, nextOrders)
    |> Seq.iter reportResults
