module TrendFollowing.Experiment12

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 1.0m
let private paramWait = 200u
let private paramRes = 200
let private paramSup = 200
let private paramSarAfInc = 0.002m
let private paramSarAfMax = 0.002m

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { ResLookback    : decimal[]
      SupLookback    : decimal[]
      Res            : decimal
      Sup            : decimal
      SarEp          : decimal
      SarAf          : decimal
      Sar            : decimal
      TrendDirection : TrendDirection
      Signal         : bool }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { ResLookback    = Array.create paramRes recordsLog.Hi
      SupLookback    = Array.create paramSup recordsLog.Lo
      Res            = recordsLog.Hi
      Sup            = recordsLog.Lo
      SarEp          = recordsLog.Lo
      SarAf          = paramSarAfInc
      Sar            = recordsLog.Hi
      TrendDirection = Negative
      Signal         = false }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let resLookback =
        prevElementLog.MetricsLog.ResLookback
        |> Array.map computeAdjustedAmount
        |> Array.append [| recordsLog.Hi |]
        |> Array.take paramRes

    let supLookback =
        prevElementLog.MetricsLog.SupLookback
        |> Array.map computeAdjustedAmount
        |> Array.append [| recordsLog.Lo |]
        |> Array.take paramSup

    let res = resLookback |> Array.max
    let sup = supLookback |> Array.min

    let prevMetricsLog = prevElementLog.MetricsLog
    let prevSarEp = computeAdjustedAmount prevMetricsLog.SarEp
    let prevSarAf = prevMetricsLog.SarAf
    let prevSar = computeAdjustedAmount prevMetricsLog.Sar

    let (sarEp, sarAf, sar, trendDirection) =
        match prevMetricsLog.TrendDirection with
        | Positive when recordsLog.Lo <= prevSar
            ->
            let sarEp = recordsLog.Lo
            let sarAf = paramSarAfInc
            let sar = prevSarEp
            let trendDirection = Negative
            (sarEp, sarAf, sar, trendDirection)
        | Negative when recordsLog.Hi >= prevSar
            ->
            let sarEp = recordsLog.Hi
            let sarAf = paramSarAfInc
            let sar = prevSarEp
            let trendDirection = Positive
            (sarEp, sarAf, sar, trendDirection)
        | Positive
            ->
            let incAf = if recordsLog.Hi > prevSarEp then paramSarAfInc else 0m
            let sarEp = max recordsLog.Hi prevSarEp
            let sarAf = min paramSarAfMax (prevSarAf + incAf)
            let sar = prevSar + (prevSarAf * (prevSarEp - prevSar))
            let trendDirection = Positive
            (sarEp, sarAf, sar, trendDirection)
        | Negative
            ->
            let incAf = if recordsLog.Lo < prevSarEp then paramSarAfInc else 0m
            let sarEp = min recordsLog.Lo prevSarEp
            let sarAf = min paramSarAfMax (prevSarAf + incAf)
            let sar = prevSar + (prevSarAf * (prevSarEp - prevSar))
            let trendDirection = Negative
            (sarEp, sarAf, sar, trendDirection)

    let signal = recordsLog.Hi > computeAdjustedAmount prevElementLog.MetricsLog.Res

    { ResLookback    = resLookback
      SupLookback    = supLookback
      Res            = res
      Sup            = sup
      SarEp          = sarEp
      SarAf          = sarAf
      Sar            = sar
      TrendDirection = trendDirection
      Signal         = signal }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    let computeOrder elementLog : TakeOrder =

        let sarEp = elementLog.MetricsLog.SarEp
        let cash = summaryLog.Cash
        let shares = (cash * paramRisk) / sarEp

        { Ticker = elementLog.RecordsLog.Ticker
          Shares = uint32 shares }

    elementLogs
    |> Array.filter (fun x -> x.RecordsLog.Count >= paramWait)
    |> Array.filter (fun x -> x.RecordsLog.Shares = 0u)
    |> Array.filter (fun x -> x.MetricsLog.TrendDirection = Positive)
    |> Array.filter (fun x -> x.MetricsLog.Signal)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    let alternateStop =
        match elementLog.RecordsLog.ExitStop with
        | Some stop -> stop
        | None -> elementLog.MetricsLog.Sup

    max alternateStop elementLog.MetricsLog.Sar

//-------------------------------------------------------------------------------------------------

let reportAgent = ReportAgent()

let dateStart = DateTime(1990, 01, 01)
let dateFinal = DateTime.Today
let getQuotes = Data.getQuotes dateStart dateFinal
let dates = Data.getDates dateStart dateFinal

let model =
    { ComputeMetricsLog = computeMetricsLog
      ComputeTakeOrders = computeTakeOrders
      CalculateExitStop = calculateExitStop }

let simulation =
    { Principal         = 1000000.00m
      GetQuotes         = getQuotes
      Dates             = dates
      Model             = model
      ReportElementLog  = reportAgent.ReportElementLog
      ReportSummaryLog  = reportAgent.ReportSummaryLog
      ReportJournalLog  = reportAgent.ReportJournalLog
      ReportCompletion  = reportAgent.ReportCompletion }
