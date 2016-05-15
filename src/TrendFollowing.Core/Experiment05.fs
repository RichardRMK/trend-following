module TrendFollowing.Experiment05

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramSarAfInc = 0.0002m
let private paramSarAfMax = 0.0020m
let private paramEma = 200
let private paramRoc = 0.25m
let private paramLag = 200

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { SarEp          : decimal
      SarAf          : decimal
      Sar            : decimal
      TrendDirection : TrendDirection
      Ema            : decimal
      Rps            : decimal
      Roc            : decimal
      RocAdj         : decimal
      RocAdjLag      : decimal }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { SarEp          = recordsLog.Lo
      SarAf          = paramSarAfInc
      Sar            = recordsLog.Hi
      TrendDirection = Negative
      Ema            = recordsLog.Close
      Rps            = recordsLog.Hi - recordsLog.Lo
      Roc            = 0m
      RocAdj         = 0m
      RocAdjLag      = 0m }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

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

    let prevEma = computeAdjustedAmount prevElementLog.MetricsLog.Ema
    let ema = prevEma + ((recordsLog.Close - prevEma) / decimal paramEma)

    let rps = (sarEp - sar) |> abs
    let roc = (recordsLog.Close - prevEma) / (prevEma)
    let roc = ((1.0 + float roc) ** (250.0 / float paramEma)) - 1.0 |> decimal
    let rocAdj = roc * (sarEp / rps)

    let prevRocAdjLag = prevElementLog.MetricsLog.RocAdjLag
    let rocAdjLag = prevRocAdjLag + ((rocAdj - prevRocAdjLag) / decimal paramLag)

    { SarEp          = sarEp
      SarAf          = sarAf
      Sar            = sar
      TrendDirection = trendDirection
      Ema            = ema
      Rps            = rps
      Roc            = roc
      RocAdj         = rocAdj
      RocAdjLag      = rocAdjLag }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    let computeOrder elementLog : TakeOrder =

        let sarEp = elementLog.MetricsLog.SarEp
        let sar = elementLog.MetricsLog.Sar
        let shares = (summaryLog.ExitValue * paramRisk) / (sarEp - sar)

        { Ticker = elementLog.RecordsLog.Ticker
          Shares = uint32 shares }

    elementLogs
    |> Array.filter (fun x -> x.RecordsLog.Count >= paramWait)
    |> Array.filter (fun x -> x.RecordsLog.Shares = 0u)
    |> Array.filter (fun x -> x.MetricsLog.TrendDirection = Positive)
    |> Array.filter (fun x -> x.MetricsLog.Roc >= paramRoc)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Sar

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
