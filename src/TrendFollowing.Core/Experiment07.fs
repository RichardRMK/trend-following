module TrendFollowing.Experiment07

open System
open TrendFollowing.Types
open TrendFollowing.Metrics
open TrendFollowing.Output

//-------------------------------------------------------------------------------------------------

let private paramRisk = 0.1m
let private paramWait = 200u
let private paramRes = 200
let private paramRange = 20
let private paramRangeAvg = 20
let private paramRangeMult = 3.0m

//-------------------------------------------------------------------------------------------------

type MetricsLog =
    { ResLookback    : decimal[]
      UpperLookback  : decimal[]
      LowerLookback  : decimal[]
      Res            : decimal
      Upper          : decimal
      Lower          : decimal
      Range          : decimal
      RangeAvg       : decimal
      Stop           : decimal
      Signal         : bool
      TrendDirection : TrendDirection }

let private computeMetricsLogInit (recordsLog : RecordsLog) =

    { ResLookback    = Array.create paramRes recordsLog.Hi
      UpperLookback  = Array.create paramRange recordsLog.Hi
      LowerLookback  = Array.create paramRange recordsLog.Lo
      Res            = recordsLog.Hi
      Upper          = recordsLog.Hi
      Lower          = recordsLog.Lo
      Range          = recordsLog.Hi - recordsLog.Lo
      RangeAvg       = recordsLog.Hi - recordsLog.Lo
      Stop           = recordsLog.Lo - (paramRangeMult * (recordsLog.Hi - recordsLog.Lo))
      Signal         = false
      TrendDirection = Negative }

let private computeMetricsLogNext (recordsLog : RecordsLog) (prevElementLog : ElementLog<MetricsLog>) =

    let computeAdjustedAmount =
        Metrics.computeAdjustedAmount recordsLog prevElementLog

    let resLookback =
        prevElementLog.MetricsLog.ResLookback
        |> Array.map computeAdjustedAmount
        |> Array.append [| recordsLog.Hi |]
        |> Array.take paramRes

    let upperLookback =
        prevElementLog.MetricsLog.UpperLookback
        |> Array.map computeAdjustedAmount
        |> Array.append [| recordsLog.Hi |]
        |> Array.take paramRange

    let lowerLookback =
        prevElementLog.MetricsLog.LowerLookback
        |> Array.map computeAdjustedAmount
        |> Array.append [| recordsLog.Lo |]
        |> Array.take paramRange

    let res = resLookback |> Array.max
    let upper = upperLookback |> Array.max
    let lower = lowerLookback |> Array.min

    let range = upper - lower
    
    let prevAtr = computeAdjustedAmount prevElementLog.MetricsLog.RangeAvg
    let rangeAvg = prevAtr + ((range - prevAtr) / decimal paramRangeAvg)

    let stop = recordsLog.Lo - (paramRangeMult * rangeAvg)
    let stop =
        match recordsLog.ExitStop with
        | Some prevStop -> max prevStop stop
        | None -> stop

    let signal = recordsLog.Hi > computeAdjustedAmount prevElementLog.MetricsLog.Res

    let trendDirection =
        match recordsLog.ExitStop, signal with
        | Some exitStop, _ when recordsLog.Lo <= exitStop -> Negative
        | _, true -> Positive
        | _ -> prevElementLog.MetricsLog.TrendDirection

    { ResLookback    = resLookback
      UpperLookback  = upperLookback
      LowerLookback  = lowerLookback
      Res            = res
      Upper          = upper
      Lower          = lower
      Range          = range
      RangeAvg       = rangeAvg
      Stop           = stop
      Signal         = signal
      TrendDirection = trendDirection }

let computeMetricsLog (recordsLog : RecordsLog) = function
    | None      -> computeMetricsLogInit recordsLog
    | Some prev -> computeMetricsLogNext recordsLog prev

//-------------------------------------------------------------------------------------------------

let computeTakeOrders (elementLogs : ElementLog<MetricsLog>[]) (summaryLog : SummaryLog) : TakeOrder[] =

    let computeOrder elementLog : TakeOrder =

        let res = elementLog.MetricsLog.Res
        let stop = elementLog.MetricsLog.Stop
        let shares = (summaryLog.ExitValue * paramRisk) / (res - stop)

        { Ticker = elementLog.RecordsLog.Ticker
          Shares = uint32 shares }

    elementLogs
    |> Array.filter (fun x -> x.RecordsLog.Count >= paramWait)
    |> Array.filter (fun x -> x.RecordsLog.Shares = 0u)
    |> Array.filter (fun x -> x.MetricsLog.Signal)
    |> Array.filter (fun x -> x.MetricsLog.TrendDirection = Positive)
    |> Array.map computeOrder

let calculateExitStop (elementLog : ElementLog<MetricsLog>) : decimal =

    elementLog.MetricsLog.Stop

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
