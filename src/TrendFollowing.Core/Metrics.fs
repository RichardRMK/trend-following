module TrendFollowing.Metrics

open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

let computeAdjustedAmount (recordsLog : RecordsLog) (prevElementLog : ElementLog<_>) =
    let splitNew = recordsLog.SplitNew
    let splitOld = recordsLog.SplitOld
    let dividend = recordsLog.Dividend
    let basis = prevElementLog.RecordsLog.Close
    match (splitNew, splitOld, dividend) with
    | (1u, 1u, 0m) -> id
    | _ -> Processing.computeAdjustedAmount splitNew splitOld dividend basis
