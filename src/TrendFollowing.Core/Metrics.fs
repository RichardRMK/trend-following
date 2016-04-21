module TrendFollowing.Metrics

open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

let computeAdjustedAmount (recordsLog : RecordsLog) (prevElementLog : ElementLog<_>) =
    let splitNew = recordsLog.SplitNew
    let splitOld = recordsLog.SplitOld
    let dividend = recordsLog.Dividend
    let basis = prevElementLog.RecordsLog.Close
    Processing.computeAdjustedAmount splitNew splitOld dividend basis
