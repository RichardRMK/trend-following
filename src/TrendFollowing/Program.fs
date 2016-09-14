module Program

open System.Diagnostics
open TrendFollowing

//-------------------------------------------------------------------------------------------------

let stopwatch = Stopwatch.StartNew()

Processing.runSimulation System00.simulation

stopwatch.Stop()
printfn "%A" stopwatch.Elapsed
