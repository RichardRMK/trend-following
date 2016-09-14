module Program

open System.Diagnostics
open TrendFollowing

//-------------------------------------------------------------------------------------------------

let stopwatch = Stopwatch.StartNew()

Processing.runSimulation Experiment01.simulation

stopwatch.Stop()
printfn "%A" stopwatch.Elapsed
