module TrendFollowing.Data

open System
open FSharp.Data
open TrendFollowing.Types

//-------------------------------------------------------------------------------------------------

[<Literal>]
let private connectionName = @"name=database"

[<Literal>]
let private configFile = @"..\..\private\App.config"

//-------------------------------------------------------------------------------------------------

[<Literal>]
let private sqlGetQuotesByDate = @"..\..\sql\GetQuotesByDate.sql"

type private GetQuotesByDateCommandProvider = SqlCommandProvider<sqlGetQuotesByDate, connectionName, ConfigFile = configFile>

let private toQuote (record : GetQuotesByDateCommandProvider.Record) : Quote =

    { Date     = record.Date
      Ticker   = record.Ticker
      Hi       = record.Hi
      Lo       = record.Lo
      Close    = record.Close
      Dividend = None
      SplitNew = None
      SplitOld = None }

let getQuotes date =
    use command = new GetQuotesByDateCommandProvider()
    let records = command.Execute(date)
    records
    |> Seq.map toQuote
    |> Seq.toArray

//-------------------------------------------------------------------------------------------------

[<Literal>]
let private sqlGetHolidays = @"..\..\sql\GetHolidays.sql"

type private GetHolidaysCommandProvider = SqlCommandProvider<sqlGetHolidays, connectionName, ConfigFile = configFile>

let getHolidays (dateFrom : DateTime) (dateTo : DateTime) =
    use command = new GetHolidaysCommandProvider()
    let records = command.Execute(dateFrom, dateTo)
    records
    |> Seq.toArray
