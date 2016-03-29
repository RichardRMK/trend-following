module TrendFollowing.Data

open FSharp.Data

//-------------------------------------------------------------------------------------------------

[<Literal>]
let connectionName = @"name=database"

[<Literal>]
let configFile = @"..\..\private\App.config"

[<Literal>]
let sqlGetQuote = @"..\..\sql\GetQuotes.sql"

type GetQuoteCommandProvider = SqlCommandProvider<sqlGetQuote, connectionName, ConfigFile = configFile>

type Quote = GetQuoteCommandProvider.Record

//-------------------------------------------------------------------------------------------------

let getQuotes ticker =
    use command = new GetQuoteCommandProvider()
    command.Execute(ticker)
