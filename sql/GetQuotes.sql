select
    [Date],
    [Open],
    [High],
    [Low],
    [Close]
from
    [Quote] quote inner join [Issue] issue on quote.IssueId = issue.IssueId
where
    issue.ticker = @ticker
order by
    quote.[Date] asc
