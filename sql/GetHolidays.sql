select
    [Date] as [Date]
from
    [Holiday] holiday
where
    holiday.[Date] between @dateFrom and @dateTo
order by
    holiday.[Date] asc
