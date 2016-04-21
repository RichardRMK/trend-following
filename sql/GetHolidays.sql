select
    [Date] as [Date]
from
    [Holiday] holiday
where
    holiday.[Date] between @dateStart and @dateFinal
order by
    holiday.[Date] asc
