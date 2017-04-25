create or replace view stats as
select 'STAT...' || a.name name
      ,b.value
  from v$statname a
      ,v$mystat   b
 where a.statistic# = b.statistic#
union all
select 'LATCH.' || name
      ,gets
  from v$latch
union all
select 'STAT...Elapsed Time'
      ,hsecs
  from v$timer
/
  

create global temporary table run_stats (
  runid varchar2(15)
 ,name  varchar2(80)
 ,value int
) on commit preserve rows
/