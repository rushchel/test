create or replace package runstats_pkg is
procedure rs_start;
procedure rs_middle;
procedure rs_stop(p_difference_threshold in number default 0);
end;
/

create or replace package body runstats_pkg is

g_start number;
g_run1  number;
g_run2  number;
c_mask_dble constant varchar2(30) := '99,999.99';
c_mask_mlrd constant varchar2(30) := '999,999,999,999';
c_mask_trln constant varchar2(30) := '9,999,999,999,999';

procedure rs_start is
begin
  delete from run_stats;
  insert into run_stats select 'before', s.* from stats s;
  g_start := dbms_utility.get_cpu_time;
end;

procedure rs_middle is
begin
  g_run1 := (dbms_utility.get_cpu_time - g_start);
  insert into run_stats select 'after 1', s.* from stats s;
  g_start := dbms_utility.get_cpu_time;
end;

procedure rs_stop(p_difference_threshold in number default 0) is
begin
  g_run2 := (dbms_utility.get_cpu_time - g_start);
  dbms_output.put_line('Run1 ran in ' || g_run1 || ' cpu hsecs');
  dbms_output.put_line('Run2 ran in ' || g_run2 || ' cpu hsecs');
  if g_run2 <> 0 then
    dbms_output.put_line('run 1 ran in ' || round(g_run1/g_run2*100,2) ||'% of the time');
  end if;
  
  dbms_output.put_line(chr(9));
  insert into run_stats select 'after 2', s.* from stats s;
  dbms_output.put_line(rpad('Name', 30) || lpad('Run1', 16) || lpad('Run2', 16) || lpad('Diff', 16));
  
  for x in (select rpad(a.name, 30) 
                || to_char(b.value-a.value, c_mask_mlrd)
                || to_char(c.value-b.value, c_mask_mlrd)
                || to_char((c.value-b.value) - (b.value-a.value), c_mask_mlrd) data
              from run_stats a
                  ,run_stats b
                  ,run_stats c
             where a.name = b.name
               and b.name = c.name
               and a.runid = 'before'
               and b.runid = 'after 1'
               and c.runid = 'after 2'
               and abs((c.value-b.value) - (b.value-a.value)) > p_difference_threshold
             order by ((c.value-b.value) - (b.value-a.value))
           ) loop
    dbms_output.put_line(x.data);
  end loop;
  
  dbms_output.put_line(chr(9));
  dbms_output.put_line('Run1 latches total versus runs -- difference and pct');
  dbms_output.put_line(lpad('Run1', 14) || lpad('Run2', 19) || lpad('Diff', 18) || lpad('Pct', 11));
  for x in (select to_char(run1, c_mask_trln)
                || to_char(run2, c_mask_trln) 
                || to_char(diff, c_mask_trln)
                || to_char(round(run1/decode(run2,0,to_number(0),run2) * 100, 2), c_mask_dble) || '%' data
              from (select sum(b.value - a.value) run1
                          ,sum(c.value - b.value) run2
                          ,sum((c.value - b.value) - (b.value - a.value)) diff
                      from run_stats a
                          ,run_stats b
                          ,run_stats c
                     where a.name = b.name
                       and b.name = c.name
                       and a.runid = 'before'
                       and b.runid = 'after 1'
                       and c.runid = 'after 2'
                       and a.name like 'LATCH%'
                   )
           ) loop
    dbms_output.put_line(x.data);
  end loop;
end;

end;
/