drop table big_table;
create table big_table as
select rownum id
      ,owner
      ,object_name
      ,subobject_name
      ,object_id
      ,data_object_id
      ,object_type
      ,created
      ,last_ddl_time
      ,timestamp
      ,status
      ,temporary
      ,generated
      ,secondary
      ,namespace
      ,edition_name
  from all_objects
 where 1 = 0;
alter table big_table nologging;

declare
  l_cnt  number;
  l_rows number := &numrows;
begin
  insert /*+ append */
    into big_table
  select rownum id
        ,owner
        ,object_name
        ,subobject_name
        ,object_id
        ,data_object_id
        ,object_type
        ,created
        ,last_ddl_time
        ,timestamp
        ,status
        ,temporary
        ,generated
        ,secondary
        ,namespace
        ,edition_name
    from all_objects
   where rownum <= &numrows;
  --
  l_cnt := sql%rowcount;
  commit;
  while (l_cnt < l_rows) loop
    insert /*+ append */
      into big_table
    select rownum + l_cnt
          ,owner
          ,object_name
          ,subobject_name
          ,object_id
          ,data_object_id
          ,object_type
          ,created
          ,last_ddl_time
          ,timestamp
          ,status
          ,temporary
          ,generated
          ,secondary
          ,namespace
          ,edition_name
      from all_objects
     where rownum <= l_rows - l_cnt;
    l_cnt := l_cnt + sql%rowcount;
    commit;
  end loop;
end;
/

alter table big_table add constraint big_table_pk primary key(id);
exec dbms_stats.gather_table_stats(user, 'BIG_TABLE', estimate_percent => 1);