create or replace procedure show_space(
  p_segname   in varchar2
 ,p_owner     in varchar2 default user
 ,p_type      in varchar2 default 'TABLE'
 ,p_partition in varchar2 default null
) authid current_user as
-- Данная процедура использует authid текущего пользователя,
-- поэтому она может запрашивать представления DBA_*, используя
-- привилегии из ROLE, и допускает однократную установку для всей БД,
-- а не для каждого пользователя, который желает ее применить
  l_free_blks     number;
  l_total_blocks  number;
  l_total_bytes   number;
  l_unused_blocks number;
  l_unused_bytes  number;
  l_last_used_ext_file_id  number;
  l_last_used_ext_block_id number;
  l_last_used_block    number;
  l_segment_space_mgmt varchar2(255);
  l_unformatted_blocks number;
  l_unformatted_bytes  number;
  l_fs1_blocks  number;
  l_fs2_blocks  number;
  l_fs3_blocks  number;
  l_fs4_blocks  number;
  l_full_blocks number;
  l_fs1_bytes  number;
  l_fs2_bytes  number;
  l_fs3_bytes  number;
  l_fs4_bytes  number;
  l_full_bytes number;
  l_mask varchar2(255) := '999,999,999,999';
  -- Форматированный вывод чисел с простыми метками
  procedure p (p_label in varchar2, p_num in number) is
  begin
    dbms_output.put_line(rpad(p_label, 40, '.') || to_char(p_num, l_mask));
  end;
begin
  -- Этот запрос выполняется динамически, чтобы позволить данной процедуре
  -- быть созданной пользователем, имеющим доступ к представлениям
  -- DBA_SEGMENTS/TABLESPACES посредством роли, как обычно принято.
  -- ПРИМЕЧАНИЕ: во время выполнения вызывающаю процедура
  -- ДОЛЖНА иметь доступ к этим двум представлениям!
  -- Это запрос определяет, является ли данный объект объектом ASSM
  begin
    execute immediate 'select ts.segment_space_management
                         from dba_segments    seg
                             ,dba_tablespaces ts
                        where seg.segment_name = :p_segname
                          and (:p_partition is null or seg.partition_name = :p_partition)
                          and seg.owner = :p_owner
                          and seg.tablespace_name = ts.tablespace_name'
                         into l_segment_space_mgmt
                        using p_segname, p_partition, p_partition, p_owner;
  exception 
    when too_many_rows then
      dbms_output.put_line('This must be a partitioned table? use p_partition =>');
      return;
  end;
  -- Если ASSM, то мы должны применять API-вызов SPACE_USAGE.
  -- В противном случае используем API-интерфейс FREE_BLOCKS
  -- для сегментов, управляемых пользователем.
  if l_segment_space_mgmt = 'AUTO' then
    dbms_space.space_usage(p_owner
                          ,p_segname
                          ,p_type
                          ,l_unformatted_blocks
                          ,l_unformatted_bytes
                          ,l_fs1_blocks
                          ,l_fs1_bytes
                          ,l_fs2_blocks
                          ,l_fs2_bytes
                          ,l_fs3_blocks
                          ,l_fs3_bytes
                          ,l_fs4_blocks
                          ,l_fs4_bytes
                          ,l_full_blocks
                          ,l_full_bytes
                          ,p_partition);
    p('Unformatted Blocks ' , l_unformatted_blocks);
    p('FS1 Blocks (0-25) '  , l_fs1_blocks);
    p('FS2 Blocks (25-50) ' , l_fs2_blocks);
    p('FS3 Blocks (50-75) ' , l_fs3_blocks);
    p('FS4 Blocks (75-100) ', l_fs4_blocks);
    p('Full Blocks         ', l_full_blocks);
  else
    dbms_space.free_blocks(segment_owner => p_owner
                          ,segment_name  => p_segname
                          ,segment_type  => p_type
                          ,freelist_group_id => 0
                          ,free_blks         => l_free_blks);
    p('Free Blocks ', l_free_blks);
  end if;
  -- С помощью API-вызова unused_space извлекается остальная информация
  dbms_space.unused_space(segment_owner => p_owner
                         ,segment_name  => p_segname
                         ,segment_type  => p_type
                         ,partition_name => p_partition
                         ,total_blocks   => l_total_blocks
                         ,total_bytes    => l_total_bytes
                         ,unused_blocks  => l_unused_blocks
                         ,unused_bytes   => l_unused_bytes
                         ,last_used_extent_file_id  => l_last_used_ext_file_id
                         ,last_used_extent_block_id => l_last_used_ext_block_id
                         ,last_used_block => l_last_used_block);
  p('Total Blocks', l_total_blocks);
  p('Total Bytes', l_total_bytes);
  p('Total MBytes', trunc(l_total_bytes/1024/1024));
  p('Unused Blocks', l_unused_blocks);
  p('Unused Bytes', l_unused_bytes);
  p('Last Used Ext FileId', l_last_used_ext_file_id);
  p('Last Used Ext BlockId', l_last_used_ext_block_id);
  p('Last Used Block', l_last_used_block);
end;
/