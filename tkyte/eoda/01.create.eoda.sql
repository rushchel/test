define username=eoda
define userpass=eoda
create user &&username identified by &&userpass default tablespace users temporary tablespace temp;
grant dba to &&username;
grant execute on dbms_stats to &&username;
grant select on v_$statname to &&username;
grant select on v_$mystat to &&username;
grant select on v_$latch to &&username;
grant select on v_$timer to &&username;
