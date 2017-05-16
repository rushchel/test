/*----------------------Тестовое задание на Oracle PL/SQL----------------------
По данным таблицы period_task4 получить суммарное значение val в периодах,
которые образуются на временной оси, если на ней отметить значения date_in, date_out.

Пример исходных данных:
--drop table period_task4;
create table period_task4 (id number, val number, date_in date, date_out date);

insert into period_task4
select 1, 1, to_date('01/01/2013', 'dd/mm/yyyy'), to_date('01/11/2013', 'dd/mm/yyyy') from dual
union all
select 2, 1, to_date('01/10/2013', 'dd/mm/yyyy'), to_date('01/01/2014', 'dd/mm/yyyy') from dual
union all
select 3, 1, to_date('01/01/2014', 'dd/mm/yyyy'), to_date('01/03/2014', 'dd/mm/yyyy') from dual
union all
select 4, 3, to_date('01/02/2014', 'dd/mm/yyyy'), to_date('01/05/2014', 'dd/mm/yyyy') from dual
union all
select 5, 3, to_date('01/04/2014', 'dd/mm/yyyy'), to_date('01/06/2014', 'dd/mm/yyyy') from dual
union all
select 6, 4, to_date('01/06/2014', 'dd/mm/yyyy'), to_date('01/07/2014', 'dd/mm/yyyy') from dual

commit;

По данному примеру должен быть следующий результат:

Sum  Date_in     Date_out
1    01/01/2013  01/10/2013
2    01/10/2013  01/11/2013
1    01/11/2013  01/02/2014
4    01/02/2014  01/03/2014
.    ...         ...

-----------------------------------------------------------------------------*/

with d as (select d --получаю уникальные даты
             from (select date_in d from period_task4
                   union all
                   select date_out from period_task4)
            group by d)
    ,p as (select * --разбиваю на интервалы
             from (select d.d d_in
                         ,lead(d.d) over (order by d.d) d_out
                     from d)
            where d_out is not null)
select (select sum(val)
          from period_task4
         where date_in <= p.d_in
           and date_out >= p.d_out) as Sum
      ,p.d_in                       as Date_in
      ,p.d_out                      as Date_out
  from p
 order by Date_in;
