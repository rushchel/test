/*----------------------Тестовое задание на Oracle PL/SQL----------------------
С помощью простых операций написать SQL выражение/запрос для вычисления числа 
рабочих дней между двумя датами. Выходными предполагаются суббота, воскресенье. 
Желательно чтобы результат не зависел от локализации БД.

d1 date;
d2 date;

TRUNC( d2 - d1 ) 	- число дней между датами
TRUNC( d1, 'D' ) 	- окруление на начало недели
TRUNC( d1, 'MM' ) 	- окруление на начало месяца

LEAST ( a, b ) 	- наименьшее значение
GREATEST ( a, b )	- наибольшее значение

FLOOR (a) – округление до максимального целого, меньше а
a mod b – остаток от деления a на b

-----------------------------------------------------------------------------*/

/* Функция возвращает число рабочих дней между двумя датами
*  @param beg_date  Дата начала периода
*  @param end_date  Дата окончания
*/
create or replace function weekdays_cnt (
  beg_date in date
 ,end_date in date
) return number is
  wRes number;
begin
  -- Нужно вычислить значение выражения A + B - C, где
  -- A это число рабочих дней между началом недели наименьшей даты и началом недели наибольшей даты
  -- B это чилсо неучтенных рабочих дней на последней неделе
  -- C это число "лишних" рабочих дней, учтенных на первой неделе
  with d as (select d1
                   ,d2
                   ,mon
                   ,mod(mon + 5, 7) sat
                   ,mod(mon + 6, 7) sun
               from (select least(beg_date, end_date)          d1
                           ,greatest(beg_date, end_date)       d2
                           ,to_date('15.05.2017','dd.mm.yyyy')-trunc(to_date('15.05.2017','dd.mm.yyyy'),'D') mon --опорный пн
                       from dual)
            )
  select  /*A*/(trunc(d2,'D')-trunc(d1,'D')) * 5 / 7
        + /*B*/((d2 - trunc(d2,'D')) - (case when d2 - trunc(d2,'D') > sat then 1 else 0 end) - (case when d2 - trunc(d2,'D') > sun then 1 else 0 end))
        - /*C*/((d1 - trunc(d1,'D')) - (case when d1 - trunc(d1,'D') > sat then 1 else 0 end) - (case when d1 - trunc(d1,'D') > sun then 1 else 0 end))
    into wRes
    from d;
  return wRes;
end;
/
