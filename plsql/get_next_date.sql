/*----------------------Тестовое задание на Oracle PL/SQL----------------------
Есть матрица расписания запусков:
0,45                   15 minutes
0,4,8,12,17,22         hours
2,6                    weekdays
1,2,3,4,5,11,18,24     monthdays
1,2,3,9,11             months

Первая строка – 15-и минутные интервалы, вторая строка часовые интервалы, 
третья строка дни недели, четвертая дни месяца, пятая месяцы года. 
С помощью данной матрицы задается периодичность запусков.

Требуется написать функцию на Oracle PL/SQL, которая бы возвращала дату 
следующего запуска (тип Date) от двух входных параметров:
Первый параметр (тип Date): дата, от которой ведется отчет;
Второй параметр (тип Varchar2): это текстовая переменная, в которой 
перечислены все выбранные ячейки. Ячейки разделены «,» (запятой), а строки 
разделены «;» (точкой с запятой), например, для данного рисунка расписание 
будет выглядеть следующим образом: 
0,45;0,4,8,12,17,22;2,6;1,2,3,4,5,11,18,24;1,2,3,9,11;

Контрольный пример:
Дата отсчета: 09.07.2010 23:36
Строка: 0,45;12;1,2,6;3,6,14,18,21,24,28;1,2,3,4,5,6,7,8,9,10,11,12;
Результат: 18.07.2010 12:00

Примечание. В данном примере, используется американский календарь, 
в котором 1 – это воскресенье, 2 – понедельник и т.д.
-----------------------------------------------------------------------------*/

/* Функция возвращает дату следующего запуска по матрице периодичности
*  @param start_date       Дата, от которой ведется отчет
*  @param repeat_interval  Текстовая переменная, задающая матрицу запусков
*/
create or replace function get_next_date(
  start_date      date
 ,repeat_interval varchar2
) return date
is
  wRes      timestamp;
  wTemplate varchar2(1000 char);
begin
  begin
    -- трансформация матрицы запусков для планировщика
    with t as (select get_next_run_date.repeat_interval str from dual)
    select 'FREQ=YEARLY;'||t3.minutes||t3.hours||t3.weekdays||t3.monthdays||t3.months tmpl
      into wTemplate
      from (select t2.minutes
                  ,t2.hours
                  ,replace(
                   replace(
                   replace(
                   replace(
                   replace(
                   replace(
                   replace(t2.weekdays,'1','SUN')
                                      ,'2','MON')
                                      ,'3','TUE')
                                      ,'4','WED')
                                      ,'5','THU')
                                      ,'6','FRI')
                                      ,'7','SAT') weekdays
                  ,t2.monthdays
                  ,t2.months
              from (select 'BYMINUTE='||regexp_substr(str, '[^;]+', 1, 1)||';'   minutes
                          ,'BYHOUR='||regexp_substr(str, '[^;]+', 1, 2)||';'     hours
                          ,'BYDAY='||regexp_substr(str, '[^;]+', 1, 3)||';'      weekdays
                          ,'BYMONTHDAY='||regexp_substr(str, '[^;]+', 1, 4)||';' monthdays
                          ,'BYMONTH='||regexp_substr(str, '[^;]+', 1, 5)||';'    months
                      from t
                   ) t2 
           ) t3;
    -- получение следующей даты запуска
    dbms_scheduler.evaluate_calendar_string (wTemplate
                                            ,cast(start_date as timestamp)
                                            ,cast(start_date as timestamp)
                                            ,wRes);
  exception when others then 
    raise_application_error(-20001,'Произошла ошибка - '||sqlerrm);
  end;
  return cast(wRes as date);
end;
/