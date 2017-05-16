/*----------------------Тестовое задание на Oracle PL/SQL----------------------
Написать pl/sql функцию, которая на вход принимает строку и возвращает строку 
с убранными повторяющимися вхождениями (разделитель «;»), отсортированными 
в алфавитном порядке. 

Например, результатом обработки строки: 
"test12 34;test 12;abv;abvavb;test 12;"
должна быть строка
"abv;abvavb;test 12;test12 34;"
-----------------------------------------------------------------------------*/

/* Функция, которая на вход принимает строку и возвращает строку с убранными 
*  повторяющимися вхождениями (разделитель «;»), отсортированными в алфавитном порядке
*  @param input  Входная строка
*  @param delim  Символ-разделитель
*/
create or replace function sort_words (
  input in varchar2
 ,delim in varchar2 default ';'
) return varchar2 is
  wRes varchar2(32767);
begin
  select listagg(str) within group (order by str) as str
    into wRes
    from (select str
            from (select regexp_substr(str,'[^'||sort_words.delim||']+', 1, level) || sort_words.delim as str
                    from (select sort_words.input as str from dual)
                 connect by instr(str, sort_words.delim, 1, level) > 0)
           group by str);
  return wRes;
end;
/
