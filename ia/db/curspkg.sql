create or replace package curspkg as

type t_cursor is ref cursor;

type ora_rec is record (
  code  number
 ,name  varchar2(200 char)
 ,value varchar2(200 char)
);

type ora_rec_tab is table of ora_rec;

procedure one_cursor (
  inp_num   in number
 ,cursor_01 in out t_cursor
);

procedure two_cursors (
  inp_num   in number
 ,cursor_01 out t_cursor
 ,cursor_02 out t_cursor
);

procedure get_objects (
  inp_num   in number
 ,rec       out ora_rec
 ,tab       out ora_rec_tab
);

end;
/

create or replace package body curspkg as

procedure one_cursor (
  inp_num   in number
 ,cursor_01 in out t_cursor
) is
  v_cursor t_cursor;
  begin
  if inp_num <> 0 then
    open v_cursor for
      select *
        from user_tables ;
  else
    open v_cursor for
      select *
        from user_types ;
  end if;
  cursor_01 := v_cursor;
end;

procedure two_cursors (
  inp_num   in  number
 ,cursor_01 out t_cursor
 ,cursor_02 out t_cursor
) is
  v_cursor1 t_cursor;
  v_cursor2 t_cursor;
begin
  open v_cursor1 for 
    select '123'         Id
          ,'OTPBANK'     Name
          ,'0'           Blocked
      from dual;
  open v_cursor2 for 
    select 'АО ОТП БАНК'                           LegalName
          ,'ул. Орджоникидзе 3А'                   LegalAddress
          ,'г. Омск - 644043, ул. Орджоникидзе 3А' PostAddress
          ,'550777777777'                          TaxpayerIdNumber
          ,'550999999999'                          RRCode
          ,'+73812123456'                          ContactPhoneNumber
      from dual;
  cursor_01  := v_cursor1;
  cursor_02  := v_cursor2;
end;

procedure get_objects (
  inp_num   in number
 ,rec       out ora_rec
 ,tab       out ora_rec_tab
) is
  l_rec ora_rec; 
  l_tab ora_rec_tab;
begin
  l_rec.code  := 1;
  l_rec.name  := 'VCS';
  l_rec.value := 'GIT';
  for i in 2..9 loop
    l_tab(i).code  := i;
    l_tab(i).name  := 'A-'||i;
    l_tab(i).value := 'B-'||i;
  end loop;
  rec := l_rec;
  tab := l_tab;
end;

end;
/