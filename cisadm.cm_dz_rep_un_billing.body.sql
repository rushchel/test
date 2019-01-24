create or replace package body cisadm.CM_DZ_REP_UN_BILLING is

  -- Private type declarations
 /* type <TypeName> is <Datatype>;

  -- Private constant declarations
  <ConstantName> constant <Datatype> := <Value>;

  -- Private variable declarations
  <VariableName> <Datatype>;

  -- Function and procedure implementations
  function <FunctionName>(<Parameter> <Datatype>) return <Datatype> is
    <LocalVariable> <Datatype>;
  begin
    <Statement>;
    return(<Result>);
  end;*/
  --
  function money_delimited(i_sum in char, i_flag_cop in char) return varchar2 is
    v_sum varchar(100);
  begin
    begin
    if i_flag_cop='FULL' then
      v_sum:= to_char(to_number(i_sum, '999999999990.99'), '999,999,999,990.00');
    end if;
    if i_flag_cop='RUB' then
      v_sum:= to_char(trunc(to_number(i_sum, '999999999990.99'), 0),'999,999,999,999');
    end if;
    if (i_flag_cop='COP')or(i_flag_cop='KOP') then
      v_sum:= mod(to_number(i_sum, '9999999990.99')*100, 100);
      if length(v_sum)=1 then v_sum:= '0'||v_sum; end if;
    end if;
    --
    v_sum:= trim(replace(v_sum, ',', ' '));
    exception
      when OTHERS then v_sum := null;
    end;
    --
    return v_sum;
  end money_delimited;
  --
  function get_draft_name(i_draft_id char) return  varchar2 is
    v_buf varchar2(100);
  begin
    begin
      select ' № ' || c_n.adhoc_char_val || ' (' ||
                   trim(c_r.char_val) || ') от ' ||
                              c_d.adhoc_char_val || ' г.' pt
      into v_buf
      from
        ci_case draft
        left join ci_case_char c_n on c_n.case_id = draft.case_id and c_n.char_type_cd='DRAFTNUM'
        left join ci_case_char c_d on c_d.case_id = draft.case_id and c_d.char_type_cd='OUTGO-DT'
        left join ci_case_char c_r on c_r.case_id = draft.case_id and c_r.char_type_cd='RETLTYPE'
      where  draft.case_id = i_draft_id;
     exception
      when no_data_found then v_buf := null;
    end;
    return v_buf;
  end get_draft_name;
  --
  function get_pt_list(i_wf_id varchar2) return  varchar2 is
    v_buf varchar2(4000);
    v_wf_proc_tp ci_wf_proc_char.char_val_fk1%type;
    v_wf_proc_id ci_wf_proc.wf_proc_id%type;
  begin
    select char_val_fk1 into v_wf_proc_tp
    from ci_wf_proc_char where wf_proc_id = i_wf_id and char_type_cd='PROC_TP';
    --
    /*if v_wf_proc_tp='LIMITATION' then
      select
      v_wf_proc_id:= i_wf_id;
    else
      v_wf_proc_id:= i_wf_id;
    end if; */
    /*
    select df1.draftnum, df1.draftoutgoingdate,df1.DraftRealType -- TR-16536, TR-18362
       from
         cm_draft_vw df1
         join ci_wf_proc_char dpc1 on dpc1.char_val_fk1=df1.case_id and dpc1.char_type_cd in ('DZ-DRAFT', 'CZ-DRAFT')
      where  dpc1.wf_proc_id= i_wf_id
    */
    v_buf:='';
    for lst_kpd in (
      select ' № ' || c_n.adhoc_char_val || ' (' ||
                   trim(c_r.char_val) || ') от ' ||
                              c_d.adhoc_char_val || ' г.' pt,
      cm_util.char_val_to_date(c_d.adhoc_char_val) pt_dat
      from
        ci_case c
        join ci_wf_proc_char dpc1 on dpc1.char_val_fk1=c.case_id and dpc1.char_type_cd in ('DZ-DRAFT', 'CZ-DRAFT')
        left join ci_case_char c_n on c_n.case_id = c.case_id and c_n.char_type_cd='DRAFTNUM'
        left join ci_case_char c_d on c_d.case_id = c.case_id and c_d.char_type_cd='OUTGO-DT'
        left join ci_case_char c_r on c_r.case_id = c.case_id and c_r.char_type_cd='RETLTYPE'
      where  dpc1.wf_proc_id= i_wf_id
      order by 2
       )
        loop
          if length(v_buf)>0  then v_buf:= v_buf||', '; end if;
          v_buf := v_buf || lst_kpd.pt;
          --' № ' || lst_kpd.draftnum || ' (' || trim(lst_kpd.DraftRealType) || ') от ' ||
          --       lst_kpd.DraftOutGoingDate || ' г.';
        end loop;
      --  v_buf:=rtrim(v_buf,'., ');
    --
    return v_buf;
  end;
  --
  function get_pt_list_lim(i_doc_id varchar2) return  varchar2 is
    v_buf varchar2(4000);
    v_wf_proc_tp ci_wf_proc_char.char_val_fk1%type;
    v_wf_proc_id ci_wf_proc.wf_proc_id%type;

    v_wf_ntf_id ci_wf_proc.wf_proc_id%type;
  begin
    select wp_lim.cr_by_wf_proc_id into v_wf_ntf_id
    from ci_case_char c2 join ci_wf_proc wp_lim on wp_lim.wf_proc_id=c2.char_val_fk1
    where c2.case_id= i_doc_id and c2.char_type_cd='PCZ-EVT';
    --
    --select char_val_fk1 into v_wf_proc_tp
    --from ci_wf_proc_char where wf_proc_id = i_wf_id and char_type_cd='PROC_TP';
    --
    /*if v_wf_proc_tp='LIMITATION' then
      select
      v_wf_proc_id:= i_wf_id;
    else
      v_wf_proc_id:= i_wf_id;
    end if; */
    /*
    select df1.draftnum, df1.draftoutgoingdate,df1.DraftRealType -- TR-16536, TR-18362
       from
         cm_draft_vw df1
         join ci_wf_proc_char dpc1 on dpc1.char_val_fk1=df1.case_id and dpc1.char_type_cd in ('DZ-DRAFT', 'CZ-DRAFT')
      where  dpc1.wf_proc_id= i_wf_id
    */
    v_buf:='';
    for lst_kpd in (
      with draft as (
        select distinct c.case_id
        from
        ci_case c
        join ci_wf_proc_char dpc1 on dpc1.char_val_fk1=c.case_id and dpc1.char_type_cd in ('DZ-DRAFT', 'CZ-DRAFT')

        join ci_case_char cc_me on cc_me.case_id=c.case_id and cc_me.char_type_cd='ME-ID'
        join ci_ft ft on (ft.match_evt_id=cc_me.char_val_fk1 and ft.accounting_dt<=sysdate)
        join cm_account_entry ae on (ae.cm_generative_ft_id=ft.ft_id and ae.accounting_dt<=sysdate)
        join cm_rate_prop rp on (rp.cm_rate_prop_id = ae.cm_rate_prop_id and rp.accounting_dt<=sysdate)
        where  dpc1.wf_proc_id= v_wf_ntf_id
        group by c.case_id
        having sum(ae.cur_amt)>0
      )

      select distinct
        ' № ' || c_n.adhoc_char_val || ' (' ||
                   trim(c_r.char_val) || ') от ' ||
                              c_d.adhoc_char_val || ' г.' pt
      from
        draft dft
        left join ci_case_char c_n on c_n.case_id = dft.case_id and c_n.char_type_cd='DRAFTNUM'
        left join ci_case_char c_d on c_d.case_id = dft.case_id and c_d.char_type_cd='OUTGO-DT'
        left join ci_case_char c_r on c_r.case_id = dft.case_id and c_r.char_type_cd='RETLTYPE'



       )
        loop
          if length(v_buf)>0  then v_buf:= v_buf||', '; end if;
          v_buf := v_buf || lst_kpd.pt;
          --' № ' || lst_kpd.draftnum || ' (' || trim(lst_kpd.DraftRealType) || ') от ' ||
          --       lst_kpd.DraftOutGoingDate || ' г.';
        end loop;
      --  v_buf:=rtrim(v_buf,'., ');
    --
    return v_buf;
  end get_pt_list_lim;
  --
  function get_evt_pt_list(i_wf_proc_id in char, i_evt_seq number) return  varchar2 is
    v_buf varchar2(4000);
    v_wf_proc_tp ci_wf_proc_char.char_val_fk1%type;
    v_wf_proc_id ci_wf_proc.wf_proc_id%type;
  begin
    v_buf:='';
    for lst_kpd in (-- TR-16536, TR-18362
      select ' № ' || c_n.adhoc_char_val || ' (' ||
                   trim(c_r.char_val) || ') от ' ||
                              c_d.adhoc_char_val || ' г.' pt,
      cm_util.char_val_to_date(c_d.adhoc_char_val) pt_dat
      from
        ci_case c
        join ci_wf_evt_char dpc1 on dpc1.char_val_fk1=c.case_id and dpc1.char_type_cd in ('DZ-DRAFT', 'CZ-DRAFT')
        left join ci_case_char c_n on c_n.case_id = c.case_id and c_n.char_type_cd='DRAFTNUM'
        left join ci_case_char c_d on c_d.case_id = c.case_id and c_d.char_type_cd='OUTGO-DT'
        left join ci_case_char c_r on c_r.case_id = c.case_id and c_r.char_type_cd='RETLTYPE'
      where
        dpc1.wf_proc_id= i_wf_proc_id and dpc1.evt_seq = i_evt_seq
      order by 2)
    loop
      if length(v_buf)>0  then v_buf:= v_buf||', '; end if;
      v_buf := v_buf || lst_kpd.pt;
    end loop;
    --
    return v_buf;
  end get_evt_pt_list;
  --
  function get_ik_4_prem(in_prem_id in CHAR) return t_ik_tab pipelined
  as
    rec_row t_ik_row;
  begin
    for a1 in (select prem_id, cm_char_utils.get_prem_bigcharval(prem_id, 'TU-NAME', sysdate) ik_name
               from ci_prem where prem_type_cd like 'IK%'
               CONNECT BY PRIOR prem_id = prnt_prem_id
               start with prnt_prem_id=in_prem_id)
    loop
      rec_row.IK_NAME:= a1.prem_id;
      rec_row.ik_name:= a1.ik_name;
      pipe row(rec_row);
    end loop;
    /*
with seo as
 (select wfc1.char_val_fk1 eo_id from ci_case_char cc1
  inner join ci_wf_proc_char wfc1 on wfc1.wf_proc_id=cc1.char_val_fk1 and wfc1.char_type_cd='OB-OBSL'
  where cc1.case_id= '0335787825' and cc1.char_type_cd='PCZ-EVT'),
sik as
 (select distinct p.prem_id ik_id
  from ci_prem p
  inner join cm_prem_link l on l.cm_source_prem_id = p.prem_id
  inner join seo s1 on s1.eo_id=p.prnt_prem_id
  where prem_type_cd like 'IK%'
  CONNECT BY PRIOR p.prem_id = l.cm_dest_prem_id)

select trim(pc_num.adhoc_char_val) ik_id , --ik_num
             mtr.serial_nbr ik_name
      from       ci_prem         ik
      join ci_prem_char pc_num on ik.prem_id=pc_num.prem_id and pc_num.char_type_cd = 'TU-NUM' and effdt = (select max(effdt) from ci_prem_char pci where pci.char_type_cd=pc_num.char_type_cd and pci.prem_id=pc_num.prem_id and pci.effdt<=sysdate)
      inner join ci_sp           sp   on sp.prem_id = ik.prem_id and sp.sp_type_cd = 'ENERGY'
      inner join ci_sp_mtr_hist  spmh on spmh.sp_id = sp.sp_id
      inner join ci_mtr_config   mtrc on spmh.mtr_config_id = mtrc.mtr_config_id
      inner join ci_mtr          mtr  on mtrc.mtr_id = mtr.mtr_id
      -- только экземпляры приборов
      inner join ci_mtr_typ_char tc   on mtr.mtr_type_cd = tc.mtr_type_cd and tc.char_type_cd = 'IZM-KOMP' and tc.char_val = 'PU'
      inner join ci_mtr_char     ch   on ch.mtr_id = mtr.mtr_id and ch.char_type_cd = 'OBJ-TYPE' and ch.char_val = 'INSTANCE'
      -- отсечка по ИК
      inner join sik sik on sik.ik_id=ik.prem_id
      -- дата установки
      left join ci_sp_mtr_evt    spmei on spmei.sp_mtr_hist_id = spmh.sp_mtr_hist_id and spmei.sp_mtr_evt_flg = 'I'
      left join ci_mr            r_mr on spmei.mr_id = r_mr.mr_id
      where --ik.prem_id  in (select ik_id from sik      )

        --and
        spmh.removal_dttm is null
        and mtr.receive_dt<sysdate

      order by length(ik_id), ik_id
    */
  end  get_ik_4_prem;
  --
  function get_ik_4_doc(in_doc_id in CHAR) return t_ik_tab pipelined
  is
    rec_row t_ik_row;
  begin
    for a1 in (
      with seo as
       (select wfc1.char_val_fk1 eo_id from ci_case_char cc1
        inner join ci_wf_proc_char wfc1 on wfc1.wf_proc_id=cc1.char_val_fk1 and wfc1.char_type_cd='OB-OBSL'
        where cc1.case_id= in_doc_id and cc1.char_type_cd='PCZ-EVT'),
      sik as
       (select distinct p.prem_id ik_id
        from ci_prem p
        inner join cm_prem_link l on l.cm_source_prem_id = p.prem_id
        inner join seo s1 on s1.eo_id=p.prnt_prem_id
        where prem_type_cd like 'IK%'
        CONNECT BY PRIOR p.prem_id = l.cm_dest_prem_id)

      select trim(pc_num.adhoc_char_val) ik_id /*ik_num*/,
             mtr.serial_nbr ik_name
      from       ci_prem         ik
        join ci_prem_char pc_num on ik.prem_id=pc_num.prem_id and pc_num.char_type_cd = 'TU-NUM' and effdt = (select max(effdt) from ci_prem_char pci where pci.char_type_cd=pc_num.char_type_cd and pci.prem_id=pc_num.prem_id and pci.effdt<=sysdate)
        inner join ci_sp           sp   on sp.prem_id = ik.prem_id and sp.sp_type_cd = 'ENERGY'
        inner join ci_sp_mtr_hist  spmh on spmh.sp_id = sp.sp_id
        inner join ci_mtr_config   mtrc on spmh.mtr_config_id = mtrc.mtr_config_id
        inner join ci_mtr          mtr  on mtrc.mtr_id = mtr.mtr_id
        -- только экземпляры приборов
        inner join ci_mtr_typ_char tc   on mtr.mtr_type_cd = tc.mtr_type_cd and tc.char_type_cd = 'IZM-KOMP' and tc.char_val = 'PU'
        inner join ci_mtr_char     ch   on ch.mtr_id = mtr.mtr_id and ch.char_type_cd = 'OBJ-TYPE' and ch.char_val = 'INSTANCE'
        -- отсечка по ИК
        inner join sik sik on sik.ik_id=ik.prem_id
        -- дата установки
        left join ci_sp_mtr_evt    spmei on spmei.sp_mtr_hist_id = spmh.sp_mtr_hist_id and spmei.sp_mtr_evt_flg = 'I'
        left join ci_mr            r_mr on spmei.mr_id = r_mr.mr_id
      where
        spmh.removal_dttm is null and mtr.receive_dt<sysdate
      order by
        length(ik_id), ik_id
               )
    loop
      rec_row.ik_id:= a1.ik_id;
      rec_row.ik_name:= a1.ik_name;
      --
      pipe row(rec_row);
    end loop;
  end get_ik_4_doc;
  --
  function get_wf_proc_id_4_doc_case(i_case_id in char) return varchar2 is
    v_wf_proc_id char(10);
  begin
    select wf_proc_id into v_wf_proc_id
    from ci_wf_evt_char  where char_val_fk1=i_case_id and char_type_cd='PRINTDOC';
    --
    return v_wf_proc_id;
  end get_wf_proc_id_4_doc_case;
  --
  function get_prem_char_val(i_prem_id in char, i_type_char_cd in char) return varchar2 is
    v_char_val varchar2(255);
    v_effdt date;
    v_prem_id ci_prem.prem_id%type;
    v_char_type_cd ci_char_type.char_type_cd%type;
  begin
    v_prem_id:= trim(i_prem_id);
    v_char_type_cd:= trim(i_type_char_cd);
    begin
      select max(effdt) into v_effdt from ci_prem_char pc1
      where pc1.prem_id = i_prem_id and pc1.char_type_cd =v_char_type_cd;
    exception
      when no_data_found then v_char_val := null;
    end;
    --
    begin
      select trim(pc1.char_val) into v_char_val
      from  ci_prem_char pc1
      where pc1.prem_id = v_prem_id and pc1.char_type_cd =v_char_type_cd and pc1.effdt=v_effdt;
    exception
      when no_data_found then v_char_val := null;
    end;
    --
    return v_char_val;
  end get_prem_char_val;
  --
  function get_prem_char_val_name(i_prem_id in char, i_type_char_cd in char) return varchar2 is
    v_char_val_name varchar2(255);
  begin
    begin
      select trim(cv2.descr) into v_char_val_name
      from
        ci_prem_char pc1
        inner join ci_char_val cv1 on cv1.char_type_cd=pc1.char_type_cd and cv1.char_val=pc1.char_val
        inner join ci_char_val_l cv2 on cv2.char_type_cd=pc1.char_type_cd and cv2.char_val=pc1.char_val and cv2.language_cd='RUS'
      where pc1.prem_id = i_prem_id and pc1.char_type_cd =i_type_char_cd and pc1.effdt =
        (select max(t1.effdt) from ci_prem_char t1 where t1.prem_id=pc1.prem_id and t1.char_type_cd=pc1.char_type_cd);
    exception
      when no_data_found then v_char_val_name := null;
    end;
    --
    return v_char_val_name;
  end get_prem_char_val_name;
  --
  function get_tfg_signer_job(i_signer_id in char, i_dpt_id in char) return varchar2
  is
    v_signer_job varchar2(250);
  begin
    case i_signer_id
      when '1' then
        v_signer_job:= 'Начальник отдела';
      when '2' then
        v_signer_job:= 'Начальник управления';
      when '3' then
        v_signer_job:= 'Заместитель управляющего директора -'||CHR(10)||
                              'директор по работе с потребителями';
      when '4' then
        v_signer_job:= 'Управляющий директор - первый '||CHR(10)||
                              'заместитель генерального директора';
      else
        v_signer_job:= '_________________________________';
    end case;
    --
    return  v_signer_job;
  end get_tfg_signer_job;
  --
  function get_tfg_signer_name(i_signer_id in char, i_dpt_id in char) return varchar2
  is
    v_signer_name varchar2(250);
  begin
    case i_signer_id
      when '1' then begin
        select pn1.entity_name into v_signer_name
        from
          ci_per p1
          inner join ci_per_per pp1 on pp1.per_id1=p1.per_id and pp1.per_rel_type_cd='DL-83'
          inner join ci_per p2 on pp1.per_id2=p2.per_id
          inner join ci_per_name pn1 on pn1.per_id=p2.per_id and pn1.name_type_flg='PRIM'
          where
            p1.per_id= i_dpt_id
            and pp1.start_dt<=sysdate and(pp1.end_dt is null or pp1.end_dt>=sysdate);
      end;
      when '2' then
        v_signer_name:= 'Андронов В.Ю.';
      when '3' then
        v_signer_name:= 'Пахомова Е.В.';
      when '4' then
        v_signer_name:= 'Шаспольский М.А.';
      else
       v_signer_name:= '___________________';
    end case;
    --
    return  v_signer_name;
  end get_tfg_signer_name;
  --
  function get_receiver(i_receiver_id in char, i_receiver_flag in char) return varchar2
  is
    v_job varchar2(250);
    v_name varchar2(250);
    v_short_name varchar2(250);
  begin
    if i_receiver_id=1 then
      v_job:= 'Начальнику управления по работе с должниками';
      v_name:= 'Андронов Вадим Юрьевич';
      v_short_name:= 'Андронов В. Ю.';
    end if;
    if i_receiver_id=2 then
      v_job:= 'Начальнику линейного отдела';
      v_name:= 'Поповой Ольге Алексеевне';
      v_short_name:= 'Попова О. А.';
    end if;
    if i_receiver_id=3 then
      v_job:= 'Начальнику контрольно-диспетчерского отдела ЗАО "Петроэлектросбыт"';
      v_name:= 'Марковой Светлане Олеговне';
      v_short_name:= 'Марковой С. О.';
    end if;
    if i_receiver_id=4 then
      v_job:= 'Начальнику отдела технического аудита потребителей электроэнергии';
      v_name:= 'Богдановой Марине Геннадиевне';
      v_short_name:= 'Богдановой М. Г.';
    end if;
    if i_receiver_id=5 then
      v_job:= 'Начальнику электротехнического управления ЗАО "Петроэлектросбыт"';
      v_name:= 'Ильину Николаю Викторовичу';
      v_short_name:= 'Ильину Н. В.';
    end if;
    if i_receiver_id=6 then
      v_job:= 'Заместителю управляющего директора - директору по режиму и экономической безопасности';
      v_name:= 'Суколину Александру Николаевичу';
      v_short_name:= 'Суколину А. Н.';
    end if;
    if i_receiver_id=7 then
      v_job:= 'Заместителю директора по режиму и экономической безопасности - '||
              'начальнику управления по работе с проблемными абонентами';
      v_name:= 'Воронину Виктору Александровичу';
      v_short_name:= 'Воронину В. А.';
    end if;
    if i_receiver_id=8 then
      v_job:= 'Начальнику отдела договорной работы и расчетов с бюджетными потребителями';
      v_name:= 'Шевченко Ирине Витальевне';
      v_short_name:= 'Шевченко И. В.';
    end if;
    if i_receiver_id=9 then
      v_job:= 'Начальнику отдела договорной работы на рынке электроэнергии';
      v_name:= 'Охапкиной Анне Анатольевне';
      v_short_name:= 'Охапкиной А. А.';
    end if;
    if i_receiver_id=10 then
      v_job:= 'Начальник отдела договорной работы УОРОСЭ';
      v_name:= 'Егоровой Татьяне Алексеевне';
      v_short_name:= 'Егоровой Т. А.';
    end if;
    if i_receiver_id=11 then
      v_job:= 'Начальник отдела договорной работы управления по работе с непромышленными потребителями';
      v_name:= 'Ивановой Вере Алексеевне';
      v_short_name:= 'Ивановой В. А.';
    end if;
    if i_receiver_id=12 then
      v_job:= 'Начальнику отдела электротехнических работ ЗАО "Петроэлетросбыт"';
      v_name:= 'Ященко Ирине Ерастовне';
      v_short_name:= 'Ященко И. Е.';
    end if;
    if i_receiver_id=13 then
      v_job:= 'Начальнику управления по работе с промышленными потребителями';
      v_name:= 'Кузнецовой Наталье Викторовне';
      v_short_name:= 'Кузнецовой Н. В.';
    end if;
    if i_receiver_id=14 then
      v_job:= 'Начальнику управления по работе с непромышленными потребителями';
      v_name:= 'Белоусову Дмитрию Сергеевичу';
      v_short_name:= 'Белоусову Д. С.';
    end if;
    if i_receiver_id=15 then
      v_job:= 'Начальнику отдела по работе с непромышленными потребителями';
      v_name:= 'Воронцовой Юлии Федоровне';
      v_short_name:= 'Воронцовой Ю. Ф.';
    end if;
    if i_receiver_id=16 then
      v_job:= 'Начальник управления поработе с крупными потребителями ОАО "Расчетный центр"';
      v_name:= 'Лочмелю Валентине Михайловне';
      v_short_name:= 'Лочмелю В. М.';
    end if;
    if i_receiver_id=17 then
      v_job:= 'Заместителю управляющего диретора - директору по правовым вопросам';
      v_name:= 'Бугровой Екатерине Александровне';
      v_short_name:= 'Бугровой Е. А.';
    end if;
    if i_receiver_id=18 then
      v_job:= 'начальнику юридического управления';
      v_name:= 'Стафутиной Наталье Сергеевне';
      v_short_name:= 'Попова О. О.';
    end if;
    --
    if i_receiver_flag='JOB' then v_short_name:= v_job; end if;
    if i_receiver_flag='NAME' then v_short_name:= v_name; end if;
    if i_receiver_flag='SHORT_NAME' then v_short_name:= v_short_name; end if;
    --
    return v_short_name;
  end get_receiver;
  --
  function get_eo_list_4_wf_proc(i_wf_proc_id in char) return t_eo_set pipelined is
    v_eo_rec t_eo_rec;
  begin
    for eo1 in (
        select
        rownum as rn,
        pr1.prem_id,
        cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'PREM_COD', current_date) EO_CODE,
        cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'PREM-NAM', current_date) EO_NAME,
        cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'DISTRICT', current_date) EO_DISTRICT,
        TRIM(pr1.postal) EO_POSTAL,
        cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'F-ADR-1 ', current_date)||
        cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'F-ADR-2 ', current_date) EO_ADDRESS,
        get_prem_char_val(pr1.prem_id, 'PR-OUTAG') sw_in_dog
        --(select cp1.char_val from ci_prem_char cp1
        -- where cp1.prem_id=pr1.prem_id and cp1.char_type_cd='PR-OUTAG') sw_in_dog
        from
          ci_wf_proc_char wf1
          inner join ci_prem pr1 on pr1.prem_id=trim(wf1.char_val_fk1) and wf1.char_type_cd='OB-OBSL'
        where
          wf1.wf_proc_id=i_wf_proc_id --and rownum=1
          )
    loop
      v_eo_rec.rn :=  eo1.rn;
      v_eo_rec.eo_id:= eo1.prem_id;
      v_eo_rec.eo_code:= trim(rpad(eo1.eo_code, 254));
      v_eo_rec.eo_name:= rpad(eo1.eo_name, 254);
      v_eo_rec.eo_district:= trim(rpad(eo1.eo_district, 254));
      v_eo_rec.eo_postal:= trim(rpad(eo1.eo_postal, 254));
      v_eo_rec.eo_address:= trim(rpad(eo1.eo_address, 254));
      v_eo_rec.sw_in_dog:= eo1.sw_in_dog;
      pipe row(v_eo_rec);
    end loop;
  end get_eo_list_4_wf_proc;

  --аналог get_eo_list_4_wf_proc, только возвращаются точки поставки
   function get_tp_list_4_wf_proc(i_wf_proc_id in char) return t_eo_set pipelined is
    v_eo_rec t_eo_rec;
  begin
    for eo1 in (select * from  (
        select
        row_number() over (partition by null order by cm_char_utils.get_prem_bigcharval(decode(pc.char_val , 'CALCULATED_GROUP', ll.cm_source_prem_id, l.cm_source_prem_id), 'CP-NUM', current_date)) as rn,
        l.cm_source_prem_id as prem_id,
        'ТП № ' || cm_char_utils.get_prem_bigcharval(decode(pc.char_val , 'CALCULATED_GROUP', ll.cm_source_prem_id, l.cm_source_prem_id), 'CP-NUM', current_date) EO_CODE,
        cm_char_utils.get_prem_bigcharval(decode(pc.char_val , 'CALCULATED_GROUP', ll.cm_source_prem_id, l.cm_source_prem_id), 'CP-NAME', current_date) EO_NAME,
        cm_char_utils.get_prem_bigcharval(pr1.prem_id, 'DISTRICT', current_date) EO_DISTRICT,
        TRIM(pr1.postal) EO_POSTAL,
        cm_char_utils.get_prem_bigcharval(pr1.prem_id, 'F-ADR-1 ', current_date)||
        cm_char_utils.get_prem_bigcharval(pr1.prem_id, 'F-ADR-2 ', current_date) EO_ADDRESS,
        get_prem_char_val(pr1.prem_id, 'PR-OUTAG') sw_in_dog
        --(select cp1.char_val from ci_prem_char cp1
        -- where cp1.prem_id=pr1.prem_id and cp1.char_type_cd='PR-OUTAG') sw_in_dog
        from
           ci_wf_proc_char wf1
          inner join ci_wf_proc p on wf1.wf_proc_id = p.wf_proc_id
          inner join ci_prem pr1 on pr1.prem_id=trim(wf1.char_val_fk1) and wf1.char_type_cd='OB-OBSL'
          inner join cm_prem_link l on pr1.prem_id = l.cm_dest_prem_id and l.cm_prem_link_type_cd in ('EE_SUPPLY')
          left  join ci_prem_char pc on l.cm_source_prem_id = pc.prem_id and pc.char_type_cd = 'SUP-P-TY'
          left  join cm_prem_link ll on ll.cm_dest_prem_id = l.cm_source_prem_id and ll.cm_prem_link_type_cd in ('EE_SUPPLY')
       where
          wf1.wf_proc_id=i_wf_proc_id
         and trunc(p.cre_dttm) between l.start_dt and nvl(l.end_dt , to_date('01.01.2100' , 'dd.mm.yyyy'))
         and (pc.effdt is null or  pc.effdt = (Select max(effdt) from ci_prem_char where prem_id = pc.prem_id and char_type_cd = pc.char_type_cd))
          )  order by rn)

    loop
      v_eo_rec.rn :=  eo1.rn;
      v_eo_rec.eo_id:= eo1.prem_id;
      v_eo_rec.eo_code:= trim(rpad(eo1.eo_code, 254));
      v_eo_rec.eo_name:= rpad(eo1.eo_name, 254);
      v_eo_rec.eo_district:= trim(rpad(eo1.eo_district, 254));
      v_eo_rec.eo_postal:= trim(rpad(eo1.eo_postal, 254));
      v_eo_rec.eo_address:= trim(rpad(eo1.eo_address, 254));
      v_eo_rec.sw_in_dog:= eo1.sw_in_dog;
      pipe row(v_eo_rec);
    end loop;
  end get_tp_list_4_wf_proc;


  --аналог get_eo_list_4_wf_proc, только возвращаются точки поставки
   function get_tp_list_4_wf_proc_lim(i_wf_proc_id in char) return t_eo_set pipelined is
    v_eo_rec t_eo_rec;
  begin
    select count(distinct p.char_val_fk1)
      into v_eo_rec.CNT_TP
    from ci_wf_proc_char p
    where p.wf_proc_id = i_wf_proc_id
      and p.char_type_cd = 'POINTDLV';

    for eo1 in (select * from  (
        select
         row_number() over (partition by null order by cm_char_utils.get_prem_bigcharval(l.cm_source_prem_id, 'CP-NUM', current_date)) as rn,
        l.cm_source_prem_id as prem_id,
        'ТП № ' || cm_char_utils.get_prem_bigcharval(l.cm_source_prem_id, 'CP-NUM', current_date) EO_TU_NUMS,
        cm_char_utils.get_prem_bigcharval(pr1.prem_id, 'DISTRICT', current_date) EO_DISTRICT,
        TRIM(pr1.postal) EO_POSTAL,
        cm_char_utils.get_prem_bigcharval(decode(pc.char_val , 'CALCULATED_GROUP', ll.cm_dest_prem_id, l.cm_dest_prem_id), 'F-ADR-1 ', current_date)||
        cm_char_utils.get_prem_bigcharval(decode(pc.char_val , 'CALCULATED_GROUP', ll.cm_dest_prem_id, l.cm_dest_prem_id), 'F-ADR-2 ', current_date) EO_ADDRESS,
        DAT_LIM.ADHOC_CHAR_VAL AS DAT_LIM,
        DAT_LIMP.ADHOC_CHAR_VAL AS DAT_LIMP,
        cm_dz_rep.get_prem_char_val(pr1.prem_id, 'PR-OUTAG') sw_in_dog,
        cm_char_utils.get_prem_bigcharval(l.cm_source_prem_id,'CP-NAME',current_date) AS EO_NAME
        from
         ci_wf_proc_char wf1
          inner join ci_wf_proc p on wf1.wf_proc_id = p.wf_proc_id
          LEFT  JOIN CI_WF_PROC_CHAR DAT_LIM ON DAT_LIM.WF_PROC_ID = wf1.wf_proc_id AND DAT_LIM.CHAR_TYPE_CD = 'DAT-LIM'
          LEFT  JOIN CI_WF_PROC_CHAR DAT_LIMP ON DAT_LIMP.WF_PROC_ID = wf1.wf_proc_id AND DAT_LIMP.CHAR_TYPE_CD = 'DAT-LIMP'
          inner join ci_prem pr1 on pr1.prem_id=trim(wf1.char_val_fk1) and wf1.char_type_cd='POINTDLV'
          inner join cm_prem_link l on pr1.prem_id = l.cm_source_prem_id and l.cm_prem_link_type_cd in ('EE_SUPPLY')
          left  join ci_prem_char pc on pc.prem_id = l.cm_dest_prem_id and pc.char_type_cd = 'SUP-P-TY'
          left  join cm_prem_link ll on ll.cm_source_prem_id = l.cm_dest_prem_id and ll.cm_prem_link_type_cd in ('EE_SUPPLY')
        where
          wf1.wf_proc_id=i_wf_proc_id
          and trunc(p.cre_dttm) between l.start_dt and nvl(l.end_dt , to_date('01.01.2100' , 'dd.mm.yyyy'))
          and (pc.effdt is null or  pc.effdt = (Select max(effdt) from ci_prem_char where prem_id = pc.prem_id and char_type_cd = pc.char_type_cd))
          )  order by rn)

    loop
      v_eo_rec.rn :=  eo1.rn;
      v_eo_rec.eo_id:= eo1.prem_id;
     -- v_eo_rec.eo_code:= trim(rpad(eo1.eo_code, 254));
      v_eo_rec.eo_name:= TRIM(rpad(eo1.eo_name, 254));
      v_eo_rec.eo_district:= trim(rpad(eo1.eo_district, 254));
      v_eo_rec.eo_postal:= trim(rpad(eo1.eo_postal, 254));
      v_eo_rec.eo_address:= trim(rpad(eo1.eo_address, 254));
      v_eo_rec.sw_in_dog:= eo1.sw_in_dog;
      v_eo_rec.DAT_LIM := EO1.DAT_LIM;
      v_eo_rec.DAT_LIMP := EO1.DAT_LIMP;
      v_eo_rec.EO_TU_NUMS := EO1.EO_TU_NUMS;
      pipe row(v_eo_rec);
    end loop;
  end get_tp_list_4_wf_proc_lim;

  --
 /* function get_eo_list_4_case_doc(i_case_id in char) return T_EO_SET pipelined is
    v_eo_rec t_eo_rec;
    v_wf_proc_id char(10);
  begin
    select wf_proc_id into v_wf_proc_id
    from cm_doc_case_vw where case_id = i_case_id;

  end get_eo_list_4_case_doc;  */

  function get_tfg_select(i_case_id in char) return T_TFG_SET pipelined is
    v_tfg_rec T_TFG_REC;
    v_dprt char(30);
    v_acct_id char(10);
    v_per_id char(10);
    v_per2_id char(10);

    v_per_ke_id char(10);
    v_per_mtu_id char(10);
    v_per_mchs_id char(10);
    amount1 number(15,2);
    amount2 number(15,2);
    amount_tfg number(15,2);
    amount_tfg2 number(15,2);
    cnt number(3);
  begin
    for tfg in (select dc1.case_id, dc1.doc_num, dc1.doc_dat,
                we1.wf_proc_id, we1.evt_seq, ac1.acct_id,
                ac1.dog_num, ac1.dog_dat, ac1.per_id main_per_id,
                /*ac1.main_name*/pc.adhoc_char_val main_name, ac1.main_postal, ac1.main_address,
                --wf1.officer_name,
                --wf1.wf_amount_rub, wf1.wf_amount_kop,
                cm_dz_chty.get_wfpr_chty(we1.wf_proc_id, 'DAT-TFG') DAT_TFG,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-OPLZ') DAT_OPLZ,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMS') DAT_LIMS,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMP') DAT_LIMP,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMT') DAT_LIMT,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMA') DAT_LIMA,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIM') DAT_LIM,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'TIM-LIM') TIM_LIM
                from
                  cm_doc_case_vw dc1
                  inner join cm_acct_vw ac1 on ac1.acct_id=dc1.acct_id
                 /*Novikova. TR-21515. Наименование потребителя берем с основного подразделения, т.к. в таблице cm_acct_vw размер поля слишком мал*/
                  inner join ci_acct_per ap on ap.acct_id = ac1.acct_id and ap.main_cust_sw = 'Y'
                  inner join ci_per_char pc on pc.per_id = ap.per_id and pc.char_type_cd = 'SHORT-NM'
                          and pc.effdt = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = pc.per_id and pc2.char_type_cd = pc.char_type_cd and pc2.effdt <= to_date(DC1.doc_dat,'dd.mm.yyyy'))
                                  inner join ci_case_char ch1 on ch1.case_id=dc1.case_id and ch1.char_type_cd='PCZ-EVT '
                  inner join ci_wf_evt we1 on we1.wf_proc_id=ch1.char_val_fk1 and we1.evt_seq=ch1.char_val_fk2
                  --inner join cm_wf_evt_vw we1 on we1.printdoc=dc1.case_id
                 -- inner join cm_wf_vw wf1 on wf1.wf_proc_id=we1.wf_proc_id
                where
                  dc1.case_id= i_case_id)

    loop
      begin
        select t1.adhoc_char_val
        into v_tfg_rec.denzadol
        from ci_acct_char t1
        where t1.acct_id= tfg.acct_id and t1.char_type_cd='DENZADOL' and t1.effdt=
        (select max(t2.effdt) from ci_acct_char t2 where t1.acct_id=t2.acct_id and t1.char_type_cd=t2.char_type_cd);
        --v_tfg_rec.denzadol:= '25';
      exception
        when no_data_found then v_tfg_rec.denzadol := '__';
      end;

      --
      -- Определение суммы
      select doc_amount_rub, doc_amount_kop
      into v_tfg_rec.wf_amount_rub , v_tfg_rec.wf_amount_kop
      from table(cm_dz_rep.get_doc_sum(i_case_id));
      --
      v_tfg_rec.wf_proc_id:= tfg.wf_proc_id;
      v_tfg_rec.evt_seq:= tfg.evt_seq;
      v_tfg_rec.cc_id:= cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'CONTACT');
      /*TR-18368. Novikova. Новая функция определения номеров телефонов для отчетных форм*/
      --v_tfg_rec.cc_tel:= cm_dz_chty.get_cc_chty(v_tfg_rec.cc_id, 'CCT-TEL');
      --v_tfg_rec.cc_fax:= cm_dz_chty.get_cc_chty(v_tfg_rec.cc_id, 'CCT-FAX');
      v_tfg_rec.cc_tel:= CM_DZ_CHTY.GET_ACCT_PHONE(tfg.acct_id,'TEL');
      v_tfg_rec.cc_fax:= CM_DZ_CHTY.GET_ACCT_PHONE(tfg.acct_id,'FAX');
      v_tfg_rec.cc_tel_other:= CM_DZ_CHTY.GET_ACCT_PHONE(tfg.acct_id,'OTHER');
      v_tfg_rec.case_id:= tfg.case_id;
      -- Определение района
      -- v_tfg_rec.district:=  cm_dz.get_per_chty(tfg.main_per_id, 'DISTRICT');
      with a1 as
      (select distinct  p2.adhoc_char_val district
      from
        ci_wf_proc wp1
        inner join ci_wf_proc_char wc1 on wp1.wf_proc_id=wc1.wf_proc_id and wc1.char_type_cd='OB-OBSL'
        inner join ci_prem p1 on p1.prem_id=wc1.char_val_fk1
        inner join ci_prem_char p2 on p1.prem_id=p2.prem_id and p2.char_type_cd='DISTRICT'
      where
        wp1.wf_proc_id=tfg.wf_proc_id)
      select trim(to_char(wmsys.wm_concat(' '||a1.district)))
      into v_tfg_rec.district
      from a1;
      --v_acct_id:= tfg.acct_id;

      v_tfg_rec.tfg_num:= tfg.doc_num; -- get_wf_proc_adhoc_char_val(tfg.wf_proc_id, 'NUM-TFG');
      v_tfg_rec.tfg_dat:= tfg.doc_dat; -- get_wf_proc_adhoc_char_val(tfg.wf_proc_id, 'DAT-TFG');
      v_tfg_rec.tfg_year:= to_char(to_date(v_tfg_rec.tfg_dat, 'dd.mm.yyyy'),'YYYY');
      v_tfg_rec.dog_num:= tfg.dog_num;
      v_tfg_rec.dog_dat:= tfg.dog_dat;

      v_tfg_rec.DAT_TFG:= tfg.DAT_TFG;
      v_tfg_rec.DAT_OPLZ:= tfg.DAT_OPLZ;
      v_tfg_rec.DAT_LIMS:= tfg.DAT_LIMS;
      v_tfg_rec.DAT_LIMP:= tfg.DAT_LIMP;
      v_tfg_rec.DAT_LIMT:= tfg.DAT_LIMT;
      v_tfg_rec.DAT_LIMA:= tfg.DAT_LIMA;
      v_tfg_rec.DAT_LIM:= tfg.DAT_LIM;
      v_tfg_rec.TIM_LIM:= tfg.TIM_LIM;
      -- исполнитель и его телефон
      select executor_name, executor_tel
      into v_tfg_rec.executor_name, v_tfg_rec.executor_tel
      from table(cm_dz_rep.get_doc_head(i_case_id));
      --
      v_tfg_rec.main_name:= tfg.main_name;
      v_tfg_rec.main_postal:= tfg.main_postal;
      v_tfg_rec.main_address:= tfg.main_address;
      v_tfg_rec.pt_list:= get_pt_list(tfg.wf_proc_id); -- cm_fd.fd4wf(tfg.wf_proc_id);
      --
      select count(*) into cnt
      from ci_wf_evt_char t1 where t1.wf_proc_id=tfg.wf_proc_id and t1.evt_seq=tfg.evt_seq and t1.char_type_cd='NTF-AMT';
      if 1=cnt then
        amount1:= coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT'), '999999999999990.99'),0);
        amount2:= 0; --coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMNT-CUR'), '999999999999990.99'), 0);
        amount_tfg:= amount1+amount2;
        amount_tfg2:= coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT0'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT2'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT8'), '999999999999990.99'),0);

        v_tfg_rec.wf_amount_rub:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf_amount_kop:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'KOP');
        v_tfg_rec.wf2_amount_rub:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf2_amount_kop:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'KOP');
      else
        amount1:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'NTF-AMT'), '999999999999990.99'),0);
        amount2:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMNT-CUR'), '999999999999990.99'), 0);
        amount_tfg:= amount1+amount2;
        amount_tfg2:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT0'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT2'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT8'), '999999999999990.99'),0);

        v_tfg_rec.wf_amount_rub:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf_amount_kop:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'KOP');
        v_tfg_rec.wf2_amount_rub:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf2_amount_kop:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'KOP');
      end if;

      --amount1:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMOUNT'), '999999999999990.99'),0);
      --amount2:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMNT-CUR'), '999999999999990.99'), 0);
      --amount_tfg:= amount1+amount2;
      --amount_tfg2:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT0'), '999999999999990.99'),0)
      --+coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT2'), '999999999999990.99'),0)
      --+coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT8'), '999999999999990.99'),0);

      --v_tfg_rec.wf_amount_rub:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'RUB');
      --v_tfg_rec.wf_amount_kop:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'KOP');
      --v_tfg_rec.wf2_amount_rub:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'RUB');
      --v_tfg_rec.wf2_amount_kop:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'KOP');
      --
      /*TR-19854. Novikova. Закомментарила и ниже находим по новой функции*/
      --v_tfg_rec.signer_job:= get_tfg_signer_job(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'DZ-TPMNG'), '9900000196');
      --v_tfg_rec.signer_name:= get_tfg_signer_name(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'DZ-TPMNG'), '9900000196');
        v_tfg_rec.signer_name:=CM_DZ_CHTY.GET_PERSON_PSK(i_case_id,'FIO',to_date(tfg.doc_dat,'dd.mm.yyyy'));
        v_tfg_rec.signer_job:=CM_DZ_CHTY.GET_PERSON_PSK(i_case_id,'STAFFPOS',to_date(tfg.doc_dat,'dd.mm.yyyy'));
      -- Первое ограничение
      if cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')= 'Y' and
         cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'PRIL4530')= 'N'
      then
        v_tfg_rec.txt_limit1:= ' до уровня аварийной брони';
      elsif cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')= 'Y'
      then
        v_tfg_rec.txt_limit1:= ' до уровня технологической брони';
      else
        v_tfg_rec.txt_limit1:= '';
      end if;
      -- Второе ограничение
      if cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')= 'Y' and
         cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'PRIL4530')= 'Y'
      then
        v_tfg_rec.txt_limit2:= ' ограничена до аварийной брони';
        v_tfg_rec.txt_limit3:= ' ограничением';
      else
        v_tfg_rec.txt_limit2:= ' полностью прекращена';
        v_tfg_rec.txt_limit3:= ' прекращением';
      end if;
      -- ***********************************************************************************************
      if cm_dz.get_wfpr_chty(tfg.wf_proc_id, 'DZ-DPRT') in ('DPT-BIG', 'DPT-ZHKH', 'DPT-BUDJ') then
        v_tfg_rec.head_text1:= 'От _____________        №  ___________________';
        v_tfg_rec.head_text2:= 'На №_______________от____________________';
      else
        v_tfg_rec.head_text1:= '';
        v_tfg_rec.head_text2:= '';
      end if;
      -- ************************************************************************************************
      begin
        select pc1.per_id into v_per_id
        from ci_per_char pc1 inner join ci_per_char pc2 on pc1.per_id=pc2.per_id
        where
          pc1.char_type_cd='DISTRICT' and pc1.adhoc_char_val='Василеостровский район' and
          pc2.char_type_cd='TYPE-OIV' and pc2.char_val='HEAD-ADMIN';
      exception
        when no_data_found then v_per_id := null;
      end;
      --
      begin
        select pc1.per_id into v_per2_id
        from ci_per_char pc1 inner join ci_per_char pc2 on pc1.per_id=pc2.per_id
        where
          pc1.char_type_cd='DISTRICT' and pc1.adhoc_char_val='Василеостровский район' and
          pc2.char_type_cd='TYPE-OIV' and pc2.char_val='PROKUROR';
      exception
          when no_data_found then v_per2_id := null;
      end;
    /*  select pc1.per_id into v_per_id
      from ci_per_char pc1 inner join ci_per_char pc2 on pc1.per_id=pc2.per_id
      where
        pc1.char_type_cd='DISTRICT' and pc1.adhoc_char_val='Василеостровский район' and
        pc2.char_type_cd='TYPE-OIV' and pc2.char_val='HEAD-ADMIN';

      select pc1.per_id into v_per2_id
      from ci_per_char pc1 inner join ci_per_char pc2 on pc1.per_id=pc2.per_id
      where
        pc1.char_type_cd='DISTRICT' and pc1.adhoc_char_val='Василеостровский район' and
        pc2.char_type_cd='TYPE-OIV' and pc2.char_val='PROKUROR';*/
      --
      v_per_ke_id:= cm_dz_chty.get_per_id_char_val('TYPE-OIV', 'KOMITET-ENERGO');
      v_per_mtu_id:= cm_dz_chty.get_per_id_char_val('TYPE-OIV', 'MTU-RTN');
      v_per_mchs_id:= cm_dz_chty.get_per_id_char_val('TYPE-OIV', 'GU-MCHS');
      --
      if cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, '4-ADM-RN')='Y' then
      v_tfg_rec.head_text3:=
      'Главе администрации '||cm_dz.get_per_chty(v_per_id, 'DISTR-DP')||' района Санк-Петербурга'||chr(13)||
      'ФИО: '||cm_dz.get_per_chty(v_per_id, 'MNG-DTPD')||chr(13)||
      'адрес: '||cm_dz.get_per_chty(v_per_id, 'ADRES-SU')||chr(13)||
      'тел.  '||cm_dz.get_per_phone(v_per_id, 'RAB')||'  факс '||cm_dz.get_per_phone(v_per_id, 'FAKS')||chr(13)||
      'Прокурору '||cm_dz.get_per_chty(v_per_id, 'DISTR-DP')||' района Санк-Петербурга'||chr(13)||
      'ФИО: '||cm_dz.get_per_chty(v_per2_id, 'MNG-DTPD')||chr(13)||
      'адрес: '||cm_dz.get_per_chty(v_per2_id, 'ADRES-SU')||chr(13)||
      'тел.  '||cm_dz.get_per_phone(v_per2_id, 'RAB')||'  факс '||cm_dz.get_per_phone(v_per2_id, 'FAKS')||chr(13)||
      'Председателю комитета по энергетике и инженерному обеспечению Правительства Санкт-Петербурга'||chr(13)||
      'ФИО: '||cm_dz.get_per_chty(v_per_ke_id, 'MNG-DTPD')||chr(13)||
      '190000, СПб, пер.Антоненко д.4,'||chr(13)||
      'ф.576-59-88, т.576-58-65'||chr(13)||
      'Начальнику МТУ Ростехнадзора по СЗФО'||chr(13)||
      'ФИО: '||cm_dz.get_per_chty(v_per_mtu_id, 'MNG-DTPD')||chr(13)||
      '199048, СПб, 10-я Линия В.О.д.51, '||chr(13)||
      'ф.321-49-88, т.321-89-88, 273-33-94'||chr(13)||
      'Начальнику Главного управления МЧС России по Санкт-Петербургу'||chr(13)||
      'ФИО: '||cm_dz.get_per_chty(v_per_mchs_id, 'MNG-DTPD')||chr(13)||
      '190000, СПб, наб. р.Мойки, 85'||chr(13)||
      'ф. 315-73-30, т. 718-25-05, 718-25-00'||chr(13);
      end if;
      -- ********************************************************************
      v_tfg_rec.DAYTOLIM:= abs(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'DAYTOLIM')));
      --v_tfg_rec.DZ_DPRT:= cm_dz.get_wfpr_chty(tfg.wf_proc_id, 'DZ-DPRT'
      --select * from get_
      --select char_val_fk1 into
      --from wf_proc_char where wf_proc_id= '' and char_type_cd='DZ-DPRT'
      --
      pipe row(v_tfg_rec);
    end loop;
  end get_tfg_select;
  --
 /*dresvyannikov. TR-19675. Ф-ия используется только для отчетов "Предпупредительная тф(дог,бездог)"*/
  function get_tfg_select_new(i_case_id in char) return T_TFG_NEW_SET pipelined is
    v_tfg_rec T_TFG_NEW_REC;

    v_per_id char(10);
    v_per2_id char(10);

    v_per_ke_id char(10);
    v_per_mtu_id char(10);
    v_per_mchs_id char(10);
    amount1 number(15,2);
    amount2 number(15,2);
    amount_tfg number(15,2);
    amount_tfg2 number(15,2);
    cnt number(3);
    V_CNT number;
    v_cnt_cp number;
    v_cnt_ik_trans number;
  begin
    for tfg in (select dc1.case_id, dc1.doc_num, dc1.doc_dat,
                we1.wf_proc_id, we1.evt_seq, ac1.acct_id,
                ac1.dog_num, ac1.dog_dat, ac1.per_id main_per_id,
                pc.adhoc_char_val main_name, ac1.main_postal,

                decode(trim(ac1.main_postal) , null , null , trim(ac1.main_postal) || ', ') ||
                cm_dz.get_acct_chty(ac1.acct_id,  'F-ADR-1 ') ||
                cm_dz.get_acct_chty(ac1.acct_id,  'F-ADR-2 ') main_address,
                cm_dz_chty.get_wfpr_chty(we1.wf_proc_id, 'DAT-TFG') DAT_TFG,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-OPLZ') DAT_OPLZ,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMS') DAT_LIMS,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMP') DAT_LIMP,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMT') DAT_LIMT,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMA') DAT_LIMA,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIM')  DAT_LIM,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'TIM-LIM')  TIM_LIM,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-HAND') DAT_HAND,
                DISTRICT.adhoc_char_val as district_nm
                from
                  cm_doc_case_vw dc1
                  inner join cm_acct_vw ac1 on ac1.acct_id=dc1.acct_id
                  inner join ci_acct_per ap on ap.acct_id = ac1.acct_id and ap.main_cust_sw = 'Y'
                  inner join ci_per_char pc on pc.per_id = ap.per_id and pc.char_type_cd = 'SHORT-NM'
                          and pc.effdt = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = pc.per_id and pc2.char_type_cd = pc.char_type_cd and pc2.effdt <= to_date(DC1.doc_dat,'dd.mm.yyyy'))
                                  inner join ci_case_char ch1 on ch1.case_id=dc1.case_id and ch1.char_type_cd='PCZ-EVT '
                  inner join ci_wf_evt we1 on we1.wf_proc_id=ch1.char_val_fk1 and we1.evt_seq=ch1.char_val_fk2
                 left join ci_per_char DISTRICT on ap.per_id = DISTRICT.PER_ID and DISTRICT.CHAR_TYPE_CD = 'DISTRICT'
                where
                  dc1.case_id= i_case_id)

    loop
      begin
        select t1.adhoc_char_val
         into v_tfg_rec.denzadol
        from ci_acct_char t1
        where t1.acct_id= tfg.acct_id and t1.char_type_cd='DENZADOL' and t1.effdt=
        (select max(t2.effdt) from ci_acct_char t2 where t1.acct_id=t2.acct_id and t1.char_type_cd=t2.char_type_cd);
      exception
        when no_data_found then v_tfg_rec.denzadol := '__';
      end;

      --
      -- Определение суммы
      select doc_amount_rub, doc_amount_kop
      into v_tfg_rec.wf_amount_rub , v_tfg_rec.wf_amount_kop
      from table(cm_dz_rep.get_doc_sum(i_case_id));
      --
      v_tfg_rec.wf_proc_id := tfg.wf_proc_id;
      v_tfg_rec.evt_seq:= tfg.evt_seq;
      v_tfg_rec.cc_id:= cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'CONTACT');
      /*TR-18368. Novikova. Новая функция определения номеров телефонов для отчетных форм*/
      v_tfg_rec.cc_tel:= CM_DZ_CHTY.GET_ACCT_PHONE(tfg.acct_id,'TEL');
      v_tfg_rec.cc_fax:= CM_DZ_CHTY.GET_ACCT_PHONE(tfg.acct_id,'FAX');
      v_tfg_rec.cc_tel_other:= CM_DZ_CHTY.GET_ACCT_PHONE(tfg.acct_id,'OTHER');
      v_tfg_rec.case_id:= tfg.case_id;
      -- Определение района
      with a1 as
      (select distinct  p2.adhoc_char_val district
      from
        ci_wf_proc wp1
        inner join ci_wf_proc_char wc1 on wp1.wf_proc_id=wc1.wf_proc_id and wc1.char_type_cd='OB-OBSL'
        inner join ci_prem p1 on p1.prem_id=wc1.char_val_fk1
        inner join ci_prem_char p2 on p1.prem_id=p2.prem_id and p2.char_type_cd='DISTRICT'
      where
        wp1.wf_proc_id=tfg.wf_proc_id)
      select trim(to_char(wmsys.wm_concat(' '||a1.district)))
      into v_tfg_rec.district
      from a1;

      v_tfg_rec.tfg_num:= tfg.doc_num;
      v_tfg_rec.tfg_dat:= tfg.doc_dat;
      v_tfg_rec.tfg_year:= to_char(to_date(v_tfg_rec.tfg_dat, 'dd.mm.yyyy'),'YYYY');
      v_tfg_rec.dog_num:= tfg.dog_num;
      v_tfg_rec.dog_dat:= tfg.dog_dat;

      v_tfg_rec.DAT_TFG:= tfg.DAT_TFG;
      v_tfg_rec.DAT_OPLZ:= tfg.DAT_OPLZ;
      v_tfg_rec.DAT_LIMS:= tfg.DAT_LIMS;
      v_tfg_rec.DAT_LIMP:= NVL(tfg.DAT_LIMP , '_______________');
      v_tfg_rec.DAT_LIMT:= tfg.DAT_LIMT;
      v_tfg_rec.DAT_LIMA:= tfg.DAT_LIMA;
      --v_tfg_rec.DAT_LIM:= tfg.DAT_LIM;
      --v_tfg_rec.TIM_LIM:= tfg.TIM_LIM;
      -- исполнитель и его телефон
      select executor_name, executor_tel
      into v_tfg_rec.executor_name, v_tfg_rec.executor_tel
      from table(cm_dz_rep.get_doc_head(i_case_id));
      --
      v_tfg_rec.main_name:= tfg.main_name;
      v_tfg_rec.main_postal:= tfg.main_postal;
      v_tfg_rec.main_address:= tfg.main_address;
      v_tfg_rec.pt_list:= get_pt_list(tfg.wf_proc_id); -- cm_fd.fd4wf(tfg.wf_proc_id);
      --
      select count(*) into cnt
      from ci_wf_evt_char t1 where t1.wf_proc_id=tfg.wf_proc_id and t1.evt_seq=tfg.evt_seq and t1.char_type_cd='NTF-AMT';
      if 1=cnt then
        amount1:= coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT'), '999999999999990.99'),0);
        amount2:= 0; --coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMNT-CUR'), '999999999999990.99'), 0);
        amount_tfg:= amount1+amount2;
        amount_tfg2:= coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT0'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT2'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT8'), '999999999999990.99'),0);

        v_tfg_rec.wf_amount_rub:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf_amount_kop:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'KOP');
        v_tfg_rec.wf2_amount_rub:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf2_amount_kop:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'KOP');
      else
        amount1:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'NTF-AMT'), '999999999999990.99'),0);
        amount2:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMNT-CUR'), '999999999999990.99'), 0);
        amount_tfg:= amount1+amount2;
        amount_tfg2:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT0'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT2'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT8'), '999999999999990.99'),0);

        v_tfg_rec.wf_amount_rub:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf_amount_kop:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'KOP');
        v_tfg_rec.wf2_amount_rub:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf2_amount_kop:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'KOP');
      end if;

      select max(nm.entity_name) ,max(dlz.adhoc_char_val)
        into v_tfg_rec.SIGNER_NAME,v_tfg_rec.SIGNER_JOB
           from ci_case_char t
      inner join ci_case_char r on trim(t.char_val_fk1) = r.case_id and trim(r.char_type_cd) = 'SIGNERID'
      left  join ci_per_name nm on nm.per_id = trim(r.char_val_fk1) and nm.name_type_flg = 'PRIM'
      left join CI_PER_CHAR dlz on dlz.per_id = trim(r.char_val_fk1) and dlz.char_type_cd = 'DL$N-RUK'
            where t.case_id = i_case_id
         and trim(t.char_type_cd) = 'REESTR';



      -- ***********************************************************************************************
      if cm_dz.get_wfpr_chty(tfg.wf_proc_id, 'DZ-DPRT') in ('DPT-BIG', 'DPT-ZHKH', 'DPT-BUDJ') then
        v_tfg_rec.head_text1:= 'От _____________        №  ___________________';
        v_tfg_rec.head_text2:= 'На №_______________от____________________';
      else
        v_tfg_rec.head_text1:= '';
        v_tfg_rec.head_text2:= '';
      end if;
      -- ************************************************************************************************
      begin
        select pc1.per_id into v_per_id
        from ci_per_char pc1 inner join ci_per_char pc2 on pc1.per_id=pc2.per_id
        where
          pc1.char_type_cd='DISTRICT' and pc1.adhoc_char_val='Василеостровский район' and
          pc2.char_type_cd='TYPE-OIV' and pc2.char_val='HEAD-ADMIN';
      exception
        when no_data_found then v_per_id := null;
      end;
      --
      begin
        select pc1.per_id into v_per2_id
        from ci_per_char pc1 inner join ci_per_char pc2 on pc1.per_id=pc2.per_id
        where
          pc1.char_type_cd='DISTRICT' and pc1.adhoc_char_val='Василеостровский район' and
          pc2.char_type_cd='TYPE-OIV' and pc2.char_val='PROKUROR';
      exception
          when no_data_found then v_per2_id := null;
      end;
      v_per_ke_id:= cm_dz_chty.get_per_id_char_val('TYPE-OIV', 'KOMITET-ENERGO');
      v_per_mtu_id:= cm_dz_chty.get_per_id_char_val('TYPE-OIV', 'MTU-RTN');
      v_per_mchs_id:= cm_dz_chty.get_per_id_char_val('TYPE-OIV', 'GU-MCHS');

       select COUNT(*)
        into v_cnt_cp
            from cm_eo_account_linkage t
      inner join cm_prem_link l on t.cm_eo_id = l.cm_dest_prem_id
      inner join ci_prem cp on cp.prem_id = l.cm_source_prem_id
       where t.acct_id = tfg.acct_id
         and sysdate between t.start_dt and nvl(t.end_dt , to_date('01.01.2100' , 'dd.mm.yyyy'))
         and cp.prem_type_cd = 'CP';

       select COUNT(*)
         into v_cnt_ik_trans
            from cm_eo_account_linkage t
      inner join cm_prem_link l on t.cm_eo_id = l.cm_dest_prem_id
      inner join ci_prem cp on cp.prem_id = l.cm_source_prem_id
       where t.acct_id = tfg.acct_id
         and sysdate between t.start_dt and nvl(t.end_dt , to_date('01.01.2100' , 'dd.mm.yyyy'))
         and cp.prem_type_cd = 'IK-TRANS';


      /*Выбор шаблона*/ --доп.шаблон
      if (cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='N' and
          cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='N' and
          v_cnt_cp = 1 and
          v_cnt_ik_trans = 0) then
       v_tfg_rec.HEAD_ABTB := '';
       v_tfg_rec.TXT_TFG1 :=  'самостоятельно, путём снижения объёма электропотребления, ввести режим частичного ограничения потребления электроэнергии.
         Срок введения самостоятельного частичного ограничения: 10:00 часов ' ||  tfg.DAT_LIMS || ' г.' ;

       v_tfg_rec.txt_limit1 := 'полное ограничение' ;
       v_tfg_rec.txt_limit2:= null;
       v_tfg_rec.TXT_LIMIT3 := '';
       v_tfg_rec.TXT_TFG2 := '';
       v_tfg_rec.TXT_TFG4 := 'и в связи с отсутствием технической возможности введения частичного ограничения режима потребления э/э с питающих центров, ';

      --1 шаблон
      elsif (cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='N' and
          cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='N') then
       v_tfg_rec.HEAD_ABTB := '';
       v_tfg_rec.TXT_TFG1 :=  'самостоятельно, путём снижения объёма электропотребления, ввести режим частичного ограничения потребления электроэнергии.
         Срок введения самостоятельного частичного ограничения: 10:00 часов ' ||  tfg.DAT_LIMS || ' г.' ;

       v_tfg_rec.txt_limit1 := 'частичное ограничение' ;
       v_tfg_rec.txt_limit2:= null;
       v_tfg_rec.TXT_LIMIT3 := '';
       v_tfg_rec.TXT_TFG2 := '';
       v_tfg_rec.TXT_TFG3 := '       В дальнейшем, при неоплате сложившейся задолженности полном объёме и неисполнении обязательств по  предварительной оплате электрической энергии до 10:00 ' || TFG.DAT_LIM || ' г. , Вашему предприятию после 10:00 часов ' || TFG.DAT_LIM || ' г. будет введён режим полного ограничения потребления электроэнергии путём отключения всех энергопринимающих устройств (объектов).';
 --2 шаблон
      elsif ((cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='Y' or
             cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y') and
             cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'PRIL4530')= 'N') then
       v_tfg_rec.HEAD_ABTB := ' до уровня технологической и/или аварийной брони';
       v_tfg_rec.TXT_TFG1 := 'c 10:00 часов ' || tfg.DAT_LIMS || ' г. самостоятельно, путём снижения объёма электропотребления, ввести режим ограничения потребления электроэнергии до уровня технологической брони.';
       v_tfg_rec.txt_limit1 := 'ограничение' ;
       v_tfg_rec.txt_limit2:= 'до уровня технологической брони с питающих центров, ';
       if  cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y' then
        v_tfg_rec.TXT_TFG2:= 'При неоплате сложившейся задолженности в полном объёме Вашему предприятию после 10:00 ' ||  NVL(TO_CHAR((TO_DATE(TFG.DAT_HAND,'DD.MM.YYYY')+16),'DD.MM.YYYY') , '__________________') || ' г. будет введен режим ограничения потребления электроэнергии до уровня аварийной брони, путем отключения энергопринимающих устройств(объектов):';
        for rec in (Select * from table(get_tp_list_4_wf_proc(tfg.wf_proc_id))) loop
        v_tfg_rec.TXT_TFG2:= v_tfg_rec.TXT_TFG2 || chr(13) || '         ' || rec.EO_CODE || ' , ' || rec.EO_ADDRESS || ' , ' || rec.EO_NAME ;
         --Проверка наличия аварийной брони в действующем акте АБ/ТБ.
         --Пока убираем - в дальнейшем будет нужна
/*         SELECT COUNT(*)
           INTO V_CNT
         from TABLE(cm_char_utils.sel_akt_ab_tb(rec.EO_ID, to_date(tfg.dat_lim,'DD.MM.YYYY'))) T
                   inner join CM_RESERVATION R on T.DOC_ID = R.CM_RESERVATION_DOCUMENT_ID AND TRIM(R.CM_RESERVATION_TYPE_CD) = 'EMERGENCY'
                   INNER JOIN CM_RESERVATION_TOT_VALUE RT ON R.CM_RESERVATION_ID = RT.CM_RESERVATION_ID
             WHERE (RT.CM_POWER_VALUE > 0 OR RT.CM_ENERGY_VALUE > 0)
               AND TO_NUMBER(TO_CHAR(TO_DATE(tfg.dat_lim,'DD.MM.YYYY'), 'MM')) BETWEEN
                 RT.CM_START_MONTH AND RT.CM_END_MONTH;
              if v_cnt>0 then
              v_tfg_rec.TXT_TFG2:= v_tfg_rec.TXT_TFG2 || chr(13) || '         ' || rec.EO_CODE || ' , ' || rec.EO_ADDRESS || ' , ' || rec.EO_NAME ;
              end if;
*/          end loop;
       end if;
       v_tfg_rec.TXT_TFG3 := '       В дальнейшем, при неоплате сложившейся задолженности в полном объёме до 10:00 ' ||  TFG.DAT_LIM || ' г., Вашему предприятию после 10:00 часов ' || TFG.DAT_LIM || ' г. будет введён режим полного ограничения потребления электроэнергии путём отключения всех энергопринимающих устройств (объектов).';

      --3 шаблон
       elsif ((cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='Y' or
             cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y') and
             cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'PRIL4530')= 'Y') then
      v_tfg_rec.HEAD_ABTB := ' до уровня технологической и/или аварийной брони';
     v_tfg_rec.TXT_TFG1 := 'c 10:00 часов ' || tfg.DAT_LIMS || ' г. самостоятельно, путём снижения объёма электропотребления, ввести режим частичного ограничения потребления электроэнергии до уровня технологической брони.';
     v_tfg_rec.txt_limit1 := 'ограничение' ;
     v_tfg_rec.txt_limit2:= 'до уровня технологической брони с питающих центров, ';
     if  cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y' then
       v_tfg_rec.TXT_TFG2:= '       В дальнейшем, при неоплате сложившейся задолженности в полном объеме, Вашему предприятию после 10:00 часов '
       || tfg.DAT_LIM || ' будет введен режим ограничения потребление электроэнергии до уровня аварийной брони, путем отключения энергопринимающих устройств(объектов):';
     for rec in (Select * from table(get_tp_list_4_wf_proc(tfg.wf_proc_id))) loop
     v_tfg_rec.TXT_TFG2:= v_tfg_rec.TXT_TFG2 || chr(13) || '         ' || rec.EO_CODE || ' , ' || rec.EO_ADDRESS || ' , ' || rec.EO_NAME ;
         --Проверка наличия аварийной брони в действующем акте АБ/ТБ
         --Пока убираем - в дальнейшем будет нужна
      /*   SELECT COUNT(*)
           INTO V_CNT
         from TABLE(cm_char_utils.sel_akt_ab_tb(rec.EO_ID, to_date(tfg.dat_lim,'DD.MM.YYYY'))) T
                   inner join CM_RESERVATION R on T.DOC_ID = R.CM_RESERVATION_DOCUMENT_ID AND TRIM(R.CM_RESERVATION_TYPE_CD) = 'EMERGENCY'
                   INNER JOIN CM_RESERVATION_TOT_VALUE RT ON R.CM_RESERVATION_ID = RT.CM_RESERVATION_ID
             WHERE (RT.CM_POWER_VALUE > 0 OR RT.CM_ENERGY_VALUE > 0)
               AND TO_NUMBER(TO_CHAR(TO_DATE(tfg.dat_lim,'DD.MM.YYYY'), 'MM')) BETWEEN
                 RT.CM_START_MONTH AND RT.CM_END_MONTH;
              if v_cnt>0 then
              v_tfg_rec.TXT_TFG2:= v_tfg_rec.TXT_TFG2 || chr(13) || '         ' || rec.EO_CODE || ' , ' || rec.EO_ADDRESS || ' , ' || rec.EO_NAME ;
              end if;
         */ end loop;
     end if;
      v_tfg_rec.TXT_TFG3 := '';
      end if;

      if (cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='Y' or
          cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y') then

      v_tfg_rec.head_text3 := '';

      for rec in (Select chr(13) ||
                         decode(DL$N_DP.Adhoc_Char_Val,'UNDEF',NULL,DL$N_DP.Adhoc_Char_Val) || chr(13) || -- Должность руководителя в дательном падеже
                         decode(FIO_DP.Adhoc_Char_Val,'UNDEF',NULL,FIO_DP.Adhoc_Char_Val) || chr(13) ||  --ФИО руководителя в дательном падеже
                         decode(F_ADR_1.ADHOC_CHAR_VAL,'UNDEF',NULL,F_ADR_1.ADHOC_CHAR_VAL) || ', индекс ' || hs.ind ||
                         chr(10) || 'ф.' || nvl(FAX.PHONE , '____________') ||
                         ', т.' || nvl(TEL_SEC.Phone || decode(TEL_SEC.Phone,NULL,'', decode(TEL_DEG.Phone, NULL, '' , ' , ')),'____________') as str
                  from ci_per_char ROLE_TYP
                  join ci_per_char ADM_DISTR on ROLE_TYP.Per_Id = ADM_DISTR.Per_Id and ADM_DISTR.CHAR_TYPE_CD = 'DISTRICT'
             left join ci_per_char GOV_DIST on GOV_DIST.Per_Id = ROLE_TYP.Per_Id and GOV_DIST.CHAR_TYPE_CD = 'GOV_DIST'
             left join ci_per_char GOV_AT on GOV_AT.Per_Id = ROLE_TYP.Per_Id and GOV_AT.CHAR_TYPE_CD = 'GOV_AT'
             left join ci_per_char GOV_ORD on GOV_ORD.Per_Id = ROLE_TYP.Per_Id and GOV_ORD.CHAR_TYPE_CD = 'GOV_ORD' --очередность вывода
             left join ci_per_char DL$N_DP  on DL$N_DP.PER_ID = ROLE_TYP.per_id and DL$N_DP.CHAR_TYPE_CD = 'DL$N-DP'
             left join ci_per_char FIO_DP on FIO_DP.PER_ID = ROLE_TYP.per_id and FIO_DP.CHAR_TYPE_CD = 'FIO-DP'
             left join ci_per_char F_ADR_1 on F_ADR_1.PER_ID = ROLE_TYP.per_id and F_ADR_1.CHAR_TYPE_CD = 'F-ADR-1'
             left join ci_per_char HOUSE_ID on HOUSE_ID.PER_ID = ROLE_TYP.PER_ID and HOUSE_ID.Char_Type_Cd = 'HOUSE-ID'
             left join k_house hs on trim(hs.kodd) = trim(HOUSE_ID.Adhoc_Char_Val)
             left join ci_per_phone FAX on FAX.per_id = ROLE_TYP.per_id and FAX.phone_type_cd = 'FAKS'
             left join ci_per_phone TEL_SEC on TEL_SEC.per_id = ROLE_TYP.per_id and TEL_SEC.phone_type_cd in ('TEL_SEC')
             left join ci_per_phone TEL_DEG on TEL_DEG.per_id = ROLE_TYP.per_id and TEL_DEG.phone_type_cd in ('TEL_DEG')
             left join ci_per_char IS_WAST ON IS_WAST.PER_ID = ROLE_TYP.PER_ID AND TRIM(IS_WAST.CHAR_TYPE_CD) = 'IS-WAST'
                  where ROLE_TYP.CHAR_TYPE_CD = 'ROLE-TYP'
                    and ROLE_TYP.Char_Val_Fk1 = 'ADM-DIST'  --субъекты роли "Адм.районы"
                    and(ADM_DISTR.Adhoc_Char_Val in (Select DISTRICT.Adhoc_Char_Val
                                                                 from ci_wf_proc_char proc
                                                                 join ci_prem_char DISTRICT on proc.char_val_fk1 = DISTRICT.Prem_Id
                                                           where proc.wf_proc_id = tfg.wf_proc_id
                                                             and proc.char_type_cd = 'OB-OBSL'
                                                             and DISTRICT.Char_Type_Cd = 'DISTRICT') --районы ЭО
                     or nvl(trim(GOV_DIST.Char_Val) , 'LOCAL') = 'ALL')
                    and nvl(trim(GOV_AT.Char_Val) , 'N') = 'Y'
                    and ADM_DISTR.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'DISTRICT' and per_id = ROLE_TYP.per_id)
                    and (F_ADR_1.Effdt is null or F_ADR_1.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'F-ADR-1' and per_id = ROLE_TYP.per_id))
                    and (DL$N_DP.Effdt is null or DL$N_DP.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'DL$N-DP' and per_id = ROLE_TYP.per_id))
                    and (HOUSE_ID.Effdt is null or HOUSE_ID.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'HOUSE-ID' and per_id = HOUSE_ID.per_id))
                    and (IS_WAST.Effdt is null or IS_WAST.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = IS_WAST.Char_Type_Cd and per_id = ROLE_TYP.per_id))
                    AND nvl(TRIM(IS_WAST.CHAR_VAL) , 'N') <> 'Y'
                  order by (nvl(GOV_ORD.Adhoc_Char_Val,0))  ) loop

      v_tfg_rec.head_text3:=v_tfg_rec.head_text3 || chr(13) || rec.str;

      end loop;

      elsif (cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, '4-ADM-RN')='Y') then

      v_tfg_rec.head_text3:='';
      for rec in (Select  chr(13) ||
                          decode(DL$N_DP.Adhoc_Char_Val,'UNDEF',NULL,DL$N_DP.Adhoc_Char_Val) || chr(13) || -- Должность руководителя в дательном падеже
                          decode(FIO_DP.Adhoc_Char_Val,'UNDEF',NULL,FIO_DP.Adhoc_Char_Val) || chr(13) ||  --ФИО руководителя в дательном падеже
                          decode(F_ADR_1.ADHOC_CHAR_VAL,'UNDEF',NULL,F_ADR_1.ADHOC_CHAR_VAL) || ', индекс ' || hs.ind ||
                          chr(10) || 'ф.' || nvl(FAX.PHONE , '____________') || ', т.' ||
                          nvl(TEL_SEC.Phone || decode(TEL_SEC.Phone,NULL,'', decode(TEL_DEG.Phone, NULL, '' , ' , ')),'____________') as str  -- Адрес субъекта
                   from ci_per per --потребитель
              left join ci_per_char PLATSP on per.per_id = PLATSP.Per_Id and PLATSP.Char_Type_Cd = 'PLATSP' --платежеспособность
                   --органы исп.власти
                   join ci_per_char ROLE_TYP on ROLE_TYP.CHAR_TYPE_CD = 'ROLE-TYP' and ROLE_TYP.Char_Val_Fk1 = 'ADM-DIST'  --субъекты роли "Адм.районы"
                   join ci_per_char ADM_DISTR on ROLE_TYP.Per_Id = ADM_DISTR.Per_Id and ADM_DISTR.CHAR_TYPE_CD = 'DISTRICT'
              left join ci_per_char DL$N_DP  on DL$N_DP.PER_ID = ROLE_TYP.per_id and DL$N_DP.CHAR_TYPE_CD = 'DL$N-DP'
              left join ci_per_char FIO_DP on FIO_DP.PER_ID = ROLE_TYP.per_id and FIO_DP.CHAR_TYPE_CD = 'FIO-DP'
              left join ci_per_char F_ADR_1 on F_ADR_1.PER_ID = ROLE_TYP.per_id and F_ADR_1.CHAR_TYPE_CD = 'F-ADR-1'
              left join ci_per_char GOV_DIST on GOV_DIST.Per_Id = ROLE_TYP.Per_Id and GOV_DIST.CHAR_TYPE_CD = 'GOV_DIST'
              left join ci_per_char GOV_ORD on GOV_ORD.Per_Id = ROLE_TYP.Per_Id and GOV_ORD.CHAR_TYPE_CD = 'GOV_ORD' --очередность вывода
              left join ci_per_char GOV_PR0 on GOV_PR0.PER_ID = ROLE_TYP.per_id and GOV_PR0.CHAR_TYPE_CD = 'GOV_PR0'
              left join ci_per_char GOV_PR1 on GOV_PR1.PER_ID = ROLE_TYP.per_id and GOV_PR1.CHAR_TYPE_CD = 'GOV_PR1'
              left join ci_per_char GOV_PR2 on GOV_PR2.PER_ID = ROLE_TYP.per_id and GOV_PR2.CHAR_TYPE_CD = 'GOV_PR2'
              left join ci_per_char GOV_PR3 on GOV_PR3.PER_ID = ROLE_TYP.per_id and GOV_PR3.CHAR_TYPE_CD = 'GOV_PR3'
              left join ci_per_char GOV_PR4 on GOV_PR4.PER_ID = ROLE_TYP.per_id and GOV_PR4.CHAR_TYPE_CD = 'GOV_PR4'
              left join ci_per_char GOV_PR5 on GOV_PR5.PER_ID = ROLE_TYP.per_id and GOV_PR5.CHAR_TYPE_CD = 'GOV_PR5'
              left join ci_per_char GOV_PR6 on GOV_PR6.PER_ID = ROLE_TYP.per_id and GOV_PR6.CHAR_TYPE_CD = 'GOV_PR6'
              left join ci_per_char GOV_PR7 on GOV_PR7.PER_ID = ROLE_TYP.per_id and GOV_PR7.CHAR_TYPE_CD = 'GOV_PR7'
              left join ci_per_char HOUSE_ID on HOUSE_ID.PER_ID = ROLE_TYP.PER_ID and HOUSE_ID.Char_Type_Cd = 'HOUSE-ID'
              left join k_house hs on trim(hs.kodd) = trim(HOUSE_ID.Adhoc_Char_Val)
              left join ci_per_phone FAX on FAX.per_id = ROLE_TYP.per_id and FAX.phone_type_cd = 'FAKS'
              left join ci_per_phone TEL_SEC on TEL_SEC.per_id = ROLE_TYP.per_id and TEL_SEC.phone_type_cd in ('TEL_SEC')
              left join ci_per_phone TEL_DEG on TEL_DEG.per_id = ROLE_TYP.per_id and TEL_DEG.phone_type_cd in ('TEL_DEG')
              left join ci_per_char IS_WAST ON IS_WAST.PER_ID = ROLE_TYP.PER_ID AND TRIM(IS_WAST.CHAR_TYPE_CD) = 'IS-WAST'
                   where per.per_id = tfg.main_per_id /*'3345417712'*/
                     and ADM_DISTR.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'DISTRICT' and per_id = ROLE_TYP.per_id)
                     and (ADM_DISTR.Adhoc_Char_Val in (Select DISTRICT.Adhoc_Char_Val
                                                                 from ci_wf_proc_char proc
                                                                 join ci_prem_char DISTRICT on proc.char_val_fk1 = DISTRICT.Prem_Id
                                                           where proc.wf_proc_id = tfg.wf_proc_id
                                                             and proc.char_type_cd = 'OB-OBSL'
                                                             and DISTRICT.Char_Type_Cd = 'DISTRICT') --районы ЭО
                      or nvl(trim(GOV_DIST.Char_Val) , 'LOCAL') = 'ALL') --совпадение по району либо GOV_DIST = ALL
                     and (trim(PLATSP.Char_Val)=decode(trim(GOV_PR0.Char_Val),'1','0','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR1.Char_Val),'1','1','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR2.Char_Val),'1','2','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR3.Char_Val),'1','3','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR4.Char_Val),'1','4','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR5.Char_Val),'1','5','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR6.Char_Val),'1','6','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR7.Char_Val),'1','7','n'))
                     and (PLATSP.Effdt is null or PLATSP.Effdt = (Select max(effdt) from ci_per_char where per_id = per.per_id and char_type_cd  = 'PLATSP'))
                     and (F_ADR_1.Effdt is null or F_ADR_1.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'F-ADR-1' and per_id = ROLE_TYP.per_id))
                     and (DL$N_DP.Effdt is null or DL$N_DP.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'DL$N-DP' and per_id = ROLE_TYP.per_id))
                     and (HOUSE_ID.Effdt is null or HOUSE_ID.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'HOUSE-ID' and per_id = HOUSE_ID.per_id))
                     and (IS_WAST.Effdt is null or IS_WAST.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = IS_WAST.Char_Type_Cd and per_id = ROLE_TYP.per_id))
                     AND nvl(TRIM(IS_WAST.CHAR_VAL) , 'N') <> 'Y'
                    order by (nvl(GOV_ORD.Adhoc_Char_Val,0))) loop

      v_tfg_rec.head_text3:=v_tfg_rec.head_text3 || chr(13) || rec.str;

      end loop;

      else
      v_tfg_rec.head_text3:= '';
      end if;
      -- ********************************************************************
      --

      SELECT --сумма на конец месяца(Для Орла)
        money_delimited(to_char(
        SUM(CASE WHEN TO_DATE(DR_TERMP.ADHOC_CHAR_VAL,'DD.MM.YYYY')<=TRUNC(ADD_MONTHS(TO_DATE(tfg.doc_dat,'DD.MM.YYYY'),1),'MONTH')-1 THEN AE.CUR_AMT ELSE 0 END)
        , '999999999999990.99'), 'RUB') CUR_AMT_END_MONTH
          into v_tfg_rec.WF3_AMOUNT_RUB
        FROM ci_acct a2
        inner join CI_SA SA on a2.acct_id = sa.acct_id
        INNER JOIN CM_RATE_PROP RP ON RP.SA_ID = SA.SA_ID
        INNER JOIN CM_ACCOUNT_ENTRY AE ON AE.CM_RATE_PROP_ID = RP.CM_RATE_PROP_ID AND AE.FREEZE_DTTM IS NOT NULL
        INNER JOIN CI_FT FT ON FT.FT_ID = AE.CM_GENERATIVE_FT_ID
        inner join ci_case_char me_id on me_id.char_val_fk1 = ft.match_evt_id and me_id.char_type_cd = 'ME-ID'
        INNER JOIN CI_CASE_CHAR DR_TERMP ON DR_TERMP.CASE_ID = ME_ID.CASE_ID AND DR_TERMP.CHAR_TYPE_CD = 'DR-TERMP'
        INNER JOIN CM_DEBT_CL_EXP DE ON DE.DEBT_CL_CD = AE.DEBT_CL_CD AND DE.CM_DEBT_CL_GROUP_CD<>'OFBL'
        LEFT  JOIN CI_CASE FD ON FD.CASE_ID = AE.CM_FIN_DOCUMENT_ID
        LEFT  JOIN CI_CASE_CHAR FDC ON FDC.CASE_ID  = FD.CASE_ID AND FDC.CHAR_TYPE_CD IN ('INVDATEC','BILLDATE')
        WHERE NOT (RP.CM_RETAIL_TYPE='0' AND FD.CASE_TYPE_CD='INVOICE')
        and a2.acct_id = tfg.acct_id;

      pipe row(v_tfg_rec);

    end loop;
  end get_tfg_select_new;

function get_tfg_select_nopeni(i_case_id in char) return T_TFG_NEW_SET pipelined is
    v_tfg_rec T_TFG_NEW_REC;

    v_per_id char(10);
    v_per2_id char(10);

    v_per_ke_id char(10);
    v_per_mtu_id char(10);
    v_per_mchs_id char(10);
    amount1 number(15,2);
    amount2 number(15,2);
    amount_tfg number(15,2);
    amount_tfg2 number(15,2);
    cnt number(3);
    V_CNT number;
    v_cnt_cp number;
    v_cnt_ik_trans number;
  begin
    for tfg in (select dc1.case_id, dc1.doc_num, dc1.doc_dat,
                we1.wf_proc_id, we1.evt_seq, ac1.acct_id,
                ac1.dog_num, ac1.dog_dat, ac1.per_id main_per_id,
                pc.adhoc_char_val main_name, ac1.main_postal,

                decode(trim(ac1.main_postal) , null , null , trim(ac1.main_postal) || ', ') ||
                cm_dz.get_acct_chty(ac1.acct_id,  'F-ADR-1 ') ||
                cm_dz.get_acct_chty(ac1.acct_id,  'F-ADR-2 ') main_address,
                cm_dz_chty.get_wfpr_chty(we1.wf_proc_id, 'DAT-TFG') DAT_TFG,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-OPLZ') DAT_OPLZ,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMS') DAT_LIMS,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMP') DAT_LIMP,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMT') DAT_LIMT,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMA') DAT_LIMA,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIM')  DAT_LIM,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'TIM-LIM')  TIM_LIM,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-HAND') DAT_HAND,
                DISTRICT.adhoc_char_val as district_nm
                from
                  cm_doc_case_vw dc1
                  inner join cm_acct_vw ac1 on ac1.acct_id=dc1.acct_id
                  inner join ci_acct_per ap on ap.acct_id = ac1.acct_id and ap.main_cust_sw = 'Y'
                  inner join ci_per_char pc on pc.per_id = ap.per_id and pc.char_type_cd = 'SHORT-NM'
                          and pc.effdt = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = pc.per_id and pc2.char_type_cd = pc.char_type_cd and pc2.effdt <= to_date(DC1.doc_dat,'dd.mm.yyyy'))
                                  inner join ci_case_char ch1 on ch1.case_id=dc1.case_id and ch1.char_type_cd='PCZ-EVT '
                  inner join ci_wf_evt we1 on we1.wf_proc_id=ch1.char_val_fk1 and we1.evt_seq=ch1.char_val_fk2
                 left join ci_per_char DISTRICT on ap.per_id = DISTRICT.PER_ID and DISTRICT.CHAR_TYPE_CD = 'DISTRICT'
                where
                  dc1.case_id= i_case_id)

    loop
      begin
        select t1.adhoc_char_val
         into v_tfg_rec.denzadol
        from ci_acct_char t1
        where t1.acct_id= tfg.acct_id and t1.char_type_cd='DENZADOL' and t1.effdt=
        (select max(t2.effdt) from ci_acct_char t2 where t1.acct_id=t2.acct_id and t1.char_type_cd=t2.char_type_cd);
      exception
        when no_data_found then v_tfg_rec.denzadol := '__';
      end;

      --
      -- Определение суммы
      select doc_amount_rub, doc_amount_kop
      into v_tfg_rec.wf_amount_rub , v_tfg_rec.wf_amount_kop
      from table(CM_DZ_REP.get_doc_sum(i_case_id));
      --
      v_tfg_rec.wf_proc_id := tfg.wf_proc_id;
      v_tfg_rec.evt_seq:= tfg.evt_seq;
      v_tfg_rec.cc_id:= cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'CONTACT');
      /*TR-18368. Novikova. Новая функция определения номеров телефонов для отчетных форм*/
      v_tfg_rec.cc_tel:= CM_DZ_CHTY.GET_ACCT_PHONE(tfg.acct_id,'TEL');
      v_tfg_rec.cc_fax:= CM_DZ_CHTY.GET_ACCT_PHONE(tfg.acct_id,'FAX');
      v_tfg_rec.cc_tel_other:= CM_DZ_CHTY.GET_ACCT_PHONE(tfg.acct_id,'OTHER');
      v_tfg_rec.case_id:= tfg.case_id;
      -- Определение района
      with a1 as
      (select distinct  p2.adhoc_char_val district
      from
        ci_wf_proc wp1
        inner join ci_wf_proc_char wc1 on wp1.wf_proc_id=wc1.wf_proc_id and wc1.char_type_cd='OB-OBSL'
        inner join ci_prem p1 on p1.prem_id=wc1.char_val_fk1
        inner join ci_prem_char p2 on p1.prem_id=p2.prem_id and p2.char_type_cd='DISTRICT'
      where
        wp1.wf_proc_id=tfg.wf_proc_id)
      select trim(to_char(wmsys.wm_concat(' '||a1.district)))
      into v_tfg_rec.district
      from a1;

      v_tfg_rec.tfg_num:= tfg.doc_num;
      v_tfg_rec.tfg_dat:= tfg.doc_dat;
      v_tfg_rec.tfg_year:= to_char(to_date(v_tfg_rec.tfg_dat, 'dd.mm.yyyy'),'YYYY');
      v_tfg_rec.dog_num:= tfg.dog_num;
      v_tfg_rec.dog_dat:= tfg.dog_dat;

      v_tfg_rec.DAT_TFG:= tfg.DAT_TFG;
      v_tfg_rec.DAT_OPLZ:= tfg.DAT_OPLZ;
      v_tfg_rec.DAT_LIMS:= tfg.DAT_LIMS;
      v_tfg_rec.DAT_LIMP:= NVL(tfg.DAT_LIMP , '_______________');
      v_tfg_rec.DAT_LIMT:= tfg.DAT_LIMT;
      v_tfg_rec.DAT_LIMA:= tfg.DAT_LIMA;
      --v_tfg_rec.DAT_LIM:= tfg.DAT_LIM;
      --v_tfg_rec.TIM_LIM:= tfg.TIM_LIM;
      -- исполнитель и его телефон
      begin
      select executor_name, executor_tel
      into v_tfg_rec.executor_name, v_tfg_rec.executor_tel
      from table(CM_DZ_REP.get_doc_head(i_case_id));
      exception when no_data_found then v_tfg_rec.executor_name := null; v_tfg_rec.executor_tel := null;
      end;
      --
      v_tfg_rec.main_name:= tfg.main_name;
      v_tfg_rec.main_postal:= tfg.main_postal;
      v_tfg_rec.main_address:= tfg.main_address;
      v_tfg_rec.pt_list:= get_pt_list(tfg.wf_proc_id); -- cm_fd.fd4wf(tfg.wf_proc_id);
      --
      select count(*) into cnt
      from ci_wf_evt_char t1 where t1.wf_proc_id=tfg.wf_proc_id and t1.evt_seq=tfg.evt_seq and t1.char_type_cd='NTF-AMT';
      if 1=cnt then
        amount1:= coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT'), '999999999999990.99'),0)
                 --TR-30264 Печать пени в уведомлениях
                 -coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT4'), '999999999999990.99'),0)
                 -coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT7'), '999999999999990.99'),0)
                 -coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT8'), '999999999999990.99'),0);
        amount2:= 0; --coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMNT-CUR'), '999999999999990.99'), 0);
        amount_tfg:= amount1+amount2;
        amount_tfg2:= coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT0'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT2'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'NTF-AMT8'), '999999999999990.99'),0);

        v_tfg_rec.wf_amount_rub:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf_amount_kop:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'KOP');
        v_tfg_rec.wf2_amount_rub:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf2_amount_kop:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'KOP');
      else
        amount1:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'NTF-AMT'), '999999999999990.99'),0)
                 --TR-30264 Печать пени в уведомлениях
                 -coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'NTF-AMT4'), '999999999999990.99'),0)
                 -coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'NTF-AMT7'), '999999999999990.99'),0)
                 -coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'NTF-AMT8'), '999999999999990.99'),0);
        amount2:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMNT-CUR'), '999999999999990.99'), 0);
        amount_tfg:= amount1+amount2;
        amount_tfg2:= coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT0'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT2'), '999999999999990.99'),0)
        +coalesce(to_number(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'WZ-AMT8'), '999999999999990.99'),0);

        v_tfg_rec.wf_amount_rub:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf_amount_kop:= money_delimited(to_char(amount_tfg, '999999999999990.99'), 'KOP');
        v_tfg_rec.wf2_amount_rub:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'RUB');
        v_tfg_rec.wf2_amount_kop:= money_delimited(to_char(amount_tfg2, '999999999999990.99'), 'KOP');
      end if;

      select nm.entity_name, dlz.adhoc_char_val
        into v_tfg_rec.SIGNER_NAME,v_tfg_rec.SIGNER_JOB
           from ci_case_char t
      inner join ci_case_char r on trim(t.char_val_fk1) = r.case_id and trim(r.char_type_cd) = 'SIGNERID'
      left  join ci_per_name nm on nm.per_id = trim(r.char_val_fk1) and nm.name_type_flg = 'PRIM'
      left join CI_PER_CHAR dlz on dlz.per_id = trim(r.char_val_fk1) and dlz.char_type_cd = 'DL$N-RUK'
            where t.case_id = i_case_id
         and trim(t.char_type_cd) = 'REESTR'
         -- TR-31249. Отображаем Должность подписанта исторично
         and (dlz.effdt is null or dlz.effdt = (select max(dlz2.effdt) from CI_PER_CHAR dlz2 where dlz2.per_id = dlz.per_id and dlz2.char_type_cd = dlz.char_type_cd));



      -- ***********************************************************************************************
      if cm_dz.get_wfpr_chty(tfg.wf_proc_id, 'DZ-DPRT') in ('DPT-BIG', 'DPT-ZHKH', 'DPT-BUDJ') then
        v_tfg_rec.head_text1:= 'От _____________        №  ___________________';
        v_tfg_rec.head_text2:= 'На №_______________от____________________';
      else
        v_tfg_rec.head_text1:= '';
        v_tfg_rec.head_text2:= '';
      end if;
      -- ************************************************************************************************
      begin
        select pc1.per_id into v_per_id
        from ci_per_char pc1 inner join ci_per_char pc2 on pc1.per_id=pc2.per_id
        where
          pc1.char_type_cd='DISTRICT' and pc1.adhoc_char_val='Василеостровский район' and
          pc2.char_type_cd='TYPE-OIV' and pc2.char_val='HEAD-ADMIN';
      exception
        when no_data_found then v_per_id := null;
      end;
      --
      begin
        select pc1.per_id into v_per2_id
        from ci_per_char pc1 inner join ci_per_char pc2 on pc1.per_id=pc2.per_id
        where
          pc1.char_type_cd='DISTRICT' and pc1.adhoc_char_val='Василеостровский район' and
          pc2.char_type_cd='TYPE-OIV' and pc2.char_val='PROKUROR';
      exception
          when no_data_found then v_per2_id := null;
      end;
      v_per_ke_id:= cm_dz_chty.get_per_id_char_val('TYPE-OIV', 'KOMITET-ENERGO');
      v_per_mtu_id:= cm_dz_chty.get_per_id_char_val('TYPE-OIV', 'MTU-RTN');
      v_per_mchs_id:= cm_dz_chty.get_per_id_char_val('TYPE-OIV', 'GU-MCHS');

       select COUNT(*)
        into v_cnt_cp
            from cm_eo_account_linkage t
      inner join cm_prem_link l on t.cm_eo_id = l.cm_dest_prem_id
      inner join ci_prem cp on cp.prem_id = l.cm_source_prem_id
       where t.acct_id = tfg.acct_id
         and sysdate between t.start_dt and nvl(t.end_dt , to_date('01.01.2100' , 'dd.mm.yyyy'))
         and cp.prem_type_cd = 'CP';

       select COUNT(*)
         into v_cnt_ik_trans
            from cm_eo_account_linkage t
      inner join cm_prem_link l on t.cm_eo_id = l.cm_dest_prem_id
      inner join ci_prem cp on cp.prem_id = l.cm_source_prem_id
       where t.acct_id = tfg.acct_id
         and sysdate between t.start_dt and nvl(t.end_dt , to_date('01.01.2100' , 'dd.mm.yyyy'))
         and cp.prem_type_cd = 'IK-TRANS';


      /*Выбор шаблона*/ --доп.шаблон
      if (cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='N' and
          cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='N' and
          v_cnt_cp = 1 and
          v_cnt_ik_trans = 0) then
       v_tfg_rec.HEAD_ABTB := '';
       v_tfg_rec.TXT_TFG1 :=  'самостоятельно, путём снижения объёма электропотребления, ввести режим частичного ограничения потребления электроэнергии.
         Срок введения самостоятельного частичного ограничения: 10:00 часов ' ||  tfg.DAT_LIMS || ' г.' ;

       v_tfg_rec.txt_limit1 := 'полное ограничение' ;
       v_tfg_rec.txt_limit2:= null;
       v_tfg_rec.TXT_LIMIT3 := '';
       v_tfg_rec.TXT_TFG2 := '';
       v_tfg_rec.TXT_TFG4 := 'и в связи с отсутствием технической возможности введения частичного ограничения режима потребления э/э с питающих центров, ';

      --1 шаблон
      elsif (cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='N' and
          cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='N') then
       v_tfg_rec.HEAD_ABTB := '';
       v_tfg_rec.TXT_TFG1 :=  'самостоятельно, путём снижения объёма электропотребления, ввести режим частичного ограничения потребления электроэнергии.
         Срок введения самостоятельного частичного ограничения: 10:00 часов ' ||  tfg.DAT_LIMS || ' г.' ;

       v_tfg_rec.txt_limit1 := 'частичное ограничение' ;
       v_tfg_rec.txt_limit2:= null;
       v_tfg_rec.TXT_LIMIT3 := '';
       v_tfg_rec.TXT_TFG2 := '';
       v_tfg_rec.TXT_TFG3 := '       В дальнейшем, при неоплате сложившейся задолженности полном объёме и неисполнении обязательств по  предварительной оплате электрической энергии до 10:00 ' || TFG.DAT_LIM || ' г. , Вашему предприятию после 10:00 часов ' || TFG.DAT_LIM || ' г. будет введён режим полного ограничения потребления электроэнергии путём отключения всех энергопринимающих устройств (объектов).';
 --2 шаблон
      elsif ((cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='Y' or
             cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y') and
             cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'PRIL4530')= 'N') then
       v_tfg_rec.HEAD_ABTB := ' до уровня технологической и/или аварийной брони';
       v_tfg_rec.TXT_TFG1 := 'c 10:00 часов ' || tfg.DAT_LIMS || ' г. самостоятельно, путём снижения объёма электропотребления, ввести режим ограничения потребления электроэнергии до уровня технологической брони.';
       v_tfg_rec.txt_limit1 := 'ограничение' ;
       v_tfg_rec.txt_limit2:= 'до уровня технологической брони с питающих центров, ';
       if  cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y' then
        v_tfg_rec.TXT_TFG2:= 'При неоплате сложившейся задолженности в полном объёме Вашему предприятию после 10:00 ' ||  NVL(TO_CHAR((TO_DATE(TFG.DAT_HAND,'DD.MM.YYYY')+16),'DD.MM.YYYY') , '__________________') || ' г. будет введен режим ограничения потребления электроэнергии до уровня аварийной брони, путем отключения энергопринимающих устройств(объектов):';
        for rec in (Select * from table(get_tp_list_4_wf_proc(tfg.wf_proc_id))) loop
        v_tfg_rec.TXT_TFG2:= v_tfg_rec.TXT_TFG2 || chr(13) || '         ' || rec.EO_CODE || ' , ' || rec.EO_ADDRESS || ' , ' || rec.EO_NAME ;
         --Проверка наличия аварийной брони в действующем акте АБ/ТБ.
         --Пока убираем - в дальнейшем будет нужна
/*         SELECT COUNT(*)
           INTO V_CNT
         from TABLE(cm_char_utils.sel_akt_ab_tb(rec.EO_ID, to_date(tfg.dat_lim,'DD.MM.YYYY'))) T
                   inner join CM_RESERVATION R on T.DOC_ID = R.CM_RESERVATION_DOCUMENT_ID AND TRIM(R.CM_RESERVATION_TYPE_CD) = 'EMERGENCY'
                   INNER JOIN CM_RESERVATION_TOT_VALUE RT ON R.CM_RESERVATION_ID = RT.CM_RESERVATION_ID
             WHERE (RT.CM_POWER_VALUE > 0 OR RT.CM_ENERGY_VALUE > 0)
               AND TO_NUMBER(TO_CHAR(TO_DATE(tfg.dat_lim,'DD.MM.YYYY'), 'MM')) BETWEEN
                 RT.CM_START_MONTH AND RT.CM_END_MONTH;
              if v_cnt>0 then
              v_tfg_rec.TXT_TFG2:= v_tfg_rec.TXT_TFG2 || chr(13) || '         ' || rec.EO_CODE || ' , ' || rec.EO_ADDRESS || ' , ' || rec.EO_NAME ;
              end if;
*/          end loop;
       end if;
       v_tfg_rec.TXT_TFG3 := '       В дальнейшем, при неоплате сложившейся задолженности в полном объёме до 10:00 ' ||  TFG.DAT_LIM || ' г., Вашему предприятию после 10:00 часов ' || TFG.DAT_LIM || ' г. будет введён режим полного ограничения потребления электроэнергии путём отключения всех энергопринимающих устройств (объектов).';

      --3 шаблон
       elsif ((cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='Y' or
             cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y') and
             cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'PRIL4530')= 'Y') then
      v_tfg_rec.HEAD_ABTB := ' до уровня технологической и/или аварийной брони';
     v_tfg_rec.TXT_TFG1 := 'c 10:00 часов ' || tfg.DAT_LIMS || ' г. самостоятельно, путём снижения объёма электропотребления, ввести режим частичного ограничения потребления электроэнергии до уровня технологической брони.';
     v_tfg_rec.txt_limit1 := 'ограничение' ;
     v_tfg_rec.txt_limit2:= 'до уровня технологической брони с питающих центров, ';
     if  cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y' then
       v_tfg_rec.TXT_TFG2:= '       В дальнейшем, при неоплате сложившейся задолженности в полном объеме, Вашему предприятию после 10:00 часов '
       || tfg.DAT_LIM || ' будет введен режим ограничения потребление электроэнергии до уровня аварийной брони, путем отключения энергопринимающих устройств(объектов):';
     for rec in (Select * from table(get_tp_list_4_wf_proc(tfg.wf_proc_id))) loop
     v_tfg_rec.TXT_TFG2:= v_tfg_rec.TXT_TFG2 || chr(13) || '         ' || rec.EO_CODE || ' , ' || rec.EO_ADDRESS || ' , ' || rec.EO_NAME ;
         --Проверка наличия аварийной брони в действующем акте АБ/ТБ
         --Пока убираем - в дальнейшем будет нужна
      /*   SELECT COUNT(*)
           INTO V_CNT
         from TABLE(cm_char_utils.sel_akt_ab_tb(rec.EO_ID, to_date(tfg.dat_lim,'DD.MM.YYYY'))) T
                   inner join CM_RESERVATION R on T.DOC_ID = R.CM_RESERVATION_DOCUMENT_ID AND TRIM(R.CM_RESERVATION_TYPE_CD) = 'EMERGENCY'
                   INNER JOIN CM_RESERVATION_TOT_VALUE RT ON R.CM_RESERVATION_ID = RT.CM_RESERVATION_ID
             WHERE (RT.CM_POWER_VALUE > 0 OR RT.CM_ENERGY_VALUE > 0)
               AND TO_NUMBER(TO_CHAR(TO_DATE(tfg.dat_lim,'DD.MM.YYYY'), 'MM')) BETWEEN
                 RT.CM_START_MONTH AND RT.CM_END_MONTH;
              if v_cnt>0 then
              v_tfg_rec.TXT_TFG2:= v_tfg_rec.TXT_TFG2 || chr(13) || '         ' || rec.EO_CODE || ' , ' || rec.EO_ADDRESS || ' , ' || rec.EO_NAME ;
              end if;
         */ end loop;
     end if;
      v_tfg_rec.TXT_TFG3 := '';
      end if;

      if (cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')='Y' or
          cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')='Y') then

      v_tfg_rec.head_text3 := '';

      for rec in (Select chr(13) ||
                         decode(DL$N_DP.Adhoc_Char_Val,'UNDEF',NULL,DL$N_DP.Adhoc_Char_Val) || chr(13) || -- Должность руководителя в дательном падеже
                         decode(FIO_DP.Adhoc_Char_Val,'UNDEF',NULL,FIO_DP.Adhoc_Char_Val) || chr(13) ||  --ФИО руководителя в дательном падеже
                         decode(F_ADR_1.ADHOC_CHAR_VAL,'UNDEF',NULL,F_ADR_1.ADHOC_CHAR_VAL) || ', индекс ' || hs.ind ||
                         chr(10) || 'ф.' || nvl(FAX.PHONE , '____________') ||
                         ', т.' || nvl(TEL_SEC.Phone || decode(TEL_SEC.Phone,NULL,'', decode(TEL_DEG.Phone, NULL, '' , ' , ')),'____________') as str
                  from ci_per_char ROLE_TYP
                  join ci_per_char ADM_DISTR on ROLE_TYP.Per_Id = ADM_DISTR.Per_Id and ADM_DISTR.CHAR_TYPE_CD = 'DISTRICT'
             left join ci_per_char GOV_DIST on GOV_DIST.Per_Id = ROLE_TYP.Per_Id and GOV_DIST.CHAR_TYPE_CD = 'GOV_DIST'
             left join ci_per_char GOV_AT on GOV_AT.Per_Id = ROLE_TYP.Per_Id and GOV_AT.CHAR_TYPE_CD = 'GOV_AT'
             left join ci_per_char GOV_ORD on GOV_ORD.Per_Id = ROLE_TYP.Per_Id and GOV_ORD.CHAR_TYPE_CD = 'GOV_ORD' --очередность вывода
             left join ci_per_char DL$N_DP  on DL$N_DP.PER_ID = ROLE_TYP.per_id and DL$N_DP.CHAR_TYPE_CD = 'DL$N-DP'
             left join ci_per_char FIO_DP on FIO_DP.PER_ID = ROLE_TYP.per_id and FIO_DP.CHAR_TYPE_CD = 'FIO-DP'
             left join ci_per_char F_ADR_1 on F_ADR_1.PER_ID = ROLE_TYP.per_id and F_ADR_1.CHAR_TYPE_CD = 'F-ADR-1'
             left join ci_per_char HOUSE_ID on HOUSE_ID.PER_ID = ROLE_TYP.PER_ID and HOUSE_ID.Char_Type_Cd = 'HOUSE-ID'
             left join k_house hs on trim(hs.kodd) = trim(HOUSE_ID.Adhoc_Char_Val)
             left join ci_per_phone FAX on FAX.per_id = ROLE_TYP.per_id and FAX.phone_type_cd = 'FAKS'
             left join ci_per_phone TEL_SEC on TEL_SEC.per_id = ROLE_TYP.per_id and TEL_SEC.phone_type_cd in ('TEL_SEC')
             left join ci_per_phone TEL_DEG on TEL_DEG.per_id = ROLE_TYP.per_id and TEL_DEG.phone_type_cd in ('TEL_DEG')
             left join ci_per_char IS_WAST ON IS_WAST.PER_ID = ROLE_TYP.PER_ID AND TRIM(IS_WAST.CHAR_TYPE_CD) = 'IS-WAST'
                  where ROLE_TYP.CHAR_TYPE_CD = 'ROLE-TYP'
                    and ROLE_TYP.Char_Val_Fk1 = 'ADM-DIST'  --субъекты роли "Адм.районы"
                    and(ADM_DISTR.Adhoc_Char_Val in (Select DISTRICT.Adhoc_Char_Val
                                                                 from ci_wf_proc_char proc
                                                                 join ci_prem_char DISTRICT on proc.char_val_fk1 = DISTRICT.Prem_Id
                                                           where proc.wf_proc_id = tfg.wf_proc_id
                                                             and proc.char_type_cd = 'OB-OBSL'
                                                             and DISTRICT.Char_Type_Cd = 'DISTRICT') --районы ЭО
                     or nvl(trim(GOV_DIST.Char_Val) , 'LOCAL') = 'ALL')
                    and nvl(trim(GOV_AT.Char_Val) , 'N') = 'Y'
                    and ADM_DISTR.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'DISTRICT' and per_id = ROLE_TYP.per_id)
                    and (F_ADR_1.Effdt is null or F_ADR_1.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'F-ADR-1' and per_id = ROLE_TYP.per_id))
                    and (DL$N_DP.Effdt is null or DL$N_DP.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'DL$N-DP' and per_id = ROLE_TYP.per_id))
                    and (HOUSE_ID.Effdt is null or HOUSE_ID.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'HOUSE-ID' and per_id = HOUSE_ID.per_id))
                    and (IS_WAST.Effdt is null or IS_WAST.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = IS_WAST.Char_Type_Cd and per_id = ROLE_TYP.per_id))
                    AND nvl(TRIM(IS_WAST.CHAR_VAL) , 'N') <> 'Y'
                  order by (nvl(GOV_ORD.Adhoc_Char_Val,0))  ) loop

      v_tfg_rec.head_text3:=v_tfg_rec.head_text3 || chr(13) || rec.str;

      end loop;

      elsif (cm_dz_chty.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, '4-ADM-RN')='Y') then

      v_tfg_rec.head_text3:='';
      for rec in (Select  chr(13) ||
                          decode(DL$N_DP.Adhoc_Char_Val,'UNDEF',NULL,DL$N_DP.Adhoc_Char_Val) || chr(13) || -- Должность руководителя в дательном падеже
                          decode(FIO_DP.Adhoc_Char_Val,'UNDEF',NULL,FIO_DP.Adhoc_Char_Val) || chr(13) ||  --ФИО руководителя в дательном падеже
                          decode(F_ADR_1.ADHOC_CHAR_VAL,'UNDEF',NULL,F_ADR_1.ADHOC_CHAR_VAL) || ', индекс ' || hs.ind ||
                          chr(10) || 'ф.' || nvl(FAX.PHONE , '____________') || ', т.' ||
                          nvl(TEL_SEC.Phone || decode(TEL_SEC.Phone,NULL,'', decode(TEL_DEG.Phone, NULL, '' , ' , ')),'____________') as str  -- Адрес субъекта
                   from ci_per per --потребитель
              left join ci_per_char PLATSP on per.per_id = PLATSP.Per_Id and PLATSP.Char_Type_Cd = 'PLATSP' --платежеспособность
                   --органы исп.власти
                   join ci_per_char ROLE_TYP on ROLE_TYP.CHAR_TYPE_CD = 'ROLE-TYP' and ROLE_TYP.Char_Val_Fk1 = 'ADM-DIST'  --субъекты роли "Адм.районы"
                   join ci_per_char ADM_DISTR on ROLE_TYP.Per_Id = ADM_DISTR.Per_Id and ADM_DISTR.CHAR_TYPE_CD = 'DISTRICT'
              left join ci_per_char DL$N_DP  on DL$N_DP.PER_ID = ROLE_TYP.per_id and DL$N_DP.CHAR_TYPE_CD = 'DL$N-DP'
              left join ci_per_char FIO_DP on FIO_DP.PER_ID = ROLE_TYP.per_id and FIO_DP.CHAR_TYPE_CD = 'FIO-DP'
              left join ci_per_char F_ADR_1 on F_ADR_1.PER_ID = ROLE_TYP.per_id and F_ADR_1.CHAR_TYPE_CD = 'F-ADR-1'
              left join ci_per_char GOV_DIST on GOV_DIST.Per_Id = ROLE_TYP.Per_Id and GOV_DIST.CHAR_TYPE_CD = 'GOV_DIST'
              left join ci_per_char GOV_ORD on GOV_ORD.Per_Id = ROLE_TYP.Per_Id and GOV_ORD.CHAR_TYPE_CD = 'GOV_ORD' --очередность вывода
              left join ci_per_char GOV_PR0 on GOV_PR0.PER_ID = ROLE_TYP.per_id and GOV_PR0.CHAR_TYPE_CD = 'GOV_PR0'
              left join ci_per_char GOV_PR1 on GOV_PR1.PER_ID = ROLE_TYP.per_id and GOV_PR1.CHAR_TYPE_CD = 'GOV_PR1'
              left join ci_per_char GOV_PR2 on GOV_PR2.PER_ID = ROLE_TYP.per_id and GOV_PR2.CHAR_TYPE_CD = 'GOV_PR2'
              left join ci_per_char GOV_PR3 on GOV_PR3.PER_ID = ROLE_TYP.per_id and GOV_PR3.CHAR_TYPE_CD = 'GOV_PR3'
              left join ci_per_char GOV_PR4 on GOV_PR4.PER_ID = ROLE_TYP.per_id and GOV_PR4.CHAR_TYPE_CD = 'GOV_PR4'
              left join ci_per_char GOV_PR5 on GOV_PR5.PER_ID = ROLE_TYP.per_id and GOV_PR5.CHAR_TYPE_CD = 'GOV_PR5'
              left join ci_per_char GOV_PR6 on GOV_PR6.PER_ID = ROLE_TYP.per_id and GOV_PR6.CHAR_TYPE_CD = 'GOV_PR6'
              left join ci_per_char GOV_PR7 on GOV_PR7.PER_ID = ROLE_TYP.per_id and GOV_PR7.CHAR_TYPE_CD = 'GOV_PR7'
              left join ci_per_char HOUSE_ID on HOUSE_ID.PER_ID = ROLE_TYP.PER_ID and HOUSE_ID.Char_Type_Cd = 'HOUSE-ID'
              left join k_house hs on trim(hs.kodd) = trim(HOUSE_ID.Adhoc_Char_Val)
              left join ci_per_phone FAX on FAX.per_id = ROLE_TYP.per_id and FAX.phone_type_cd = 'FAKS'
              left join ci_per_phone TEL_SEC on TEL_SEC.per_id = ROLE_TYP.per_id and TEL_SEC.phone_type_cd in ('TEL_SEC')
              left join ci_per_phone TEL_DEG on TEL_DEG.per_id = ROLE_TYP.per_id and TEL_DEG.phone_type_cd in ('TEL_DEG')
              left join ci_per_char IS_WAST ON IS_WAST.PER_ID = ROLE_TYP.PER_ID AND TRIM(IS_WAST.CHAR_TYPE_CD) = 'IS-WAST'
                   where per.per_id = tfg.main_per_id /*'3345417712'*/
                     and ADM_DISTR.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'DISTRICT' and per_id = ROLE_TYP.per_id)
                     and (ADM_DISTR.Adhoc_Char_Val in (Select DISTRICT.Adhoc_Char_Val
                                                                 from ci_wf_proc_char proc
                                                                 join ci_prem_char DISTRICT on proc.char_val_fk1 = DISTRICT.Prem_Id
                                                           where proc.wf_proc_id = tfg.wf_proc_id
                                                             and proc.char_type_cd = 'OB-OBSL'
                                                             and DISTRICT.Char_Type_Cd = 'DISTRICT') --районы ЭО
                      or nvl(trim(GOV_DIST.Char_Val) , 'LOCAL') = 'ALL') --совпадение по району либо GOV_DIST = ALL
                     and (trim(PLATSP.Char_Val)=decode(trim(GOV_PR0.Char_Val),'1','0','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR1.Char_Val),'1','1','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR2.Char_Val),'1','2','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR3.Char_Val),'1','3','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR4.Char_Val),'1','4','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR5.Char_Val),'1','5','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR6.Char_Val),'1','6','n') or
                          trim(PLATSP.Char_Val)=decode(trim(GOV_PR7.Char_Val),'1','7','n'))
                     and (PLATSP.Effdt is null or PLATSP.Effdt = (Select max(effdt) from ci_per_char where per_id = per.per_id and char_type_cd  = 'PLATSP'))
                     and (F_ADR_1.Effdt is null or F_ADR_1.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'F-ADR-1' and per_id = ROLE_TYP.per_id))
                     and (DL$N_DP.Effdt is null or DL$N_DP.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'DL$N-DP' and per_id = ROLE_TYP.per_id))
                     and (HOUSE_ID.Effdt is null or HOUSE_ID.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = 'HOUSE-ID' and per_id = HOUSE_ID.per_id))
                     and (IS_WAST.Effdt is null or IS_WAST.Effdt = (Select max(Effdt) from ci_per_char where CHAR_TYPE_CD = IS_WAST.Char_Type_Cd and per_id = ROLE_TYP.per_id))
                     AND nvl(TRIM(IS_WAST.CHAR_VAL) , 'N') <> 'Y'
                    order by (nvl(GOV_ORD.Adhoc_Char_Val,0))) loop

      v_tfg_rec.head_text3:=v_tfg_rec.head_text3 || chr(13) || rec.str;

      end loop;

      else
      v_tfg_rec.head_text3:= '';
      end if;
      -- ********************************************************************
      --

      SELECT --сумма на конец месяца(Для Орла)
        money_delimited(to_char(
        SUM(CASE WHEN TO_DATE(DR_TERMP.ADHOC_CHAR_VAL,'DD.MM.YYYY')<=TRUNC(ADD_MONTHS(TO_DATE(tfg.doc_dat,'DD.MM.YYYY'),1),'MONTH')-1 THEN AE.CUR_AMT ELSE 0 END)
        , '999999999999990.99'), 'RUB') CUR_AMT_END_MONTH
          into v_tfg_rec.WF3_AMOUNT_RUB
        FROM ci_acct a2
        inner join CI_SA SA on a2.acct_id = sa.acct_id
        INNER JOIN CM_RATE_PROP RP ON RP.SA_ID = SA.SA_ID
        INNER JOIN CM_ACCOUNT_ENTRY AE ON AE.CM_RATE_PROP_ID = RP.CM_RATE_PROP_ID AND AE.FREEZE_DTTM IS NOT NULL
        INNER JOIN CI_FT FT ON FT.FT_ID = AE.CM_GENERATIVE_FT_ID
        inner join ci_case_char me_id on me_id.char_val_fk1 = ft.match_evt_id and me_id.char_type_cd = 'ME-ID'
        INNER JOIN CI_CASE_CHAR DR_TERMP ON DR_TERMP.CASE_ID = ME_ID.CASE_ID AND DR_TERMP.CHAR_TYPE_CD = 'DR-TERMP'
        INNER JOIN CM_DEBT_CL_EXP DE ON DE.DEBT_CL_CD = AE.DEBT_CL_CD AND DE.CM_DEBT_CL_GROUP_CD<>'OFBL'
        LEFT  JOIN CI_CASE FD ON FD.CASE_ID = AE.CM_FIN_DOCUMENT_ID
        LEFT  JOIN CI_CASE_CHAR FDC ON FDC.CASE_ID  = FD.CASE_ID AND FDC.CHAR_TYPE_CD IN ('INVDATEC','BILLDATE')
        WHERE NOT (RP.CM_RETAIL_TYPE='0' AND FD.CASE_TYPE_CD='INVOICE')
        and a2.acct_id = tfg.acct_id;

      pipe row(v_tfg_rec);

    end loop;
  end get_tfg_select_nopeni;

    function get_doc_slf(i_case_id in char) return T_DOC_SLF_SET pipelined is
    v_doc_slf_rec T_DOC_SLF_REC;
    v_buf varchar2(2000);
    v_amt number(15,2);
    v_wf_proc_id char(10);
  begin
    select wf1.wf_proc_id
    into v_wf_proc_id
    from
      ci_case c1
      inner join ci_wf_proc_char wc1 on wc1.char_val_fk1=c1.acct_id and wc1.char_type_cd='LIC-S4ET'
      inner join ci_wf_proc wf1 on wf1.wf_proc_id=wc1.wf_proc_id and wf1.wf_proc_tmpl_cd='DZ-MAIN' and wf1.wf_stat_flg='10'
    where
      c1.case_id = i_case_id and rownum=1;
    --
    v_amt:= 0;
    v_doc_slf_rec.DOC_SLF_PT:= '';
    for a1 in (
      select
        draft.case_id draft_id, sum(ae.cur_amt) draft_sum,
        max(fte.cm_unaccounted_sw) un_ac_sw, decode(max(fte.cm_unaccounted_sw),'Y', 'N', 'N', 'Y') q1
      from
        ci_wf_proc wf1
        join ci_wf_proc_char wf2 on wf1.wf_proc_id=wf2.wf_proc_id and wf2.char_type_cd = 'DZ-DRAFT'
        join ci_case draft on draft.case_id=RPAD(wf2.char_val_fk1, 10)
        join ci_case_char cc_me on cc_me.case_id=draft.case_id and cc_me.char_type_cd='ME-ID'
        join ci_ft ft on (ft.match_evt_id=cc_me.char_val_fk1 and ft.accounting_dt<=sysdate)
        join cm_ft_exp fte on fte.ft_id=ft.ft_id
        join cm_account_entry ae on (ae.cm_generative_ft_id=ft.ft_id and ae.accounting_dt<=sysdate)
        join cm_rate_prop rp on (rp.cm_rate_prop_id = ae.cm_rate_prop_id and rp.accounting_dt<=sysdate)
      where
        wf1.wf_proc_id = v_wf_proc_id --'6430663840' --'5206604356'
        group by draft.case_id
      having sum(ae.cur_amt)>0
        --decode(max(fte.cm_unaccounted_sw),'Y', 'N', 'N', 'Y')='1' or 1=1
      )
    loop
      v_amt:= v_amt + a1.draft_sum;
      if length(v_doc_slf_rec.DOC_SLF_PT)>0
        then v_doc_slf_rec.DOC_SLF_PT:= v_doc_slf_rec.DOC_SLF_PT||', '; end if;
      v_doc_slf_rec.DOC_SLF_PT := v_doc_slf_rec.DOC_SLF_PT || get_draft_name(a1.draft_id);
      null;
    end loop;
    v_doc_slf_rec.DOC_SLF_SUM:= to_char(v_amt, '999,999,999,990.00');
    v_doc_slf_rec.DOC_SLF_SUM_RUB:= money_delimited(to_char(v_amt, '999999999999990.99'), 'RUB');
    v_doc_slf_rec.DOC_SLF_SUM_KOP:= money_delimited(to_char(v_amt, '999999999999990.99'), 'KOP');
    v_doc_slf_rec.DOC_SLF_SUM:= trim(replace(v_doc_slf_rec.DOC_SLF_SUM, ',', ' '));
    pipe row(v_doc_slf_rec);
  end get_doc_slf;
  --
  function get_sogl_restr_select (i_case_id in char) return T_SOGL_RESTR_TAB pipelined is
    v_sogl_rec T_SOGL_RESTR_REC;
    amount1 number(15,2);
    amount2 number(15,2);
    amount3 number(15,2);
    amount_sogl number(15,2);
    v_evt_type ci_wf_evt.wf_evt_type_cd%type;
    v_srok number;
    v_cm_division_flg cm_wf_department.cm_division_flg%type;
  begin
   for sogl in (select dc1.case_id,
                       pdoc.wf_proc_id,
                       pdoc.evt_seq,
                       dc1_num.Adhoc_Char_Val AS doc_num,
                       dc1_dat.adhoc_char_val as doc_dat,
                       cm_dz.get_acct_chty(ac1.acct_id, 'DOG-NUM') dog_num,
                       to_char(AC1.setup_dt, 'dd.mm.yyyy') dog_dat,
                       mpn1.entity_name main_name,
                       mpa1.postal main_postal,
                       mpa1.address1 main_address,
                       u1.last_name || ' ' || u1.first_name as officer_name,
                       cm_dz_chty.get_wfev_chty(pdoc.wf_proc_id, pdoc.evt_seq, 'SOGL-NUM') SOGL_NUM,
                       cm_dz_chty.get_wfev_chty(pdoc.wf_proc_id, pdoc.evt_seq, 'SOGL-DAT') SOGL_DAT,
                       cm_dz_chty.get_wfev_chty(pdoc.wf_proc_id, pdoc.evt_seq, 'RSTR-SUM') RSTR_SUM,
                       cm_dz_chty.get_wfev_chty(pdoc.wf_proc_id, pdoc.evt_seq, 'S-AMT4') S_AMT4,
                       cm_dz_chty.get_wfev_chty(pdoc.wf_proc_id, pdoc.evt_seq, 'S-AMT7') S_AMT7,
                       cm_dz_chty.get_wfev_chty(pdoc.wf_proc_id, pdoc.evt_seq, 'S-AMT2') S_AMT2,
                       cm_dz_chty.get_wfev_chty(pdoc.wf_proc_id, pdoc.evt_seq, 'S-AMT8') S_AMT8,
                       cm_dz_chty.get_wfev_chty(pdoc.wf_proc_id, pdoc.evt_seq, 'S-AMT0') S_AMT0

  from ci_case dc1
  left join ci_case_char dc1_num on dc1_num.case_id = dc1.case_id and dc1_num.char_type_cd = 'DOC-NUM'
  left join ci_case_char dc1_dat on dc1_dat.case_id = dc1.case_id and dc1_dat.char_type_cd = 'DOC-DAT'
 inner join ci_acct ac1 on ac1.acct_id = dc1.acct_id
 inner join ci_acct_per mp1 on mp1.acct_id = ac1.acct_id and mp1.main_cust_sw = 'Y'
 inner join ci_per_name mpn1 on mpn1.per_id = mp1.per_id and mpn1.prim_name_sw = 'Y'
 inner join ci_per_addr_ovrd mpa1 on mpa1.per_id = mp1.per_id
 inner join ci_wf_evt_char pdoc on pdoc.char_type_cd = 'PRINTDOC' AND PDOC.CHAR_VAL_FK1 = DC1.CASE_ID
 inner join ci_wf_proc_char executor on executor.wf_proc_id = pdoc.wf_proc_id and executor.char_type_cd = 'EXECUTOR'
 inner join sc_user u1 on trim(u1.user_id) = trim(executor.char_val_fk1)
 where dc1.case_id = i_case_id)
    loop
      v_sogl_rec.case_id:= sogl.case_id;
      v_sogl_rec.doc_num := sogl.doc_num;
      v_sogl_rec.doc_dat := sogl.doc_dat;
      v_sogl_rec.dog_num := sogl.dog_num;
      v_sogl_rec.dog_dat := sogl.dog_dat;
      v_sogl_rec.main_name := sogl.main_name;
      v_sogl_rec.main_postal := sogl.main_postal;
      v_sogl_rec.main_address := sogl.main_address;
      v_sogl_rec.SOGL_NUM := sogl.sogl_num;
      v_sogl_rec.SOGL_DAT := sogl.sogl_dat;
     -- v_sogl_rec.DOVR_NUM := sogl.dovr_num;
     -- v_sogl_rec.DOVR_DAT := sogl.dovr_dat;
     -- v_sogl_rec.FIO_SIGN := sogl.fio_sign;
      amount1:= coalesce(to_number(cm_dz_chty.get_wfev_chty(sogl.wf_proc_id, sogl.evt_seq, 'RSTR-SUM'), '999999999999990.99'),0);
      amount2:= coalesce(to_number(cm_dz_chty.get_wfev_chty(sogl.wf_proc_id, sogl.evt_seq, 'PRTPAY-O'), '999999999999990.99'), 0);
      amount3:= coalesce(to_number(cm_dz_chty.get_wfev_chty(sogl.wf_proc_id, sogl.evt_seq, 'PRTPAY-P'), '999999999999990.99'), 0);
      amount_sogl:= amount1+amount2+amount3;
      v_sogl_rec.RSTR_SUM_RUB := money_delimited(to_char(amount_sogl, '999999999999990.99'), 'RUB');
      v_sogl_rec.RSTR_SUM_KOP := money_delimited(to_char(amount_sogl, '999999999999990.99'), 'KOP');




      --вычисляем срок
        select trunc(MONTHS_BETWEEN(max(to_date(rstr_dat.adhoc_char_val , 'dd.mm.yyyy')) , to_date(sogl.doc_dat , 'dd.mm.yyyy')))
          into v_srok
      from ci_wf_evt_char c1
           inner join ci_wf_evt_char c2 on c1.wf_proc_id=c2.wf_proc_id and c1.evt_seq=c2.evt_seq and c2.char_type_cd='RSTR-GRF'
           inner join ci_wf_evt evt on evt.wf_proc_id = c2.char_val_fk1 and evt.wf_evt_type_cd in  ('DZ-RSTR-PAY','DZ-RSTR-PENY')
           inner join ci_wf_evt_char rstr_dat on rstr_dat.wf_proc_id = evt.wf_proc_id and rstr_dat.evt_seq = evt.evt_seq and rstr_dat.char_type_cd = 'PLAT-DAT'
      where trim(c1.char_val_fk1)=trim(i_case_id);

      --v_cm_division_flg
      SELECT dep.cm_division_flg
        into v_cm_division_flg FROM CI_CASE T
      INNER JOIN CI_WF_PROC_CHAR ACCT ON T.ACCT_ID = ACCT.CHAR_VAL_FK1 AND ACCT.CHAR_TYPE_CD = 'LIC-S4ET'
      INNER JOIN CI_WF_PROC PROC_MAIN ON PROC_MAIN.WF_PROC_ID = ACCT.WF_PROC_ID AND PROC_MAIN.WF_PROC_TMPL_CD = 'DZ-MAIN'
      INNER JOIN CI_WF_PROC_CHAR DZ_DPRT ON DZ_DPRT.WF_PROC_ID = PROC_MAIN.WF_PROC_ID AND DZ_DPRT.CHAR_TYPE_CD = 'DZ-DPRT'
      INNER JOIN CM_WF_DEPARTMENT DEP ON DEP.CM_WF_DEPARTMENT_CD = DZ_DPRT.CHAR_VAL_FK1
         WHERE T.CASE_ID = i_case_id
           and PROC_MAIN.WF_STAT_FLG <> 20;


       if (v_srok > 12 or to_number(sogl.rstr_sum , '999999999999990.99')>1000000) then
         select perc2.adhoc_char_val, null
          into v_sogl_rec.FIO_SIGN , v_sogl_rec.DOVR_NUM
          from ci_per_per t
                               inner join ci_per per2 on t.per_id2 = per2.per_id
                               inner join ci_per_char perc2 on perc2.per_id = per2.per_id and perc2.char_type_cd = 'FIO-SIGN'
                where t.per_id1 = '9900000476' /*Дирекция по произв.д-ти и сбыту*/
                  and t.per_rel_type_cd = 'DL-50'--ген.директор
                  and trunc(sysdate) between t.start_dt and nvl(t.end_dt, trunc(sysdate));
        elsif (v_srok > 6 or to_number(sogl.rstr_sum , '999999999999990.99')>201000) then

         select perc2.adhoc_char_val , ', действующего на основании доверенности ' || dovr.adhoc_char_val
           into v_sogl_rec.FIO_SIGN ,  v_sogl_rec.DOVR_NUM
                from ci_per_per t
          inner join ci_per per2 on t.per_id2 = per2.per_id
          inner join ci_per_char perc2 on perc2.per_id = per2.per_id and perc2.char_type_cd = 'FIO-SIGN'
          inner join ci_per_char dovr on dovr.per_id = per2.per_id and dovr.char_type_cd = 'DOVER-SG'
          where t.per_id1 = '9900000476' /*Дирекция по произв.д-ти и сбыту*/
            and t.per_rel_type_cd = 'DL-39'  --Зам.ген.дир.
            and trunc(sysdate) between t.start_dt and nvl(t.end_dt, trunc(sysdate))
            and dovr.effdt = (select max(effdt) from ci_per_char where per_id = dovr.per_id and char_type_cd = dovr.char_type_cd and sysdate>effdt);
         else
           if v_cm_division_flg = 'URD' then
           SELECT nvl(max(perc2.adhoc_char_val),'__________________') , ', действующего на основании доверенности ' || decode(max(dovr.adhoc_char_val) , null , '_______________________' ,  max(dovr.adhoc_char_val))
             into v_sogl_rec.FIO_SIGN ,  v_sogl_rec.DOVR_NUM
           from ci_per_per t
          inner join ci_per per2 on t.per_id2 = per2.per_id
          inner join ci_per_char perc2 on perc2.per_id = per2.per_id and perc2.char_type_cd = 'FIO-SIGN'
          inner join ci_per_char dovr on dovr.per_id = per2.per_id and dovr.char_type_cd = 'DOVER-SG'
          where t.per_id1 = '9900000456' /*УРД*/
            and t.per_rel_type_cd = 'DL-347' --Начальник управления(Андронов)
            and trunc(sysdate) between t.start_dt and nvl(t.end_dt, trunc(sysdate))
            and dovr.effdt = (select max(effdt) from ci_per_char where per_id = dovr.per_id and char_type_cd = dovr.char_type_cd and sysdate>effdt);
           else --Для УРДЛО вытаскиваем Начальников управления соответствующих ОСЭ
           SELECT nvl(max(perc2.adhoc_char_val),'__________________') , ', действующего на основании доверенности ' || decode(max(dovr.adhoc_char_val) , null , '_______________________' ,  max(dovr.adhoc_char_val))
             into v_sogl_rec.FIO_SIGN ,  v_sogl_rec.DOVR_NUM
                  FROM CI_CASE T
            INNER JOIN CI_WF_PROC_CHAR ACCT ON T.ACCT_ID = ACCT.CHAR_VAL_FK1 AND ACCT.CHAR_TYPE_CD = 'LIC-S4ET'
            INNER JOIN CI_WF_PROC PROC_MAIN ON PROC_MAIN.WF_PROC_ID = ACCT.WF_PROC_ID AND PROC_MAIN.WF_PROC_TMPL_CD = 'DZ-MAIN'
            INNER JOIN CI_WF_PROC_CHAR DZ_DPRT ON DZ_DPRT.WF_PROC_ID = PROC_MAIN.WF_PROC_ID AND DZ_DPRT.CHAR_TYPE_CD = 'DZ-DPRT'
            INNER JOIN CM_WF_DEPARTMENT DEP ON DEP.CM_WF_DEPARTMENT_CD = DZ_DPRT.CHAR_VAL_FK1
            INNER JOIN CI_PER_PER PP ON PP.PER_ID1 = DEP.PER_ID AND PP.PER_REL_TYPE_CD = 'DL-175' --Директор ОСЭ
            INNER JOIN CI_PER PER2 ON PP.PER_ID2 = PER2.PER_ID
            INNER JOIN CI_PER_CHAR perc2 ON perc2.per_id = PER2.PER_ID AND perc2.Char_Type_Cd = 'FIO-SIGN'
            LEFT  JOIN ci_per_char dovr on dovr.per_id = per2.per_id and dovr.char_type_cd = 'DOVER-SG'
               WHERE T.CASE_ID = i_case_id
                 and PROC_MAIN.WF_STAT_FLG <> 20
                 and trunc(sysdate) between pp.start_dt and nvl(pp.end_dt, trunc(sysdate))
                 and (dovr.effdt is null or dovr.effdt = (select max(effdt) from ci_per_char where per_id = dovr.per_id and char_type_cd = dovr.char_type_cd and sysdate>effdt));
            end if;
         end if;

      begin
      select distinct evt.wf_evt_type_cd ----узнаем тип графика
        into v_evt_type
      from ci_wf_evt_char c1
           inner join ci_wf_evt_char c2 on c1.wf_proc_id=c2.wf_proc_id and c1.evt_seq=c2.evt_seq and c2.char_type_cd='RSTR-GRF'
           inner join ci_wf_evt evt on evt.wf_proc_id = c2.char_val_fk1 and evt.wf_evt_type_cd in  ('DZ-RSTR-PAY','DZ-RSTR-PENY')
      where trim(c1.char_val_fk1)=trim(i_case_id);
      exception
        when too_many_rows then --временно, до поры пока юристы не предоставят бланк для смешанного набора графиков
         v_evt_type := 'DZ-RSTR-PAY';
      end;

      if (to_number(sogl.s_amt4, '999999999999990.99') > 0 and to_number(sogl.s_amt2, '999999999999990.99') = 0 and to_number(sogl.s_amt8, '999999999999990.99') = 0 and to_number(sogl.s_amt0 , '999999999999990.99') = 0 ) then v_sogl_rec.SOGL_TXT1 := 'по оплате штрафной неустойки';
      elsif (to_number(sogl.s_amt7, '999999999999990.99') > 0 and to_number(sogl.s_amt2, '999999999999990.99') = 0 and to_number(sogl.s_amt8, '999999999999990.99') = 0 and to_number(sogl.s_amt0, '999999999999990.99') = 0) then v_sogl_rec.SOGL_TXT1 := 'по оплате штрафной неустойки в виде пени';
      else v_sogl_rec.SOGL_TXT1 := '';
      end if;

      --Список ПТ
      for lst_kpd in (
       SELECT DISTINCT
      ' № ' || FD_NUM.adhoc_char_val || ' от ' || FD_DATE.adhoc_char_val || ' г.' FD,
      cm_util.char_val_to_date(FD_DATE.adhoc_char_val) FD_DAT
             FROM ci_wf_evt_char c1
       inner join ci_wf_evt_char c2 on c2.wf_proc_id = c1.wf_proc_id and c2.evt_seq = c1.evt_seq and c2.char_type_cd = 'RSTR-FIN'
       INNER JOIN CI_ADJ_CHAR AC ON AC.CHAR_TYPE_CD = 'FTR-ID' AND AC.CHAR_VAL_FK1 = C2.CHAR_VAL_FK1
       INNER JOIN CI_FT FT ON FT.FT_TYPE_FLG = 'AD' AND FT.SIBLING_ID = AC.ADJ_ID
       INNER JOIN CM_ACCOUNT_ENTRY AE ON AE.CM_GENERATIVE_FT_ID = FT.FT_ID
       INNER JOIN CI_CASE FD ON AE.CM_FIN_DOCUMENT_ID = FD.CASE_ID
       INNER JOIN CI_CASE_CHAR FD_NUM  ON FD_NUM.CASE_ID = FD.CASE_ID AND FD_NUM.CHAR_TYPE_CD IN ('INVNUM','BILLNUM')
       INNER JOIN CI_CASE_CHAR FD_DATE ON FD_DATE.CASE_ID = FD.CASE_ID AND FD_DATE.CHAR_TYPE_CD IN ('INVDATEC','BILLDATE')
       LEFT  JOIN CI_CASE_CHAR INVOICING ON INVOICING.CASE_ID=FD.CASE_ID AND INVOICING.CHAR_TYPE_CD = 'IS-INVG'
             WHERE trim(c1.char_val_fk1) = trim(i_case_id)
               AND FD.CASE_TYPE_CD IN ('INVOICE','BILL','INVOICE-CORR','INVOICEPRNF','INVOICCEPRNF')
               AND (INVOICING.CASE_ID IS NULL OR INVOICING.CHAR_VAL<>'PAYMENT')
               AND C1.CHAR_TYPE_CD = 'PRINTDOC'
      order by 2
       )
        loop
          if length(v_sogl_rec.pt_list)>0  then v_sogl_rec.pt_list:= v_sogl_rec.pt_list||', '; end if;
          v_sogl_rec.pt_list := v_sogl_rec.pt_list || lst_kpd.FD;
        end loop;

       if (v_evt_type = 'DZ-RSTR-PAY' and length(v_sogl_rec.pt_list)>0) then
        v_sogl_rec.pt_list := v_sogl_rec.pt_list || ' за потребленную электроэнергию.';
       end if;

      if v_evt_type = 'DZ-RSTR-PAY'
      then v_sogl_rec.SOGL_TXT :=
'3. Однократное нарушение Потребителем условий настоящего соглашения, а также условий оплаты текущего энергопотребления по Договору с даты заключения настоящего соглашения дает право Гарантирующему поставщику требовать полного погашения задолженности с даты просрочки, либо в одностороннем порядке расторгнуть настоящее соглашение.
       4. При однократном нарушении условий настоящего соглашения Гарантирующий поставщик вправе ограничить режим потребления энергоресурсов Потребителю в порядке, установленном действующем законодательством.
       5. Вне зависимости от согласования новых сроков оплаты задолженности, установленных п. 2 настоящего Соглашения, Гарантирующий поставщик продолжает начислять пени, установленные Договором, на сумму задолженности за потребленную электрическую энергию, указанную в п. 1 настоящего Соглашения до даты фактической её оплаты, а Потребитель обязуется оплачивать пени.';
      else       v_sogl_rec.SOGL_TXT :=
'3. Однократное нарушение Потребителем условий настоящего соглашения, дает право Гарантирующему поставщику требовать полного погашения задолженности по оплате штрафной неустойки с даты просрочки, либо в одностороннем порядке расторгнуть настоящее соглашение. В этом случае соглашение является расторгнутым с даты его заключения.
       4. За неисполнение либо ненадлежащее исполнение обязательств по настоящему соглашению Потребитель несет ответственность в соответствии с действующим законодательством РФ.
       5. Настоящее соглашение вступает в силу с момента его подписания и действует до полного исполнения Потребителем своих обязательств по настоящему соглашению.';
      end if;
      pipe row(v_sogl_rec);
    end loop;
  end get_sogl_restr_select;
  --
  function get_graphic_restr_select (i_case_id in char) return T_GRAPHIC_RESTR_TAB pipelined is
    v_graphic_restr T_GRAPHIC_RESTR_REC;
    v_wf_proc_id char(10);
    v_num number(3);
  begin
    select c2.char_val_fk1 into v_wf_proc_id
    from ci_wf_evt_char c1
         inner join ci_wf_evt_char c2
         on c1.wf_proc_id=c2.wf_proc_id and c1.evt_seq=c2.evt_seq and c2.char_type_cd='RSTR-GRF'
    where trim(c1.char_val_fk1)=trim(i_case_id);
    --
    v_num:= 1;
    for a1 in (select t1.*
               from ci_wf_evt_char t1
               where t1.wf_proc_id = v_wf_proc_id and t1.char_type_cd = 'PLAT-DAT'
               order by to_date(TRIM(t1.adhoc_char_val), 'dd.mm.yyyy')
               )
    loop
      v_graphic_restr.wf_proc_id:= a1.wf_proc_id;
      v_graphic_restr.EVT_SEQ:= a1.evt_seq;
      v_graphic_restr.PLAT_NUM:= v_num;
      v_graphic_restr.PLAT_DAT:= cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-DAT');
      v_graphic_restr.PLAT_SUM_RUB:= money_delimited(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM'), 'RUB');
      v_graphic_restr.PLAT_SUM_KOP:= money_delimited(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM'), 'KOP');
      v_graphic_restr.PLAT_SUM_WORD := cm_lan_rep.number2word(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM'));
      v_graphic_restr.PLAT_SUM_WORD := REPLACE(v_graphic_restr.PLAT_SUM_WORD , 'рублей' , 'руб.');
      v_graphic_restr.PLAT_SUM_WORD := REPLACE(v_graphic_restr.PLAT_SUM_WORD , 'копеек' , 'коп.');
      pipe row(v_graphic_restr);
      v_num:= v_num+1;
    end loop;
  end get_graphic_restr_select;
  --
  /*Задание на ограничение*/
  function get_td_lim_select(i_case_id in char) return T_TD_LIM_SET pipelined is
    v_tfg_rec T_TD_LIM_REC;
    v_eo_id varchar2(10);
  begin
    for tfg in (select dc1.case_id, wf1.wf_proc_id, we1.evt_seq, ac1.acct_id, dc1.doc_num, dc1.doc_dat,
                ac1.dog_num, ac1.dog_dat, wf1.wf_amount_rub, wf1.wf_amount_kop,
                ac1.main_name, ac1.main_postal, ac1.main_address,
                wf1.officer_name,
                cm_dz.get_wfpr_chty(wf1.wf_proc_id, 'DAT-TFG') DAT_TFG,
                cm_dz.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-OPLZ') DAT_OPLZ,
                cm_dz.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMS') DAT_LIMS,
                cm_dz.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMP') DAT_LIMP,
                cm_dz.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMT') DAT_LIMT,
                cm_dz.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMA') DAT_LIMA,
                cm_dz.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIM') DAT_LIM,
                cm_dz.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'TIM-LIM') TIM_LIM
                from
                  cm_doc_case_vw dc1
                  inner join cm_acct_vw ac1 on ac1.acct_id=dc1.acct_id
                  inner join cm_wf_evt_vw we1 on we1.printdoc=dc1.case_id
                  inner join cm_wf_vw wf1 on wf1.wf_proc_id=we1.wf_proc_id
                where
                  dc1.case_id= i_case_id)

    loop
      v_tfg_rec.wf_proc_id:= tfg.wf_proc_id;
      v_tfg_rec.case_id:= tfg.case_id;

      v_tfg_rec.TFG_num:= cm_dz.get_wfpr_chty(tfg.wf_proc_id, 'NUM-TFG');
      v_tfg_rec.TFG_dat:= cm_dz.get_wfpr_chty(tfg.wf_proc_id, 'DAT-TFG');

      v_tfg_rec.td_lim_num:= cm_dz.get_case_chty(tfg.case_id, 'DOC-NUM');
      v_tfg_rec.td_lim_dat:= cm_dz.get_case_chty(tfg.case_id, 'DOC-DAT');
      v_tfg_rec.dog_num:= tfg.dog_num;
      v_tfg_rec.dog_dat:= tfg.dog_dat;

      v_tfg_rec.DAT_TFG:= tfg.DAT_TFG;
      v_tfg_rec.DAT_OPLZ:= tfg.DAT_OPLZ;
      v_tfg_rec.DAT_LIMS:= tfg.DAT_LIMS;
      v_tfg_rec.DAT_LIMP:= tfg.DAT_LIMP;
      v_tfg_rec.DAT_LIMT:= tfg.DAT_LIMT;
      v_tfg_rec.DAT_LIMA:= tfg.DAT_LIMA;
      v_tfg_rec.DAT_LIM:= tfg.DAT_LIM;
      v_tfg_rec.TIM_LIM:= tfg.TIM_LIM;
      --
      v_tfg_rec.executor_name:= tfg.officer_name;
      v_tfg_rec.executor_tel:= '22-33-22';
      --
      v_tfg_rec.main_name:= tfg.main_name;
      v_tfg_rec.main_postal:= tfg.main_postal;
      v_tfg_rec.main_address:= tfg.main_address;
      v_tfg_rec.pt_list:= get_pt_list(tfg.wf_proc_id); -- cm_fd.fd4wf(tfg.wf_proc_id);
      --v_tfg_rec.wf_amount_rub:= tfg.wf_amount_rub;
      --v_tfg_rec.wf_amount_kop:= tfg.wf_amount_kop;
      v_tfg_rec.wf_amount_rub:= money_delimited(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMOUNT'), 'RUB');
      v_tfg_rec.wf_amount_kop:= money_delimited(cm_dz_chty.get_wfpr_chty(tfg.wf_proc_id, 'AMOUNT'), 'KOP');
      --
      v_tfg_rec.signer_job:= get_tfg_signer_job(cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'DZ-TPMNG'), '9900000196');
      v_tfg_rec.signer_name:= get_tfg_signer_name(cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'DZ-TPMNG'), '9900000196');
      -- Первое ограничение
      if cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')= 'Y' and
         cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'PRIL4530')= 'N'
      then
        v_tfg_rec.txt_limit1:= ' до уровня аварийной брони';
      elsif cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-TB')= 'Y'
      then
        v_tfg_rec.txt_limit1:= ' до уровня технологической брони';
      else
        v_tfg_rec.txt_limit1:= '';
      end if;
      -- Второе ограничение
      if cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'SW-AB')= 'Y' and
         cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'PRIL4530')= 'Y'
      then
        v_tfg_rec.txt_limit2:= ' ограничена до аварийной брони';
        v_tfg_rec.txt_limit3:= ' ограничением';
      else
        v_tfg_rec.txt_limit2:= ' полностью прекращена';
        v_tfg_rec.txt_limit3:= ' прекращением';
      end if;
      --
      select eo_code, eo_name, eo_district, eo_postal, eo_address, get_conn_name(eo_id)
      into v_tfg_rec.eo_code, v_tfg_rec.eo_name, v_tfg_rec.eo_district,
           v_tfg_rec.eo_postal, v_tfg_rec.eo_address, v_tfg_rec.EO_CONN_NAME
      from table(get_eo_list_4_wf_proc(tfg.wf_proc_id))
      where rownum=1;
      --
      select  max(t1.phone) TEL_SEC, max(t2.phone) FAKS
      into v_tfg_rec.MAIN_TEL_SEC, v_tfg_rec.main_fax
      from ci_acct_per ac1
          left join ci_per_phone t1 on ac1.per_id=t1.per_id and t1.phone_type_cd='TEL_SEC'
          left join ci_per_phone t2 on ac1.per_id=t2.per_id and t2.phone_type_cd='FAKS'
      where ac1.acct_id=tfg.acct_id and ac1.acct_rel_type_cd='OSNOV'
      group by ac1.acct_id;
      --
      pipe row(v_tfg_rec);
    end loop;
  end get_td_lim_select;
  --
  function get_notice_id(i_case_id in char) return varchar2 is
    v_doc_id ci_case.case_id%type;
  begin
    select c3.char_val_fk1 into v_doc_id
    from
      ci_case c1
      inner join ci_case_char c2 on c2.case_id=c1.case_id and c2.char_type_cd='PCZ-EVT '
      inner join ci_wf_proc_char c3 on c3.wf_proc_id=c2.char_val_fk1 and c3.char_type_cd='NOTICE  '
    where
      c1.case_id = i_case_id;
    --
    return v_doc_id;
  end;
  --
  function get_notice(i_case_id in char) return t_notice_set pipelined is
    v_notice_rec t_notice_rec;
    v_proc_type cm_wf_proc_type.cm_wf_proc_type_cd%type;
    v_lim_wf_proc_id ci_wf_proc.wf_proc_id%type;
    v_ntc_wf_proc_id ci_wf_proc.wf_proc_id%type;
    v_wf_proc_id ci_wf_proc.wf_proc_id%type;
  begin
    select wf2.wf_proc_id, wf2.char_val_fk1 into v_wf_proc_id, v_proc_type
    from
    ci_case c1
    inner join ci_case_char c2 on c2.case_id=c1.case_id and c2.char_type_cd='PCZ-EVT'
    inner join ci_wf_proc_char wf2 on wf2.wf_proc_id = c2.char_val_fk1 and wf2.char_type_cd='PROC_TP'
    where
      c1.case_id=i_case_id and rownum=1;
    --
    select wc1.adhoc_char_val dat_lim1, wc2.adhoc_char_val tim_lim1
    into v_notice_rec.NTC_DAT_LIM, v_notice_rec.NTC_TIM_LIM
    from
      ci_case c1
      inner join ci_case_char c2 on c2.case_id=c1.case_id and c2.char_type_cd='PCZ-EVT'
      inner join ci_wf_proc wlim on wlim.wf_proc_id=c2.char_val_fk1
      inner join ci_wf_proc_char wcn on wcn.wf_proc_id=wlim.cr_by_wf_proc_id and wcn.char_type_cd='NOTICE'
      inner join ci_case_char ntc on ntc.case_id=wcn.char_val_fk1 and ntc.char_type_cd='PCZ-EVT'
      left join ci_wf_evt_char wc1 on wc1.wf_proc_id=ntc.char_val_fk1 and wc1.evt_seq=ntc.char_val_fk2
        and wc1.char_type_cd ='DAT-LIM'
      left join ci_wf_evt_char wc2 on wc2.wf_proc_id=ntc.char_val_fk1 and wc2.evt_seq=ntc.char_val_fk2
        and wc2.char_type_cd ='TIM-LIM'
    where
      c1.case_id = i_case_id;
    --
    pipe row(v_notice_rec);
  end get_notice;
  --
  function get_notice4doc(i_case_id in char) return t_notice_set pipelined is
    v_notice_rec t_notice_rec;
  begin
    for a1 in (select * from table(get_notice(i_case_id)))
    loop
      v_notice_rec:= a1;
    end loop;
    --
    pipe row(v_notice_rec);
  end get_notice4doc;
  --
  function get_doc_sum(i_case_id in char) return t_doc_sum_set pipelined is
    v_doc_sum_rec t_doc_sum_rec;
    v_proc_type cm_wf_proc_type.cm_wf_proc_type_cd%type;
    v_wf_proc_id ci_wf_proc.wf_proc_id%type;
    v_evt_seq ci_wf_evt.evt_seq%type;
    v_wf_evt_type_cd ci_wf_evt.wf_evt_type_cd%type;
  begin
    v_doc_sum_rec.doc_id:= i_case_id;
    --
    select wf2.wf_proc_id,  wf2.char_val_fk1, we1.evt_seq, we1.wf_evt_type_cd
    into v_wf_proc_id, v_proc_type, v_evt_seq, v_wf_evt_type_cd
    from
    ci_case c1
    inner join ci_case_char c2 on c2.case_id=c1.case_id and c2.char_type_cd='PCZ-EVT'
    inner join ci_wf_evt we1 on we1.wf_proc_id = c2.char_val_fk1 and we1.evt_seq=c2.char_val_fk2
    inner join ci_wf_proc_char wf2 on wf2.wf_proc_id = c2.char_val_fk1 and wf2.char_type_cd='PROC_TP'
    where
      c1.case_id=i_case_id and rownum=1;
    --
    begin
      if (v_proc_type='NOTIFICATION')or(v_proc_type='COLLECTION') then
        select adhoc_char_val into v_doc_sum_rec.doc_amount
        from ci_wf_proc_char
        where wf_proc_id=v_wf_proc_id and char_type_cd='WZ-AMT';
      end if;
      if (trim(v_proc_type)='LIMITATION') then
        select sum(ae.cur_amt) sum_eo into v_doc_sum_rec.doc_amount
        from
          ci_case c1
          join ci_case_char c2 on c1.case_id=c2.case_id and c2.char_type_cd='PCZ-EVT'
          join ci_wf_proc wp_lim on wp_lim.wf_proc_id=c2.char_val_fk1
          join ci_wf_proc_char wc1 on wc1.wf_proc_id = wp_lim.wf_proc_id  and wc1.char_type_cd='OB-OBSL' --все ЭО
          join ci_wf_proc wp_ntf on wp_ntf.wf_proc_id=wp_lim.cr_by_wf_proc_id
          join ci_wf_proc_char wc2 on wc2.wf_proc_id=wp_ntf.wf_proc_id and wc2.char_type_cd='DZ-DRAFT' --все ПД
          join ci_case_char cc_me on cc_me.case_id=wc2.char_val_fk1 and cc_me.char_type_cd='ME-ID'
          join ci_ft ft on (ft.match_evt_id=cc_me.char_val_fk1 and ft.accounting_dt<=sysdate)
          join cm_account_entry ae on (ae.cm_generative_ft_id=ft.ft_id and ae.accounting_dt<=sysdate)
          join cm_rate_prop rp on (rp.cm_rate_prop_id = ae.cm_rate_prop_id and rp.accounting_dt<=sysdate)
          join cm_sa_exp se on (se.sa_id = rp.sa_id)
          join cm_eo_rate er on (er.cm_eo_rate_id=se.cm_eo_rate_id and er.cm_start_date<=sysdate and (er.cm_end_date is null or er.cm_end_date>=sysdate))
          join cm_eo_account_linkage eal on (eal.cm_eo_account_linkage_id=er.cm_eo_account_linkage_id
          and eal.start_dt<=sysdate and (eal.end_dt is null or eal.end_dt>=sysdate) and eal.cm_eo_id = wc1.char_val_fk1)
        where c1.case_id= i_case_id;
        --
        if (trim(v_wf_evt_type_cd)='DZ_CC_SLF') then
          begin
          select adhoc_char_val sum_eo into v_doc_sum_rec.doc_amount
          from ci_wf_evt_char
          where wf_proc_id= v_wf_proc_id and evt_seq=v_evt_seq and char_type_cd='AMOUNT';
          exception
            when no_data_found then v_doc_sum_rec.doc_amount:= '0.00';
          end;
        end if;
          --
        if (trim(v_wf_evt_type_cd)='DZ_CC_SLF') then
          v_doc_sum_rec.WF_PT_LIST:= get_evt_pt_list(v_wf_proc_id, v_evt_seq);
        else
          v_doc_sum_rec.WF_PT_LIST:= get_pt_list(v_wf_proc_id);
        end if;
      end if;
    exception
      when no_data_found then v_doc_sum_rec.doc_amount:= '0.00';
    end;
    -- v_doc_sum_rec.doc_amount:= cm_dz_chty.get_wfpr_chty(v_wf_proc_id, 'WZ-AMT'); -- 'AMOUNT'
    v_doc_sum_rec.doc_amount_rub:= money_delimited(v_doc_sum_rec.doc_amount, 'RUB');
    v_doc_sum_rec.doc_amount_kop:= money_delimited(v_doc_sum_rec.doc_amount, 'KOP');
    v_doc_sum_rec.doc_amount:= money_delimited(v_doc_sum_rec.doc_amount, 'FULL');
    --
    v_doc_sum_rec.proc_type:= v_proc_type;
    -- Сумма по характеристике процесса AMOUNT
    begin
      select adhoc_char_val into v_doc_sum_rec.PROC_AMOUNT
      from ci_wf_proc_char where wf_proc_id= v_wf_proc_id and char_type_cd='AMOUNT';
    exception
      when no_data_found then v_doc_sum_rec.proc_amount:= '0.00';
    end;
    --
    if (trim(v_proc_type)='LIMITATION') then
      begin
        select wf2.adhoc_char_val sum_eo into v_doc_sum_rec.proc_amount
        from
          ci_wf_proc_char wf1
          join ci_wf_proc_char wf2 on wf1.char_val_fk1=wf2.wf_proc_id
            and wf1.char_val_fk2=wf2.char_type_cd and wf1.char_val_fk3=wf2.seq_num
        where
          wf1.wf_proc_id= v_wf_proc_id and wf1.char_type_cd='LWZ-AMT';
      exception
        when no_data_found then v_doc_sum_rec.proc_amount:= '0.00';
      end;
    end if;

    v_doc_sum_rec.proc_amount_rub:= money_delimited(v_doc_sum_rec.proc_amount, 'RUB');
    v_doc_sum_rec.proc_amount_kop:= money_delimited(v_doc_sum_rec.proc_amount, 'KOP');
    v_doc_sum_rec.proc_amount:= money_delimited(v_doc_sum_rec.proc_amount, 'FULL');
    --
    pipe row(v_doc_sum_rec);
  end get_doc_sum;
  -- **********************************************************************************
  function get_doc_head(i_case_id in char) return T_DOC_HEAD_SET pipelined is
    v_doc_head_rec t_doc_head_rec;
    v_tel_sec ci_per_phone.phone%type;
    v_tel_deg ci_per_phone.phone%type;
    v_tel_fax ci_per_phone.phone%type;
    v_signer_default ci_per.per_id%type;
    v_cis_division ci_acct.cis_division%type;
  begin
    v_doc_head_rec.doc_id:= i_case_id;
    v_doc_head_rec.doc_num := cm_dz_chty.get_case_chty(i_case_id, 'DOC-NUM');
    v_doc_head_rec.doc_dat:= cm_dz_chty.get_case_chty(i_case_id, 'DOC-DAT');

    select acct_id into v_doc_head_rec.acct_id
    from ci_case where case_id= i_case_id;
    --
    select acct.cis_division
    into v_cis_division
    from ci_acct acct
    where acct.acct_id = v_doc_head_rec.acct_id;
    --
    begin
      select cast(char_val_fk1 as char(10)), to_number(char_val_fk2)
      into v_doc_head_rec.wf_proc_id , v_doc_head_rec.evt_seq
      from ci_case_char
      where case_id = i_case_id and char_type_cd = 'PCZ-EVT';
    exception
      when no_data_found then begin
        v_doc_head_rec.wf_proc_id:= '0';
        v_doc_head_rec.evt_seq:= 0;
      end;
    end;
    
    v_doc_head_rec.dz_dprt:= cm_dz_chty.get_wfpr_chty(v_doc_head_rec.wf_proc_id, 'DZ-DPRT');
    
    --
    /*Данные предыдущего уведомления, берем на событии EVT-EBLIM-05*/
    begin
      select ec.char_val_fk1
      into v_doc_head_rec.doc_id_prev
      from ci_wf_evt e
      inner join ci_wf_evt_char ec on ec.wf_proc_id = e.wf_proc_id and ec.evt_seq = e.evt_seq and ec.char_type_cd = 'PRINTDOC'
      where e.wf_proc_id = v_doc_head_rec.wf_proc_id
       and e.wf_evt_type_cd IN ('EVT-EBLIM-05', 'EVT-EBLIM-03');
    exception when no_data_found then v_doc_head_rec.doc_id_prev := null;
    end; 
    
    if v_doc_head_rec.doc_id_prev is not null then
      v_doc_head_rec.doc_num_prev := cm_dz_chty.get_case_chty(v_doc_head_rec.doc_id_prev, 'DOC-NUM');
      v_doc_head_rec.doc_dat_prev := cm_dz_chty.get_case_chty(v_doc_head_rec.doc_id_prev, 'DOC-DAT');
    end if;
    --
    /*Данные предыдущего уведомления, берем на событии EVT-EBNTF-20*/
    begin
      select ec.char_val_fk1
      into v_doc_head_rec.doc_id_prev_ntf
      from ci_wf_proc wp 
      inner join ci_wf_evt e on e.wf_proc_id = wp.cr_by_wf_proc_id and e.wf_evt_type_cd = 'EVT-EBNTF-20'
      inner join ci_wf_evt_char ec on ec.wf_proc_id = e.wf_proc_id and ec.evt_seq = e.evt_seq and ec.char_type_cd = 'PRINTDOC'
      where wp.wf_proc_id = v_doc_head_rec.wf_proc_id;       
    exception when no_data_found then v_doc_head_rec.doc_id_prev_ntf := null;
    end; 
    
    if v_doc_head_rec.doc_id_prev_ntf is not null then
      v_doc_head_rec.doc_num_prev_ntf := cm_dz_chty.get_case_chty(v_doc_head_rec.doc_id_prev_ntf, 'DOC-NUM');
      v_doc_head_rec.doc_dat_prev_ntf := cm_dz_chty.get_case_chty(v_doc_head_rec.doc_id_prev_ntf, 'DOC-DAT');
    end if;
    --      
    begin
      select u1.last_name||' '||u1.first_name EXECUTOR_NAME, trim(pp.phone) EXECUTOR_TEL, FAKS.PHONE, EMAIL.ADHOC_CHAR_VAL
      into v_doc_head_rec.executor_name, v_doc_head_rec.executor_tel, v_doc_head_rec.EXECUTOR_FAX, v_doc_head_rec.EXECUTOR_EMAIL
      from
        ci_wf_proc_char wc1
        inner join sc_user u1 on u1.user_id = rpad(wc1.char_val_fk1,8) and wc1.char_type_cd='EXECUTOR'
        left join sc_user_char uc1 on uc1.user_id=u1.user_id and uc1.char_type_cd='OFFICER '
        left join ci_per_phone pp on pp.per_id=cast(uc1.char_val_fk1 as char(10)) and pp.phone_type_cd='RAB'
        left join ci_per_phone FAKS on FAKS.per_id=cast(uc1.char_val_fk1 as char(10)) and FAKS.phone_type_cd='FAKS'
        left join ci_per_char EMAIL on EMAIL.PER_ID = cast(uc1.char_val_fk1 as char(10)) and EMAIL.CHAR_TYPE_CD = 'EMAIL'       
      where wc1.wf_proc_id = v_doc_head_rec.wf_proc_id
        and (uc1.seq_num is null or uc1.seq_num = (select max(uc2.seq_num) from sc_user_char uc2 where uc2.user_id = uc1.user_id and uc2.char_type_cd = uc1.char_type_cd))
        and (pp.seq_num is null or pp.seq_num = (select max(pp2.seq_num) from ci_per_phone pp2 where pp2.per_id = pp.per_id and pp2.phone_type_cd = pp.phone_type_cd))
        and (EMAIL.effdt is null or EMAIL.effdt=(select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = EMAIL.PER_ID and pc2.char_type_cd = EMAIL.CHAR_TYPE_CD));
    exception
      when no_data_found then
        begin
        Select u1.last_name||' '||u1.first_name EXECUTOR_NAME, trim(pp.phone) EXECUTOR_TEL, FAKS.PHONE, EMAIL.ADHOC_CHAR_VAL
          into v_doc_head_rec.executor_name, v_doc_head_rec.executor_tel, v_doc_head_rec.EXECUTOR_FAX, v_doc_head_rec.EXECUTOR_EMAIL
        from ci_case_char c
        inner join sc_user u1 on u1.user_id = rpad(c.char_val_fk1,8) 
        left  join sc_user_char uc1 on uc1.user_id=u1.user_id and uc1.char_type_cd='OFFICER '
        left  join ci_per_phone pp on pp.per_id=cast(uc1.char_val_fk1 as char(10)) and pp.phone_type_cd='RAB'
        left join ci_per_phone FAKS on FAKS.per_id=cast(uc1.char_val_fk1 as char(10)) and FAKS.phone_type_cd='FAKS' 
        left join ci_per_char EMAIL on EMAIL.PER_ID = cast(uc1.char_val_fk1 as char(10)) and EMAIL.CHAR_TYPE_CD = 'EMAIL'                     
        where c.case_id = i_case_id and c.char_type_cd = 'EXECUTOR'
          and (uc1.seq_num is null or uc1.seq_num = (select max(uc2.seq_num) from sc_user_char uc2 where uc2.user_id = uc1.user_id and uc2.char_type_cd = uc1.char_type_cd))        
          and (pp.seq_num is null or pp.seq_num = (select max(pp2.seq_num) from ci_per_phone pp2 where pp2.per_id = pp.per_id and pp2.phone_type_cd = pp.phone_type_cd))
          and (EMAIL.effdt is null or EMAIL.effdt=(select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = EMAIL.PER_ID and pc2.char_type_cd = EMAIL.CHAR_TYPE_CD));          
        exception
          when no_data_found then
        v_doc_head_rec.executor_name:= 'Не определен';
        end;
    end;
    --
    select t1.adhoc_char_val into v_doc_head_rec.dog_num
    from ci_acct_char t1 where t1.acct_id=v_doc_head_rec.acct_id and t1.char_type_cd='DOG-NUM'
    and (t1.effdt is null or t1.effdt =
    (select max(t2.effdt) from ci_acct_char t2
     where t1.char_type_cd=t2.char_type_cd and t2.acct_id=t1.acct_id and t2.effdt <= sysdate)) ;
    --
    select to_char(setup_dt, 'dd.mm.yyyy') into v_doc_head_rec.dog_dat
    from ci_acct where acct_id = v_doc_head_rec.acct_id;
    --
    select t1.char_val, cvl.descr 
    into v_doc_head_rec.dog_form, v_doc_head_rec.dog_form_descr  
    from ci_acct_char t1, ci_char_val_l cvl 
    where t1.acct_id=v_doc_head_rec.acct_id 
      and t1.char_type_cd='DOG-FORM'
      and cvl.char_type_cd = t1.char_type_cd
      and cvl.char_val = t1.char_val
      and cvl.language_cd = 'RUS'
      and t1.effdt =
          (select max(t2.effdt) from ci_acct_char t2
           where t2.acct_id=t1.acct_id and t1.char_type_cd=t2.char_type_cd);
    --
    SELECT COALESCE(CM_PER_UTILS.get_per_by_per_rel_type(CM_CASE_UTILS.get_cust_id(i_case_id), 'DL-1', to_date(CM_DZ_CHTY.get_case_chty(i_case_id, 'DOC-DAT'),'dd.mm.yyyy')),CM_PER_UTILS.get_per_by_per_rel_type(CM_CASE_UTILS.get_cust_id(i_case_id), 'DL-50', to_date(CM_DZ_CHTY.get_case_chty(i_case_id, 'DOC-DAT'),'dd.mm.yyyy')))
    INTO v_doc_head_rec.RUK_ID
    FROM DUAL;
    --    
    select
      mp1.per_id, /*mpn1.entity_name*/pc.adhoc_char_val main_name, /*trim(mpa1.postal)*/ trim(main_per.postal) main_postal,
      /*mpa1.address1*/ /*addr.adhoc_char_val*/ pc_adr.adhoc_char_val main_address, phr.phone main_tel, phf.phone main_faks
    into
      v_doc_head_rec.main_per_id, v_doc_head_rec.main_name, v_doc_head_rec.main_postal,
      v_doc_head_rec.main_address, v_doc_head_rec.main_tel, v_doc_head_rec.main_fax
    from
      ci_acct ac1
      --join ci_acct_char addr on addr.acct_id = ac1.acct_id and addr.char_type_cd = 'F-ADR-1'
      join ci_acct_per mp1 on mp1.acct_id=ac1.acct_id and mp1.main_cust_sw='Y'
      /*TR-21515. Novikova. Наименование потребителя берем с хар-ки*/
      inner join ci_per_char pc on pc.per_id = mp1.per_id and pc.char_type_cd = 'SHORT-NM'
              and pc.effdt = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = pc.per_id and pc2.char_type_cd = pc.char_type_cd)
      inner join ci_per_char pc_adr on pc_adr.per_id = mp1.per_id and pc_adr.char_type_cd = 'F-ADR-1'
              and pc_adr.effdt = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = pc_adr.per_id and pc2.char_type_cd = pc_adr.char_type_cd)              
      join ci_per_name mpn1 on mpn1.per_id=mp1.per_id and mpn1.prim_name_sw='Y'
      join ci_per_addr_ovrd mpa1 on mpa1.per_id=mp1.per_id
      join Ci_per main_per on main_per.per_id = mp1.per_id
      left join ci_per_phone phr on phr.per_id=mp1.per_id and phr.phone_type_cd='RAB'
      left join ci_per_phone phf on phf.per_id=mp1.per_id and phf.phone_type_cd='FAKS'
    where
      ac1.acct_id = v_doc_head_rec.acct_id and rownum=1;
    -- Определение телефонов
    v_tel_deg:= cm_dz_chty.get_per_phone(v_doc_head_rec.MAIN_PER_ID , 'TEL_DEG     ');
    v_tel_sec:= cm_dz_chty.get_per_phone(v_doc_head_rec.MAIN_PER_ID , 'TEL_SEC     ');
    if length(trim(v_tel_deg))> 0 then
      v_doc_head_rec.main_tel:= v_tel_deg;
    elsif length(trim(v_tel_sec))> 0 then
      v_doc_head_rec.main_tel:= v_tel_sec;
    end if;
    v_doc_head_rec.main_fax := cm_dz_chty.get_per_phone(v_doc_head_rec.MAIN_PER_ID , 'FAKS        ');
    --
    begin
      select trim(pn_est.entity_name)||' '||trim(pn_bil.entity_name) MAIN_NAME
      into v_doc_head_rec.SPEC_CALC
      from
      ci_acct ac
        left join ci_acct_per ap_est on ap_est.acct_id =ac.acct_id  and ap_est.acct_rel_type_cd='ESTIMATR'
        left join ci_per_name pn_est on pn_est.per_id=ap_est.per_id and pn_est.prim_name_sw='Y'
        left join ci_acct_per ap_bil on ap_bil.acct_id=ap_est.acct_id and ap_bil.acct_rel_type_cd='BILLING'
        left join ci_per_name pn_bil on pn_bil.per_id=ap_bil.per_id and pn_bil.prim_name_sw='Y'
      where
        ac.acct_id=v_doc_head_rec.acct_id;
    exception
      when no_data_found then v_doc_head_rec.SPEC_CALC:= '_____________';
    end;
    --
    BEGIN
      SELECT CM_DZ_CHTY.get_per_chty(ap.per_id, 'JOBD-SRC', to_date(v_doc_head_rec.doc_dat,'dd.mm.yyyy')) AS ESTIMATR_JOB ,
        CM_DZ_CHTY.get_per_chty(ap.per_id, 'DL$N-RP', to_date(v_doc_head_rec.doc_dat,'dd.mm.yyyy'))       AS ESTIMATR_JOB_RP ,
        CM_DZ_CHTY.get_per_chty(ap.per_id, 'NAIM-PLN', to_date(v_doc_head_rec.doc_dat,'dd.mm.yyyy'))      AS ESTIMATR_NAIM_PLN ,
        CM_DZ_CHTY.get_per_chty(ap.per_id, 'FIO-RP', to_date(v_doc_head_rec.doc_dat,'dd.mm.yyyy'))        AS ESTIMATR_FIO_RP ,
        CM_DZ_CHTY.get_per_chty(ap.per_id, 'PHONE', to_date(v_doc_head_rec.doc_dat,'dd.mm.yyyy'))         AS ESTIMATR_PHONE
      INTO v_doc_head_rec.ESTIMATR_JOB,
        v_doc_head_rec.ESTIMATR_JOB_RP,
        v_doc_head_rec.ESTIMATR_NAIM_PLN,
        v_doc_head_rec.ESTIMATR_FIO_RP,
        v_doc_head_rec.ESTIMATR_PHONE
      FROM ci_acct_per ap
      WHERE ap.acct_id       =v_doc_head_rec.acct_id
      AND ap.acct_rel_type_cd='ESTIMATR';
    EXCEPTION
    WHEN no_data_found THEN
      v_doc_head_rec.ESTIMATR_JOB     := NULL;
      v_doc_head_rec.ESTIMATR_JOB_RP  := NULL;
      v_doc_head_rec.ESTIMATR_NAIM_PLN:= NULL;
      v_doc_head_rec.ESTIMATR_FIO_RP  := NULL;
      v_doc_head_rec.ESTIMATR_PHONE   := NULL;
    END;
    --
    --v_doc_head_rec.main_name:= tfg.main_name;
    --v_doc_head_rec.main_postal:= tfg.main_postal;
    --v_doc_head_rec.main_address:= tfg.main_address;
    --v_doc_head_rec.MAIN_FAX
    --v_doc_head_rec.MAIN_TEL_SEC
    --v_doc_head_rec.MAIN_TEL_DEG
    --v_doc_head_rec.SIGNER_JOB:= get_tfg_signer_job(cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'DZ-TPMNG'), '9900000196');
    --v_doc_head_rec.SIGNER_NAME:= get_tfg_signer_name(cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'DZ-TPMNG'), '9900000196');
    --
/*     for r in (
      select --на базе cur_dz_rc_st_r1
             ap_mn.per_id main_per_id,
             ppm.phone main_fax,
             ppts.phone main_tel_sec,
             pptg.phone main_tel_deg,
             cm_dz_rep.get_tfg_signer_job(cm_dz.get_case_chty(i_case_id, 'DZ-TPMNG'), '9900000196') signer_job,
             cm_dz_rep.get_tfg_signer_name(cm_dz.get_case_chty(i_case_id, 'DZ-TPMNG'), '9900000196') signer_name,
             trim(prem_cod.adhoc_char_val) EO_CODE,
             trim(pn_mn.entity_name) EO_NAME,
             cm_char_utils.get_prem_bigcharval(wpc_eo.char_val_fk1, 'DISTRICT', sysdate) eo_district,
             trim(prem.postal) eo_postal,
             trim(prem_adr1.adhoc_char_val)||trim(prem_adr2.adhoc_char_val) EO_ADDRESS,
             cm_dz_rep.get_prem_char_val(prem_cod.prem_id, 'PR-OUTAG') sw_in_dog,
             cm_dz_rep.get_conn_name(prem_cod.prem_id) eo_conn_name
      from dual
           join ci_wf_evt_char wec on wec.char_val_fk1=i_case_id and wec.char_type_cd='PRINTDOC'
           join ci_wf_proc_char wpc_eo on wpc_eo.wf_proc_id=wec.wf_proc_id and wpc_eo.char_type_cd='OB-OBSL '
           join ci_prem_char prem_cod on prem_cod.prem_id = wpc_eo.char_val_fk1 and prem_cod.char_type_cd = 'PREM_COD' and prem_cod.EFFDT = (select max(o.EFFDT) from CI_PREM_CHAR o where o.PREM_ID = prem_cod.PREM_ID and o.CHAR_TYPE_CD = prem_cod.CHAR_TYPE_CD and o.EFFDT <= sysdate)
  --         join ci_prem_char prem_nam on prem_nam.prem_id = wpc_eo.char_val_fk1 and prem_nam.char_type_cd = 'PREM-NAM' and prem_nam.EFFDT = (select max(o.EFFDT) from CI_PREM_CHAR o where o.PREM_ID = prem_nam.PREM_ID and o.CHAR_TYPE_CD = prem_nam.CHAR_TYPE_CD and o.EFFDT <= sysdate)
           join ci_prem prem on prem.prem_id = wpc_eo.char_val_fk1
           join ci_prem_char prem_adr1 on prem_adr1.prem_id = wpc_eo.char_val_fk1 and prem_adr1.char_type_cd = 'F-ADR-1' and prem_adr1.EFFDT = (select max(o.EFFDT) from CI_PREM_CHAR o where o.PREM_ID = prem_adr1.PREM_ID and o.CHAR_TYPE_CD = prem_adr1.CHAR_TYPE_CD and o.EFFDT <= sysdate)
           left join ci_prem_char prem_adr2 on prem_adr2.prem_id = wpc_eo.char_val_fk1 and prem_adr2.char_type_cd = 'F-ADR-2' and prem_adr2.EFFDT  <= sysdate
           join cm_eo_account_linkage eal on eal.cm_eo_id=wpc_eo.char_val_fk1 and eal.start_dt<=sysdate and (eal.end_dt is null or eal.end_dt>=sysdate)
           left join ci_acct_per ap_est on ap_est.acct_id=eal.acct_id and ap_est.acct_rel_type_cd='ESTIMATR'
           left join ci_per_name pn_est on pn_est.per_id=ap_est.per_id and pn_est.prim_name_sw='Y'
           left join ci_acct_per ap_bil on ap_bil.acct_id=eal.acct_id and ap_bil.acct_rel_type_cd='BILLING'
           left join ci_per_name pn_bil on pn_bil.per_id=ap_bil.per_id and pn_bil.prim_name_sw='Y'
           join ci_acct_per ap_mn on ap_mn.acct_id=eal.acct_id and ap_mn.main_cust_sw='Y'
           join ci_per_name pn_mn on pn_mn.per_id=ap_mn.per_id and pn_mn.prim_name_sw='Y'
           left join ci_per_phone ppm on ppm.per_id=ap_mn.per_id and ppm.phone_type_cd='FAKS'
           left join ci_per_phone ppts on ppts.per_id=ap_mn.per_id and ppts.phone_type_cd='TEL_SEC'
           left join ci_per_phone pptg on pptg.per_id=ap_mn.per_id and pptg.phone_type_cd='TEL_DEG'
      where (prem_adr2.EFFDT is null or prem_adr2.EFFDT = (select max(o.EFFDT) from CI_PREM_CHAR o where o.PREM_ID = prem_adr2.PREM_ID and o.CHAR_TYPE_CD = prem_adr2.CHAR_TYPE_CD and o.EFFDT <= sysdate))
    )loop

      v_doc_head_rec.main_fax:=r.main_fax;
      v_doc_head_rec.main_tel_sec:=r.main_tel_sec;
      v_doc_head_rec.main_tel_deg:=r.main_tel_deg;
      v_doc_head_rec.signer_job:=r.signer_job;
      v_doc_head_rec.signer_name:=r.signer_name;
      --

     --v_doc_eo_rec.WF_AMOUNT_RUB
      v_doc_eo_rec.eo_code:=r.eo_code;
      v_doc_eo_rec.eo_name:=r.eo_name;
      v_doc_eo_rec.eo_district:=r.eo_district;
      v_doc_eo_rec.eo_postal:=r.eo_postal;
      v_doc_eo_rec.eo_address:=r.eo_address;
      v_doc_eo_rec.sw_in_dog:=r.sw_in_dog;
        v_doc_head_rec.eo_conn_name:=r.eo_conn_name;
    end loop;*/
  --
     if trim(v_cis_division) = 'OMS2' then
        begin
		  select cm_dz_chty.get_case_chty(doc_claim.char_val_fk1,'SIGNERID') per_id
		    into v_signer_default
		    from ci_case_char doc_claim
		   where doc_claim.case_id = i_case_id 
		     and doc_claim.char_type_cd = 'REESTR';
        exception when others then
          v_signer_default := 6398780652;
        end;
     elsif trim(v_cis_division) = 'ORL1'
        then v_signer_default := 1243232308;
     else v_signer_default := null;
     end if;
                 
      begin
      select nvl(SHORT_NM.ADHOC_CHAR_VAL, nm.entity_name), dlz.adhoc_char_val, trim(replace(DOVER_SG.ADHOC_CHAR_VAL,'доверенности',''))
        into v_doc_head_rec.SIGNER_NAME,v_doc_head_rec.SIGNER_JOB, v_doc_head_rec.SIGNER_DOVER
      from ci_per c
      left join ci_per_name nm on nm.per_id = c.per_id and nm.name_type_flg = 'PRIM'
      left join CI_PER_CHAR SHORT_NM on SHORT_NM.per_id = c.per_id and SHORT_NM.char_type_cd = 'SHORT-NM'      
      left join CI_PER_CHAR dlz on dlz.per_id = c.per_id and dlz.char_type_cd = 'DL$N-RUK'
      left join CI_PER_CHAR DOVER_SG on DOVER_SG.per_id = c.per_id and DOVER_SG.char_type_cd = 'DOVER-SG'      
      where c.per_id = nvl(cm_dz_chty.get_case_chty(i_case_id, 'SIGNERID'), v_signer_default)
        and (SHORT_NM.effdt is null or SHORT_NM.effdt = (select max(dlz2.effdt) from CI_PER_CHAR dlz2 where dlz2.per_id = SHORT_NM.per_id and dlz2.char_type_cd = SHORT_NM.char_type_cd))      
        and (dlz.effdt is null or dlz.effdt = (select max(dlz2.effdt) from CI_PER_CHAR dlz2 where dlz2.per_id = dlz.per_id and dlz2.char_type_cd = dlz.char_type_cd))
        and (DOVER_SG.effdt is null or DOVER_SG.effdt = (select max(dlz2.effdt) from CI_PER_CHAR dlz2 where dlz2.per_id = DOVER_SG.per_id and dlz2.char_type_cd = DOVER_SG.char_type_cd));        
      exception when no_data_found then v_doc_head_rec.SIGNER_NAME := null; v_doc_head_rec.SIGNER_JOB := null; v_doc_head_rec.SIGNER_DOVER := null;
      end;
    --  
    begin
      select per_phone.adhoc_char_val
      into v_doc_head_rec.RUK_TEL
      from ci_per_per pp
      inner join ci_per_char per_phone on per_phone.per_id = pp.per_id2 and per_phone.char_type_cd = 'PHONE'
          and per_phone.effdt = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = per_phone.per_id and pc2.char_type_cd = per_phone.char_type_cd)
      where pp.per_id1 = v_doc_head_rec.main_per_id
        and pp.per_rel_type_cd = RPAD('DL-1',8)
        and trunc(sysdate) between pp.start_dt and nvl(pp.end_dt, trunc(sysdate));
    exception when no_data_found then v_doc_head_rec.RUK_TEL:= NULL;    
    end;
    --
    pipe row(v_doc_head_rec);
  end get_doc_head;
  --
  function get_doc_eo(i_case_id in char) return t_eo_set pipelined is
    v_eo_rec t_eo_rec;
    v_wf_proc_id char(10);
  begin
    for eo1 in (
      select
        pr1.prem_id,
        cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'PREM_COD', current_date) EO_CODE,
        cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'PREM-NAM', current_date) EO_NAME,
        cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'DISTRICT', current_date) EO_DISTRICT,
        TRIM(pr1.postal) EO_POSTAL,
        cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'F-ADR-1', current_date) EO_ADDRESS,
        get_prem_char_val(pr1.prem_id, 'PR-OUTAG') sw_in_dog_val,
        get_prem_char_val_name(pr1.prem_id, 'GRUPEIEO') eo_group_ei
        --(select cp1.char_val from ci_prem_char cp1
        -- where cp1.prem_id=pr1.prem_id and cp1.char_type_cd='PR-OUTAG') sw_in_dog
      from
        ci_case c1
        join ci_case_char dc1 on dc1.case_id=c1.case_id and dc1.char_type_cd='PCZ-EVT'
        join ci_wf_proc proc on proc.wf_proc_id = rpad(dc1.char_val_fk1, 10)
        join ci_wf_proc_char wf1 on wf1.wf_proc_id=proc.cr_by_wf_proc_id and wf1.char_type_cd='OB-OBSL '
        join ci_prem pr1 on pr1.prem_id=rpad(wf1.char_val_fk1,10)
      where
        c1.case_id=i_case_id and rownum=1)
    loop
      v_eo_rec.eo_id:= eo1.prem_id;
      v_eo_rec.eo_code:= eo1.eo_code;
      v_eo_rec.eo_name:= eo1.eo_name;
      v_eo_rec.eo_district:= eo1.eo_district;
      v_eo_rec.eo_postal:= eo1.eo_postal;
      v_eo_rec.eo_address:= eo1.eo_address;
      v_eo_rec.EO_GROUP_EI:= eo1.eo_group_ei;
      select descr into v_eo_rec.sw_in_dog
      from ci_char_val_l where char_type_cd= 'PR-OUTAG' and char_val= cast(eo1.sw_in_dog_val as char(16)) and language_cd='RUS';
      -- v_eo_rec.sw_in_dog:= eo1.sw_in_dog_val;
      -- Точка поключения
      begin
    select wm_concat(ch2.descr||' '||ph1.adhoc_char_val) tp into v_eo_rec.POINT_SERVICE_NAME
    from
      ci_prem cp1
      inner join cm_prem_link l1 on cp1.prem_id=l1.cm_source_prem_id
      inner join cm_connection_schema cs1 on cs1.prem_id=cp1.prem_id
      inner join cm_connection_schema_element cse1 on cse1.cm_connection_schema_id=cs1.cm_connection_schema_id
      inner join cm_eo_net_node en1 on en1.cm_eo_net_node_id=cse1.cm_eo_net_node_id
      inner join ci_prem oeh on oeh.prem_id=en1.prem_id and oeh.prem_type_cd='OEH'
      inner join ci_prem_char ph1 on ph1.char_type_cd='PREM-NAM' and oeh.prem_id=ph1.prem_id
      inner join ci_prem_char ph2 on ph2.char_type_cd='VID-OEH' and oeh.prem_id=ph2.prem_id
      inner join ci_char_val_l ch2 on ch2.char_type_cd='VID-OEH' and ch2.char_val=ph2.char_val and ch2.language_cd='RUS'
      inner join ci_wf_proc_char wc1 on wc1.char_type_cd='OB-OBSL' and wc1.char_val_fk1=l1.cm_dest_prem_id
      inner join ci_case_char dc1 on dc1.char_type_cd='PCZ-EVT' and dc1.char_val_fk1=wc1.wf_proc_id
    where  cp1.prem_type_cd='CP' and dc1.case_id= i_case_id
      and cse1.seq_num=
      (select max(seq_num) from
      cm_connection_schema_element cse2
      inner join cm_eo_net_node en2 on en2.cm_eo_net_node_id=cse2.cm_eo_net_node_id
      inner join ci_prem oeh2 on oeh2.prem_id=en2.prem_id and oeh2.prem_type_cd='OEH'
      where cse2.cm_connection_schema_id=cs1.cm_connection_schema_id);
    exception
      when no_data_found then v_eo_rec.point_service_name:= '';
    end;
    --
    begin
      select rtrim(xmlagg(xmlelement("a", trim(cm_char_utils.get_prem_bigcharval(prem_id, 'TU-NUM', sysdate)) || ', ')).extract('/a/text()').getstringval(), ', ')
      into v_eo_rec.EO_TU_NUMS
      from ci_prem where prem_type_cd like 'IK%' and trim(cm_char_utils.get_prem_bigcharval(prem_id, 'TU-NUM', sysdate))<>'000'
      CONNECT BY PRIOR prem_id = prnt_prem_id
      start with prnt_prem_id=eo1.prem_id;
    exception
      when no_data_found then v_eo_rec.EO_TU_NUMS:= '';
    end;
    --
    begin
      select t2.adhoc_char_val into v_eo_rec.DAT_LIM
      from ci_case_char t1 inner join ci_wf_evt_char t2 on
        t1.char_type_cd='PCZ-EVT ' and
        t1.char_val_fk1=t2.wf_proc_id and t1.char_val_fk2=t2.evt_seq and t2.char_type_cd='DAT-LIM'
      where
        t1.case_id = i_case_id and rownum=1 ;
    exception
      when no_data_found then v_eo_rec.dat_lim:= '';
    end;

    /*19574 dresviannikov. Убираю дубли*/
    Select wm_concat (str) into v_eo_rec.POINT_SERVICE_NAME from (
      SELECT distinct regexp_substr(str, '[^,]+', 1, level) str
   FROM (SELECT v_eo_rec.POINT_SERVICE_NAME as str FROM dual) t
   CONNECT BY instr(str, ',', 1, level - 1) > 0);

    --
      pipe row(v_eo_rec);
    end loop;

  end get_doc_eo;
  --
  function get_doc_lim(i_case_id in char) return t_doc_lim_set pipelined is
    v_doc_lim_rec t_doc_lim_rec;
    v_char_val ci_char_val.char_val%type;
    v_cp_prem_id ci_prem.prem_id%type;
    v_eo_id ci_prem.prem_id%type;

    v_wf_proc_id ci_wf_evt.wf_proc_id%type;
    v_evt_seq ci_wf_evt.evt_seq%type;
    v_SUM_CP_MAX_P number(14,2);
    v_DZ_R_MOD     ci_wf_evt_char.char_val%type; 
    v_ACCT_ID      ci_acct.acct_id%type;
    v_DOC_DAT      varchar2(10);
    v_count_eo     number;
    N1             number;
    N2             number;
    N3             number;
    N4             number;
    N5             number;     
    NUM1           number;
    NUM2           number;
    NUM3           number;
    NUM4           number;
    NUM5           number;                  
  begin
  --
    v_doc_lim_rec.PT_LIST:= get_pt_list_lim(i_case_id);
    -- Определение id процесса и события
    begin
      select wf_proc_id, evt_seq
      into v_wf_proc_id, v_evt_seq
      from ci_case_char c1 join ci_wf_evt we1 on we1.wf_proc_id=c1.char_val_fk1 and we1.evt_seq=c1.char_val_fk2
      where c1.case_id=i_case_id and c1.char_type_cd='PCZ-EVT ';
    exception
        when no_data_found then v_wf_proc_id:= ' ';
    end;
    --
    begin
    select wf1.adhoc_char_val, DAT_LIMP.adhoc_char_val, DAT_LIMT.ADHOC_CHAR_VAL, DAT_LIMA.ADHOC_CHAR_VAL, DAT_LIMS.ADHOC_CHAR_VAL, DZ_R_MOD.CHAR_VAL, LIC_S4ET.CHAR_VAL_FK1, DOC_DAT.ADHOC_CHAR_VAL 
    into v_doc_lim_rec.LIM_DATE , v_doc_lim_rec.DAT_LIMP, v_doc_lim_rec.DAT_LIMT, v_doc_lim_rec.DAT_LIMA, v_doc_lim_rec.DAT_LIMS, v_DZ_R_MOD, v_ACCT_ID, v_DOC_DAT
    from
      ci_case c1
      inner join ci_case_char c2 on c1.case_id=c2.case_id and c2.char_type_cd='PCZ-EVT'
      inner join ci_case_char DOC_DAT on DOC_DAT.case_id=c1.case_id and DOC_DAT.char_type_cd='DOC-DAT'          
      /*Дата планируемого ограничения*/
      left join ci_wf_proc_char wf1 on wf1.wf_proc_id= c2.char_val_fk1 and wf1.char_type_cd='DAT-LIM'
      /*Дата частичного ограничения*/
      left join ci_wf_proc_char DAT_LIMP on DAT_LIMP.wf_proc_id= c2.char_val_fk1 and DAT_LIMP.char_type_cd='DAT-LIMP'
      /*Дата ограничения до ТБ*/
      left join ci_wf_proc_char DAT_LIMT on DAT_LIMT.wf_proc_id= c2.char_val_fk1 and DAT_LIMT.char_type_cd='DAT-LIMT'
      /*Дата ограничения до АБ*/
      left join ci_wf_proc_char DAT_LIMA on DAT_LIMA.wf_proc_id= c2.char_val_fk1 and DAT_LIMA.char_type_cd='DAT-LIMA'
      /*Дата самоограничения*/ 
      left join ci_wf_proc_char DAT_LIMS on DAT_LIMS.wf_proc_id= c2.char_val_fk1 and DAT_LIMS.char_type_cd='DAT-LIMS'  
      left join ci_wf_evt_char DZ_R_MOD on DZ_R_MOD.WF_PROC_ID = c2.char_val_fk1 and DZ_R_MOD.CHAR_TYPE_CD = 'DZ-R-MOD' and DZ_R_MOD.EVT_SEQ = v_evt_seq   
      /**/
      join ci_wf_proc_char LIC_S4ET on LIC_S4ET.wf_proc_id=c2.char_val_fk1 and LIC_S4ET.char_type_cd='LIC-S4ET'                 
    where
      c1.case_id= i_case_id;
    exception
        when no_data_found then v_doc_lim_rec.DAT_LIMP:= ' '; v_doc_lim_rec.DAT_LIMT := NULL; v_doc_lim_rec.DAT_LIMA := NULL; v_doc_lim_rec.DAT_LIMS := null; v_ACCT_ID := null;
    end;
    -- Id оператора ограничения и его тип
    begin
      select opog.char_val_fk1, eo.char_val_fk1, cvl.descr
      into v_doc_lim_rec.OPER_LIM_ID, v_eo_id, v_doc_lim_rec.OPER_LIM_TYPE
      from
        ci_case doc
        inner join ci_case_char wfid on doc.case_id=wfid.case_id and wfid.char_type_cd='PCZ-EVT'
        /*TR-20388. Novikova. Согласно замечания: исполнителя ограничения берем из процесса, а не из события*/
        --inner join ci_wf_evt_char opog on opog.wf_proc_id=wfid.char_val_fk1 and opog.evt_seq  = to_number(wfid.char_val_fk2) and opog.char_type_cd='OPER-LIM'
        inner join ci_wf_proc_char opog on opog.wf_proc_id=wfid.char_val_fk1 and opog.char_type_cd='OPER-LIM'
        inner join ci_wf_proc proc on opog.wf_proc_id = proc.wf_proc_id
        /**/
        inner join ci_wf_proc_char eo on eo.wf_proc_id=proc.cr_by_wf_proc_id and eo.char_type_cd='OB-OBSL ' -- ЭО
        left join ci_wf_proc_char TPOPRLIM on TPOPRLIM.wf_proc_id=wfid.char_val_fk1 and TPOPRLIM.char_type_cd='TPOPRLIM'
        left join ci_char_val_l cvl on cvl.char_type_cd = TPOPRLIM.CHAR_TYPE_CD and cvl.char_val = TPOPRLIM.CHAR_VAL and cvl.language_cd = 'RUS'        
      where
        doc.case_id = i_case_id and rownum=1;
    exception
        when no_data_found then v_doc_lim_rec.OPER_LIM_ID:= ''; v_doc_lim_rec.OPER_LIM_TYPE := ''; v_eo_id := null;
    end;
    
      /*Найдем Величину технологической брони*/
  begin
      select to_char(per.cm_power_value)
      into v_doc_lim_rec.eo_tb
      from 
      (select q.case_id doc_id, q.start_dt, q.end_dt
      from (
        select
           c.case_id, c.case_status_cd
           ,to_date(cm_char_utils.get_entity_bigcharval('CASE', c.case_id, 'DOC-DAT'), 'DD.MM.YYYY') start_dt
           ,to_date(cm_char_utils.get_entity_bigcharval('CASE', c.case_id, 'DAT-END'), 'DD.MM.YYYY') end_dt
        from ci_case c
        where c.prem_id = v_eo_id
        and c.case_type_cd = 'BR-AKT'
      ) q
      where to_date(nvl(v_doc_lim_rec.LIM_DATE, v_doc_lim_rec.PROC_DAT_LIMF), 'dd.mm.yyyy') between q.start_dt and coalesce(q.end_dt, to_date(nvl(v_doc_lim_rec.LIM_DATE, v_doc_lim_rec.PROC_DAT_LIMF), 'dd.mm.yyyy'))
     ) doc
      inner join cm_reservation br on br.cm_reservation_document_id = doc.doc_id and br.cm_reservation_type_cd = 'TECHNOLOGICAL'
      inner join cm_reservation_tot_value per on per.cm_reservation_id = br.cm_reservation_id; 
  exception when no_data_found then v_doc_lim_rec.eo_tb := 'НЕТ';       
  end;

  /*Найдем Величину аварийной брони*/
  begin
      select to_char(per.cm_power_value)
      into v_doc_lim_rec.eo_ab
      from 
      (select q.case_id doc_id, q.start_dt, q.end_dt
      from (
        select
           c.case_id, c.case_status_cd
           ,to_date(cm_char_utils.get_entity_bigcharval('CASE', c.case_id, 'DOC-DAT'), 'DD.MM.YYYY') start_dt
           ,to_date(cm_char_utils.get_entity_bigcharval('CASE', c.case_id, 'DAT-END'), 'DD.MM.YYYY') end_dt
        from ci_case c
        where c.prem_id = v_eo_id
        and c.case_type_cd = 'BR-AKT'
      ) q
      where to_date(nvl(v_doc_lim_rec.LIM_DATE, v_doc_lim_rec.PROC_DAT_LIMF), 'dd.mm.yyyy') between q.start_dt and coalesce(q.end_dt, to_date(nvl(v_doc_lim_rec.LIM_DATE, v_doc_lim_rec.PROC_DAT_LIMF), 'dd.mm.yyyy'))
     ) doc
      inner join cm_reservation br on br.cm_reservation_document_id = doc.doc_id and br.cm_reservation_type_cd = 'EMERGENCY'
      inner join cm_reservation_tot_value per on per.cm_reservation_id = br.cm_reservation_id; 
  exception when no_data_found then v_doc_lim_rec.eo_ab := 'НЕТ';       
  end; 

      /*Уровень вводимых ограничений, это сумма максимальной мощности всех ТП на процессе ограничения*/
    begin
      select 
      --SUM(TO_NUMBER(replace(TRIM(cp_max_p.adhoc_char_val),',', '.'), '9999999990D00' /*'99999999990D99'*/))
         ROUND(SUM(CM_UTIL.char_val_to_number(TRIM(cp_max_p.adhoc_char_val))), 2)           
      into v_SUM_CP_MAX_P
      from ci_wf_proc proc
        inner join ci_wf_proc_char CP on CP.wf_proc_id=proc.wf_proc_id and CP.char_type_cd='POINTDLV'  -- ТП
        inner join ci_prem_char cp_max_p on cp_max_p.prem_id = CP.CHAR_VAL_FK1 and cp_max_p.char_type_cd = 'CP-MAX-P'
          and cp_max_p.effdt = (select max(pc2.effdt) from ci_prem_char pc2 where pc2.prem_id = cp_max_p.prem_id and pc2.char_type_cd = cp_max_p.char_type_cd)       
      where proc.wf_proc_id = v_wf_proc_id;
    exception
        when no_data_found then v_SUM_CP_MAX_P := 0;
    end;  
    
    v_doc_lim_rec.LEVEL_LIMIT := TRIM(TO_CHAR(v_SUM_CP_MAX_P, '99999999990D99'));
       
       begin
       select count(*)
       into v_count_eo
       from cm_eo_account_linkage eal
       where eal.acct_id = v_ACCT_ID
       and to_date(v_DOC_DAT,'dd.mm.yyyy') between eal.start_dt and nvl(eal.end_dt, to_date(v_DOC_DAT,'dd.mm.yyyy'));    
       exception when no_data_found then v_count_eo:=0;
       end;
    
    --if v_doc_lim_rec.DAT_LIMP is not null and trim(v_DZ_R_MOD) = 1 then
    if v_count_eo = 1 then
      v_doc_lim_rec.CP_MAX_P := TRIM(TO_CHAR(v_SUM_CP_MAX_P * 0.5, '99999999990D99'));
      else
       v_doc_lim_rec.CP_MAX_P :=TRIM(TO_CHAR(v_SUM_CP_MAX_P, '99999999990D99'));
    end if;
      
                 
    -- Причина восстановления подключения
    begin
      /*select cvl.descr
      into  v_doc_lim_rec.PRICH_VK
      from ci_wf_proc_char PRICH_VK
      inner join ci_char_val_l cvl on cvl.char_type_cd = PRICH_VK.CHAR_TYPE_CD and cvl.char_val = PRICH_VK.Char_Val and cvl.language_cd = 'RUS'
      where PRICH_VK.wf_proc_id= v_wf_proc_id and PRICH_VK.char_type_cd='PRICH-VK';*/

      --dresviannikov TR-19574. Причину берем из события
       select cvl.descr
      into  v_doc_lim_rec.PRICH_VK
      from ci_wf_evt_char PRICH_VK
      inner join ci_char_val_l cvl on cvl.char_type_cd = PRICH_VK.CHAR_TYPE_CD and cvl.char_val = PRICH_VK.Char_Val and cvl.language_cd = 'RUS'
      where PRICH_VK.wf_proc_id= v_wf_proc_id and PRICH_VK.EVT_SEQ = v_evt_seq  and PRICH_VK.char_type_cd='PRICH-VK';
    exception when no_data_found then v_doc_lim_rec.PRICH_VK := '';
    end;
    -- Название и телефон оператора ограничения
    begin
      select pn.entity_name, cm_char_utils.get_entity_bigcharval('PER', pr.per_id, 'DISTR-DP'),-- pn2.entity_name,
      ph.phone
      into v_doc_lim_rec.oper_lim_name, v_doc_lim_rec.OPER_LIM_DISTR_RP,
      v_doc_lim_rec.oper_lim_tel
      from
        ci_per pr
        left join ci_per_name pn on pr.per_id = pn.per_id and pn.name_type_flg = 'PRIM'
        left join ci_per_name pn2 on pr.per_id = pn2.per_id and pn2.name_type_flg = 'DOCR'
        left join ci_per_phone ph on pr.per_id = ph.per_id and ph.phone_type_cd ='RAB         '
      where
        pr.per_id = v_doc_lim_rec.oper_lim_id;
    exception
        when no_data_found then v_doc_lim_rec.OPER_LIM_NAME:= 'Не найден субъект';
    end;
    -- Телефон КС
    begin
      select t1.adhoc_char_val into v_doc_lim_rec.oper_lim_tel
      from ci_per_char t1
      where t1.per_id= v_doc_lim_rec.oper_lim_id and t1.char_type_cd='DISTR-TD' and t1.effdt=
      (select effdt from ci_per_char t2 where t2.per_id=t1.per_id and t2.char_type_cd=t1.char_type_cd);
    exception
      when no_data_found then v_doc_lim_rec.oper_lim_tel:= '-';
    end;
    -- Район КС
    begin
      select t1.adhoc_char_val into v_doc_lim_rec.OPER_LIM_DISTRICT
      from ci_per_char t1
      where t1.per_id= v_doc_lim_rec.oper_lim_id and t1.char_type_cd='DISTR-DP' and t1.effdt=
      (select effdt from ci_per_char t2 where t2.per_id=t1.per_id and t2.char_type_cd=t1.char_type_cd);
    exception
      when no_data_found then v_doc_lim_rec.OPER_LIM_DISTRICT:= ' ';
    end;
    -- Дополнительный номер документа
     IF v_doc_lim_rec.DAT_LIMS IS NOT NULL then N1:=1; else N1:=0; end if; 
     IF v_doc_lim_rec.DAT_LIMP IS NOT NULL  then N2:=1; else N2:=0; end if; 
     IF v_doc_lim_rec.DAT_LIMA IS NOT NULL  then N3:=1; else N3:=0; end if; 
     IF v_doc_lim_rec.DAT_LIMT IS NOT NULL  then N4:=1; else N4:=0; end if; 
     IF v_doc_lim_rec.LIM_DATE IS NOT NULL  then N5:=1; else N5:=0; end if; 
     NUM1 := N1;
     NUM2 := NUM1 + N2;
     NUM3 := NUM2 + N3;
     NUM4 := NUM3 + N4;
     NUM5 := NUM4 + N5;

     v_doc_lim_rec.S1 := '_'|| NUM1;
     v_doc_lim_rec.S2 := '_'|| NUM2;
     v_doc_lim_rec.S3 := '_'|| NUM3;
     v_doc_lim_rec.S4 := '_'|| NUM4;
     v_doc_lim_rec.S5 := '_'|| NUM5;
                  
    
   /* begin
      select cv1.char_val, cv1.descr--, ec2.adhoc_char_val
      into v_char_val, v_doc_lim_rec.AREA_KS--, v_doc_lim_rec.proc_dat_limf
      from
        ci_case c1
        inner join ci_case_char c2 on c1.case_id=c2.case_id and c2.char_type_cd= 'PCZ-EVT '
        inner join ci_wf_evt we1 on we1.wf_proc_id=c2.char_val_fk1 and we1.evt_seq=c2.char_val_fk2
        left join ci_wf_evt_char wec1 on wec1.wf_proc_id=we1.wf_proc_id and wec1.evt_seq=we1.evt_seq and wec1.char_type_cd='AREA-KS'
        left join ci_char_val_l cv1 on cv1.char_type_cd=wec1.char_type_cd and cv1.char_val=wec1.char_val and cv1.language_cd='RUS'
        left join ci_wf_proc_char ec2 on ec2.wf_proc_id=we1.wf_proc_id and ec2.char_type_cd='DZT-LIMF'
    where
      c1.case_id = i_case_id;
    exception
        when no_data_found then v_char_val:= '';
    end;   */
    -- ОЕХ
/*    begin
      select distinct oeh.prem_id, cp1.prem_id --, en1.prem_id
      into v_doc_lim_rec.OEH_ID, v_cp_prem_id
      from
        ci_prem cp1
        inner join cm_prem_link l1 on cp1.prem_id=l1.cm_source_prem_id
        inner join cm_connection_schema cs1 on cs1.prem_id=cp1.prem_id
        inner join cm_connection_schema_element cse1 on
          cse1.cm_connection_schema_id=cs1.cm_connection_schema_id and cse1.seq_num=
          (select max(seq_num) from
          cm_connection_schema_element cse2
          inner join cm_eo_net_node en2 on en2.cm_eo_net_node_id=cse2.cm_eo_net_node_id
          inner join ci_prem oeh2 on oeh2.prem_id=en2.prem_id and oeh2.prem_type_cd='OEH'
          where cse2.cm_connection_schema_id=cs1.cm_connection_schema_id)
        inner join cm_eo_net_node en1 on en1.cm_eo_net_node_id=cse1.cm_eo_net_node_id
        inner join ci_prem oeh on oeh.prem_id=en1.prem_id and oeh.prem_type_cd='OEH'

        inner join ci_prem_char bp1 on oeh.prem_id=bp1.prem_id and bp1.char_type_cd='RAJON_ES' and bp1.effdt =
        (select max(bp3.effdt) from ci_prem_char bp3 where bp3.prem_id=bp1.prem_id and bp3.char_type_cd=bp1.char_type_cd)
        inner join ci_wf_proc_char wc1 on wc1.char_type_cd='OB-OBSL' and wc1.char_val_fk1=l1.cm_dest_prem_id
        inner join ci_case_char doc on doc.char_type_cd='PCZ-EVT' and doc.char_val_fk1=wc1.wf_proc_id

      where
        cp1.prem_type_cd='CP'
        and rownum=1
        --and l1.cm_dest_prem_id = '3666896470' -- EO
        and bp1.char_val_fk1 = v_doc_lim_rec.OPER_LIM_ID
        and doc.case_id = i_case_id;
    exception
        when no_data_found then v_doc_lim_rec.OEH_ID:= '';
    end;*/
    -- Снять напряжение
/*    begin
      select distinct t7.descr into v_doc_lim_rec.STOP_VOLT
      from
        ci_prem t1
        inner join ci_sp t2 on t2.prem_id=t1.prem_id
        inner join ci_sp_eq t3 on t3.sp_id=t2.sp_id
        inner join ci_item t4 on t4.item_id=t3.item_id_eq
        inner join ci_item_char t5 on t5.item_id=t4.item_id and t5.char_type_cd='NAPR-GRN' and t5.effdt=
          (select max(t6.effdt) from ci_item_char t6 where t6.item_id=t5.item_id and t6.char_type_cd=t5.char_type_cd)
        inner join ci_char_val_l t7 on t7.char_type_cd=t5.char_type_cd and t7.char_val=t5.char_val and t7.language_cd='RUS'
      where
        t1.prem_id= v_cp_prem_id;
    exception
        when no_data_found then v_doc_lim_rec.STOP_VOLT:= '';
    end;
    v_doc_lim_rec.PRIS_P:= v_doc_lim_rec.STOP_VOLT;
    -- Получатель для КС
    begin
      select t3.entity_name
      into v_doc_lim_rec.KS_RECEIVER
      from
        ci_prem t1
        inner join ci_prem_char t2 on t2.prem_id=t1.prem_id and t2.char_type_cd='RAJON_ES'
        inner join ci_per_name t3 on t3.per_id = t2.char_val_fk1 and t3.name_type_flg='DOCR'
      where
        t1.prem_id=v_doc_lim_rec.OEH_ID;
    exception
      when no_data_found then v_doc_lim_rec.OEH_ID:= '';
    end;
    --
    v_doc_lim_rec.LIM_TIME:= '00 час. 00 мин.';
    --
    begin
      --select rtrim(xmlagg(xmlelement("a", REPLACE(substr(SHEMA_VIEW,instr(SHEMA_VIEW,'->','-1')),'-> ') || ', ')).extract('/a/text()').getstringval(), ', ')
      --into v_doc_lim_rec.T_LOC
      --from table(cm_lan_utils.GET_CONNCTION_CHEMA_VIEW('',v_eo_id));
      select ch1.DESCR||' - '||pch2.adhoc_char_val
      into v_doc_lim_rec.T_LOC
      from
        ci_prem oeh
        inner join ci_prem_char pch1 on pch1.prem_id=oeh.prem_id and pch1.char_type_cd='VID-OEH ' and pch1.effdt =
          (select effdt from ci_prem_char t1 where t1.prem_id=pch1.prem_id and t1.char_type_cd=pch1.char_type_cd)
        inner join ci_char_val_l ch1 on ch1.char_type_cd=pch1.char_type_cd and pch1.char_val=ch1.char_val and ch1.language_cd='RUS'
        inner join ci_prem_char pch2 on pch2.prem_id=oeh.prem_id and pch2.char_type_cd='PREM-NAM' and pch2.effdt =
          (select effdt from ci_prem_char t1 where t1.prem_id=pch2.prem_id and t1.char_type_cd=pch2.char_type_cd)
      where
        oeh.prem_id = v_doc_lim_rec.OEH_ID;
    exception
      when no_data_found then v_doc_lim_rec.T_LOC:= '';
    end;*/
    -- Дата и номер задания ограничения
    begin
      select c1.adhoc_char_val doc_num, c2.adhoc_char_val doc_dat
      into v_doc_lim_rec.DOC_LIM_NUM, v_doc_lim_rec.DOC_LIM_DAT
      from
        ci_wf_evt t1
        join ci_wf_evt_char t2 on t1.wf_proc_id=t2.wf_proc_id and
          t1.evt_seq=t2.evt_seq and t2.char_type_cd='OPER-LIM'
          and t2.char_val_fk1=v_doc_lim_rec.OPER_LIM_ID
        join ci_wf_evt_char t3 on t1.wf_proc_id=t3.wf_proc_id and t1.evt_seq=t3.evt_seq and t3.char_type_cd='PRINTDOC'
        join ci_case_char c1 on c1.case_id=t3.char_val_fk1 and c1.char_type_cd='DOC-NUM'
        join ci_case_char c2 on c2.case_id=t3.char_val_fk1 and c2.char_type_cd='DOC-DAT'
      where
        t1.wf_proc_id=  v_wf_proc_id and t1.wf_evt_type_cd in ('EVT-ZLIM1')
        and t1.evt_seq= (select max(we1.evt_seq) from ci_wf_evt we1
        where we1.wf_proc_id=t1.wf_proc_id and we1.wf_evt_type_cd=t1.wf_evt_type_cd);
    exception
      when no_data_found then begin
        v_doc_lim_rec.DOC_LIM_DAT := '';
        v_doc_lim_rec.DOC_LIM_NUM := '';
      end;
    end;
    -- Причина не ограничения
    begin
      select cv.descr
      into v_doc_lim_rec.PRICH_NOT_LIM
      from ci_wf_evt_char evc
        join ci_char_val_l cv on cv.char_type_cd=evc.char_type_cd and
          cv.char_val=evc.char_val and cv.language_cd='RUS'
      where evc.wf_proc_id=v_wf_proc_id and evc.evt_seq=v_evt_seq and evc.char_type_cd='PR-N-LIM';
    exception
      when no_data_found then v_doc_lim_rec.PRICH_NOT_LIM:= ' ';
    end;

    -- Район кабельной сети
  /*  begin
      select  ch2.descr
      into v_doc_lim_rec.oper_lim_district
      from
        ci_prem cp1
        inner join cm_prem_link l1 on cp1.prem_id=l1.cm_source_prem_id
        inner join cm_connection_schema cs1 on cs1.prem_id=cp1.prem_id
        inner join cm_connection_schema_element cse1 on
          cse1.cm_connection_schema_id=cs1.cm_connection_schema_id and cse1.seq_num=
          (select max(seq_num) from
          cm_connection_schema_element cse2
          inner join cm_eo_net_node en2 on en2.cm_eo_net_node_id=cse2.cm_eo_net_node_id
          inner join ci_prem oeh2 on oeh2.prem_id=en2.prem_id and oeh2.prem_type_cd='OEH'
          where cse2.cm_connection_schema_id=cs1.cm_connection_schema_id)
        inner join cm_eo_net_node en1 on en1.cm_eo_net_node_id=cse1.cm_eo_net_node_id
        inner join ci_prem oeh on oeh.prem_id=en1.prem_id and oeh.prem_type_cd='OEH'
        --inner join ci_prem_char ph1 on ph1.char_type_cd='PREM-NAM' and oeh.prem_id=ph1.prem_id
        --inner join ci_prem_char ph2 on ph2.char_type_cd='VID-OEH' and oeh.prem_id=ph2.prem_id
        inner join ci_prem_char ph3 on ph3.char_type_cd='RAJON-ES' and oeh.prem_id=ph3.prem_id
        inner join ci_char_val_l ch2 on ch2.char_type_cd=ph3.char_type_cd and ch2.char_val=ph3.char_val and ch2.language_cd='RUS'

        inner join ci_wf_proc_char wc1 on wc1.char_type_cd='OB-OBSL' and wc1.char_val_fk1=l1.cm_dest_prem_id
        inner join ci_prem_char bp1 on oeh.prem_id=bp1.prem_id and bp1.char_type_cd='B_PRIN' and bp1.effdt =
        (select max(bp3.effdt) from ci_prem_char bp3 where bp3.prem_id=bp1.prem_id and bp3.char_type_cd=bp1.char_type_cd)
        inner join ci_case_char doc on doc.char_type_cd='PCZ-EVT' and doc.char_val_fk1=wc1.wf_proc_id
      where
        cp1.prem_type_cd='CP'
        and doc.case_id= i_case_id
        and bp1.char_val_fk1 = v_doc_lim_rec.OPER_LIM_ID
        and rownum=1;
    exception
      when no_data_found then v_doc_lim_rec.oper_lim_district:= '';
    end;    */
    -- Точка поключения
 /*   begin
    select ch2.descr||' '||ph1.adhoc_char_val tp into v_doc_lim_rec.POINT_SERVICE_NAME
    from
      ci_prem cp1
      inner join cm_prem_link l1 on cp1.prem_id=l1.cm_source_prem_id
      inner join cm_connection_schema cs1 on cs1.prem_id=cp1.prem_id
      inner join cm_connection_schema_element cse1 on cse1.cm_connection_schema_id=cs1.cm_connection_schema_id
      inner join cm_eo_net_node en1 on en1.cm_eo_net_node_id=cse1.cm_eo_net_node_id
      inner join ci_prem oeh on oeh.prem_id=en1.prem_id and oeh.prem_type_cd='OEH'
      inner join ci_prem_char ph1 on ph1.char_type_cd='PREM-NAM' and oeh.prem_id=ph1.prem_id
      inner join ci_prem_char ph2 on ph2.char_type_cd='VID-OEH' and oeh.prem_id=ph2.prem_id
      inner join ci_char_val_l ch2 on ch2.char_type_cd='VID-OEH' and ch2.char_val=ph2.char_val and ch2.language_cd='RUS'
      inner join ci_wf_proc_char wc1 on wc1.char_type_cd='OB-OBSL' and wc1.char_val_fk1=l1.cm_dest_prem_id
      inner join ci_case_char dc1 on dc1.char_type_cd='PCZ-EVT' and dc1.char_val_fk1=wc1.wf_proc_id
    where  cp1.prem_type_cd='CP' and dc1.case_id= i_case_id
      and cse1.seq_num=
      (select max(seq_num) from
      cm_connection_schema_element cse2
      inner join cm_eo_net_node en2 on en2.cm_eo_net_node_id=cse2.cm_eo_net_node_id
      inner join ci_prem oeh2 on oeh2.prem_id=en2.prem_id and oeh2.prem_type_cd='OEH'
      where cse2.cm_connection_schema_id=cs1.cm_connection_schema_id);
    exception
      when no_data_found then v_doc_lim_rec.point_service_name:= '';
    end;
   */ --
    --
    v_doc_lim_rec.ks_tel:= v_doc_lim_rec.OPER_LIM_TEL;
    pipe row(v_doc_lim_rec);
  end;
  --
  function get_doc_eo_data(i_case_id in char) return T_DOC_EO_SET pipelined is
    v_doc_eo_rec t_doc_eo_rec;
  begin
    v_doc_eo_rec.doc_id:= i_case_id;
    v_doc_eo_rec.doc_num := cm_dz_chty.get_case_chty(i_case_id, 'DOC-NUM');
    v_doc_eo_rec.doc_dat:= cm_dz_chty.get_case_chty(i_case_id, 'DOC-DAT');

    select acct_id into v_doc_eo_rec.acct_id
    from ci_case where case_id= i_case_id;
    --
    select t1.adhoc_char_val into v_doc_eo_rec.dog_num
    from ci_acct_char t1 where t1.acct_id=v_doc_eo_rec.acct_id and t1.char_type_cd='DOG-NUM'
    and (t1.effdt is null or t1.effdt =
    (select max(t2.effdt) from ci_acct_char t2
     where t1.char_type_cd=t2.char_type_cd and t2.acct_id=t1.acct_id and t2.effdt <= sysdate)) ;
    --
    select setup_dt into v_doc_eo_rec.dog_dat
    from ci_acct where acct_id = v_doc_eo_rec.acct_id;
    --
    select main_name, main_postal, main_address
    into v_doc_eo_rec.main_name, v_doc_eo_rec.main_postal, v_doc_eo_rec.main_address
    from cm_acct_vw where acct_id=v_doc_eo_rec.acct_id;

    --v_doc_eo_rec.main_name:= tfg.main_name;
    --v_doc_eo_rec.main_postal:= tfg.main_postal;
    --v_doc_eo_rec.main_address:= tfg.main_address;
    --v_doc_eo_rec.MAIN_FAX
    --v_doc_eo_rec.MAIN_TEL_SEC
    --v_doc_eo_rec.MAIN_TEL_DEG
    --v_doc_eo_rec.SIGNER_JOB:= get_tfg_signer_job(cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'DZ-TPMNG'), '9900000196');
    --v_doc_eo_rec.SIGNER_NAME:= get_tfg_signer_name(cm_dz.get_wfev_chty(tfg.wf_proc_id, tfg.evt_seq, 'DZ-TPMNG'), '9900000196');
    --
    for r in (
      select --на базе cur_dz_rc_st_r1
             ap_mn.per_id main_per_id,
             ppm.phone main_fax,
             ppts.phone main_tel_sec,
             pptg.phone main_tel_deg,
             wec.wf_proc_id,
             u1.last_name||' '||u1.first_name EXECUTOR_NAME,
             trim(pp.phone) EXECUTOR_TEL,
             cm_dz_rep.get_tfg_signer_job(cm_dz.get_case_chty(i_case_id, 'DZ-TPMNG'), '9900000196') signer_job,
             cm_dz_rep.get_tfg_signer_name(cm_dz.get_case_chty(i_case_id, 'DZ-TPMNG'), '9900000196') signer_name,
             trim(prem_cod.adhoc_char_val) EO_CODE,
             trim(pn_mn.entity_name) EO_NAME,
             cm_char_utils.get_prem_bigcharval(wpc_eo.char_val_fk1, 'DISTRICT', sysdate) eo_district,
             trim(prem.postal) eo_postal,
             trim(prem_adr1.adhoc_char_val)||trim(prem_adr2.adhoc_char_val) EO_ADDRESS,
             cm_dz_rep.get_prem_char_val(prem_cod.prem_id, 'PR-OUTAG') sw_in_dog,
             cm_dz_rep.get_conn_name(prem_cod.prem_id) eo_conn_name
      from dual
           join ci_wf_evt_char wec on wec.char_val_fk1=i_case_id and wec.char_type_cd='PRINTDOC'
           join ci_wf_proc_char wpc_eo on wpc_eo.wf_proc_id=wec.wf_proc_id and wpc_eo.char_type_cd='OB-OBSL '
           join ci_prem_char prem_cod on prem_cod.prem_id = wpc_eo.char_val_fk1 and prem_cod.char_type_cd = 'PREM_COD' and prem_cod.EFFDT = (select max(o.EFFDT) from CI_PREM_CHAR o where o.PREM_ID = prem_cod.PREM_ID and o.CHAR_TYPE_CD = prem_cod.CHAR_TYPE_CD and o.EFFDT <= sysdate)
  --         join ci_prem_char prem_nam on prem_nam.prem_id = wpc_eo.char_val_fk1 and prem_nam.char_type_cd = 'PREM-NAM' and prem_nam.EFFDT = (select max(o.EFFDT) from CI_PREM_CHAR o where o.PREM_ID = prem_nam.PREM_ID and o.CHAR_TYPE_CD = prem_nam.CHAR_TYPE_CD and o.EFFDT <= sysdate)
           join ci_prem prem on prem.prem_id = wpc_eo.char_val_fk1
           join ci_prem_char prem_adr1 on prem_adr1.prem_id = wpc_eo.char_val_fk1 and prem_adr1.char_type_cd = 'F-ADR-1' and prem_adr1.EFFDT = (select max(o.EFFDT) from CI_PREM_CHAR o where o.PREM_ID = prem_adr1.PREM_ID and o.CHAR_TYPE_CD = prem_adr1.CHAR_TYPE_CD and o.EFFDT <= sysdate)
           left join ci_prem_char prem_adr2 on prem_adr2.prem_id = wpc_eo.char_val_fk1 and prem_adr2.char_type_cd = 'F-ADR-2' and prem_adr2.EFFDT  <= sysdate
           join cm_eo_account_linkage eal on eal.cm_eo_id=wpc_eo.char_val_fk1 and eal.start_dt<=sysdate and (eal.end_dt is null or eal.end_dt>=sysdate)
           left join ci_acct_per ap_est on ap_est.acct_id=eal.acct_id and ap_est.acct_rel_type_cd='ESTIMATR'
           left join ci_per_name pn_est on pn_est.per_id=ap_est.per_id and pn_est.prim_name_sw='Y'
           left join ci_acct_per ap_bil on ap_bil.acct_id=eal.acct_id and ap_bil.acct_rel_type_cd='BILLING'
           left join ci_per_name pn_bil on pn_bil.per_id=ap_bil.per_id and pn_bil.prim_name_sw='Y'
           join ci_acct_per ap_mn on ap_mn.acct_id=eal.acct_id and ap_mn.main_cust_sw='Y'
           join ci_per_name pn_mn on pn_mn.per_id=ap_mn.per_id and pn_mn.prim_name_sw='Y'
           join ci_wf_proc_char wec_ex on wec_ex.wf_proc_id=wec.wf_proc_id and wec_ex.char_type_cd='EXECUTOR'
           join sc_user u1 on u1.user_id=wec_ex.char_val_fk1
           left join sc_user_char uc1 on uc1.user_id=wec_ex.char_val_fk1 and uc1.char_type_cd='OFFICER '
           left join ci_per_phone pp on pp.per_id=uc1.char_val_fk1 and pp.phone_type_cd='RAB'
           left join ci_per_phone ppm on ppm.per_id=ap_mn.per_id and ppm.phone_type_cd='FAKS'
           left join ci_per_phone ppts on ppts.per_id=ap_mn.per_id and ppts.phone_type_cd='TEL_SEC'
           left join ci_per_phone pptg on pptg.per_id=ap_mn.per_id and pptg.phone_type_cd='TEL_DEG'
      where (prem_adr2.EFFDT is null or prem_adr2.EFFDT = (select max(o.EFFDT) from CI_PREM_CHAR o where o.PREM_ID = prem_adr2.PREM_ID and o.CHAR_TYPE_CD = prem_adr2.CHAR_TYPE_CD and o.EFFDT <= sysdate))
    )loop
      v_doc_eo_rec.main_per_id:=r.main_per_id;
      v_doc_eo_rec.main_fax:=r.main_fax;
      v_doc_eo_rec.main_tel_sec:=r.main_tel_sec;
      v_doc_eo_rec.main_tel_deg:=r.main_tel_deg;
      v_doc_eo_rec.wf_proc_id:=r.wf_proc_id;
      v_doc_eo_rec.executor_name:=r.executor_name;
      v_doc_eo_rec.executor_tel:=r.executor_tel;
      v_doc_eo_rec.signer_job:=r.signer_job;
      v_doc_eo_rec.signer_name:=r.signer_name;
      select adhoc_char_val into v_doc_eo_rec.WF_AMOUNT_RUB
      from ci_wf_proc_char where wf_proc_id=r.wf_proc_id and char_type_cd='WZ-AMT';
      --v_doc_eo_rec.WF_AMOUNT_RUB
      v_doc_eo_rec.eo_code:=r.eo_code;
      v_doc_eo_rec.eo_name:=r.eo_name;
      v_doc_eo_rec.eo_district:=r.eo_district;
      v_doc_eo_rec.eo_postal:=r.eo_postal;
      v_doc_eo_rec.eo_address:=r.eo_address;
      v_doc_eo_rec.sw_in_dog:=r.sw_in_dog;
      v_doc_eo_rec.eo_conn_name:=r.eo_conn_name;
    end loop;
    --
    pipe row(v_doc_eo_rec);
  end get_doc_eo_data;
  --
  function get_conn_name(i_eo_id in char) return varchar2 is
    v_conn_name varchar2(100);
    v_tp varchar2(20);
    v_prem_id varchar2(12);
  begin
    select p5.prem_id into v_prem_id
    from
      ci_prem p1
      inner join cm_connection_schema p2 on p2.prem_id=p1.prem_id
      inner join cm_connection_schema_element p3 on p3.cm_connection_schema_id=p2.cm_connection_schema_id
        and p3.seq_num= (select max(p33.seq_num) from cm_connection_schema_element p33
                         where p33.cm_connection_schema_id=p2.cm_connection_schema_id)
      inner join cm_eo_net_node p4 on p4.cm_eo_net_node_id=p3.cm_eo_net_node_id
      inner join ci_prem p5 on p5.prem_id=p4.prem_id
    where p1.prnt_prem_id= i_eo_id and p1.prem_type_cd='CP' and rownum=1;
    --
    select c2.char_val into v_tp
    from ci_prem_char pc1 inner join ci_char_val c2
      on c2.char_type_cd=pc1.char_type_cd and c2.char_val=pc1.char_val
    where pc1.prem_id=v_prem_id and pc1.char_type_cd='VID-OEH' and rownum=1;
    v_conn_name:= v_tp;
    return v_conn_name;
  end ;
  --
  function get_case_reestr_head(i_case_id in char) return t_case_reestr_set  pipelined is
    v_rec t_case_reestr_rec;
  begin
    for a1 in (select * from ci_case where case_id = i_case_id)
    loop
      v_rec.doc_id:= i_case_id;
      v_rec.doc_num:= cm_dz_chty.get_case_chty(i_case_id, 'DOC-NUM');
      v_rec.doc_dat:= cm_dz_chty.get_case_chty(i_case_id, 'DOC-DAT');
      --
      v_rec.signer_job:= get_tfg_signer_job(cm_dz_chty.get_case_chty(i_case_id, 'SIGNER'), '9900000196');
      v_rec.signer_name:= get_tfg_signer_name(cm_dz_chty.get_case_chty(i_case_id, 'SIGNER'),
                                              cm_dz_chty.get_case_chty(i_case_id, 'DPRT-SRC'));
     --
      begin
      select u1.last_name||' '||u1.first_name, trim(pp.phone)
      into v_rec.executor_name, v_rec.executor_tel
      from
        sc_user u1
        left join sc_user_char uc1 on uc1.user_id=u1.user_id and uc1.char_type_cd='OFFICER '
        left join ci_per_phone pp on pp.per_id=uc1.char_val_fk1 and pp.phone_type_cd='RAB'
      where u1.user_id  = cast(cm_dz_chty.get_case_chty(i_case_id, 'EXECUTOR')as char(8));
      exception
        when no_data_found then  v_rec.executor_name:= null;
      end;
      --
      begin
        select t2.entity_name into v_rec.dprt_name
        from ci_case_char t1 inner join ci_per_name t2 on t1.char_val_fk1=t2.per_id
        where t1.case_id = i_case_id and t1.char_type_cd='DPRT-SRC' and t2.name_type_flg='PRIM';
      exception
        when no_data_found then  v_rec.dprt_name:= null;
      end;
      begin
        select t2.entity_name into v_rec.dprt_boss
        from ci_case_char t1 inner join ci_per_name t2 on t1.char_val_fk1=t2.per_id
        where t1.case_id = i_case_id and t1.char_type_cd='DPRT-SRC' and t2.name_type_flg='DBA';
      exception
        when no_data_found then v_rec.dprt_boss:= null;
      end;
      --
      v_rec.receiver_job:= get_receiver(cm_dz_chty.get_case_chty(i_case_id, 'RECEIVER'), 'JOB');
      v_rec.receiver_name:= get_receiver(cm_dz_chty.get_case_chty(i_case_id, 'RECEIVER'), 'NAME');
      --
      pipe row(v_rec);
    end loop;
  end get_case_reestr_head;
  --
  --
  function get_reestr_det_eo(i_case_id in char) return t_reestr_det_set pipelined is
    v_rec t_reestr_det_rec;
    v_doc_eo_rec t_doc_eo_rec;
  begin
    for a1 in (
      select distinct dch1.case_id doc_id, rpad(dch2.char_val_fk1, 10) wf_proc_id
      from
        ci_case r1
        join ci_case_char dch1 on dch1.char_type_cd='REESTR' and  rpad(dch1.char_val_fk1, 10)=r1.case_id
        join ci_case_char dch2 on dch1.case_id=dch2.case_id and dch2.char_type_cd='PCZ-EVT '
        join ci_wf_proc_char weo on weo.wf_proc_id=dch2.char_val_fk1 and weo.char_type_cd='OB-OBSL '
        join ci_wf_proc_char wz  on wz.wf_proc_id=dch2.char_val_fk1 and wz.char_type_cd='WZ-AMT  '
      where r1.case_id=i_case_id
        and coalesce(to_number(wz.adhoc_char_val, '999999999999990.99'), 0)>0
               )

    loop
      v_rec.case_id:= a1.doc_id;
      v_rec.wf_proc_id:= a1.wf_proc_id;

      begin
        select doc_num, doc_dat, main_name
        into v_rec.doc_num, v_rec.doc_dat, v_rec.main_name
        from table(get_doc_head(trim(v_rec.case_id)));
      exception
        when no_data_found then null;
      end;

      for a2 in (
        select
          pr1.prem_id eo_id,
          cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'PREM_COD', current_date) EO_CODE,
          cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'PREM-NAM', current_date) EO_NAME,
          TRIM(pr1.postal) EO_POSTAL,
          cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'F-ADR-1', current_date) EO_ADDRESS,
          cm_char_utils.get_prem_bigcharval(wf1.char_val_fk1, 'DISTRICT', current_date) EO_DISTRICT
          --get_prem_char_val(pr1.prem_id, 'PR-OUTAG') sw_in_dog_val,
          --get_prem_char_val_name(pr1.prem_id, 'GRUPEIEO') eo_group_ei
        from
          ci_case c1
          join ci_case_char dc1 on dc1.case_id=c1.case_id and dc1.char_type_cd='PCZ-EVT'
          join ci_wf_proc_char wf1 on wf1.wf_proc_id=rpad(dc1.char_val_fk1, 10) and wf1.char_type_cd='OB-OBSL '
          join ci_prem pr1 on pr1.prem_id=rpad(wf1.char_val_fk1,10)
        where
          c1.case_id=v_rec.case_id
         )
      loop
        v_rec.eo_id:= a2.eo_id;
        v_rec.eo_code:= a2.eo_code;
        v_rec.eo_name:= a2.eo_name;
        v_rec.eo_postal:= a2.eo_postal;
        v_rec.eo_address:= a2.eo_address;
        v_rec.eo_district:= a2.eo_district;

     -- v_doc_eo_rec:= ;
     -- v_rec.main_name:= a1.main_name;
        /*  for tfg in (select dc1.case_id, wf1.wf_proc_id, we1.evt_seq, ac1.acct_id, dc1.doc_num, dc1.doc_dat,
                ac1.dog_num, ac1.dog_dat, wf1.wf_amount_rub, wf1.wf_amount_kop,
                ac1.main_name, ac1.main_postal, ac1.main_address,
                wf1.officer_name, ac1.per_id main_per_id,
                cm_dz_chty.get_wfpr_chty(wf1.wf_proc_id, 'DAT-TFG') DAT_TFG,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-OPLZ') DAT_OPLZ,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMS') DAT_LIMS,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMP') DAT_LIMP,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMT') DAT_LIMT,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIMA') DAT_LIMA,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'DAT-LIM') DAT_LIM,
                cm_dz_chty.get_wfev_chty(we1.wf_proc_id, we1.evt_seq, 'TIM-LIM') TIM_LIM
                from
                  cm_doc_case_vw dc1
                  inner join cm_acct_vw ac1 on ac1.acct_id=dc1.acct_id
                  inner join cm_wf_evt_vw we1 on we1.printdoc=dc1.case_id
                  inner join cm_wf_vw wf1 on wf1.wf_proc_id=we1.wf_proc_id
                where
                  dc1.case_id= i_case_id)


      for a2 in (select * from table(cm_dz_rep.get_eo_list_4_wf_proc(a1.wf_proc_id)))
      loop
        v_rec.EO_ID:= a2.eo_id;
        v_rec.EO_CODE:= a2.eo_code;
        v_rec.EO_NAME:= a2.eo_name;
        v_rec.EO_DISTRICT:= a2.eo_district;
        v_rec.EO_POSTAL:= a2.eo_postal;
        v_rec.EO_ADDRESS:= a2.eo_address;
        if a2.SW_IN_DOG = 'Y' then
          v_rec.SW_IN_DOG:= 'Заключен';
        else
          v_rec.SW_IN_DOG:= 'Не заключен';
        end if;
        --
      begin
    select sum(ae.cur_amt) sum_eo into v_rec.EO_SUM
    from
      ci_case c1
      join ci_case_char c2 on c1.case_id=c2.case_id and c2.char_type_cd='PCZ-EVT'
      join ci_wf_proc wp_lim on wp_lim.wf_proc_id=c2.char_val_fk1
      join ci_wf_proc_char wc1 on wc1.wf_proc_id = wp_lim.wf_proc_id  and wc1.char_type_cd='OB-OBSL' --все ЭО
      join ci_wf_proc wp_ntf on wp_ntf.wf_proc_id=wp_lim.cr_by_wf_proc_id
      join ci_wf_proc_char wc2 on wc2.wf_proc_id=wp_ntf.wf_proc_id and wc2.char_type_cd='DZ-DRAFT' --все ПД
      join ci_case_char cc_me on cc_me.case_id=wc2.char_val_fk1 and cc_me.char_type_cd='ME-ID'
      join ci_ft ft on (ft.match_evt_id=cc_me.char_val_fk1 and ft.accounting_dt<=sysdate)
      join cm_account_entry ae on (ae.cm_generative_ft_id=ft.ft_id and ae.accounting_dt<=sysdate)
      join cm_rate_prop rp on (rp.cm_rate_prop_id = ae.cm_rate_prop_id and rp.accounting_dt<=sysdate)
      join cm_sa_exp se on (se.sa_id = rp.sa_id)
      join cm_eo_rate er on (er.cm_eo_rate_id=se.cm_eo_rate_id and er.cm_start_date<=sysdate and (er.cm_end_date is null or er.cm_end_date>=sysdate))
      join cm_eo_account_linkage eal on (eal.cm_eo_account_linkage_id=er.cm_eo_account_linkage_id
      and eal.start_dt<=sysdate and (eal.end_dt is null or eal.end_dt>=sysdate) and eal.cm_eo_id = wc1.char_val_fk1)

      where c1.case_id= a1.doc_id;-- '0310155236'
      exception
        when no_data_found then v_rec.EO_SUM:= '0.00';
      end;

        --
      end loop;
*/        pipe row(v_rec);

      end loop;
    end loop;
      --
  end get_reestr_det_eo;
  --
  --
  function get_acct_char_val(i_acct_id in char,
                             i_char_type_cd in char,
                             i_dt in date,
                             i_kind in char) return varchar2 is
    v_dat date;
    v_char_val varchar2(150);
  begin
    select max(EFFDT) into v_dat
    from ci_acct_char
    where acct_id=i_acct_id and char_type_cd=i_char_type_cd;
    --
    if i_kind = 'VAL' then
      select char_val into v_char_val
      from ci_acct_char
      where acct_id=i_acct_id and char_type_cd=i_char_type_cd and effdt = v_dat;
    end if;
    --
    if i_kind = 'ADHOC' then
      select adhoc_char_val into v_char_val
      from ci_acct_char
      where acct_id=i_acct_id and char_type_cd=i_char_type_cd and effdt = v_dat;
    end if;
    --
    return v_char_val;
  end get_acct_char_val;

  function get_dz_rc_st_r1(p_case_id CI_CASE.CASE_ID%TYPE) return T_DZ_RC_ST_R1_TAB pipelined is
  begin
    for r in CUR_DZ_RC_ST_R1(p_case_id) loop
      pipe row (r);
    end loop;
  end get_dz_rc_st_r1;

  function get_dz_rc_st_r2(p_case_id CI_CASE.CASE_ID%TYPE) return T_DZ_RC_ST_R2_TAB pipelined is
  begin
    for r in CUR_DZ_RC_ST_R2(p_case_id) loop
      pipe row (r);
    end loop;
  end get_dz_rc_st_r2;

  function get_list_operlim_by_case_c(i_case_id in ci_case.case_id%type) return T_LIST_OPERLIM_TAB pipelined is
   v_list_operlim T_LIST_OPERLIM;
  begin
    for rec in
  (select * from
 (select row_number() over (partition by c4.adhoc_char_val order by to_number(doc_num.adhoc_char_val)) || '.' AS RN,
  per_nm.adhoc_char_val as PER_NM,
  'Договор № ' || cm_dz.get_dog_num(i_acct_id => lic_s4et.char_val_fk1) as DOG_NUM,
  '(заявка № ' || doc_num.adhoc_char_val || ') (' || oper_limnm.adhoc_char_val || ')' as DOC_NUM,
  null as oper_limnm,
  null as dat_lim,
  1 as sort1,
  to_date(c4.adhoc_char_val,'dd.mm.yyyy') as sort2,
  row_number() over (partition by c4.adhoc_char_val order by to_number(doc_num.adhoc_char_val)) as sort3
from
  ci_case_char c1
  inner join ci_case_char c3 on c1.case_id = c3.case_id and trim(c3.char_type_cd) = 'PCZ-EVT'
  inner join ci_wf_evt e on e.wf_proc_id = trim(c3.char_val_fk1) and e.wf_evt_type_cd = 'EVT-ZLIM1'
  inner join ci_wf_evt_char ec on ec.wf_proc_id = e.wf_proc_id and ec.evt_seq = e.evt_seq and ec.char_type_cd = 'PRINTDOC'
  inner join ci_case_char doc_num on ec.Char_Val_Fk1 = doc_num.case_id and doc_num.char_type_cd = 'DOC-NUM'
  inner join ci_wf_evt_char c2 on c2.char_val_fk1=c1.case_id and c2.char_type_cd='PRINTDOC'
  inner join ci_wf_evt_char oper_lim on c2.wf_proc_id = oper_lim.wf_proc_id and c2.evt_seq = oper_lim.evt_seq and oper_lim.char_type_cd = 'OPER-LIM'
  inner join ci_per_char oper_limnm on oper_lim.char_val_fk1 = oper_limnm.per_id and oper_limnm.char_type_cd = 'NAIM-PLN'
  inner join ci_wf_proc_char lic_s4et on lic_s4et.wf_proc_id = c2.wf_proc_id and lic_s4et.char_type_cd = 'LIC-S4ET'
  inner join ci_acct_per ap on ap.acct_id = lic_s4et.char_val_fk1 and ap.main_cust_sw = 'Y'
  inner join ci_per per on per.per_id = ap.per_id
  inner join ci_per_char per_nm on per_nm.per_id = per.per_id and per_nm.char_type_cd = 'SHORT-NM'
  inner join ci_wf_proc_char c3 on c3.wf_proc_id=c2.wf_proc_id and c3.char_type_cd='POINTDLV'
  inner join ci_wf_proc_char c4 on c4.wf_proc_id=c2.wf_proc_id and c4.char_type_cd='DAT-LIMP'
  WHERE c1.char_val_fk1=i_case_id and trim(c1.char_type_cd)='REESTR'
  and per_nm.effdt = (Select max(effdt) from ci_per_char where per_id = per_nm.per_id and char_type_cd = per_nm.char_type_cd)
  and oper_limnm.effdt = (Select max(effdt) from ci_per_char where per_id = oper_limnm.per_id and char_type_cd = oper_limnm.char_type_cd)
union all
   select
  null AS RN,
  null as PER_NM,
  null as DOG_NUM,
  null as DOC_NUM,
  null  as oper_limnm,
  to_char(to_date(c4.adhoc_char_val,'dd.mm.yyyy'),'dd.mm.yyyy') || ' г.:' as dat_lim,
  0 as sort1,
  to_date(c4.adhoc_char_val,'dd.mm.yyyy') as sort2,
  null as sort3
from
  ci_case_char c1
  join ci_case_char doc_num on c1.case_id = doc_num.case_id and doc_num.char_type_cd = 'DOC-NUM'
  join ci_wf_evt_char c2 on c2.char_val_fk1=c1.case_id and c2.char_type_cd='PRINTDOC'
  join ci_wf_evt_char oper_lim on c2.wf_proc_id = oper_lim.wf_proc_id and c2.evt_seq = oper_lim.evt_seq and oper_lim.char_type_cd = 'OPER-LIM'
  join ci_per_char oper_limnm on oper_lim.char_val_fk1 = oper_limnm.per_id and oper_limnm.char_type_cd = 'NAIM-PLN'
  join ci_wf_proc_char lic_s4et on lic_s4et.wf_proc_id = c2.wf_proc_id and lic_s4et.char_type_cd = 'LIC-S4ET'
  join ci_acct_per ap on ap.acct_id = lic_s4et.char_val_fk1 and ap.main_cust_sw = 'Y'
  join ci_per per on per.per_id = ap.per_id
  join ci_per_char per_nm on per_nm.per_id = per.per_id and per_nm.char_type_cd = 'SHORT-NM'
  join ci_wf_proc_char c3 on c3.wf_proc_id=c2.wf_proc_id and c3.char_type_cd='POINTDLV'
  join ci_wf_proc_char c4 on c4.wf_proc_id=c2.wf_proc_id and c4.char_type_cd='DAT-LIMP'
where
  c1.char_val_fk1=i_case_id and trim(c1.char_type_cd)='REESTR'
  and per_nm.effdt = (Select max(effdt) from ci_per_char where per_id = per_nm.per_id and char_type_cd = per_nm.char_type_cd)
  and oper_limnm.effdt = (Select max(effdt) from ci_per_char where per_id = oper_limnm.per_id and char_type_cd = oper_limnm.char_type_cd)
  group by c4.adhoc_char_val
  ) order by dat_lim , sort1
   ) loop
      v_list_operlim.rn := rec.rn;
      v_list_operlim.PER_NM := rec.per_nm;
      v_list_operlim.DOG_NUM := rec.DOG_NUM;
      v_list_operlim.DOC_NUM := rec.DOC_NUM;
      v_list_operlim.oper_limnm := rec.oper_limnm;
      v_list_operlim.dat_lim := rec.dat_lim;
      v_list_operlim.sort1 := rec.sort1;
      v_list_operlim.sort2 := rec.sort2;
      v_list_operlim.sort3 := rec.sort3;
       pipe row(v_list_operlim);
      end loop;
   end get_list_operlim_by_case_c;

function get_list_operlim_by_case(i_case_id in ci_case.case_id%type) return T_LIST_OPERLIM_TAB pipelined is
   v_list_operlim T_LIST_OPERLIM;
  begin
    for rec in
  (select * from
 (select
  row_number() over (partition by c4.adhoc_char_val order by to_number(doc_num.adhoc_char_val)) || '.' AS RN,
  per_nm.adhoc_char_val as PER_NM,
  'Договор № ' || cm_dz.get_dog_num(i_acct_id => lic_s4et.char_val_fk1) as DOG_NUM,
  '(заявка № ' || doc_num.adhoc_char_val || ') (' || oper_limnm.adhoc_char_val || ')' as DOC_NUM,
  null as oper_limnm,
  null as dat_lim,
  1 as sort1,
  to_date(c4.adhoc_char_val,'dd.mm.yyyy') as sort2,
  row_number() over (partition by c4.adhoc_char_val order by to_number(doc_num.adhoc_char_val)) as sort3
from
  ci_case_char c1
  join ci_case_char doc_num on c1.case_id = doc_num.case_id and doc_num.char_type_cd = 'DOC-NUM'
  join ci_wf_evt_char c2 on c2.char_val_fk1=c1.case_id and c2.char_type_cd='PRINTDOC'
  join ci_wf_evt_char oper_lim on c2.wf_proc_id = oper_lim.wf_proc_id and c2.evt_seq = oper_lim.evt_seq and oper_lim.char_type_cd = 'OPER-LIM'
  join ci_per_char oper_limnm on oper_lim.char_val_fk1 = oper_limnm.per_id and oper_limnm.char_type_cd = 'NAIM-PLN'
  join ci_wf_proc_char lic_s4et on lic_s4et.wf_proc_id = c2.wf_proc_id and lic_s4et.char_type_cd = 'LIC-S4ET'
  join ci_acct_per ap on ap.acct_id = lic_s4et.char_val_fk1 and ap.main_cust_sw = 'Y'
  join ci_per per on per.per_id = ap.per_id
  join ci_per_char per_nm on per_nm.per_id = per.per_id and per_nm.char_type_cd = 'SHORT-NM'
  join ci_wf_proc_char c3 on c3.wf_proc_id=c2.wf_proc_id and c3.char_type_cd='POINTDLV'
  join ci_wf_proc_char c4 on c4.wf_proc_id=c2.wf_proc_id and c4.char_type_cd='DAT-LIMP'
where
  c1.char_val_fk1=i_case_id and trim(c1.char_type_cd)='REESTR'
  and per_nm.effdt = (Select max(effdt) from ci_per_char where per_id = per_nm.per_id and char_type_cd = per_nm.char_type_cd)
  and oper_limnm.effdt = (Select max(effdt) from ci_per_char where per_id = oper_limnm.per_id and char_type_cd = oper_limnm.char_type_cd)
union all
   select
  null AS RN,
  null as PER_NM,
  null as DOG_NUM,
  null as DOC_NUM,
  null  as oper_limnm,
  to_char(to_date(c4.adhoc_char_val,'dd.mm.yyyy'),'dd.mm.yyyy') || ' г.:' as dat_lim,
  0 as sort1,
  to_date(c4.adhoc_char_val,'dd.mm.yyyy') as sort2,
  null as sort3
from
  ci_case_char c1
  join ci_case_char doc_num on c1.case_id = doc_num.case_id and doc_num.char_type_cd = 'DOC-NUM'
  join ci_wf_evt_char c2 on c2.char_val_fk1=c1.case_id and c2.char_type_cd='PRINTDOC'
  join ci_wf_evt_char oper_lim on c2.wf_proc_id = oper_lim.wf_proc_id and c2.evt_seq = oper_lim.evt_seq and oper_lim.char_type_cd = 'OPER-LIM'
  join ci_per_char oper_limnm on oper_lim.char_val_fk1 = oper_limnm.per_id and oper_limnm.char_type_cd = 'NAIM-PLN'
  join ci_wf_proc_char lic_s4et on lic_s4et.wf_proc_id = c2.wf_proc_id and lic_s4et.char_type_cd = 'LIC-S4ET'
  join ci_acct_per ap on ap.acct_id = lic_s4et.char_val_fk1 and ap.main_cust_sw = 'Y'
  join ci_per per on per.per_id = ap.per_id
  join ci_per_char per_nm on per_nm.per_id = per.per_id and per_nm.char_type_cd = 'SHORT-NM'
  join ci_wf_proc_char c3 on c3.wf_proc_id=c2.wf_proc_id and c3.char_type_cd='POINTDLV'
  join ci_wf_proc_char c4 on c4.wf_proc_id=c2.wf_proc_id and c4.char_type_cd='DAT-LIMP'
where
  c1.char_val_fk1=i_case_id and trim(c1.char_type_cd)='REESTR'
  and per_nm.effdt = (Select max(effdt) from ci_per_char where per_id = per_nm.per_id and char_type_cd = per_nm.char_type_cd)
  and oper_limnm.effdt = (Select max(effdt) from ci_per_char where per_id = oper_limnm.per_id and char_type_cd = oper_limnm.char_type_cd)
  group by c4.adhoc_char_val
  ) order by dat_lim , sort1
   ) loop
      v_list_operlim.rn := rec.rn;
      v_list_operlim.PER_NM := rec.per_nm;
      v_list_operlim.DOG_NUM := rec.DOG_NUM;
      v_list_operlim.DOC_NUM := rec.DOC_NUM;
      v_list_operlim.oper_limnm := rec.oper_limnm;
      v_list_operlim.dat_lim := rec.dat_lim;
      v_list_operlim.sort1 := rec.sort1;
      v_list_operlim.sort2 := rec.sort2;
      v_list_operlim.sort3 := rec.sort3;
       pipe row(v_list_operlim);
      end loop;
   end get_list_operlim_by_case;

function get_tp_list_wf_proc_lim_omsk(i_wf_proc_id in char, i_doc_dat DATE DEFAULT TRUNC(SYSDATE)) return t_eo_set pipelined is
    v_eo_rec t_eo_rec;
  begin
/*    select count(distinct p.char_val_fk1)
      into v_eo_rec.CNT_TP
    from ci_wf_proc_char p
    where p.wf_proc_id = i_wf_proc_id
      and p.char_type_cd = 'POINTDLV';*/

    for eo1 in (
      
/*      select * from  (
        select
         row_number() over (partition by null order by cm_char_utils.get_prem_bigcharval(l.cm_source_prem_id, 'CP-NUM', current_date)) as rn,
        l.cm_source_prem_id as prem_id,
        cm_char_utils.get_prem_bigcharval(l.cm_source_prem_id, 'CP-NAME', current_date) EO_TU_NUMS,
        cm_char_utils.get_prem_bigcharval(pr1.prem_id, 'DISTRICT', current_date) EO_DISTRICT,
        TRIM(pr1.postal) EO_POSTAL,
        cm_char_utils.get_prem_bigcharval(decode(pc.char_val , 'CALCULATED_GROUP', ll.cm_dest_prem_id, l.cm_dest_prem_id), 'F-ADR-1 ', current_date)||
        cm_char_utils.get_prem_bigcharval(decode(pc.char_val , 'CALCULATED_GROUP', ll.cm_dest_prem_id, l.cm_dest_prem_id), 'F-ADR-2 ', current_date) EO_ADDRESS,
        DAT_LIM.ADHOC_CHAR_VAL AS DAT_LIM,
        DAT_LIMP.ADHOC_CHAR_VAL AS DAT_LIMP,
        cm_dz_rep.get_prem_char_val(pr1.prem_id, 'PR-OUTAG') sw_in_dog,
        cm_char_utils.get_prem_bigcharval(decode(pc.char_val , 'CALCULATED_GROUP', ll.cm_dest_prem_id, l.cm_dest_prem_id),'PREM-NAM',current_date) AS EO_NAME,
        cm_char_utils.get_prem_bigcharval(l.cm_source_prem_id, 'CP-MAX-P', current_date) CP_MAX_P
        from
         ci_wf_proc_char wf1
          inner join ci_wf_proc p on wf1.wf_proc_id = p.wf_proc_id
          LEFT  JOIN CI_WF_PROC_CHAR DAT_LIM ON DAT_LIM.WF_PROC_ID = wf1.wf_proc_id AND DAT_LIM.CHAR_TYPE_CD = 'DAT-LIM'
          LEFT  JOIN CI_WF_PROC_CHAR DAT_LIMP ON DAT_LIMP.WF_PROC_ID = wf1.wf_proc_id AND DAT_LIMP.CHAR_TYPE_CD = 'DAT-LIMP'
          inner join ci_prem pr1 on pr1.prem_id=trim(wf1.char_val_fk1) and wf1.char_type_cd='POINTDLV'
          inner join cm_prem_link l on pr1.prem_id = l.cm_source_prem_id and l.cm_prem_link_type_cd in ('EE_SUPPLY')
          left  join ci_prem_char pc on pc.prem_id = l.cm_dest_prem_id and pc.char_type_cd = 'SUP-P-TY'
          left  join cm_prem_link ll on ll.cm_source_prem_id = l.cm_dest_prem_id and ll.cm_prem_link_type_cd in ('EE_SUPPLY')
        where
          wf1.wf_proc_id=i_wf_proc_id
          and trunc(p.cre_dttm) between l.start_dt and nvl(l.end_dt , to_date('01.01.2100' , 'dd.mm.yyyy'))
          and (pc.effdt is null or  pc.effdt = (Select max(effdt) from ci_prem_char where prem_id = pc.prem_id and char_type_cd = pc.char_type_cd))
          )  order by rn*/
        select  
        row_number() over (partition by null order by cm_char_utils.get_prem_bigcharval(trim(POINTDLV.CHAR_VAL_FK1), 'CP-NUM', i_doc_dat)) as rn,
        OB_OBSL.CHAR_VAL_FK1 as prem_id,
        cm_char_utils.get_prem_bigcharval(trim(POINTDLV.CHAR_VAL_FK1), 'CP-NAME', i_doc_dat) EO_TU_NUMS,
        cm_char_utils.get_prem_bigcharval(trim(OB_OBSL.CHAR_VAL_FK1), 'DISTRICT', i_doc_dat) EO_DISTRICT,
        (select TRIM(pr1.postal) from ci_prem pr1 where pr1.prem_id = trim(OB_OBSL.CHAR_VAL_FK1)) EO_POSTAL,
        cm_char_utils.get_prem_bigcharval(trim(OB_OBSL.CHAR_VAL_FK1), 'F-ADR-1 ', i_doc_dat)||
        cm_char_utils.get_prem_bigcharval(trim(OB_OBSL.CHAR_VAL_FK1), 'F-ADR-2 ', i_doc_dat) EO_ADDRESS,
        DAT_LIM.ADHOC_CHAR_VAL AS DAT_LIM,
        DAT_LIMP.ADHOC_CHAR_VAL AS DAT_LIMP,
        cm_dz_rep.get_prem_char_val(OB_OBSL.CHAR_VAL_FK1, 'PR-OUTAG') sw_in_dog,
        cm_char_utils.get_prem_bigcharval(OB_OBSL.CHAR_VAL_FK1, 'PREM-NAM', i_doc_dat) AS EO_NAME,
        cm_char_utils.get_prem_bigcharval(trim(POINTDLV.CHAR_VAL_FK1), 'CP-MAX-P', i_doc_dat) CP_MAX_P        
        from ci_wf_proc wf1
        LEFT  JOIN CI_WF_PROC_CHAR DAT_LIM ON DAT_LIM.WF_PROC_ID = wf1.wf_proc_id AND DAT_LIM.CHAR_TYPE_CD = 'DAT-LIM'
        LEFT  JOIN CI_WF_PROC_CHAR DAT_LIMP ON DAT_LIMP.WF_PROC_ID = wf1.wf_proc_id AND DAT_LIMP.CHAR_TYPE_CD = 'DAT-LIMP'
        LEFT  JOIN CI_WF_PROC_CHAR OB_OBSL ON OB_OBSL.WF_PROC_ID = wf1.wf_proc_id AND OB_OBSL.CHAR_TYPE_CD = 'OB-OBSL'
        LEFT  JOIN CI_WF_PROC_CHAR POINTDLV ON POINTDLV.WF_PROC_ID = wf1.wf_proc_id AND POINTDLV.CHAR_TYPE_CD = 'POINTDLV' AND POINTDLV.SEQ_NUM = OB_OBSL.SEQ_NUM           
        where
          wf1.wf_proc_id = i_wf_proc_id          
          ) loop
      v_eo_rec.rn :=  eo1.rn;
      v_eo_rec.eo_id:= eo1.prem_id;
      --v_eo_rec.eo_code:= trim(eo1.eo_code);
      v_eo_rec.eo_name:= TRIM(eo1.eo_name);
      v_eo_rec.eo_district:= trim(eo1.eo_district);
      v_eo_rec.eo_postal:= trim(eo1.eo_postal);
      v_eo_rec.eo_address:= trim(eo1.eo_address);
      v_eo_rec.sw_in_dog:= eo1.sw_in_dog;
      v_eo_rec.DAT_LIM := EO1.DAT_LIM;
      v_eo_rec.DAT_LIMP := EO1.DAT_LIMP;
      v_eo_rec.EO_TU_NUMS := EO1.EO_TU_NUMS;
      v_eo_rec.CP_MAX_P := EO1.CP_MAX_P;
      pipe row(v_eo_rec);
    end loop;
  end get_tp_list_wf_proc_lim_omsk;

function get_list_operlim_omsk(i_case_id in ci_case.case_id%type) return T_LIST_OPERLIM_TAB pipelined is
   v_list_operlim T_LIST_OPERLIM;
  begin
    for rec in
  (select * from
 (select
  row_number() over (/*partition by c4.adhoc_char_val*/ order by to_number(doc_num.adhoc_char_val)) || '.' AS RN,
  per_nm.adhoc_char_val as PER_NM,
  'договор № ' || cm_dz.get_dog_num(i_acct_id => lic_s4et.char_val_fk1) as DOG_NUM,
  'Заявку № ' || doc_num.adhoc_char_val /*|| ') (' || oper_limnm.adhoc_char_val || ')'*/ as DOC_NUM,
  null as oper_limnm,
  null as dat_lim,
  1 as sort1,
  to_date(c4.adhoc_char_val,'dd.mm.yyyy') as sort2,
  row_number() over (partition by c4.adhoc_char_val order by to_number(doc_num.adhoc_char_val)) as sort3
from
  ci_case_char c1
  join ci_case_char doc_num on c1.case_id = doc_num.case_id and doc_num.char_type_cd = 'DOC-NUM'
  join ci_wf_evt_char c2 on c2.char_val_fk1=c1.case_id and c2.char_type_cd='PRINTDOC'
  join ci_wf_evt_char oper_lim on c2.wf_proc_id = oper_lim.wf_proc_id and c2.evt_seq = oper_lim.evt_seq and oper_lim.char_type_cd = 'OPER-LIM'
  join ci_per_char oper_limnm on oper_lim.char_val_fk1 = oper_limnm.per_id and oper_limnm.char_type_cd = 'NAIM-PLN'
  join ci_wf_proc_char lic_s4et on lic_s4et.wf_proc_id = c2.wf_proc_id and lic_s4et.char_type_cd = 'LIC-S4ET'
  join ci_acct_per ap on ap.acct_id = lic_s4et.char_val_fk1 and ap.main_cust_sw = 'Y'
  join ci_per per on per.per_id = ap.per_id
  join ci_per_char per_nm on per_nm.per_id = per.per_id and per_nm.char_type_cd = 'SHORT-NM'
  join ci_wf_proc_char c3 on c3.wf_proc_id=c2.wf_proc_id and c3.char_type_cd='POINTDLV'
  join ci_wf_proc_char c4 on c4.wf_proc_id=c2.wf_proc_id and c4.char_type_cd='DAT-LIMP'
where
  c1.char_val_fk1=i_case_id and trim(c1.char_type_cd)='REESTR'
  and per_nm.effdt = (Select max(effdt) from ci_per_char where per_id = per_nm.per_id and char_type_cd = per_nm.char_type_cd)
  and oper_limnm.effdt = (Select max(effdt) from ci_per_char where per_id = oper_limnm.per_id and char_type_cd = oper_limnm.char_type_cd)
/*union all
   select
  null AS RN,
  null as PER_NM,
  null as DOG_NUM,
  null as DOC_NUM,
  null  as oper_limnm,
  to_char(to_date(c4.adhoc_char_val,'dd.mm.yyyy'),'dd.mm.yyyy') || ' г.:' as dat_lim,
  0 as sort1,
  to_date(c4.adhoc_char_val,'dd.mm.yyyy') as sort2,
  null as sort3
from
  ci_case_char c1
  join ci_case_char doc_num on c1.case_id = doc_num.case_id and doc_num.char_type_cd = 'DOC-NUM'
  join ci_wf_evt_char c2 on c2.char_val_fk1=c1.case_id and c2.char_type_cd='PRINTDOC'
  join ci_wf_evt_char oper_lim on c2.wf_proc_id = oper_lim.wf_proc_id and c2.evt_seq = oper_lim.evt_seq and oper_lim.char_type_cd = 'OPER-LIM'
  join ci_per_char oper_limnm on oper_lim.char_val_fk1 = oper_limnm.per_id and oper_limnm.char_type_cd = 'NAIM-PLN'
  join ci_wf_proc_char lic_s4et on lic_s4et.wf_proc_id = c2.wf_proc_id and lic_s4et.char_type_cd = 'LIC-S4ET'
  join ci_acct_per ap on ap.acct_id = lic_s4et.char_val_fk1 and ap.main_cust_sw = 'Y'
  join ci_per per on per.per_id = ap.per_id
  join ci_per_char per_nm on per_nm.per_id = per.per_id and per_nm.char_type_cd = 'SHORT-NM'
  join ci_wf_proc_char c3 on c3.wf_proc_id=c2.wf_proc_id and c3.char_type_cd='POINTDLV'
  join ci_wf_proc_char c4 on c4.wf_proc_id=c2.wf_proc_id and c4.char_type_cd='DAT-LIMP'
where
  c1.char_val_fk1=i_case_id and trim(c1.char_type_cd)='REESTR'
  and per_nm.effdt = (Select max(effdt) from ci_per_char where per_id = per_nm.per_id and char_type_cd = per_nm.char_type_cd)
  and oper_limnm.effdt = (Select max(effdt) from ci_per_char where per_id = oper_limnm.per_id and char_type_cd = oper_limnm.char_type_cd)
  group by c4.adhoc_char_val*/
  ) order by sort1
   ) loop
      v_list_operlim.rn := rec.rn;
      v_list_operlim.PER_NM := rec.per_nm;
      v_list_operlim.DOG_NUM := rec.DOG_NUM;
      v_list_operlim.DOC_NUM := rec.DOC_NUM;
      v_list_operlim.oper_limnm := rec.oper_limnm;
      v_list_operlim.dat_lim := rec.dat_lim;
      v_list_operlim.sort1 := rec.sort1;
      v_list_operlim.sort2 := rec.sort2;
      v_list_operlim.sort3 := rec.sort3;
       pipe row(v_list_operlim);
      end loop;
   end get_list_operlim_omsk;

function get_list_pril_41(p_case_id in ci_case.case_id%type) return t_list_pril_41_42_tab pipelined is
  v t_list_pril_41_42;
begin
  for r in (
    with t as (
      select /*+MATERIALIZE*/ case_id from ci_case_char cc where cc.char_type_cd='REESTR' and cc.char_val_fk1=P_CASE_ID
    )
    select c.case_id, cc.char_val_fk1,
           su.last_name||case when regexp_like(last_name,'\w') then ' '||replace(regexp_substr(su.first_name,'\w')||'.'||regexp_substr(su.first_name,'\w\s(\w)',1,1,'',1)||'.','..','.') end executor
    from ci_case c
         left join ci_case_char cc on cc.case_id = c.case_id and cc.char_type_cd='EXECUTOR'
         left join sc_user su on su.user_id=cc.char_val_fk1
    where c.case_id = P_CASE_ID
  ) loop
    v.executor_id:=r.char_val_fk1;
    v.executor:=r.executor;
    v.case_id:=r.case_id;
    for r1 in (
      with t as (
        select /*+MATERIALIZE*/ case_id from ci_case_char cc where cc.char_type_cd='REESTR' and cc.char_val_fk1=P_CASE_ID
      ),
      t_text as (
        select c.per_id,
               rtrim(xmlagg(xmlelement("a", ' '||ccn.adhoc_char_val || ',') ORDER BY lpad(ccn.adhoc_char_val,20)).extract('/a/text()').getclobval(), ',') nums,
               pc_name.adhoc_char_val per_name, count(*) cnt
        from t
             join ci_case c on c.case_id=t.case_id
             join ci_case_char ccn on ccn.char_type_cd='DOC-NUM' and ccn.case_id=t.case_id
             left join ci_per_char pc_name on pc_name.per_id = c.per_id and pc_name.char_type_cd='SHORT-NM' and pc_name.effdt<=sysdate
       where (pc_name.effdt is null or pc_name.effdt=(select max(i.effdt) from ci_per_char i where i.per_id=pc_name.per_id and i.char_type_cd=pc_name.char_type_cd and i.effdt<=sysdate))
       group by c.per_id, pc_name.adhoc_char_val
      )
      select row_number() over(order by per_name) rn,
             t_text.*
      from t_text
      order by per_name
    ) loop
      v.text:=v.text||r1.rn||
              case when r1.cnt=1 then '. Заявка №' else '. Заявки №' end||
              r1.nums||' '||r1.per_name||' на '||r1.cnt||'л. в 1 экз;'||chr(10);
    end loop;
    pipe row(v);
  end loop;
end;

function get_list_pril_42(p_case_id in ci_case.case_id%type) return t_list_pril_41_42_tab pipelined is
  v t_list_pril_41_42;
begin
  for r in (
    with t as (
      select /*+MATERIALIZE*/ case_id from ci_case_char cc where cc.char_type_cd='REESTR' and cc.char_val_fk1=P_CASE_ID
    )
    select c.case_id, cc.char_val_fk1,
           su.last_name||case when regexp_like(last_name,'\w') then ' '||replace(regexp_substr(su.first_name,'\w')||'.'||regexp_substr(su.first_name,'\w\s(\w)',1,1,'',1)||'.','..','.') end executor
    from ci_case c
         left join ci_case_char cc on cc.case_id = c.case_id and cc.char_type_cd='EXECUTOR'
         left join sc_user su on su.user_id=cc.char_val_fk1
    where c.case_id = P_CASE_ID
  ) loop
    v.executor_id:=r.char_val_fk1;
    v.executor:=r.executor;
    v.case_id:=r.case_id;
    for r1 in (
      with t as (
        select /*+MATERIALIZE*/ case_id from ci_case_char cc where cc.char_type_cd='REESTR' and cc.char_val_fk1=P_CASE_ID
      ),
      t_text as (
        select c.per_id,
               rtrim(xmlagg(xmlelement("a", ' '||ccn.adhoc_char_val || ',') ORDER BY lpad(ccn.adhoc_char_val,20)).extract('/a/text()').getclobval(), ',') nums,
               pc_name.adhoc_char_val per_name, count(*) cnt,
               (select max(i.adhoc_char_val) keep(dense_rank last order by effdt)
                  from ci_acct_char i
                 where i.acct_id = c.acct_id
                   and i.char_type_cd='DOG-NUM'
                   and i.effdt <= sysdate) dog_num
        from t
             join ci_case c on c.case_id=t.case_id
             join ci_case_char ccn on ccn.char_type_cd='DOC-NUM' and ccn.case_id=t.case_id
             left join ci_per_char pc_name on pc_name.per_id = c.per_id and pc_name.char_type_cd='SHORT-NM' and pc_name.effdt<=sysdate
       where (pc_name.effdt is null or pc_name.effdt=(select max(i.effdt) from ci_per_char i where i.per_id=pc_name.per_id and i.char_type_cd=pc_name.char_type_cd and i.effdt<=sysdate))
       group by c.per_id, pc_name.adhoc_char_val, c.acct_id
      )
      select row_number() over(order by per_name) rn,
             t_text.*
      from t_text
      order by per_name
    ) loop
      v.nums:=v.nums||chr(9)||'Направляю Вам заявки №'||r1.nums||' на контроль самостоятельного частичного ограничения, частичное и полное ограничение '||
      	      r1.per_name||' дог.'||r1.dog_num||'.'||chr(10);
      v.text:=v.text||to_char(r1.rn*2-1)||'. Заявки №'||r1.nums||' '||r1.per_name||' на '||r1.cnt||'л. в '||r1.cnt||' экз;'||chr(10)||
      	      to_char(r1.rn*2)||'. Копия уведомления о задолженности '||r1.per_name||' на 2л. в 1 экз;'||chr(10);
    end loop;
    pipe row(v);
  end loop;
end;

FUNCTION DZ_SCHEDULE_LIMITATION(P_DATE_BEGIN DATE, P_DATE_END DATE, P_DEPARTMENT VARCHAR2, P_USER VARCHAR2) 
   RETURN T_SCHEDULE_LIMITATION_SET PIPELINED
   
   IS 
   V_RET  T_SCHEDULE_LIMITATION;
   V_CUR_AMT_BEGIN_ALL NUMBER; 
   V_SUM_CHARGE_ALL NUMBER;     
   V_SUM_SCHEDULE_ALL NUMBER;     
   V_SUM_PAY_ALL NUMBER;
     
   BEGIN
   
   FOR I IN (
          WITH PARAMETERS AS (
          SELECT 
          /*+MATERIALIZE*/
           TRUNC(P_DATE_BEGIN) /*TO_DATE('01.08.2016','DD.MM.YYYY')*/ DATE_BEGIN,
           TRUNC(P_DATE_END) /*TO_DATE('31.08.2016','DD.MM.YYYY') */DATE_END,
           P_DEPARTMENT /*'TSK1'*/ DEPARTMENT
          FROM DUAL
          ) 
          , CIS_DIVISION_USER AS (
           SELECT /*+MATERIALIZE*/ UE.CIS_DIVISION
           FROM CM_USER_EXP UE
           WHERE UE.USER_ID = RPAD(TRIM(P_USER), 8)
          )           
          , ACCT_LITE AS (
          SELECT DISTINCT 
            A.ACCT_ID 
          FROM CI_DAR_USR U
          INNER JOIN CI_ACC_GRP_DAR AG_DAR ON AG_DAR.DAR_CD=U.DAR_CD
          INNER JOIN CI_ACCT A ON A.ACCESS_GRP_CD=AG_DAR.ACCESS_GRP_CD
          INNER JOIN CIS_DIVISION_USER DU ON DU.CIS_DIVISION = A.CIS_DIVISION           
          WHERE U.USER_ID = RPAD(TRIM(P_USER), 8) AND U.EXPIRE_DT > SYSDATE
            -- and a.acct_id = '9555386828'
          )          
          , ACCT AS (
          select /*+MATERIALIZE*/
                  w.wf_proc_id,
                  w.cr_by_wf_proc_id wf_proc_id_NOTIFICATION,      
                  -- DAT_LIM.ADHOC_CHAR_VAL DAT_LIM,
                  -- 'DAT-LIM' TYPE_DATE,
                  A.ACCT_ID ACCT_ID,
                  DENSE_RANK() OVER (PARTITION BY A.ACCT_ID, DAT_LIM.ADHOC_CHAR_VAL, POINTDLV.CHAR_VAL_FK1 ORDER BY A.ACCT_ID, DAT_LIM.ADHOC_CHAR_VAL, POINTDLV.CHAR_VAL_FK1, W.CRE_DTTM DESC) RANK_ROW                                 
          from PARAMETERS PR
          INNER JOIN ci_wf_proc w ON 1 = 1
          inner join ci_wf_proc_char DZ_DPRT on DZ_DPRT.wf_proc_id = w.wf_proc_id and DZ_DPRT.char_type_cd='DZ-DPRT'
          inner join ci_wf_proc_char DAT_LIM on DAT_LIM.WF_PROC_ID = w.wf_proc_id 
           and DAT_LIM.CHAR_TYPE_CD in ('DAT-LIM', 'DAT-LIMP'/*, 'DAT-LIMA', 'DAT-LIMT', 'DAT-LIMS'*/)
           and trim(DAT_LIM.ADHOC_CHAR_VAL) <> 'UNDEF'
          --inner join ci_wf_proc_char SOGLIMIT on SOGLIMIT.wf_proc_id = W.WF_PROC_ID and SOGLIMIT.char_type_cd = 'SOGLIMIT'             
          inner join ci_wf_proc_char POINTDLV on POINTDLV.wf_proc_id = W.WF_PROC_ID and POINTDLV.char_type_cd = 'POINTDLV'              
          inner join ci_wf_proc_char LIC_S4ET on LIC_S4ET.WF_PROC_ID = w.wf_proc_id and LIC_S4ET.CHAR_TYPE_CD = 'LIC-S4ET'  
          INNER JOIN ACCT_LITE A ON A.ACCT_ID = LIC_S4ET.CHAR_VAL_FK1
          where 1 = 1
            and w.wf_proc_tmpl_cd IN ('TMPL-EBLIM1','TMPL-EBLIM2','TMPL-EBLIM5')
            and (TRIM(PR.DEPARTMENT) is null or TRIM(DZ_DPRT.CHAR_VAL_FK1) = TRIM(PR.DEPARTMENT))
            and to_date(trim(DAT_LIM.ADHOC_CHAR_VAL),'dd.mm.yyyy') between PR.DATE_BEGIN and PR.DATE_END  
            --and SOGLIMIT.CHAR_VAL = RPAD('Y',16)
          )
          /*Уникальные ACCT_ID - для подсчета расходов, начислений и т.д., чтобы при 2 процессах данные не задваивались*/
          , DIST_ACCT AS (
            select /*+MATERIALIZE*/ distinct acct_id from acct 
          )
          /*Данные ЭО и ТП*/
          , ACCT_EO_CP AS (
          SELECT  /*+MATERIALIZE*/ A.WF_PROC_ID
                 ,A.wf_proc_id_NOTIFICATION
                 ,A.ACCT_ID
                 ,acct_per.per_id
                 --,A.DAT_LIM
                 --,A.TYPE_DATE
                 ,OB_OBSL.CHAR_VAL_FK1 EO_ID
                 ,POINTDLV.CHAR_VAL_FK1 CP_ID       
                ,(select max(ac.adhoc_char_val) keep (dense_rank last order by ac.effdt)
                  from ci_acct_char ac
                  where ac.acct_id = A.ACCT_ID
                   and ac.char_type_cd = 'DOG-NUM'
                   and ac.effdt <= p.date_end) DOG_NUM
                ,(select max(adhoc_char_val) keep (dense_rank last order by effdt)
                    from ci_per_char
                    where char_type_cd = 'SHORT-NM'
                       and per_id = acct_per.per_id
                       and effdt <=  p.date_end) DOG_NAME
                ,(select inn.per_id_nbr
                  from ci_per_id inn
                  where inn.per_id = acct_per.per_id
                    and inn.id_type_cd = 'INN-Q' ) DOG_INN                          
                ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = OB_OBSL.CHAR_VAL_FK1
                   and pc.char_type_cd = 'PREM_COD'
                   and pc.effdt <= p.date_end) EO_CODE         
                ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = OB_OBSL.CHAR_VAL_FK1
                   and pc.char_type_cd = 'PREM-NAM'
                   and pc.effdt <= p.date_end) EO_NAME    
                ,(select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = OB_OBSL.CHAR_VAL_FK1
                   and pc.char_type_cd = 'CP-MAX-P'
                   and pc.effdt <= p.date_end) EO_MAX_P
                ,(select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = POINTDLV.CHAR_VAL_FK1
                   and pc.char_type_cd = 'CP-MAX-P'
                   and pc.effdt <= p.date_end) CP_MAX_P         
                ,(select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = POINTDLV.CHAR_VAL_FK1
                   and pc.char_type_cd = 'PRIS-P'
                   and pc.effdt <= p.date_end) CP_PRIS_P 
                ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = POINTDLV.CHAR_VAL_FK1
                   and pc.char_type_cd = 'CP-NAME'
                   and pc.effdt <= p.date_end) CP_NAME  

          FROM ACCT A
          join parameters p on 1 = 1
          inner join ci_wf_proc_char OB_OBSL on OB_OBSL.WF_PROC_ID = A.WF_PROC_ID and OB_OBSL.CHAR_TYPE_CD = 'OB-OBSL'
          inner join ci_wf_proc_char POINTDLV on POINTDLV.wf_proc_id = A.WF_PROC_ID and POINTDLV.char_type_cd = 'POINTDLV'
          inner join ci_acct_per acct_per on acct_per.acct_id = a.acct_id and acct_per.main_cust_sw = 'Y' 
          WHERE A.RANK_ROW = 1          
          )
          /*Данные нагрузки ТП*/
          , CP_NAGRUZ AS (
          SELECT /*+MATERIALIZE*/
            AEC.ACCT_ID,
            AEC.EO_ID,
            AEC.CP_ID,
            MAX(TL.DESCR || ', ' || EQ.MODEL_CD || ', ' || EQ.SERIAL_NBR) NAGRUZ_DESCR
          /*  SUM(CASE WHEN A.HALF_LIM = 'Y' THEN TO_NUMBER(NVL(REPLACE(P_OBORUD.ADHOC_CHAR_VAL, ',', '.'), '0'), '9999999990.00') * SP_EQ.EQ_CNT
                     ELSE 0 END) SUM_P,
            SUM(TO_NUMBER(NVL(REPLACE(P_OBORUD.ADHOC_CHAR_VAL, ',', '.'), '0'), '9999999990.00') * SP_EQ.EQ_CNT ) SUM_P_ALL_CP  */          
          FROM ACCT_EO_CP AEC
          INNER JOIN CI_SP CONNECT_SP ON CONNECT_SP.PREM_ID = AEC.CP_ID AND CONNECT_SP.SP_TYPE_CD='SVIAZI-K'
          INNER JOIN CI_SP_EQ SP_EQ ON SP_EQ.SP_ID = CONNECT_SP.SP_ID
          INNER JOIN CI_ITEM EQ ON EQ.ITEM_ID = SP_EQ.ITEM_ID_EQ
          INNER JOIN CI_ITEM_TYPE_L TL ON TL.ITEM_TYPE_CD = EQ.ITEM_TYPE_CD AND TL.LANGUAGE_CD = 'RUS'
          --LEFT JOIN CI_ITEM_CHAR P_OBORUD ON P_OBORUD.ITEM_ID = EQ.ITEM_ID AND P_OBORUD.CHAR_TYPE_CD='P-OBORUD'
          --WHERE (P_OBORUD.EFFDT IS NULL OR P_OBORUD.EFFDT = (SELECT MAX(O.EFFDT) FROM CI_ITEM_CHAR O WHERE O.ITEM_ID = P_OBORUD.ITEM_ID AND O.CHAR_TYPE_CD = P_OBORUD.CHAR_TYPE_CD))
          GROUP BY 
            AEC.ACCT_ID,
            AEC.EO_ID,
            AEC.CP_ID
          )
          /*Среднесуточное потребление*/
/*          , ACCT_VERSION AS (
          SELECT \* +MATERIALIZE*\ distinct
                  -- A.WF_PROC_ID
                 A.ACCT_ID
                 -- ,A.EO_ID
                 ,V.CM_BILL_CALCULATION_VERSION_ID
          FROM DIST_ACCT A 
          join PARAMETERS p on 1 = 1
          JOIN CM_BILL_CALCULATION BC ON BC.ACCT_ID = A.ACCT_ID 
           AND BC.CM_CONSUMPTION_PERIOD_ID = (select max(bc2.CM_CONSUMPTION_PERIOD_ID) from CM_BILL_CALCULATION bc2, CM_BILL_CALCULATION_VERSION v2
                                               where bc2.acct_id = a.acct_id 
                                                 and bc2.CM_CONSUMPTION_PERIOD_ID < TO_CHAR(p.DATE_BEGIN,'YYYY-MM')
                                                 and v2.cm_bill_calculation_id = bc2.cm_bill_calculation_id
                                                 and v2.freeze_dttm is not null)
          INNER JOIN CM_BILL_CALCULATION_VERSION V ON V.CM_BILL_CALCULATION_ID = BC.CM_BILL_CALCULATION_ID
          WHERE 1 = 1
             AND V.SEQ_NUM = (SELECT MAX(O.SEQ_NUM) FROM CM_BILL_CALCULATION_VERSION O WHERE V.CM_BILL_CALCULATION_ID = O.CM_BILL_CALCULATION_ID AND O.FREEZE_DTTM IS not NULL)
             AND BC.SEQ_NUM = (SELECT MAX(O.SEQ_NUM) FROM CM_BILL_CALCULATION O WHERE BC.BILL_ID = O.BILL_ID)
          )
          , ACCT_CALC AS (
          SELECT \* +MATERIALIZE*\
                --V.WF_PROC_ID
                V.ACCT_ID
                ,ROUND(SUM(I.CM_CONSUMPTION * decode(trim(I.CM_TRANSIT_EO_CODE), null, 1, -1) / I.CM_NUMBER_OF_WORK_DAY_IN_MONTH),2) SUM_CONSUMPTION
          FROM ACCT_VERSION V
          INNER JOIN CM_BILL_CALCULATION_EO_ACCT EO_ACCT ON EO_ACCT.CM_BILL_CALCULATION_VERSION_ID = V.CM_BILL_CALCULATION_VERSION_ID
          INNER JOIN CM_BILL_CALCULATION_EO_IND I ON I.CM_BILL_CALCULATION_EO_ACCT_ID = EO_ACCT.CM_BILL_CALCULATION_EO_ACCT_ID
          WHERE 1 = 1
             AND (I.CM_IS_MAIN_SA_SW = 'Y' OR TRIM(I.CM_TRANSIT_EO_CODE) IS NOT NULL)
          GROUP BY \*V.WF_PROC_ID, *\V.ACCT_ID
          )*/
          , DRAFT AS (
             Select  /*+MATERIALIZE*/
                     DISTINCT
                     A.ACCT_ID,
                     a.wf_proc_id,
                     ME_ID.CHAR_VAL_FK1 AS MATH_EVT_ID,
                     c.case_id draft_id,
                     TO_DATE(DR_TERMP.ADHOC_CHAR_VAL,'DD.MM.YYYY') DR_TERMP                     
              from ACCT A
              inner join ci_wf_proc_char DZ_DRAFT on a.wf_proc_id_NOTIFICATION = DZ_DRAFT.WF_PROC_ID and DZ_DRAFT.Char_Type_Cd = 'DZ-DRAFT'
              inner join ci_case c on c.case_id = DZ_DRAFT.Char_Val_Fk1 AND c.case_type_cd = 'DRAFT'
              inner join ci_case_char ME_ID ON ME_ID.CASE_ID = C.CASE_ID and ME_ID.Char_Type_Cd = 'ME-ID'
              inner join ci_case_char DR_TERMP ON DR_TERMP.CASE_ID = C.CASE_ID and DR_TERMP.Char_Type_Cd = 'DR-TERMP'               
               WHERE A.RANK_ROW = 1 
          )
          /*Задолженность потребителя на начало месяца*/
          , ACCOUNT_ENTRY AS (
          Select /*+MATERIALIZE*/
                 D.ACCT_ID, 
                 D.wf_proc_id,
                 --RP.CM_RETAIL_TYPE,
                 --D.MATH_EVT_ID,
                 SUM(CASE WHEN RP.CM_RETAIL_TYPE IN ('0','2')  THEN AE.CUR_AMT ELSE 0 END) CUR_AMT_BEGIN   
          from PARAMETERS PR 
             inner join DRAFT D ON 1 = 1
              inner join ci_ft ft on ft.match_evt_id = D.MATH_EVT_ID
              inner join cm_account_entry ae on ae.cm_generative_ft_id = ft.ft_id
              LEFT  JOIN CM_ACCT_PAY_MATCH_EVT_PROP PAY_PROP ON PAY_PROP.CM_ACCT_PAY_MATCH_EVT_PROP_ID=AE.CM_ACCT_PAY_MATCH_EVT_PROP_ID    
              INNER JOIN CM_RATE_PROP RP ON AE.CM_RATE_PROP_ID = RP.CM_RATE_PROP_ID AND AE.FREEZE_DTTM IS NOT NULL
              INNER JOIN CI_FT FT ON FT.FT_ID = AE.CM_GENERATIVE_FT_ID 
              inner join CI_CASE FD ON FD.CASE_ID = AE.CM_FIN_DOCUMENT_ID
              inner join CI_CASE_CHAR FDC ON FDC.CASE_ID  = FD.CASE_ID AND FDC.CHAR_TYPE_CD IN ('INVDATEC','BILLDATE')
              --left join ci_case_char dnz on dnz.case_id = d.draft_id and dnz.char_type_cd = 'DR-TERMP'
              WHERE AE.Accounting_Dt <= TRUNC(PR.DATE_BEGIN, 'MM')
                AND D.DR_TERMP < TRUNC(PR.DATE_BEGIN, 'MM')              
                --and (dnz.case_id is null or to_date(dnz.adhoc_char_val, 'dd.mm.yyyy') < TRUNC(PR.DATE_BEGIN, 'MM'))
          GROUP BY D.ACCT_ID , /*D.MATH_EVT_ID , RP.CM_RETAIL_TYPE,*/ D.wf_proc_id
          )
          /*Оплата, поступившая за месяц*/
          , ACCT_PAY AS (
          Select /*+MATERIALIZE*/
                 D.ACCT_ID, 
                 D.wf_proc_id,
                 --RP.CM_RETAIL_TYPE,
                 --D.MATH_EVT_ID,
                 -SUM(CASE WHEN (NOT GFT_PAY.PARENT_ID IN ('REFOUND','REFOUNDP') AND NOT PAY_PROP_PAY.CM_ACCT_PAY_MATCH_EVT_PROP_ID IS NULL  AND NOT(RP_PAY.CM_RETAIL_TYPE='0' AND GFT_PAY.PARENT_ID='TRANSF')) THEN AE_PAY.CUR_AMT ELSE 0 END) SUM_PAY
          from PARAMETERS PR 
          inner join DRAFT D ON 1 = 1
          INNER JOIN CI_FT            GFT_PAY ON GFT_PAY.MATCH_EVT_ID = D.MATH_EVT_ID
          INNER JOIN CM_ACCOUNT_ENTRY AE_PAY ON GFT_PAY.FT_ID=AE_PAY.CM_GENERATIVE_FT_ID
          LEFT  JOIN CM_ACCT_PAY_MATCH_EVT_PROP PAY_PROP_PAY ON PAY_PROP_PAY.CM_ACCT_PAY_MATCH_EVT_PROP_ID=AE_PAY.CM_ACCT_PAY_MATCH_EVT_PROP_ID
          INNER JOIN CM_RATE_PROP     RP_PAY ON RP_PAY.CM_RATE_PROP_ID = AE_PAY.CM_RATE_PROP_ID
          INNER JOIN CM_FT_EXP        FE_PAY ON FE_PAY.FT_ID=AE_PAY.CM_ACCT_REC_MATCH_EVT_ID
          INNER JOIN CI_SA SA_PAY ON RP_PAY.SA_ID = SA_PAY.SA_ID
          WHERE 1 = 1 
            AND AE_PAY.Accounting_Dt BETWEEN TRUNC(PR.DATE_BEGIN, 'MM') AND LEAST(TRUNC(SYSDATE), PR.DATE_END)
          GROUP BY D.ACCT_ID , /*D.MATH_EVT_ID , RP.CM_RETAIL_TYPE,*/ D.wf_proc_id
          )
          /*Начисление за месяц*/
          , ACCT_CHARGE AS (
          select /*+ MATERIALIZE*/ 
                 A.ACCT_ID,
                 SUM(AE.CUR_AMT) SUM_CHARGE,
                 ROUND(SUM(AE.CUR_AMT) / (MAX(FE.Cm_Price_Amount)*24)) SUM_CONSUMPTION                            
          from PARAMETERS PR 
          INNER JOIN DIST_ACCT A ON 1 = 1 
          inner join ci_case bill on bill.acct_id = A.acct_id and bill.case_type_cd = 'INVOICE'
          INNER JOIN CM_ACCOUNT_ENTRY AE ON AE.CM_FIN_DOCUMENT_ID=BILL.CASE_ID AND AE.SHOW_ON_BILL_SW='Y'
          LEFT  JOIN CM_ACCT_PAY_MATCH_EVT_PROP PAY_PROP ON PAY_PROP.CM_ACCT_PAY_MATCH_EVT_PROP_ID=AE.CM_ACCT_PAY_MATCH_EVT_PROP_ID
          INNER JOIN CM_RATE_PROP     RP ON RP.CM_RATE_PROP_ID = AE.CM_RATE_PROP_ID AND RP.CM_RETAIL_TYPE='2'
          INNER JOIN CI_FT            GFT ON GFT.FT_ID=AE.CM_GENERATIVE_FT_ID
          INNER JOIN CM_FT_EXP        FE ON FE.FT_ID=AE.CM_ACCT_REC_MATCH_EVT_ID
          WHERE 1 = 1  
            AND trunc(AE.Accounting_Dt, 'mm') = (select max(trunc(ae2.Accounting_Dt, 'mm')) from CM_ACCOUNT_ENTRY ae2, ci_case bill2
                                                  where bill2.acct_id = a.acct_id
                                                    and bill2.case_type_cd = 'INVOICE'
                                                    and ae2.cm_fin_document_id = bill2.case_id 
                                                    and ae2.show_on_bill_sw = 'Y' 
                                                    and ae2.Accounting_Dt < TRUNC(PR.DATE_BEGIN, 'MM')
                                                    and exists(select 1 from CM_RATE_PROP RP2, ci_ft ft2
                                                                where RP2.CM_RATE_PROP_ID = AE2.CM_RATE_PROP_ID 
                                                                  AND RP2.CM_RETAIL_TYPE = '2'
                                                                  and ft2.FT_ID = AE2.CM_GENERATIVE_FT_ID))
          GROUP BY A.ACCT_ID
          )
          , PRE_FINAL AS
          (
          select --A.WF_PROC_ID
          /*+ MATERIALIZE*/ 
                DISTINCT
                DAT_LIM.ADHOC_CHAR_VAL DAT_LIM
                ,A.ACCT_ID
                ,A.DOG_NUM
                ,A.DOG_NAME  
                ,A.DOG_INN                    
                ,A.EO_CODE               
                ,A.EO_MAX_P 
                ,A.CP_NAME
                ,NVL(A.CP_PRIS_P, A.CP_MAX_P) CP_PRIS_P       
                ,A.CP_MAX_P   CP_MAX_P_PLAN        
                ,coalesce(CASE WHEN DAT_LIMF.ADHOC_CHAR_VAL IS NOT NULL THEN
                      CASE WHEN trim(DAT_LIM.CHAR_TYPE_CD) = 'DAT-LIM' AND TRIM(DZ_R_MOD.CHAR_VAL) = '0' THEN  
                            (select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                              from ci_prem_char pc
                              where pc.prem_id = A.CP_ID
                               and pc.char_type_cd = 'CP-MAX-P'
                               and pc.effdt <= p.date_end)              
                           WHEN trim(DAT_LIM.CHAR_TYPE_CD) = 'DAT-LIMA' AND TRIM(DZ_R_MOD.CHAR_VAL) = 'TO-AB' THEN 
                            (select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                              from ci_prem_char pc
                              where pc.prem_id = A.CP_ID
                               and pc.char_type_cd = 'CP-MAX-P'
                               and pc.effdt <= p.date_end)                      
                           WHEN trim(DAT_LIM.CHAR_TYPE_CD) = 'DAT-LIMT' AND TRIM(DZ_R_MOD.CHAR_VAL) = 'TO-TB' THEN
                            (select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                              from ci_prem_char pc
                              where pc.prem_id = A.CP_ID
                               and pc.char_type_cd = 'CP-MAX-P'
                               and pc.effdt <= p.date_end)                    
                           WHEN trim(DAT_LIM.CHAR_TYPE_CD) = 'DAT-LIMP' AND TRIM(DZ_R_MOD.CHAR_VAL) = '1' THEN
                            (select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                              from ci_prem_char pc
                              where pc.prem_id = A.CP_ID
                               and pc.char_type_cd = 'CP-MAX-P'
                               and pc.effdt <= p.date_end)                    
                           WHEN trim(DAT_LIM.CHAR_TYPE_CD) = 'DAT-LIMS' AND TRIM(DZ_R_MOD.CHAR_VAL) = 'SELF' THEN 
                            (select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                              from ci_prem_char pc
                              where pc.prem_id = A.CP_ID
                               and pc.char_type_cd = 'CP-MAX-P'
                               and pc.effdt <= p.date_end)                    
                       ELSE 0 END   
               END,0) CP_MAX_P_FAKT

                --,NVL(AC.SUM_CONSUMPTION,0) SUM_CONSUMPTION 
                ,NVL(ACH.SUM_CONSUMPTION,0) SUM_CONSUMPTION                           

                ,TO_NUMBER(NVL(REPLACE(NTF_AMT.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') NTF_AMT 
                
               /* ,TO_NUMBER(NVL(REPLACE(NTF_AMT0.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') +      
                 TO_NUMBER(NVL(REPLACE(NTF_AMT2.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') SUM_SCHEDULE*/
                ,TO_NUMBER(NVL(REPLACE(EVT_EBNTF_NTF_AMT0.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') +      
                 TO_NUMBER(NVL(REPLACE(EVT_EBNTF_NTF_AMT2.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') SUM_SCHEDULE   

                ,AE.CUR_AMT_BEGIN
                ,AP.SUM_PAY
                ,NVL(ACH.SUM_CHARGE, 0)  SUM_CHARGE
                ,DECODE(TRIM(CN.NAGRUZ_DESCR), NULL, 'эл. оборудование', TRIM(CN.NAGRUZ_DESCR)) NAGRUZ_DESCR
                --,NVL(CN.SUM_P_ALL_CP,0) SUM_P_ALL_CP
                --,NVL(CN.SUM_P,0) SUM_P

                ,OPER_LIM.CHAR_VAL_FK1 OPER_LIM_ID   
                ,(select max(adhoc_char_val) keep (dense_rank last order by effdt)
                  from ci_per_char
                  where char_type_cd = 'SHORT-NM'
                    and per_id = rpad(trim(OPER_LIM.CHAR_VAL_FK1),10)
                    and effdt <=  p.date_end) OPER_LIM_NAME            
                --,us.last_name||' '||us.first_name EXECUTOR_NAME 
                ,NVL(TRIM(HALF_LIM.CHAR_VAL), 'N') HALF_LIM
                ,CASE WHEN DAT_LIMF.ADHOC_CHAR_VAL IS NOT NULL THEN DZ_R_MOD_DESCR.DESCR || ' ' || DAT_LIMF.ADHOC_CHAR_VAL
                 ELSE DZ_R_MOD_DESCR.DESCR || ' ' || PR_N_LIM_DESCR.DESCR
                 END  DZ_R_MOD     
          from ACCT_EO_CP A
          join PARAMETERS p on 1 = 1
          JOIN ci_wf_proc w ON w.wf_proc_id = a.wf_proc_id
          inner join ci_wf_proc_char DZ_DPRT on DZ_DPRT.wf_proc_id = w.wf_proc_id and DZ_DPRT.char_type_cd='DZ-DPRT'
           and (TRIM(P.DEPARTMENT) is null or TRIM(DZ_DPRT.CHAR_VAL_FK1) = TRIM(P.DEPARTMENT))
          inner join ci_wf_proc_char DAT_LIM on DAT_LIM.WF_PROC_ID = w.wf_proc_id 
           and DAT_LIM.CHAR_TYPE_CD in ('DAT-LIM', 'DAT-LIMP'/*, 'DAT-LIMA', 'DAT-LIMT', 'DAT-LIMS'*/)
           and trim(DAT_LIM.ADHOC_CHAR_VAL) <> 'UNDEF'
           and to_date(trim(DAT_LIM.ADHOC_CHAR_VAL),'dd.mm.yyyy') between P.DATE_BEGIN and P.DATE_END
          LEFT JOIN CP_NAGRUZ CN ON CN.ACCT_ID = A.ACCT_ID AND CN.EO_ID = A.EO_ID AND CN.CP_ID = A.CP_ID 
          left join ACCOUNT_ENTRY AE on AE.ACCT_ID = A.ACCT_ID AND AE.WF_PROC_ID = A.WF_PROC_ID
          left join ACCT_PAY AP ON AP.ACCT_ID = A.ACCT_ID AND AP.WF_PROC_ID = A.WF_PROC_ID
         -- left join ACCT_CALC AC ON AC.ACCT_ID = A.ACCT_ID -- AND AC.WF_PROC_ID = A.WF_PROC_ID
          left join ACCT_CHARGE ACH ON ACH.ACCT_ID = A.ACCT_ID -- AND ACH.WF_PROC_ID = A.WF_PROC_ID
          ----
          left join ci_wf_proc_char NTF_AMT  on NTF_AMT.WF_PROC_ID = a.wf_proc_id_NOTIFICATION and NTF_AMT.CHAR_TYPE_CD = 'NTF-AMT'
          left join ci_wf_proc_char NTF_AMT0 on NTF_AMT0.WF_PROC_ID = a.wf_proc_id_NOTIFICATION and NTF_AMT0.CHAR_TYPE_CD = 'NTF-AMT0'
          left join ci_wf_proc_char NTF_AMT2 on NTF_AMT2.WF_PROC_ID = a.wf_proc_id_NOTIFICATION and NTF_AMT2.CHAR_TYPE_CD = 'NTF-AMT2'
          left join ci_wf_evt EVT_EBNTF on EVT_EBNTF.WF_PROC_ID = a.wf_proc_id_NOTIFICATION and EVT_EBNTF.WF_EVT_TYPE_CD IN ('EVT-EBNTF-03', 'EVT-EBNTF-05')
          left join ci_wf_evt_char EVT_EBNTF_NTF_AMT0 on EVT_EBNTF_NTF_AMT0.WF_PROC_ID = EVT_EBNTF.wf_proc_id and EVT_EBNTF_NTF_AMT0.EVT_SEQ = EVT_EBNTF.EVT_SEQ and EVT_EBNTF_NTF_AMT0.CHAR_TYPE_CD = 'NTF-AMT0'
          left join ci_wf_evt_char EVT_EBNTF_NTF_AMT2 on EVT_EBNTF_NTF_AMT2.WF_PROC_ID = EVT_EBNTF.wf_proc_id and EVT_EBNTF_NTF_AMT2.EVT_SEQ = EVT_EBNTF.EVT_SEQ and EVT_EBNTF_NTF_AMT2.CHAR_TYPE_CD = 'NTF-AMT2'
          -----
          inner join ci_wf_proc_char OPER_LIM on OPER_LIM.wf_proc_id = A.WF_PROC_ID and OPER_LIM.char_type_cd = 'OPER-LIM'
          left join ci_wf_proc_char EXECUTOR on EXECUTOR.wf_proc_id = A.WF_PROC_ID and EXECUTOR.char_type_cd = 'EXECUTOR'
          left join sc_user us on us.user_id = RPAD(TRIM(EXECUTOR.char_val_fk1),8)
          -----

          left join ci_wf_proc_char HALF_LIM on HALF_LIM.wf_proc_id = A.WF_PROC_ID and HALF_LIM.char_type_cd = 'HALF-LIM'
          left join 
            (ci_wf_evt EVT_EBLIM_04
              left join ci_wf_evt_char DAT_LIMF on DAT_LIMF.WF_PROC_ID = EVT_EBLIM_04.wf_proc_id and DAT_LIMF.EVT_SEQ = EVT_EBLIM_04.EVT_SEQ and DAT_LIMF.CHAR_TYPE_CD = 'DAT-LIMF'
              left join ci_wf_evt_char DZ_R_MOD on DZ_R_MOD.WF_PROC_ID = EVT_EBLIM_04.wf_proc_id and DZ_R_MOD.EVT_SEQ = EVT_EBLIM_04.EVT_SEQ and DZ_R_MOD.CHAR_TYPE_CD = 'DZ-R-MOD'
              left join ci_wf_evt_char DZ_R_LIM on DZ_R_LIM.WF_PROC_ID = EVT_EBLIM_04.wf_proc_id and DZ_R_LIM.EVT_SEQ = EVT_EBLIM_04.EVT_SEQ and DZ_R_LIM.CHAR_TYPE_CD = 'DZ-R-LIM'
              left join ci_wf_evt_char PR_N_LIM on PR_N_LIM.WF_PROC_ID = EVT_EBLIM_04.wf_proc_id and PR_N_LIM.EVT_SEQ = EVT_EBLIM_04.EVT_SEQ and PR_N_LIM.CHAR_TYPE_CD = 'PR-N-LIM'
              left join ci_char_val_l DZ_R_MOD_DESCR ON DZ_R_MOD_DESCR.CHAR_TYPE_CD = DZ_R_MOD.CHAR_TYPE_CD and DZ_R_MOD_DESCR.CHAR_VAL = DZ_R_MOD.CHAR_VAL
              and DZ_R_MOD_DESCR.LANGUAGE_CD = 'RUS'
              left join ci_char_val_l DZ_R_LIM_DESCR ON DZ_R_LIM_DESCR.CHAR_TYPE_CD = DZ_R_LIM.CHAR_TYPE_CD and DZ_R_LIM_DESCR.CHAR_VAL = DZ_R_LIM.CHAR_VAL
              and DZ_R_LIM_DESCR.LANGUAGE_CD = 'RUS'
              left join ci_char_val_l PR_N_LIM_DESCR ON PR_N_LIM_DESCR.CHAR_TYPE_CD = PR_N_LIM.CHAR_TYPE_CD and PR_N_LIM_DESCR.CHAR_VAL = PR_N_LIM.CHAR_VAL
              and PR_N_LIM_DESCR.LANGUAGE_CD = 'RUS'
           ) on EVT_EBLIM_04.WF_PROC_ID = A.wf_proc_id 
            and EVT_EBLIM_04.WF_EVT_TYPE_CD = 'EVT-EBLIM-04'
            and EVT_EBLIM_04.wf_evt_stat_flg = '30'
            and trim(DAT_LIM.CHAR_TYPE_CD) = decode(TRIM(DZ_R_MOD.CHAR_VAL),
                                              'SELF', 'DAT-LIMS',
                                              '1',    'DAT-LIMP',
                                              'TO-TB','DAT-LIMT',
                                              'TO-AB','DAT-LIMA',
                                              '0',    'DAT-LIM')
          where (DAT_LIMF.WF_PROC_ID is null 
                 or 
                 DAT_LIMF.SEQ_NUM = (select max(ec.seq_num) from ci_wf_evt_char ec 
                                      where ec.wf_proc_id = DAT_LIMF.WF_PROC_ID and ec.evt_seq = DAT_LIMF.evt_seq and ec.char_type_cd = DAT_LIMF.char_type_cd))
            and (DZ_R_MOD.WF_PROC_ID is null 
                 or 
                 DZ_R_MOD.SEQ_NUM = (select max(ec.seq_num) from ci_wf_evt_char ec 
                                      where ec.wf_proc_id = DZ_R_MOD.WF_PROC_ID and ec.evt_seq = DZ_R_MOD.evt_seq and ec.char_type_cd = DZ_R_MOD.char_type_cd))
            and (DZ_R_LIM.WF_PROC_ID is null 
                 or 
                 DZ_R_LIM.SEQ_NUM = (select max(ec.seq_num) from ci_wf_evt_char ec 
                                      where ec.wf_proc_id = DZ_R_LIM.WF_PROC_ID and ec.evt_seq = DZ_R_LIM.evt_seq and ec.char_type_cd = DZ_R_LIM.char_type_cd))
            and (PR_N_LIM.WF_PROC_ID is null 
                 or 
                 PR_N_LIM.SEQ_NUM = (select max(ec.seq_num) from ci_wf_evt_char ec 
                                      where ec.wf_proc_id = PR_N_LIM.WF_PROC_ID and ec.evt_seq = PR_N_LIM.evt_seq and ec.char_type_cd = PR_N_LIM.char_type_cd))
          ORDER BY A.DOG_NUM                                       
          )

          SELECT 
                  ROW_NUMBER() OVER(PARTITION BY F.DAT_LIM ORDER BY TO_DATE(F.DAT_LIM, 'DD.MM.YYYY'), F.DOG_NUM, F.EO_CODE, F.CP_NAME) RN,
                  COUNT (1) OVER(PARTITION BY F.DAT_LIM) RN_TOTAL,                   
                  F.DAT_LIM,
                  F.ACCT_ID,
                  F.DOG_NUM,
                  F.DOG_NAME,
                  F.DOG_INN,
                  F.EO_CODE,
                  F.EO_MAX_P,
                  F.CP_NAME,
                  F.CP_PRIS_P,
                  F.CP_MAX_P_PLAN,
                  F.CP_MAX_P_FAKT,
                  F.SUM_CONSUMPTION,
                  F.NTF_AMT,
                  F.SUM_SCHEDULE,
                  F.CUR_AMT_BEGIN,
                  F.SUM_PAY,
                  F.SUM_CHARGE,
                  F.NAGRUZ_DESCR,
                  F.OPER_LIM_ID,
                  F.OPER_LIM_NAME,
                  --F.EXECUTOR_NAME,
                  F.HALF_LIM,
                  F.DZ_R_MOD,
                  SUM(DISTINCT F.CUR_AMT_BEGIN) OVER(PARTITION BY F.DAT_LIM) CUR_AMT_BEGIN_TOTAL, 
                  SUM(DISTINCT F.SUM_CHARGE) OVER(PARTITION BY F.DAT_LIM) SUM_CHARGE_TOTAL,     
                  SUM(DISTINCT F.SUM_SCHEDULE) OVER(PARTITION BY F.DAT_LIM) SUM_SCHEDULE_TOTAL,     
                  SUM(DISTINCT F.SUM_PAY) OVER(PARTITION BY F.DAT_LIM) SUM_PAY_TOTAL,
                  SUM(DISTINCT F.CUR_AMT_BEGIN) OVER() CUR_AMT_BEGIN_ALL, 
                  SUM(DISTINCT F.SUM_CHARGE) OVER() SUM_CHARGE_ALL,     
                  SUM(DISTINCT F.SUM_SCHEDULE) OVER() SUM_SCHEDULE_ALL,     
                  SUM(DISTINCT F.SUM_PAY) OVER() SUM_PAY_ALL                                              
          FROM PRE_FINAL F
          ORDER BY to_date(F.DAT_LIM, 'dd.mm.yyyy'), F.DOG_NUM, F.EO_CODE, F.CP_NAME
     
     ) LOOP
     
     IF I.RN = 1 THEN
       
        V_RET.RN := NULL;
        V_RET.DAT_LIM := NULL;
        V_RET.ACCT_ID := NULL;
        V_RET.DOG_NUM := NULL;
        V_RET.DOG_NAME := I.DAT_LIM || ' г.';
        V_RET.DOG_INN := NULL;
        V_RET.EO_CODE := NULL;
        V_RET.EO_MAX_P := NULL;
        V_RET.CP_NAME := NULL;
        V_RET.CP_PRIS_P := NULL;
        V_RET.CP_MAX_P_PLAN := NULL;
        V_RET.CP_MAX_P_FAKT := NULL;
        V_RET.SUM_CONSUMPTION := NULL;
        V_RET.NTF_AMT := NULL;
        V_RET.SUM_SCHEDULE := NULL;
        V_RET.CUR_AMT_BEGIN :=NULL;
        V_RET.SUM_PAY := NULL;
        V_RET.SUM_CHARGE := NULL;
        V_RET.NAGRUZ_DESCR := NULL;
        V_RET.OPER_LIM_ID := NULL;
        V_RET.OPER_LIM_NAME := NULL;
        V_RET.EXECUTOR_NAME := NULL;
        V_RET.HALF_LIM := NULL;
        V_RET.DZ_R_MOD := NULL;
        V_RET.FORMAT := 'BOLD';     

        PIPE ROW(V_RET);        
       
      END IF;
     
      V_RET.RN := I.RN;
      V_RET.DAT_LIM := I.DAT_LIM;
      V_RET.ACCT_ID := I.ACCT_ID;
      V_RET.DOG_NUM := I.DOG_NUM;
      V_RET.DOG_NAME := I.DOG_NAME;
      V_RET.DOG_INN := I.DOG_INN;
      V_RET.EO_CODE := I.EO_CODE;
      V_RET.EO_MAX_P := I.EO_MAX_P;
      V_RET.CP_NAME := I.CP_NAME;
      V_RET.CP_PRIS_P := I.CP_PRIS_P;
      V_RET.CP_MAX_P_PLAN := I.CP_MAX_P_PLAN;
      V_RET.CP_MAX_P_FAKT := I.CP_MAX_P_FAKT;
      V_RET.SUM_CONSUMPTION := I.SUM_CONSUMPTION;
      V_RET.NTF_AMT := I.NTF_AMT;
      V_RET.SUM_SCHEDULE := I.SUM_SCHEDULE;
      V_RET.CUR_AMT_BEGIN := I.CUR_AMT_BEGIN;
      V_RET.SUM_PAY := I.SUM_PAY;
      V_RET.SUM_CHARGE := I.SUM_CHARGE;
      V_RET.NAGRUZ_DESCR := I.NAGRUZ_DESCR;
      V_RET.OPER_LIM_ID := I.OPER_LIM_ID;
      V_RET.OPER_LIM_NAME := I.OPER_LIM_NAME;
      V_RET.EXECUTOR_NAME := NULL; --I.EXECUTOR_NAME;
      V_RET.HALF_LIM := I.HALF_LIM;
      V_RET.DZ_R_MOD := I.DZ_R_MOD;
      V_RET.FORMAT := 'NOBOLD';       
    
      PIPE ROW(V_RET);   
      
      IF I.RN = I.RN_TOTAL THEN
        
        V_RET.RN := NULL;
        V_RET.DAT_LIM := NULL;
        V_RET.ACCT_ID := NULL;
        V_RET.DOG_NUM := NULL;
        V_RET.DOG_NAME := NULL;
        V_RET.DOG_INN := NULL;
        V_RET.EO_CODE := NULL;
        V_RET.EO_MAX_P := NULL;
        V_RET.CP_NAME := NULL;
        V_RET.CP_PRIS_P := NULL;
        V_RET.CP_MAX_P_PLAN := NULL;
        V_RET.CP_MAX_P_FAKT := NULL;
        V_RET.SUM_CONSUMPTION := NULL;
        V_RET.NTF_AMT := NULL;
        V_RET.SUM_SCHEDULE := I.SUM_SCHEDULE_TOTAL;
        V_RET.CUR_AMT_BEGIN := I.CUR_AMT_BEGIN_TOTAL;
        V_RET.SUM_PAY := I.SUM_PAY_TOTAL;
        V_RET.SUM_CHARGE := I.SUM_CHARGE_TOTAL;
        V_RET.NAGRUZ_DESCR := NULL;
        V_RET.OPER_LIM_ID := NULL;
        V_RET.OPER_LIM_NAME := NULL;
        V_RET.EXECUTOR_NAME := NULL;
        V_RET.HALF_LIM := NULL;
        V_RET.DZ_R_MOD := NULL; 
        V_RET.FORMAT := 'RED';              

        PIPE ROW(V_RET);              
             
      END IF; 
      
       V_CUR_AMT_BEGIN_ALL := I.CUR_AMT_BEGIN_ALL; 
       V_SUM_CHARGE_ALL := I.SUM_CHARGE_ALL ;     
       V_SUM_SCHEDULE_ALL := I.SUM_SCHEDULE_ALL;     
       V_SUM_PAY_ALL := I.SUM_PAY_ALL;
     
   END LOOP;  

        V_RET.RN := NULL;
        V_RET.DAT_LIM := NULL;
        V_RET.ACCT_ID := NULL;
        V_RET.DOG_NUM := NULL;
        V_RET.DOG_NAME := 'Итого';
        V_RET.DOG_INN := NULL;
        V_RET.EO_CODE := NULL;
        V_RET.EO_MAX_P := NULL;
        V_RET.CP_NAME := NULL;
        V_RET.CP_PRIS_P := NULL;
        V_RET.CP_MAX_P_PLAN := NULL;
        V_RET.CP_MAX_P_FAKT := NULL;
        V_RET.SUM_CONSUMPTION := NULL;
        V_RET.NTF_AMT := NULL;
        V_RET.SUM_SCHEDULE := V_SUM_SCHEDULE_ALL;
        V_RET.CUR_AMT_BEGIN := V_CUR_AMT_BEGIN_ALL;
        V_RET.SUM_PAY := V_SUM_PAY_ALL;
        V_RET.SUM_CHARGE := V_SUM_CHARGE_ALL;
        V_RET.NAGRUZ_DESCR := NULL;
        V_RET.OPER_LIM_ID := NULL;
        V_RET.OPER_LIM_NAME := NULL;
        V_RET.EXECUTOR_NAME := NULL;
        V_RET.HALF_LIM := NULL;
        V_RET.DZ_R_MOD := NULL;
        V_RET.FORMAT := 'BOLD';          

        PIPE ROW(V_RET);         

   END DZ_SCHEDULE_LIMITATION;
   
FUNCTION DZ_REQUEST_LIMITATION(P_DATE_BEGIN DATE, P_DATE_END DATE, P_DEPARTMENT VARCHAR2, P_USER VARCHAR2) 
   RETURN T_REQUEST_LIMITATION_SET PIPELINED
   
   IS 
   V_RET  T_REQUEST_LIMITATION;
     
   BEGIN
   
   FOR I IN (
          WITH PARAMETERS AS (
          SELECT 
          /*+MATERIALIZE*/
           TRUNC(P_DATE_BEGIN) /*TO_DATE('01.08.2016','DD.MM.YYYY')*/ DATE_BEGIN,
           TRUNC(P_DATE_END) /*TO_DATE('31.08.2016','DD.MM.YYYY') */DATE_END,
           P_DEPARTMENT /*'TSK1'*/ DEPARTMENT
          FROM DUAL
          ) 
          , CIS_DIVISION_USER AS (
           SELECT /*+MATERIALIZE*/ UE.CIS_DIVISION
           FROM CM_USER_EXP UE
           WHERE UE.USER_ID = RPAD(TRIM(P_USER), 8)
          )           
          , ACCT_LITE AS (
          SELECT DISTINCT 
            A.ACCT_ID 
          FROM CI_DAR_USR U
          INNER JOIN CI_ACC_GRP_DAR AG_DAR ON AG_DAR.DAR_CD=U.DAR_CD
          INNER JOIN CI_ACCT A ON A.ACCESS_GRP_CD=AG_DAR.ACCESS_GRP_CD
          INNER JOIN CIS_DIVISION_USER DU ON DU.CIS_DIVISION = A.CIS_DIVISION   
          WHERE U.USER_ID = RPAD(trim(P_USER), 8) AND U.EXPIRE_DT > SYSDATE
            -- and a.acct_id = '9555386828'
          )
          , ACCT AS (
          SELECT  /*+MATERIALIZE*/
                  W.WF_PROC_ID,
                  W.WF_STAT_FLG,
                  w.cr_by_wf_proc_id WF_PROC_ID_NOTIFICATION,      
                  A.ACCT_ID ACCT_ID,
                  DAT_LIM.ADHOC_CHAR_VAL DAT_LIM,
                  POINTDLV.CHAR_VAL_FK1 CP_ID, 
                  W.CRE_DTTM,                 
                  DENSE_RANK() OVER (PARTITION BY A.ACCT_ID, DAT_LIM.ADHOC_CHAR_VAL, POINTDLV.CHAR_VAL_FK1 ORDER BY A.ACCT_ID, DAT_LIM.ADHOC_CHAR_VAL, POINTDLV.CHAR_VAL_FK1, W.CRE_DTTM DESC) RANK_ROW               
   
          from PARAMETERS PR
          INNER JOIN ci_wf_proc w ON 1 = 1
          inner join ci_wf_proc_char DZ_DPRT on DZ_DPRT.wf_proc_id = w.wf_proc_id and DZ_DPRT.char_type_cd='DZ-DPRT'
          inner join ci_wf_proc_char DAT_LIM on DAT_LIM.WF_PROC_ID = w.wf_proc_id and DAT_LIM.CHAR_TYPE_CD = 'DAT-LIM'
          inner join ci_wf_proc_char POINTDLV on POINTDLV.wf_proc_id = W.WF_PROC_ID and POINTDLV.char_type_cd = 'POINTDLV'          
           --and DAT_LIM.CHAR_TYPE_CD in ('DAT-LIM', 'DAT-LIMA', 'DAT-LIMT', 'DAT-LIMP', 'DAT-LIMS')
           and trim(DAT_LIM.ADHOC_CHAR_VAL) <> 'UNDEF'
          --inner join ci_wf_proc_char SOGLIMIT on SOGLIMIT.wf_proc_id = W.WF_PROC_ID and SOGLIMIT.char_type_cd = 'SOGLIMIT'            
          inner join ci_wf_proc_char LIC_S4ET on LIC_S4ET.WF_PROC_ID = w.wf_proc_id and LIC_S4ET.CHAR_TYPE_CD = 'LIC-S4ET'  
          INNER JOIN ACCT_LITE A ON A.ACCT_ID = LIC_S4ET.CHAR_VAL_FK1
          where 1 = 1
            and w.wf_proc_tmpl_cd IN ('TMPL-EBLIM1','TMPL-EBLIM2','TMPL-EBLIM5')
            and (TRIM(PR.DEPARTMENT) is null or TRIM(DZ_DPRT.CHAR_VAL_FK1) = TRIM(PR.DEPARTMENT))
            and to_date(trim(DAT_LIM.ADHOC_CHAR_VAL),'dd.mm.yyyy') between PR.DATE_BEGIN and PR.DATE_END 
            --and SOGLIMIT.CHAR_VAL = RPAD('Y',16)             
          )
          /*Уникальные ACCT_ID - для подсчета расходов, начислений и т.д., чтобы при 2 процессах данные не задваивались*/
          , DIST_ACCT AS (
            select /*+ MATERIALIZE*/ distinct acct_id from acct 
          )
          /*Данные ЭО и ТП*/
          , ACCT_EO_CP AS (
          SELECT  A.WF_PROC_ID
                 ,A.wf_proc_id_NOTIFICATION
                 ,A.ACCT_ID
                 ,A.WF_STAT_FLG
                 ,A.DAT_LIM
                 ,acct_per.per_id
                 ,OB_OBSL.CHAR_VAL_FK1 EO_ID
                 ,POINTDLV.CHAR_VAL_FK1 CP_ID       
                ,(select max(ac.adhoc_char_val) keep (dense_rank last order by ac.effdt)
                  from ci_acct_char ac
                  where ac.acct_id = A.ACCT_ID
                   and ac.char_type_cd = 'DOG-NUM'
                   and ac.effdt <= p.date_end) DOG_NUM
                ,(select max(adhoc_char_val) keep (dense_rank last order by effdt)
                    from ci_per_char
                    where char_type_cd = 'SHORT-NM'
                       and per_id = acct_per.per_id
                       and effdt <=  p.date_end) DOG_NAME                        
                ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = OB_OBSL.CHAR_VAL_FK1
                   and pc.char_type_cd = 'PREM_COD'
                   and pc.effdt <= p.date_end) EO_CODE         
                ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = OB_OBSL.CHAR_VAL_FK1
                   and pc.char_type_cd = 'PREM-NAM'
                   and pc.effdt <= p.date_end) EO_NAME  
              ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = OB_OBSL.CHAR_VAL_FK1
                   and pc.char_type_cd = 'F-ADR-1'
                   and pc.effdt <= p.date_end) ||
              (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = OB_OBSL.CHAR_VAL_FK1
                   and pc.char_type_cd = 'F-ADR-2'
                   and pc.effdt <= p.date_end) EO_ADRES                     
                ,(select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = OB_OBSL.CHAR_VAL_FK1
                   and pc.char_type_cd = 'CP-MAX-P'
                   and pc.effdt <= p.date_end) EO_MAX_P
                ,(select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = POINTDLV.CHAR_VAL_FK1
                   and pc.char_type_cd = 'CP-MAX-P'
                   and pc.effdt <= p.date_end) CP_MAX_P         
                ,(select max(TO_NUMBER(NVL(TRIM(REPLACE(PC.ADHOC_CHAR_VAL, ',', '.')), '0.00'), '9999999990.000'))keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = POINTDLV.CHAR_VAL_FK1
                   and pc.char_type_cd = 'PRIS-P'
                   and pc.effdt <= p.date_end) CP_PRIS_P 
                ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = POINTDLV.CHAR_VAL_FK1
                   and pc.char_type_cd = 'CP-NAME'
                   and pc.effdt <= p.date_end) CP_NAME 
                ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                  from ci_prem_char pc
                  where pc.prem_id = POINTDLV.CHAR_VAL_FK1
                   and pc.char_type_cd = 'CP-NUM'
                   and pc.effdt <= p.date_end) CP_NUM                     

          FROM ACCT A
          join parameters p on 1 = 1
          inner join ci_wf_proc_char OB_OBSL on OB_OBSL.WF_PROC_ID = A.WF_PROC_ID and OB_OBSL.CHAR_TYPE_CD = 'OB-OBSL'
          inner join ci_wf_proc_char POINTDLV on POINTDLV.wf_proc_id = A.WF_PROC_ID and POINTDLV.char_type_cd = 'POINTDLV'
          inner join ci_acct_per acct_per on acct_per.acct_id = a.acct_id and acct_per.main_cust_sw = 'Y'
          WHERE A.RANK_ROW = 1 
          )
          , IK AS (
          SELECT DISTINCT
            CP.ACCT_ID, 
            CP.CP_ID,
            L.CM_SOURCE_PREM_ID IK_ID
          FROM ACCT_EO_CP CP
          join parameters p on 1 = 1
          INNER JOIN CM_PREM_LINK L ON L.CM_DEST_PREM_ID = CP.CP_ID AND L.CM_PREM_LINK_TYPE_CD='SUPPLY_POINT'
                AND p.date_end between L.START_DT AND NVL(L.END_DT,p.date_end)
          )
        /*Данные ПУ*/  
        , PU AS (
        SELECT 
         IK.ACCT_ID,
         IK.CP_ID,
         rtrim(xmlagg(xmlelement("a", 'Тип ПУ ' || trim(mtl.descr) || ' № ПУ ' || trim(m.serial_nbr) || ' K=' || trim(reg.reg_const) || CHR(13))).extract('/a/text()').getstringval(), CHR(13)) PU_DESCR                 
        FROM IK
        INNER JOIN CI_SP SP ON SP.PREM_ID = IK.IK_ID AND SP.SP_TYPE_CD='ENERGY'
        /*inner join ci_prem_char tu_num on tu_num.prem_id = IK.IK_ID and tu_num.char_type_cd = 'TU-NUM'
        and TU_NUM.EFFDT = (SELECT MAX(O.EFFDT) FROM CI_PREM_CHAR O WHERE O.PREM_ID = TU_NUM.PREM_ID AND O.CHAR_TYPE_CD = TU_NUM.CHAR_TYPE_CD )
        inner join ci_prem_char tu_name on tu_name.prem_id = IK.IK_ID and tu_name.char_type_cd = 'TU-NAME'
        and tu_name.EFFDT = (SELECT MAX(O.EFFDT) FROM CI_PREM_CHAR O WHERE O.PREM_ID = tu_name.PREM_ID AND O.CHAR_TYPE_CD = tu_name.CHAR_TYPE_CD )*/
        inner join ci_sp_mtr_hist mh on mh.sp_id = sp.sp_id and (mh.removal_dttm is null or mh.removal_dttm > trunc(sysdate))
        inner join ci_sp_mtr_evt evt on (evt.sp_mtr_hist_id = mh.sp_mtr_hist_id  and evt.sp_mtr_evt_flg = 'I')
        inner join ci_mr install_mr on (install_mr.mr_id = evt.mr_id  and install_mr.read_dttm <= trunc(sysdate))
        JOIN ci_mtr_config mc ON(mc.mtr_config_id = mh.mtr_config_id
                                  AND mc.eff_dttm = (SELECT MAX(mc2.eff_dttm)
                                                     FROM ci_mtr_config mc2
                                                     WHERE mc2.mtr_config_id = mh.mtr_config_id
                                                           AND mc2.eff_dttm <= trunc(sysdate)
                                                     )
                                  )
        JOIN ci_mtr m ON(m.mtr_id = mc.mtr_id)          
        inner join ci_model_l md on md.model_cd = m.model_cd and md.language_cd = 'RUS' and md.mfg_cd = m.mfg_cd
        inner join ci_mtr_type_l  mtl on mtl.mtr_type_cd = m.mtr_type_cd and mtl.language_cd = 'RUS'
        inner join ci_reg reg on reg.mtr_id = m.mtr_id and reg.eff_dttm >= mc.eff_dttm
        GROUP BY 
                 IK.ACCT_ID,
                 IK.CP_ID
        )
       
          /*Начисление за месяц*/
          , ACCT_CHARGE AS (
          select /*+ MATERIALIZE*/ 
                 A.ACCT_ID,
                 SUM(AE.CUR_AMT) SUM_CHARGE          
          from PARAMETERS PR 
          INNER JOIN DIST_ACCT A ON 1 = 1 
          inner join ci_case bill on bill.acct_id = A.acct_id and bill.case_type_cd = 'INVOICE'
          INNER JOIN CM_ACCOUNT_ENTRY AE ON AE.CM_FIN_DOCUMENT_ID=BILL.CASE_ID AND AE.SHOW_ON_BILL_SW='Y'
          LEFT  JOIN CM_ACCT_PAY_MATCH_EVT_PROP PAY_PROP ON PAY_PROP.CM_ACCT_PAY_MATCH_EVT_PROP_ID=AE.CM_ACCT_PAY_MATCH_EVT_PROP_ID
          INNER JOIN CM_RATE_PROP     RP ON RP.CM_RATE_PROP_ID = AE.CM_RATE_PROP_ID AND RP.CM_RETAIL_TYPE='2'
          INNER JOIN CI_FT            GFT ON GFT.FT_ID=AE.CM_GENERATIVE_FT_ID
          INNER JOIN CM_FT_EXP        FE ON FE.FT_ID=AE.CM_ACCT_REC_MATCH_EVT_ID
          WHERE 1 = 1  
            AND trunc(AE.Accounting_Dt, 'mm') = (select max(trunc(ae2.Accounting_Dt, 'mm')) from CM_ACCOUNT_ENTRY ae2, ci_case bill2
                                                  where bill2.acct_id = a.acct_id
                                                    and bill2.case_type_cd = 'INVOICE'
                                                    and ae2.cm_fin_document_id = bill2.case_id 
                                                    and ae2.show_on_bill_sw = 'Y' 
                                                    and ae2.Accounting_Dt < TRUNC(PR.DATE_BEGIN, 'MM')
                                                    and exists(select 1 from CM_RATE_PROP RP2, ci_ft ft2
                                                                where RP2.CM_RATE_PROP_ID = AE2.CM_RATE_PROP_ID 
                                                                  AND RP2.CM_RETAIL_TYPE = '2'
                                                                  and ft2.FT_ID = AE2.CM_GENERATIVE_FT_ID))
          GROUP BY A.ACCT_ID
          )
          , PRE_FINAL AS
          (
          SELECT --A.WF_PROC_ID
                 DISTINCT
                 DAT_LIM.ADHOC_CHAR_VAL DAT_LIM
                ,A.WF_STAT_FLG
                ,A.ACCT_ID
                ,A.DOG_NUM
                ,A.DOG_NAME                     
                ,A.EO_CODE  
                ,A.EO_ADRES             
                ,A.EO_MAX_P 
                ,A.CP_NAME
                ,A.CP_NUM                
                ,NVL(A.CP_PRIS_P, A.CP_MAX_P) CP_PRIS_P       
                ,A.CP_MAX_P   CP_MAX_P_PLAN             
                ,TO_NUMBER(NVL(REPLACE(NTF_AMT.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') NTF_AMT                 
                ,TO_NUMBER(NVL(REPLACE(EVT_EBNTF_NTF_AMT0.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') +      
                 TO_NUMBER(NVL(REPLACE(EVT_EBNTF_NTF_AMT2.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') SUM_SCHEDULE   
                ,NVL(ACH.SUM_CHARGE, 0)  SUM_CHARGE,
               DAT_LIM.CHAR_TYPE_CD  TYPE_LIMIT, 
               PU.PU_DESCR,
               /*DATLIMFF.ADHOC_CHAR_VAL DATLIMFF,
               DATLIMFS.ADHOC_CHAR_VAL DATLIMFS,  
               DATLIMFP.ADHOC_CHAR_VAL DATLIMFP,   
               DATLIMFT.ADHOC_CHAR_VAL DATLIMFT,
               DATLIMFA.ADHOC_CHAR_VAL DATLIMFA,*/
               /*CASE WHEN (DATLIMFS.ADHOC_CHAR_VAL IS NULL AND DATLIMFP.ADHOC_CHAR_VAL IS NULL AND DATLIMFT.ADHOC_CHAR_VAL IS NULL AND DATLIMFA.ADHOC_CHAR_VAL IS NULL)
                 THEN 'Норм'
                  WHEN (W.WF_STAT_FLG = '10' AND (DATLIMFS.ADHOC_CHAR_VAL IS NULL OR DATLIMFP.ADHOC_CHAR_VAL IS NULL OR DATLIMFT.ADHOC_CHAR_VAL IS NULL OR DATLIMFA.ADHOC_CHAR_VAL IS NULL))
                  THEN  'Огр'  
                  WHEN (W.WF_STAT_FLG = '10' AND DATLIMFF.ADHOC_CHAR_VAL  IS NOT NULL) THEN 'Откл'                                 
                END CUR_STATE */ 
                'Норм' CUR_STATE
          from ACCT_EO_CP A
          join PARAMETERS p on 1 = 1
          JOIN ci_wf_proc w ON w.wf_proc_id = a.wf_proc_id
          inner join ci_wf_proc_char DZ_DPRT on DZ_DPRT.wf_proc_id = w.wf_proc_id and DZ_DPRT.char_type_cd='DZ-DPRT'
           and (TRIM(P.DEPARTMENT) is null or TRIM(DZ_DPRT.CHAR_VAL_FK1) = TRIM(P.DEPARTMENT))
          inner join ci_wf_proc_char DAT_LIM on DAT_LIM.WF_PROC_ID = w.wf_proc_id and DAT_LIM.CHAR_TYPE_CD = 'DAT-LIM'
           --and DAT_LIM.CHAR_TYPE_CD in ('DAT-LIM', 'DAT-LIMA', 'DAT-LIMT', 'DAT-LIMP', 'DAT-LIMS')
           and trim(DAT_LIM.ADHOC_CHAR_VAL) <> 'UNDEF'
           and to_date(trim(DAT_LIM.ADHOC_CHAR_VAL),'dd.mm.yyyy') between P.DATE_BEGIN and P.DATE_END
          left join ACCT_CHARGE ACH ON ACH.ACCT_ID = A.ACCT_ID -- AND ACH.WF_PROC_ID = A.WF_PROC_ID
          left join PU ON PU.ACCT_ID = A.ACCT_ID AND PU.CP_ID = A.CP_ID
          ----
          left join ci_wf_proc_char NTF_AMT  on NTF_AMT.WF_PROC_ID = a.wf_proc_id_NOTIFICATION and NTF_AMT.CHAR_TYPE_CD = 'NTF-AMT'
          left join ci_wf_proc_char NTF_AMT0 on NTF_AMT0.WF_PROC_ID = a.wf_proc_id_NOTIFICATION and NTF_AMT0.CHAR_TYPE_CD = 'NTF-AMT0'
          left join ci_wf_proc_char NTF_AMT2 on NTF_AMT2.WF_PROC_ID = a.wf_proc_id_NOTIFICATION and NTF_AMT2.CHAR_TYPE_CD = 'NTF-AMT2'
          left join ci_wf_evt EVT_EBNTF on EVT_EBNTF.WF_PROC_ID = a.wf_proc_id_NOTIFICATION and EVT_EBNTF.WF_EVT_TYPE_CD IN ('EVT-EBNTF-03', 'EVT-EBNTF-05')
          left join ci_wf_evt_char EVT_EBNTF_NTF_AMT0 on EVT_EBNTF_NTF_AMT0.WF_PROC_ID = EVT_EBNTF.wf_proc_id and EVT_EBNTF_NTF_AMT0.EVT_SEQ = EVT_EBNTF.EVT_SEQ and EVT_EBNTF_NTF_AMT0.CHAR_TYPE_CD = 'NTF-AMT0'
          left join ci_wf_evt_char EVT_EBNTF_NTF_AMT2 on EVT_EBNTF_NTF_AMT2.WF_PROC_ID = EVT_EBNTF.wf_proc_id and EVT_EBNTF_NTF_AMT2.EVT_SEQ = EVT_EBNTF.EVT_SEQ and EVT_EBNTF_NTF_AMT2.CHAR_TYPE_CD = 'NTF-AMT2'
          -----
          /*left join ci_wf_proc_char DATLIMFF  on DATLIMFF.WF_PROC_ID = w.wf_proc_id and DATLIMFF.CHAR_TYPE_CD = 'DATLIMFF'     
          left join ci_wf_proc_char DATLIMFS  on DATLIMFS.WF_PROC_ID = w.wf_proc_id and DATLIMFS.CHAR_TYPE_CD = 'DATLIMFS'  
          left join ci_wf_proc_char DATLIMFP  on DATLIMFP.WF_PROC_ID = w.wf_proc_id and DATLIMFP.CHAR_TYPE_CD = 'DATLIMFP' 
          left join ci_wf_proc_char DATLIMFT  on DATLIMFT.WF_PROC_ID = w.wf_proc_id and DATLIMFT.CHAR_TYPE_CD = 'DATLIMFT'   
          left join ci_wf_proc_char DATLIMFA  on DATLIMFA.WF_PROC_ID = w.wf_proc_id and DATLIMFA.CHAR_TYPE_CD = 'DATLIMFA'*/                                      
          -----
          where 1 = 1
          )
          SELECT 
                  ROW_NUMBER() OVER(/*PARTITION BY F.DAT_LIM*/ ORDER BY TO_DATE(F.DAT_LIM,'DD.MM.YYYY'), F.DOG_NUM) RN,                 
                  F.DAT_LIM,
                  --F.WF_STAT_FLG,
                  F.ACCT_ID,
                  F.DOG_NUM,
                  F.DOG_NAME,
                  --F.EO_CODE,
                  --F.EO_ADRES,
                  --F.EO_MAX_P,
                 -- F.CP_NAME,
                 -- F.CP_NUM, 
                  SUM(TO_NUMBER(REPLACE(TRIM(F.CP_PRIS_P), '.', ','),'999999D999','NLS_NUMERIC_CHARACTERS='',.''')) CP_PRIS_P,               
                 -- F.CP_PRIS_P,
                 -- F.CP_MAX_P_PLAN,   
                  F.NTF_AMT,
                  F.SUM_SCHEDULE,
                  F.SUM_CHARGE,
                  F.TYPE_LIMIT,
                  --F.PU_DESCR,
                  /*F.DATLIMFF,
                  F.DATLIMFS,
                  F.DATLIMFP,
                  F.DATLIMFT,
                  F.DATLIMFA,*/
                  F.CUR_STATE,
--                  rtrim(xmlagg(xmlelement("a", TO_NUMBER(SUBSTR(F.EO_CODE,-4),'999999999999D999999') || '-' || F.CP_NUM || ' ' || F.CP_NAME || ' ' || F.EO_ADRES || CHR(13) || F.PU_DESCR || CHR(13) )).extract('/a/text()').getstringval(),  CHR(13)) LIMIT_OBJECTS                                                                                                 
                  wm_concat(TO_NUMBER(SUBSTR(F.EO_CODE,-4),'999999999999D999999') || '-' || F.CP_NUM || ' ' || F.CP_NAME || ' ' || F.EO_ADRES || CHR(13) || F.PU_DESCR || CHR(13)) LIMIT_OBJECTS
          FROM PRE_FINAL F
          GROUP BY 
                  F.DAT_LIM,
                  F.ACCT_ID,
                  F.DOG_NUM,
                  F.DOG_NAME, 
                  F.NTF_AMT,
                  F.SUM_SCHEDULE,
                  F.SUM_CHARGE,
                  F.TYPE_LIMIT,
                  /*F.DATLIMFF,
                  F.DATLIMFS,
                  F.DATLIMFP,
                  F.DATLIMFT,
                  F.DATLIMFA,*/
                  F.CUR_STATE             
     ) LOOP     
     
      V_RET.RN := I.RN;
      V_RET.DAT_LIM := I.DAT_LIM;
      V_RET.ACCT_ID := I.ACCT_ID;
      V_RET.DOG_NUM := I.DOG_NUM;
      V_RET.DOG_NAME := I.DOG_NAME;
      V_RET.EO_CODE := NULL/*I.EO_CODE*/;
      V_RET.EO_MAX_P := NULL/*I.EO_MAX_P*/;
      V_RET.CP_NAME := NULL/*I.CP_NAME*/;
      V_RET.CP_PRIS_P := I.CP_PRIS_P;
      V_RET.CP_MAX_P_PLAN := NULL/*I.CP_MAX_P_PLAN*/;
      V_RET.NTF_AMT := I.NTF_AMT;
      V_RET.SUM_SCHEDULE := I.SUM_SCHEDULE;
      V_RET.SUM_CHARGE := I.SUM_CHARGE;
      V_RET.OPER_LIM_ID := NULL/*I.OPER_LIM_ID*/;
      V_RET.OPER_LIM_NAME := NULL/*I.OPER_LIM_NAME*/;
      V_RET.NOTICE_DESCR := 'Уведомлен';
      V_RET.TYPE_LIMIT_1 := NULL;
      V_RET.POWER_LIMIT_1 := NULL;
      V_RET.TYPE_LIMIT_2 := NULL;
      V_RET.POWER_LIMIT_2 := NULL;
      V_RET.TYPE_LIMIT_3 := NULL;
      V_RET.POWER_LIMIT_3 := NULL;
      V_RET.TYPE_LIMIT_4 := NULL;
      V_RET.POWER_LIMIT_4 := NULL;
      V_RET.TYPE_LIMIT_5 := NULL;
      V_RET.POWER_LIMIT_5 := NULL;                        
            
      IF TO_CHAR(TO_DATE(I.DAT_LIM,'DD.MM.YYYY'),'D', 'NLS_DATE_LANGUAGE = RUSSIAN') = '2' THEN
        IF I.TYPE_LIMIT = 'DAT-LIM' THEN 
           V_RET.TYPE_LIMIT_1 := 'Полное';
        ELSE V_RET.TYPE_LIMIT_1 := 'Частичное';
          END IF;
        IF V_RET.TYPE_LIMIT_1 = 'Полное' THEN
           V_RET.POWER_LIMIT_1 := I.CP_PRIS_P;
          ELSE
           V_RET.POWER_LIMIT_1 := I.CP_PRIS_P/2;             
        END IF;
      END IF;
      IF TO_CHAR(TO_DATE(I.DAT_LIM,'DD.MM.YYYY'),'D', 'NLS_DATE_LANGUAGE = RUSSIAN') = '3' THEN
       IF I.TYPE_LIMIT = 'DAT-LIM' THEN 
           V_RET.TYPE_LIMIT_2 := 'Полное';
        ELSE V_RET.TYPE_LIMIT_2 := 'Частичное';
          END IF;
        IF V_RET.TYPE_LIMIT_2 = 'Полное' THEN
           V_RET.POWER_LIMIT_2 := I.CP_PRIS_P;
          ELSE
           V_RET.POWER_LIMIT_2 := I.CP_PRIS_P/2;             
        END IF;
      END IF;
      IF TO_CHAR(TO_DATE(I.DAT_LIM,'DD.MM.YYYY'),'D', 'NLS_DATE_LANGUAGE = RUSSIAN') = '4' THEN
       IF I.TYPE_LIMIT = 'DAT-LIM' THEN 
           V_RET.TYPE_LIMIT_3 := 'Полное';
        ELSE V_RET.TYPE_LIMIT_3 := 'Частичное';
          END IF;
        IF V_RET.TYPE_LIMIT_3 = 'Полное' THEN
           V_RET.POWER_LIMIT_3 := I.CP_PRIS_P;
          ELSE
           V_RET.POWER_LIMIT_3 := I.CP_PRIS_P/2;             
        END IF;
      END IF;
      IF TO_CHAR(TO_DATE(I.DAT_LIM,'DD.MM.YYYY'),'D', 'NLS_DATE_LANGUAGE = RUSSIAN') = '5' THEN
       IF I.TYPE_LIMIT = 'DAT-LIM' THEN 
           V_RET.TYPE_LIMIT_4 := 'Полное';
        ELSE V_RET.TYPE_LIMIT_4 := 'Частичное';
          END IF;
        IF V_RET.TYPE_LIMIT_4 = 'Полное' THEN
           V_RET.POWER_LIMIT_4 := I.CP_PRIS_P;
          ELSE
           V_RET.POWER_LIMIT_4 := I.CP_PRIS_P/2;             
        END IF;
      END IF;
      IF TO_CHAR(TO_DATE(I.DAT_LIM,'DD.MM.YYYY'),'D', 'NLS_DATE_LANGUAGE = RUSSIAN') = '6' THEN
       IF I.TYPE_LIMIT = 'DAT-LIM' THEN 
           V_RET.TYPE_LIMIT_5 := 'Полное';
        ELSE V_RET.TYPE_LIMIT_5 := 'Частичное';
          END IF;
        IF V_RET.TYPE_LIMIT_5 = 'Полное' THEN
           V_RET.POWER_LIMIT_5 := I.CP_PRIS_P;
          ELSE
           V_RET.POWER_LIMIT_5 := I.CP_PRIS_P/2;             
        END IF;
      END IF;  
      
      V_RET.CUR_STATE := I.CUR_STATE;  
      
/*      IF I.DATLIMFS IS NULL AND I.DATLIMFP IS NULL AND I.DATLIMFT IS NULL AND I.DATLIMFA IS NULL THEN
        V_RET.CUR_STATE :=  'Норм';
      ELSIF I.WF_STAT_FLG = '10' AND (I.DATLIMFS IS NULL OR I.DATLIMFP IS NULL OR I.DATLIMFT IS NULL OR I.DATLIMFA IS NULL) THEN
        V_RET.CUR_STATE :=  'Огр'; 
      ELSIF I.WF_STAT_FLG = '10' AND I.DATLIMFF IS NOT NULL THEN
        V_RET.CUR_STATE :=  'Откл';                     
      END IF;*/  
      
      V_RET.LIMIT_OBJECTS := SUBSTR(SUBSTR(REPLACE(I.LIMIT_OBJECTS,'&quot;','"'), 1, LENGTH(REPLACE(I.LIMIT_OBJECTS,'&quot;','"'))-1),1,4000); /*TO_NUMBER(SUBSTR(I.EO_CODE,-4),'999999999999D999999') || '-' || I.CP_NUM || ' ' || I.CP_NAME || ' ' || I.EO_ADRES || CHR(13) || I.PU_DESCR;*/              
    
      PIPE ROW(V_RET);              
     
   END LOOP;   

   END DZ_REQUEST_LIMITATION;

FUNCTION GET_SOGL_RESTR_HEAD (i_case_id in char) return T_SOGL_RESTR_HEAD pipelined is
  
  v_sogl_rec T_SOGL_RESTR_HEAD_REC;
  v_per_id_agrmnt ci_per.per_id%type;
  v_per_id_dog ci_per.per_id%type;
  v_per_id_grnt ci_per.per_id%type;    
    
  begin

  select 
       dc1_num.Adhoc_Char_Val AS doc_num,
       dc1_dat.adhoc_char_val as doc_dat,
       PCZ_EVT.CHAR_VAL_FK1 wf_proc_id,
       PCZ_EVT.CHAR_VAL_FK2 evt_seq                       
  into v_sogl_rec.doc_num, v_sogl_rec.doc_dat, v_sogl_rec.wf_proc_id, v_sogl_rec.evt_seq
  from ci_case dc1
  inner join ci_case_char dc1_num on dc1_num.case_id = dc1.case_id and dc1_num.char_type_cd = 'DOC-NUM'
  inner join ci_case_char dc1_dat on dc1_dat.case_id = dc1.case_id and dc1_dat.char_type_cd = 'DOC-DAT'
  inner join ci_case_char PCZ_EVT on PCZ_EVT.case_id = dc1.case_id and PCZ_EVT.char_type_cd = 'PCZ-EVT'  
  where dc1.case_id = i_case_id;
  
  -- Данные договора
  select a.acct_id
      ,(select max(ac.adhoc_char_val) keep (dense_rank last order by ac.effdt)
      from ci_acct_char ac
      where ac.acct_id = A.ACCT_ID
       and ac.char_type_cd = 'DOG-NUM') DOG_NUM
     ,coalesce(
     (select max(ac.adhoc_char_val) keep (dense_rank last order by ac.effdt)
      from ci_acct_char ac
      where ac.acct_id = A.ACCT_ID
       and ac.char_type_cd = 'DOG-BDAT')
     ,to_char(a.setup_dt, 'dd.mm.yyyy'))
     ,CASE WHEN A.Cis_Division = 'TSK1' THEN 'г. Томск'
           WHEN A.Cis_Division = 'OMS2' THEN 'г. Омск' 
           WHEN A.Cis_Division = 'ORL1' THEN 'г. Орел'                   
      END
     ,ap.per_id
     ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
        from ci_per_char pc
        where pc.per_id = ap.per_id
          and pc.char_type_cd = 'SHORT-NM')  DOG_NAME
     ,DOGOVOR.PER_ID 
     ,CASE WHEN A.Cis_Division = 'TSK1' THEN 'ПАО "Томскэнергосбыт"'
           WHEN A.Cis_Division = 'OMS2' THEN 'АО "Петербургская сбытовая компания"' 
           WHEN A.Cis_Division = 'ORL1' THEN 'ООО "Орловский энергосбыт"'                   
      END            
  into v_sogl_rec.acct_id, v_sogl_rec.DOG_NUM, v_sogl_rec.DOG_DAT, v_sogl_rec.city, v_sogl_rec.main_per_id, v_sogl_rec.DOG_NAME, v_per_id_dog, v_sogl_rec.GRNT_SHORT_NAME
  from Ci_Wf_Proc W
  inner join ci_wf_proc_char LIC_S4ET on LIC_S4ET.WF_PROC_ID = w.wf_proc_id and LIC_S4ET.CHAR_TYPE_CD = 'LIC-S4ET'
  inner join ci_acct a on a.acct_id = LIC_S4ET.CHAR_VAL_FK1
  inner join ci_acct_per ap on ap.acct_id = a.acct_id and ap.main_cust_sw = 'Y'
  inner join ci_acct_per DOGOVOR on DOGOVOR.acct_id = a.acct_id and DOGOVOR.ACCT_REL_TYPE_CD = 'DOGOVOR'
  where w.wf_proc_id = v_sogl_rec.wf_proc_id;

    -- Подписанты со стороны Потребителя
    begin
      select distinct DECODE(DL_RP.ADHOC_CHAR_VAL, NULL, NULL, lower(DL_RP.ADHOC_CHAR_VAL) || ' ') || FIO_RP.ADHOC_CHAR_VAL, 
             RIGHTDOC.ADHOC_CHAR_VAL,
             SHORT_NM.ADHOC_CHAR_VAL,
             NAIM_PLN.ADHOC_CHAR_VAL,
             FIO_RP.ADHOC_CHAR_VAL,
             NVL(TRIM(JOBD_SRC.ADHOC_CHAR_VAL), PRTL.DESCR12)      
      into v_sogl_rec.in_face, v_sogl_rec.acct_osnov, v_sogl_rec.NAME_SIGNATORY, v_sogl_rec.NAME_SIGNATORY_PLN, v_sogl_rec.NAME_SIGNATORY_RP, v_sogl_rec.STAFFPOS_SIGNATORY
      from CI_PER_PER PP
      INNER JOIN CI_PER_REL_TYPE_L PRTL ON PRTL.PER_REL_TYPE_CD = PP.PER_REL_TYPE_CD AND PRTL.LANGUAGE_CD = 'RUS'                
      inner join ci_per_char FIO_RP on FIO_RP.PER_ID = pp.per_id2 and FIO_RP.CHAR_TYPE_CD = rpad('FIO-RP',8) 
                 and FIO_RP.EFFDT = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = FIO_RP.PER_ID and pc2.char_type_cd = FIO_RP.CHAR_TYPE_CD)
      inner join ci_per_char RIGHTDOC on RIGHTDOC.PER_ID = pp.per_id2 and RIGHTDOC.CHAR_TYPE_CD = rpad('RIGHTDOC',8) 
                 and RIGHTDOC.EFFDT = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = RIGHTDOC.PER_ID and pc2.char_type_cd = RIGHTDOC.CHAR_TYPE_CD)                         
      /*У ИП должности может не быть*/
      left join ci_per_char DL_RP on DL_RP.PER_ID = pp.per_id2 and DL_RP.CHAR_TYPE_CD = rpad('DL$N-RP',8)
      left join ci_per_char JOBD_SRC on JOBD_SRC.PER_ID = pp.per_id2 and JOBD_SRC.CHAR_TYPE_CD = rpad('JOBD-SRC',8)      
      /*Для формирования подписанта в И. падеже в Доп. соглашениях*/
      left join ci_per_char SHORT_NM on SHORT_NM.PER_ID = pp.per_id2 and SHORT_NM.CHAR_TYPE_CD = rpad('SHORT-NM',8)
      left join ci_per_char NAIM_PLN on NAIM_PLN.PER_ID = pp.per_id2 and NAIM_PLN.CHAR_TYPE_CD = rpad('NAIM-PLN',8)                    
      where pp.per_id1 = v_sogl_rec.main_per_id
        --and v_dogstart between PP.START_DT and nvl(PP.END_DT, v_dogstart)
        and PP.END_DT IS NULL
        AND PP.PER_REL_TYPE_CD LIKE 'DL%'
        and (DL_RP.EFFDT IS NULL OR DL_RP.EFFDT = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = DL_RP.PER_ID and pc2.char_type_cd = DL_RP.CHAR_TYPE_CD ))
        and (JOBD_SRC.EFFDT IS NULL OR JOBD_SRC.EFFDT = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = JOBD_SRC.PER_ID and pc2.char_type_cd = JOBD_SRC.CHAR_TYPE_CD ))                
        and (SHORT_NM.EFFDT IS NULL OR SHORT_NM.EFFDT = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = SHORT_NM.PER_ID and pc2.char_type_cd = SHORT_NM.CHAR_TYPE_CD ))
        and (NAIM_PLN.EFFDT IS NULL OR NAIM_PLN.EFFDT = (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id = NAIM_PLN.PER_ID and pc2.char_type_cd = NAIM_PLN.CHAR_TYPE_CD ));
    exception when no_data_found then v_sogl_rec.in_face := null; v_sogl_rec.NAME_SIGNATORY := null; v_sogl_rec.NAME_SIGNATORY_RP:= null; v_sogl_rec.acct_osnov := null; v_sogl_rec.STAFFPOS_SIGNATORY := null;
              when too_many_rows then raise_application_error(-20001, 'Найдено несколько подписантов у Потребителя!');       
    end;   
    -- Подписанты со стороны ГП    
    begin
      select 
        -- Краткое Наименование подписанта
       (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
       from ci_per_char pc
       where pc.per_id = PRES_PSK.CHAR_VAL_FK1
         and pc.char_type_cd = 'SHORT-NM')
        -- Полное Наименование подписанта
       ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
       from ci_per_char pc
       where pc.per_id = PRES_PSK.CHAR_VAL_FK1
         and pc.char_type_cd = 'NAIM-PLN')
        -- Полное Наименование подписанта
       ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
       from ci_per_char pc
       where pc.per_id = PRES_PSK.CHAR_VAL_FK1
         and pc.char_type_cd = 'DL$N-RUK')         
      ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
       from ci_per_char pc
       where pc.per_id = PRES_PSK.CHAR_VAL_FK1
         and pc.char_type_cd = 'FIO-RP')
      ,'доверенности № ' ||
      (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
       from ci_per_char pc
       where pc.per_id = PRES_PSK.CHAR_VAL_FK1
         and pc.char_type_cd = 'DOVR-NUM') || ' от ' ||
      (select max(CM_TOMSK_PRIL.RusDate(to_date(pc.adhoc_char_val,'dd.mm.yyyy'),1)) keep (dense_rank last order by pc.effdt)
       from ci_per_char pc
       where pc.per_id = PRES_PSK.CHAR_VAL_FK1
         and pc.char_type_cd = 'DOVR-DAT') || ' года'                       
              
      into v_sogl_rec.NAME_AGRMNT, v_sogl_rec.NAME_AGRMNT_PLN, v_sogl_rec.STAFFPOS_AGRMNT, v_sogl_rec.RUK_FIO, v_sogl_rec.DOVER_NUM 
      from ci_wf_evt_char PRES_PSK
      where PRES_PSK.WF_PROC_ID = v_sogl_rec.wf_proc_id
        and PRES_PSK.CHAR_TYPE_CD = 'PRES-PSK' 
        and PRES_PSK.EVT_SEQ = v_sogl_rec.evt_seq;  
     exception when no_data_found then v_sogl_rec.NAME_AGRMNT := null; v_sogl_rec.NAME_AGRMNT_PLN := null;  v_sogl_rec.STAFFPOS_AGRMNT:=null;     
    end;    
    
   -- Cуммы
   begin
   select --TRUNC(TO_NUMBER(replace(TRIM(RSTR_SUM.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''')) RSTR_SUM,
          money_delimited(RSTR_SUM.ADHOC_CHAR_VAL, 'RUB') RSTR_SUM,
          money_delimited(RSTR_SUM.ADHOC_CHAR_VAL, 'KOP') RSTR_SUM_KOP,          
          TO_NUMBER(replace(TRIM(S_AMT2.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') S_AMT2,  
          TO_NUMBER(replace(TRIM(S_AMT0.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') S_AMT0,
          TO_NUMBER(replace(TRIM(S_AMT4.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') S_AMT4,
          TO_NUMBER(replace(TRIM(S_AMT7.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') S_AMT7,          
          /*trim(replace(to_char(TRUNC(TO_NUMBER(replace(TRIM(S_AMT2.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') + 
          TO_NUMBER(replace(TRIM(S_AMT0.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''')),'999,999,999,999'), ',', ' ')) SUM_MAIN, 
          
          trim(replace(to_char(TRUNC(TO_NUMBER(replace(TRIM(S_AMT4.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') +
          TO_NUMBER(replace(TRIM(S_AMT7.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''')),'999,999,999,999'), ',', ' ')) SUM_FINE,*/
          money_delimited(
          to_char(TO_NUMBER(replace(TRIM(S_AMT2.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') + 
          TO_NUMBER(replace(TRIM(S_AMT0.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', '''),'99999999990D99'), 'RUB') SUM_MAIN, 
          money_delimited(
          to_char(TO_NUMBER(replace(TRIM(S_AMT4.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') +
          TO_NUMBER(replace(TRIM(S_AMT7.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', '''),'99999999990D99'), 'RUB') SUM_FINE,          
                                                            
          number2word(TRUNC(TO_NUMBER(replace(TRIM(RSTR_SUM.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''')), 'integer') RSTR_SUM_INWORDS,
          number2word(TRUNC(TO_NUMBER(replace(TRIM(S_AMT2.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') + 
          TO_NUMBER(replace(TRIM(S_AMT0.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''')), 'integer') SUM_MAIN_INWORDS, 
          number2word(TRUNC(TO_NUMBER(replace(TRIM(S_AMT4.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') +
          TO_NUMBER(replace(TRIM(S_AMT7.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''')), 'integer') SUM_FINE_INWORDS,
          --substr('4000,32', instr('4000,32',',')+1,2)  
          money_delimited(
       to_char(TO_NUMBER(replace(TRIM(S_AMT4.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') +
          TO_NUMBER(replace(TRIM(S_AMT7.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', '''), '99999999990D99'), 'KOP') SUM_FINE_KOP, 
          money_delimited(
       to_char(TO_NUMBER(replace(TRIM(S_AMT2.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''') +
          TO_NUMBER(replace(TRIM(S_AMT0.ADHOC_CHAR_VAL),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', '''), '99999999990D99'), 'KOP') SUM_MAIN_KOP                            
   into  v_sogl_rec.RSTR_SUM, v_sogl_rec.RSTR_SUM_KOP, v_sogl_rec.S_AMT2, v_sogl_rec.S_AMT0, v_sogl_rec.S_AMT4, v_sogl_rec.S_AMT7, v_sogl_rec.SUM_MAIN, v_sogl_rec.SUM_FINE, v_sogl_rec.RSTR_SUM_INWORDS,
         v_sogl_rec.SUM_MAIN_INWORDS, v_sogl_rec.SUM_FINE_INWORDS, v_sogl_rec.SUM_FINE_KOP, v_sogl_rec.SUM_MAIN_KOP
   from ci_wf_evt we
   left join ci_wf_evt_char RSTR_SUM on RSTR_SUM.WF_PROC_ID = WE.WF_PROC_ID and RSTR_SUM.CHAR_TYPE_CD = 'RSTR-SUM' and RSTR_SUM.EVT_SEQ = we.evt_seq
   left join ci_wf_evt_char S_AMT2 on S_AMT2.WF_PROC_ID = WE.WF_PROC_ID and S_AMT2.CHAR_TYPE_CD = 'S-AMT2' and S_AMT2.EVT_SEQ = we.evt_seq
   left join ci_wf_evt_char S_AMT0 on S_AMT0.WF_PROC_ID = WE.WF_PROC_ID and S_AMT0.CHAR_TYPE_CD = 'S-AMT0' and S_AMT0.EVT_SEQ = we.evt_seq  
   left join ci_wf_evt_char S_AMT4 on S_AMT4.WF_PROC_ID = WE.WF_PROC_ID and S_AMT4.CHAR_TYPE_CD = 'S-AMT4' and S_AMT4.EVT_SEQ = we.evt_seq  
   left join ci_wf_evt_char S_AMT7 on S_AMT7.WF_PROC_ID = WE.WF_PROC_ID and S_AMT7.CHAR_TYPE_CD = 'S-AMT7' and S_AMT7.EVT_SEQ = we.evt_seq             
   where we.wf_proc_id = v_sogl_rec.wf_proc_id
     and we.wf_evt_type_cd = 'DZ_RESTR'
     and we.evt_seq = v_sogl_rec.evt_seq;
     exception when no_data_found then v_sogl_rec.RSTR_SUM := null;
    end;       
    
   -- Периоды задолженности
         
   begin
    SELECT TO_CHAR(MIN(TRUNC(CASE WHEN RETLTYPE.CHAR_VAL  = '0' THEN /*FE.CM_PREPAYMENT_DATE*/ to_date(fe.cm_prepayment_period_id||'-01','yyyy-mm-dd') ELSE /*FE.CM_CONSUMPTION_END_DATE*/ to_date(fe.cm_consumption_period_id||'-01','yyyy-mm-dd') END,'MM')),'DD.MM.YYYY'),
           TO_CHAR(MAX(LAST_DAY(CASE WHEN RETLTYPE.CHAR_VAL  = '0' THEN /*FE.CM_PREPAYMENT_DATE*/ to_date(fe.cm_prepayment_period_id||'-01','yyyy-mm-dd') ELSE /*FE.CM_CONSUMPTION_END_DATE*/ to_date(fe.cm_consumption_period_id||'-01','yyyy-mm-dd')END)),'DD.MM.YYYY')  
    INTO v_sogl_rec.DEBT_BEGIN,  v_sogl_rec.DEBT_END 
    FROM ci_wf_evt_char c1
    inner join ci_wf_evt_char c2 on c2.wf_proc_id = c1.wf_proc_id and c2.evt_seq = c1.evt_seq and c2.char_type_cd = 'RSTR-FIN'
    INNER JOIN CI_ADJ_CHAR AC ON AC.CHAR_TYPE_CD = 'FTR-ID' AND AC.CHAR_VAL_FK1 = C2.CHAR_VAL_FK1
    INNER JOIN CI_FT FT ON FT.FT_TYPE_FLG = 'AD' AND FT.SIBLING_ID = AC.ADJ_ID
    INNER JOIN CM_ACCOUNT_ENTRY AE ON AE.CM_GENERATIVE_FT_ID = FT.FT_ID    
    INNER JOIN CM_FT_EXP FE ON  FE.FT_ID=AE.CM_ACCT_REC_MATCH_EVT_ID
    inner join ci_case_char me_id on me_id.char_val_fk1 = ft.match_evt_id and me_id.char_type_cd = 'ME-ID'
    INNER JOIN CI_CASE_CHAR DR_TERMP ON DR_TERMP.CASE_ID = ME_ID.CASE_ID AND DR_TERMP.CHAR_TYPE_CD = 'DR-TERMP'      
    INNER JOIN CI_CASE_CHAR RETLTYPE ON RETLTYPE.CASE_ID = ME_ID.CASE_ID AND RETLTYPE.CHAR_TYPE_CD = 'RETLTYPE' AND RETLTYPE.CHAR_VAL IN ('2','0')         
    WHERE  c1.wf_proc_id = v_sogl_rec.wf_proc_id
     AND c1.evt_seq =  v_sogl_rec.evt_seq;
   exception when no_data_found then  v_sogl_rec.DEBT_BEGIN:=null;  v_sogl_rec.DEBT_END:=null;  
   end;  
              
      pipe row(v_sogl_rec);

  end GET_SOGL_RESTR_HEAD;   

  FUNCTION GET_GRAPHIC_RESTR_PAY (i_case_id in char) return T_GRAPHIC_RESTR_TAB pipelined is
    v_graphic_restr T_GRAPHIC_RESTR_REC;
    v_wf_proc_id char(10);
    v_num number(3);
  begin
    select c2.char_val_fk1 into v_wf_proc_id
    from ci_wf_evt_char c1
    inner join ci_wf_evt_char c2  on c1.wf_proc_id=c2.wf_proc_id and c1.evt_seq=c2.evt_seq and c2.char_type_cd='RSTR-GRF'
    where trim(c1.char_val_fk1)=trim(i_case_id);
    --
    v_num:= 1;
    for a1 in (select we.*
                from ci_wf_evt we
                where we.wf_proc_id = v_wf_proc_id 
                and we.wf_evt_type_cd = 'DZ-RSTR-PAY'
                order by we.trigger_dt)
    loop
      v_graphic_restr.wf_proc_id:= a1.wf_proc_id;
      v_graphic_restr.EVT_SEQ:= a1.evt_seq;
      v_graphic_restr.PLAT_NUM:= v_num;
      v_graphic_restr.PLAT_DAT:= cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-DAT');
      v_graphic_restr.PLAT_SUM_RUB:= money_delimited(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM'), 'RUB');
      v_graphic_restr.PLAT_SUM_KOP:= money_delimited(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM'), 'KOP');
      v_graphic_restr.PLAT_SUM := v_graphic_restr.PLAT_SUM_RUB || ' руб. ' || v_graphic_restr.PLAT_SUM_KOP || ' коп.';
      v_graphic_restr.PLAT_SUM_WORD := LOWER(CM_UTIL.NUMBER2WORD(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM')));
      --v_graphic_restr.PLAT_SUM_WORD := cm_lan_rep.number2word(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM'));
      --v_graphic_restr.PLAT_SUM_WORD := REPLACE(v_graphic_restr.PLAT_SUM_WORD , 'рублей' , 'руб.');
      --v_graphic_restr.PLAT_SUM_WORD := REPLACE(v_graphic_restr.PLAT_SUM_WORD , 'копеек' , 'коп.');
      pipe row(v_graphic_restr);
      v_num:= v_num+1;
    end loop;
  end GET_GRAPHIC_RESTR_PAY;  

  FUNCTION GET_GRAPHIC_RESTR_FINE (i_case_id in char) return T_GRAPHIC_RESTR_TAB pipelined is
    v_graphic_restr T_GRAPHIC_RESTR_REC;
    v_wf_proc_id char(10);
    v_num number(3);

  begin
    select c2.char_val_fk1 into v_wf_proc_id
    from ci_wf_evt_char c1
    inner join ci_wf_evt_char c2  on c1.wf_proc_id=c2.wf_proc_id and c1.evt_seq=c2.evt_seq and c2.char_type_cd='RSTR-GRF'
    where trim(c1.char_val_fk1)=trim(i_case_id);

    --
    v_num:= 1;
    for a1 in (select we.*
                from ci_wf_evt we
                where we.wf_proc_id = v_wf_proc_id 
                and we.wf_evt_type_cd = 'DZ-RSTR-PENY'
                order by we.trigger_dt)
    loop
      v_graphic_restr.wf_proc_id:= a1.wf_proc_id;
      v_graphic_restr.EVT_SEQ:= a1.evt_seq;
      v_graphic_restr.PLAT_NUM:= v_num;
      v_graphic_restr.PLAT_DAT:= cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-DAT');
      v_graphic_restr.PLAT_SUM_RUB:= money_delimited(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM'), 'RUB');
      v_graphic_restr.PLAT_SUM_KOP:= money_delimited(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM'), 'KOP');
      v_graphic_restr.PLAT_SUM := v_graphic_restr.PLAT_SUM_RUB || ' руб. ' || v_graphic_restr.PLAT_SUM_KOP || ' коп.';
      v_graphic_restr.PLAT_SUM_WORD := LOWER(CM_UTIL.NUMBER2WORD(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM')));
      --v_graphic_restr.PLAT_SUM_WORD := cm_lan_rep.number2word(cm_dz_chty.get_wfev_chty(a1.wf_proc_id, a1.evt_seq, 'PLAT-SUM'));
      --v_graphic_restr.PLAT_SUM_WORD := REPLACE(v_graphic_restr.PLAT_SUM_WORD , 'рублей' , 'руб.');
      --v_graphic_restr.PLAT_SUM_WORD := REPLACE(v_graphic_restr.PLAT_SUM_WORD , 'копеек' , 'коп.');
      pipe row(v_graphic_restr);
      v_num:= v_num+1;
    end loop;

  end GET_GRAPHIC_RESTR_FINE;  

 /*Функция возвращает 1 - было пять подряд неудачных дозвонов или 0 - если */  

  FUNCTION GET_COUNT_UNSUCCESSFULL_CALL (p_acct_id in ci_acct.acct_id%type) return number
    
  IS
  
  V_SUM NUMBER; 
  V_COUNT NUMBER; 
  RES NUMBER DEFAULT NULL; 
  
  BEGIN    

        with r as (
        select distinct a.cm_autocall_id, a.acct_id, a.create_dt
               ,dense_rank() over (order by a.create_dt desc) rn 
        from cm_autocall a
        inner join cm_autocall_result ar on ar.cm_autocall_id = a.cm_autocall_id
        where a.acct_id = p_acct_id
        order by a.create_dt desc
        )
        , b as (
        select trunc(ar.call_date),
               sum(case when ar.result in ('31', '32') then 1 else 0 end) sum_bad
        from r
        inner join cm_autocall_result ar on ar.cm_autocall_id = r.cm_autocall_id
        where rn <= 5
        group by trunc(ar.call_date)
        )
        select sum(sum_bad), count(*)
        into V_SUM, V_COUNT
        from b;
      
    
      if V_SUM = 0 and V_COUNT = 5 then
        RES := 0;
      else
        RES := 1;
      end if;
      
      return(NVL(RES,1));  
    
   END  GET_COUNT_UNSUCCESSFULL_CALL;  

 /*Функция возвращает дату последнего успешного обзвона  + 5 рабочих дней (или 7 календарных)*/
  FUNCTION GET_DAY_SUCCESSFULL_CALL (p_acct_id in ci_acct.acct_id%type) return date
    
  IS
  
    RES DATE; 
 
  BEGIN

    select max(trunc(ar.call_date)) + 7
    into RES
    from cm_autocall a
    inner join cm_autocall_result ar on ar.cm_autocall_id = a.cm_autocall_id
    where a.acct_id = p_acct_id
      and ar.result in ('31', '32');   
    
     RETURN(NVL(RES,TO_DATE('01.01.1990','DD.MM.YYYY')));  
     
    END GET_DAY_SUCCESSFULL_CALL;   

  FUNCTION GET_CALL_RES_FALSE5 (p_acct_id in ci_acct.acct_id%type, p_date_begin date default sysdate, p_date_end date default sysdate) return number
    IS
      V_SUM NUMBER; 
      V_COUNT NUMBER; 
      RES NUMBER DEFAULT NULL;   
    BEGIN
        with r as (
        select distinct a.cm_autocall_id, a.acct_id, a.create_dt
               ,dense_rank() over (order by a.create_dt desc) rn 
        from cm_autocall a
        inner join cm_autocall_result ar on ar.cm_autocall_id = a.cm_autocall_id
        where a.acct_id = p_acct_id
          and a.create_dt <= p_date_end
        order by a.create_dt desc
        )
        , b as (
        select trunc(ar.call_date),
               sum(case when ar.result in ('31', '32') then 1 else 0 end) sum_bad
        from r
        inner join cm_autocall_result ar on ar.cm_autocall_id = r.cm_autocall_id
        where rn <= 5
        group by trunc(ar.call_date)
        )
        select sum(sum_bad), count(*)
        into V_SUM, V_COUNT
        from b;
      
    
      if V_SUM = 0 and V_COUNT = 5 then
        RES := 0;
      else
        RES := 1;
      end if;
      
      return(NVL(RES,1));        
      END GET_CALL_RES_FALSE5; 

  FUNCTION GET_PERSON_TOMSK (P_ACCT_ID CI_ACCT.ACCT_ID%TYPE, P_FLAG varchar2, P_DATE date default sysdate) return varchar2
  IS
  V_RES VARCHAR2(256);
  BEGIN
    
  SELECT 
   CASE WHEN TRIM(A.ACCESS_GRP_CD) IN ('TSK1-ADUL','TSK1-UPR') THEN
          CASE WHEN P_FLAG = 'FIO' THEN 'Петренко Т.И.' 
            ELSE 'Начальник управления по работе с юридическими лицами в городе' END
         WHEN TRIM(A.ACCESS_GRP_CD) IN ('TSK1-COUL', 'TSK1-NORTH') THEN
          CASE WHEN P_FLAG = 'FIO' THEN 'Тарасов А.Г.' 
            ELSE 'Начальник управления по работе с клиентами центральных районов области' END             
         WHEN TRIM(A.ACCESS_GRP_CD) IN ('TSK1-VOUL') THEN
          CASE WHEN P_FLAG = 'FIO' THEN 'Терпелова Л.П.' 
            ELSE 'Начальник Восточного отделения' END 
         WHEN TRIM(A.ACCESS_GRP_CD) IN ('TSK1-SOUL','TSK1-SSUL') THEN
          CASE WHEN P_FLAG = 'FIO' THEN 'Бурячкова Г.М.' 
            ELSE 'Начальник Северного отделения' END 
    END                                
           
    INTO V_RES
    FROM CI_ACCT A
    WHERE A.ACCT_ID = P_ACCT_ID;
  
    RETURN(V_RES);
    END GET_PERSON_TOMSK;
    
  FUNCTION GET_PERSON_TOMSK_4CLM (P_ACCT_ID CI_ACCT.ACCT_ID%TYPE, P_FLAG varchar2, P_DATE date default sysdate) return varchar2
  IS
  V_RES VARCHAR2(256);
  BEGIN
    
  SELECT 
    CASE WHEN TRIM(A.ACCESS_GRP_CD) IN ('TSK1-ADUL','TSK1-UPR','TSK1-COUL','TSK1-NORTH') THEN
          CASE WHEN P_FLAG = 'FIO' THEN 'Рудьман М.М.' 
            ELSE 'Представитель ПАО "Томскэнергосбыт"' END         
         WHEN TRIM(A.ACCESS_GRP_CD) IN ('TSK1-SOUL') THEN
          CASE WHEN P_FLAG = 'FIO' THEN 'Бурячкова Г.М.' 
            ELSE 'Представитель ПАО "Томскэнергосбыт"' END 
         WHEN TRIM(A.ACCESS_GRP_CD) IN ('TSK1-SSUL') THEN
          CASE WHEN P_FLAG = 'FIO' THEN 'Золотарев А.А.' 
            ELSE 'Представитель ПАО "Томскэнергосбыт"' END
         WHEN TRIM(A.ACCESS_GRP_CD) IN ('TSK1-VOUL') THEN
          CASE WHEN P_FLAG = 'FIO' THEN 'Терпелова Л.П.' 
            ELSE 'Представитель ПАО "Томскэнергосбыт"' END
    END                               
           
    INTO V_RES
    FROM CI_ACCT A
    WHERE A.ACCT_ID = P_ACCT_ID;
  
    RETURN(V_RES);
    END GET_PERSON_TOMSK_4CLM; 

  FUNCTION GET_PERSON_OMSK (P_WF_ROC_ID CI_WF_PROC.WF_PROC_ID%TYPE, P_FLAG varchar2, P_DATE date DEFAULT sysdate) return varchar2

  IS
  V_RES VARCHAR2(256);
  BEGIN
   
  IF  P_FLAG = 'NOTIF' THEN
  SELECT 
   CASE WHEN TRIM(WP.CHAR_VAL_FK1) IN 'OMS-SEVR' THEN 'Юрченко В.В.'
        WHEN TRIM(WP.CHAR_VAL_FK1) IN 'OMS-USHN' THEN 'Лясковец А.Н.'
        WHEN TRIM(WP.CHAR_VAL_FK1) IN 'OMS-VOST' THEN 'Грушевский Ю.В.'    
        WHEN TRIM(WP.CHAR_VAL_FK1) IN 'OMS-ZAPD' THEN 'Вострухин А.В.'                              
            ELSE 'Рудник А.В.' END 
    END                                
           
    INTO V_RES
    FROM CI_WF_PROC_CHAR WP
    WHERE WP.WF_PROC_ID = P_WF_ROC_ID
      AND WP.CHAR_TYPE_CD = 'DZ-DPRT';
  
  END IF;

   
  IF  P_FLAG = 'LIMIT' THEN
  SELECT 
   CASE WHEN TRIM(WP.CHAR_VAL_FK1) IN 'OMS-SEVR' THEN 'Юрченко В.В.'
        WHEN TRIM(WP.CHAR_VAL_FK1) IN 'OMS-USHN' THEN 'Лясковец А.Н.'
        WHEN TRIM(WP.CHAR_VAL_FK1) IN 'OMS-VOST' THEN 'Грушевский Ю.В.'    
        WHEN TRIM(WP.CHAR_VAL_FK1) IN 'OMS-ZAPD' THEN 'Вострухин А.В.'                              
            ELSE 'Катрич П.А.' END 
    END                                
           
    INTO V_RES
    FROM CI_WF_PROC_CHAR WP
    WHERE WP.WF_PROC_ID = P_WF_ROC_ID
      AND WP.CHAR_TYPE_CD = 'DZ-DPRT';
  
  END IF;  
  
    RETURN(V_RES);
    END GET_PERSON_OMSK;      
        

  FUNCTION GET_HALF_LIM_CP_BY_EO (P_EO_ID CI_PREM.PREM_ID%TYPE) RETURN NUMBER
    IS
    V_RES NUMBER;
    
    BEGIN     

    SELECT COUNT(*)
    INTO V_RES
      FROM
        (SELECT CI_P.PREM_ID
        FROM CM_PREM_LINK PL
        INNER JOIN CI_PREM CI_P
        ON CI_P.PREM_ID                 = PL.CM_SOURCE_PREM_ID
        AND CI_P.PREM_TYPE_CD           = 'CP'
        WHERE PL.CM_PREM_LINK_TYPE_CD   = 'EE_SUPPLY'
        AND PL.START_DT                <= TRUNC(SYSDATE)
        AND (PL.END_DT                 IS NULL
        OR PL.END_DT                   >= TRUNC(SYSDATE))
          START WITH PL.CM_DEST_PREM_ID = P_EO_ID
        AND PL.START_DT                        <= TRUNC(SYSDATE)
        AND (PL.END_DT                         IS NULL
        OR PL.END_DT                           >= TRUNC(SYSDATE))
        AND PL.CM_PREM_LINK_TYPE_CD             = 'EE_SUPPLY'
          CONNECT BY PRIOR PL.CM_SOURCE_PREM_ID = PL.CM_DEST_PREM_ID
        AND PL.CM_PREM_LINK_TYPE_CD             = 'EE_SUPPLY'
        AND TRUNC(SYSDATE) BETWEEN PL.START_DT AND NVL(PL.END_DT, TRUNC(SYSDATE))
        ) PREM
      JOIN CI_PREM_CHAR PC
      ON PREM.PREM_ID    = PC.PREM_ID
      AND PC.CHAR_TYPE_CD='HALF-LIM'
      AND PC.EFFDT       =
        (SELECT MAX(PC2.EFFDT)
        FROM CI_PREM_CHAR PC2
        WHERE PC2.PREM_ID    = PC.PREM_ID
        AND PC2.CHAR_TYPE_CD = 'HALF-LIM'
        AND PC2.EFFDT       <= SYSDATE
        )
      AND PC.CHAR_VAL ='Y';
      
    RETURN(V_RES);
    END; 

  /*Функция возвращает кол-во объектов ЭО или ТП по договору*/
  FUNCTION GET_COUNT_OBJECT_BY_ACCT (P_ACCT_ID CI_ACCT.ACCT_ID%TYPE, P_FLAG VARCHAR2, P_DATE DATE default SYSDATE) return number
    IS
    V_RES NUMBER;    
    BEGIN
      
      IF P_FLAG = 'EO' THEN
        
        SELECT COUNT(*)
        INTO V_RES  
        FROM CM_EO_ACCOUNT_LINKAGE EAL
        WHERE EAL.ACCT_ID = P_ACCT_ID
          AND P_DATE BETWEEN EAL.START_DT AND NVL(EAL.END_DT, P_DATE);
          
      END IF;
      
      IF P_FLAG = 'CP' THEN
        
          WITH EO AS(
          SELECT EAL.CM_EO_ID EO_ID
          FROM CM_EO_ACCOUNT_LINKAGE EAL
          WHERE EAL.ACCT_ID = P_ACCT_ID
            AND P_DATE BETWEEN EAL.START_DT AND NVL(EAL.END_DT, P_DATE)
            )  
          , PARENT_CP AS (
          SELECT
            EO.EO_ID
           ,L.CM_SOURCE_PREM_ID SUPPLY_POINT_ID
          FROM EO
          INNER JOIN CM_PREM_LINK L ON L.CM_DEST_PREM_ID = EO.EO_ID
          INNER JOIN CI_PREM P ON P.PREM_ID = L.CM_SOURCE_PREM_ID AND P.PREM_TYPE_CD='CP'
          WHERE (L.START_DT <=P_DATE AND (L.END_DT IS NULL OR L.END_DT >= P_DATE))
            ) 
        , ALL_CP AS (
        SELECT 
            EO_ID
           ,SUPPLY_POINT_ID
        FROM PARENT_CP
        UNION ALL
        SELECT
            EO_ID
           ,L.CM_SOURCE_PREM_ID SUPPLY_POINT_ID
        FROM PARENT_CP
        INNER JOIN CM_PREM_LINK L ON L.CM_DEST_PREM_ID = PARENT_CP.SUPPLY_POINT_ID
        INNER JOIN CI_PREM P ON P.PREM_ID = L.CM_SOURCE_PREM_ID AND P.PREM_TYPE_CD='CP'
        WHERE (L.START_DT <=P_DATE AND (L.END_DT IS NULL OR L.END_DT >= P_DATE))
            )  
       SELECT COUNT(DISTINCT SUPPLY_POINT_ID)
       INTO V_RES        
       FROM  ALL_CP;                             

      END IF;  
   
    RETURN(V_RES);      
    END GET_COUNT_OBJECT_BY_ACCT;        

  FUNCTION GET_COUNT_CP_BY_MAIN_PROC (P_PROC_ID CI_WF_PROC.WF_PROC_ID%TYPE, P_DATE DATE default SYSDATE) return number
    IS
    V_RES NUMBER;       
    BEGIN
      
    SELECT COUNT(DISTINCT POINTDLV.CHAR_VAL_FK1)
    INTO V_RES
    FROM CI_WF_PROC PROC_NOTIF
    INNER JOIN CI_WF_PROC RPOC_LIM ON RPOC_LIM.CR_BY_WF_PROC_ID = PROC_NOTIF.WF_PROC_ID AND RPOC_LIM.WF_STAT_FLG = '10'
    INNER JOIN CI_WF_PROC_CHAR POINTDLV ON POINTDLV.WF_PROC_ID = RPOC_LIM.WF_PROC_ID AND POINTDLV.CHAR_TYPE_CD = 'POINTDLV'
    WHERE PROC_NOTIF.CR_BY_WF_PROC_ID = P_PROC_ID;                                  

    RETURN(V_RES);            
    END GET_COUNT_CP_BY_MAIN_PROC;    

  /*Функция возвращает величину аварийной или технологической брони по всем ЭО договора или по конкретному ЭО*/
  /*
  P_RESERV принимает значения AB или TB
  P_FLAG принимает значения ACCT или EO
  */
  FUNCTION GET_ROWER_RESERV (P_RESERV CHAR, P_ACCT_ID CI_ACCT.ACCT_ID%TYPE, P_EO_ID CI_PREM.PREM_ID%TYPE DEFAULT NULL, P_DATE DATE DEFAULT SYSDATE) return number
  IS
    V_RES cm_reservation_tot_value.cm_power_value%type;     
  BEGIN
    
    IF P_RESERV = 'AB' THEN
        
        SELECT SUM(per.cm_power_value)
         --sum(to_number(coalesce(replace(per.cm_power_value, ',', '.'), '0.000'), '9999999990.000'))
        INTO V_RES  
        FROM CM_EO_ACCOUNT_LINKAGE EAL
        INNER JOIN CI_CASE C ON C.PREM_ID = EAl.CM_EO_ID AND C.CASE_TYPE_CD = 'BR-AKT'
        INNER JOIN cm_reservation br on br.cm_reservation_document_id = c.case_id and br.cm_reservation_type_cd = 'EMERGENCY'
        INNER JOIN cm_reservation_tot_value per on per.cm_reservation_id = br.cm_reservation_id           
        WHERE EAL.ACCT_ID = P_ACCT_ID
          AND EAl.CM_EO_ID = NVL(P_EO_ID, EAL.CM_EO_ID)
          AND P_DATE BETWEEN EAL.START_DT AND NVL(EAL.END_DT, P_DATE)
          AND P_DATE BETWEEN to_date(cm_char_utils.get_entity_bigcharval('CASE', c.case_id, 'DOC-DAT'), 'DD.MM.YYYY') 
               AND P_DATE
              /*AND coalesce(to_date(cm_char_utils.get_entity_bigcharval('CASE', c.case_id, 'DAT-END'), 'DD.MM.YYYY'), P_DATE)*/;                
      
      END IF;

    IF P_RESERV = 'TB' THEN
        
        SELECT SUM(per.cm_power_value)
        --sum(to_number(coalesce(replace(per.cm_power_value, ',', '.'), '0.000'), '9999999990.000'))
        INTO V_RES  
        FROM CM_EO_ACCOUNT_LINKAGE EAL
        INNER JOIN CI_CASE C ON C.PREM_ID = EAl.CM_EO_ID AND C.CASE_TYPE_CD = 'BR-AKT'
        INNER JOIN cm_reservation br on br.cm_reservation_document_id = c.case_id and br.cm_reservation_type_cd = 'TECHNOLOGICAL'
        INNER JOIN cm_reservation_tot_value per on per.cm_reservation_id = br.cm_reservation_id           
        WHERE EAL.ACCT_ID = P_ACCT_ID
          AND EAl.CM_EO_ID = NVL(P_EO_ID, EAL.CM_EO_ID)
          AND P_DATE BETWEEN EAL.START_DT AND NVL(EAL.END_DT, P_DATE)
          AND P_DATE BETWEEN to_date(cm_char_utils.get_entity_bigcharval('CASE', c.case_id, 'DOC-DAT'), 'DD.MM.YYYY')
               AND P_DATE           
              /*AND coalesce(to_date(cm_char_utils.get_entity_bigcharval('CASE', c.case_id, 'DAT-END'), 'DD.MM.YYYY'), P_DATE)*/;                
      
      END IF;
      
    RETURN(V_RES);       
    END GET_ROWER_RESERV;   

  /*Функция возвращает кол-во ЭО по договору c определенной характестикой*/
  FUNCTION GET_COUNT_EO_CHAR_BY_ACCT (P_ACCT_ID CI_ACCT.ACCT_ID%TYPE, P_EO_ID CI_PREM.PREM_ID%TYPE DEFAULT NULL, P_CHAR_TYPE_CD VARCHAR2, P_DATE DATE DEFAULT SYSDATE) return number
  IS
    V_RES NUMBER;    
  BEGIN

        SELECT COUNT(DISTINCT EAL.CM_EO_ID)
        INTO V_RES  
        FROM CM_EO_ACCOUNT_LINKAGE EAL
        INNER JOIN CI_PREM_CHAR PC ON PC.PREM_ID = EAL.CM_EO_ID AND PC.CHAR_TYPE_CD = RPAD(TRIM(P_CHAR_TYPE_CD),8) AND PC.CHAR_VAL = RPAD('Y',16)
        WHERE EAL.ACCT_ID = P_ACCT_ID
          AND EAl.CM_EO_ID = NVL(P_EO_ID, EAL.CM_EO_ID)
          AND PC.EFFDT = (SELECT MAX(PC2.EFFDT) FROM CI_PREM_CHAR PC2 WHERE PC2.PREM_ID = PC.PREM_ID AND PC2.CHAR_TYPE_CD = PC.CHAR_TYPE_CD AND PC2.EFFDT <= P_DATE); 
  
    RETURN(V_RES);     
    END  GET_COUNT_EO_CHAR_BY_ACCT;

  FUNCTION GET_INFO_BY_IK_TRANS (P_IK_TRANS CI_PREM.PREM_ID%TYPE, P_FLAG VARCHAR2, P_DATE DATE DEFAULT SYSDATE) return VARCHAR2
   IS 
       V_RES VARCHAR2(256);   
       V_EO_ID_TRANS CI_PREM.PREM_ID%TYPE;
   BEGIN     
                    
      -- Определим ЭО транзитной ТУ
      begin
      select distinct p_cp.prem_id
      into V_EO_ID_TRANS
      from cm_prem_link pl, ci_prem p_cp
      where 1 = 1
        and pl.cm_prem_link_type_cd = 'EE_SUPPLY'
        and P_DATE between pl.start_dt and nvl(pl.end_dt, P_DATE)
        and p_cp.prem_id = pl.cm_dest_prem_id
        and p_cp.prem_type_cd = 'EO'    
      connect by prior pl.cm_dest_prem_id = pl.cm_source_prem_id and pl.cm_prem_link_type_cd = 'EE_SUPPLY'
      start with pl.cm_source_prem_id = P_IK_TRANS;
      exception when others then V_EO_ID_TRANS := null;   
      end;  

    begin
      select 
       CASE WHEN P_FLAG = 'EO_NAME' THEN 
          (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
          from ci_prem_char pc
          where pc.char_type_cd = 'PREM-NAM'
            and pc.prem_id = V_EO_ID_TRANS
            and pc.effdt <= P_DATE)  
            
            WHEN P_FLAG = 'DOG_NUM' THEN                         
            (select max(trim(ac.adhoc_char_val)) keep (dense_rank last order by ac.effdt)
              from ci_acct_char ac
              where ac.acct_id = eal.acct_id
                and ac.char_type_cd = 'DOG-NUM'
                and ac.effdt <= P_DATE)

            WHEN P_FLAG = 'DOG_NAME' THEN                                                 
            (select max(trim(pc.adhoc_char_val)) keep (dense_rank last order by pc.effdt)
              from ci_per_char pc
              where pc.per_id = ap.per_id
                and pc.char_type_cd = 'SHORT-NM'
                and pc.effdt <= P_DATE)
            END                                                               
      into V_RES
      from cm_eo_account_linkage eal
      inner join ci_acct_per ap on ap.acct_id = eal.acct_id and ap.main_cust_sw = 'Y'
      where eal.cm_eo_id = V_EO_ID_TRANS
        and P_DATE between eal.start_dt and nvl(eal.end_dt, P_DATE);
    exception when no_data_found then V_RES := null;      
    end;       
   
    RETURN(V_RES);        
    END;     
             
/*Список неплательщиков*/
  FUNCTION DZ_LIST_OF_DEFAULTERS(P_DEPARTMENT VARCHAR2 DEFAULT NULL, P_DOG_NUM VARCHAR2 DEFAULT NULL, P_DOG_NAME  VARCHAR2 DEFAULT NULL, P_ACCT_STAT VARCHAR2 DEFAULT NULL, P_OESK VARCHAR2 DEFAULT NULL,
                                 P_RETAIL_TYPE VARCHAR2 DEFAULT NULL, P_MAX_AMT NUMBER DEFAULT NULL, P_MIN_AMT NUMBER DEFAULT NULL, P_USER VARCHAR2, P_SHOW_TRAN CHAR DEFAULT 'N')
   RETURN T_LIST_OF_DEFAULTERS_SET PIPELINED
   IS
   V_RET  T_LIST_OF_DEFAULTERS;   
   BEGIN
     NULL;

   FOR I IN (
        WITH PARAMETERS AS (
          SELECT 
          /*+MATERIALIZE*/
           TRIM(P_DEPARTMENT)   DEPARTMENT,
           P_DOG_NUM            DOG_NUM,
           P_DOG_NAME           DOG_NAME,
           TRIM(P_ACCT_STAT)    ACCT_STAT,
           TRIM(P_OESK)         OESK,
           TRIM(P_RETAIL_TYPE)  RETAIL_TYPE,
           P_MIN_AMT            MIN_AMT,
           P_MAX_AMT            MAX_AMT,          
           RPAD(TRIM(P_USER),8) USER_ID,
           NVL(P_SHOW_TRAN,'N') SHOW_TRAN
          FROM DUAL
          )
         , ACCT_LITE AS(
        SELECT /*+ MATERIALIZE */
          A.ACCT_ID, 
          DZ_MAIN.WF_PROC_ID, 
          DZ_MAIN.CRE_DTTM WF_DAT
        FROM PARAMETERS PR
        INNER JOIN CM_USER_EXP UE ON UE.USER_ID = PR.USER_ID
        INNER JOIN CI_ACCT A ON A.CIS_DIVISION =UE.CIS_DIVISION
        INNER JOIN CI_WF_PROC_CHAR LIC_S4ET ON LIC_S4ET.CHAR_TYPE_CD = 'LIC-S4ET' AND LIC_S4ET.CHAR_VAL_FK1 = A.ACCT_ID
        INNER JOIN CI_WF_PROC DZ_MAIN ON DZ_MAIN.WF_PROC_ID = LIC_S4ET.WF_PROC_ID AND DZ_MAIN.WF_PROC_TMPL_CD = 'DZ-MAIN' AND DZ_MAIN.WF_STAT_FLG = '10'
        INNER JOIN CI_WF_PROC_CHAR STATUS ON STATUS.WF_PROC_ID = DZ_MAIN.WF_PROC_ID AND STATUS.CHAR_TYPE_CD = 'STATUS' AND STATUS.CHAR_VAL IN (RPAD('1',16),RPAD('2',16))
        INNER JOIN CI_WF_PROC_CHAR DZ_DPRT ON DZ_DPRT.WF_PROC_ID = DZ_MAIN.WF_PROC_ID AND DZ_DPRT.CHAR_TYPE_CD = 'DZ-DPRT'
        WHERE 1 = 1 
           AND TRIM(DZ_DPRT.CHAR_VAL_FK1) = PR.DEPARTMENT
           AND NOT Exists
           (SELECT 'X' FROM CI_WF_EVT WE WHERE WE.WF_PROC_ID = DZ_MAIN.WF_PROC_ID AND WE.WF_EVT_TYPE_CD = 'DZ-SELTMPL' AND WE.WF_EVT_STAT_FLG IN ('10', '20'))
          )        
        , ACCT AS(  
        SELECT  
           A.ACCT_ID
          ,A.WF_PROC_ID
          ,A.WF_DAT
          ,DOG_NUM.adhoc_char_val DOG_NUM 
          ,DOG_NAME.ADHOC_CHAR_VAL DOG_NAME
          ,astn.descr DOG_STAT
          ,oeskn.descr DOG_KLS_OESK 
          ,(select max(cp.WF_PROC_ID) keep (dense_rank last order by cp.CRE_DTTM) 
                from CI_WF_PROC cp 
                inner join CM_WF_PROC_TMPL_EXP pt on pt.WF_PROC_TMPL_CD = cp.WF_PROC_TMPL_CD 
                where pt.CM_WF_PROC_TYPE_CD = 'NOTIFICATION' or pt.CM_WF_PROC_TYPE_CD = 'LIMITATION' 
                connect by prior cp.WF_PROC_ID = cp.CR_BY_WF_PROC_ID start with cp.CR_BY_WF_PROC_ID = A.WF_PROC_ID
            ) as LAST_PROC_ID
          ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
            from ci_per_char pc
            where pc.per_id = PER_EXECUT_C.per_id
              and pc.char_type_cd = 'SHORT-NM') NAME_EXECUT_C
          ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
            from ci_per_char pc
            where pc.per_id = ESTIMATR.per_id
              and pc.char_type_cd = 'SHORT-NM') NAME_ESTIMATR              
          ,to_number(coalesce(replace(srch_amt.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') srch_amt
          ,GRUPEIn.descr GRUPEI
          ,(select per_u.ENTITY_NAME
            from ci_per_name per_u
            where per_u.per_id = U4ASTOK.char_val_fk1
             and per_u.NAME_TYPE_FLG = 'PRIM') U4ASTOK   
        ,SW_EMAIL.CHAR_VAL SW_EMAIL
        ,SW_SMS.CHAR_VAL SW_SMS
        ,SW_SITE.CHAR_VAL SW_SITE
        ,SW_BILL.CHAR_VAL SW_BILL
        ,SW_SMI.CHAR_VAL SW_SMI
        ,SW_POST.CHAR_VAL SW_POST
        ,SW_COUR.CHAR_VAL SW_COUR                     
        FROM PARAMETERS PR
        INNER JOIN ACCT_LITE A ON 1 = 1
        INNER JOIN ci_acct_per MAIN_PER on MAIN_PER.acct_id=A.acct_id and MAIN_PER.main_cust_sw='Y' 
        INNER JOIN ci_per_char DOG_NAME on DOG_NAME.per_id = MAIN_PER.per_id and DOG_NAME.char_type_cd='SHORT-NM' 
         and DOG_NAME.effdt= (select max(pc2.effdt) from ci_per_char pc2 where pc2.per_id=DOG_NAME.per_id and pc2.char_type_cd=DOG_NAME.char_type_cd)    
         and (PR.DOG_NAME is null or instr(DOG_NAME.adhoc_char_val, PR.DOG_NAME) = 1)         

         INNER JOIN ci_acct_char DOG_NUM on DOG_NUM.acct_id = a.acct_id and DOG_NUM.char_type_cd='DOG-NUM' 
         and DOG_NUM.effdt=(select max(ac2.effdt) from ci_acct_char ac2 where ac2.acct_id=DOG_NUM.acct_id and ac2.char_type_cd=DOG_NUM.char_type_cd)            
         and (PR.DOG_NUM is null or DOG_NUM.adhoc_char_val = PR.DOG_NUM)

         INNER JOIN ci_acct_char ast on ast.acct_id = a.acct_id and ast.char_type_cd='ACCTSTAT' and ast.effdt=
            (select max(ac2.effdt) from ci_acct_char ac2 where ac2.acct_id=ast.acct_id and ac2.char_type_cd=ast.char_type_cd)
         INNER JOIN ci_char_val_l astn on astn.char_type_cd=ast.char_type_cd and astn.char_val=ast.char_val and astn.language_cd='RUS'
            and (PR.ACCT_STAT is null or ast.char_val = RPAD(PR.ACCT_STAT, 16))

         INNER JOIN ci_acct_char oesk on oesk.acct_id = a.acct_id and oesk.char_type_cd='KLS-OESK' and oesk.effdt=
            (select max(ac2.effdt) from ci_acct_char ac2 where ac2.acct_id=oesk.acct_id and ac2.char_type_cd=oesk.char_type_cd)
         INNER JOIN ci_char_val_l oeskn on oeskn.char_type_cd=oesk.char_type_cd and oeskn.char_val=oesk.char_val and oeskn.language_cd='RUS'
            and (PR.OESK IS NULL OR oesk.char_val = RPAD(PR.OESK, 16))

          LEFT JOIN CI_ACCT_PER PER_EXECUT_C ON PER_EXECUT_C.ACCT_ID = a.acct_id AND PER_EXECUT_C.ACCT_REL_TYPE_CD = 'EXECUT-C'
          LEFT JOIN CI_ACCT_PER ESTIMATR ON ESTIMATR.ACCT_ID = a.acct_id AND ESTIMATR.ACCT_REL_TYPE_CD = 'ESTIMATR'          
          LEFT join ci_wf_proc_char srch_amt on srch_amt.wf_proc_id=a.wf_proc_id and srch_amt.char_type_cd= rpad('NZ-AMT' || PR.RETAIL_TYPE, 8)  
          LEFT JOIN CI_ACCT_CHAR VIRT_ACC ON VIRT_ACC.ACCT_ID = A.ACCT_ID AND VIRT_ACC.CHAR_TYPE_CD='VIRT-ACC'
          LEFT join ci_acct_char GRUPEI on GRUPEI.acct_id = a.acct_id and GRUPEI.char_type_cd='GRUPEI'
          LEFT join ci_char_val_l GRUPEIn on GRUPEIn.char_type_cd=GRUPEI.char_type_cd and GRUPEIn.char_val=GRUPEI.char_val and GRUPEIn.language_cd='RUS'

          LEFT join ci_acct_char U4ASTOK on U4ASTOK.acct_id = a.acct_id and U4ASTOK.char_type_cd='U4ASTOK' 

          LEFT JOIN CI_ACCT_CHAR SW_EMAIL ON SW_EMAIL.ACCT_ID =a.acct_id AND SW_EMAIL.CHAR_TYPE_CD='SW-EMAIL'
          LEFT JOIN CI_ACCT_CHAR SW_SMS ON SW_SMS.ACCT_ID =a.acct_id AND SW_SMS.CHAR_TYPE_CD='SW-SMS' 
          LEFT JOIN CI_ACCT_CHAR SW_SITE ON SW_SITE.ACCT_ID =a.acct_id AND SW_SITE.CHAR_TYPE_CD='SW-SITE'  
          LEFT JOIN CI_ACCT_CHAR SW_BILL ON SW_BILL.ACCT_ID =a.acct_id AND SW_BILL.CHAR_TYPE_CD='SW-BILL'     
          LEFT JOIN CI_ACCT_CHAR SW_SMI ON SW_SMI.ACCT_ID =a.acct_id AND SW_SMI.CHAR_TYPE_CD='SW-SMI'      
          LEFT JOIN CI_ACCT_CHAR SW_POST ON SW_POST.ACCT_ID =a.acct_id AND SW_POST.CHAR_TYPE_CD='SW-POST' 
          LEFT JOIN CI_ACCT_CHAR SW_COUR ON SW_COUR.ACCT_ID =a.acct_id AND SW_COUR.CHAR_TYPE_CD='SW-COUR'                                  
        where 
           1 = 1
          AND 'N'=TRIM(coalesce( 
              (select dzm.char_val
               from ci_acct_char dzm
               where dzm.acct_id = a.acct_id and dzm.char_type_cd='DZ-MORAT' and  dzm.effdt=
                 (select max(effdt) from ci_acct_char where acct_id=dzm.acct_id and char_type_cd=dzm.char_type_cd)
              ), 'N')) 
         and (VIRT_ACC.EFFDT is null or VIRT_ACC.EFFDT = (select max(ac2.effdt) from CI_ACCT_CHAR ac2 where ac2.acct_id = VIRT_ACC.ACCT_ID and ac2.char_type_cd = VIRT_ACC.CHAR_TYPE_CD and ac2.effdt <= sysdate))                      
         and nvl(trim(VIRT_ACC.CHAR_VAL),'N') <> 'Y' 
         and (GRUPEI.effdt is null or GRUPEI.effdt=(select max(effdt) from ci_acct_char where acct_id=GRUPEI.acct_id and char_type_cd=GRUPEI.char_type_cd))
         and (U4ASTOK.effdt is null or U4ASTOK.effdt=(select max(effdt) from ci_acct_char where acct_id=U4ASTOK.acct_id and char_type_cd=U4ASTOK.char_type_cd))  
          AND (SW_EMAIL.EFFDT is null or SW_EMAIL.EFFDT = (select max(ac2.effdt) from CI_ACCT_CHAR ac2 where ac2.acct_id = SW_EMAIL.ACCT_ID and ac2.char_type_cd = SW_EMAIL.CHAR_TYPE_CD))  
          AND (SW_SMS.EFFDT is null or SW_SMS.EFFDT = (select max(ac2.effdt) from CI_ACCT_CHAR ac2 where ac2.acct_id = SW_SMS.ACCT_ID and ac2.char_type_cd = SW_SMS.CHAR_TYPE_CD)) 
          AND (SW_SITE.EFFDT is null or SW_SITE.EFFDT = (select max(ac2.effdt) from CI_ACCT_CHAR ac2 where ac2.acct_id = SW_SITE.ACCT_ID and ac2.char_type_cd = SW_SITE.CHAR_TYPE_CD)) 
          AND (SW_BILL.EFFDT is null or SW_BILL.EFFDT = (select max(ac2.effdt) from CI_ACCT_CHAR ac2 where ac2.acct_id = SW_BILL.ACCT_ID and ac2.char_type_cd = SW_BILL.CHAR_TYPE_CD))      
          AND (SW_SMI.EFFDT is null or SW_SMI.EFFDT = (select max(ac2.effdt) from CI_ACCT_CHAR ac2 where ac2.acct_id = SW_SMI.ACCT_ID and ac2.char_type_cd = SW_SMI.CHAR_TYPE_CD))
          AND (SW_POST.EFFDT is null or SW_POST.EFFDT = (select max(ac2.effdt) from CI_ACCT_CHAR ac2 where ac2.acct_id = SW_POST.ACCT_ID and ac2.char_type_cd = SW_POST.CHAR_TYPE_CD))  
          AND (SW_COUR.EFFDT is null or SW_COUR.EFFDT = (select max(ac2.effdt) from CI_ACCT_CHAR ac2 where ac2.acct_id = SW_COUR.ACCT_ID and ac2.char_type_cd = SW_COUR.CHAR_TYPE_CD))                                 
          )
        , WFMAIN AS (
         SELECT
           A.ACCT_ID
          ,A.WF_PROC_ID
          ,A.WF_DAT
          ,A.DOG_NUM 
          ,A.DOG_NAME
          ,A.DOG_STAT
          ,A.DOG_KLS_OESK
          ,A.GRUPEI
          ,A.U4ASTOK          
          ,A.LAST_PROC_ID
          ,A.NAME_EXECUT_C
          ,A.NAME_ESTIMATR
          ,to_number(coalesce(replace(nz_amt.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') nz_amt
          ,to_number(coalesce(replace(nz_amt0.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') nz_amt0
          ,to_number(coalesce(replace(nz_amt2.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') nz_amt2
          ,to_number(coalesce(replace(nz_amt4.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') nz_amt4
          ,to_number(coalesce(replace(nz_amt7.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') nz_amt7
          ,to_number(coalesce(replace(nz_amt8.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') nz_amt8

          ,to_number(coalesce(replace(wz_amt.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') wz_amt
          ,to_number(coalesce(replace(wz_amt0.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') wz_amt0
          ,to_number(coalesce(replace(wz_amt2.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') wz_amt2
          ,to_number(coalesce(replace(wz_amt4.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') wz_amt4
          ,to_number(coalesce(replace(wz_amt7.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') wz_amt7
          ,to_number(coalesce(replace(wz_amt8.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') wz_amt8

          ,to_number(coalesce(replace(cz_amt.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') cz_amt
          ,to_number(coalesce(replace(cz_amt0.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') cz_amt0
          ,to_number(coalesce(replace(cz_amt2.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') cz_amt2
          ,to_number(coalesce(replace(cz_amt4.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') cz_amt4
          ,to_number(coalesce(replace(cz_amt7.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') cz_amt7
          ,to_number(coalesce(replace(cz_amt8.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') cz_amt8
  
          ,to_number(coalesce(replace(perep.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') PEREPLAT
          ,to_number(coalesce(replace(nerop.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') T_NERSUM
          ,to_number(coalesce(replace(nz_amtbu.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') NZ_AMTBU
          ,to_number(coalesce(replace(AMNT_CUR.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') AMNT_CUR
          ,to_number(coalesce(replace(NZ_AMT21.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') NZ_AMT21
          ,to_number(coalesce(replace(NZ_AMT22.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') NZ_AMT22

          ,to_number(coalesce(replace(obz.adhoc_char_val, ',', '.'), '0.00'), '9999999990.00') OBZ_M_SR 
          ,decode(rpad(ntctp.char_val, 5), 'CLAIM', 'Претензия', 'Уведомление') NOTICETP 

          --,intor.adhoc_char_val CNTINTOR
          ,CM_DZ_REP_UN_BILLING.GET_COUNT_OBJECT_BY_ACCT(a.acct_id, 'EO') cnt_eo
          ,CM_DZ_REP_UN_BILLING.GET_COUNT_OBJECT_BY_ACCT(a.acct_id, 'CP') cnt_cp
          ,CM_DZ_REP_UN_BILLING.GET_COUNT_CP_BY_MAIN_PROC(a.wf_proc_id) CNT_CP_LIM
  
          ,(select to_char(trunc(wp2.cre_dttm),'DD.MM.YYYY') from ci_wf_proc wp2 where wp2.wf_proc_id=a.last_proc_id) last_tmpl_date
          , (select w2.descr 
             from ci_wf_proc w1 
             join ci_wf_proc_tmpl_l w2 on w1.wf_proc_tmpl_cd=w2.wf_proc_tmpl_cd and w2.language_cd='RUS'
             where wf_proc_id=a.last_proc_id) last_tmpl_name
             
          ,CM_DZ_REP_UN_BILLING.GET_ROWER_RESERV('AB', A.ACCT_ID) POWER_AB
          ,CM_DZ_REP_UN_BILLING.GET_ROWER_RESERV('TB', A.ACCT_ID) POWER_TB
          ,CM_DZ_REP_UN_BILLING.GET_COUNT_EO_CHAR_BY_ACCT(A.ACCT_ID, NULL, 'PRIL4530')  PRIL4530
          ,CM_DZ_REP_UN_BILLING.GET_COUNT_EO_CHAR_BY_ACCT(A.ACCT_ID, NULL, 'SOC-EO')  SOC_EO
          ,(SELECT wm_concat(INTNEOPL.ADHOC_CHAR_VAL) 
            FROM ci_wf_proc_char INTNEOPL
            WHERE INTNEOPL.WF_PROC_ID = A.WF_PROC_ID
              AND INTNEOPL.CHAR_TYPE_CD = 'INTNEOPL' 
            ) INTNEOPL
          ,/*(SELECT COUNT(INTNEOPL.ADHOC_CHAR_VAL) 
            FROM ci_wf_proc_char INTNEOPL
            WHERE INTNEOPL.WF_PROC_ID = A.WF_PROC_ID
              AND INTNEOPL.CHAR_TYPE_CD = 'INTNEOPL' 
            )*/ 
           CNTINTOR.ADHOC_CHAR_VAL CNTINTOR 
        ,CASE WHEN TRIM(A.SW_EMAIL) = 'Y' THEN CM_DZ_REP_UTILS.GET_CONTACT_DATA_BY_ACCT(A.ACCT_ID,'EMAI') ELSE '' END SW_EMAIL
        ,CASE WHEN TRIM(A.SW_SMS) = 'Y' THEN CM_DZ_REP_UTILS.GET_CONTACT_DATA_BY_ACCT(A.ACCT_ID,'PHON') ELSE '' END SW_SMS  
        ,CASE WHEN TRIM(A.SW_SITE) = 'Y' THEN 'Да' WHEN TRIM(A.SW_SITE) = 'N' THEN 'Нет' ELSE '' END SW_SITE
        ,CASE WHEN TRIM(A.SW_BILL) = 'Y' THEN 'Да' WHEN TRIM(A.SW_BILL) = 'N' THEN 'Нет' ELSE '' END SW_BILL
        ,CASE WHEN TRIM(A.SW_SMI) = 'Y' THEN 'Да' WHEN TRIM(A.SW_SMI) = 'N' THEN 'Нет' ELSE '' END SW_SMI
        ,CASE WHEN TRIM(A.SW_POST) = 'Y' THEN 'Да' WHEN TRIM(A.SW_POST) = 'N' THEN 'Нет' ELSE '' END SW_POST 
        ,CASE WHEN TRIM(A.SW_COUR) = 'Y' THEN 'Да' WHEN TRIM(A.SW_COUR) = 'N' THEN 'Нет' ELSE '' END SW_COUR      
        ,TF_WAY_P_DESCR.DESCR TF_WAY_P                       
                    
      FROM ACCT A
      left join ci_wf_proc_char nz_amt on nz_amt.wf_proc_id=a.wf_proc_id and nz_amt.char_type_cd='NZ-AMT'
      left join ci_wf_proc_char nz_amt0 on nz_amt0.wf_proc_id=a.wf_proc_id and nz_amt0.char_type_cd='NZ-AMT0'
      left join ci_wf_proc_char nz_amt2 on nz_amt2.wf_proc_id=a.wf_proc_id and nz_amt2.char_type_cd='NZ-AMT2'
      left join ci_wf_proc_char nz_amt4 on nz_amt4.wf_proc_id=a.wf_proc_id and nz_amt4.char_type_cd='NZ-AMT4'
      left join ci_wf_proc_char nz_amt7 on nz_amt7.wf_proc_id=a.wf_proc_id and nz_amt7.char_type_cd='NZ-AMT7'
      left join ci_wf_proc_char nz_amt8 on nz_amt8.wf_proc_id=a.wf_proc_id and nz_amt8.char_type_cd='NZ-AMT8'
      
      left join ci_wf_proc_char wz_amt on wz_amt.wf_proc_id=a.wf_proc_id and wz_amt.char_type_cd='WZ-AMT'
      left join ci_wf_proc_char wz_amt0 on wz_amt0.wf_proc_id=a.wf_proc_id and wz_amt0.char_type_cd='WZ-AMT0'
      left join ci_wf_proc_char wz_amt2 on wz_amt2.wf_proc_id=a.wf_proc_id and wz_amt2.char_type_cd='WZ-AMT2'
      left join ci_wf_proc_char wz_amt4 on wz_amt4.wf_proc_id=a.wf_proc_id and wz_amt4.char_type_cd='WZ-AMT4'
      left join ci_wf_proc_char wz_amt7 on wz_amt7.wf_proc_id=a.wf_proc_id and wz_amt7.char_type_cd='WZ-AMT7'
      left join ci_wf_proc_char wz_amt8 on wz_amt8.wf_proc_id=a.wf_proc_id and wz_amt8.char_type_cd='WZ-AMT8'
      
      left join ci_wf_proc_char cz_amt on cz_amt.wf_proc_id=a.wf_proc_id and cz_amt.char_type_cd='CZ-AMT'
      left join ci_wf_proc_char cz_amt0 on cz_amt0.wf_proc_id=a.wf_proc_id and cz_amt0.char_type_cd='CZ-AMT0'
      left join ci_wf_proc_char cz_amt2 on cz_amt2.wf_proc_id=a.wf_proc_id and cz_amt2.char_type_cd='CZ-AMT2'
      left join ci_wf_proc_char cz_amt4 on cz_amt4.wf_proc_id=a.wf_proc_id and cz_amt4.char_type_cd='CZ-AMT4'
      left join ci_wf_proc_char cz_amt7 on cz_amt7.wf_proc_id=a.wf_proc_id and cz_amt7.char_type_cd='CZ-AMT7'
      left join ci_wf_proc_char cz_amt8 on cz_amt8.wf_proc_id=a.wf_proc_id and cz_amt8.char_type_cd='CZ-AMT8'

      left join ci_wf_proc_char perep on perep.wf_proc_id=a.wf_proc_id and perep.char_type_cd='PEREPLAT'
      left join ci_wf_proc_char nerop on nerop.wf_proc_id=a.wf_proc_id and nerop.char_type_cd='T-NERSUM'
      left join ci_wf_proc_char nz_amtbu on nz_amtbu.wf_proc_id=a.wf_proc_id and nz_amtbu.char_type_cd='NZ-AMTBU'
      left join ci_wf_proc_char AMNT_CUR on AMNT_CUR.wf_proc_id=a.wf_proc_id and AMNT_CUR.char_type_cd='AMNT-CUR'
      left join ci_wf_proc_char NZ_AMT21 on NZ_AMT21.wf_proc_id=a.wf_proc_id and NZ_AMT21.char_type_cd='NZ-AMT2' /*TR-39837 Временно*/
      left join ci_wf_proc_char NZ_AMT22 on NZ_AMT22.wf_proc_id=a.wf_proc_id and NZ_AMT22.char_type_cd='NZ-AMT22'   

      left join ci_wf_proc_char CNTINTOR on CNTINTOR.wf_proc_id=a.wf_proc_id and CNTINTOR.char_type_cd='CNTINTOR'
      left join ci_wf_proc_char obz on obz.wf_proc_id=a.wf_proc_id and obz.char_type_cd='OBZ-M-SR'
      left join ci_wf_proc_char ntctp on ntctp.wf_proc_id=a.wf_proc_id and ntctp.char_type_cd='NOTICETP' 

      left join ci_wf_proc_char TF_WAY_P on TF_WAY_P.wf_proc_id=a.wf_proc_id and TF_WAY_P.char_type_cd='TF-WAY-P'
      left join ci_char_val_l TF_WAY_P_DESCR on TF_WAY_P_DESCR.CHAR_TYPE_CD = TF_WAY_P.CHAR_TYPE_CD and TF_WAY_P_DESCR.CHAR_VAL = TF_WAY_P.CHAR_VAL and TF_WAY_P_DESCR.LANGUAGE_CD = 'RUS'          
          )
      /*Оплата, поступившая за месяц*/
      , ACCT_PAY AS (
      SELECT /*+MATERIALIZE*/
             A.ACCT_ID, 
             -SUM(CASE WHEN (NOT GFT.PARENT_ID IN ('REFOUND','REFOUNDP') AND NOT PAY_PROP.CM_ACCT_PAY_MATCH_EVT_PROP_ID IS NULL  AND NOT(RP.CM_RETAIL_TYPE='0' AND GFT.PARENT_ID='TRANSF')) THEN AE.CUR_AMT ELSE 0 END) SUM_PAY
      FROM ACCT A
      INNER JOIN CI_PAY PAY ON PAY.ACCT_ID=A.ACCT_ID
      INNER JOIN CI_PAY_SEG PS ON PS.PAY_ID=PAY.PAY_ID
      INNER JOIN CM_ACCT_PAY_MATCH_EVT_PROP PAY_PROP ON PAY_PROP.PAY_SEG_ID=PS.PAY_SEG_ID
      INNER JOIN CM_ACCOUNT_ENTRY AE ON AE.CM_ACCT_PAY_MATCH_EVT_PROP_ID=PAY_PROP.CM_ACCT_PAY_MATCH_EVT_PROP_ID
      INNER JOIN CM_RATE_PROP RP ON RP.CM_RATE_PROP_ID = AE.CM_RATE_PROP_ID
      INNER JOIN CI_FT GFT ON GFT.FT_ID=AE.CM_GENERATIVE_FT_ID
      INNER JOIN CM_FT_EXP PS_PROP ON PS_PROP.FT_ID=PAY_PROP.CM_ACCT_PAY_MATCH_EVT_ID
      WHERE 1 = 1 
        AND AE.Accounting_Dt BETWEEN TRUNC(TRUNC(SYSDATE), 'MM') AND TRUNC(SYSDATE)
      GROUP BY A.ACCT_ID 
      ) 
     /*Информация по транзитным потребителям*/
     , INFO_POINTDLV AS(
     SELECT  /*+MATERIALIZE*/
             A.ACCT_ID
            ,A.WF_PROC_ID             
            ,L.CM_SOURCE_PREM_ID CP_ID
     FROM PARAMETERS PR
     INNER JOIN ACCT A ON 1 = 1
     INNER JOIN CM_EO_ACCOUNT_LINKAGE EAL ON EAL.ACCT_ID = A.ACCT_ID AND TRUNC(SYSDATE) BETWEEN EAL.START_DT AND NVL(EAL.END_DT, TRUNC(SYSDATE))
     INNER JOIN CM_PREM_LINK L ON L.CM_DEST_PREM_ID = EAL.CM_EO_ID AND L.CM_PREM_LINK_TYPE_CD='EE_SUPPLY'
                  AND TRUNC(SYSDATE) BETWEEN L.START_DT AND nvl(L.END_DT, TRUNC(SYSDATE))                       
     WHERE 1 = 1
       AND PR.SHOW_TRAN = 'Y'
     UNION
     SELECT  /*+MATERIALIZE*/
             A.ACCT_ID
            ,A.WF_PROC_ID               
            ,POINTDLV.CHAR_VAL_FK1 CP_ID
            /*,LIM.WF_PROC_ID PROC_ID_LIM*/
     FROM PARAMETERS PR
     INNER JOIN ACCT A ON 1 = 1
     INNER JOIN CI_WF_PROC NOTIF ON NOTIF.CR_BY_WF_PROC_ID = A.WF_PROC_ID AND NOTIF.WF_STAT_FLG = '10'
     INNER JOIN CI_WF_PROC LIM ON LIM.CR_BY_WF_PROC_ID = NOTIF.WF_PROC_ID AND LIM.WF_STAT_FLG = '10'
     INNER JOIN CI_WF_PROC_CHAR POINTDLV ON POINTDLV.WF_PROC_ID = LIM.WF_PROC_ID AND POINTDLV.CHAR_TYPE_CD = 'POINTDLV'                         
     WHERE 1 = 1
       AND PR.SHOW_TRAN = 'N'                  
     ) 
     , INTO_ALL_CP AS (
      SELECT IP.ACCT_ID
            --,IP.WF_PROC_ID
            ,IP.CP_ID
      FROM INFO_POINTDLV IP 
      UNION
      SELECT IP.ACCT_ID
            --,IP.WF_PROC_ID
            ,CP.PREM_ID CP_ID
      FROM INFO_POINTDLV IP 
      INNER JOIN CM_PREM_LINK L ON L.CM_DEST_PREM_ID = IP.CP_ID AND L.CM_PREM_LINK_TYPE_CD='EE_SUPPLY'
                  AND TRUNC(SYSDATE) BETWEEN L.START_DT AND nvl(L.END_DT, TRUNC(SYSDATE))
      INNER JOIN CI_PREM CP ON CP.PREM_ID = L.CM_SOURCE_PREM_ID AND CP.PREM_TYPE_CD = 'CP'     
     ) 
     , INFO_IK_TRANS AS(
      SELECT /*+MATERIALIZE*/
             IAC.ACCT_ID
            --,IAC.WF_PROC_ID
            ,IAC.CP_ID
            ,TRANSFER.CM_DEST_PREM_ID IK_TRANS
            ,CM_DZ_REP_UN_BILLING.GET_INFO_BY_IK_TRANS(TRANSFER.CM_DEST_PREM_ID, 'EO_NAME') EO_NAME_TRANS
            ,CM_DZ_REP_UN_BILLING.GET_INFO_BY_IK_TRANS(TRANSFER.CM_DEST_PREM_ID, 'DOG_NUM') DOG_NUM_TRANS
            ,CM_DZ_REP_UN_BILLING.GET_INFO_BY_IK_TRANS(TRANSFER.CM_DEST_PREM_ID, 'DOG_NAME') DOG_NAME_TRANS                        
     FROM INTO_ALL_CP IAC
     -- основные ТУ
     INNER JOIN CM_PREM_LINK L ON L.CM_DEST_PREM_ID = IAC.CP_ID AND L.CM_PREM_LINK_TYPE_CD='EE_SUPPLY'
        AND TRUNC(SYSDATE) BETWEEN L.START_DT AND nvl(L.END_DT, TRUNC(SYSDATE))
     -- транзитные ТУ  
      INNER JOIN CM_PREM_LINK L2 ON L2.CM_DEST_PREM_ID = L.CM_SOURCE_PREM_ID AND L2.CM_PREM_LINK_TYPE_CD='EE_SUPPLY'
        AND TRUNC(SYSDATE) BETWEEN L2.START_DT AND nvl(L2.END_DT, TRUNC(SYSDATE))
      INNER JOIN CM_PREM_LINK TRANSFER on TRANSFER.CM_SOURCE_PREM_ID = L2.CM_SOURCE_PREM_ID and TRANSFER.CM_PREM_LINK_TYPE_CD = 'TRANSFER'
        AND TRUNC(SYSDATE) BETWEEN TRANSFER.START_DT AND nvl(TRANSFER.END_DT, TRUNC(SYSDATE))              
     )
     /*Информация по ТП и ЭО, в отношение которых на момент выгрузки введено самоограничение/частичное ограничение*/  
     ,INFO_OBJECT_LIM AS (
      SELECT /*+MATERIALIZE*/
             DISTINCT
             A.ACCT_ID
            /*,A.WF_PROC_ID*/
            ,POINTDLV.CHAR_VAL_FK1 CP_ID
            ,nvl(pl2.cm_dest_prem_id, pl.cm_dest_prem_id) EO_ID                    
      FROM ACCT A
      INNER JOIN CI_WF_PROC NOTIF ON NOTIF.CR_BY_WF_PROC_ID = A.WF_PROC_ID AND NOTIF.WF_STAT_FLG = '10'
      INNER JOIN CI_WF_PROC LIM ON LIM.CR_BY_WF_PROC_ID = NOTIF.WF_PROC_ID AND LIM.WF_STAT_FLG = '10'
      INNER JOIN CI_WF_PROC_CHAR POINTDLV ON POINTDLV.WF_PROC_ID = LIM.WF_PROC_ID AND POINTDLV.CHAR_TYPE_CD = 'POINTDLV'         
      LEFT JOIN CI_WF_PROC_CHAR DAT_LIMS ON DAT_LIMS.WF_PROC_ID = LIM.WF_PROC_ID AND DAT_LIMS.CHAR_TYPE_CD = 'DAT-LIMS'  
      LEFT JOIN CI_WF_PROC_CHAR DAT_LIMP ON DAT_LIMP.WF_PROC_ID = LIM.WF_PROC_ID AND DAT_LIMP.CHAR_TYPE_CD = 'DAT-LIMP'
      INNER JOIN cm_prem_link pl on pl.cm_source_prem_id=POINTDLV.CHAR_VAL_FK1 and pl.cm_prem_link_type_cd='EE_SUPPLY                     '
                         and pl.start_dt<=TRUNC(SYSDATE) and (pl.end_dt is null or pl.end_dt>=TRUNC(SYSDATE))
     -- ЭО может на другом уровне, например, ЭО / Расчетная ТП / Нерасчетная ТП                         
      LEFT JOIN cm_prem_link pl2 on pl2.cm_source_prem_id=pl.cm_dest_prem_id and pl2.cm_prem_link_type_cd='EE_SUPPLY                     '
                         and pl2.start_dt<=TRUNC(SYSDATE) and (pl2.end_dt is null or pl2.end_dt>=TRUNC(SYSDATE))   
      WHERE DAT_LIMS.ADHOC_CHAR_VAL IS NOT NULL OR DAT_LIMP.ADHOC_CHAR_VAL IS NOT NULL     
        )
     ,INFO_OBJECT_LIM_DESCR AS (
      SELECT /*+MATERIALIZE*/
             OL.ACCT_ID
            /*,OL.WF_PROC_ID*/
           /* ,rtrim(xmlagg(xmlelement("a", 
            (SELECT max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
            FROM ci_prem_char pc
            WHERE pc.prem_id = OL.CP_ID
              AND pc.char_type_cd = 'CP-NAME') || ', '||CHR(13))).extract('/a/text()').getstringval(), ', ' ||CHR(13)) CP_NAME_LIM
            ,rtrim(xmlagg(xmlelement("a",               
            (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
            from ci_prem_char pc
            where pc.char_type_cd = 'PREM-NAM'
              and pc.prem_id = OL.EO_ID) || ', '||CHR(13))).extract('/a/text()').getstringval(), ', '||CHR(13)) EO_NAME_LIM*/
            ,REPLACE(WM_CONCAT(               
                        (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                        from ci_prem_char pc
                        where pc.char_type_cd = 'CP-NAME'
                          and pc.prem_id = OL.CP_ID)
                         ), ',,', ', '||CHR(13))  CP_NAME_LIM  
            ,REPLACE(WM_CONCAT(               
                        (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                        from ci_prem_char pc
                        where pc.char_type_cd = 'PREM-NAM'
                          and pc.prem_id = OL.EO_ID)
                         ), ',,', ', '||CHR(13))  EO_NAME_LIM                                         
           /*,rtrim(xmlagg(xmlelement("a",               
            (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
            from ci_prem_char pc
            where pc.char_type_cd = 'F-ADR-1'
              and pc.prem_id = OL.EO_ID) || ', '||CHR(13))).extract('/a/text()').getstringval(), ', '||CHR(13)) EO_ADR_LIM*/
            ,REPLACE(WM_CONCAT(               
                        (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
                        from ci_prem_char pc
                        where pc.char_type_cd = 'F-ADR-1'
                          and pc.prem_id = OL.EO_ID)
                         ), ',,', ', '||CHR(13))  EO_ADR_LIM                                           
                    
      FROM INFO_OBJECT_LIM OL
      GROUP BY 
             OL.ACCT_ID
            /*,OL.WF_PROC_ID*/         
     )     
     
     SELECT
           F.ACCT_ID
          ,F.WF_PROC_ID
          ,F.WF_DAT              
          ,F.DOG_NUM 
          ,F.DOG_NAME
          ,F.DOG_STAT
          ,F.DOG_KLS_OESK
          ,F.GRUPEI
          ,F.U4ASTOK            
          ,F.LAST_PROC_ID
          ,F.NAME_EXECUT_C
          ,F.NAME_ESTIMATR
          ,F.LAST_TMPL_DATE 
          ,F.LAST_TMPL_NAME
          ,F.POWER_AB
          ,F.POWER_TB
          ,F.PRIL4530   
          ,F.SOC_EO          
          ,F.NOTICETP
          ,F.OBZ_M_SR
          ,F.CNTINTOR
          ,F.INTNEOPL
          ,F.CNT_CP_LIM
          ,F.SW_EMAIL
          ,F.SW_SMS  
          ,F.SW_SITE
          ,F.SW_BILL
          ,F.SW_SMI
          ,F.SW_POST
          ,F.SW_COUR    
          ,F.TF_WAY_P        
                    
          ,F.AMNT_CUR -- Обязательства, срок исполнения по которым еще не наступил        
          ,PEREPLAT + T_NERSUM  KZ_DATE -- КЗ на дату
          ,nz_amt0 + nz_amt2 + wz_amt0 + wz_amt2  DZ_DATE -- ДЗ на дату
          ,(nz_amt0 + nz_amt2 + wz_amt0 + wz_amt2) - (PEREPLAT + T_NERSUM) DZ_KZ_DATE -- ДЗ-КЗ на дату
          ,nz_amt0 + nz_amt2 NEW_DEBT -- Новая задолженность
          ,NZ_AMT21 NEW_DEBT_2 -- Окончательный платеж
          ,NZ_AMT0  NEW_DEBT_0-- Предварительный платеж  
          ,/*NZ_AMT22*/ NZ_AMTBU NEW_DEBT_BU-- Безучетное потребеление 
          ,wz_amt0 + wz_amt2 WORK_DEBT_NOFINE -- Задолженность в работе, без пени 
          ,nz_amt7 + nz_amt4 NEW_DEBT_FINE -- Задолженность по пени новая 
          ,wz_amt7 + wz_amt4 WORK_DEBT_FINE -- Задолженность по пени в работе   
          
          ,PAY.SUM_PAY
          
          ,IT.IK_TRANS
          ,IT.EO_NAME_TRANS
          ,IT.DOG_NUM_TRANS
          ,IT.DOG_NAME_TRANS
          
          ,OL.CP_NAME_LIM
          ,OL.EO_NAME_LIM
          ,OL.EO_ADR_LIM
          ,ROW_NUMBER() OVER(PARTITION BY F.DOG_NUM ORDER BY F.DOG_NUM) RN     
      
      FROM WFMAIN F 
      LEFT JOIN ACCT_PAY PAY ON PAY.ACCT_ID = F.ACCT_ID 
      LEFT JOIN INFO_IK_TRANS IT ON IT.ACCT_ID = F.ACCT_ID 
      LEFT JOIN INFO_OBJECT_LIM_DESCR OL ON OL.ACCT_ID = F.ACCT_ID  
      ORDER BY F.DOG_NUM
            
     ) LOOP
     
     IF I.RN <> 1 THEN
       V_RET.ACCT_ID := I.ACCT_ID;
       V_RET.DOG_NUM := NULL;
       V_RET.DOG_NAME := NULL;
       V_RET.DOG_STAT := NULL;
       V_RET.DOG_KLS_OESK := NULL;
       V_RET.DOG_GRUPEI := NULL;
       V_RET.DOG_U4ASTOK := NULL;       
       V_RET.NAME_EXECUT_C := NULL;  
       V_RET.NAME_ESTIMATR := NULL;
       V_RET.LAST_TMPL_DATE := NULL;
       V_RET.LAST_TMPL_NAME := NULL;  
       V_RET.POWER_AB := NULL;
       V_RET.POWER_TB := NULL;
       V_RET.PRIL4530 := NULL;  
       V_RET.SOC_EO := NULL;
       V_RET.NOTICETP := NULL;
       V_RET.OBZ_M_SR := NULL;  
       V_RET.CNTINTOR := NULL; 
       V_RET.INTNEOPL := NULL;        
       V_RET.CNT_CP_LIM := NULL;       
       
       V_RET.AMNT_CUR := NULL;
       V_RET.KZ_DATE := NULL;  
       V_RET.DZ_DATE := NULL;  
       V_RET.DZ_KZ_DATE := NULL;                                                                 
       V_RET.NEW_DEBT := NULL;
       V_RET.NEW_DEBT_2 := NULL;
       V_RET.NEW_DEBT_0 := NULL;  
       V_RET.NEW_DEBT_BU := NULL;  
       V_RET.WORK_DEBT_NOFINE := NULL;
       V_RET.NEW_DEBT_FINE := NULL;
       V_RET.WORK_DEBT_FINE := NULL;   

       V_RET.SUM_PAY := NULL;   

       V_RET.CP_NAME_LIM := NULL;   
       V_RET.EO_NAME_LIM := NULL;   
       V_RET.EO_ADR_LIM := NULL; 

       V_RET.SW_EMAIL := NULL;
       V_RET.SW_SMS := NULL; 
       V_RET.SW_SITE := NULL;
       V_RET.SW_BILL := NULL;
       V_RET.SW_SMI := NULL;
       V_RET.SW_POST := NULL;
       V_RET.SW_COUR := NULL;                 
       
     ELSE
    
       V_RET.ACCT_ID := I.ACCT_ID;
       V_RET.DOG_NUM := I.DOG_NUM;
       V_RET.DOG_NAME := I.DOG_NAME;
       V_RET.DOG_STAT := I.DOG_STAT;
       V_RET.DOG_KLS_OESK := I.DOG_KLS_OESK;
       V_RET.DOG_GRUPEI := I.GRUPEI;
       V_RET.DOG_U4ASTOK := I.U4ASTOK;       
       V_RET.NAME_EXECUT_C := I.NAME_EXECUT_C;  
       V_RET.NAME_ESTIMATR := I.NAME_ESTIMATR;
       V_RET.LAST_TMPL_DATE := I.LAST_TMPL_DATE;
       V_RET.LAST_TMPL_NAME := I.LAST_TMPL_NAME;  
       V_RET.POWER_AB := I.POWER_AB;
       V_RET.POWER_TB := I.POWER_TB;
       V_RET.PRIL4530 := I.PRIL4530;  
       V_RET.SOC_EO := I.SOC_EO;
       V_RET.NOTICETP := I.NOTICETP;
       V_RET.OBZ_M_SR := I.OBZ_M_SR;  
       V_RET.CNTINTOR := I.CNTINTOR; 
       V_RET.INTNEOPL := I.INTNEOPL;        
       V_RET.CNT_CP_LIM := I.CNT_CP_LIM;       
       
       V_RET.AMNT_CUR := I.AMNT_CUR;
       V_RET.KZ_DATE := I.KZ_DATE;  
       V_RET.DZ_DATE := I.DZ_DATE;  
       V_RET.DZ_KZ_DATE := I.DZ_KZ_DATE;                                                                 
       V_RET.NEW_DEBT := I.NEW_DEBT;
       V_RET.NEW_DEBT_2 := I.NEW_DEBT_2;
       V_RET.NEW_DEBT_0 := I.NEW_DEBT_0;  
       V_RET.NEW_DEBT_BU := I.NEW_DEBT_BU;         
       V_RET.WORK_DEBT_NOFINE := I.WORK_DEBT_NOFINE;
       V_RET.NEW_DEBT_FINE := I.NEW_DEBT_FINE;
       V_RET.WORK_DEBT_FINE := I.WORK_DEBT_FINE;   


       V_RET.SUM_PAY := I.SUM_PAY;   

       V_RET.CP_NAME_LIM := REPLACE(I.CP_NAME_LIM,'&quot;','"');   
       V_RET.EO_NAME_LIM := REPLACE(I.EO_NAME_LIM,'&quot;','"');   
       V_RET.EO_ADR_LIM := REPLACE(I.EO_ADR_LIM,'&quot;','"');   

       V_RET.SW_EMAIL := I.SW_EMAIL;
       V_RET.SW_SMS := I.SW_SMS;
       V_RET.SW_SITE := I.SW_SITE;
       V_RET.SW_BILL := I.SW_BILL;
       V_RET.SW_SMI := I.SW_SMI;
       V_RET.SW_POST := I.SW_POST;
       V_RET.SW_COUR := I.SW_COUR;     
       
      END IF;                                          
     
       V_RET.IK_TRANS := I.IK_TRANS;  
       V_RET.EO_NAME_TRANS := I.EO_NAME_TRANS;    
       V_RET.DOG_NUM_TRANS := I.DOG_NUM_TRANS;    
       V_RET.DOG_NAME_TRANS := I.DOG_NAME_TRANS;                                                
            
       PIPE ROW(V_RET);  
    END LOOP;      
   END DZ_LIST_OF_DEFAULTERS;                                          

FUNCTION GET_SOGL_RESTR_HEAD_PSK (i_case_id in char) return T_SOGL_RESTR_HEAD_PSK pipelined is
  
  v_sogl_rec T_SOGL_RESTR_HEAD_PSK_REC;
  v_per_id_agrmnt ci_per.per_id%type;
  v_per_id_dog ci_per.per_id%type;
  v_per_id_grnt ci_per.per_id%type;    
  v_buf varchar2(4000);
  
  begin

  select 
       dc1_num.Adhoc_Char_Val AS doc_num,
       dc1_dat.adhoc_char_val as doc_dat,
       PCZ_EVT.CHAR_VAL_FK1 wf_proc_id,
       PCZ_EVT.CHAR_VAL_FK2 evt_seq                       
  into v_sogl_rec.doc_num, v_sogl_rec.doc_dat, v_sogl_rec.wf_proc_id, v_sogl_rec.evt_seq
  from ci_case dc1
  inner join ci_case_char dc1_num on dc1_num.case_id = dc1.case_id and dc1_num.char_type_cd = 'DOC-NUM'
  inner join ci_case_char dc1_dat on dc1_dat.case_id = dc1.case_id and dc1_dat.char_type_cd = 'DOC-DAT'
  inner join ci_case_char PCZ_EVT on PCZ_EVT.case_id = dc1.case_id and PCZ_EVT.char_type_cd = 'PCZ-EVT'  
  where dc1.case_id = i_case_id;
  
  -- Данные договора
  select a.acct_id
      ,(select max(ac.adhoc_char_val) keep (dense_rank last order by ac.effdt)
      from ci_acct_char ac
      where ac.acct_id = A.ACCT_ID
       and ac.char_type_cd = 'DOG-NUM') DOG_NUM
     ,coalesce(
     (select max(ac.adhoc_char_val) keep (dense_rank last order by ac.effdt)
      from ci_acct_char ac
      where ac.acct_id = A.ACCT_ID
       and ac.char_type_cd = 'DOG-BDAT')
     ,to_char(a.setup_dt, 'dd.mm.yyyy'))
     ,CASE WHEN A.Cis_Division = 'TSK1' THEN 'г. Томск'
           WHEN A.Cis_Division = 'OMS2' THEN 'г. Омск' 
           WHEN A.Cis_Division = 'ORL1' THEN 'г. Орел'
           WHEN A.Cis_Division = 'SPB' THEN 'г. Санкт-Петербург' 
      END
     ,ap.per_id
     ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
        from ci_per_char pc
        where pc.per_id = ap.per_id
          and pc.char_type_cd = 'SHORT-NM')  DOG_NAME
     ,(select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
        from ci_per_char pc
        where pc.per_id = ap.per_id
          and pc.char_type_cd = 'NAIM-PLN')  DOG_NAME_FULL
     ,DOGOVOR.PER_ID 
     ,CASE WHEN A.Cis_Division = 'TSK1' THEN 'ПАО "Томскэнергосбыт"'
           WHEN A.Cis_Division = 'OMS2' THEN 'АО "Петербургская сбытовая компания"' 
           WHEN A.Cis_Division = 'ORL1' THEN 'ООО "Орловский энергосбыт"'
           WHEN A.Cis_Division = 'SPB' THEN 'АО "Петербургская сбытовая компания"'
      END            
  into v_sogl_rec.acct_id, v_sogl_rec.DOG_NUM, v_sogl_rec.DOG_DAT, v_sogl_rec.city, v_sogl_rec.main_per_id, v_sogl_rec.DOG_NAME, v_sogl_rec.DOG_NAME_FULL, v_per_id_dog, v_sogl_rec.GRNT_SHORT_NAME
  from Ci_Wf_Proc W
  inner join ci_wf_proc_char LIC_S4ET on LIC_S4ET.WF_PROC_ID = w.wf_proc_id and LIC_S4ET.CHAR_TYPE_CD = 'LIC-S4ET'
  inner join ci_acct a on a.acct_id = LIC_S4ET.CHAR_VAL_FK1
  inner join ci_acct_per ap on ap.acct_id = a.acct_id and ap.main_cust_sw = 'Y'
  inner join ci_acct_per DOGOVOR on DOGOVOR.acct_id = a.acct_id and DOGOVOR.ACCT_REL_TYPE_CD = 'DOGOVOR'
  where w.wf_proc_id = v_sogl_rec.wf_proc_id;

    -- Подписанты со стороны Потребителя
    begin
SELECT DISTINCT
  CASE
    WHEN (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
      ORDER BY pc.effdt)
      FROM ci_per_char pc
      WHERE pc.per_id     = pp.per_id2
      AND pc.char_type_cd = 'DL$N-RP'
      AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)) IS NULL
    THEN
      (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
      ORDER BY pc.effdt)
      FROM ci_per_char pc
      WHERE pc.per_id     = pp.per_id2
      AND pc.char_type_cd = 'FIO-RP'
      AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)
      )
    ELSE lower(
      (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
      ORDER BY pc.effdt)
      FROM ci_per_char pc
      WHERE pc.per_id     = pp.per_id2
      AND pc.char_type_cd = 'DL$N-RP'
      AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)
      ))
      ||
      (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
      ORDER BY pc.effdt)
      FROM ci_per_char pc
      WHERE pc.per_id     = pp.per_id2
      AND pc.char_type_cd = 'FIO-RP'
      AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)
      )
  END in_face,
  (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
  ORDER BY pc.effdt)
  FROM ci_per_char pc
  WHERE pc.per_id     = pp.per_id2
  AND pc.char_type_cd = 'RIGHTDOC'
  AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)
  ) AS acct_osnov,
  NVL(
  (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
  ORDER BY pc.effdt)
  FROM ci_per_char pc
  WHERE pc.per_id     = pp.per_id2
  AND pc.char_type_cd = 'SHORT-NM'
  AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)
  ),
  (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
  ORDER BY pc.effdt)
  FROM ci_per_char pc
  WHERE pc.per_id     = pp.per_id2
  AND pc.char_type_cd = 'NAIM-PLN'
  AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)
  )) AS NAME_SIGNATORY,
  (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
  ORDER BY pc.effdt)
  FROM ci_per_char pc
  WHERE pc.per_id     = pp.per_id2
  AND pc.char_type_cd = 'NAIM-PLN'
  AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)
  ) AS NAME_SIGNATORY_PLN,
  (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
  ORDER BY pc.effdt)
  FROM ci_per_char pc
  WHERE pc.per_id     = pp.per_id2
  AND pc.char_type_cd = 'FIO-RP'
  AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)
  ) AS NAME_SIGNATORY_RP,
  NVL(
  (SELECT MAX(trim(pc.adhoc_char_val)) keep (dense_rank last
  ORDER BY pc.effdt)
  FROM ci_per_char pc
  WHERE pc.per_id     = pp.per_id2
  AND pc.char_type_cd = 'JOBD-SRC'
  AND pc.effdt       <= NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),sysdate)
  ),PRTL.DESCR12) AS STAFFPOS_SIGNATORY
  into v_sogl_rec.in_face, v_sogl_rec.acct_osnov, v_sogl_rec.NAME_SIGNATORY, v_sogl_rec.NAME_SIGNATORY_PLN, v_sogl_rec.NAME_SIGNATORY_RP, v_sogl_rec.STAFFPOS_SIGNATORY
FROM CI_PER_PER PP
INNER JOIN CI_PER_REL_TYPE_L PRTL
ON PRTL.PER_REL_TYPE_CD = PP.PER_REL_TYPE_CD
AND PRTL.LANGUAGE_CD    = 'RUS'
INNER JOIN ci_per_char FIO_RP
ON FIO_RP.PER_ID        = pp.per_id2
AND FIO_RP.CHAR_TYPE_CD = 'FIO-RP'
LEFT JOIN ci_per_char SHORT_NM
ON SHORT_NM.PER_ID        = pp.per_id2
AND SHORT_NM.CHAR_TYPE_CD = 'SHORT-NM'
WHERE pp.per_id1          = v_sogl_rec.main_per_id
AND NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),TRUNC(SYSDATE)) BETWEEN pp.start_dt AND NVL(pp.end_dt, NVL(to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'),TRUNC(SYSDATE)))
AND PP.PER_REL_TYPE_CD LIKE 'DL%';
    exception when no_data_found then v_sogl_rec.in_face := null; v_sogl_rec.NAME_SIGNATORY := null; v_sogl_rec.NAME_SIGNATORY_RP:= null; v_sogl_rec.acct_osnov := null; v_sogl_rec.STAFFPOS_SIGNATORY := null;
              when too_many_rows then raise_application_error(-20001, 'Найдено несколько подписантов у Потребителя!');       
    end;
    
-- Подписанты со стороны ГП    
    begin
  -- Находим подразделение с хар-кой Гарантирующий поставщик = ДА (GRNT-PST)
  --Логика такая: начиная с ниженего уровня по связи субъект-субъект находим находим нужного нам субъекта
  begin
      select per_ok.per_id
      into v_per_id_grnt
      from
      (
      select all_per.*
      from
      (
      select v_per_id_dog per_id, 0 lev
      from dual
      union all
      select pp.per_id1 per_id, level lev
      from ci_per_per pp
      where 1 = 1
      and pp.per_rel_type_cd = 'PRED-OTD'
      and to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy') between pp.start_dt and nvl(pp.end_dt, to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy'))
      connect by prior pp.per_id1 = pp.per_id2
      start with pp.per_id2 = v_per_id_dog
      ) all_per
      inner join ci_per_char pc on pc.per_id = all_per.per_id and pc.char_type_cd = 'GRNT-PST' and pc.char_val = 'DA'
      order by all_per.lev
      ) per_ok
      where rownum = 1;
  exception when no_data_found then v_per_id_grnt := null;
  end;
  
  -- Если нашли Гарантирующего поставщика, то берем нужные нам хар-ки
    select (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
              from ci_per_char pc
              where pc.per_id = p.per_id
                and pc.char_type_cd = 'SHORT-NM'
                and pc.effdt <= to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy')) GRNT_SHORT_NAME,
           (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
              from ci_per_char pc
              where pc.per_id = p.per_id
                and pc.char_type_cd = 'NAIM-PLN'
                and pc.effdt <= to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy')) GRNT_PLN_NAME,
           (select max(pc.adhoc_char_val) keep (dense_rank last order by pc.effdt)
              from ci_per_char pc
              where pc.per_id = p.per_id
                and pc.char_type_cd = 'D-ADR-1'
                and pc.effdt <= to_date(v_sogl_rec.doc_dat,'dd.mm.yyyy')) GRNT_MAIL_ADDR              
      into  v_sogl_rec.GRNT_SHORT_NAME, v_sogl_rec.GRNT_PLN_NAME, v_sogl_rec.GRNT_MAIL_ADDR
      from ci_per p
      where p.per_id = v_per_id_grnt;
     exception when no_data_found then v_sogl_rec.GRNT_SHORT_NAME := null; v_sogl_rec.GRNT_PLN_NAME := null;  v_sogl_rec.GRNT_MAIL_ADDR:=null;     
    end;
    
    -- Определим адрес доставки корреспонденции
    begin
      select decode(trim(pao.postal),null,null,trim(pao.postal)||', ') || pao.address1
      into v_sogl_rec.addr_korr
      from ci_per_addr_ovrd pao
      where pao.acct_id = v_sogl_rec.acct_id
        and pao.per_id = v_sogl_rec.main_per_id;
    exception when no_data_found then v_sogl_rec.addr_korr := null; 
    end;   
  
   -- Список платежных документов
   begin
   v_sogl_rec.PT_LIST:=get_pt_list(v_sogl_rec.wf_proc_id);
   end;
   -- Список платежных документов по видам реализации 2 и 8   
   begin
    v_buf:='';
    for lst_kpd in (
      select ' № ' || c_n.adhoc_char_val || ' (' ||
                   trim(c_r.char_val) || ') от ' ||
                              c_d.adhoc_char_val || ' г.' pt,
      cm_util.char_val_to_date(c_d.adhoc_char_val) pt_dat
      from
        ci_case c
        join ci_wf_proc_char dpc1 on dpc1.char_val_fk1=c.case_id and dpc1.char_type_cd in ('DZ-DRAFT', 'CZ-DRAFT')
        left join ci_case_char c_n on c_n.case_id = c.case_id and c_n.char_type_cd='DRAFTNUM'
        left join ci_case_char c_d on c_d.case_id = c.case_id and c_d.char_type_cd='OUTGO-DT'
        left join ci_case_char c_r on c_r.case_id = c.case_id and c_r.char_type_cd='RETLTYPE'
      where  dpc1.wf_proc_id= v_sogl_rec.wf_proc_id
      and trim(c_r.char_val) IN (2,8)
      order by 2
       )
        loop
          if length(v_buf)>0  then v_buf:= v_buf||', '; end if;
          v_buf := v_buf || lst_kpd.pt;
        end loop;
    v_sogl_rec.PT_LIST_PAY:=v_buf;
  end;
     -- Список платежных документов по видам реализации 7   
   begin
    v_buf:='';
    for lst_kpd in (
      select ' № ' || c_n.adhoc_char_val || ' (' ||
                   trim(c_r.char_val) || ') от ' ||
                              c_d.adhoc_char_val || ' г.' pt,
      cm_util.char_val_to_date(c_d.adhoc_char_val) pt_dat
      from
        ci_case c
        join ci_wf_proc_char dpc1 on dpc1.char_val_fk1=c.case_id and dpc1.char_type_cd in ('DZ-DRAFT', 'CZ-DRAFT')
        left join ci_case_char c_n on c_n.case_id = c.case_id and c_n.char_type_cd='DRAFTNUM'
        left join ci_case_char c_d on c_d.case_id = c.case_id and c_d.char_type_cd='OUTGO-DT'
        left join ci_case_char c_r on c_r.case_id = c.case_id and c_r.char_type_cd='RETLTYPE'
      where  dpc1.wf_proc_id= v_sogl_rec.wf_proc_id
      and trim(c_r.char_val)=7
      order by 2
       )
        loop
          if length(v_buf)>0  then v_buf:= v_buf||', '; end if;
          v_buf := v_buf || lst_kpd.pt;
        end loop;
    v_sogl_rec.PT_LIST_FINE:=v_buf;
  end;
   
   -- Суммы
   begin
   v_sogl_rec.S_AMT0:= TO_NUMBER(replace(TRIM(cm_dz.get_wfev_chty(v_sogl_rec.wf_proc_id, v_sogl_rec.evt_seq, 'S-AMT0')),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''');
   v_sogl_rec.S_AMT2:= TO_NUMBER(replace(TRIM(cm_dz.get_wfev_chty(v_sogl_rec.wf_proc_id, v_sogl_rec.evt_seq, 'S-AMT2')),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''');
   v_sogl_rec.S_AMT8:= TO_NUMBER(replace(TRIM(cm_dz.get_wfev_chty(v_sogl_rec.wf_proc_id, v_sogl_rec.evt_seq, 'S-AMT8')),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''');
   --
   v_sogl_rec.S_AMT4:= TO_NUMBER(replace(TRIM(cm_dz.get_wfev_chty(v_sogl_rec.wf_proc_id, v_sogl_rec.evt_seq, 'S-AMT4')),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''');
   v_sogl_rec.S_AMT7:= TO_NUMBER(replace(TRIM(cm_dz.get_wfev_chty(v_sogl_rec.wf_proc_id, v_sogl_rec.evt_seq, 'S-AMT7')),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''');
   --=
   v_sogl_rec.RSTR_SUM:= TO_NUMBER(replace(TRIM(cm_dz.get_wfev_chty(v_sogl_rec.wf_proc_id, v_sogl_rec.evt_seq, 'RSTR-SUM')),'.',','), '99999999990D99','NLS_NUMERIC_CHARACTERS='', ''');
   --Определяем подписанат длокумента со стороны ПСК в зависимости от значения RSTR_SUM
   IF (v_sogl_rec.RSTR_SUM < 500000) THEN
   v_sogl_rec.IN_FACE2:='Начальника УРДЗ в СПб Андронова В.Ю., действующего на основании доверенности № 395-053 от 13.11.2017 г.';
   v_sogl_rec.NAME_AGRMNT:='Андронов В.Ю.';
   ELSIF (v_sogl_rec.RSTR_SUM >= 5000000) THEN
   v_sogl_rec.IN_FACE2:='Генерального директора Шаскольского М.А., действующего на основании Устава';
   v_sogl_rec.NAME_AGRMNT:='Шаскольский М.А.';
   ELSE
   v_sogl_rec.IN_FACE2:='Заместителя генерального директора по энергосбытовой деятельности Самойлова С.Е., действующего на основании доверенности № 858-053 от 05.12.2017 г.';
   v_sogl_rec.NAME_AGRMNT:='Самойлов С.Е.';
   END IF;
   end;
               
      pipe row(v_sogl_rec);

  end GET_SOGL_RESTR_HEAD_PSK;

 /*Отчет по работе с ДЗ*/
  FUNCTION REPORT_WORK_WITH_DZ(P_DEPARTMENT VARCHAR2 DEFAULT NULL, P_DATE_BEGIN DATE, P_DATE_END DATE, P_OESK VARCHAR2 DEFAULT NULL, P_USER VARCHAR2)
   RETURN T_REPORT_WORK_WITH_DZ_SET PIPELINED
  IS
    V_RET T_REPORT_WORK_WITH_DZ;   
  BEGIN

    FOR I IN (
          WITH PARAMETER AS (
          SELECT /*+MATERIALIZE*/
            P_DATE_END END_DT
           ,P_DATE_BEGIN START_DT
           ,RPAD(P_USER,8) USER_ID
           ,RPAD(P_DEPARTMENT,30) DEPARTMENTS
           ,RPAD(P_OESK,16) OESK 
          FROM DUAL
          ) 
          , ACCT_PRE AS (
          SELECT DISTINCT A.ACCT_ID 
          FROM CI_DAR_USR U
          INNER JOIN CI_ACC_GRP_DAR AG_DAR ON AG_DAR.DAR_CD=U.DAR_CD
          INNER JOIN CI_ACCT A ON A.ACCESS_GRP_CD=AG_DAR.ACCESS_GRP_CD
          WHERE U.USER_ID=RPAD(P_USER, 8) AND U.EXPIRE_DT>SYSDATE
          ) 
          , ACCT_NOTIF AS (
          SELECT  /*+ MATERIALIZE*/
                A.ACCT_ID,
               (SELECT MAX(AC.ADHOC_CHAR_VAL) keep (dense_rank last order by AC.EFFDT)
                FROM CI_ACCT_CHAR AC
                WHERE AC.ACCT_ID = A.ACCT_ID
                  AND AC.CHAR_TYPE_CD = 'DOG-NUM') DOG_NUM,
               (SELECT MAX(PC.ADHOC_CHAR_VAL) keep (dense_rank last order by PC.EFFDT)
                FROM CI_PER_CHAR PC
                WHERE PC.PER_ID = AP.PER_ID
                  AND PC.CHAR_TYPE_CD = 'SHORT-NM') DOG_NAME,               
               (SELECT MAX(VL.DESCR) keep (dense_rank last order by AC.EFFDT)
                FROM CI_ACCT_CHAR AC, CI_CHAR_VAL_L VL
                WHERE AC.ACCT_ID = A.ACCT_ID
                  AND AC.CHAR_TYPE_CD = 'GRUPEI'
                  AND VL.CHAR_TYPE_CD = AC.CHAR_TYPE_CD
                  AND VL.CHAR_VAL = AC.CHAR_VAL
                  AND VL.LANGUAGE_CD = 'RUS') GRUPEI,        
                DZ_DPRT.CHAR_VAL_FK1 DZ_DPRT,
                KLS_OESK_DESCR.DESCR KLS_OESK,
                TRUNC(WP.CRE_DTTM) CRE_DTTM,
                WP.WF_PROC_ID WF_NOTIF_ID,
                TO_NUMBER(COALESCE(REPLACE(NTF_AMT2.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') NTF_AMT2,
                TO_NUMBER(COALESCE(REPLACE(NTF_AMT8.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') NTF_AMT8,
                TO_NUMBER(COALESCE(REPLACE(NTF_AMT0.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') NTF_AMT0,
                TO_NUMBER(COALESCE(REPLACE(EVT_AMT2.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') EVT_AMT2,
                TO_NUMBER(COALESCE(REPLACE(EVT_AMT8.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') EVT_AMT8,        
                TO_NUMBER(COALESCE(REPLACE(EVT_AMT0.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') EVT_AMT0,               
                CASE WHEN WP.WF_PROC_TMPL_CD <> RPAD('TMPL-EBNTF5',12) THEN 'NTF' ELSE 'EVT' END SUM_EXISTS,
                WP.CR_BY_WF_PROC_ID PROC_ID_MAIN_DZ                  
                                             
          FROM PARAMETER PR
          INNER JOIN CI_WF_PROC WP ON 1 = 1
          INNER JOIN CM_WF_PROC_TMPL_EXP PTE ON PTE.WF_PROC_TMPL_CD = WP.WF_PROC_TMPL_CD AND PTE.CM_WF_PROC_TYPE_CD = 'NOTIFICATION'  
          INNER JOIN CI_WF_PROC_CHAR LIC_S4ET ON LIC_S4ET.WF_PROC_ID = WP.WF_PROC_ID AND LIC_S4ET.CHAR_TYPE_CD = 'LIC-S4ET'
          INNER JOIN ACCT_PRE A ON A.ACCT_ID = LIC_S4ET.CHAR_VAL_FK1
          INNER JOIN CI_WF_PROC_CHAR DZ_DPRT ON DZ_DPRT.WF_PROC_ID = WP.WF_PROC_ID AND DZ_DPRT.CHAR_TYPE_CD = 'DZ-DPRT'
          INNER JOIN CI_ACCT_CHAR KLS_OESK ON KLS_OESK.ACCT_ID = A.ACCT_ID AND KLS_OESK.CHAR_TYPE_CD = 'KLS-OESK'
          INNER JOIN CI_CHAR_VAL_L KLS_OESK_DESCR ON KLS_OESK_DESCR.CHAR_TYPE_CD = KLS_OESK.CHAR_TYPE_CD and KLS_OESK_DESCR.CHAR_VAL = KLS_OESK.CHAR_VAL AND KLS_OESK_DESCR.LANGUAGE_CD = 'RUS'          
          INNER JOIN CI_ACCT_PER AP ON AP.ACCT_ID = A.ACCT_ID AND AP.MAIN_CUST_SW = 'Y'
          LEFT JOIN CI_WF_PROC_CHAR NTF_AMT2 ON NTF_AMT2.WF_PROC_ID = WP.WF_PROC_ID AND NTF_AMT2.CHAR_TYPE_CD = 'NTF-AMT2'
          LEFT JOIN CI_WF_PROC_CHAR NTF_AMT8 ON NTF_AMT8.WF_PROC_ID = WP.WF_PROC_ID AND NTF_AMT8.CHAR_TYPE_CD = 'NTF-AMT8'
          LEFT JOIN CI_WF_PROC_CHAR NTF_AMT0 ON NTF_AMT0.WF_PROC_ID = WP.WF_PROC_ID AND NTF_AMT0.CHAR_TYPE_CD = 'NTF-AMT0'
          LEFT JOIN CI_WF_EVT EVT_EBNTF_20 ON EVT_EBNTF_20.WF_PROC_ID = WP.WF_PROC_ID AND EVT_EBNTF_20.WF_EVT_TYPE_CD = 'EVT-EBNTF-20'
          LEFT JOIN CI_WF_EVT_CHAR EVT_AMT2 ON EVT_AMT2.WF_PROC_ID = WP.WF_PROC_ID AND EVT_AMT2.EVT_SEQ = EVT_EBNTF_20.EVT_SEQ AND EVT_AMT2.CHAR_TYPE_CD = 'NTF-AMT2'
          LEFT JOIN CI_WF_EVT_CHAR EVT_AMT8 ON EVT_AMT8.WF_PROC_ID = WP.WF_PROC_ID AND EVT_AMT8.EVT_SEQ = EVT_EBNTF_20.EVT_SEQ AND EVT_AMT8.CHAR_TYPE_CD = 'NTF-AMT8'
          LEFT JOIN CI_WF_EVT_CHAR EVT_AMT0 ON EVT_AMT0.WF_PROC_ID = WP.WF_PROC_ID AND EVT_AMT0.EVT_SEQ = EVT_EBNTF_20.EVT_SEQ AND EVT_AMT0.CHAR_TYPE_CD = 'NTF-AMT0'          
          WHERE 1 = 1
            AND TRUNC(WP.CRE_DTTM) BETWEEN PR.START_DT AND PR.END_DT
            AND DZ_DPRT.CHAR_VAL_FK1 = PR.DEPARTMENTS
            AND KLS_OESK.EFFDT = (SELECT MAX(AC2.EFFDT) FROM CI_ACCT_CHAR AC2 WHERE AC2.ACCT_ID = KLS_OESK.ACCT_ID AND AC2.CHAR_TYPE_CD = KLS_OESK.CHAR_TYPE_CD)
            AND (TRIM(PR.OESK) IS NULL OR KLS_OESK.CHAR_VAL = PR.OESK)
            AND EXISTS (SELECT 'X'
                        FROM CI_WF_PROC WP_LIM
                        INNER JOIN CM_WF_PROC_TMPL_EXP PTE ON PTE.WF_PROC_TMPL_CD = WP_LIM.WF_PROC_TMPL_CD AND PTE.CM_WF_PROC_TYPE_CD = 'LIMITATION'                 
                        WHERE WP_LIM.CR_BY_WF_PROC_ID = WP.WF_PROC_ID
                       )
          )
          , ACCT_NOTIF_DISTINCT AS (
          SELECT /*+ MATERIALIZE*/
                ACCT.ACCT_ID,
                ACCT.DOG_NUM,
                ACCT.DOG_NAME,
                ACCT.DZ_DPRT,
                ACCT.KLS_OESK,
                ACCT.GRUPEI,
                SUM(CASE WHEN ACCT.SUM_EXISTS = 'NTF' THEN ACCT.NTF_AMT2+ACCT.NTF_AMT8+ACCT.NTF_AMT0 ELSE ACCT.EVT_AMT2+ACCT.EVT_AMT8+ACCT.EVT_AMT0 END) NTF_AMT,
                COUNT(DISTINCT ACCT.WF_NOTIF_ID) COUNT_PROC_NOTIF
          FROM ACCT_NOTIF ACCT
          GROUP BY ACCT.ACCT_ID,
                ACCT.DOG_NUM,
                ACCT.DOG_NAME,
                ACCT.DZ_DPRT,
                ACCT.KLS_OESK,
                ACCT.GRUPEI
          )
         /* , ACCT_MAIN_DZ_DISTINCT AS (
          SELECT DISTINCT
                 N.ACCT_ID,
                 N.PROC_ID_MAIN_DZ
          FROM ACCT_NOTIF N
          )
          , ACCT_MAIN_DZ AS (
          SELECT \*+ MATERIALIZE*\
                 ND.ACCT_ID,
                 ND.PROC_ID_MAIN_DZ,
                 TO_NUMBER(COALESCE(REPLACE(NTF_AMT2.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') +
                 TO_NUMBER(COALESCE(REPLACE(NTF_AMT8.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') +      
                 TO_NUMBER(COALESCE(REPLACE(NTF_AMT0.ADHOC_CHAR_VAL, ',', '.'), '0.00'), '9999999990.00') NTF_AMT_MAIN_DZ         
          FROM ACCT_MAIN_DZ_DISTINCT ND
          LEFT JOIN CI_WF_PROC_CHAR NTF_AMT2 ON NTF_AMT2.WF_PROC_ID = ND.PROC_ID_MAIN_DZ AND NTF_AMT2.CHAR_TYPE_CD = 'NZ-AMT2'
          LEFT JOIN CI_WF_PROC_CHAR NTF_AMT8 ON NTF_AMT8.WF_PROC_ID = ND.PROC_ID_MAIN_DZ AND NTF_AMT8.CHAR_TYPE_CD = 'NZ-AMT8'
          LEFT JOIN CI_WF_PROC_CHAR NTF_AMT0 ON NTF_AMT0.WF_PROC_ID = ND.PROC_ID_MAIN_DZ AND NTF_AMT0.CHAR_TYPE_CD = 'NZ-AMT0'
          )  */                  
          , ACCT_PAY_NO_EXISTS_LIM AS (
          SELECT  /*+ MATERIALIZE*/
                AN.ACCT_ID,
               -SUM(CASE WHEN (NOT GFT_PAY.PARENT_ID IN ('REFOUND','REFOUNDP') AND NOT PAY_PROP_PAY.CM_ACCT_PAY_MATCH_EVT_PROP_ID IS NULL  AND NOT(RP_PAY.CM_RETAIL_TYPE='0' AND GFT_PAY.PARENT_ID='TRANSF')) THEN AE_PAY.CUR_AMT ELSE 0 END) SUM_PAY
          FROM ACCT_NOTIF AN
          INNER JOIN CI_WF_PROC_CHAR DAT_LIM ON DAT_LIM.WF_PROC_ID = AN.WF_NOTIF_ID AND DAT_LIM.CHAR_TYPE_CD = 'DAT-LIM'          
          INNER JOIN CI_WF_PROC_CHAR DZ_DRAFT ON DZ_DRAFT.WF_PROC_ID = AN.WF_NOTIF_ID AND DZ_DRAFT.CHAR_TYPE_CD = 'DZ-DRAFT'
          INNER JOIN CI_CASE DRAFT ON DRAFT.CASE_ID = DZ_DRAFT.CHAR_VAL_FK1 AND DRAFT.CASE_TYPE_CD = 'DRAFT'
          INNER JOIN CI_CASE_CHAR ME_ID ON ME_ID.CASE_ID = DRAFT.CASE_ID AND ME_ID.CHAR_TYPE_CD = 'ME-ID'
          INNER JOIN CI_FT GFT_PAY ON GFT_PAY.MATCH_EVT_ID = ME_ID.CHAR_VAL_FK1
          INNER JOIN CM_ACCOUNT_ENTRY AE_PAY ON AE_PAY.CM_GENERATIVE_FT_ID = GFT_PAY.FT_ID
          LEFT  JOIN CM_ACCT_PAY_MATCH_EVT_PROP PAY_PROP_PAY ON PAY_PROP_PAY.CM_ACCT_PAY_MATCH_EVT_PROP_ID=AE_PAY.CM_ACCT_PAY_MATCH_EVT_PROP_ID
          INNER JOIN CM_RATE_PROP RP_PAY ON RP_PAY.CM_RATE_PROP_ID = AE_PAY.CM_RATE_PROP_ID AND RP_PAY.CM_RETAIL_TYPE IN (RPAD('2',16), RPAD('8',16), RPAD('0',16))
          WHERE 1 = 1
            AND EXISTS (SELECT 'X'
                        FROM CI_WF_PROC WP_LIM
                        INNER JOIN CM_WF_PROC_TMPL_EXP PTE ON PTE.WF_PROC_TMPL_CD = WP_LIM.WF_PROC_TMPL_CD AND PTE.CM_WF_PROC_TYPE_CD = 'LIMITATION'
                        LEFT JOIN CI_WF_PROC_CHAR DAT_LIMF ON DAT_LIMF.WF_PROC_ID = WP_LIM.WF_PROC_ID AND DAT_LIMF.CHAR_TYPE_CD = 'DAT-LIMF'
                        LEFT JOIN CI_WF_PROC_CHAR DATLIMFF ON DATLIMFF.WF_PROC_ID = WP_LIM.WF_PROC_ID AND DATLIMFF.CHAR_TYPE_CD = 'DATLIMFF'                 
                        WHERE WP_LIM.CR_BY_WF_PROC_ID = AN.WF_NOTIF_ID
                          AND (DAT_LIMF.ADHOC_CHAR_VAL IS NULL OR DATLIMFF.ADHOC_CHAR_VAL IS NULL)
                       )
            AND NOT AE_PAY.FREEZE_DTTM IS NULL                        
            --AND PAY_PROP_PAY.PAY_DT < TO_DATE(DAT_LIM.ADHOC_CHAR_VAL,'DD.MM.YYYY')   
            AND PAY_PROP_PAY.PAY_DT BETWEEN AN.CRE_DTTM AND TO_DATE(DAT_LIM.ADHOC_CHAR_VAL,'DD.MM.YYYY')                                                     
          GROUP BY AN.ACCT_ID
          )
          , ACCT_PAY_EXISTS_LIM AS (
          SELECT  /*+ MATERIALIZE*/
                AN.ACCT_ID,
               -SUM(CASE WHEN (NOT GFT_PAY.PARENT_ID IN ('REFOUND','REFOUNDP') AND NOT PAY_PROP_PAY.CM_ACCT_PAY_MATCH_EVT_PROP_ID IS NULL  AND NOT(RP_PAY.CM_RETAIL_TYPE='0' AND GFT_PAY.PARENT_ID='TRANSF')) THEN AE_PAY.CUR_AMT ELSE 0 END) SUM_PAY
          FROM ACCT_NOTIF AN
          INNER JOIN CI_WF_PROC_CHAR DZ_DRAFT ON DZ_DRAFT.WF_PROC_ID = AN.WF_NOTIF_ID AND DZ_DRAFT.CHAR_TYPE_CD = 'DZ-DRAFT'
          INNER JOIN CI_CASE DRAFT ON DRAFT.CASE_ID = DZ_DRAFT.CHAR_VAL_FK1 AND DRAFT.CASE_TYPE_CD = 'DRAFT'
          INNER JOIN CI_CASE_CHAR ME_ID ON ME_ID.CASE_ID = DRAFT.CASE_ID AND ME_ID.CHAR_TYPE_CD = 'ME-ID'
          INNER JOIN CI_FT GFT_PAY ON GFT_PAY.MATCH_EVT_ID = ME_ID.CHAR_VAL_FK1
          INNER JOIN CM_ACCOUNT_ENTRY AE_PAY ON AE_PAY.CM_GENERATIVE_FT_ID = GFT_PAY.FT_ID
          LEFT  JOIN CM_ACCT_PAY_MATCH_EVT_PROP PAY_PROP_PAY ON PAY_PROP_PAY.CM_ACCT_PAY_MATCH_EVT_PROP_ID=AE_PAY.CM_ACCT_PAY_MATCH_EVT_PROP_ID
          INNER JOIN CM_RATE_PROP RP_PAY ON RP_PAY.CM_RATE_PROP_ID = AE_PAY.CM_RATE_PROP_ID AND RP_PAY.CM_RETAIL_TYPE IN (RPAD('2',16), RPAD('8',16), RPAD('0',16))
          WHERE 1 = 1
            AND EXISTS (SELECT 'X'
                        FROM CI_WF_PROC WP_LIM
                        INNER JOIN CM_WF_PROC_TMPL_EXP PTE ON PTE.WF_PROC_TMPL_CD = WP_LIM.WF_PROC_TMPL_CD AND PTE.CM_WF_PROC_TYPE_CD = 'LIMITATION'
                        LEFT JOIN CI_WF_PROC_CHAR DAT_LIMF ON DAT_LIMF.WF_PROC_ID = WP_LIM.WF_PROC_ID AND DAT_LIMF.CHAR_TYPE_CD = 'DAT-LIMF'
                        LEFT JOIN CI_WF_PROC_CHAR DATLIMFF ON DATLIMFF.WF_PROC_ID = WP_LIM.WF_PROC_ID AND DATLIMFF.CHAR_TYPE_CD = 'DATLIMFF'                 
                        WHERE WP_LIM.CR_BY_WF_PROC_ID = AN.WF_NOTIF_ID
                          AND (DAT_LIMF.ADHOC_CHAR_VAL IS NOT NULL OR DATLIMFF.ADHOC_CHAR_VAL IS NOT NULL)
                       )
            AND NOT AE_PAY.FREEZE_DTTM IS NULL                       
            AND PAY_PROP_PAY.PAY_DT > 
                       (SELECT LEAST(MAX(NVL(TO_DATE(DAT_LIMF.ADHOC_CHAR_VAL,'DD.MM.YYYY'),SYSDATE)), MAX(NVL(TO_DATE(DATLIMFF.ADHOC_CHAR_VAL,'DD.MM.YYYY'),SYSDATE)))
                        FROM CI_WF_PROC WP_LIM
                        INNER JOIN CM_WF_PROC_TMPL_EXP PTE ON PTE.WF_PROC_TMPL_CD = WP_LIM.WF_PROC_TMPL_CD AND PTE.CM_WF_PROC_TYPE_CD = 'LIMITATION'
                        LEFT JOIN CI_WF_PROC_CHAR DAT_LIMF ON DAT_LIMF.WF_PROC_ID = WP_LIM.WF_PROC_ID AND DAT_LIMF.CHAR_TYPE_CD = 'DAT-LIMF'
                        LEFT JOIN CI_WF_PROC_CHAR DATLIMFF ON DATLIMFF.WF_PROC_ID = WP_LIM.WF_PROC_ID AND DATLIMFF.CHAR_TYPE_CD = 'DATLIMFF'                 
                        WHERE WP_LIM.CR_BY_WF_PROC_ID = AN.WF_NOTIF_ID
                          AND (DAT_LIMF.ADHOC_CHAR_VAL IS NOT NULL OR DATLIMFF.ADHOC_CHAR_VAL IS NOT NULL)
                       )                        
          GROUP BY AN.ACCT_ID
          )
          , ACCT_LIM AS (
          SELECT  /*+ MATERIALIZE*/
                AN.ACCT_ID,
                AN.WF_NOTIF_ID,
                WP_LIM.WF_PROC_ID WF_LIM_ID,
               (SELECT MAX(PC.ADHOC_CHAR_VAL) keep (dense_rank last order by PC.EFFDT)
                FROM CI_PREM_CHAR PC
                WHERE PC.PREM_ID = OB_OBSL.CHAR_VAL_FK1
                  AND PC.CHAR_TYPE_CD = 'PREM_COD') EO_CODE,
               (SELECT MAX(PC.ADHOC_CHAR_VAL) keep (dense_rank last order by PC.EFFDT)
                FROM CI_PREM_CHAR PC
                WHERE PC.PREM_ID = POINTDLV.CHAR_VAL_FK1
                  AND PC.CHAR_TYPE_CD = 'CP-NUM') CP_NUM,
                CASE WHEN DATLIM.CHAR_TYPE_CD = 'DATLIMFA' THEN DATLIM.ADHOC_CHAR_VAL END DATLIMFA, 
                CASE WHEN DATLIM.CHAR_TYPE_CD = 'DATLIMFT' THEN DATLIM.ADHOC_CHAR_VAL END DATLIMFT, 
                CASE WHEN DATLIM.CHAR_TYPE_CD = 'DAT-LIMF' THEN DATLIM.ADHOC_CHAR_VAL END DAT_LIMF,     
                CASE WHEN DATLIM.CHAR_TYPE_CD = 'DATLIMFF' THEN DATLIM.ADHOC_CHAR_VAL END DATLIMFF,
                DATLIM.ADHOC_CHAR_VAL DAT_LIM,                            
                STATELIM.CHAR_VAL STATELIM,
                STATUS.CHAR_VAL STATUS,
                CASE WHEN DAT_LIMF.ADHOC_CHAR_VAL IS NOT NULL OR DATLIMFF.ADHOC_CHAR_VAL IS NOT NULL THEN 1 ELSE 0 END COUNT_PROC_LIM,
                CASE WHEN /*STATUS.CHAR_VAL = RPAD('1',16) AND*/ STATELIM.CHAR_VAL = RPAD('50',16) THEN 1 ELSE 0 END COUNT_ACTIV_PROC_LIM
                --CASE WHEN DAT_LIMF.ADHOC_CHAR_VAL IS NOT NULL OR DATLIMFF.ADHOC_CHAR_VAL IS NOT NULL THEN AN.NTF_AMT2 + AN.NTF_AMT8 ELSE 0 END SUM_NTF_PROC_LIM,
                --CASE WHEN STATUS.CHAR_VAL = RPAD('1',16) AND STATELIM.CHAR_VAL = RPAD('50',16) THEN AN.NTF_AMT2 + AN.NTF_AMT8 ELSE 0 END SUM_NTF_ACTIV_PROC_LIM             
          FROM ACCT_NOTIF AN
          INNER JOIN CI_WF_PROC WP_LIM ON WP_LIM.CR_BY_WF_PROC_ID = AN.WF_NOTIF_ID
          INNER JOIN CM_WF_PROC_TMPL_EXP PTE ON PTE.WF_PROC_TMPL_CD = WP_LIM.WF_PROC_TMPL_CD AND PTE.CM_WF_PROC_TYPE_CD = 'LIMITATION'
          LEFT JOIN CI_WF_PROC_CHAR OB_OBSL ON OB_OBSL.WF_PROC_ID = WP_LIM.WF_PROC_ID AND OB_OBSL.CHAR_TYPE_CD = 'OB-OBSL'
          LEFT JOIN CI_WF_PROC_CHAR POINTDLV ON POINTDLV.WF_PROC_ID = WP_LIM.WF_PROC_ID AND POINTDLV.CHAR_TYPE_CD = 'POINTDLV'
          LEFT JOIN CI_WF_PROC_CHAR DATLIM ON DATLIM.WF_PROC_ID = WP_LIM.WF_PROC_ID AND DATLIM.CHAR_TYPE_CD IN ('DATLIMFA', 'DATLIMFT', 'DAT-LIMF', 'DATLIMFF')
          LEFT JOIN CI_WF_PROC_CHAR STATUS ON STATUS.WF_PROC_ID = WP_LIM.WF_PROC_ID AND STATUS.CHAR_TYPE_CD = 'STATUS'   
          LEFT JOIN CI_WF_PROC_CHAR STATELIM ON STATELIM.WF_PROC_ID = WP_LIM.WF_PROC_ID AND STATELIM.CHAR_TYPE_CD = 'STATELIM'  
          LEFT JOIN CI_WF_PROC_CHAR DAT_LIMF ON DAT_LIMF.WF_PROC_ID = WP_LIM.WF_PROC_ID AND DAT_LIMF.CHAR_TYPE_CD = 'DAT-LIMF'
          LEFT JOIN CI_WF_PROC_CHAR DATLIMFF ON DATLIMFF.WF_PROC_ID = WP_LIM.WF_PROC_ID AND DATLIMFF.CHAR_TYPE_CD = 'DATLIMFF'              
          WHERE 1 = 1
          )
          SELECT
                ROW_NUMBER() OVER(PARTITION BY ACCT.DOG_NUM ORDER BY ACCT.DOG_NUM) RN,
                COUNT (1) OVER(PARTITION BY ACCT.DOG_NUM) RN_TOTAL,              
                ACCT.ACCT_ID,
                ACCT.DOG_NUM,
                ACCT.DOG_NAME,
                ACCT.DZ_DPRT,
                ACCT.KLS_OESK,
                ACCT.GRUPEI,
                ACCT.NTF_AMT,
                ACCT.COUNT_PROC_NOTIF, 
                --NVL(PAY_NO_EXISTS.SUM_PAY,0) SUM_PAY_NO_EXISTS,
                --NVL(PAY_EXISTS.SUM_PAY,0) SUM_PAY_EXISTS,
      NVL((SELECT NVL(PAY_NO_EXISTS.SUM_PAY,0) FROM ACCT_PAY_NO_EXISTS_LIM PAY_NO_EXISTS WHERE PAY_NO_EXISTS.ACCT_ID = ACCT.ACCT_ID),0) SUM_PAY_NO_EXISTS, 
      NVL((SELECT NVL(PAY_EXISTS.SUM_PAY,0) FROM ACCT_PAY_EXISTS_LIM PAY_EXISTS WHERE PAY_EXISTS.ACCT_ID = ACCT.ACCT_ID),0) SUM_PAY_EXISTS,                       
                AL.WF_NOTIF_ID,
                AL.WF_LIM_ID,
                AL.EO_CODE,
                AL.CP_NUM,
                AL.DATLIMFA,
                AL.DATLIMFT,
                AL.DATLIMFF,
                AL.DAT_LIMF,
                AL.DAT_LIM,
                CASE WHEN AL.DATLIMFA IS NOT NULL THEN 'до уровня АБ'
                     WHEN AL.DATLIMFT IS NOT NULL THEN 'до уровня ТБ'
                     WHEN AL.DATLIMFF IS NOT NULL THEN 'полное'               
                 END TYPE_LIM,                 
                AL.STATELIM,
                AL.STATUS,
                CASE WHEN TRIM(AL.STATELIM) = '50' THEN AL.EO_CODE END EO_CODE_CUR_DATE,
                CASE WHEN TRIM(AL.STATELIM) = '50' THEN AL.CP_NUM END CP_NUM_CUR_DATE,
                CASE WHEN TRIM(AL.STATELIM) = '50' THEN AL.DAT_LIM END DAT_LIM_CUR_DATE,                 
                CASE WHEN TRIM(AL.STATELIM) = '50' THEN 
                  CASE WHEN AL.DATLIMFA IS NOT NULL THEN 'до уровня АБ'
                       WHEN AL.DATLIMFT IS NOT NULL THEN 'до уровня ТБ'
                       WHEN AL.DATLIMFF IS NOT NULL THEN 'полное'               
                   END     
                 END TYPE_LIM_CUR_DATE,  
                AL.COUNT_PROC_LIM,
                AL.COUNT_ACTIV_PROC_LIM,
                /*CASE WHEN AL.COUNT_PROC_LIM >= 1 THEN*/ NVL(ACCT.NTF_AMT,0)-/*NVL(PAY_NO_EXISTS.SUM_PAY,0)*/ 
                  NVL((SELECT NVL(PAY_NO_EXISTS.SUM_PAY,0) FROM ACCT_PAY_NO_EXISTS_LIM PAY_NO_EXISTS WHERE PAY_NO_EXISTS.ACCT_ID = ACCT.ACCT_ID),0)
                  /*ELSE 0 END*/ SUM_NTF_PROC_LIM,
                --MD.NTF_AMT_MAIN_DZ SUM_NTF_ACTIV_PROC_LIM
                CASE WHEN AL.COUNT_PROC_LIM >= 1 AND ACCT.NTF_AMT-/*NVL(PAY_NO_EXISTS.SUM_PAY,0)*/ 
                  NVL((SELECT NVL(PAY_NO_EXISTS.SUM_PAY,0) FROM ACCT_PAY_NO_EXISTS_LIM PAY_NO_EXISTS WHERE PAY_NO_EXISTS.ACCT_ID = ACCT.ACCT_ID),0) > 0 THEN
                  
ACCT.NTF_AMT-/*NVL(PAY_NO_EXISTS.SUM_PAY,0)*/ 
                  NVL((SELECT NVL(PAY_NO_EXISTS.SUM_PAY,0) FROM ACCT_PAY_NO_EXISTS_LIM PAY_NO_EXISTS WHERE PAY_NO_EXISTS.ACCT_ID = ACCT.ACCT_ID),0) -                   
NVL((SELECT NVL(PAY_EXISTS.SUM_PAY,0) FROM ACCT_PAY_EXISTS_LIM PAY_EXISTS WHERE PAY_EXISTS.ACCT_ID = ACCT.ACCT_ID),0) END 
 SUM_NTF_ACTIV_PROC_LIM                    
                
          FROM ACCT_NOTIF_DISTINCT ACCT
          INNER JOIN ACCT_LIM AL ON AL.ACCT_ID = ACCT.ACCT_ID
          ---INNER JOIN ACCT_MAIN_DZ MD ON MD.ACCT_ID = ACCT.ACCT_ID          
          --LEFT JOIN ACCT_PAY_NO_EXISTS_LIM PAY_NO_EXISTS ON PAY_NO_EXISTS.ACCT_ID = ACCT.ACCT_ID
          --LEFT JOIN ACCT_PAY_EXISTS_LIM PAY_EXISTS ON PAY_EXISTS.ACCT_ID = ACCT.ACCT_ID
          WHERE 1 = 1
          ORDER BY ACCT.DOG_NUM, AL.EO_CODE      
        ) LOOP
        
     IF I.RN = 1 THEN      
        
       V_RET.ACCT_ID := I.ACCT_ID;
       V_RET.DOG_NUM := I.DOG_NUM;
       V_RET.DOG_NAME := I.DOG_NAME;
       V_RET.DOG_KLS_OESK := I.KLS_OESK; 
       V_RET.DOG_GRUPEI := I.GRUPEI;
       V_RET.WF_NOTIF_ID := I.WF_NOTIF_ID;
       V_RET.WF_LIM_ID := I.WF_LIM_ID;
       V_RET.EO_CODE := I.EO_CODE;
       V_RET.CP_NUM := I.CP_NUM;
       V_RET.DATLIMFA := I.DATLIMFA;      
       V_RET.DATLIMFT := I.DATLIMFT;
       V_RET.DATLIMFF := I.DATLIMFF;
       V_RET.DAT_LIMF := I.DAT_LIMF;
       V_RET.DAT_LIM := I.DAT_LIM;
       V_RET.TYPE_LIM := I.TYPE_LIM;
       V_RET.STATELIM := I.STATELIM;
       V_RET.STATUS := I.STATUS;
       V_RET.EO_CODE_CUR_DATE := I.EO_CODE_CUR_DATE;
       V_RET.CP_NUM_CUR_DATE := I.CP_NUM_CUR_DATE;
       V_RET.DAT_LIM_CUR_DATE := I.DAT_LIM_CUR_DATE;
       V_RET.TYPE_LIM_CUR_DATE := I.TYPE_LIM_CUR_DATE;
        
       V_RET.NTF_AMT := I.NTF_AMT;
       V_RET.COUNT_PROC_NOTIF := I.COUNT_PROC_NOTIF;
       V_RET.SUM_PAY_NO_EXISTS := I.SUM_PAY_NO_EXISTS;
       V_RET.SUM_PAY_EXISTS := I.SUM_PAY_EXISTS;
       V_RET.COUNT_PROC_LIM := I.COUNT_PROC_LIM;
       V_RET.COUNT_ACTIV_PROC_LIM := I.COUNT_ACTIV_PROC_LIM;
       V_RET.SUM_NTF_PROC_LIM := I.SUM_NTF_PROC_LIM;
       V_RET.SUM_NTF_ACTIV_PROC_LIM := I.SUM_NTF_ACTIV_PROC_LIM;   

      /* V_RET.COUNT_PROC_LIM := NULL; 
       V_RET.COUNT_ACTIV_PROC_LIM := NULL; 
       V_RET.SUM_NTF_PROC_LIM := NULL;
       V_RET.SUM_NTF_ACTIV_PROC_LIM := NULL; */     
      
      ELSE
        
       V_RET.ACCT_ID := I.ACCT_ID;
       V_RET.DOG_NUM := NULL;
       V_RET.DOG_NAME := NULL;
       V_RET.DOG_KLS_OESK := I.KLS_OESK; 
       V_RET.DOG_GRUPEI := NULL;
       V_RET.WF_NOTIF_ID := I.WF_NOTIF_ID;
       V_RET.WF_LIM_ID := I.WF_LIM_ID;
       V_RET.EO_CODE := I.EO_CODE;
       V_RET.CP_NUM := I.CP_NUM;
       V_RET.DATLIMFA := I.DATLIMFA;      
       V_RET.DATLIMFT := I.DATLIMFT;
       V_RET.DATLIMFF := I.DATLIMFF;
       V_RET.DAT_LIMF := I.DAT_LIMF;
       V_RET.DAT_LIM := I.DAT_LIM;
       V_RET.TYPE_LIM := I.TYPE_LIM;
       V_RET.STATELIM := I.STATELIM;
       V_RET.STATUS := I.STATUS;
       V_RET.EO_CODE_CUR_DATE := I.EO_CODE_CUR_DATE;
       V_RET.CP_NUM_CUR_DATE := I.CP_NUM_CUR_DATE;
       V_RET.DAT_LIM_CUR_DATE := I.DAT_LIM_CUR_DATE;
       V_RET.TYPE_LIM_CUR_DATE := I.TYPE_LIM_CUR_DATE;
        
       V_RET.NTF_AMT := NULL;
       V_RET.COUNT_PROC_NOTIF := NULL;
       V_RET.SUM_PAY_NO_EXISTS := NULL;
       V_RET.SUM_PAY_EXISTS := NULL;
       V_RET.COUNT_PROC_LIM := NULL;
       V_RET.COUNT_ACTIV_PROC_LIM := NULL;
       V_RET.SUM_NTF_PROC_LIM := NULL;
       V_RET.SUM_NTF_ACTIV_PROC_LIM := NULL;           
      
      END IF;                                                 
            
       PIPE ROW(V_RET);   
             
    END LOOP;    
        
   END REPORT_WORK_WITH_DZ;    

/*Отчет ДЗ на дату*/
FUNCTION REPORT_DZ_ON_DATE(P_USER VARCHAR2, P_DATE DATE, P_BILLING_DEPARTMENTS VARCHAR2 DEFAULT NULL, P_DOGOVOR_DEPARTMENTS VARCHAR2 DEFAULT NULL, P_DOG_NUM VARCHAR2 DEFAULT NULL,
                           P_OESK VARCHAR2 DEFAULT NULL, P_SUM_DZ NUMBER)
 RETURN T_REPORT_DZ_ON_DATE_SET PIPELINED
 
IS
    V_RET T_REPORT_DZ_ON_DATE;   
 
BEGIN
  
  IF P_DATE IS NOT NULL THEN
    
    FOR I IN (
          WITH PARAMETER AS (
          SELECT /*+MATERIALIZE*/
            TRUNC(P_DATE) DATE_REPORT
           ,TRUNC(P_DATE,'MM') DATE_CUR_MONTH_01
           ,TRUNC(TRUNC(P_DATE,'MM')-1,'MM') DATE_PREV_MONTH_01 
           ,TRUNC(P_DATE,'MM')-1 DATE_PREV_MONTH_30
           ,TRUNC(P_DATE,'YYYY') DATE_CUR_YEAR_01
           ,TO_CHAR(P_DATE,'YYYY-MM') PERIOD_CUR
           ,TO_CHAR(TRUNC(P_DATE,'MM')-1,'YYYY-MM') PERIOD_PREV
           ,TO_CHAR(LAST_DAY(P_DATE)+1,'YYYY-MM') PERIOD_NEXT           
           ,RPAD(P_USER,8) USER_ID
           ,P_BILLING_DEPARTMENTS BILLING_DEPARTMENTS            
           ,P_DOGOVOR_DEPARTMENTS DOGOVOR_DEPARTMENTS
           ,P_OESK OESK
           ,P_DOG_NUM DOG_NUM
           ,P_SUM_DZ SUM_DZ 
          FROM DUAL
          )
          , BILLING_DEPARTMENTS AS(
          SELECT /*+MATERIALIZE*/CAST(Q.COLUMN_VALUE AS CHAR(10)) PER_ID FROM TABLE(CM_UTIL.VARCHAR_TO_ROWS(P_BILLING_DEPARTMENTS,',')) Q WHERE Q.COLUMN_VALUE IS NOT NULL
          )
          , DOGOVOR_DEPARTMENTS AS(
          SELECT /*+MATERIALIZE*/CAST(Q.COLUMN_VALUE AS CHAR(10)) PER_ID FROM TABLE(CM_UTIL.VARCHAR_TO_ROWS(P_DOGOVOR_DEPARTMENTS,',')) Q WHERE Q.COLUMN_VALUE IS NOT NULL
          )          
, ACCT_LITE AS (
SELECT A.ACCT_ID
      ,A.ACCESS_GRP_CD        
      ,AGL.DESCR ACCESS_GRP_DESCR
      ,TO_CHAR(A.SETUP_DT,'DD.MM.YYYY') DOG_DATE      
      ,MAX(DOG_NUM.ADHOC_CHAR_VAL) KEEP (DENSE_RANK LAST ORDER BY DOG_NUM.EFFDT) DOG_NUM
      ,MAX(KLS_OESK.CHAR_VAL) KEEP (DENSE_RANK LAST ORDER BY KLS_OESK.EFFDT) KLS_OESK      
FROM CI_DAR_USR U
INNER JOIN CI_ACC_GRP_DAR AG_DAR ON AG_DAR.DAR_CD = U.DAR_CD
INNER JOIN CI_ACCT A ON A.ACCESS_GRP_CD = AG_DAR.ACCESS_GRP_CD
INNER JOIN CI_ACC_GRP_L AGL ON AGL.ACCESS_GRP_CD = A.ACCESS_GRP_CD AND AGL.LANGUAGE_CD = 'RUS'
INNER JOIN PARAMETER PR ON 1 = 1
INNER JOIN CI_ACCT_PER BILLING_DEPARTMENT ON BILLING_DEPARTMENT.ACCT_ID = A.ACCT_ID AND BILLING_DEPARTMENT.ACCT_REL_TYPE_CD='BILLING '
INNER JOIN CI_ACCT_PER DOGOVOR_DEPARTMENT ON DOGOVOR_DEPARTMENT.ACCT_ID = A.ACCT_ID AND DOGOVOR_DEPARTMENT.ACCT_REL_TYPE_CD='DOGOVOR'
INNER JOIN CI_ACCT_CHAR KLS_OESK ON KLS_OESK.ACCT_ID=A.ACCT_ID AND KLS_OESK.CHAR_TYPE_CD='KLS-OESK'
INNER JOIN CI_ACCT_CHAR DOG_NUM ON DOG_NUM.ACCT_ID=A.ACCT_ID AND DOG_NUM.CHAR_TYPE_CD='DOG-NUM'

WHERE U.USER_ID = RPAD(PR.USER_ID,8) AND U.EXPIRE_DT > SYSDATE
AND (PR.BILLING_DEPARTMENTS IS NULL OR BILLING_DEPARTMENT.PER_ID IN (SELECT PER_ID FROM BILLING_DEPARTMENTS))
AND (PR.DOGOVOR_DEPARTMENTS IS NULL OR DOGOVOR_DEPARTMENT.PER_ID IN (SELECT PER_ID FROM DOGOVOR_DEPARTMENTS))
HAVING 1 = 1
AND (P_OESK IS NULL OR RPAD(P_OESK,16) = MAX(KLS_OESK.CHAR_VAL) KEEP (DENSE_RANK LAST ORDER BY KLS_OESK.EFFDT))
AND (P_DOG_NUM IS NULL OR P_DOG_NUM = MAX(DOG_NUM.ADHOC_CHAR_VAL) KEEP (DENSE_RANK LAST ORDER BY DOG_NUM.EFFDT))
GROUP BY A.ACCT_ID
        ,A.ACCESS_GRP_CD
        ,AGL.DESCR
        ,A.SETUP_DT             
)
, ACCT_FILTERED AS (
SELECT /*+MATERIALIZE*/
        A.ACCT_ID
       ,A.ACCESS_GRP_CD
       ,A.ACCESS_GRP_DESCR
       ,A.DOG_DATE
       ,A.DOG_NUM
       ,(SELECT KLS_OESK_DESCR.DESCR 
         FROM CI_CHAR_VAL_L KLS_OESK_DESCR 
         WHERE KLS_OESK_DESCR.CHAR_TYPE_CD = 'KLS-OESK' 
         AND KLS_OESK_DESCR.CHAR_VAL = A.KLS_OESK 
         AND KLS_OESK_DESCR.LANGUAGE_CD = 'RUS') KLS_OESK
       ,MAX(DOG_NAME.ADHOC_CHAR_VAL) KEEP (DENSE_RANK LAST ORDER BY DOG_NAME.EFFDT) DOG_NAME       
       ,MAX(ACCTSTAT.CHAR_VAL) KEEP (DENSE_RANK LAST ORDER BY ACCTSTAT.EFFDT) DOG_STATUS
       ,MAX(COMM.CHAR_VAL) KEEP (DENSE_RANK LAST ORDER BY COMM.EFFDT) DOG_COMM                  
       ,INN.PER_ID_NBR INN
       ,MAX((SELECT COUNT(*)
        FROM CM_EO_ACCOUNT_LINKAGE EAL
        WHERE EAL.ACCT_ID = A.ACCT_ID
        AND PR.DATE_REPORT BETWEEN EAL.START_DT AND NVL(EAL.END_DT, PR.DATE_REPORT)
        )) EO_COUNT
      ,CASE WHEN CM_DZ_REP_UN_BILLING.GET_ROWER_RESERV('AB', A.ACCT_ID) IS NOT NULL THEN 'Да' ELSE 'Нет' END POWER_AB
      ,CASE WHEN CM_DZ_REP_UN_BILLING.GET_ROWER_RESERV('TB', A.ACCT_ID) IS NOT NULL THEN 'Да' ELSE 'Нет' END POWER_TB   
      ,CM_DZ_REP_UTILS.GET_MM_CAT_BY_ACCT(A.ACCT_ID, NULL, P_DATE) MM_CAT      
      ,CM_DZ_REP_UTILS.GET_SETI_BY_ACCT(A.ACCT_ID, NULL, P_DATE) SETI_NAME   
      ,CM_DZ_REP_UTILS.GET_CP_MAX_P_BY_ACCT(A.ACCT_ID, NULL, P_DATE) CP_MAX_P
      ,MAX(DENZADOL.ADHOC_CHAR_VAL ) KEEP (DENSE_RANK LAST ORDER BY DENZADOL.EFFDT) DENZADOL  
      ,MAX(STV_PENI.ADHOC_CHAR_VAL ) KEEP (DENSE_RANK LAST ORDER BY STV_PENI.EFFDT) STV_PENI                                
      ,DZ_MAIN.WF_PROC_ID MAIN_PROC_ID
      ,CM_DZ.get_char_val('NOTICETP', CM_DZ.get_wfpr_chty(DZ_MAIN.WF_PROC_ID, 'NOTICETP')) NOTICETP
      ,(SELECT W2.DESCR 
        FROM CI_WF_PROC W1 
        JOIN CI_WF_PROC_TMPL_L W2 ON W1.WF_PROC_TMPL_CD=W2.WF_PROC_TMPL_CD AND W2.LANGUAGE_CD='RUS'
        WHERE WF_PROC_ID = (SELECT MAX(CP.WF_PROC_ID) KEEP (DENSE_RANK LAST ORDER BY CP.CRE_DTTM) 
                            FROM CI_WF_PROC CP 
                            INNER JOIN CM_WF_PROC_TMPL_EXP PT ON PT.WF_PROC_TMPL_CD = CP.WF_PROC_TMPL_CD 
                            WHERE PT.CM_WF_PROC_TYPE_CD = 'NOTIFICATION' OR PT.CM_WF_PROC_TYPE_CD = 'LIMITATION' 
                            CONNECT BY PRIOR CP.WF_PROC_ID = CP.CR_BY_WF_PROC_ID START WITH CP.CR_BY_WF_PROC_ID = DZ_MAIN.WF_PROC_ID
                            ) 
        ) LAST_TMPL_NAME
      ,MAX(ADDR_ID.CHAR_VAL_FK1) KEEP (DENSE_RANK LAST ORDER BY ADDR_ID.EFFDT) ADDR_ID_POST
      ,MAX(ADDR_ID_PER.CHAR_VAL_FK1) KEEP (DENSE_RANK LAST ORDER BY ADDR_ID_PER.EFFDT) ADDR_ID_PER  
      ,MAX(CM_DZ_REP_UTILS.GET_PHONE_FOR_PERSON_TYPE(MAIN_PER.PER_ID, 'CHIEF')) CHIEF_TEL 
      ,MAX(CM_DZ_REP_UTILS.GET_PHONE_FOR_PERSON_TYPE(MAIN_PER.PER_ID, 'BOOK')) BOOK_TEL
      ,MAX(CM_DZ_REP_UTILS.GET_FIO_FOR_PERSON_TYPE(MAIN_PER.PER_ID, 'CHIEF')) CHIEF_NAME
      ,MAX(CM_DZ_REP_UTILS.GET_EMAIL_FOR_PERSON_TYPE(MAIN_PER.PER_ID, 'BOOK')) EMAIL 
      ,(SELECT MAX(AC.ADHOC_CHAR_VAL) KEEP (DENSE_RANK LAST ORDER BY AC.EFFDT) 
       FROM CI_ACCT_CHAR AC 
       WHERE AC.ACCT_ID = A.ACCT_ID
         AND AC.CHAR_TYPE_CD = 'D-ZT-AV1') D_ZT_AV1 
      ,(SELECT MAX(AC.ADHOC_CHAR_VAL) KEEP (DENSE_RANK LAST ORDER BY AC.EFFDT) 
       FROM CI_ACCT_CHAR AC 
       WHERE AC.ACCT_ID = A.ACCT_ID
         AND AC.CHAR_TYPE_CD = 'D-ZT-AV2') D_ZT_AV2                                 
FROM ACCT_LITE A
INNER JOIN PARAMETER PR ON 1 = 1
INNER JOIN CI_WF_PROC_CHAR LIC_S4ET ON LIC_S4ET.CHAR_VAL_FK1 = A.ACCT_ID AND LIC_S4ET.CHAR_TYPE_CD = 'LIC-S4ET'
INNER JOIN CI_WF_PROC DZ_MAIN ON DZ_MAIN.WF_PROC_ID = LIC_S4ET.WF_PROC_ID  AND DZ_MAIN.WF_PROC_TMPL_CD = 'DZ-MAIN' AND DZ_MAIN.WF_STAT_FLG = '10'
INNER JOIN CI_ACCT_PER MAIN_PER ON MAIN_PER.ACCT_ID = A.ACCT_ID AND MAIN_PER.MAIN_CUST_SW = 'Y'
INNER JOIN CI_PER_ID INN ON INN.PER_ID = MAIN_PER.PER_ID AND INN.ID_TYPE_CD = 'INN-Q'
INNER JOIN CI_PER_CHAR DOG_NAME ON DOG_NAME.PER_ID = MAIN_PER.PER_ID AND DOG_NAME.CHAR_TYPE_CD='SHORT-NM'
INNER JOIN CI_ACCT_CHAR ACCTSTAT ON ACCTSTAT.ACCT_ID=A.ACCT_ID AND ACCTSTAT.CHAR_TYPE_CD='ACCTSTAT'
LEFT JOIN CI_ACCT_CHAR COMM ON COMM.ACCT_ID=A.ACCT_ID AND COMM.CHAR_TYPE_CD='COMM'
LEFT JOIN CI_ACCT_CHAR DENZADOL ON DENZADOL.ACCT_ID=A.ACCT_ID AND DENZADOL.CHAR_TYPE_CD='DENZADOL'
LEFT JOIN CI_ACCT_CHAR STV_PENI ON STV_PENI.ACCT_ID=A.ACCT_ID AND STV_PENI.CHAR_TYPE_CD='STV-PENI' 
-- Почтовый адрес
LEFT JOIN CI_ACCT_CHAR ADDR_ID ON ADDR_ID.ACCT_ID=A.ACCT_ID AND ADDR_ID.CHAR_TYPE_CD='ADDR-ID' 
-- Юридический адрес
LEFT JOIN CI_PER_CHAR ADDR_ID_PER ON ADDR_ID_PER.PER_ID=MAIN_PER.PER_ID AND ADDR_ID_PER.CHAR_TYPE_CD='ADDR-ID' 
WHERE 1 = 1
GROUP BY 
        A.ACCT_ID
       ,A.ACCESS_GRP_CD
       ,A.ACCESS_GRP_DESCR
       ,A.DOG_DATE
       ,A.DOG_NUM
       ,A.KLS_OESK
       ,INN.PER_ID_NBR 
       ,DZ_MAIN.WF_PROC_ID               
) 
, ACCT_AE_PRE AS (
SELECT   
  RP.ACCT_ID
 ,RP.CM_RETAIL_TYPE 
 ,CASE WHEN (AE.ACCOUNTING_DT<PR.DATE_CUR_YEAR_01) THEN 'Y' ELSE 'N' END START_OF_CUR_YEAR_01_SW -- На начало года 
 ,CASE WHEN (AE.ACCOUNTING_DT<PR.DATE_CUR_MONTH_01) THEN 'Y' ELSE 'N' END START_OF_CUR_MONTH_01_SW -- На начало текущего месяца года
 ,CASE WHEN (AE.ACCOUNTING_DT>=PR.DATE_PREV_MONTH_01 AND AE.ACCOUNTING_DT<=PR.DATE_PREV_MONTH_30) THEN 'Y' ELSE 'N' END IN_PERIOD_PREV_MONTH_SW -- За предыдущимй месяц
 ,CASE WHEN (AE.ACCOUNTING_DT>=PR.DATE_CUR_MONTH_01 AND AE.ACCOUNTING_DT<=PR.DATE_REPORT) THEN 'Y' ELSE 'N' END IN_PERIOD_CUR_MONTH_SW -- За текущий месяц
 ,CASE WHEN (AE.ACCOUNTING_DT>=PR.DATE_CUR_YEAR_01 AND AE.ACCOUNTING_DT<=PR.DATE_REPORT) THEN 'Y' ELSE 'N' END IN_PERIOD_CUR_YEAR_SW -- За текущий год 
 ,CASE WHEN (FD.CASE_TYPE_CD = 'BILL' OR CM_RETAIL_TYPE NOT IN ('0','9')) THEN 'Y' ELSE 'N' END IS_RECEIVABLE_SW
 ,CASE WHEN (NOT(FD.CASE_TYPE_CD = 'BILL') AND CM_RETAIL_TYPE IN ('0','9') AND NOT AE.CM_FT_TYPE_CD IN ('AD_INV-PREP','AD_DISPPREP')AND AE.CM_ACCT_PAY_MATCH_EVT_PROP_ID <> ' ') THEN 'Y' ELSE 'N' END IS_PAYABLE_SW 
 ,CASE WHEN AE.CM_ACCT_PAY_MATCH_EVT_PROP_ID <> ' ' THEN 'Y' ELSE 'N' END IS_CREDIT_SW
 ,CASE WHEN (AE.SHOW_ON_BILL_SW='Y' AND (AE.CM_GENERATIVE_FT_ID=RP.CM_GENERATIVE_FT_ID OR RP.CM_GENERATIVE_FT_ID=REVERSAL_LINK.CM_DEST_FT_ID) AND AE.CM_ACCT_PAY_MATCH_EVT_PROP_ID = ' ') THEN 'Y' ELSE 'N' END IS_CHARGE_SW
 ,AE.CM_FT_TYPE_CD
 ,CASE WHEN AE.CM_OUT_OF_BALANCE_SW='N' THEN AE.CUR_AMT ELSE 0 END CUR_AMT
 ,CASE WHEN AE.DEBT_CL_CD='SPIS' THEN AE.CUR_AMT ELSE 0 END SPIS_AMT
 ,CASE WHEN AE.DEBT_CL_CD='REST' THEN AE.CUR_AMT ELSE 0 END REST_AMT
 ,CASE WHEN AE.DEBT_CL_CD='MEM' THEN AE.CUR_AMT ELSE 0 END MEM_AMT 
 ,CASE WHEN AE.DEBT_CL_CD='ISK' THEN AE.CUR_AMT ELSE 0 END ISK_AMT 
 ,CASE WHEN AE.DEBT_CL_CD='MOR' THEN AE.CUR_AMT ELSE 0 END MOR_AMT   
 ,CASE WHEN CM_RETAIL_TYPE IN ('4','7') AND AE.CM_ACKNOWLEDGEMENT_STAT_FLG = 'Y' THEN 'Y' ELSE 'N' END ACKNOWLEDGEMENT_STAT_YES
 ,CASE WHEN CM_RETAIL_TYPE IN ('4','7') AND AE.CM_ACKNOWLEDGEMENT_STAT_FLG = 'N' THEN 'Y' ELSE 'N' END ACKNOWLEDGEMENT_STAT_NO
 ,CASE WHEN FD.CASE_TYPE_CD = 'BILL' AND CM_RETAIL_TYPE = '0' AND PRC_AVAN.ADHOC_CHAR_VAL = '30' AND ACCT_PER.ADHOC_CHAR_VAL = PR.PERIOD_PREV THEN 'Y' ELSE 'N' END IS_AVANS30_PERIOD_PREV
 ,CASE WHEN FD.CASE_TYPE_CD = 'BILL' AND CM_RETAIL_TYPE = '0' AND PRC_AVAN.ADHOC_CHAR_VAL = '40' AND ACCT_PER.ADHOC_CHAR_VAL = PR.PERIOD_PREV THEN 'Y' ELSE 'N' END IS_AVANS40_PERIOD_PREV
 ,CASE WHEN FD.CASE_TYPE_CD = 'BILL' AND CM_RETAIL_TYPE = '0' AND PRC_AVAN.ADHOC_CHAR_VAL = '30' AND ACCT_PER.ADHOC_CHAR_VAL = PR.PERIOD_CUR THEN 'Y' ELSE 'N' END IS_AVANS30_PERIOD_CUR
 ,CASE WHEN FD.CASE_TYPE_CD = 'BILL' AND CM_RETAIL_TYPE = '0' AND PRC_AVAN.ADHOC_CHAR_VAL = '40' AND ACCT_PER.ADHOC_CHAR_VAL = PR.PERIOD_CUR THEN 'Y' ELSE 'N' END IS_AVANS40_PERIOD_CUR  
 ,CASE WHEN FD.CASE_TYPE_CD = 'BILL' AND CM_RETAIL_TYPE = '0' AND PRC_AVAN.ADHOC_CHAR_VAL = '30' AND PREP_PER.CASE_ID IS NOT NULL THEN 'Y' ELSE 'N' END IS_AVANS30_PERIOD_NEXT
 ,CASE WHEN FD.CASE_TYPE_CD = 'BILL' AND CM_RETAIL_TYPE = '0' AND PRC_AVAN.ADHOC_CHAR_VAL = '40' AND PREP_PER.CASE_ID IS NOT NULL THEN 'Y' ELSE 'N' END IS_AVANS40_PERIOD_NEXT  
FROM ACCT_LITE A
INNER JOIN PARAMETER PR ON 1 = 1
INNER JOIN CM_RATE_PROP RP ON RP.ACCT_ID = A.ACCT_ID
INNER JOIN CM_ACCOUNT_ENTRY AE ON AE.CM_RATE_PROP_ID = RP.CM_RATE_PROP_ID
LEFT  JOIN CM_FT_LINK REVERSAL_LINK ON REVERSAL_LINK.CM_SOURCE_FT_ID = AE.CM_GENERATIVE_FT_ID AND REVERSAL_LINK.CM_FT_LINK_TYPE_CD='REVERSAL'
INNER JOIN CI_CASE FD ON FD.CASE_ID = AE.CM_FIN_DOCUMENT_ID
LEFT JOIN CI_CASE_CHAR ACCT_PER ON ACCT_PER.CASE_ID = FD.CASE_ID AND ACCT_PER.CHAR_TYPE_CD = 'ACCT-PER' AND ACCT_PER.ADHOC_CHAR_VAL IN (PR.PERIOD_PREV, PR.PERIOD_CUR) -- Аванс выставлен в пред. и текущем периодах
LEFT JOIN CI_CASE_CHAR PREP_PER ON PREP_PER.CASE_ID = FD.CASE_ID AND PREP_PER.CHAR_TYPE_CD = 'PREP-PER' AND PREP_PER.ADHOC_CHAR_VAL = PR.PERIOD_NEXT -- Аванс выставлен на след. период
LEFT JOIN CI_CASE_CHAR PRC_AVAN ON PRC_AVAN.CASE_ID = FD.CASE_ID AND PRC_AVAN.CHAR_TYPE_CD = 'PRC-AVAN'
WHERE AE.ACCOUNTING_DT <= PR.DATE_REPORT
AND (AE.CM_OUT_OF_BALANCE_SW='N' OR AE.DEBT_CL_CD='SPIS')
AND NOT AE.CM_FT_TYPE_CD IN ('AD_DISPPREP')
)
, ACCOUNT_ENTRY AS (
SELECT 
  ACCT_ID
 ,CM_RETAIL_TYPE 
 ,START_OF_CUR_YEAR_01_SW
 ,START_OF_CUR_MONTH_01_SW
 ,IN_PERIOD_PREV_MONTH_SW 
 ,IN_PERIOD_CUR_MONTH_SW
 ,IN_PERIOD_CUR_YEAR_SW
 ,CASE WHEN (IS_RECEIVABLE_SW='Y') THEN AE.CUR_AMT ELSE 0 END ACCOUNT_RECEIVABLE_AMT -- ДЗ 
 ,CASE WHEN (IS_PAYABLE_SW='Y') THEN AE.CUR_AMT ELSE 0 END ACCOUNT_PAYABLES_AMT      -- КЗ 
 ,CASE WHEN (IS_CHARGE_SW='Y' OR (IS_CREDIT_SW='N' AND AE.CM_FT_TYPE_CD='AD_INV-PREP')) THEN AE.CUR_AMT ELSE 0 END CHARGE_AMT -- Начисления
 ,CASE WHEN (IS_CREDIT_SW='Y') THEN AE.CUR_AMT ELSE 0 END PAY_AMT                       -- Оплата
 ,SPIS_AMT  -- Списание 
 ,REST_AMT
 ,MEM_AMT
 ,ISK_AMT
 ,MOR_AMT   
 ,ACKNOWLEDGEMENT_STAT_YES
 ,ACKNOWLEDGEMENT_STAT_NO                                                                            
 ,IS_AVANS30_PERIOD_PREV
 ,IS_AVANS40_PERIOD_PREV  
 ,IS_AVANS30_PERIOD_CUR      
 ,IS_AVANS40_PERIOD_CUR  
 ,IS_AVANS30_PERIOD_NEXT
 ,IS_AVANS40_PERIOD_NEXT 
FROM  ACCT_AE_PRE AE
)
, ACCOUNT_ENTRY_FULL AS (
SELECT
  ACCT_ID
 ,CM_RETAIL_TYPE  
 -- ДЗ по основной реализации на 01 число текущего месяца
 ,CASE WHEN (START_OF_CUR_MONTH_01_SW='Y') THEN ACCOUNT_RECEIVABLE_AMT ELSE 0 END ST_ACCOUNT_RECEIVABLE_AMT
 -- ДЗ по основной реализации на начало года
 ,CASE WHEN (START_OF_CUR_YEAR_01_SW='Y') THEN ACCOUNT_RECEIVABLE_AMT ELSE 0 END ST_ACCOUNT_RECEIVABLE_AMT_YEAR 
 -- Начисление по основной реализации прошлого расчетного периода
 ,CASE WHEN (IN_PERIOD_PREV_MONTH_SW='Y') THEN CHARGE_AMT ELSE 0 END CHARGE_AMT_PREV_MM    
 -- Начислено с начала года
 ,CASE WHEN (IN_PERIOD_CUR_YEAR_SW='Y') THEN CHARGE_AMT ELSE 0 END CHARGE_AMT_YEAR   
 --Начислено в текущем месяце
 ,CASE WHEN (IN_PERIOD_CUR_MONTH_SW='Y') THEN CHARGE_AMT ELSE 0 END CHARGE_AMT_CUR_MONTH    
 ,CHARGE_AMT
 -- КЗ
 ,ACCOUNT_PAYABLES_AMT E_ACCOUNT_PAYABLES_AMT
 -- ДЗ     
 ,ACCOUNT_RECEIVABLE_AMT E_ACCOUNT_RECEIVABLE_AMT 
 -- Просроченная ДЗ
 ,SPIS_AMT
 ,REST_AMT
 ,MEM_AMT
 ,ISK_AMT
 ,MOR_AMT  
 -- Оплачено в текущем периоде
 ,CASE WHEN IN_PERIOD_CUR_MONTH_SW = 'Y' THEN PAY_AMT ELSE 0 END PAY_AMT
 -- Оплачено с начала года
 ,CASE WHEN IN_PERIOD_CUR_YEAR_SW = 'Y' THEN PAY_AMT ELSE 0 END PAY_AMT_YEAR 
 ,ACKNOWLEDGEMENT_STAT_YES
 ,ACKNOWLEDGEMENT_STAT_NO
 ,CASE WHEN IS_AVANS30_PERIOD_PREV = 'Y' THEN ACCOUNT_RECEIVABLE_AMT ELSE 0 END AVANS30_PREV_RECEIVABLE_AMT
 ,CASE WHEN IS_AVANS40_PERIOD_PREV = 'Y' THEN ACCOUNT_RECEIVABLE_AMT ELSE 0 END AVANS40_PREV_RECEIVABLE_AMT 
 ,CASE WHEN IS_AVANS30_PERIOD_CUR = 'Y' THEN ACCOUNT_RECEIVABLE_AMT ELSE 0 END AVANS30_CUR_RECEIVABLE_AMT      
 ,CASE WHEN IS_AVANS40_PERIOD_CUR = 'Y' THEN ACCOUNT_RECEIVABLE_AMT ELSE 0 END AVANS40_CUR_RECEIVABLE_AMT 
 ,CASE WHEN IS_AVANS30_PERIOD_CUR = 'Y' THEN CHARGE_AMT ELSE 0 END AVANS30_CUR_CHARGE_AMT      
 ,CASE WHEN IS_AVANS40_PERIOD_CUR = 'Y' THEN CHARGE_AMT ELSE 0 END AVANS40_CUR_CHARGE_AMT    
 ,CASE WHEN IS_AVANS30_PERIOD_NEXT = 'Y' THEN CHARGE_AMT ELSE 0 END AVANS30_NEXT_CHARGE_AMT
 ,CASE WHEN IS_AVANS40_PERIOD_NEXT = 'Y' THEN CHARGE_AMT ELSE 0 END AVANS40_NEXT_CHARGE_AMT 
FROM ACCOUNT_ENTRY
)
, ACCOUNT_ENTRY_GROUP AS (
SELECT /*+ MATERIALIZE*/
  ACCT_ID
 --,CM_RETAIL_TYPE
 ,SUM(E_ACCOUNT_RECEIVABLE_AMT) E_ACCT_RECEIVABLE_AMT    
 --<< Основная реализация >>
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN ST_ACCOUNT_RECEIVABLE_AMT ELSE 0 END) ST_RECEIVABLE_AMT_CUR_MM_01  
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN CHARGE_AMT_PREV_MM ELSE 0 END) CHARGE_AMT_PREV_MM 
 ,SUM(/*CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN E_ACCOUNT_PAYABLES_AMT ELSE 0 END*/ E_ACCOUNT_PAYABLES_AMT) E_ACCOUNT_PAYABLES_AMT
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN E_ACCOUNT_RECEIVABLE_AMT ELSE 0 END) E_ACCOUNT_RECEIVABLE_AMT   
 --,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN SPIS_AMT ELSE 0 END) SPIS_AMT_2   
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN PAY_AMT ELSE 0 END) PAY_AMT_2
 -- ДЗ-КЗ     
 ,SUM(/*CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN E_ACCOUNT_RECEIVABLE_AMT - E_ACCOUNT_PAYABLES_AMT ELSE 0 END*/ E_ACCOUNT_RECEIVABLE_AMT + E_ACCOUNT_PAYABLES_AMT) DZ_KZ 
  --<< Аванс >>
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('0') THEN E_ACCOUNT_RECEIVABLE_AMT ELSE 0 END) E_ACCOUNT_RECEIVABLE_AMT_0 
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('0') THEN PAY_AMT ELSE 0 END) PAY_AMT_0     
  --<< Неустойка >>  
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') AND ACKNOWLEDGEMENT_STAT_YES = 'Y' THEN E_ACCOUNT_RECEIVABLE_AMT ELSE 0 END) E_ACCOUNT_RECEIVABLE_AMT_Y_4
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') AND ACKNOWLEDGEMENT_STAT_NO = 'Y' THEN E_ACCOUNT_RECEIVABLE_AMT ELSE 0 END) E_ACCOUNT_RECEIVABLE_AMT_N_4
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN E_ACCOUNT_RECEIVABLE_AMT ELSE 0 END) E_ACCOUNT_RECEIVABLE_AMT_4 
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN CHARGE_AMT_CUR_MONTH ELSE 0 END) CHARGE_AMT_4
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN PAY_AMT ELSE 0 END) PAY_AMT_4 
 ,SUM(PAY_AMT) PAY_AMT
 --<< Списание >>   
 ,SUM(SPIS_AMT) SPIS_AMT
 --<< Иные категории задолженности >>    
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN REST_AMT ELSE 0 END) REST_AMT_2
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN REST_AMT ELSE 0 END) REST_AMT_4
 ,SUM(REST_AMT) REST_AMT   
 ,SUM(MEM_AMT) MEM_AMT
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN ISK_AMT ELSE 0 END) ISK_AMT_2
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN ISK_AMT ELSE 0 END) ISK_AMT_4 
 ,SUM(ISK_AMT) ISK_AMT
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN MOR_AMT ELSE 0 END) MOR_AMT_2
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN MOR_AMT ELSE 0 END) MOR_AMT_4  
 ,SUM(MOR_AMT) MOR_AMT
 --<< Задолженность на начало года >>  
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN ST_ACCOUNT_RECEIVABLE_AMT_YEAR ELSE 0 END) ST_RECEIVABLE_AMT_YEAR_2 
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('0') THEN ST_ACCOUNT_RECEIVABLE_AMT_YEAR ELSE 0 END) ST_RECEIVABLE_AMT_YEAR_0  
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN ST_ACCOUNT_RECEIVABLE_AMT_YEAR ELSE 0 END) ST_RECEIVABLE_AMT_YEAR_4  
 --<< Начислено на начало года >>  
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN CHARGE_AMT_YEAR ELSE 0 END) CHARGE_AMT_YEAR_2  
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('0') THEN CHARGE_AMT_YEAR ELSE 0 END) CHARGE_AMT_YEAR_0  
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN CHARGE_AMT_YEAR ELSE 0 END) CHARGE_AMT_YEAR_4  
 ,SUM(CHARGE_AMT_YEAR) CHARGE_AMT_YEAR  
 --<< Оплачено на начало года >>
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN PAY_AMT_YEAR ELSE 0 END) PAY_AMT_YEAR_2  
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('0') THEN PAY_AMT_YEAR ELSE 0 END) PAY_AMT_YEAR_0  
 ,SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN PAY_AMT_YEAR ELSE 0 END) PAY_AMT_YEAR_4   
 ,SUM(PAY_AMT_YEAR) PAY_AMT_YEAR    
 --<< Периоды задолженности >>
 ,CM_DZ_REP_UTILS.GET_NO_PAY_PERIOD_BY_ACCT(ACCT_ID,'2') NO_PAYMENTS_PERIODS_2
 ,CM_DZ_REP_UTILS.GET_COUNT_NOPAY_PERIOD_BY_ACCT(ACCT_ID,'2') COUNT_NO_PAYMENTS_PERIODS_2 
 ,CASE WHEN SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') AND ACKNOWLEDGEMENT_STAT_YES = 'Y' THEN E_ACCOUNT_RECEIVABLE_AMT ELSE 0 END) > 0 
       THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, '7', NULL, 'Y') END NO_PAYMENTS_PERIODS_Y_4
 ,CASE WHEN SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') AND ACKNOWLEDGEMENT_STAT_NO = 'Y' THEN E_ACCOUNT_RECEIVABLE_AMT ELSE 0 END) > 0 
       THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, '7', NULL, 'N') END NO_PAYMENTS_PERIODS_N_4 
 ,CASE WHEN SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN REST_AMT ELSE 0 END)>0 THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, '2', 'REST') END NO_PAYMENTS_PERIODS_REST_2 
 ,CASE WHEN SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN REST_AMT ELSE 0 END)>0 THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, '7', 'REST') END NO_PAYMENTS_PERIODS_REST_4     
 ,CASE WHEN SUM(MEM_AMT)>0 THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, NULL, 'MEM') END NO_PAYMENTS_PERIODS_MEM   
 ,CASE WHEN SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN ISK_AMT ELSE 0 END)>0 THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, '2', 'ISK') END NO_PAYMENTS_PERIODS_ISK_2 
 ,CASE WHEN SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN ISK_AMT ELSE 0 END)>0 THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, '7', 'ISK') END NO_PAYMENTS_PERIODS_ISK_4    
 ,CASE WHEN SUM(CASE WHEN CM_RETAIL_TYPE IN ('2','8') THEN MOR_AMT ELSE 0 END)>0 THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, '2', 'MOR') END NO_PAYMENTS_PERIODS_MOR_2  
 ,CASE WHEN SUM(CASE WHEN CM_RETAIL_TYPE IN ('4','7') THEN MOR_AMT ELSE 0 END)>0 THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, '7', 'MOR') END NO_PAYMENTS_PERIODS_MOR_4
 ,CASE WHEN SUM(SPIS_AMT)>0 THEN CM_DZ_REP_UTILS.DEBT_PERIOD_BY_ACCT(ACCT_ID, NULL, 'SPIS') END NO_PAYMENTS_PERIODS_SPIS
 --<< Аванс >> 
 ,SUM(AVANS30_PREV_RECEIVABLE_AMT) AVANS30_PREV_RECEIVABLE_AMT
 ,SUM(AVANS40_PREV_RECEIVABLE_AMT) AVANS40_PREV_RECEIVABLE_AMT
 ,SUM(AVANS30_CUR_RECEIVABLE_AMT) AVANS30_CUR_RECEIVABLE_AMT      
 ,SUM(AVANS40_CUR_RECEIVABLE_AMT) AVANS40_CUR_RECEIVABLE_AMT
 ,SUM(AVANS30_CUR_CHARGE_AMT)     AVANS30_CUR_CHARGE_AMT  
 ,SUM(AVANS40_CUR_CHARGE_AMT)   AVANS40_CUR_CHARGE_AMT 
 ,SUM(AVANS30_NEXT_CHARGE_AMT) AVANS30_NEXT_CHARGE_AMT
 ,SUM(AVANS40_NEXT_CHARGE_AMT) AVANS40_NEXT_CHARGE_AMT     
FROM ACCOUNT_ENTRY_FULL
GROUP BY 
  ACCT_ID
 --,CM_RETAIL_TYPE
)
SELECT 
        ROW_NUMBER() OVER(ORDER BY A.DOG_NUM) RN,
        A.ACCESS_GRP_CD,
        A.ACCESS_GRP_DESCR,
        A.DOG_DATE,
        A.DOG_NUM,
        A.KLS_OESK,
        A.DOG_NAME,
        (SELECT DOG_STATUS.DESCR 
         FROM CI_CHAR_VAL_L DOG_STATUS 
         WHERE DOG_STATUS.CHAR_TYPE_CD = 'ACCTSTAT' 
         AND DOG_STATUS.CHAR_VAL = A.DOG_STATUS 
         AND DOG_STATUS.LANGUAGE_CD = 'RUS') DOG_STATUS,   
        (SELECT DOG_COMM.DESCR 
         FROM CI_CHAR_VAL_L DOG_COMM 
         WHERE DOG_COMM.CHAR_TYPE_CD = 'COMM' 
         AND DOG_COMM.CHAR_VAL = A.DOG_COMM 
         AND DOG_COMM.LANGUAGE_CD = 'RUS') DOG_COMM,                 
        A.INN,
        A.EO_COUNT,
        A.POWER_AB,
        A.POWER_TB,
        A.MM_CAT,
        A.SETI_NAME,
        A.CP_MAX_P,
        A.DENZADOL,
        A.STV_PENI,
        A.MAIN_PROC_ID,
        A.NOTICETP,
        A.LAST_TMPL_NAME,
        A.ADDR_ID_POST,
        A.ADDR_ID_PER,
        A.CHIEF_TEL,
        A.BOOK_TEL,
        A.CHIEF_NAME,
        A.EMAIL,
        A.D_ZT_AV1,
        A.D_ZT_AV2,        
        ADR.CM_ADDRESS_DISTRICT_DESCR,  
        ADR.CM_ADDRESS_DESCR ADDRESS_ACCT,
        ADR_PER.CM_ADDRESS_DESCR ADDRESS_PER,               
        AE.*
FROM ACCT_FILTERED A
LEFT JOIN CM_ADDRESS ADR ON ADR.CM_ADDRESS_ID = A.ADDR_ID_POST
LEFT JOIN CM_ADDRESS ADR_PER  ON ADR_PER.CM_ADDRESS_ID = A.ADDR_ID_PER
LEFT JOIN ACCOUNT_ENTRY_GROUP AE ON AE.ACCT_ID = A.ACCT_ID
WHERE 1 = 1 
      ) LOOP
      
     V_RET.RN := I.RN; 
     V_RET.ACCT_ID := I.ACCT_ID; 
     V_RET.DOG_NUM := I.DOG_NUM;  
     V_RET.DOG_NAME := I.DOG_NAME;     
     V_RET.DOG_DATE := I.DOG_DATE;                 
     V_RET.ACCESS_GRP_CD := I.ACCESS_GRP_CD;
     V_RET.ACCESS_GRP_DESCR := I.ACCESS_GRP_DESCR;
     V_RET.KLS_OESK := I.KLS_OESK;
     V_RET.DOG_STATUS := I.DOG_STATUS;
     V_RET.DOG_COMM := I.DOG_COMM;
     V_RET.INN := I.INN;
     V_RET.EO_COUNT := I.EO_COUNT;
     V_RET.POWER_AB := I.POWER_AB;
     V_RET.POWER_TB := I.POWER_TB;
     V_RET.MM_CAT := I.MM_CAT;
     V_RET.SETI_NAME := I.SETI_NAME;
     V_RET.CP_MAX_P := I.CP_MAX_P;
     V_RET.DENZADOL := I.DENZADOL;
     V_RET.STV_PENI := I.STV_PENI;
     V_RET.MAIN_PROC_ID := I.MAIN_PROC_ID;
     V_RET.NOTICETP := I.NOTICETP;
     V_RET.LAST_TMPL_NAME := I.LAST_TMPL_NAME;
     V_RET.CM_ADDRESS_DISTRICT_DESCR := I.CM_ADDRESS_DISTRICT_DESCR;
     V_RET.ADDRESS_ACCT := I.ADDRESS_ACCT ;       
     V_RET.ADDRESS_PER := I.ADDRESS_PER;
     V_RET.CHIEF_NAME := I.CHIEF_NAME;
     V_RET.CHIEF_TEL := I.CHIEF_TEL;              
     V_RET.BOOK_TEL := I.BOOK_TEL;
     V_RET.EMAIL := I.EMAIL;
     V_RET.D_ZT_AV1 := I.D_ZT_AV1;
     V_RET.D_ZT_AV2 := I.D_ZT_AV2;          
                
    -- V_RET.CHARGE_AMT := I.CHARGE_AMT;
     V_RET.CHARGE_AMT_PREV_MM := I.CHARGE_AMT_PREV_MM;
     V_RET.ST_RECEIVABLE_AMT_CUR_MM_01 := I.ST_RECEIVABLE_AMT_CUR_MM_01;
     V_RET.E_ACCOUNT_PAYABLES_AMT := - I.E_ACCOUNT_PAYABLES_AMT; 
     V_RET.E_ACCOUNT_RECEIVABLE_AMT := I.E_ACCOUNT_RECEIVABLE_AMT;
     V_RET.E_ACCOUNT_RECEIVABLE_AMT_OVER := I.E_ACCOUNT_RECEIVABLE_AMT;
     V_RET.PAY_AMT_2 := I.PAY_AMT_2;
     V_RET.DZ_KZ := I.DZ_KZ;
     V_RET.E_ACCOUNT_RECEIVABLE_AMT_0 := I.E_ACCOUNT_RECEIVABLE_AMT_0;
     V_RET.PAY_AMT_0 := I.PAY_AMT_0;
     V_RET.E_ACCOUNT_RECEIVABLE_AMT_Y_4 := I.E_ACCOUNT_RECEIVABLE_AMT_Y_4;
     V_RET.E_ACCOUNT_RECEIVABLE_AMT_N_4 := I.E_ACCOUNT_RECEIVABLE_AMT_N_4;
     V_RET.E_ACCOUNT_RECEIVABLE_AMT_4 := I.E_ACCOUNT_RECEIVABLE_AMT_4;
     V_RET.E_ACCT_RECEIVABLE_AMT := V_RET.E_ACCOUNT_RECEIVABLE_AMT_OVER + I.E_ACCOUNT_RECEIVABLE_AMT_0 + I.E_ACCOUNT_RECEIVABLE_AMT_4;
     V_RET.CHARGE_AMT_4 := I.CHARGE_AMT_4;
     V_RET.PAY_AMT_4 := - I.PAY_AMT_4;
     V_RET.PAY_AMT := - I.PAY_AMT;
     V_RET.SPIS_AMT := I.SPIS_AMT;
     V_RET.REST_AMT_2 := I.REST_AMT_2;
     V_RET.REST_AMT_4 := I.REST_AMT_4;
     V_RET.REST_AMT := I.REST_AMT;
     V_RET.MEM_AMT := I.MEM_AMT;
     V_RET.ISK_AMT_2 := I.ISK_AMT_2;
     V_RET.ISK_AMT_4 := I.ISK_AMT_4;
     V_RET.ISK_AMT := I.ISK_AMT;
     V_RET.MOR_AMT_2 := I.MOR_AMT_2;
     V_RET.MOR_AMT_4 := I.MOR_AMT_4;
     V_RET.MOR_AMT := I.MOR_AMT;
     V_RET.ST_RECEIVABLE_AMT_YEAR_2 := I.ST_RECEIVABLE_AMT_YEAR_2;
     V_RET.ST_RECEIVABLE_AMT_YEAR_2_CUR := I.ST_RECEIVABLE_AMT_YEAR_2;
     V_RET.ST_RECEIVABLE_AMT_YEAR_2_OVER := I.ST_RECEIVABLE_AMT_YEAR_2;
     V_RET.ST_RECEIVABLE_AMT_YEAR_0 := I.ST_RECEIVABLE_AMT_YEAR_0;
     V_RET.ST_RECEIVABLE_AMT_YEAR_4 := I.ST_RECEIVABLE_AMT_YEAR_4;
     V_RET.CHARGE_AMT_YEAR_2 := I.CHARGE_AMT_YEAR_2;
     V_RET.CHARGE_AMT_YEAR_0 := I.CHARGE_AMT_YEAR_0;
     V_RET.CHARGE_AMT_YEAR_4 := I.CHARGE_AMT_YEAR_4;
     V_RET.CHARGE_AMT_YEAR := I.CHARGE_AMT_YEAR;
     V_RET.PAY_AMT_YEAR_2 := - I.PAY_AMT_YEAR_2;
     V_RET.PAY_AMT_YEAR_0 := - I.PAY_AMT_YEAR_0;
     V_RET.PAY_AMT_YEAR_4 := - I.PAY_AMT_YEAR_4;
     V_RET.PAY_AMT_YEAR := - I.PAY_AMT_YEAR;
     V_RET.NO_PAYMENTS_PERIODS_2 := I.NO_PAYMENTS_PERIODS_2;
     V_RET.COUNT_NO_PAYMENTS_PERIODS_2 := I.COUNT_NO_PAYMENTS_PERIODS_2;
     V_RET.NO_PAYMENTS_PERIODS_N_4 := I.NO_PAYMENTS_PERIODS_N_4;
     V_RET.NO_PAYMENTS_PERIODS_Y_4 := I.NO_PAYMENTS_PERIODS_Y_4;     
     V_RET.NO_PAYMENTS_PERIODS_REST_2 := I.NO_PAYMENTS_PERIODS_REST_2;
     V_RET.NO_PAYMENTS_PERIODS_REST_4 := I.NO_PAYMENTS_PERIODS_REST_4;
     V_RET.NO_PAYMENTS_PERIODS_MEM := I.NO_PAYMENTS_PERIODS_MEM;
     V_RET.NO_PAYMENTS_PERIODS_ISK_2 := I.NO_PAYMENTS_PERIODS_ISK_2;
     V_RET.NO_PAYMENTS_PERIODS_ISK_4 := I.NO_PAYMENTS_PERIODS_ISK_4;
     V_RET.NO_PAYMENTS_PERIODS_MOR_2 := I.NO_PAYMENTS_PERIODS_MOR_2;
     V_RET.NO_PAYMENTS_PERIODS_MOR_4 := I.NO_PAYMENTS_PERIODS_MOR_4;
     V_RET.NO_PAYMENTS_PERIODS_SPIS := I.NO_PAYMENTS_PERIODS_SPIS;                   

     V_RET.AVANS30_PREV_RECEIVABLE_AMT := I.AVANS30_PREV_RECEIVABLE_AMT;
     V_RET.AVANS40_PREV_RECEIVABLE_AMT := I.AVANS40_PREV_RECEIVABLE_AMT;
     V_RET.AVANS30_CUR_RECEIVABLE_AMT := I.AVANS30_CUR_RECEIVABLE_AMT;    
     V_RET.AVANS40_CUR_RECEIVABLE_AMT := I.AVANS40_CUR_RECEIVABLE_AMT;
     V_RET.AVANS30_CUR_CHARGE_AMT := I.AVANS30_CUR_CHARGE_AMT;
     V_RET.AVANS40_CUR_CHARGE_AMT := I.AVANS40_CUR_CHARGE_AMT;
     V_RET.AVANS30_NEXT_CHARGE_AMT := I.AVANS30_NEXT_CHARGE_AMT;
     V_RET.AVANS40_NEXT_CHARGE_AMT := I.AVANS40_NEXT_CHARGE_AMT;
       
      PIPE ROW(V_RET);      
      
    END LOOP;  
   
  END IF; 
                
END REPORT_DZ_ON_DATE;      
  
begin
  -- Initialization
  --<Statement>;
  null;
end CM_DZ_REP_UN_BILLING;
