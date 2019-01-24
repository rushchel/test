SELECT TO_CHAR(SYSDATE, 'DD.MM.YYYY') CUR_DATE, 
      T.DOC_ID,
      '070-' || T.DOC_NUM || '-УЭ' DOC_NUM,
      T.DOC_DAT,
      T.DOC_ID_PREV,
      T.DOC_NUM_PREV,
      T.DOC_DAT_PREV,
      T.ACCT_ID,
      T.DOG_NUM,
      T.DOG_DAT,
      T.SPEC_CALC,
      T.MAIN_PER_ID,
      T.MAIN_NAME,
      T.MAIN_POSTAL,
      T.MAIN_ADDRESS,
      DECODE(TRIM(T.MAIN_POSTAL), NULL, T.MAIN_ADDRESS, TRIM(T.MAIN_POSTAL) || ', ' || T.MAIN_ADDRESS) ADDRESS,
      T.MAIN_FAX,
      T.MAIN_TEL,
      T.WF_PROC_ID,
      T.EVT_SEQ,
      T.EXECUTOR_NAME,
      T.EXECUTOR_TEL,
      T.SIGNER_JOB,
      --T.SIGNER_NAME,
      CM_DZ_REP_UN_BILLING.GET_PERSON_OMSK(T.WF_PROC_ID,'NOTIF') SIGNER_NAME,
      nvl((with signer as (select cm_dz_chty.get_case_chty(doc_claim.char_val_fk1,'SIGNERID') per_id
                             from ci_case_char doc_claim
                            where doc_claim.case_id = :CASE_ID 
                              and doc_claim.char_type_cd = 'REESTR')
           select trim(replace(dover_sg.adhoc_char_val,'доверенности',''))
             from signer t left join ci_per_char dover_sg on dover_sg.per_id = t.per_id 
                                                         and dover_sg.char_type_cd = 'DOVER-SG'
            where (dover_sg.effdt is null or dover_sg.effdt = (select max(dlz2.effdt) 
                                                                 from ci_per_char dlz2 
                                                                where dlz2.per_id = dover_sg.per_id 
                                                                  and dlz2.char_type_cd = dover_sg.char_type_cd)))
         ,T.SIGNER_DOVER) SIGNER_DOVER,
      T.RECEIVER_JOB,
      T.RECEIVER_NAME,
      TO_CHAR(TO_DATE(T.DOC_DAT, 'DD.MM.YYYY')+10, 'DD.MM.YYYY') PAY_TO_DATE,
  CM_LAW.get_logo(:CASE_ID,to_date(CM_DZ_CHTY.get_case_chty(:CASE_ID, 'DOC-DAT'),'dd.mm.yyyy')) AS LOGO,
  CM_LAW.get_logo(:CASE_ID,to_date(CM_DZ_CHTY.get_case_chty(:CASE_ID, 'DOC-DAT'),'dd.mm.yyyy'), 'FOOTER1') AS LOGO_FOOTER,
      T.ESTIMATR_JOB,    
      T.ESTIMATR_JOB_RP,
      T.ESTIMATR_NAIM_PLN,
      T.ESTIMATR_FIO_RP,
      CASE WHEN TRIM(T.ESTIMATR_PHONE) IS NOT NULL THEN T.ESTIMATR_PHONE ELSE '293-253, каб 108' END ESTIMATR_PHONE
FROM TABLE(CM_DZ_REP_UN_BILLING.GET_DOC_HEAD(:CASE_ID)) T
