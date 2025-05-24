*&---------------------------------------------------------------------*
*& Include ztm_pdr_sel
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK ba WITH FRAME TITLE tta.
  PARAMETERS: p_r_fom RADIOBUTTON GROUP r1 USER-COMMAND ca DEFAULT 'X',
              p_r_alv RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK ba.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
  " ---------------------------------------------------------------------
  " 10: Forwarding Quotation
  " ---------------------------------------------------------------------
  SELECT-OPTIONS s_fwq_no FOR /scmtms/d_trqrot-trq_id
                 NO-EXTENSION NO INTERVALS MEMORY ID fwq MODIF ID fom.
  SELECT-OPTIONS sr_fwqno FOR /scmtms/d_trqrot-trq_id MODIF ID alv MEMORY ID sfw.
  " ---------------------------------------------------------------------
  " 20: BL Number
  " *********************************************************************
  SELECT-OPTIONS s_bolno FOR gv_bolno.
  " ---------------------------------------------------------------------
  " 30: Manifest
  " *********************************************************************
  PARAMETERS s_mantyp TYPE /scmtms/d_trqitm-zzmanifest_type
             AS LISTBOX VISIBLE LENGTH 15 DEFAULT 'IMPORT' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
