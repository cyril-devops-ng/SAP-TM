*&---------------------------------------------------------------------*
*& Include          ZTM039_P_OUT_TURN_NEW_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
 SET PF-STATUS 'ZSTATUS_0100'.
 SET TITLEBAR 'ZTITLE_100'.

 try.
   data(lo_otr)
   = lcl_otr=>create_otr_report(
       it_fwq_no = sr_fwqno[]
       it_bl_no  = s_bolno[]
       if_mantyp = s_mantyp
     ).
   lo_otr->create_grid(
     CHANGING
       ct_tab = gt_output
   ).

   lo_otr->show_grid( ).
 catch zcx_tm_form_exception into data(lo_tm_ex).
   DATA(lt_msg) = lcl_otr=>create_bapi_ret_from_exception( lo_tm_ex ).
   lcl_otr=>display_bapi_log_gui( lt_msg ).
   leave to screen 0.
 endtry.
ENDMODULE.
