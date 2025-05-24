*&---------------------------------------------------------------------*
*& Include ztm039_p_out_turn_new_top
*&---------------------------------------------------------------------*
TYPE-POOLS zotr.
" ---------------------------------------------------------------------
" 10: Table Declaration
" ---------------------------------------------------------------------
TABLES: /scmtms/d_trqrot,
        ztm005t_trq_exec,
        /scmtms/d_trqitm.
" ---------------------------------------------------------------------
" 20: Data Declaration
" ---------------------------------------------------------------------
DATA gv_bolno      TYPE ztm039s_manif_itm-zzbolnumber.
DATA gs_header     TYPE ztm039s_header.
DATA gs_param      TYPE sfpoutputparams.
DATA gt_items      TYPE ztm039_item_tt.
DATA gt_tures_man  TYPE ztm039_tures_tt.
DATA gt_tures_dis  TYPE ztm039_tures_tt.
DATA gf_func_name  TYPE funcname.
DATA gf_form_name  TYPE fpname VALUE 'ZTM039_OUT_TURN_FRM_2'.
DATA gv_fwq        TYPE string.
" ---------------------------------------------------------------------
" 30: TM BOR Data
" ---------------------------------------------------------------------
DATA lo_trq_srvmgr TYPE REF TO /bobf/if_tra_service_manager.
DATA lt_trq_selpar TYPE /bobf/t_frw_query_selparam.
" ---------------------------------------------------------------------
" 40: Field symbols
" ---------------------------------------------------------------------
FIELD-SYMBOLS <fs_trq_fwq>      TYPE /scmtms/t_trq_root_k.
FIELD-SYMBOLS <fs_trq_fwo>      TYPE /scmtms/t_trq_root_k.
FIELD-SYMBOLS <fs_fwq_items>    TYPE /scmtms/t_trq_item_k.
FIELD-SYMBOLS <fs_fwq_products> TYPE /scmtms/t_trq_item_k.
FIELD-SYMBOLS <fs_fwo_items>    TYPE /scmtms/t_trq_item_k.
FIELD-SYMBOLS <fs_fwq_exec>     TYPE ztm005_trq_exec_k_tt.
FIELD-SYMBOLS <fs_fwo_cargo>    TYPE zotr_tt_cargo_details.
FIELD-SYMBOLS <fs_fwq_party>    type /scmtms/t_trq_party_k.
" ---------------------------------------------------------------------
" 50: Constants
" ---------------------------------------------------------------------
CONSTANTS trq_id             TYPE string              VALUE 'TRQ_ID' ##NO_TEXT.
CONSTANTS gc_arrival_event   TYPE ztm005e_event_code  VALUE 'Arrival' ##NO_TEXT.
CONSTANTS gc_departure_event TYPE ztm005e_event_code  VALUE 'Departure' ##NO_TEXT.
CONSTANTS gc_zexec_assoc_key TYPE /bobf/obm_assoc_key VALUE '5D786398C4FA1EDDAFEB80C51BD3B2BC' ##NO_TEXT.

" ---------------------------------------------------------------------
" 60: Macros
" ---------------------------------------------------------------------
DEFINE hide_screen_fields.
  LOOP AT SCREEN.
    IF screen-group1 = &1.
      screen-input     = '0'.
      screen-invisible = '1'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
END-OF-DEFINITION.

DEFINE show_screen_fields.
  LOOP AT SCREEN.
    IF screen-group1 = &1.
      screen-input     = '1'.
      screen-invisible = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
END-OF-DEFINITION.
DEFINE _thiscolumn.
  go_column ?= go_columns->get_column( <fs_component>-name ).
END-OF-DEFINITION.
DEFINE add_func.
  go_alv->get_functions( )->add_function( tooltip  = &1
                                          name     = &2
                                          icon     = &3
                                          text     = &4
                                          position = 1 ).
END-OF-DEFINITION.
" ---------------------------------------------------------------------
" 70: ALV Data Declaration
" ---------------------------------------------------------------------
DATA gt_output  TYPE TABLE OF zotr_alv_output.
DATA go_alv     TYPE REF TO cl_salv_table.
DATA go_columns TYPE REF TO cl_salv_columns_table.
DATA go_column  TYPE REF TO cl_salv_column_table.
DATA go_layout  TYPE REF TO cl_salv_layout.
DATA go_cont    TYPE REF TO cl_gui_container.
DATA go_select  TYPE REF TO cl_salv_selections.
" ---------------------------------------------------------------------
" 80: Class declarations
" ---------------------------------------------------------------------
CLASS lcl_otr DEFINITION DEFERRED.

CLASS lcl_otr DEFINITION.
  PUBLIC SECTION.
    TYPES lr_fwq_no TYPE RANGE OF /scmtms/d_trqrot-trq_id.
    TYPES lr_bl_no  TYPE RANGE OF /scmtms/d_trqitm-zzbolnumber.

    CLASS-METHODS create_bapi_ret_from_exception
      IMPORTING io_exception  TYPE REF TO cx_root
      RETURNING VALUE(rt_msg) TYPE bapiret2_t.

    CLASS-METHODS create_otr_report
      IMPORTING it_fwq_no       TYPE any
                it_bl_no        TYPE any
                if_mantyp       TYPE any
      RETURNING VALUE(r_result) TYPE REF TO lcl_otr
      RAISING   zcx_tm_form_exception.

    CLASS-METHODS display_bapi_log_gui
      IMPORTING it_bapi_ret TYPE bapiret2_t.
    CONSTANTS report TYPE string VALUE '0100'.
    METHODS constructor IMPORTING it_fwq_no TYPE ANY TABLE OPTIONAL
                                  it_bl_no  TYPE ANY TABLE OPTIONAL
                                  if_mantyp TYPE /scmtms/d_trqitm-zzmanifest_type
                        RAISING   zcx_tm_form_exception.

    METHODS create_grid CHANGING ct_tab TYPE ANY TABLE
                        RAISING  zcx_tm_form_exception.

    METHODS show_grid
      RAISING zcx_tm_form_exception.

    METHODS refresh_grid
      RAISING zcx_tm_form_exception.

    METHODS retrieve_fwo
      RAISING zcx_tm_form_exception.

    METHODS fill_selection_params
      RAISING zcx_tm_form_exception.

    METHODS on_added_function
      FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

    METHODS on_link_click
      FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
      IMPORTING !row !column.

    METHODS display_form IMPORTING is_output TYPE zotr_alv_output
                         RAISING   zcx_tm_form_exception.

  PRIVATE SECTION.
    DATA gr_fwq_no TYPE RANGE OF /scmtms/d_trqrot-trq_id.
    DATA gr_bl_no  TYPE RANGE OF /scmtms/d_trqitm-zzbolnumber.
    DATA gf_mantyp TYPE /scmtms/d_trqitm-zzmanifest_type.

    CONSTANTS trq_id             TYPE string              VALUE 'TRQ_ID' ##NO_TEXT.
    CONSTANTS gc_arrival_event   TYPE ztm005e_event_code  VALUE 'Arrival' ##NO_TEXT.
    CONSTANTS gc_departure_event TYPE ztm005e_event_code  VALUE 'Departure' ##NO_TEXT.
    CONSTANTS gc_zexec_assoc_key TYPE /bobf/obm_assoc_key VALUE '5D786398C4FA1EDDAFEB80C51BD3B2BC' ##NO_TEXT.

    DATA lo_trq_srvmgr   TYPE REF TO /bobf/if_tra_service_manager.
    DATA lt_trq_selpar   TYPE /bobf/t_frw_query_selparam.
    DATA dref            TYPE REF TO data.
    DATA dref_itm        TYPE REF TO data.
    DATA dref_fwo        TYPE REF TO data.
    DATA dref_prd        TYPE REF TO data.
    DATA dref_fwo_itm    TYPE REF TO data.
    DATA lr_excl_it_type TYPE RANGE OF /scmtms/trq_item_type.
    DATA dref_zexec      TYPE REF TO data.
    DATA lf_yard_no      TYPE /sapyl/e_yard_no.
    DATA lr_partner      TYPE RANGE OF partner.
    DATA lr_des_loc      TYPE RANGE OF /scmtms/s_trq_root_k-des_loc_id.

    METHODS ____create_fwq_keys
      RETURNING VALUE(rt_trq_key) TYPE /bobf/t_frw_key
      RAISING   /bobf/cx_frw_contrct_violation.

    TYPES lr_key TYPE RANGE OF /bobf/conf_key.
ENDCLASS.
