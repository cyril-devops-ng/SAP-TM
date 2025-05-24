*&---------------------------------------------------------------------*
*& Include ztm_pdr_top
*&---------------------------------------------------------------------*
TYPE-POOLS zpdr.
" ---------------------------------------------------------------------
" 10: Table Declaration
" ---------------------------------------------------------------------
TABLES: /scmtms/d_trqrot,
        ztm005t_trq_exec,
        /scmtms/d_trqitm.
" ---------------------------------------------------------------------
" 20: Data Declaration
" ---------------------------------------------------------------------
DATA gv_bolno      TYPE ztm049s_manif_itm-zzbolnumber.
DATA gs_header     TYPE ztm049s_header.
DATA gs_param      TYPE sfpoutputparams.
DATA gt_items      TYPE ztm049_item_tt.
DATA gf_form_name  TYPE fpname VALUE 'ZTM049_PDR_FRM'.
DATA gf_func_name  TYPE funcname.
DATA gv_fwq        TYPE string.
DATA gt_alv        TYPE zpdr_ty_alv_tt.
" ---------------------------------------------------------------------
" 30: TM BOR Data
" ---------------------------------------------------------------------
DATA lo_trq_srvmgr TYPE REF TO /bobf/if_tra_service_manager.
DATA lt_trq_selpar TYPE /bobf/t_frw_query_selparam.
" ---------------------------------------------------------------------
" 40: Field symbols
" ---------------------------------------------------------------------
*FIELD-SYMBOLS <fs_trq_fwq>      TYPE /scmtms/t_trq_root_k.
*FIELD-SYMBOLS <fs_trq_fwo>      TYPE /scmtms/t_trq_root_k.
*FIELD-SYMBOLS <fs_fwq_items>    TYPE /scmtms/t_trq_item_k.
*FIELD-SYMBOLS <fs_fwq_products> TYPE /scmtms/t_trq_item_k.
*FIELD-SYMBOLS <fs_fwo_items>    TYPE /scmtms/t_trq_item_k.
*FIELD-SYMBOLS <fs_fwq_exec>     TYPE ztm005_trq_exec_k_tt.
*FIELD-SYMBOLS <fs_fwo_cargo>    TYPE zotr_tt_cargo_details.
*FIELD-SYMBOLS <fs_fwq_party>    TYPE /scmtms/t_trq_party_k.
" ---------------------------------------------------------------------
" 70: ALV Data Declaration
" ---------------------------------------------------------------------
DATA gt_output  TYPE TABLE OF zpdr_ty_alv.
DATA go_alv     TYPE REF TO cl_salv_table.
DATA go_columns TYPE REF TO cl_salv_columns_table.
DATA go_column  TYPE REF TO cl_salv_column_table.
DATA go_layout  TYPE REF TO cl_salv_layout.
DATA go_cont    TYPE REF TO cl_gui_container.
DATA go_select  TYPE REF TO cl_salv_selections.
" ---------------------------------------------------------------------
" 80: Class declarations
" ---------------------------------------------------------------------
CLASS lcl_pdr DEFINITION DEFERRED.

CLASS lcl_pdr DEFINITION.
  PUBLIC SECTION.
    TYPES lr_fwq_no TYPE RANGE OF /scmtms/d_trqrot-trq_id.
    TYPES lr_bl_no  TYPE RANGE OF /scmtms/d_trqitm-zzbolnumber.
    DATA gt_header TYPE zpdr_tm049s_t_header READ-ONLY.
    DATA gt_tures_man  TYPE ztm039_tures_tt READ-ONLY.
    DATA gt_tures_dis  TYPE ztm039_tures_tt READ-ONLY.
    DATA gt_cargoes TYPE zotr_tt_cargo_details READ-ONLY.
    CLASS-METHODS create_pdr_report
      IMPORTING it_fwq_no       TYPE any
                it_bl_no        TYPE any
                if_mantyp       TYPE any
      RETURNING VALUE(r_result) TYPE REF TO lcl_pdr
      RAISING   zcx_tm_form_exception.

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

    METHODS retrieve____freightdocs
      RAISING zcx_tm_form_exception.

    METHODS create_pdr_form_header
      RETURNING VALUE(rt_header) TYPE zpdr_tm049s_t_header
      RAISING   zcx_tm_form_exception.

    METHODS create_pdr_form_items
      RETURNING VALUE(rt_items) TYPE ztm049_item_tt
      RAISING   zcx_tm_form_exception.

    METHODS create_manifest
      IMPORTING is_trq_item TYPE /scmtms/s_trq_item_k
      CHANGING  cs_manifest TYPE ztm049s_manif_itm
                cs_item     TYPE ztm049s_item
      RAISING   zcx_tm_form_exception.

    METHODS create_tures_man
      IMPORTING is_trq_item TYPE /scmtms/s_trq_item_k
                is_manifest TYPE ztm049s_manif_itm
      RAISING   zcx_tm_form_exception.

    METHODS create_discharged_items
      IMPORTING is_trq_item TYPE /scmtms/s_trq_item_k
      CHANGING  cs_manifest TYPE ztm049s_manif_itm
                cs_item     TYPE ztm049s_item
      RAISING   zcx_tm_form_exception.

    METHODS create_manifdisch
      IMPORTING is_discharged TYPE ztm049s_disch_itm optional
                is_manifest   TYPE ztm049s_manif_itm optional
      CHANGING
                cs_item       TYPE ztm049s_item
      RAISING   zcx_tm_form_exception.
    METHODS get_cargo_details
      RETURNING VALUE(rt_cargoes) TYPE zotr_tt_cargo_details
      RAISING   zcx_tm_form_exception.

    methods pdr____modfiy_records
    RAISING zcx_tm_form_exception.

    METHODS on_added_function
      FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

    METHODS on_link_click
      FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
      IMPORTING !row !column.

    METHODS display_form IMPORTING is_output TYPE zpdr_ty_alv
                         RAISING   zcx_tm_form_exception.

    METHODS show_form
      RAISING zcx_tm_form_exception.

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

    DATA  gt_trq_fwq   TYPE /scmtms/t_trq_root_k.
    DATA  gt_trq_fwo   TYPE /scmtms/t_trq_root_k.
    DATA  gt_fwq_items TYPE /scmtms/t_trq_item_k.
    DATA  gt_fwo_items TYPE /scmtms/t_trq_item_k.
    DATA  gt_fwq_exec  TYPE ztm005_trq_exec_k_tt.
    DATA  gt_fwq_party TYPE /scmtms/t_trq_party_k.
    TYPES lr_key TYPE RANGE OF /bobf/conf_key.
ENDCLASS.
