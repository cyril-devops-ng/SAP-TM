class ZCL_TM_DET_CONFIRMATION_DATE definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TM_DET_CONFIRMATION_DATE IMPLEMENTATION.
  METHOD /bobf/if_frw_determination~execute.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_root_data     TYPE /scmtms/t_trq_root_k.
    DATA lt_root_data_bi  TYPE /scmtms/t_trq_root_k.
    DATA dref_trq_root    TYPE REF TO /scmtms/s_trq_root_k.
    DATA lt_modifications TYPE /bobf/t_frw_modification.
    CONSTANTS lc_change_field TYPE string VALUE 'ZZCONFIRM_DATE'.

    " ---------------------------------------------------------------------
    " 20: Retrieve current node data
    " ---------------------------------------------------------------------
    io_read->retrieve( EXPORTING iv_node      = is_ctx-node_key
                                 it_key       = it_key
                                 iv_fill_data = abap_true
                       IMPORTING et_data      = lt_root_data ).
    " ---------------------------------------------------------------------
    " 30: Retrieve before image node data
    " ---------------------------------------------------------------------
    io_read->retrieve( EXPORTING iv_node         = is_ctx-node_key
                                 it_key          = it_key
                                 iv_fill_data    = abap_true
                                 iv_before_image = abap_true
                       IMPORTING et_data         = lt_root_data_bi ).
    " ---------------------------------------------------------------------
    " 40: Check document type is FWO
    " ---------------------------------------------------------------------
    IF NOT line_exists( lt_root_data[ 1 ] ).
      RETURN.
    ENDIF.
    ASSIGN lt_root_data[ 1 ] TO FIELD-SYMBOL(<fs_fwo_root>).
    IF <fs_fwo_root>-trq_cat <> '03'.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 50: Get before image confirmation status
    " ---------------------------------------------------------------------
    IF line_exists( lt_root_data_bi[ 1 ] ).
      " TODO: variable is assigned but never used (ABAP cleaner)
      ASSIGN lt_root_data_bi[ 1 ] TO FIELD-SYMBOL(<fs_fwo_root_bi>).
    ENDIF.
    " ---------------------------------------------------------------------
    " 60: Check for FRC Exceptions
    " ---------------------------------------------------------------------
    IF zcl_tm_behv_validation=>_________check_frc_exceptions( io_read = io_read
                                                              is_ctx  = CORRESPONDING #( is_ctx )
                                                              it_key  = it_key )
       = abap_true.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 70: Get etag
    " ---------------------------------------------------------------------
    GET TIME STAMP FIELD DATA(lf_etag_time).
    TRY.
        CONVERT TIME STAMP lf_etag_time
                TIME ZONE cl_abap_context_info=>get_user_time_zone( )
                INTO DATE DATA(lf_confirmation_date).
      CATCH cx_abap_context_info_error.
        lf_confirmation_date = sy-datum.
    ENDTRY.
    " ---------------------------------------------------------------------
    " 70.1 Check dispatch date and confirmation date is set
    " ---------------------------------------------------------------------
    IF xsdbool(  <fs_fwo_root>-zzdispatch_date IS NOT INITIAL AND <fs_fwo_root>-zzconfirm_date IS NOT INITIAL
   and <fs_fwo_root_bi> is assigned )
       = abap_true AND zcl_tm_reset_confirmation=>gc_reset_confirmation = abap_false.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 80: Check confirmation status
    " ---------------------------------------------------------------------
    IF xsdbool( <fs_fwo_root>-confirmation = `01` ) = abap_true.
      " ---------------------------------------------------------------------
      " 80.1 Unset confirmation date
      " ---------------------------------------------------------------------
      FREE lf_confirmation_date.
    ENDIF.
    "
    FREE dref_trq_root.
    " ---------------------------------------------------------------------
    " 80.2 Assign current node data to data ref
    " *********************************************************************
    CREATE DATA dref_trq_root.
    dref_trq_root->* = <fs_fwo_root>.
    " ---------------------------------------------------------------------
    " 80.3 Create modifications
    " *********************************************************************
    APPEND INITIAL LINE TO lt_modifications ASSIGNING
           FIELD-SYMBOL(<fs_modifications>).
    " ---------------------------------------------------------------------
    " 80.4 Set changed fields
    " *********************************************************************
    IF xsdbool( line_exists( <fs_modifications>-changed_fields[ 1 ] ) )
       = abap_false.
      INSERT lc_change_field
             INTO TABLE <fs_modifications>-changed_fields.
    ENDIF.
    " ---------------------------------------------------------------------
    " 80.5 Assign confirmation date
    " ---------------------------------------------------------------------
    dref_trq_root->zzconfirm_date = lf_confirmation_date.
    " ---------------------------------------------------------------------
    " 80.6 Assign change key and data
    " ---------------------------------------------------------------------
    <fs_modifications>-node        = is_ctx-node_key.
    <fs_modifications>-key         = dref_trq_root->key.
    <fs_modifications>-data        = dref_trq_root.
    " ---------------------------------------------------------------------
    " 80.7 Set change mode
    " ---------------------------------------------------------------------
    <fs_modifications>-change_mode = /bobf/if_frw_c=>sc_modify_update.
    " ---------------------------------------------------------------------
    " 90: Do modification
    " ---------------------------------------------------------------------
    TRY.
        io_modify->do_modify( lt_modifications ).
      CATCH /bobf/cx_frw_contrct_violation.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
