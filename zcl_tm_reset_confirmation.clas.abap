class ZCL_TM_RESET_CONFIRMATION definition
  public
  inheriting from /BOBF/CL_LIB_A_SUPERCLASS
  final
  create public .

public section.

  class-data GC_RESET_CONFIRMATION type FLAG .

  methods /BOBF/IF_FRW_ACTION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TM_RESET_CONFIRMATION IMPLEMENTATION.
  METHOD /bobf/if_frw_action~execute.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_root_data     TYPE /scmtms/t_trq_root_k.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_root_data_bi  TYPE /scmtms/t_trq_root_k.
    DATA dref_trq_root    TYPE REF TO /scmtms/s_trq_root_k.
    DATA lt_modifications TYPE /bobf/t_frw_modification.
    CONSTANTS lc_conf_date_field     TYPE string VALUE 'ZZCONFIRM_DATE'.
    CONSTANTS lc_disp_date_field     TYPE string VALUE 'ZZDISPATCH_DATE'.
    CONSTANTS lc_dispatched_field    TYPE string VALUE 'ZZDISPATCHED'.
    CONSTANTS lc_dispatched_by_field TYPE string VALUE 'ZZDISPATCHED_BY'.

    gc_reset_confirmation = abap_true.

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
    " 50: Check action
    " ---------------------------------------------------------------------
    CASE is_ctx-act_key.
      WHEN zif_tm001_trq_c=>sc_action-root-ztm_reset_confirmation.
        " ---------------------------------------------------------------------
        " 60: Reset confirmation status
        " *********************************************************************
        TRY.
            io_modify->do_action( EXPORTING iv_act_key    = /scmtms/if_trq_c=>sc_action-root-reset_confirmation_status
                                            it_key        = it_key
                                  IMPORTING eo_message    = eo_message
                                            et_failed_key = et_failed_key ).

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

            APPEND lc_conf_date_field
                   TO <fs_modifications>-changed_fields.
            APPEND lc_dispatched_by_field
                   TO <fs_modifications>-changed_fields.
            APPEND lc_dispatched_field
                   TO <fs_modifications>-changed_fields.
            APPEND lc_disp_date_field
                   TO <fs_modifications>-changed_fields.

            " ---------------------------------------------------------------------
            " 80.5 Assign confirmation date
            " ---------------------------------------------------------------------
            dref_trq_root->zzconfirm_date  = '00000000'.
            dref_trq_root->zzdispatch_date = '00000000'.
            dref_trq_root->zzdispatched    = ''.
            dref_trq_root->zzdispatched_by = ''.
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
          CATCH /bobf/cx_frw_contrct_violation.
        ENDTRY.
    ENDCASE.
    gc_reset_confirmation = abap_false.
  ENDMETHOD.
ENDCLASS.
