class ZCL_BEHV_CHECK_RESET_CONIRM definition
  public
  inheriting from /BOBF/CL_LIB_V_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods /BOBF/IF_FRW_VALIDATION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BEHV_CHECK_RESET_CONIRM IMPLEMENTATION.
  METHOD /bobf/if_frw_validation~execute.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_root_data    TYPE /scmtms/t_trq_root_k.
    DATA lt_root_data_bi TYPE /scmtms/t_trq_root_k.

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
    " 50: Check New FWO?
    " ---------------------------------------------------------------------
    IF NOT line_exists( lt_root_data_bi[ 1 ] ).
      RETURN.
    ENDIF.
    " TODO: variable is assigned but never used (ABAP cleaner)
    ASSIGN lt_root_data_bi[ 1 ] TO FIELD-SYMBOL(<fs_fwo_root_bi>).

    " ---------------------------------------------------------------------
    " 60: Check for FRC Exceptions; do no reset confirmation
    " ---------------------------------------------------------------------
    IF zcl_tm_behv_validation=>_________check_frc_exceptions( io_read = io_read
                                                              is_ctx  = is_ctx
                                                              it_key  = it_key )
       = abap_true.
      APPEND INITIAL LINE TO et_failed_key ASSIGNING FIELD-SYMBOL(<fs_failed_key>).
      <fs_failed_key>-key = <fs_fwo_root>-key.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 60.1: Check for Validation override
    " ---------------------------------------------------------------------
    IF zcl_tm_behv_validation=>__validation_ovr( cl_abap_classdescr=>get_class_name( me ) ).
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 70: Check action type
    " *********************************************************************
    IF xsdbool( is_ctx-act_key = /scmtms/if_trq_c=>sc_action-root-reset_confirmation_status
     AND zcl_tm_reset_confirmation=>gc_reset_confirmation = abap_false ) = abap_true.
      CLEAR: et_failed_key,
             eo_message.
      TRY.
          " ---------------------------------------------------------------------
          " 80: Get Factory message
          " ---------------------------------------------------------------------
          eo_message = /bobf/cl_frw_factory=>get_message( ).

          MESSAGE e007(ztm_behv_msg) WITH |{ <fs_fwo_root>-zzconfirm_date DATE = ENVIRONMENT }| INTO /sapyl/cl_api_message=>sv_message_text.
          /sapyl/cl_helper_base=>add_symsg2bobf( EXPORTING io_message    = eo_message
                                                           iv_node       = is_ctx-node_key
                                                           iv_key        = <fs_fwo_root>-key
                                                 CHANGING  ct_failed_key = et_failed_key ).
          RETURN.
        CATCH /bobf/cx_frw_contrct_violation.
      ENDTRY.
    ENDIF.
    " ---------------------------------------------------------------------
    " 90: Check if user is authorized to reset confirmation status
    " ---------------------------------------------------------------------
    TRY.
        DATA(lf_logged_on_user) = cl_abap_context_info=>get_user_technical_name( ).
      CATCH cx_abap_context_info_error.
        lf_logged_on_user = sy-uname.
    ENDTRY.
    SELECT SINGLE
      FROM pa0105
             INNER JOIN
               ztm_fwo_c_reset ON  ztm_fwo_c_reset~persno
                                   = pa0105~pernr
                               AND pa0105~endda
                                   <= '99991231'
      FIELDS @abap_true
      WHERE usrty  = `0001`
        AND usrid  = @lf_logged_on_user
        AND status = @abap_true
      INTO @DATA(lf_user_authorized).
    IF xsdbool( lf_user_authorized = 'X' )
       = abap_false.
      TRY.
          " ---------------------------------------------------------------------
          " 100: Return messages
          " ---------------------------------------------------------------------
          eo_message = /bobf/cl_frw_factory=>get_message( ).

          MESSAGE e008(ztm_behv_msg) INTO /sapyl/cl_api_message=>sv_message_text.
          /sapyl/cl_helper_base=>add_symsg2bobf( EXPORTING io_message    = eo_message
                                                           iv_node       = is_ctx-node_key
                                                           iv_key        = <fs_fwo_root>-key
                                                 CHANGING  ct_failed_key = et_failed_key ).
        CATCH /bobf/cx_frw_contrct_violation.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
