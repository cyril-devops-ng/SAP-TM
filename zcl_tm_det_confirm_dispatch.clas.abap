class ZCL_TM_DET_CONFIRM_DISPATCH definition
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



CLASS ZCL_TM_DET_CONFIRM_DISPATCH IMPLEMENTATION.
  METHOD /bobf/if_frw_determination~execute.
    " ---------------------------------------------------------------------
    " 10: Check if dispatch confirmation is required
    " ---------------------------------------------------------------------
    IF zcl_tm001_d_ztm_dispatch_date=>gc_dispatch_confirmation = abap_false.
      RETURN.
    ENDIF.

    DATA dref_params TYPE REF TO data.
    FIELD-SYMBOLS <fs_params> TYPE /scmtms/s_trq_a_confirm.
    DATA ls_frc_exc TYPE ztm_fwo_c_excpt.
    CREATE DATA dref_params TYPE ('/SCMTMS/S_TRQ_A_CONFIRM').
    DATA(lf_exception_flag) =
    zcl_tm_behv_validation=>_________check_frc_exceptions( EXPORTING io_read    = io_read
                                                                     is_ctx     = CORRESPONDING #( is_ctx )
                                                                     it_key     = it_key
                                                           IMPORTING es_frc_exc = ls_frc_exc ).
    IF xsdbool( lf_exception_flag = abap_false
                OR ls_frc_exc IS INITIAL ) = abap_true.
      RETURN.
    ENDIF.
    ASSIGN dref_params->* TO <fs_params>.
    ASSIGN COMPONENT 'AUTOMATIC' OF STRUCTURE <fs_params>
           TO FIELD-SYMBOL(<fs_automatic>).
    <fs_automatic> = 'X'.
    ASSIGN COMPONENT 'NO_CHECK' OF STRUCTURE <fs_params>
           TO FIELD-SYMBOL(<fs_nocheck>).
    <fs_nocheck> = 'X'.
    <fs_params> = CORRESPONDING #( BASE ( <fs_params> ) ls_frc_exc
                                      MAPPING
                                        overwrite_conf = overwrite_conf
                                        keep_exist_conf = keep_exist_conf
                                        EXCEPT * ).

    TRY.
*          io_modify->do_action( EXPORTING iv_act_key    = /scmtms/if_trq_c=>sc_action-root-confirm
*                                          is_parameters = dref_params
*                                          it_key        = it_key
*                                IMPORTING eo_message    = eo_message
*                                          et_failed_key = et_failed_key ).
      CATCH /bobf/cx_frw_contrct_violation.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
