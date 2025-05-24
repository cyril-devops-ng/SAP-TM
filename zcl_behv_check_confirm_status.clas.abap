CLASS zcl_behv_check_confirm_status DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_v_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    METHODS /bobf/if_frw_validation~execute
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
  class-data: gt_frc_exceptions type table of ztm_fwo_c_excpt.
ENDCLASS.



CLASS ZCL_BEHV_CHECK_CONFIRM_STATUS IMPLEMENTATION.


  METHOD CLASS_CONSTRUCTOR.
    gt_frc_exceptions = zcl_tm_behv_validation=>_________frc_exceptions(  ).
  ENDMETHOD.

  METHOD /bobf/if_frw_validation~execute.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_root_data     TYPE /scmtms/t_trq_root_k.
    DATA lt_root_data_bi  TYPE /scmtms/t_trq_root_k.
    DATA lt_k_root        TYPE /bobf/t_frw_key.
    DATA ls_k_read_req    TYPE /scmtms/cl_trq_data=>ty_s_k_read_req.
    DATA lt_k_read_req    TYPE /scmtms/cl_trq_data=>ty_t_k_read_req.
    DATA lt_k_read_req_bi TYPE /scmtms/cl_trq_data=>ty_t_k_read_req.
    DATA mo_data          TYPE REF TO /scmtms/cl_trq_data.
    DATA mo_data_bi       TYPE REF TO /scmtms/cl_trq_data.

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
    IF <fs_fwo_root>-trq_cat <> zcl_tm_behv_validation=>fwo_cat.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 50: Check New FWO?
    " ---------------------------------------------------------------------
    IF NOT line_exists( lt_root_data_bi[ 1 ] ).
      RETURN.
    ENDIF.
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    ASSIGN lt_root_data_bi[ 1 ] TO FIELD-SYMBOL(<fs_fwo_root_bi>).

    " ---------------------------------------------------------------------
    " 60: Check for FRC Exceptions
    " ---------------------------------------------------------------------
    IF zcl_tm_behv_validation=>_________check_frc_exceptions( io_read = io_read
                                                              is_ctx  = is_ctx
                                                              it_key  = it_key )
       = abap_true.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 60.1: Check for Validation override
    " ---------------------------------------------------------------------
    IF zcl_tm_behv_validation=>__validation_ovr( cl_abap_classdescr=>get_class_name( me ) ).
      RETURN.
    ENDIF.

    " ---------------------------------------------------------------------
    " 70. Create TRQ data keys
    " ---------------------------------------------------------------------
    lt_k_root = it_key.
    ls_k_read_req-node_key = /scmtms/if_trq_c=>sc_node-root.
    ls_k_read_req-read_req = /scmtms/cl_trq_data=>sc_read_request-d_root.
    ls_k_read_req-t_key    = lt_k_root.
    INSERT ls_k_read_req INTO TABLE lt_k_read_req.
    INSERT ls_k_read_req INTO TABLE lt_k_read_req_bi.

    ls_k_read_req-read_req   = /scmtms/cl_trq_data=>sc_read_request-d_stage.
    ls_k_read_req-assoc_key  = /scmtms/if_trq_c=>sc_association-root-stage.
    ls_k_read_req-table_type = '/SCMTMS/T_TRQ_STAGE_K'.
    INSERT ls_k_read_req INTO TABLE lt_k_read_req.
    INSERT ls_k_read_req INTO TABLE lt_k_read_req_bi.

    ls_k_read_req-read_req   = /scmtms/cl_trq_data=>sc_read_request-d_item.
    ls_k_read_req-assoc_key  = /scmtms/if_trq_c=>sc_association-root-item.
    ls_k_read_req-table_type = '/SCMTMS/T_TRQ_ITEM_K'.
    INSERT ls_k_read_req INTO TABLE lt_k_read_req.
    INSERT ls_k_read_req INTO TABLE lt_k_read_req_bi.
    " ---------------------------------------------------------------------
    " 80: Get trq instance
    " ---------------------------------------------------------------------
    mo_data = /scmtms/cl_trq_data=>get_instance( it_k_read_req = lt_k_read_req
                                                 io_read       = io_read ).
    " ---------------------------------------------------------------------
    " 90: Get trq before instance
    " ---------------------------------------------------------------------
    mo_data_bi = /scmtms/cl_trq_data=>get_instance( it_k_read_req   = lt_k_read_req_bi
                                                    io_read         = io_read
                                                    iv_before_image = abap_true ).
    " ---------------------------------------------------------------------
    " 100: Check for changes in item instance
    " ---------------------------------------------------------------------
    IF xsdbool( mo_data->mt_d_trq_item[] <> mo_data_bi->mt_d_trq_item[]
    AND zcl_tm_reset_confirmation=>gc_reset_confirmation = abap_false
    AND <fs_fwo_root>-confirmation = '02' ) = abap_true.
      CLEAR: et_failed_key,
             eo_message.

      TRY.
          " ---------------------------------------------------------------------
          " 110: Get Factory message
          " ---------------------------------------------------------------------
          eo_message = /bobf/cl_frw_factory=>get_message( ).

          MESSAGE e007(ztm_behv_msg) WITH |{ <fs_fwo_root>-zzconfirm_date DATE = ENVIRONMENT }| INTO /sapyl/cl_api_message=>sv_message_text.
          /sapyl/cl_helper_base=>add_symsg2bobf( EXPORTING io_message    = eo_message
                                                           iv_node       = is_ctx-node_key
                                                           iv_key        = <fs_fwo_root>-key
                                                 CHANGING  ct_failed_key = et_failed_key ).

        CATCH /bobf/cx_frw_contrct_violation.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
