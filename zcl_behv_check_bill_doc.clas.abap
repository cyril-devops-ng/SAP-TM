CLASS zcl_behv_check_bill_doc DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_v_supercl_simple FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /bobf/if_frw_validation~execute REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES tt_c_fwo_val TYPE TABLE OF ztm_c_fwo_val WITH DEFAULT KEY.

    METHODS get_fwo_val_config RETURNING VALUE(rt_c_fwo_val) TYPE tt_c_fwo_val.

    DATA gt_c_fwo_val TYPE tt_c_fwo_val.
ENDCLASS.



CLASS ZCL_BEHV_CHECK_BILL_DOC IMPLEMENTATION.
  METHOD /bobf/if_frw_validation~execute.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_root_data     TYPE /scmtms/t_trq_root_k.
    DATA lt_root_data_bi  TYPE /scmtms/t_trq_root_k.
    DATA lo_trq_srvmgr    TYPE REF TO /bobf/if_tra_service_manager.
    DATA lt_cfir_root     TYPE /scmtms/t_cfir_root_node_k.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_trq_cfir_link TYPE /bobf/t_frw_key_link.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_cfir_root_key TYPE /bobf/t_frw_key.
    DATA lr_cfir_root_key TYPE RANGE OF /bobf/conf_key.
    DATA lt_k_root        TYPE /bobf/t_frw_key.
    DATA ls_k_read_req    TYPE /scmtms/cl_trq_data=>ty_s_k_read_req.
    DATA lt_k_read_req    TYPE /scmtms/cl_trq_data=>ty_t_k_read_req.
    DATA lt_k_read_req_bi TYPE /scmtms/cl_trq_data=>ty_t_k_read_req.
    DATA mo_data          TYPE REF TO /scmtms/cl_trq_data.
    DATA mo_data_bi       TYPE REF TO /scmtms/cl_trq_data.

    " ---------------------------------------------------------------------
    " 20: Get configuration data
    " ---------------------------------------------------------------------
    me->gt_c_fwo_val =
        get_fwo_val_config( ).
    " ---------------------------------------------------------------------
    " 30: Retrieve current node data
    " ---------------------------------------------------------------------
    io_read->retrieve( EXPORTING iv_node      = is_ctx-node_key
                                 it_key       = it_key
                                 iv_fill_data = abap_true
                       IMPORTING et_data      = lt_root_data ).
    " ---------------------------------------------------------------------
    " 40: Retrieve before image node data
    " ---------------------------------------------------------------------
    io_read->retrieve( EXPORTING iv_node         = is_ctx-node_key
                                 it_key          = it_key
                                 iv_fill_data    = abap_true
                                 iv_before_image = abap_true
                       IMPORTING et_data         = lt_root_data_bi ).
    " ---------------------------------------------------------------------
    " 50: Check if its a new FWO/FWQ/Trans.Rqmnts
    " ---------------------------------------------------------------------
    IF xsdbool( line_exists( lt_root_data_bi[ 1 ] ) ) = abap_false.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 50.1: Check that Billing Doc Check is active for doc type/category
    " ---------------------------------------------------------------------
    IF xsdbool( line_exists( lt_root_data[ 1 ] ) ) = abap_true.
      IF NOT line_exists( me->gt_c_fwo_val[ type          = lt_root_data[ 1 ]-trq_type
                                            category      = lt_root_data[ 1 ]-trq_cat
                                            billing_check = abap_true ] ).
        RETURN.
      ENDIF.
    ENDIF.

    " ---------------------------------------------------------------------
    " 60: Get CFIR node by association
    " ---------------------------------------------------------------------
    CLEAR:
        lt_cfir_root,
        lt_trq_cfir_link.
    TRY.
        " ---------------------------------------------------------------------
        " 70: Create Instance: trq_srvmgr
        " ---------------------------------------------------------------------
        TRY.
            lo_trq_srvmgr =
            /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_trq_c=>sc_bo_key ).
            " ---------------------------------------------------------------------
            " 80: Retrieve Cust.Freight.Invoice.Requiest by Association
            " ---------------------------------------------------------------------

            lo_trq_srvmgr->retrieve_by_association(
              EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                        it_key         = it_key
                        iv_association = /scmtms/if_trq_c=>sc_association-root-cfir_root
                        iv_fill_data   = abap_true
              IMPORTING et_data        = lt_cfir_root
                        et_key_link    = lt_trq_cfir_link ).
          CATCH /bobf/cx_frw_contrct_violation.
          CATCH cx_root.
        ENDTRY.

        " ---------------------------------------------------------------------
        " 90: Check if cfir node exists
        " ---------------------------------------------------------------------

        IF xsdbool( line_exists( lt_cfir_root[ 1 ] ) ) = abap_false.
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " 100: Get cfir root key
        " ---------------------------------------------------------------------
        lt_cfir_root_key = VALUE #( FOR <fs> IN lt_cfir_root
                                    ( key = <fs>-key ) ).
        " ---------------------------------------------------------------------
        " 110: Get root key selection
        " ---------------------------------------------------------------------
        lr_cfir_root_key = VALUE #( FOR <fs> IN lt_cfir_root
                                    ( sign   = 'I'
                                      option = 'EQ'
                                      low    = <fs>-key ) ).

        IF xsdbool( line_exists( lr_cfir_root_key[ 1 ] ) ) = abap_false.
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " 110.1 Create TRQ data keys
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
        " 110.2: Get trq instance
        " ---------------------------------------------------------------------
        mo_data = /scmtms/cl_trq_data=>get_instance( it_k_read_req = lt_k_read_req
                                                     io_read       = io_read ).
        " ---------------------------------------------------------------------
        " 110.3: Get trq before instance
        " ---------------------------------------------------------------------
        mo_data_bi = /scmtms/cl_trq_data=>get_instance( it_k_read_req = lt_k_read_req_bi
                                                        io_read       = io_read ).
        " ---------------------------------------------------------------------
        " 110.3: No relevant changes exit
        " ---------------------------------------------------------------------
        IF xsdbool( mo_data->mt_d_trq_item[] = mo_data_bi->mt_d_trq_item[] )
           = abap_true.
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " 120: Check for billing document
        " ---------------------------------------------------------------------
        SELECT SINGLE FROM /scmtms/d_cf_doc
          FIELDS btd_id,
                 @abap_true
          WHERE parent_key IN
                @lr_cfir_root_key
            AND fwsd_btd_status = `01`
            AND btd_tco         = `28`
          INTO ( @DATA(lf_billing_doc), @DATA(lf_billing_doc_exists) ).
        " ---------------------------------------------------------------------
        " 130: Check if billing document exists
        " ---------------------------------------------------------------------
        IF xsdbool( lf_billing_doc_exists = abap_true AND lf_billing_doc IS NOT INITIAL ) = abap_true.
          " ---------------------------------------------------------------------
          " 140: Unset failed key, message
          " ---------------------------------------------------------------------
          CLEAR: et_failed_key,
                 eo_message.
          " ---------------------------------------------------------------------
          " 150: Get Factory message
          " ---------------------------------------------------------------------
          eo_message = /bobf/cl_frw_factory=>get_message( ).
          " ---------------------------------------------------------------------
          " 160: Get frq doc
          " ---------------------------------------------------------------------
          DATA lf_fwo_doc TYPE string.
          lf_fwo_doc = lt_root_data[ 1 ]-trq_id.
          lf_fwo_doc = |{ lf_fwo_doc ALPHA = OUT }|.
          " ---------------------------------------------------------------------
          " 170: Message creation
          " ---------------------------------------------------------------------
          MESSAGE e001(ztm_behv_msg) WITH lf_fwo_doc INTO /sapyl/cl_api_message=>sv_message_text.
          /sapyl/cl_helper_base=>add_symsg2bobf( EXPORTING io_message    = eo_message
                                                           iv_node       = is_ctx-node_key
                                                           iv_key        = lt_root_data[ 1 ]-key
                                                 CHANGING  ct_failed_key = et_failed_key ).
        ENDIF.

      CATCH /bobf/cx_frw_contrct_violation INTO DATA(lo_cx_frw). " TODO: variable is assigned but never used (ABAP cleaner)

    ENDTRY.
    " ---------------------------------------------------------------------
    " 180: Unset billing doc & flag
    " ---------------------------------------------------------------------
    CLEAR: lf_billing_doc,
           lf_billing_doc_exists.
  ENDMETHOD.


  METHOD get_fwo_val_config.
    " ---------------------------------------------------------------------
    " 10: Retrieve configuration
    " ---------------------------------------------------------------------
    SELECT
      FROM ztm_c_fwo_val
      FIELDS *
      WHERE billing_check = @abap_true
      INTO TABLE @rt_c_fwo_val.
  ENDMETHOD.
ENDCLASS.
