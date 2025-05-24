CLASS zcl_behv_check_fo_cfir DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_v_supercl_simple FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /bobf/if_frw_validation~execute REDEFINITION.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES tt_c_fwo_val TYPE TABLE OF ztm_c_fwo_val WITH DEFAULT KEY.

    DATA gt_c_fwo_val TYPE tt_c_fwo_val.

    CLASS-DATA gt_trq_npa_doc_tl TYPE TABLE OF ztm047_fwo_block.
    CLASS-DATA gt_tor_npa_doc_tl TYPE TABLE OF ztm047_fo_block.

    METHODS get_fwo_val_config
      RETURNING VALUE(rt_c_fwo_val) TYPE tt_c_fwo_val.
ENDCLASS.


CLASS zcl_behv_check_fo_cfir IMPLEMENTATION.
  METHOD /bobf/if_frw_validation~execute.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_root_data     TYPE /scmtms/t_trq_root_k.
    DATA lt_root_data_bi  TYPE /scmtms/t_trq_root_k.
    DATA lo_trq_srvmgr    TYPE REF TO /bobf/if_tra_service_manager.
    DATA lt_tor_root      TYPE /scmtms/t_tor_root_k.
    DATA lt_tor_root_key  TYPE /bobf/t_frw_key.
    DATA lr_tor_root_key  TYPE RANGE OF /bobf/conf_key.
    DATA lt_charge_root   TYPE /scmtms/t_tcc_root_k.
    DATA lf_charge_exists TYPE flag.
    DATA lf_not_invoiced  TYPE flag.

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
    " 50.1: Check if doc type is a pilotage document
    " ---------------------------------------------------------------------
    ASSIGN lt_root_data[ 1 ]-trq_type TO FIELD-SYMBOL(<Fs_trq_type>).
    IF xsdbool( line_exists( gt_trq_npa_doc_tl[ trq_type = <fs_trq_type> ] ) )
       = abap_false.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 60: Get TOR node by association
    " ---------------------------------------------------------------------
    CLEAR lt_tor_root.
    TRY.
        " ---------------------------------------------------------------------
        " 70: Create Instance: trq_srvmgr
        " ---------------------------------------------------------------------
        lo_trq_srvmgr =
        /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_trq_c=>sc_bo_key ).
        " ---------------------------------------------------------------------
        " 80: Retrieve TOR by Association
        " ---------------------------------------------------------------------
        lo_trq_srvmgr->retrieve_by_association(
          EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                    it_key         = it_key
                    iv_association = /scmtms/if_trq_c=>sc_association-root-tor_root
                    iv_fill_data   = abap_true
          IMPORTING et_data        = lt_tor_root ).
        " ---------------------------------------------------------------------
        " 90: Check if tor node exists
        " ---------------------------------------------------------------------
        CLEAR: et_failed_key,
               eo_message.
        " ---------------------------------------------------------------------
        " 100: Get Factory message
        " ---------------------------------------------------------------------
        eo_message = /bobf/cl_frw_factory=>get_message( ).

        IF xsdbool( line_exists( lt_tor_root[ 1 ] ) ) = abap_false.
          MESSAGE e002(ztm_behv_msg) INTO /sapyl/cl_api_message=>sv_message_text.
          /sapyl/cl_helper_base=>add_symsg2bobf( EXPORTING io_message    = eo_message
                                                           iv_node       = is_ctx-node_key
                                                           iv_key        = lt_root_data[ 1 ]-key
                                                 CHANGING  ct_failed_key = et_failed_key ).
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " 110: Get tor root key
        " ---------------------------------------------------------------------
        lt_tor_root_key = VALUE #( FOR <fs> IN lt_tor_root
                                   ( key = <fs>-key ) ).
        " ---------------------------------------------------------------------
        " 120: Get root key selection
        " ---------------------------------------------------------------------
        lr_tor_root_key = VALUE #( FOR <fs> IN lt_tor_root
                                   ( sign   = 'I'
                                     option = 'EQ'
                                     low    = <fs>-key ) ).

        IF xsdbool( line_exists( lr_tor_root_key[ 1 ] ) ) = abap_false.
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " 130: Get root charges
        " ---------------------------------------------------------------------
        /scmtms/cl_tcc_do_helper=>retrive_do_nodes(
          EXPORTING is_ctx      = VALUE #( host_bo_key        = /scmtms/if_tor_c=>sc_bo_key
                                           host_root_node_key = /scmtms/if_tor_c=>sc_node-root )

                    it_root_key = CORRESPONDING #( lt_tor_root_key MAPPING key = key )
          IMPORTING et_do_root  = lt_charge_root ).
        " ---------------------------------------------------------------------
        " 140: Check if any charges exist
        " ---------------------------------------------------------------------
        IF xsdbool( line_exists( lt_charge_root[ 1 ]  ) ) = abap_false.
          MESSAGE e002(ztm_behv_msg) INTO /sapyl/cl_api_message=>sv_message_text.
          /sapyl/cl_helper_base=>add_symsg2bobf( EXPORTING io_message    = eo_message
                                                           iv_node       = is_ctx-node_key
                                                           iv_key        = lt_root_data[ 1 ]-key
                                                 CHANGING  ct_failed_key = et_failed_key ).
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " 150: Check if all charges are cancelled
        " ---------------------------------------------------------------------
        " TODO: variable is assigned but never used (ABAP cleaner)
        LOOP AT lt_charge_root ASSIGNING FIELD-SYMBOL(<fs_charge_root>)
             WHERE cancel_flag <> abap_true.
          lf_charge_exists = abap_true.
        ENDLOOP.
        " ---------------------------------------------------------------------
        " 160: Raise exception if all charges are cancelled
        " ---------------------------------------------------------------------
        IF lf_charge_exists = abap_false.
          MESSAGE e002(ztm_behv_msg) INTO /sapyl/cl_api_message=>sv_message_text.
          /sapyl/cl_helper_base=>add_symsg2bobf( EXPORTING io_message    = eo_message
                                                           iv_node       = is_ctx-node_key
                                                           iv_key        = lt_root_data[ 1 ]-key
                                                 CHANGING  ct_failed_key = et_failed_key ).
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " 170: Check the invoice status is verified or completed
        " ---------------------------------------------------------------------
        LOOP AT lt_charge_root ASSIGNING <fs_charge_root>
             WHERE     cancel_flag <> abap_true
                   AND ( invoicing <> '04' AND invoicing <> '08' ).
          lf_not_invoiced = abap_true.
        ENDLOOP.
        " ---------------------------------------------------------------------
        " 180: Raise exception if not verified or completed
        " ---------------------------------------------------------------------
        IF xsdbool( lf_not_invoiced = 'X' ) = abap_true.
          MESSAGE e002(ztm_behv_msg) INTO /sapyl/cl_api_message=>sv_message_text.
          /sapyl/cl_helper_base=>add_symsg2bobf( EXPORTING io_message    = eo_message
                                                           iv_node       = is_ctx-node_key
                                                           iv_key        = lt_root_data[ 1 ]-key
                                                 CHANGING  ct_failed_key = et_failed_key ).
          RETURN.
        ENDIF.
      CATCH /bobf/cx_frw_contrct_violation.

    ENDTRY.
  ENDMETHOD.

  METHOD get_fwo_val_config.
    " ---------------------------------------------------------------------
    " 10: Retrieve configuration
    " ---------------------------------------------------------------------
    SELECT FROM ztm_c_fwo_val
      FIELDS *
      WHERE billing_check = @abap_true
      INTO TABLE @rt_c_fwo_val.
  ENDMETHOD.

  METHOD class_constructor.
    " ---------------------------------------------------------------------
    " 10: Get TRQ NPA Doc. Types
    " ---------------------------------------------------------------------
    IF gt_trq_npa_doc_tl IS INITIAL.
      SELECT FROM ztm047_fwo_block
        FIELDS *
        INTO TABLE @gt_trq_npa_doc_tl.
    ENDIF.
    " ---------------------------------------------------------------------
    " 20: Get TOR NPA Doc. Types
    " ---------------------------------------------------------------------
    IF gt_tor_npa_doc_tl IS INITIAL.
      SELECT FROM ztm047_fo_block
        FIELDS *
        INTO TABLE @gt_tor_npa_doc_tl.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
