CLASS zcl_tm_form_helper DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES bo_type TYPE string.

    CLASS-DATA gf_tor    TYPE bo_type VALUE 'TOR'.
    CLASS-DATA gf_trq    TYPE bo_type VALUE 'TRQ'.
    CLASS-DATA trq_id    TYPE string  VALUE 'TRQ_ID'.
    CLASS-DATA tor_id    TYPE string  VALUE 'TOR_ID'.
    CLASS-DATA trq_root  TYPE string  VALUE '/SCMTMS/T_TRQ_ROOT_K'.
    CLASS-DATA trq_item  TYPE string  VALUE '/SCMTMS/T_TRQ_ITEM_K'.
    CLASS-DATA trq_exec  TYPE string  VALUE 'ZTM005_TRQ_EXEC_K_TT'.
    CLASS-DATA trq_party TYPE string  VALUE '/SCMTMS/T_TRQ_PARTY_K'.

    CLASS-METHODS create_bo_service_manager IMPORTING if_bo_type          TYPE bo_type
                                            RETURNING VALUE(ro_bo_srvmgr) TYPE REF TO /bobf/if_tra_service_manager
                                            RAISING   zcx_tm_form_exception.

    CLASS-METHODS convert_select_opt_to_sel_par IMPORTING if_sel_par_name     TYPE string
                                                          if_fieldname        TYPE fieldname OPTIONAL
                                                          it_sel_options      TYPE ANY TABLE OPTIONAL
                                                          it_table_key        TYPE ANY TABLE OPTIONAL
                                                RETURNING VALUE(rt_sel_param) TYPE /bobf/t_frw_query_selparam
                                                RAISING   zcx_tm_form_exception.

    CLASS-METHODS get___tm_keys IMPORTING if_bo_type     TYPE bo_type                             OPTIONAL
                                          io_bo_srvmgr   TYPE REF TO /bobf/if_tra_service_manager OPTIONAL
                                          it_selparam    TYPE /bobf/t_frw_query_selparam
                                          if_query_key   TYPE /bobf/obm_query_key
                                RETURNING VALUE(rt_keys) TYPE /bobf/t_frw_key
                                RAISING   zcx_tm_form_exception.

    CLASS-METHODS get___tm_data IMPORTING if_bo_type   TYPE bo_type
                                          io_bo_srvmgr TYPE REF TO /bobf/if_tra_service_manager OPTIONAL
                                          it_selparam  TYPE /bobf/t_frw_query_selparam          OPTIONAL
                                          if_query_key TYPE /bobf/obm_query_key
                                          if_node_key  TYPE /bobf/conf_key
                                CHANGING  ct_tab       TYPE ANY TABLE
                                RAISING   zcx_tm_form_exception.

    CLASS-METHODS get___tm_assoc_data IMPORTING if_bo_type   TYPE bo_type
                                                io_bo_srvmgr TYPE REF TO /bobf/if_tra_service_manager OPTIONAL
                                                it_selparam  TYPE /bobf/t_frw_query_selparam          OPTIONAL
                                                it_key       TYPE /bobf/t_frw_key                     OPTIONAL
                                                if_query_key TYPE /bobf/obm_query_key
                                                if_node_key  TYPE /bobf/conf_key
                                                if_assoc_key TYPE /bobf/obm_assoc_key
                                      CHANGING  ct_tab       TYPE ANY TABLE
                                      RAISING   zcx_tm_form_exception.

    CLASS-METHODS get___fwo_cargoes IMPORTING it_fwo_data     TYPE /scmtms/t_trq_root_k
                                    RETURNING VALUE(rt_cargo) TYPE zotr_tt_cargo_details
                                    RAISING   zcx_tm_form_exception.

    CLASS-METHODS get___form_data IMPORTING if_mantype   TYPE /scmtms/d_trqitm-zzmanifest_type
                                            it_fwq_no    TYPE ANY TABLE
                                            it_bl_no     TYPE ANY TABLE
                                  EXPORTING et_trq_fwq   TYPE /scmtms/t_trq_root_k
                                            et_trq_fwo   TYPE /scmtms/t_trq_root_k
                                            et_fwq_items TYPE /scmtms/t_trq_item_k
                                            et_fwo_items TYPE /scmtms/t_trq_item_k
                                            et_fwq_exec  TYPE ztm005_trq_exec_k_tt
                                            et_fwq_party TYPE /scmtms/t_trq_party_k
                                            ef_shipper   TYPE string
                                  RAISING   zcx_tm_form_exception.

    CLASS-METHODS display_bapi_log_gui
      IMPORTING it_bapi_ret TYPE bapiret2_t.

    CLASS-METHODS create_bapi_ret_from_exception
      IMPORTING io_exception  TYPE REF TO cx_root
      RETURNING VALUE(rt_msg) TYPE bapiret2_t.

    CONSTANTS gc_arrival_event   TYPE ztm005e_event_code  VALUE 'Arrival' ##NO_TEXT.
    CONSTANTS gc_departure_event TYPE ztm005e_event_code  VALUE 'Departure' ##NO_TEXT.
    CONSTANTS gc_zexec_assoc_key TYPE /bobf/obm_assoc_key VALUE '5D786398C4FA1EDDAFEB80C51BD3B2BC' ##NO_TEXT.
    CONSTANTS gc_attr_itm_pa_key TYPE string              VALUE 'ITEM_PARENT_KEY'.
    CONSTANTS gc_attr_quot_key   TYPE string              VALUE 'QUOT_TRQ_KEY'.

ENDCLASS.


CLASS zcl_tm_form_helper IMPLEMENTATION.
  METHOD create_bo_service_manager.
    TRY.
        " ---------------------------------------------------------------------
        " 10: Check BOR type
        " ---------------------------------------------------------------------
        CASE if_bo_type.
          " ---------------------------------------------------------------------
          " 20: Check if TOR type
          " ---------------------------------------------------------------------
          WHEN gf_tor.
            ro_bo_srvmgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
                               iv_bo_key = /scmtms/if_tor_c=>sc_bo_key ).
            " ---------------------------------------------------------------------
            " 30: Check if TRQ type
            " ---------------------------------------------------------------------
          WHEN gf_trq.
            ro_bo_srvmgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
                               iv_bo_key = /scmtms/if_trq_c=>sc_bo_key ).
            " ---------------------------------------------------------------------
            " 40: Raise exception if other types
            " ---------------------------------------------------------------------
          WHEN OTHERS.
            RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_invalid_bor ).
        ENDCASE.
        " ---------------------------------------------------------------------
        " 50: Raise exception if contract violation
        " ---------------------------------------------------------------------
      CATCH /bobf/cx_frw_contrct_violation INTO DATA(lo_tm_cx).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_form_proc_error
                                                   previous = lo_tm_cx ).
    ENDTRY.
  ENDMETHOD.

  METHOD convert_select_opt_to_sel_par.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lo_str TYPE REF TO cl_abap_structdescr.

    " ---------------------------------------------------------------------
    " 20: Check that tables are supplied
    " *********************************************************************
    IF xsdbool( it_table_key IS NOT SUPPLIED AND it_sel_options IS NOT SUPPLIED ) = abap_true.
      RETURN.
    ENDIF.

    " ---------------------------------------------------------------------
    " 30: Check if IT_TABLE_KEY is supplied
    " ---------------------------------------------------------------------
    IF xsdbool( it_table_key IS SUPPLIED AND it_table_key IS NOT INITIAL )
       = abap_true.
      DATA dref TYPE REF TO data.
      CREATE DATA dref LIKE LINE OF it_table_key.

      lo_str ?= cl_abap_structdescr=>describe_by_data( dref->* ).
      " ---------------------------------------------------------------------
      " 40: Raise exception if field name is not provided
      " ---------------------------------------------------------------------
      IF xsdbool( if_fieldname IS NOT SUPPLIED OR if_fieldname IS INITIAL ) = abap_true.
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_null_fieldname ).
      ENDIF.
      " ---------------------------------------------------------------------
      " 50: Raise exception if field does not exist in key structure
      " ---------------------------------------------------------------------
      IF xsdbool( line_exists( lo_str->components[ name = if_fieldname ] ) ) = abap_false.
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid       = zcx_tm_form_exception=>tm_field_does_not_exist
                                                   tm_fieldname = CONV #( if_fieldname ) ).
      ENDIF.
      " ---------------------------------------------------------------------
      " 60: Create selection parameter
      " ---------------------------------------------------------------------
      LOOP AT it_table_key ASSIGNING FIELD-SYMBOL(<fs_table_key>).
        ASSIGN COMPONENT if_fieldname OF STRUCTURE <fs_table_key> TO FIELD-SYMBOL(<fs_key_field>).
        APPEND INITIAL LINE TO rt_sel_param ASSIGNING FIELD-SYMBOL(<fs_sel_param>).
        <fs_sel_param> = VALUE #( attribute_name = if_sel_par_name
                                  sign           = 'I'
                                  option         = 'EQ'
                                  low            = <fs_key_field> ).
      ENDLOOP.
    ELSE.
      " ---------------------------------------------------------------------
      " 70: Check if IT_SEL_OPTIONS is supplied
      " ---------------------------------------------------------------------
      IF it_sel_options IS NOT INITIAL.
        rt_sel_param = CORRESPONDING #( it_sel_options ).
        LOOP AT rt_sel_param ASSIGNING FIELD-SYMBOL(<fs_selpar>).
          <fs_selpar>-attribute_name = if_sel_par_name.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get___tm_keys.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " *********************************************************************
    DATA lo_srvmgr TYPE REF TO /bobf/if_tra_service_manager.

    TRY.
        " ---------------------------------------------------------------------
        " 20: If transaction mgr is provided
        " *********************************************************************
        IF xsdbool( io_bo_srvmgr IS BOUND AND  io_bo_srvmgr IS NOT INITIAL )
           = abap_true.
          lo_srvmgr = io_bo_srvmgr.
          " ---------------------------------------------------------------------
          " 30: If transaction mgr is not provided
          " *********************************************************************
        ELSE.
          IF if_bo_type IS NOT SUPPLIED.
            RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_invalid_bor ).
          ENDIF.
          " ---------------------------------------------------------------------
          " 40: Create BO Service manager
          " ---------------------------------------------------------------------
          lo_srvmgr = create_bo_service_manager( if_bo_type = if_bo_type  ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 50: If BO service manager is bound
        " ---------------------------------------------------------------------
        IF lo_srvmgr IS BOUND.
          " ---------------------------------------------------------------------
          " 60: Query
          " ---------------------------------------------------------------------
          lo_srvmgr->query( EXPORTING iv_query_key            = if_query_key
                                      it_selection_parameters = it_selparam
                            IMPORTING et_key                  = rt_keys ).
        ENDIF.
      CATCH /bobf/cx_frw_contrct_violation INTO DATA(lo_tm_cx).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_form_proc_error
                                                   previous = lo_tm_cx ).
    ENDTRY.
  ENDMETHOD.

  METHOD get___tm_data.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " *********************************************************************
    DATA lo_srvmgr TYPE REF TO /bobf/if_tra_service_manager ##NEEDED.

    TRY.
        " ---------------------------------------------------------------------
        " 20: Get Transaction manager
        " ---------------------------------------------------------------------
        lo_srvmgr = COND #( WHEN io_bo_srvmgr IS SUPPLIED AND io_bo_srvmgr IS BOUND
                            THEN io_bo_srvmgr
                            ELSE create_bo_service_manager( if_bo_type = if_bo_type  ) ).
        " ---------------------------------------------------------------------
        " 30: Get keys
        " ---------------------------------------------------------------------
        DATA(lt_keys) = get___tm_keys( if_bo_type   = if_bo_type
                                       io_bo_srvmgr = lo_srvmgr
                                       it_selparam  = it_selparam
                                       if_query_key = if_query_key ).
        " ---------------------------------------------------------------------
        " 40: Raise exception if keys is empty
        " ---------------------------------------------------------------------
        IF lt_keys IS INITIAL.
          RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_null_keys ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 50: Retrieve data
        " ---------------------------------------------------------------------
        lo_srvmgr->retrieve( EXPORTING iv_node_key  = if_node_key
                                       it_key       = lt_keys
                                       iv_fill_data = abap_true
                             IMPORTING et_data      = ct_tab ).
        " ---------------------------------------------------------------------
        " 60: Catch TM errors
        " ---------------------------------------------------------------------
      CATCH /bobf/cx_frw_contrct_violation INTO DATA(lo_frw_ex).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_null_data
                                                   previous = lo_frw_ex ).
        " ---------------------------------------------------------------------
        " 70: Catch form exceptions
        " *********************************************************************
      CATCH zcx_tm_form_exception INTO DATA(lo_form_ex).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_null_data
                                                   previous = lo_form_ex ).
    ENDTRY.
  ENDMETHOD.

  METHOD create_bapi_ret_from_exception.
    " ---------------------------------------------------------------------
    " 10: Create BAPI return from Exception
    " ---------------------------------------------------------------------
    CALL FUNCTION 'RS_EXCEPTION_TO_BAPIRET2'
      EXPORTING i_r_exception = io_exception
      CHANGING  c_t_bapiret2  = rt_msg.
  ENDMETHOD.

  METHOD display_bapi_log_gui.
    " ---------------------------------------------------------------------
    " 10: Display Bapi return
    " ---------------------------------------------------------------------
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING it_message = it_bapi_ret.
  ENDMETHOD.

  METHOD get___tm_assoc_data.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " *********************************************************************
    DATA lo_srvmgr TYPE REF TO /bobf/if_tra_service_manager ##NEEDED.

    TRY.
        " ---------------------------------------------------------------------
        " 20: Get Transaction manager
        " ---------------------------------------------------------------------
        lo_srvmgr = COND #( WHEN io_bo_srvmgr IS SUPPLIED AND io_bo_srvmgr IS BOUND
                            THEN io_bo_srvmgr
                            ELSE create_bo_service_manager( if_bo_type = if_bo_type  ) ).
        " ---------------------------------------------------------------------
        " 30: Get keys
        " ---------------------------------------------------------------------
        DATA(lt_keys) = get___tm_keys( if_bo_type   = if_bo_type
                                       io_bo_srvmgr = lo_srvmgr
                                       it_selparam  = it_selparam
                                       if_query_key = if_query_key ).
        IF it_key IS NOT INITIAL.
          lt_keys = it_key.
        ENDIF.
        " ---------------------------------------------------------------------
        " 40: Raise exception if keys is empty
        " ---------------------------------------------------------------------
        IF lt_keys IS INITIAL.
          RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_null_keys ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 50: Retrieve Associated data
        " ---------------------------------------------------------------------
        lo_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = if_node_key
                                                      it_key         = lt_keys
                                                      iv_fill_data   = abap_true
                                                      iv_association = if_assoc_key
                                            IMPORTING et_data        = ct_tab ).
        " ---------------------------------------------------------------------
        " 60: Catch TM errors
        " ---------------------------------------------------------------------
      CATCH /bobf/cx_frw_contrct_violation INTO DATA(lo_frw_ex).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_null_data
                                                   previous = lo_frw_ex ).
        " ---------------------------------------------------------------------
        " 70: Catch form exceptions
        " *********************************************************************
      CATCH zcx_tm_form_exception INTO DATA(lo_form_ex).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_null_data
                                                   previous = lo_form_ex ).
    ENDTRY.
  ENDMETHOD.

  METHOD get___fwo_cargoes.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lf_cargo_not_found TYPE xfeld.

    " ---------------------------------------------------------------------
    " 20: Check all FWO for this FWQ
    " ---------------------------------------------------------------------
    LOOP AT it_fwo_data ASSIGNING FIELD-SYMBOL(<fs_fwo>).
      lf_cargo_not_found = abap_false.
      " ---------------------------------------------------------------------
      " 30: Create cargo record and assign fwo number
      " ---------------------------------------------------------------------
      APPEND INITIAL LINE TO rt_cargo ASSIGNING FIELD-SYMBOL(<fs_cargo>).
      <fs_cargo>-trq_id = <fs_fwo>-trq_id.
      " ---------------------------------------------------------------------
      " 40: Retrieve cargo details and assign to cargo record
      " ---------------------------------------------------------------------
      CALL FUNCTION 'ZYL028_GET_CARGO_DETAILS'
        EXPORTING iv_fwo_no         = <fs_cargo>-trq_id
        IMPORTING et_tu_output      = <fs_cargo>-cargo_summary
                  et_tu_output_cont = <fs_cargo>-cargo_container
                  ev_yo_not_found   = lf_cargo_not_found.
      " ---------------------------------------------------------------------
      " 50: convert cargo not found to cargo exists
      " ---------------------------------------------------------------------
      <fs_cargo>-cargo_exists = COND #( WHEN lf_cargo_not_found = abap_true THEN abap_false ELSE abap_true ).
      " ---------------------------------------------------------------------
      " 60: Assign container unit
      " ---------------------------------------------------------------------
      LOOP AT <fs_cargo>-cargo_container ASSIGNING FIELD-SYMBOL(<fs_container>).
        <fs_container>-product_qty_total = 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get___form_data.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lr_excl_it_type TYPE RANGE OF /scmtms/trq_item_type.
    TYPES lr_key TYPE RANGE OF /bobf/conf_key.

    TRY.
        " ---------------------------------------------------------------------
        " 20: Get TM selection parameter
        " ---------------------------------------------------------------------
        DATA(lt_sel_par) =
         zcl_tm_form_helper=>convert_select_opt_to_sel_par( if_sel_par_name = trq_id
                                                            it_sel_options  = it_fwq_no ).
        " ---------------------------------------------------------------------
        " 30: If selection parameter is initial
        " ---------------------------------------------------------------------
        IF lt_sel_par IS INITIAL.
          APPEND INITIAL LINE TO lt_sel_par ASSIGNING FIELD-SYMBOL(<fs_selpar>).
          <fs_selpar> = VALUE #( attribute_name = trq_id
                                 low            = '*'
                                 sign           = 'I'
                                 option         = 'CP' ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 40: Get FWQ data
        " ---------------------------------------------------------------------
        zcl_tm_form_helper=>get___tm_data( EXPORTING if_bo_type   = zcl_tm_form_helper=>gf_trq
                                                     it_selparam  = lt_sel_par
                                                     if_node_key  = /scmtms/if_trq_c=>sc_node-root
                                                     if_query_key = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                                           CHANGING  ct_tab       = et_trq_fwq ).
        " ---------------------------------------------------------------------
        " 50: Max 500 records can be displayed on ALV
        " ---------------------------------------------------------------------
        IF lines( et_trq_fwq ) > 500.
          RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_too_many_records ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 60: Get FWQ Items
        " ---------------------------------------------------------------------
        zcl_tm_form_helper=>get___tm_assoc_data(
          EXPORTING if_bo_type   = zcl_tm_form_helper=>gf_trq
                    it_selparam  = lt_sel_par
                    if_node_key  = /scmtms/if_trq_c=>sc_node-root
                    if_assoc_key = /scmtms/if_trq_c=>sc_association-root-item
                    if_query_key = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
          CHANGING  ct_tab       = et_fwq_items ).
        " ---------------------------------------------------------------------
        " 60.1: Get excluded item types
        " ---------------------------------------------------------------------
        SELECT FROM ztm049t_exc_ityp
          FIELDS 'I'       AS sign,
                 'EQ'      AS option,
                 item_type AS low
          INTO TABLE @lr_excl_it_type.
        " ---------------------------------------------------------------------
        " 60.2: Delete excluded items
        " ---------------------------------------------------------------------
        DELETE et_fwq_items
               WHERE
                        zzbolnumber NOT IN
                        it_bl_no
                     OR (     zzmanifest_type <> if_mantype
                          AND zzmanifest_type <> space )
                     OR
                        item_type IN lr_excl_it_type.
        " ---------------------------------------------------------------------
        " 70: Get FWO Data
        " ---------------------------------------------------------------------
        zcl_tm_form_helper=>get___tm_assoc_data(
          EXPORTING if_bo_type   = zcl_tm_form_helper=>gf_trq
                    it_selparam  = lt_sel_par
                    if_node_key  = /scmtms/if_trq_c=>sc_node-root
                    if_assoc_key = /scmtms/if_trq_c=>sc_association-root-trq_root_quot2successor
                    if_query_key = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
          CHANGING  ct_tab       = et_trq_fwo ).
        " ---------------------------------------------------------------------
        " 70.1: Get excluded fwo types
        " ---------------------------------------------------------------------
        DATA lr_trq_type TYPE RANGE OF /scmtms/trq_type.
        SELECT FROM ztm050t_fwo_typ
          FIELDS 'I'      AS sign,
                 'EQ'     AS option,
                 trq_type AS low
          INTO CORRESPONDING FIELDS OF TABLE @lr_trq_type.
        " ---------------------------------------------------------------------
        " 70.2: Delete excluded fwo
        " ---------------------------------------------------------------------
        DELETE et_trq_fwo WHERE trq_type NOT IN lr_trq_type.
        " ---------------------------------------------------------------------
        " 80: Create Selection parameters for fwq key
        " ---------------------------------------------------------------------
        DATA(lt_quot_keys) = zcl_tm_form_helper=>convert_select_opt_to_sel_par(
                                 if_sel_par_name = zcl_tm_form_helper=>gc_attr_quot_key
                                 if_fieldname    = `KEY`
                                 it_table_key    = et_trq_fwq ).
        " ---------------------------------------------------------------------
        " 90: Get FWO Items
        " ---------------------------------------------------------------------
        if et_trq_fwo is not initial.
        zcl_tm_form_helper=>get___tm_assoc_data(
          EXPORTING if_bo_type   = zcl_tm_form_helper=>gf_trq
                    it_selparam  = lt_quot_keys
                    if_node_key  = /scmtms/if_trq_c=>sc_node-root
                    if_assoc_key = /scmtms/if_trq_c=>sc_association-root-item
                    if_query_key = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
          CHANGING  ct_tab       = et_fwo_items ).
        endif.
        " ---------------------------------------------------------------------
        " 90.1: Get excluded items
        " ---------------------------------------------------------------------
        SELECT FROM ztm049t_exc_ityp
          FIELDS 'I'       AS sign,
                 'EQ'      AS option,
                 item_type AS low
          INTO CORRESPONDING FIELDS OF TABLE @lr_excl_it_type.
        " ---------------------------------------------------------------------
        " 90.2: Delete excluded items
        " ---------------------------------------------------------------------
        DELETE et_fwo_items
               WHERE    item_cat         = 'SRV'
                     OR zzbolnumber NOT IN
                        it_bl_no
                     OR zzmanifest_type <> if_mantype
                     OR
                        item_type       IN lr_excl_it_type.
        IF et_fwo_items IS NOT INITIAL.
          " ---------------------------------------------------------------------
          " 90.3: Delete exlcluded FWO
          " ---------------------------------------------------------------------
          DELETE et_trq_fwo
                 WHERE key NOT IN VALUE lr_key( FOR <fs> IN et_fwo_items
                                                ( low = <fs>-parent_key sign = 'I' option = 'EQ' ) ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 100: Get execution data
        " ---------------------------------------------------------------------
        zcl_tm_form_helper=>get___tm_assoc_data(
          EXPORTING if_bo_type   = zcl_tm_form_helper=>gf_trq
                    if_query_key = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                    if_node_key  = /scmtms/if_trq_c=>sc_node-root
                    if_assoc_key = zcl_tm_form_helper=>gc_zexec_assoc_key
                    it_key       = CORRESPONDING #( et_trq_fwq MAPPING key = key )
          CHANGING  ct_tab       = et_fwq_exec ).
        " ---------------------------------------------------------------------
        " 110: Get party data
        " ---------------------------------------------------------------------
        zcl_tm_form_helper=>get___tm_assoc_data(
          EXPORTING if_bo_type   = zcl_tm_form_helper=>gf_trq
                    if_query_key = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                    if_node_key  = /scmtms/if_trq_c=>sc_node-root
                    if_assoc_key = /scmtms/if_trq_c=>sc_association-root-party
                    it_key       = CORRESPONDING #( et_trq_fwq MAPPING key = key )
          CHANGING  ct_tab       = et_fwq_party ).
        " ---------------------------------------------------------------------
        " 120: Get party name
        " ---------------------------------------------------------------------
        IF et_fwq_party IS NOT INITIAL.
          SELECT FROM but000
            FIELDS partner,
                   mc_name1
            FOR ALL ENTRIES IN
            @et_fwq_party
            WHERE partner
                  = @et_fwq_party-party_id
            INTO TABLE @DATA(lt_shipper).
        ENDIF.
        " ---------------------------------------------------------------------
        " 130: Assign Shipper partner type name
        " ---------------------------------------------------------------------
        ef_shipper = space.
        IF line_exists( et_fwq_party[ party_rco = 'Z5' ] ).
          ASSIGN et_fwq_party[ party_rco = 'Z5' ] TO FIELD-SYMBOL(<fs_partner_type>).
          IF line_exists( lt_shipper[ partner = <fs_partner_type>-party_id ] ).
            ef_shipper = ef_shipper && lt_shipper[ partner = <fs_partner_type>-party_id ]-mc_name1.
          ENDIF.
        ENDIF.
        " ---------------------------------------------------------------------
        " 140: Assign Shipper name
        " ---------------------------------------------------------------------
        IF line_exists( et_fwq_party[ party_rco = 'Z6' ] ).
          ASSIGN et_fwq_party[ party_rco = 'Z6' ] TO <fs_partner_type>.
          IF line_exists( lt_shipper[ partner = <fs_partner_type>-party_id ] ).
            ef_shipper = |{ ef_shipper }/{ lt_shipper[ partner = <fs_partner_type>-party_id ]-mc_name1 }|.
          ENDIF.
        ENDIF.
        " ---------------------------------------------------------------------
        " 150: Catch exceptions
        " ---------------------------------------------------------------------
      CATCH /bobf/cx_frw_contrct_violation INTO DATA(lo_cx_frw).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_record_not_found
                                                   previous = lo_cx_frw ).
      CATCH zcx_tm_form_exception INTO DATA(lo_tm_ex).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_form_proc_error
                                                   previous = lo_tm_ex ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
