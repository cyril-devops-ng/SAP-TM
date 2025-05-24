*&---------------------------------------------------------------------*
*& Include          ZTM039_P_OUT_TURN_NEW_LCL
*&---------------------------------------------------------------------*
CLASS lcl_otr IMPLEMENTATION.
  METHOD create_otr_report.
    r_result = NEW #( it_fwq_no = it_fwq_no
                      it_bl_no  = it_bl_no
                      if_mantyp = if_mantyp ).

    r_result->retrieve_fwo( ).
  ENDMETHOD.

  METHOD constructor.
    " ---------------------------------------------------------------------
    " 10: Assign Optional params.
    " ---------------------------------------------------------------------
    IF it_fwq_no IS SUPPLIED.
      me->gr_fwq_no = it_fwq_no.
    ENDIF.

    IF it_bl_no IS SUPPLIED.
      me->gr_bl_no = it_bl_no.
    ENDIF.
    " ---------------------------------------------------------------------
    " 20: Assign Mandatory params
    " ---------------------------------------------------------------------
    me->gf_mantyp = if_mantyp.
    IF me->gf_mantyp IS INITIAL.
      RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_null_manifest ).
    ENDIF.
  ENDMETHOD.

  METHOD create_grid.
    " ---------------------------------------------------------------------
    " 10: Create ALV
    " ---------------------------------------------------------------------
    TRY.
        IF go_alv IS INITIAL.
          cl_salv_table=>factory( EXPORTING r_container  = cl_gui_container=>default_screen
                                  IMPORTING r_salv_table = go_alv
                                  CHANGING  t_table      = ct_tab ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 20: Raise Exception
        " ---------------------------------------------------------------------
      CATCH cx_salv_msg INTO DATA(lo_alv_ex).
        RAISE EXCEPTION NEW Zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_alv_error
                                                   previous = lo_alv_ex ).
    ENDTRY.
    " ---------------------------------------------------------------------
    " 40: Set up ALv
    " ---------------------------------------------------------------------
    IF go_alv IS BOUND.
      TRY.
          go_alv->get_functions( )->set_all( abap_true ).

          SET HANDLER on_added_function FOR go_alv->get_event( ).
          SET HANDLER on_link_click     FOR go_alv->get_event( ).
          " ---------------------------------------------------------------------
          " 50: Raise Exception
          " ---------------------------------------------------------------------
        CATCH cx_salv_existing
              cx_salv_wrong_call
              cx_salv_method_not_supported.
          RAISE EXCEPTION NEW Zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_alv_error ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD display_form.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " *********************************************************************
    TYPES tr_fwq_no TYPE RANGE OF /scmtms/d_trqrot-trq_id.
    TYPES tr_bl_no  TYPE RANGE OF ztm039s_manif_itm-zzbolnumber.
    DATA lr_fwq_no TYPE tr_fwq_no.

    " ---------------------------------------------------------------------
    " 20: Check record exists
    " *********************************************************************
    CHECK is_output IS NOT INITIAL.
    " ---------------------------------------------------------------------
    " 30: Print display
    " *********************************************************************
    lr_fwq_no = VALUE #( ( low = is_output-fwq_id sign = 'I' option = 'EQ' ) ).

    SUBMIT ztm039_p_out_turn_new
           WITH p_r_fom = abap_true
           WITH s_fwq_no IN lr_fwq_no
           WITH s_mantyp = me->gf_mantyp
           AND RETURN.
  ENDMETHOD.

  METHOD on_added_function.
  ENDMETHOD.

  METHOD on_link_click.
    " ---------------------------------------------------------------------
    " 10: Check record exists
    " ---------------------------------------------------------------------
    IF NOT line_exists( gt_output[ row ] ).
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 20: Assign record
    " ---------------------------------------------------------------------
    ASSIGN gt_output[ row ] TO FIELD-SYMBOL(<fs>).
    CASE column.
      " ---------------------------------------------------------------------
      " 30: Handle print function
      " *********************************************************************
      WHEN 'PRINT'.
        TRY.
            display_form( <fs> ).
            " ---------------------------------------------------------------------
            " 40: Catch errors
            " *********************************************************************
          CATCH zcx_tm_form_exception INTO DATA(lo_tm_ex).
            DATA(lt_msg) = lcl_otr=>create_bapi_ret_from_exception( lo_tm_ex ).
            lcl_otr=>display_bapi_log_gui( lt_msg ).
        ENDTRY.
    ENDCASE.
  ENDMETHOD.

  METHOD retrieve_fwo.
    FIELD-SYMBOLS <fs_trq_fwq>   TYPE /scmtms/t_trq_root_k.
    FIELD-SYMBOLS <fs_trq_fwo>   TYPE /scmtms/t_trq_root_k.
    FIELD-SYMBOLS <fs_fwq_items> TYPE /scmtms/t_trq_item_k.
    FIELD-SYMBOLS <fs_fwo_items> TYPE /scmtms/t_trq_item_k.
    FIELD-SYMBOLS <fs_fwq_exec>  TYPE ztm005_trq_exec_k_tt.

    TRY.

        " ---------------------------------------------------------------------
        " 10: New. Transaction Manager
        " ---------------------------------------------------------------------
        me->lo_trq_srvmgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
                                iv_bo_key = /scmtms/if_trq_c=>sc_bo_key ).
        " ---------------------------------------------------------------------
        " 20: Create Sel.par
        " ---------------------------------------------------------------------
        fill_selection_params( ).
        " ---------------------------------------------------------------------
        " 30: Create TRQ keys
        " ---------------------------------------------------------------------
        DATA lt_trq_key TYPE /bobf/t_frw_key.
        lt_trq_key = ____create_fwq_keys( ).
        " ---------------------------------------------------------------------
        " 30.1: Assign TRQ data
        " ---------------------------------------------------------------------
        ASSIGN me->dref->* TO <fs_trq_fwq>.

        " ---------------------------------------------------------------------
        " 40: Create Data ref. -> item
        " ---------------------------------------------------------------------
        CREATE DATA me->dref_itm TYPE ('/SCMTMS/T_TRQ_ITEM_K').
        " ---------------------------------------------------------------------
        " 50: Retrieve by association Item node
        " ---------------------------------------------------------------------
        me->lo_trq_srvmgr->retrieve_by_association(
          EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                    it_key         = CORRESPONDING #( lt_trq_key MAPPING key = key )
                    iv_association = /scmtms/if_trq_c=>sc_association-root-item
                    iv_fill_data   = abap_true
          IMPORTING et_data        = me->dref_itm->* ).
        " ---------------------------------------------------------------------
        " 60: Assign Item node data
        " ---------------------------------------------------------------------
        ASSIGN me->dref_itm->* TO <fs_fwq_items>.

        " ---------------------------------------------------------------------
        " 70: Get fwo keys
        " *********************************************************************
        CREATE DATA me->dref_fwo TYPE ('/SCMTMS/T_TRQ_ROOT_K').
        me->lo_trq_srvmgr->retrieve_by_association(
          EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                    it_key         = CORRESPONDING #( lt_trq_key MAPPING key = key )
                    iv_association = /scmtms/if_trq_c=>sc_association-root-trq_root_quot2successor
                    iv_fill_data   = abap_true
          IMPORTING et_data        = me->dref_fwo->* ).
        ASSIGN me->dref_fwo->* TO <fs_trq_fwo>.
        " ---------------------------------------------------------------------
        " 80: Retrieve Allowed FWO types
        " ---------------------------------------------------------------------
        DATA lr_trq_type TYPE RANGE OF /scmtms/trq_type.
        SELECT FROM ztm050t_fwo_typ
          FIELDS 'I'      AS sign,
                 'EQ'     AS option,
                 trq_type AS low
          INTO CORRESPONDING FIELDS OF TABLE @lr_trq_type.
        DELETE <fs_trq_fwo> WHERE trq_type NOT IN lr_trq_type.
        IF lines( <fs_trq_fwo> ) > 500.
          RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_too_many_records ).

        ENDIF.
        " ---------------------------------------------------------------------
        " 90: Retrieve excl. Item type
        " ---------------------------------------------------------------------
        SELECT FROM ztm039t_exc_ityp
          FIELDS 'I'       AS sign,
                 'EQ'      AS option,
                 item_type AS low
          INTO CORRESPONDING FIELDS OF TABLE @me->lr_excl_it_type.
        " ---------------------------------------------------------------------
        " 100: Delete unwanted item node
        " ---------------------------------------------------------------------
        DELETE <fs_fwq_items>
               WHERE
                        zzbolnumber NOT IN
                        s_bolno
                     OR (     zzmanifest_type <> s_mantyp
                          AND zzmanifest_type <> space )
                     OR
                        item_type IN me->lr_excl_it_type.
        " ---------------------------------------------------------------------
        " 110: Raise exception if no relevant items found
        " ---------------------------------------------------------------------
        IF <fs_fwq_items> IS INITIAL.
          RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_record_not_found ).

        ENDIF.
        " ---------------------------------------------------------------------
        " 120 Create Product Data -> ref.
        " ---------------------------------------------------------------------
        CREATE DATA me->dref_prd TYPE ('/SCMTMS/T_TRQ_ITEM_K').
        " ---------------------------------------------------------------------
        " 130 Get products keys/data
        " ---------------------------------------------------------------------
        me->lo_trq_srvmgr->query(
          EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-item-query_by_attributes
                    iv_fill_data            = abap_true
                    it_selection_parameters = VALUE /bobf/t_frw_query_selparam( FOR <fs> IN <fs_fwq_items>
                                                                                ( attribute_name = 'ITEM_PARENT_KEY'
                                                                                  sign           = 'I'
                                                                                  option         = 'EQ'
                                                                                  low            = <fs>-key ) )
          IMPORTING et_data                 = me->dref_prd->* ).

        " ---------------------------------------------------------------------
        " 140: Assign Product node data
        " ---------------------------------------------------------------------
        ASSIGN me->dref_prd->* TO <fs_fwq_products>.
        " ---------------------------------------------------------------------
        " 150: Retrieve FWO node
        " ---------------------------------------------------------------------
        me->lo_trq_srvmgr->query(
          EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                    it_selection_parameters = VALUE /bobf/t_frw_query_selparam( FOR <fsq> IN <fs_trq_fwq>
                                                                                ( attribute_name = 'QUOT_TRQ_KEY'
                                                                                  sign           = 'I'
                                                                                  option         = 'EQ'
                                                                                  low            = <fsq>-key ) )
          IMPORTING et_key                  = DATA(lt_trq_fwo_key) ).
        CREATE DATA me->dref_fwo_itm TYPE ('/SCMTMS/T_TRQ_ITEM_K').
        " ---------------------------------------------------------------------
        " 160: Retrieve by association Item node
        " ---------------------------------------------------------------------
        me->lo_trq_srvmgr->retrieve_by_association(
          EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                    it_key         = CORRESPONDING #( lt_trq_fwo_key MAPPING key = key )
                    iv_association = /scmtms/if_trq_c=>sc_association-root-item
                    iv_fill_data   = abap_true
          IMPORTING et_data        = me->dref_fwo_itm->* ).
        " ---------------------------------------------------------------------
        " 170: Assign Item node data
        " ---------------------------------------------------------------------
        ASSIGN me->dref_fwo_itm->* TO <fs_fwo_items>.
        " ---------------------------------------------------------------------
        " 180: Retrieve excl. Item type
        " ---------------------------------------------------------------------
        SELECT FROM ztm039t_exc_ityp
          FIELDS 'I'       AS sign,
                 'EQ'      AS option,
                 item_type AS low
          INTO CORRESPONDING FIELDS OF TABLE @me->lr_excl_it_type.
        " ---------------------------------------------------------------------
        " 190: Delete unwanted item node
        " ---------------------------------------------------------------------
        DELETE <fs_fwo_items>
               WHERE    item_cat         = 'SRV'
                     OR zzbolnumber NOT IN
                        s_bolno
                     OR zzmanifest_type <> s_mantyp
                     OR
                        item_type       IN me->lr_excl_it_type.
        IF <fs_fwo_items> IS NOT INITIAL.

          DELETE <fs_trq_fwo>
                 WHERE key NOT IN VALUE me->lr_key( FOR <fs> IN <fs_fwo_items>
                                                    ( low = <fs>-parent_key sign = 'I' option = 'EQ' ) ).

        ENDIF.
        CREATE DATA me->dref_zexec TYPE ('ZTM005_TRQ_EXEC_K_TT').
        " ---------------------------------------------------------------------
        " 200: Retrieve by association Execution node
        " ---------------------------------------------------------------------
        me->lo_trq_srvmgr->retrieve_by_association(
          EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                    it_key         = CORRESPONDING #( lt_trq_key MAPPING key = key )
                    iv_association = lcl_otr=>gc_zexec_assoc_key
                    iv_fill_data   = abap_true
          IMPORTING et_data        = me->dref_zexec->* ).
        " ---------------------------------------------------------------------
        " 210: Assign execution data
        " ---------------------------------------------------------------------
        ASSIGN me->dref_zexec->* TO <fs_fwq_exec>.
      CATCH /bobf/cx_frw_contrct_violation INTO DATA(lo_cx_frw).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_record_not_found
                                                   previous = lo_cx_frw ).
      CATCH zcx_tm_form_exception INTO DATA(lo_tm_ex).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_form_proc_error
                                                   previous = lo_tm_ex ).
    ENDTRY.
    " ---------------------------------------------------------------------
    " 220: Check FWO exists
    " ---------------------------------------------------------------------
    IF <fs_trq_fwo> IS NOT ASSIGNED.
      RAISE EXCEPTION NEW zcx_tm_form_exception( textid = zcx_tm_form_exception=>tm_record_not_found ).
    ENDIF.
    " ---------------------------------------------------------------------
    " 230: Create carrier selection
    " ---------------------------------------------------------------------
    lr_partner = VALUE #( FOR <fsp> IN <fs_trq_fwo>
                          ( low = <fsp>-tsp_id sign = 'I' option = 'EQ' ) ).
    " ---------------------------------------------------------------------
    " 240: Create yard selection
    " ---------------------------------------------------------------------
    DATA lr_yard_no TYPE RANGE OF /sapyl/e_yard_no.

    lr_des_loc = VALUE #( FOR <fsp> IN <fs_trq_fwo>
                          ( low = <fsp>-des_loc_id sign = 'I' option = 'EQ' ) ).
    lr_yard_no = VALUE #( FOR <fsp> IN <fs_trq_fwo>
                          ( low = <fsp>-des_loc_id sign = 'I' option = 'EQ' ) ).
    " ---------------------------------------------------------------------
    " 250: Get carriers
    " ---------------------------------------------------------------------
    SELECT FROM but000
      FIELDS partner,
             mc_name1
      WHERE partner
            IN @lr_partner
      INTO TABLE @DATA(lt_carriers).

    " ---------------------------------------------------------------------
    " 260: Get yards
    " ---------------------------------------------------------------------
    SELECT FROM /sapyl/yardno
      FIELDS yard_no    AS yard_no,
             @abap_true AS found
      WHERE yard_no
            IN @lr_yard_no
      INTO TABLE @DATA(lt_yards).

    " ---------------------------------------------------------------------
    " 270: Create output from FWOs
    " ---------------------------------------------------------------------
    " break csayeh.

    LOOP AT <fs_trq_fwo> ASSIGNING FIELD-SYMBOL(<fs_fwo>).
      APPEND INITIAL LINE TO gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
      <fs_output> = VALUE #(
          carrier_id   = <fs_fwo>-tsp_id
          carrier_name = COND #( WHEN line_exists( lt_carriers[ partner = <fs_fwo>-tsp_id ] )
                                 THEN lt_carriers[ partner = <fs_fwo>-tsp_id ]-mc_name1 )
          vessel       = <fs_fwo>-zzresource
          direction    = s_mantyp
          base         = COND #( WHEN line_exists( lt_yards[ yard_no = <fs_fwo>-des_loc_id ] ) AND <fs_fwq_exec> IS NOT INITIAL THEN
                                   |{ <fs_fwo>-des_loc_id(4) }({ <fs_fwo>-des_loc_id+5(3) })|
                                 WHEN <fs_fwo>-src_loc_id IS NOT INITIAL AND xsdbool( line_exists(
                                                                                          lt_yards[
                                                                                              yard_no = <fs_fwo>-des_loc_id ] ) ) = abap_false
                                  AND <fs_fwq_exec>       IS NOT INITIAL THEN
                                   |{ <fs_fwo>-src_loc_id(4) }|
                                 WHEN line_exists( lt_yards[ yard_no = <fs_fwo>-des_loc_id ] ) AND <fs_fwq_exec> IS INITIAL THEN
                                   |{ <fs_fwo>-des_loc_id(4) }|
                                 WHEN <fs_fwo>-src_loc_id IS NOT INITIAL AND xsdbool( line_exists(
                                                                                          lt_yards[
                                                                                              yard_no = <fs_fwo>-des_loc_id ] ) ) = abap_false
                                  AND <fs_fwq_exec>       IS INITIAL THEN
                                   |{ <fs_fwo>-src_loc_id(4) }| )
          fwq_id       = |{ <fs_fwo>-zzfwq_ref_id  ALPHA = OUT }|
          print        = icon_print ).

      IF <fs_fwq_exec> IS INITIAL.
        RETURN.
      ENDIF.
      " ---------------------------------------------------------------------
      " 70: Assign Departure Date
      " ---------------------------------------------------------------------
      SELECT SINGLE FROM /scmtms/d_trqrot
        FIELDS db_key
        WHERE trq_id
              = @<fs_fwo>-zzfwq_ref_id
        INTO @DATA(lf_trq_key).
      IF xsdbool( line_exists( <fs_fwq_exec>[ parent_key = lf_trq_key
                                              event_code = gc_departure_event ] )  )
         = abap_true.
        ASSIGN <fs_fwq_exec>[ parent_key = lf_trq_key
                              event_code = gc_departure_event ]
               TO FIELD-SYMBOL(<fs_departure>).
        <fs_output> = CORRESPONDING #( BASE ( <fs_output> ) <fs_departure>
         MAPPING departure = actual_date ).
      ENDIF.
      " ---------------------------------------------------------------------
      " 80: Assign Arrival Date
      " ---------------------------------------------------------------------
      IF xsdbool( line_exists( <fs_fwq_exec>[ parent_key = lf_trq_key
                                              event_code = gc_arrival_event ] )  )
         = abap_true.
        ASSIGN <fs_fwq_exec>[ parent_key = lf_trq_key
                              event_code = gc_arrival_event ]
               TO FIELD-SYMBOL(<fs_arrival>).
        <fs_output> = CORRESPONDING #( BASE ( <fs_output> ) <fs_arrival>
         MAPPING arrival = actual_date ).
      ENDIF.
    ENDLOOP.
    SORT gt_output BY fwq_id DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_output COMPARING fwq_id.
  ENDMETHOD.

  METHOD ____create_fwq_keys.
    " ---------------------------------------------------------------------
    " 10 Create Data ref. -> root
    " ---------------------------------------------------------------------
    CREATE DATA me->dref TYPE ('/SCMTMS/T_TRQ_ROOT_K').
    " ---------------------------------------------------------------------
    " 20: Retrieve TRQ keys
    " ---------------------------------------------------------------------
    me->lo_trq_srvmgr->query( EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                                        it_selection_parameters = me->lt_trq_selpar
                              IMPORTING et_key                  = rt_trq_key  ).
    " ---------------------------------------------------------------------
    " 30: Retrieve TRQ Root Data
    " ---------------------------------------------------------------------
    me->lo_trq_srvmgr->retrieve( EXPORTING iv_node_key  = /scmtms/if_trq_c=>sc_node-root
                                           it_key       = rt_trq_key
                                           iv_fill_data = abap_true
                                 IMPORTING et_data      = me->dref->* ).
  ENDMETHOD.

  METHOD show_grid.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lo_str TYPE REF TO cl_abap_structdescr.

    " ---------------------------------------------------------------------
    " 20: Create output structure
    " ---------------------------------------------------------------------
    lo_str ?= cl_abap_structdescr=>describe_by_data( VALUE zotr_alv_output( )  ).
    go_columns = go_alv->get_columns( ).
    TRY.
        " ---------------------------------------------------------------------
        " 30: Create field catalog
        " *********************************************************************
        DO lines( lo_str->components ) TIMES.
          ASSIGN lo_str->components[ sy-index ] TO FIELD-SYMBOL(<fs_component>).
          CASE <fs_component>-name.
            WHEN 'PRINT'.
              _thiscolumn.
              go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
              go_column->set_icon( abap_true ).
              go_column->set_short_text( 'Print' ).
              go_column->set_medium_text( 'Print' ).
              go_column->set_long_text( 'Print' ).
            WHEN 'VESSEL'.
              _thiscolumn.
              go_column->set_output_length( 20 ).
            WHEN 'ARRIVAL'.
              _thiscolumn.
              go_column->set_short_text( 'Arrival' ).
              go_column->set_medium_text( 'Arrival' ).
              go_column->set_long_text( 'Arrival' ).
            WHEN 'DEPARTURE'.
              _thiscolumn.
              go_column->set_short_text( 'Departure' ).
              go_column->set_medium_text( 'Departure' ).
              go_column->set_long_text( 'Departure' ).
            WHEN 'CARRIER_NAME'.
              _thiscolumn.
              go_column->set_short_text( 'Carrier' ).
              go_column->set_medium_text( 'Carrier Name' ).
              go_column->set_long_text( 'Carrier Name' ).
            WHEN 'FWQ_ID'.
              _thiscolumn.
              go_column->set_short_text( 'FWQ' ).
              go_column->set_medium_text( 'Forwarding Quotation' ).
              go_column->set_long_text( 'Forwarding Quotationr' ).

          ENDCASE.

        ENDDO.
        " ---------------------------------------------------------------------
        " 40: Create selection
        " ---------------------------------------------------------------------
        go_select = go_alv->get_selections( ).
        IF xsdbool(  go_select IS NOT INITIAL ) = abap_true.
          go_select->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 50: Display ALV
        " *********************************************************************
        go_alv->display( ).

      CATCH cx_salv_not_found INTO DATA(lo_alv_ex).
        RAISE EXCEPTION NEW zcx_tm_form_exception( textid   = zcx_tm_form_exception=>tm_alv_error
                                                   previous = lo_alv_ex ).
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

  METHOD refresh_grid.
    " ---------------------------------------------------------------------
    " 10: Unset report data
    " ---------------------------------------------------------------------
    CLEAR gt_output.
    TRY.
        " ---------------------------------------------------------------------
        " 20: Retrieve report data
        " ---------------------------------------------------------------------
        retrieve_fwo( ).
        " ---------------------------------------------------------------------
        " 30: Handle exception
        " ---------------------------------------------------------------------
      CATCH zcx_tm_form_exception INTO DATA(lo_tm_ex).
        DATA(lt_msg) = lcl_otr=>create_bapi_ret_from_exception( lo_tm_ex ).
        lcl_otr=>display_bapi_log_gui( lt_msg ).
    ENDTRY.
    " ---------------------------------------------------------------------
    " 40: GUI Refresh
    " ---------------------------------------------------------------------
    go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD fill_selection_params.
    IF me->lt_trq_selpar IS INITIAL.
      me->lt_trq_selpar = CORRESPONDING #( me->gr_fwq_no ).
      LOOP AT me->lt_trq_selpar ASSIGNING FIELD-SYMBOL(<fs_selpar>).
        <fs_selpar>-attribute_name = lcl_otr=>trq_id.
      ENDLOOP.
    ENDIF.

    IF lt_trq_selpar IS INITIAL.

      APPEND INITIAL LINE TO me->lt_trq_selpar ASSIGNING <fs_selpar>.
      <fs_selpar> = VALUE #( attribute_name = lcl_otr=>trq_id
                             low            = '*'
                             sign           = 'I'
                             option         = 'CP' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
