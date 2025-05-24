*&---------------------------------------------------------------------*
*& Include ztm_pdr_lcl
*&---------------------------------------------------------------------*
CLASS lcl_pdr IMPLEMENTATION.
  METHOD create_pdr_report.
    " ---------------------------------------------------------------------
    " 10: PDR Factory
    " *********************************************************************
    r_result = NEW #( it_fwq_no = it_fwq_no
                      it_bl_no  = it_bl_no
                      if_mantyp = if_mantyp ).
    r_result->retrieve____freightdocs( ).
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
    " 30: Set up ALv
    " ---------------------------------------------------------------------
    IF go_alv IS BOUND.
      TRY.
          go_alv->get_functions( )->set_all( abap_true ).

          SET HANDLER on_added_function FOR go_alv->get_event( ).
          SET HANDLER on_link_click     FOR go_alv->get_event( ).
          " ---------------------------------------------------------------------
          " 40: Raise Exception
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

    SUBMIT ztm_pdr
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
      "---------------------------------------------------------------------
      WHEN 'PRINT'.
        TRY.
            display_form( <fs> ).
            " ---------------------------------------------------------------------
            " 40: Catch errors
            " ---------------------------------------------------------------------
          CATCH zcx_tm_form_exception INTO DATA(lo_tm_ex).
            DATA(lt_msg) = zcl_tm_form_helper=>create_bapi_ret_from_exception( lo_tm_ex ).
            zcl_tm_form_helper=>display_bapi_log_gui( lt_msg ).
        ENDTRY.
    ENDCASE.
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
        retrieve____freightdocs( ).
        " ---------------------------------------------------------------------
        " 30: Handle exception
        " ---------------------------------------------------------------------
      CATCH zcx_tm_form_exception INTO DATA(lo_tm_ex).
        DATA(lt_msg) = zcl_tm_form_helper=>create_bapi_ret_from_exception( lo_tm_ex ).
        zcl_tm_form_helper=>display_bapi_log_gui( lt_msg ).
    ENDTRY.
    " ---------------------------------------------------------------------
    " 40: GUI Refresh
    " ---------------------------------------------------------------------
    go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD retrieve____freightdocs.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    FIELD-SYMBOLS <fs_trq_fwq>   TYPE /scmtms/t_trq_root_k.
    FIELD-SYMBOLS <fs_trq_fwo>   TYPE /scmtms/t_trq_root_k.
    FIELD-SYMBOLS <fs_fwq_items> TYPE /scmtms/t_trq_item_k.
    FIELD-SYMBOLS <fs_fwo_items> TYPE /scmtms/t_trq_item_k.
    FIELD-SYMBOLS <fs_fwq_exec>  TYPE ztm005_trq_exec_k_tt.
    FIELD-SYMBOLS <fs_fwq_party> TYPE /scmtms/t_trq_party_k.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lf_shipper     TYPE string.
    " ---------------------------------------------------------------------
    " 20: Create empty data ref.
    " ---------------------------------------------------------------------
    DATA dref_fwq       TYPE REF TO data.
    DATA dref_fwo       TYPE REF TO data.
    DATA dref_fwq_items TYPE REF TO data.
    DATA dref_fwo_items TYPE REF TO data.
    DATA dref_fwq_exec  TYPE REF TO data.
    DATA dref_fwq_party TYPE REF TO data.

    CREATE DATA dref_fwq TYPE ('/SCMTMS/T_TRQ_ROOT_K').
    ASSIGN dref_fwq->* TO <fs_trq_fwq>.
    CREATE DATA dref_fwo TYPE ('/SCMTMS/T_TRQ_ROOT_K').
    ASSIGN dref_fwo->* TO <fs_trq_fwo>.
    CREATE DATA dref_fwq_items TYPE ('/SCMTMS/T_TRQ_ITEM_K').
    ASSIGN dref_fwq_items->* TO <fs_fwq_items>.
    CREATE DATA dref_fwo_items TYPE ('/SCMTMS/T_TRQ_ITEM_K').
    ASSIGN dref_fwo_items->* TO <fs_fwo_items>.
    CREATE DATA dref_fwq_exec TYPE ('ZTM005_TRQ_EXEC_K_TT').
    ASSIGN dref_fwq_exec->* TO <fs_fwq_exec>.
    CREATE DATA dref_fwq_party TYPE ('/SCMTMS/T_TRQ_PARTY_K').
    ASSIGN dref_fwq_party->* TO <fs_fwq_party>.
    " ---------------------------------------------------------------------
    " 30: Get form data
    " ---------------------------------------------------------------------
    zcl_tm_form_helper=>get___form_data( EXPORTING if_mantype   = me->gf_mantyp
                                                   it_fwq_no    = me->gr_fwq_no[]
                                                   it_bl_no     = me->gr_bl_no[]
                                         IMPORTING et_trq_fwq   = <fs_trq_fwq>
                                                   et_trq_fwo   = <fs_trq_fwo>
                                                   et_fwq_items = <fs_fwq_items>
                                                   et_fwo_items = <fs_fwo_items>
                                                   et_fwq_exec  = <fs_fwq_exec>
                                                   et_fwq_party = <fs_fwq_party>
                                                   ef_shipper   = lf_shipper ).
    " ---------------------------------------------------------------------
    " 40: Assign class global attributes
    " ---------------------------------------------------------------------
    me->gt_trq_fwq   = CORRESPONDING #( <fs_trq_fwq> ).
    me->gt_trq_fwo   = CORRESPONDING #( <fs_trq_fwo> ).
    me->gt_fwq_items = CORRESPONDING #( <fs_fwq_items> ).
    me->gt_fwo_items = CORRESPONDING #( <fs_fwo_items> ).
    me->gt_fwq_exec  = CORRESPONDING #( <fs_fwq_exec> ).
    me->gt_fwq_party = CORRESPONDING #( <fs_fwq_party> ).

    " ---------------------------------------------------------------------
    " 50: create pdr header data
    " ---------------------------------------------------------------------
    DATA(lt_pdr_header) = create_pdr_form_header( ).
    IF lt_pdr_header IS NOT INITIAL.
      me->gt_header = lt_pdr_header.
    ENDIF.
    " ---------------------------------------------------------------------
    " 60: Create ALV output record
    " ---------------------------------------------------------------------
    LOOP AT gt_trq_fwq ASSIGNING FIELD-SYMBOL(<fs_fwq>).
      IF line_exists( lt_pdr_header[ fwq_id = |{  <fs_fwq>-trq_id ALPHA = OUT }|  ] ).
        ASSIGN lt_pdr_header[ fwq_id = |{  <fs_fwq>-trq_id ALPHA = OUT }|  ] TO FIELD-SYMBOL(<fs_pdr_header>).
        IF xsdbool( <fs_pdr_header>-vessel IS NOT INITIAL AND <fs_pdr_header>-carrier_name IS NOT INITIAL ) = abap_true.
          APPEND INITIAL LINE TO gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
          <fs_output> = CORRESPONDING #( <fs_pdr_header> ).
          <fs_output>-print = icon_layout_control.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD show_grid.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lo_str TYPE REF TO cl_abap_structdescr.

    " ---------------------------------------------------------------------
    " 20: Create output structure
    " ---------------------------------------------------------------------
    lo_str ?= cl_abap_structdescr=>describe_by_data( VALUE zpdr_ty_alv( )  ).
    go_columns = go_alv->get_columns( ).
    TRY.
        " ---------------------------------------------------------------------
        " 30: Create field catalog
        " *********************************************************************
        DO lines( lo_str->components ) TIMES.
          ASSIGN lo_str->components[ sy-index ] TO FIELD-SYMBOL(<fs_component>).
          CASE <fs_component>-name.
            WHEN 'PRINT'.
              zpdr_thiscolumn.
              go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
              go_column->set_icon( abap_true ).
              go_column->set_short_text( 'Print' ).
              go_column->set_medium_text( 'Print' ).
              go_column->set_long_text( 'Print' ).
            WHEN 'VESSEL'.
              zpdr_thiscolumn.
              go_column->set_output_length( 20 ).
            WHEN 'ARRIVAL'.
              zpdr_thiscolumn.
              go_column->set_short_text( 'Arrival' ).
              go_column->set_medium_text( 'Arrival' ).
              go_column->set_long_text( 'Arrival' ).
            WHEN 'DEPARTURE'.
              zpdr_thiscolumn.
              go_column->set_short_text( 'Departure' ).
              go_column->set_medium_text( 'Departure' ).
              go_column->set_long_text( 'Departure' ).
            WHEN 'CARRIER_NAME'.
              zpdr_thiscolumn.
              go_column->set_short_text( 'Carrier' ).
              go_column->set_medium_text( 'Carrier Name' ).
              go_column->set_long_text( 'Carrier Name' ).
            WHEN 'FWQ_ID'.
              zpdr_thiscolumn.
              go_column->set_short_text( 'FWQ' ).
              go_column->set_medium_text( 'Forwarding Quotation' ).
              go_column->set_long_text( 'Forwarding Quotation' ).
            WHEN 'BASE'.
            WHEN OTHERS.
              zpdr_thiscolumn.
              go_column->set_visible( abap_false ).
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

  METHOD create_pdr_form_header.
    DATA lf_shipper TYPE string.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lr_partner TYPE RANGE OF partner.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lr_des_loc TYPE RANGE OF /scmtms/s_trq_root_k-des_loc_id.

    lr_partner = VALUE #( FOR <fsp> IN me->gt_trq_fwq
                          ( low = <fsp>-tsp_id sign = 'I' option = 'EQ' ) ).
    " ---------------------------------------------------------------------
    " 240: Create yard selection
    " ---------------------------------------------------------------------
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lr_yard_no TYPE RANGE OF /sapyl/e_yard_no.

    lr_des_loc = VALUE #( FOR <fsp> IN  me->gt_trq_fwq
                          ( low = <fsp>-des_loc_id sign = 'I' option = 'EQ' ) ).
    lr_yard_no = VALUE #( FOR <fsp> IN me->gt_trq_fwq
                          ( low = <fsp>-des_loc_id sign = 'I' option = 'EQ' ) ).

    IF me->gt_fwq_party IS NOT INITIAL.
      SELECT FROM but000
        FIELDS partner,
               mc_name1
        FOR ALL ENTRIES IN
        @me->gt_fwq_party
        WHERE partner
              = @me->gt_fwq_party-party_id
        INTO TABLE @DATA(lt_shipper).
    ENDIF.

    CLEAR lf_shipper.
    LOOP AT gt_trq_fwq ASSIGNING FIELD-SYMBOL(<fs_fwq>).
      IF line_exists( gt_fwq_party[ party_rco  = 'Z5'
                                    parent_key = <fs_fwq>-key ] ) ##NEEDED.
        ASSIGN gt_fwq_party[ party_rco  = 'Z5'
                             parent_key = <fs_fwq>-key ] TO FIELD-SYMBOL(<fs_partner_type>) ##NEEDED.
        IF line_exists( lt_shipper[ partner = <fs_partner_type>-party_id ] ).
          lf_shipper = lf_shipper && lt_shipper[ partner = <fs_partner_type>-party_id ]-mc_name1.
        ENDIF.
      ENDIF.
      " ---------------------------------------------------------------------
      " 90: Assign Shipper name
      " *********************************************************************
      IF line_exists( gt_fwq_party[ party_rco  = 'Z6'
                                    parent_key = <fs_fwq>-key ] ) ##NEEDED.
        ASSIGN gt_fwq_party[ party_rco  = 'Z6'
                             parent_key = <fs_fwq>-key ] TO <fs_partner_type> ##NEEDED.
        IF line_exists( lt_shipper[ partner = <fs_partner_type>-party_id ] ).
          lf_shipper = |{ lf_shipper }/{ lt_shipper[ partner = <fs_partner_type>-party_id ]-mc_name1 }|.
        ENDIF.
      ENDIF.
      " ---------------------------------------------------------------------
      " 120: Get Carrier Name
      " ---------------------------------------------------------------------
      SELECT SINGLE FROM but000
        FIELDS mc_name1
        WHERE partner = @<fs_fwq>-tsp_id
        INTO @DATA(lf_carrier_name).
      " ---------------------------------------------------------------------
      " 130: Check Destination location is a yard
      " ---------------------------------------------------------------------
      lf_yard_no = <fs_fwq>-des_loc_id.
      SELECT SINGLE FROM /sapyl/yardno
        FIELDS @abap_true
        WHERE yard_no = @lf_yard_no
        INTO @DATA(lf_yard_exists).

      " ---------------------------------------------------------------------
      " 140: Assign Header fields
      " ---------------------------------------------------------------------
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA dref_fwq_exec TYPE REF TO data.
      DATA ls_exec       LIKE LINE OF gt_fwq_exec.
      CREATE DATA dref_fwq_exec TYPE ('ZTM005_TRQ_EXEC_K_TT').
      IF line_exists( gt_fwq_exec[ parent_key = <fs_fwq>-key ] ).
        ls_exec = gt_fwq_exec[ parent_key = <fs_fwq>-key ].
      ENDIF.
      APPEND INITIAL LINE TO rt_header ASSIGNING FIELD-SYMBOL(<fs_header>).
      <fs_header> =  VALUE #(
          carrier_id   = <fs_fwq>-tsp_id
          carrier_name = COND #( WHEN lf_shipper IS NOT INITIAL THEN lf_shipper ELSE lf_carrier_name )
          vessel       = <fs_fwq>-zzresource
          direction    = s_mantyp
          base         = COND #( WHEN lf_yard_exists = abap_true AND ls_exec IS NOT INITIAL THEN
                                   |{ <fs_fwq>-des_loc_id(4) }| "({ <fs_fwq>-des_loc_id+5(3) })|
                                 WHEN <fs_fwq>-src_loc_id IS NOT INITIAL AND lf_yard_exists = abap_false
                                  AND ls_exec             IS NOT INITIAL THEN
                                   |{ <fs_fwq>-src_loc_id(4) }|
                                 WHEN lf_yard_exists = abap_true AND ls_exec IS INITIAL THEN
                                   |{ <fs_fwq>-des_loc_id(4) }|
                                 WHEN <fs_fwq>-src_loc_id IS NOT INITIAL AND lf_yard_exists = abap_false
                                  AND ls_exec             IS INITIAL THEN
                                   |{ <fs_fwq>-src_loc_id(4) }| )
          fwq_id       = |{ <fs_fwq>-trq_id  ALPHA = OUT }| ).

      " ---------------------------------------------------------------------
      " 160: Assign Departure Date
      " ---------------------------------------------------------------------
      IF xsdbool( line_exists( gt_fwq_exec[ parent_key = <fs_fwq>-key
                                            event_code = gc_departure_event ] )  )
         = abap_true.
        ASSIGN gt_fwq_exec[ parent_key = <fs_fwq>-key
                            event_code = gc_departure_event ]
               TO FIELD-SYMBOL(<fs_departure>).
        <fs_header> = CORRESPONDING #( BASE ( <fs_header> ) <fs_departure>
         MAPPING departure = actual_date ).
      ENDIF.
      " ---------------------------------------------------------------------
      " 170: Assign Arrival Date
      " ---------------------------------------------------------------------
      IF xsdbool( line_exists( gt_fwq_exec[ parent_key = <fs_fwq>-key
                                            event_code = gc_arrival_event ] )  )
         = abap_true.
        ASSIGN gt_fwq_exec[ parent_key = <fs_fwq>-key
                            event_code = gc_arrival_event ]
               TO FIELD-SYMBOL(<fs_arrival>).
        <fs_header> = CORRESPONDING #( BASE ( <fs_header> ) <fs_arrival>
         MAPPING arrival = actual_date ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_pdr_form_items.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA ls_manifest TYPE ztm049s_manif_itm.

    " ---------------------------------------------------------------------
    " 10: Get Cargoes
    " ---------------------------------------------------------------------
    me->gt_cargoes = get_cargo_details( ).
    " ---------------------------------------------------------------------
    " 20: Get cont./pkg
    " ---------------------------------------------------------------------
    DATA lt_trq_items TYPE /scmtms/t_trq_item_k.
    lt_trq_items = CORRESPONDING #( me->gt_fwq_items ).
    DELETE lt_trq_items WHERE item_type = 'PRD'.
    " BREAK CSAYEH.
    LOOP AT lt_trq_items ASSIGNING FIELD-SYMBOL(<fs_item>).
      " ---------------------------------------------------------------------
      " 100: Create empty item line
      " ---------------------------------------------------------------------

      IF xsdbool( line_exists( gt_items[ zzbolnumber = <fs_item>-zzbolnumber
                                         zzcust      = <fs_item>-zzcust
                                         zzpol       = <fs_item>-zzpol
                                         "fwo_key     = <fs_item>-key
                                         ] ) )
         = abap_false.
        APPEND INITIAL LINE TO gt_items ASSIGNING FIELD-SYMBOL(<fs_items>).
        " ---------------------------------------------------------------------
        " 110: Assign bl/cust/pol
        " ---------------------------------------------------------------------
        <fs_items> = CORRESPONDING #( <fs_item> MAPPING
                                                zzbolnumber = zzbolnumber
                                                zzcust      = zzcust
                                                zzpol       = zzpol
                                                fwo_key = key

                                                EXCEPT * ).
      ELSE.
        <fs_items> = gt_items[ zzbolnumber = <fs_item>-zzbolnumber
                               zzcust      = <fs_item>-zzcust
                               zzpol       = <fs_item>-zzpol
                               "fwo_key     = <fs_item>-key
                               ].
       <fs_items>-fwo_key = <fs_item>-key.
      ENDIF.

      " ---------------------------------------------------------------------
      " 120: Unset Manifest
      " ---------------------------------------------------------------------
      CLEAR ls_manifest.
      " ---------------------------------------------------------------------
      " 130: Create Manifest
      " ---------------------------------------------------------------------
      create_manifest( EXPORTING is_trq_item = <fs_item>
                       CHANGING  cs_manifest = ls_manifest
                                 cs_item     = <fs_items> ).
*    CATCH zcx_tm_form_exception.
      " ---------------------------------------------------------------------
      " 140: Create Tures Man
      " ---------------------------------------------------------------------
      create_tures_man( is_trq_item = <fs_item>
                        is_manifest = ls_manifest ).
*    CATCH zcx_tm_form_exception..
      " ---------------------------------------------------------------------
      " 150: ZGC summary
      " ---------------------------------------------------------------------
      IF xsdbool( <fs_item>-item_type = 'ZGC' ) = abap_true.
        <fs_items>-tonne_car_manif     += ls_manifest-tonne.
        <fs_items>-qua_car_val_man     += ls_manifest-qua_pcs_val.
        <fs_items>-gro_car_val_man     += ls_manifest-gro_wei_val.
        <fs_items>-gro_car_vol_val_man += ls_manifest-gro_vol_val.
      ENDIF.
      " ---------------------------------------------------------------------
      " 160: Other summary
      " ---------------------------------------------------------------------
      IF xsdbool(  <fs_item>-item_type <> 'ZGC'  AND <fs_item>-item_type <> 'BLK' ) = abap_true.
        <fs_items>-tonne_manif     += ls_manifest-tonne.
        <fs_items>-qua_pcs_val_man += ls_manifest-qua_pcs_val.
        <fs_items>-gro_wei_val_man += ls_manifest-gro_wei_val.
        <fs_items>-gro_vol_val_man += ls_manifest-gro_vol_val.

      ENDIF.
    ENDLOOP.
    SORT gt_items BY zzbolnumber
                     zzcust.
    DELETE ADJACENT DUPLICATES FROM gt_items COMPARING zzbolnumber zzcust.
    " ---------------------------------------------------------------------
    " 170: Add Consignee name
    " ---------------------------------------------------------------------
    LOOP AT gt_items ASSIGNING FIELD-SYMBOL(<fs_t_items>).
      SELECT SINGLE FROM but000
        FIELDS mc_name1
        WHERE partner
              = @<fs_t_items>-zzcust
        INTO @<fs_t_items>-custname.
    ENDLOOP.
  ENDMETHOD.

  METHOD show_form.
    " ---------------------------------------------------------------------
    " 10: Get function module name from form name
    " ---------------------------------------------------------------------
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING i_name     = gf_form_name
      IMPORTING e_funcname = gf_func_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 20: Open Form job
    " ---------------------------------------------------------------------
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING   ie_outputparams = gs_param
      EXCEPTIONS cancel          = 1
                 usage_error     = 2
                 system_error    = 3
                 internal_error  = 4
                 OTHERS          = 5.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 30: Call form function module
    " ---------------------------------------------------------------------
    CALL FUNCTION gf_func_name
      EXPORTING is_header    = gs_header
                it_items     = gt_items
                it_tures_man = gt_tures_man
                it_tures_dis = gt_tures_dis.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 40: Close Form job
    " ---------------------------------------------------------------------
    CALL FUNCTION 'FP_JOB_CLOSE'.
  ENDMETHOD.

  METHOD get_cargo_details.
    " ---------------------------------------------------------------------
    " 10: Create cargo not found
    " ---------------------------------------------------------------------
    DATA lf_cargo_not_found TYPE xfeld.
    DATA dref               TYPE REF TO data.
    FIELD-SYMBOLS <fs_fwo_cargo> TYPE zotr_tt_cargo_details.

    " ---------------------------------------------------------------------
    " 20: Check all FWO for this FWQ
    " ---------------------------------------------------------------------
    LOOP AT me->gt_trq_fwo ASSIGNING FIELD-SYMBOL(<fs_fwo>).
      lf_cargo_not_found = abap_false.
      " ---------------------------------------------------------------------
      " 30: Create cargo record and assign fwo number
      " ---------------------------------------------------------------------
      CREATE DATA dref TYPE zotr_tt_cargo_details.
      IF <fs_fwo_cargo> IS NOT ASSIGNED.
        ASSIGN dref->* TO <fs_fwo_cargo>.
      ENDIF.
      APPEND INITIAL LINE TO rt_cargoes ASSIGNING FIELD-SYMBOL(<fs_cargo>).
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

  METHOD create_manifest.
    " ---------------------------------------------------------------------
    " 10: Assign Manifest from item node
    " ---------------------------------------------------------------------
    cs_manifest = CORRESPONDING #( is_trq_item
                    MAPPING zzstorage_type = zzstorage_type
                            zzstorage_section = zzstorage_section
                            qua_pcs_val = qua_pcs_val
                            qua_pcs_uni = qua_pcs_uni
                            gro_wei_val = gro_wei_val
                            gro_wei_uni = gro_wei_uni
                            gro_vol_val = gro_vol_val
                            gro_vol_uni = gro_vol_uni
                            tures_tco = tures_tco
                            db_key = key
                            item_descr = item_descr
                            zzfrzone = zzfrzone
                            platenumber = platenumber
                            zzbolnumber = zzbolnumber
                            zzcust = zzcust
                            zzpol = zzpol
                            EXCEPT * ).
    " ---------------------------------------------------------------------
    " 20: Assign parent node
    " ---------------------------------------------------------------------
    ASSIGN me->gt_fwq_items[ item_parent_key = is_trq_item-key ] TO FIELD-SYMBOL(<fs_parent>) ##SHADOW.
    " ---------------------------------------------------------------------
    " 30: Check item type
    " ---------------------------------------------------------------------
    CASE is_trq_item-item_type.
      " ---------------------------------------------------------------------
      " 40: When item type is ZGC
      " ---------------------------------------------------------------------
      WHEN 'ZGC'.
        IF <fs_parent> IS ASSIGNED.
          cs_manifest-qua_pcs_val = <fs_parent>-qua_pcs_val.
          cs_manifest-gro_vol_val = <fs_parent>-gro_vol_val.
          cs_manifest-tonne       = COND #( WHEN <fs_parent>-gro_vol_val >
                                                 <fs_parent>-gro_wei_val
                                            THEN <fs_parent>-gro_vol_val
                                            ELSE <fs_parent>-gro_wei_val ).
          UNASSIGN <fs_parent>.
        ENDIF.
        " ---------------------------------------------------------------------
        " 50: When item type is BLK
        " ---------------------------------------------------------------------
      WHEN 'BLK'.
        cs_manifest-tonne = COND #( WHEN cs_manifest-gro_vol_val >
                                         cs_manifest-gro_wei_val
                                    THEN cs_manifest-gro_vol_val
                                    ELSE cs_manifest-gro_wei_val ).
        cs_item-item_descr = cs_manifest-item_descr.
        cs_manifest-tures_tco   = 'BLK'.
        cs_manifest-platenumber = 'Bulking/Pumping'.
        cs_manifest-product_id  = is_trq_item-product_id.
        " ---------------------------------------------------------------------
        " 60: When others
        " ---------------------------------------------------------------------
      WHEN OTHERS.
        cs_manifest-tonne = COND #( WHEN cs_manifest-gro_vol_val >
                                         cs_manifest-gro_wei_val
                                    THEN cs_manifest-gro_vol_val
                                    ELSE cs_manifest-gro_wei_val ).
    ENDCASE.
    " ---------------------------------------------------------------------
    " 70: Create Discharged items
    " ---------------------------------------------------------------------
    create_discharged_items( EXPORTING is_trq_item = is_trq_item
                             CHANGING  cs_manifest = cs_manifest
                                       cs_item     = cs_item ).
*  CATCH zcx_tm_form_exception.
  ENDMETHOD.

  METHOD create_tures_man.
    " ---------------------------------------------------------------------
    " 10: Create new line
    " ---------------------------------------------------------------------
    APPEND INITIAL LINE TO me->gt_tures_man ASSIGNING FIELD-SYMBOL(<fs_tures_man>).
    " ---------------------------------------------------------------------
    " 20: Assign Tures Man from item node
    " ---------------------------------------------------------------------
    <fs_tures_man> = VALUE #( tures_tco = is_trq_item-tures_tco
                              item_type = is_trq_item-item_type
                              qua_uni   = is_trq_item-qua_pcs_uni ).
    " ---------------------------------------------------------------------
    " 30: Assign parent node
    " ---------------------------------------------------------------------
    ASSIGN me->gt_fwq_items[ item_parent_key = is_trq_item-key ] TO FIELD-SYMBOL(<fs_parent>) ##SHADOW.
    " ---------------------------------------------------------------------
    " 40: Check item type
    " ---------------------------------------------------------------------
    CASE is_trq_item-item_type.
      " ---------------------------------------------------------------------
      " 50: When item type is ZGC
      " ---------------------------------------------------------------------
      WHEN 'ZGC'.
        <fs_tures_man>-qua_val_car = is_trq_item-qua_pcs_val.
        IF <fs_parent> IS ASSIGNED.
          <fs_tures_man>-qua_val_car = <fs_parent>-qua_pcs_val.
        ENDIF.
        " ---------------------------------------------------------------------
        " 60: When item type is BLK
        " ---------------------------------------------------------------------
      WHEN 'BLK'.
        " ---------------------------------------------------------------------
        " 70: When others
        " ---------------------------------------------------------------------
      WHEN OTHERS.
        <fs_tures_man>-qua_val_con = is_manifest-qua_pcs_val.
    ENDCASE.
  ENDMETHOD.

  METHOD create_discharged_items.
    " ---------------------------------------------------------------------
    " 10: Data Creation
    " ---------------------------------------------------------------------
    DATA lt_all_cargoes    TYPE zyl028_cargo_summary_tt.
    DATA lt_all_containers TYPE zyl028_cargo_summary_tt.

    " ---------------------------------------------------------------------
    " 20: Check for FWO with cargo assignment
    " ---------------------------------------------------------------------
    LOOP AT gt_cargoes ASSIGNING FIELD-SYMBOL(<fs_cargo>).
      IF <fs_cargo>-cargo_exists = abap_true.
        APPEND LINES OF <fs_cargo>-cargo_summary TO lt_all_cargoes.
        APPEND LINES OF <fs_cargo>-cargo_container TO lt_all_containers.
      ENDIF.
    ENDLOOP.
    IF     lt_all_cargoes IS INITIAL
       AND xsdbool( is_trq_item-item_type = 'ZGC'
       OR is_trq_item-item_type = 'BLK' )          = abap_true.
      TRY.
          create_manifdisch( EXPORTING is_manifest = cs_manifest
                             CHANGING  cs_item     = cs_item ).
        CATCH zcx_tm_form_exception.

      ENDTRY.
    ENDIF.
    IF     lt_all_containers     IS INITIAL
       AND is_trq_item-item_type  = 'ZCN'.
      TRY.
          create_manifdisch( EXPORTING is_manifest = cs_manifest
                             CHANGING  cs_item     = cs_item ).
        CATCH zcx_tm_form_exception.

      ENDTRY.

    ENDIF.
    " ---------------------------------------------------------------------
    " 30: Check item type of node
    " ---------------------------------------------------------------------
    CASE is_trq_item-item_type.
      " ---------------------------------------------------------------------
      " 40: When ZGC or BLK
      " *********************************************************************
      WHEN 'ZGC' OR 'BLK'.
        " ---------------------------------------------------------------------
        " 50: SPlit platenumber into parts
        " *********************************************************************
        SPLIT is_trq_item-platenumber AT '-'
              " TODO: variable is assigned but never used (ABAP cleaner)
              INTO DATA(lf_platenumber_part1)
                   " TODO: variable is assigned but never used (ABAP cleaner)
              DATA(lf_platenumber_part2).
        " ---------------------------------------------------------------------
        " 60: Check for cargo with same platenumber
        " ---------------------------------------------------------------------
        LOOP AT lt_all_cargoes ASSIGNING FIELD-SYMBOL(<fs_disch_cargo>).
          DATA lf_cargo_flag TYPE flag.
          lf_cargo_flag = abap_false.
          LOOP AT me->gt_trq_fwo ASSIGNING FIELD-SYMBOL(<fs_fwo>)
               WHERE trq_id = |{  <fs_disch_cargo>-job_order_no ALPHA = IN }|.
            ASSIGN gt_fwo_items[ parent_key  = <fs_fwo>-key
                                 zzbolnumber = <fs_disch_cargo>-bol_no
                                 item_id     = |{ <fs_disch_cargo>-job_order_no_item ALPHA = IN }|
                                 platenumber = is_trq_item-platenumber ] TO FIELD-SYMBOL(<fs_f_items>).
            IF <fs_f_items> IS ASSIGNED.
              lf_cargo_flag = abap_true.
              UNASSIGN <fs_f_items>.
            ENDIF.
*          if lf_cargo_flag eq abap_false.
            IF <fs_disch_cargo>-bol_no <> is_trq_item-zzbolnumber.
              CONTINUE.
            ENDIF.

*              and <fs_disch_cargo>-tu_number eq is_trq_item-platenumber.
            ASSIGN gt_fwo_items[ zzbolnumber = <fs_disch_cargo>-bol_no
                                 platenumber = is_trq_item-platenumber ] TO <fs_f_items>.
            IF <fs_f_items> IS ASSIGNED.
              SELECT SINGLE FROM /scmtms/d_trqrot
                FIELDS
                *
                WHERE db_key
                      = @<fs_f_items>-parent_key
                  AND trq_type = 'ZMLT'
                INTO @DATA(ls_zmlt).
              IF ls_zmlt IS NOT INITIAL.
                lf_cargo_flag = abap_true.
                UNASSIGN <fs_f_items>.
              ENDIF.
            ENDIF.
            " endif.
          ENDLOOP.
          IF xsdbool( contains( val = <fs_disch_cargo>-tu_number
                                sub = is_trq_item-zzbolnumber
                                occ = 1 ) ) = abap_false.
            CONTINUE.
          ENDIF.
          IF lf_cargo_flag = abap_false.
            CONTINUE.
          ENDIF.

          " ---------------------------------------------------------------------
          " 80: Create Discharged line in Manifest
          " *********************************************************************
          APPEND INITIAL LINE TO cs_item-discharg

          " ---------------------------------------------------------------------
          " 90: Assign Discharged cargo fields
          " *********************************************************************
                 ASSIGNING FIELD-SYMBOL(<fs_discharged>).
          <fs_discharged> = VALUE #(
              qua_pcs_val   = COND #( WHEN is_trq_item-item_type <> 'BLK' THEN <fs_disch_cargo>-product_qty_total )
              gro_wei_val   = <fs_disch_cargo>-weight_to
              tonne         = <fs_disch_cargo>-tonne
              tu_number     = <fs_disch_cargo>-tu_number
              tu_type       = <fs_disch_cargo>-tu_type
              dest_location = <fs_disch_cargo>-dest_location
              date          = <fs_disch_cargo>-disch_load_at
              job_order_no  = COND #( WHEN ls_zmlt-trq_id IS INITIAL
                                      THEN <fs_disch_cargo>-job_order_no
                                      ELSE ls_zmlt-trq_id ) ).
*            pckt_desc         = <fs_disch_cargo>-product ).
          <fs_discharged> = CORRESPONDING #( BASE ( <fs_discharged> ) <fs_disch_cargo> ).
          " ---------------------------------------------------------------------
          " 100: Check fwo item node
          " *********************************************************************
          IF xsdbool( ls_zmlt-trq_id IS INITIAL ) = abap_true.
            LOOP AT me->gt_trq_fwo ASSIGNING <fs_fwo>
                 WHERE trq_id = |{  <fs_disch_cargo>-job_order_no ALPHA = IN }|
                 ##NEEDED.
              " ---------------------------------------------------------------------
              " 110: Get fwo items
              " ---------------------------------------------------------------------
              READ TABLE me->gt_fwo_items ASSIGNING <fs_f_items>
                   WITH KEY parent_key  = <fs_fwo>-key
                            zzbolnumber = <fs_disch_cargo>-bol_no
                            item_id     = |{ <fs_disch_cargo>-job_order_no_item ALPHA = IN }| ##NEEDED.
              " ---------------------------------------------------------------------
              " 120: Get status name
              " ---------------------------------------------------------------------
              IF <fs_f_items> IS ASSIGNED.
                SELECT SINGLE FROM ztm001t_cargostt
                  FIELDS bezei
                  WHERE cargo_status = @<fs_f_items>-zzcargo_status
                    AND spras        = @sy-langu
                  INTO @DATA(lf_status_name).
                <fs_discharged>-zzcargo_status = lf_status_name.
                UNASSIGN <fs_f_items>.
              ENDIF.

*          perform get_cargo_name using <fs_f_items>-zzcargo_status changing lf_status_name.

              " ---------------------------------------------------------------------
              " 130: Unset status name
              " ---------------------------------------------------------------------
              CLEAR lf_status_name.
              " ---------------------------------------------------------------------
              " 140: Assign dispatched trucks
              " *********************************************************************
              APPEND INITIAL LINE TO gt_tures_dis ASSIGNING FIELD-SYMBOL(<fs_tures_dis>).
              <fs_tures_dis> = VALUE #( tures_tco   = <fs_disch_cargo>-tu_type
                                        item_type   = COND #( WHEN <fs_disch_cargo>-mtr = 'GC'
                                                              THEN 'ZGC'
                                                              ELSE <fs_disch_cargo>-mtr )
                                        qua_val_car = COND #( WHEN <fs_disch_cargo>-mtr = 'GC'
                                                              THEN <fs_discharged>-qua_pcs_val )
                                        qua_val_con = COND #( WHEN <fs_disch_cargo>-mtr <> 'GC'
                                                              THEN <fs_discharged>-qua_pcs_val )
                                        qua_uni     = <fs_discharged>-qua_pcs_uni ).
              " ---------------------------------------------------------------------
              " 140.1 Summary for item node
              " ---------------------------------------------------------------------
              IF <fs_tures_dis>-item_type = 'ZGC'.
                cs_item-qua_car_val_dis += <fs_discharged>-qua_pcs_val.
                cs_item-gro_car_val_dis += <fs_discharged>-gro_wei_val.
                cs_item-tonne_car_dis   += <fs_discharged>-tonne.
                cs_item-vol_car_cbm_dis += <fs_discharged>-volume_cbm.
              ELSE.
                cs_item-qua_pcs_val_dis += <fs_discharged>-qua_pcs_val.
                cs_item-gro_wei_val_dis += <fs_discharged>-gro_wei_val.
                cs_item-tonne_dis       += <fs_discharged>-tonne.
                cs_item-vol_cbm_dis     += <fs_discharged>-volume_cbm.
              ENDIF.
            ENDLOOP.
            " ---------------------------------------------------------------------
            " 140.2: Handle ZMLT cargoes
            " ---------------------------------------------------------------------
          ELSE.
            SELECT SINGLE FROM /scmtms/d_trqitm
              FIELDS *
              WHERE parent_key      = @ls_zmlt-db_key
                AND zzbolnumber     = @<fs_disch_cargo>-bol_no
                AND zzmanifest_type = @s_mantyp
              INTO @DATA(ls_zmlt_trq_item).

            IF ls_zmlt_trq_item IS NOT INITIAL.
              SELECT SINGLE FROM ztm001t_cargostt
                FIELDS bezei
                WHERE cargo_status = @ls_zmlt_trq_item-zzcargo_status
                  AND spras        = @sy-langu
                INTO @lf_status_name.
              <fs_discharged>-zzcargo_status = lf_status_name.
            ENDIF.

            " ---------------------------------------------------------------------
            " 130: Unset status name
            " ---------------------------------------------------------------------
            CLEAR lf_status_name.
            " ---------------------------------------------------------------------
            " 140: Assign dispatched trucks
            " *********************************************************************
            APPEND INITIAL LINE TO gt_tures_dis ASSIGNING <fs_tures_dis>.
            <fs_tures_dis> = VALUE #( tures_tco   = <fs_disch_cargo>-tu_type
                                      item_type   = COND #( WHEN <fs_disch_cargo>-mtr = 'GC'
                                                            THEN 'ZGC'
                                                            ELSE <fs_disch_cargo>-mtr )
                                      qua_val_car = COND #( WHEN <fs_disch_cargo>-mtr = 'GC'
                                                            THEN <fs_discharged>-qua_pcs_val )
                                      qua_val_con = COND #( WHEN <fs_disch_cargo>-mtr <> 'GC'
                                                            THEN <fs_discharged>-qua_pcs_val )
                                      qua_uni     = <fs_discharged>-qua_pcs_uni ).
            " ---------------------------------------------------------------------
            " 140.1 Summary for item node
            " ---------------------------------------------------------------------
            IF <fs_tures_dis>-item_type = 'ZGC'.
              cs_item-qua_car_val_dis += <fs_discharged>-qua_pcs_val.
              cs_item-gro_car_val_dis += <fs_discharged>-gro_wei_val.
              cs_item-tonne_car_dis   += <fs_discharged>-tonne.
              cs_item-vol_car_cbm_dis += <fs_discharged>-volume_cbm.
            ELSE.
              cs_item-qua_pcs_val_dis += <fs_discharged>-qua_pcs_val.
              cs_item-gro_wei_val_dis += <fs_discharged>-gro_wei_val.
              cs_item-tonne_dis       += <fs_discharged>-tonne.
              cs_item-vol_cbm_dis     += <fs_discharged>-volume_cbm.
            ENDIF.
          ENDIF.
          CLEAR ls_zmlt.
          CLEAR ls_zmlt_trq_item.
          TRY.
              create_manifdisch( EXPORTING is_discharged = <fs_discharged>
                                           is_manifest   = cs_manifest
                                 CHANGING  cs_item       = cs_item ).
            CATCH zcx_tm_form_exception.

          ENDTRY.
        ENDLOOP.
        " ---------------------------------------------------------------------
        " 150: When ZCN
        " ---------------------------------------------------------------------
      WHEN 'ZCN'.
        LOOP AT lt_all_containers ASSIGNING FIELD-SYMBOL(<fs_container>) WHERE tu_number = is_trq_item-platenumber.
          " ---------------------------------------------------------------------
          " 160: Create Discharged line in Manifest
          " *********************************************************************
          APPEND INITIAL LINE TO cs_item-discharg

          " ---------------------------------------------------------------------
          " 170: Assign Discharged cargo fields
          " *********************************************************************
                 ASSIGNING <fs_discharged>.
          <fs_discharged> = VALUE #(
              qua_pcs_val   = COND #( WHEN is_trq_item-item_type <> 'BLK' THEN <fs_container>-product_qty_total )
              gro_wei_val   = <fs_container>-weight_to
              tonne         = <fs_container>-tonne
              tu_number     = <fs_container>-tu_number
              tu_type       = <fs_container>-tu_type
              dest_location = <fs_container>-dest_location
              job_order_no  = <fs_container>-job_order_no
              DAte          = <fs_container>-disch_load_at
*              pckt_desc     = <fs_container>-product
               ).
          <fs_discharged> = CORRESPONDING #( BASE ( <fs_discharged> ) <fs_container> ).
          " ---------------------------------------------------------------------
          " 180: Check fwo item node
          " *********************************************************************
          LOOP AT me->gt_trq_fwo ASSIGNING <fs_fwo>
               WHERE trq_id = |{  <fs_container>-job_order_no ALPHA = IN }|
               ##NEEDED.
            " ---------------------------------------------------------------------
            " 190: Get fwo items
            " ---------------------------------------------------------------------
            READ TABLE me->gt_fwo_items ASSIGNING <fs_f_items>
                 WITH KEY parent_key  = <fs_fwo>-key
                          zzbolnumber = <fs_container>-bol_no
                          item_id     = |{ <fs_container>-job_order_no_item ALPHA = IN }| ##NEEDED.
            " ---------------------------------------------------------------------
            " 200: Get status name
            " ---------------------------------------------------------------------
            CLEAR lf_status_name.
            IF <fs_f_items> IS ASSIGNED.
              SELECT SINGLE FROM ztm001t_cargostt
                FIELDS bezei
                WHERE cargo_status = @<fs_f_items>-zzcargo_status
                  AND spras        = @sy-langu
                INTO @lf_status_name.
              <fs_discharged>-zzcargo_status = lf_status_name.
            ENDIF.

*          perform get_cargo_name using <fs_f_items>-zzcargo_status changing lf_status_name.

            " ---------------------------------------------------------------------
            " 210: Unset status name
            " ---------------------------------------------------------------------
            CLEAR lf_status_name.
            " ---------------------------------------------------------------------
            " 220: Assign dispatched trucks
            " *********************************************************************
            APPEND INITIAL LINE TO gt_tures_dis ASSIGNING <fs_tures_dis>.
            <fs_tures_dis> = VALUE #( tures_tco   = <fs_container>-tu_type
                                      item_type   = COND #( WHEN <fs_container>-mtr = 'GC'
                                                            THEN 'ZGC'
                                                            ELSE <fs_container>-mtr )
                                      qua_val_car = COND #( WHEN <fs_container>-mtr = 'GC'
                                                            THEN <fs_discharged>-qua_pcs_val )
                                      qua_val_con = COND #( WHEN <fs_container>-mtr <> 'GC'
                                                            THEN <fs_discharged>-qua_pcs_val )
                                      qua_uni     = <fs_discharged>-qua_pcs_uni ).
            " ---------------------------------------------------------------------
            " 220.1 Summary for item node
            " ---------------------------------------------------------------------
            IF <fs_tures_dis>-item_type = 'ZGC'.
              cs_item-qua_car_val_dis += <fs_discharged>-qua_pcs_val.
              cs_item-gro_car_val_dis += <fs_discharged>-gro_wei_val.
              cs_item-tonne_car_dis   += <fs_discharged>-tonne.
              cs_item-vol_car_cbm_dis += <fs_discharged>-volume_cbm.
            ELSE.
              cs_item-qua_pcs_val_dis += <fs_discharged>-qua_pcs_val.
              cs_item-gro_wei_val_dis += <fs_discharged>-gro_wei_val.
              cs_item-tonne_dis       += <fs_discharged>-tonne.
              cs_item-vol_cbm_dis     += <fs_discharged>-volume_cbm.
            ENDIF.
          ENDLOOP.
          TRY.
              create_manifdisch( EXPORTING is_discharged = <fs_discharged>
                                           is_manifest   = cs_manifest
                                 CHANGING  cs_item       = cs_item ).
            CATCH zcx_tm_form_exception.

          ENDTRY.
        ENDLOOP.
    ENDCASE.
    SORT cs_item-discharg BY tu_number.
    APPEND cs_manifest TO cs_item-manifest.
  ENDMETHOD.

  METHOD create_manifdisch.
    ASSIGN cs_item-manifdisch[ platenumber_manif = is_manifest-platenumber ] TO FIELD-SYMBOL(<fs_manifdis>).
    IF <fs_manifdis> IS NOT ASSIGNED.
      APPEND INITIAL LINE TO cs_item-manifdisch ASSIGNING <fs_manifdis>.
      <fs_manifdis> = CORRESPONDING #( Is_manifest MAPPING
                                          platenumber_manif = platenumber
                                          qua_pcs_val_manif = qua_pcs_val
                                          qua_pcs_uni_manif = qua_pcs_uni
                                          gro_wei_val_manif = gro_wei_val
                                          gro_wei_uni_manif = gro_wei_uni
                                          gro_vol_val_manif = gro_vol_val
                                          gro_vol_uni_manif = gro_vol_uni
                                          tonne_manif = tonne
                                          tures_tco_manif = tures_tco
                                          zzfrzone = zzfrzone
                                          item_descr = item_descr
                                          product_id = product_id

                                          EXCEPT * ).
    ENDIF.
    DATA lt_products LIKE me->gt_fwq_items.
    lt_products = CORRESPONDING #( me->gt_fwq_items ).

    DELETE lt_products
           WHERE
           " item_type ne conv /scmtms/trq_item_type( 'PRD' )
                 item_parent_key <> cs_item-fwo_KEY.
    DELETE lt_products
           WHERE item_cat <> 'PRD'.

    <fs_manifdis>-qua_pcs_val_disch += is_discharged-qua_pcs_val.
    <fs_manifdis>-qua_pcs_uni_disch  = is_discharged-qua_pcs_uni.
    <fs_manifdis>-tu_type_disch      = is_discharged-tu_type.
    <fs_manifdis>-dest_loc           = is_discharged-dest_location.

    IF <fs_manifdis>-date_disch IS
       INITIAL.
      CONVERT TIME STAMP is_discharged-date
              TIME ZONE
               cl_abap_context_info=>get_user_time_zone( )
              INTO DATE <fs_manifdis>-date_disch.
    ENDIF.

    IF <fs_manifdis>-qua_pcs_val_manif >
       <fs_manifdis>-qua_pcs_val_disch.
      <fs_manifdis>-short_landed =
      <fs_manifdis>-qua_pcs_val_manif -
      <fs_manifdis>-qua_pcs_val_disch.

    ELSEIF <fs_manifdis>-qua_pcs_val_manif <
           <fs_manifdis>-qua_pcs_val_disch.
      <fs_manifdis>-over_landed =
      <fs_manifdis>-qua_pcs_val_disch -
      <fs_manifdis>-qua_pcs_val_manif.

    ENDIF.
    " ---------------------------------------------------------------------
    " 40: Add product lines
    " ---------------------------------------------------------------------
    IF is_manifest IS NOT INITIAL.
      LOOP AT lt_products ASSIGNING FIELD-SYMBOL(<fs_prd>).
        READ TABLE cs_item-manifdisch INTO DATA(ls_mdis) WITH KEY product_id = <fs_prd>-product_id.
        IF ls_mdis IS INITIAL.
           <fs_manifdis>-product_id = <fs_prd>-product_id.
*          APPEND INITIAL LINE TO cs_item-manifdisch ASSIGNING FIELD-SYMBOL(<fs_prod_lines>).
*          <fs_prod_lines> = VALUE #( product_id        = <fs_prd>-product_id
*                                     item_descr        = <fs_prd>-item_descr
*                                     qua_pcs_val_manif = <fs_prd>-qua_pcs_val ).
*          cs_item-qua_car_val_man = cond #(  when <fs_prd>-item_type eq 'PRD' THEN <fs_prd>-qua_pcs_val ).
        ENDIF.
        CLEAR ls_mdis.
      ENDLOOP.
    ENDIF.
*    <fs_manifdis> = CORRESPONDING #( BASE ( <fs_manifdis> ) is_discharged MAPPING
*                                      qua_pcs_val_disch = qua_pcs_val
*                                      qua_pcs_uni_disch = qua_pcs_uni
*                                      tu_type_disch = tu_type
*                                      dest_loc = dest_location
*                                      date_disch = date
*
*                                      EXCEPT * ).
  ENDMETHOD.

  METHOD pdr____modfiy_records.
    " ---------------------------------------------------------------------
    " 10: Check items exists
    " ---------------------------------------------------------------------
    CHECK gt_items IS NOT INITIAL.
    " ---------------------------------------------------------------------
    " 20: Include Data
    " ---------------------------------------------------------------------
    " break csayeh.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lo_trsmgr) = zcl_tm_form_helper=>create_bo_service_manager( if_bo_type = zcl_tm_form_helper=>gf_trq  ).
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lo_yard_node_key) = zcl_tm_form_helper=>convert_select_opt_to_sel_par( if_sel_par_name = 'PARENT_KEY'
                                                                                if_fieldname    = 'KEY'
                                                                                it_table_key    = me->gt_trq_fwo ).

    " TODO: variable is assigned but never used (ABAP cleaner)
    LOOP AT gt_items ASSIGNING FIELD-SYMBOL(<fs_items>).

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
