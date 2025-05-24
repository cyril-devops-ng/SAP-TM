*&---------------------------------------------------------------------*
*& Include ztm039_p_out_turn_new_fom
*&---------------------------------------------------------------------*
" ---------------------------------------------------------------------
" Form: CHECK Forwarding Quotation exists
" ---------------------------------------------------------------------
FORM check_fwq_exists.
  " ---------------------------------------------------------------------
  " 10: Data Declaration
  " ---------------------------------------------------------------------
  DATA dref TYPE REF TO data.

  " ---------------------------------------------------------------------
  " 20: Get Transaction Manager
  " ---------------------------------------------------------------------
  PERFORM get_transaction_mgr.
  " ---------------------------------------------------------------------
  " 30: Get Selection parameters
  " ---------------------------------------------------------------------
  PERFORM set_selection_parameter.
  TRY.
      " ---------------------------------------------------------------------
      " 40: Create Data
      " ---------------------------------------------------------------------
      CREATE DATA dref TYPE ('/SCMTMS/T_TRQ_ROOT_K').
      " ---------------------------------------------------------------------
      " 50: Retrieve TRQ keys
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->query( EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                                      it_selection_parameters = lt_trq_selpar
                            IMPORTING et_key                  = DATA(lt_trq_key) ).
      " ---------------------------------------------------------------------
      " 60: Retrieve TRQ root
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->retrieve( EXPORTING iv_node_key  = /scmtms/if_trq_c=>sc_node-root
                                         it_key       = lt_trq_key
                                         iv_fill_data = abap_true
                               IMPORTING et_data      = dref->* ).
      " ---------------------------------------------------------------------
      " 70: Assign TRQ root
      " ---------------------------------------------------------------------
      ASSIGN dref->* TO <fs_trq_fwq>.
    CATCH /bobf/cx_frw_contrct_violation.
  ENDTRY.
  " ---------------------------------------------------------------------
  " 80: Return message if TRQ not found
  " ---------------------------------------------------------------------
  IF xsdbool( <fs_trq_fwq> IS NOT ASSIGNED OR <fs_trq_fwq> IS INITIAL ) = abap_true.
    MESSAGE i001 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.

" ---------------------------------------------------------------------
" Form: GET Transaction Manager.
" ---------------------------------------------------------------------
FORM get_transaction_mgr.
  " ---------------------------------------------------------------------
  " 10: Check if transaction mgr exists
  " ---------------------------------------------------------------------
  IF lo_trq_srvmgr IS NOT BOUND.
    TRY.
        " ---------------------------------------------------------------------
        " 20: Create instance of transaction mgr.
        " ---------------------------------------------------------------------
        lo_trq_srvmgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_trq_c=>sc_bo_key ).
      CATCH /bobf/cx_frw_contrct_violation.
    ENDTRY.
  ENDIF.
ENDFORM.

" ---------------------------------------------------------------------
" Form: SET Selection parameter.
" ---------------------------------------------------------------------
FORM set_selection_parameter.
  " ---------------------------------------------------------------------
  " 10: Check if selection par exists
  " ---------------------------------------------------------------------
  IF lt_trq_selpar IS INITIAL.
    " ---------------------------------------------------------------------
    " 20: Create selection par.
    " ---------------------------------------------------------------------
    lt_trq_selpar = CORRESPONDING #( s_fwq_no[] ).
    LOOP AT lt_trq_selpar ASSIGNING FIELD-SYMBOL(<fs_selpar>).
      <fs_selpar>-attribute_name = trq_id.
    ENDLOOP.
  ENDIF.
ENDFORM.

" ---------------------------------------------------------------------
" Form: CHECK Forwarding order exists for FWQ
" ---------------------------------------------------------------------
FORM check_fwo_exists_for_fwq.
  " ---------------------------------------------------------------------
  " 10: Declare data
  " ---------------------------------------------------------------------
  DATA dref        TYPE REF TO data.
  DATA lr_trq_type TYPE RANGE OF /scmtms/trq_type.

  " ---------------------------------------------------------------------
  " 20 Create Data
  " ---------------------------------------------------------------------
  CREATE DATA dref TYPE ('/SCMTMS/T_TRQ_ROOT_K').
  " ---------------------------------------------------------------------
  " 30 Create selection parameter for FWO
  " ---------------------------------------------------------------------
  PERFORM set_selection_parameter.
  " ---------------------------------------------------------------------
  " 40 GET Transaction mgr.
  " ---------------------------------------------------------------------
  PERFORM get_transaction_mgr.
  TRY.
      " ---------------------------------------------------------------------
      " 50: Retrieve TRQ keys
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->query( EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                                      it_selection_parameters = lt_trq_selpar
                            IMPORTING et_key                  = DATA(lt_trq_key) ).
      " ---------------------------------------------------------------------
      " 60: Retrieve FWO TRQ by association
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->retrieve_by_association(
        EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                  it_key         = CORRESPONDING #( lt_trq_key MAPPING key = key )
                  iv_association = /scmtms/if_trq_c=>sc_association-root-trq_root_quot2successor
                  iv_fill_data   = abap_true
        IMPORTING et_data        = dref->* ).
      ASSIGN dref->* TO <fs_trq_fwo>.
      " ---------------------------------------------------------------------
      " 110: Retrieve Allowed FWO types
      " ---------------------------------------------------------------------
      SELECT FROM ztm050t_fwo_typ
        FIELDS 'I'      AS sign,
               'EQ'     AS option,
               trq_type AS low
        INTO CORRESPONDING FIELDS OF TABLE @lr_trq_type.
      DELETE <fs_trq_fwo> WHERE trq_type NOT IN lr_trq_type.
      " ---------------------------------------------------------------------
      " 70 Raise error if no forwarding order exists
      " ---------------------------------------------------------------------
      IF xsdbool( <fs_trq_fwo> IS NOT ASSIGNED OR <fs_trq_fwo> IS INITIAL ) = abap_true.
        MESSAGE i002 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      " ---------------------------------------------------------------------
      " 80 Raise error if exception occurs
      " ---------------------------------------------------------------------
    CATCH /bobf/cx_frw_contrct_violation.
      MESSAGE i002 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.
ENDFORM.

" ---------------------------------------------------------------------
" Form: Retrieve FWQ Items
" ---------------------------------------------------------------------
FORM retrieve_fwq_items.
  " ---------------------------------------------------------------------
  " 10: Data Declaration
  " ---------------------------------------------------------------------
  DATA dref            TYPE REF TO data.
  DATA dref_itm        TYPE REF TO data.
  DATA dref_prd        TYPE REF TO data.
  DATA lr_excl_it_type TYPE RANGE OF /scmtms/trq_item_type.

  " ---------------------------------------------------------------------
  " 20: SET selection par.
  " ---------------------------------------------------------------------
  PERFORM set_selection_parameter.
  " ---------------------------------------------------------------------
  " 30: Get transaction mgr.
  " ---------------------------------------------------------------------
  PERFORM get_transaction_mgr.
  TRY.
      " ---------------------------------------------------------------------
      " 40 Create Data ref. -> root
      " ---------------------------------------------------------------------
      CREATE DATA dref TYPE ('/SCMTMS/T_TRQ_ROOT_K').
      " ---------------------------------------------------------------------
      " 50: Retrieve TRQ keys
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->query( EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                                      it_selection_parameters = lt_trq_selpar
                            IMPORTING et_key                  = DATA(lt_trq_key) ).
      " ---------------------------------------------------------------------
      " 60: Retrieve TRQ Root Data
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->retrieve( EXPORTING iv_node_key  = /scmtms/if_trq_c=>sc_node-root
                                         it_key       = lt_trq_key
                                         iv_fill_data = abap_true
                               IMPORTING et_data      = dref->* ).
      " ---------------------------------------------------------------------
      " 70: Assign TRQ data
      " ---------------------------------------------------------------------
      ASSIGN dref->* TO <fs_trq_fwq>.
      " ---------------------------------------------------------------------
      " 80: Create Data ref. -> item
      " ---------------------------------------------------------------------
      CREATE DATA dref_itm TYPE ('/SCMTMS/T_TRQ_ITEM_K').
      " ---------------------------------------------------------------------
      " 90: Retrieve by association Item node
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                                                        it_key         = CORRESPONDING #( lt_trq_key MAPPING key = key )
                                                        iv_association = /scmtms/if_trq_c=>sc_association-root-item
                                                        iv_fill_data   = abap_true
                                              IMPORTING et_data        = dref_itm->* ).
      " ---------------------------------------------------------------------
      " 100: Assign Item node data
      " ---------------------------------------------------------------------
      ASSIGN dref_itm->* TO <fs_fwq_items>.
      " ---------------------------------------------------------------------
      " 110: Retrieve excl. Item type
      " ---------------------------------------------------------------------
      SELECT FROM ztm039t_exc_ityp
        FIELDS 'I'       AS sign,
               'EQ'      AS option,
               item_type AS low
        INTO CORRESPONDING FIELDS OF TABLE @lr_excl_it_type.
      " ---------------------------------------------------------------------
      " 120: Delete unwanted item node
      " ---------------------------------------------------------------------
      DELETE <fs_fwq_items>
             WHERE
*              item_cat        <> 'SRV' OR
                      zzbolnumber NOT IN
                      s_bolno
                   OR (     zzmanifest_type <> s_mantyp
                        AND zzmanifest_type <> space )
                   OR
                      item_type IN lr_excl_it_type.
      " ---------------------------------------------------------------------
      " 120.0: Raise exception if no relevant items found
      " ---------------------------------------------------------------------
      IF <fs_fwq_items> IS INITIAL.
        MESSAGE i003 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      " ---------------------------------------------------------------------
      " 120.1 Create Product Data -> ref.
      " ---------------------------------------------------------------------
      CREATE DATA dref_prd TYPE ('/SCMTMS/T_TRQ_ITEM_K').
      " ---------------------------------------------------------------------
      " 120.2 Get products keys/data
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->query(
        EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-item-query_by_attributes
                  iv_fill_data            = abap_true
                  it_selection_parameters = VALUE /bobf/t_frw_query_selparam( FOR <fs> IN <fs_fwq_items>
                                                                              ( attribute_name = 'ITEM_PARENT_KEY'
                                                                                sign           = 'I'
                                                                                option         = 'EQ'
                                                                                low            = <fs>-key ) )
        IMPORTING et_data                 = dref_prd->* ).

      " ---------------------------------------------------------------------
      " 130: Assign Product node data
      " ---------------------------------------------------------------------
      ASSIGN dref_prd->* TO <fs_fwq_products>.
      " ---------------------------------------------------------------------
      " 140: Raise exception if TM exception occurs
      " ---------------------------------------------------------------------
    CATCH /bobf/cx_frw_contrct_violation.
      MESSAGE i004 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.
ENDFORM.

" ---------------------------------------------------------------------
" Form: Retrieve FWO Items
" ---------------------------------------------------------------------
FORM retrieve_fwo_items.
  " ---------------------------------------------------------------------
  " 10: Data Declaration
  " ---------------------------------------------------------------------
  DATA dref            TYPE REF TO data.
  DATA dref_itm        TYPE REF TO data.
  DATA lr_excl_it_type TYPE RANGE OF /scmtms/trq_item_type.
  TYPES lr_key TYPE RANGE OF /bobf/conf_key.

  " ---------------------------------------------------------------------
  " 20: SET selection par.
  " ---------------------------------------------------------------------
  PERFORM set_selection_parameter.
  " ---------------------------------------------------------------------
  " 30: Get transaction mgr.
  " ---------------------------------------------------------------------
  PERFORM get_transaction_mgr.
  TRY.
      " ---------------------------------------------------------------------
      " 40 Create Data ref. -> root
      " ---------------------------------------------------------------------
      CREATE DATA dref TYPE ('/SCMTMS/T_TRQ_ROOT_K').
      " ---------------------------------------------------------------------
      " 50: Retrieve TRQ keys
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->query( EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                                      it_selection_parameters = lt_trq_selpar
                            IMPORTING et_key                  = DATA(lt_trq_key) ).
      " ---------------------------------------------------------------------
      " 60: Retrieve TRQ Root Data
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->retrieve( EXPORTING iv_node_key  = /scmtms/if_trq_c=>sc_node-root
                                         it_key       = lt_trq_key
                                         iv_fill_data = abap_true
                               IMPORTING et_data      = dref->* ).
      " ---------------------------------------------------------------------
      " 70: Assign TRQ data
      " ---------------------------------------------------------------------

      ASSIGN dref->* TO <fs_trq_fwq>.
      lo_trq_srvmgr->query(
        EXPORTING
          iv_query_key            = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
          it_selection_parameters = VALUE /bobf/t_frw_query_selparam( ( attribute_name = 'QUOT_TRQ_KEY'
                                                                        sign           = 'I'
                                                                        option         = 'EQ'
                                                                        low            = <fs_trq_fwq>[ 1 ]-key ) )
        IMPORTING
          et_key                  = DATA(lt_trq_fwo_key) ).
      " ---------------------------------------------------------------------
      " 70.1 Get FWO Keys
      " ---------------------------------------------------------------------

      " ---------------------------------------------------------------------
      " 80: Create Data ref. -> item
      " ---------------------------------------------------------------------
      CREATE DATA dref_itm TYPE ('/SCMTMS/T_TRQ_ITEM_K').
      " ---------------------------------------------------------------------
      " 90: Retrieve by association Item node
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->retrieve_by_association(
        EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                  it_key         = CORRESPONDING #( lt_trq_fwo_key MAPPING key = key )
                  iv_association = /scmtms/if_trq_c=>sc_association-root-item
                  iv_fill_data   = abap_true
        IMPORTING et_data        = dref_itm->* ).
      " ---------------------------------------------------------------------
      " 100: Assign Item node data
      " ---------------------------------------------------------------------
      ASSIGN dref_itm->* TO <fs_fwo_items>.
      " ---------------------------------------------------------------------
      " 110: Retrieve excl. Item type
      " ---------------------------------------------------------------------
      SELECT FROM ztm039t_exc_ityp
        FIELDS 'I'       AS sign,
               'EQ'      AS option,
               item_type AS low
        INTO CORRESPONDING FIELDS OF TABLE @lr_excl_it_type.
      " ---------------------------------------------------------------------
      " 120: Delete unwanted item node
      " ---------------------------------------------------------------------
      DELETE <fs_fwo_items>
             WHERE    item_cat         = 'SRV'
                   OR zzbolnumber NOT IN
                      s_bolno
                   OR zzmanifest_type <> s_mantyp
                   OR
                      item_type       IN lr_excl_it_type.
      IF <fs_fwo_items> IS NOT INITIAL.

        DELETE <fs_trq_fwo>
               WHERE key NOT IN VALUE lr_key( FOR <fs> IN <fs_fwo_items>
                                              ( low = <fs>-parent_key sign = 'I' option = 'EQ' ) ).

      ENDIF.

      " ---------------------------------------------------------------------
      " 130: Raise exception if no relevant items found
      " ---------------------------------------------------------------------
      IF <fs_fwo_items> IS INITIAL.
        MESSAGE i007 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      " ---------------------------------------------------------------------
      " 140: Raise exception if TM exception occurs
      " ---------------------------------------------------------------------
    CATCH /bobf/cx_frw_contrct_violation.
      MESSAGE i008 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.
ENDFORM.

" ---------------------------------------------------------------------
" Form: Get FWQ Execution Data
" ---------------------------------------------------------------------
FORM get_fwq_execution_data.
  " ---------------------------------------------------------------------
  " 10: Data Declaration
  " ---------------------------------------------------------------------
  DATA dref       TYPE REF TO data.
  DATA dref_zexec TYPE REF TO data.

  " ---------------------------------------------------------------------
  " 20: SET selection par.
  " ---------------------------------------------------------------------
  PERFORM set_selection_parameter.
  " ---------------------------------------------------------------------
  " 30: Get transaction mgr.
  " ---------------------------------------------------------------------
  PERFORM get_transaction_mgr.
  TRY.
      " ---------------------------------------------------------------------
      " 40 Create Data ref. -> root
      " ---------------------------------------------------------------------

      CREATE DATA dref TYPE ('/SCMTMS/T_TRQ_ROOT_K').
      " ---------------------------------------------------------------------
      " 50: Retrieve TRQ keys
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->query( EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                                      it_selection_parameters = lt_trq_selpar
                            IMPORTING et_key                  = DATA(lt_trq_key) ).
      " ---------------------------------------------------------------------
      " 60: Retrieve TRQ Root Data
      " ---------------------------------------------------------------------
      IF xsdbool( <fs_trq_fwq> IS NOT ASSIGNED OR <fs_trq_fwq> IS INITIAL )
         = abap_true.
        lo_trq_srvmgr->retrieve( EXPORTING iv_node_key  = /scmtms/if_trq_c=>sc_node-root
                                           it_key       = lt_trq_key
                                           iv_fill_data = abap_true
                                 IMPORTING et_data      = dref->* ).
        " ---------------------------------------------------------------------
        " 70: Assign TRQ data
        " ---------------------------------------------------------------------
        ASSIGN dref->* TO <fs_trq_fwq>.
      ENDIF.
      " ---------------------------------------------------------------------
      " 80: Create Data ref. -> item
      " ---------------------------------------------------------------------
      CREATE DATA dref_zexec TYPE ('ZTM005_TRQ_EXEC_K_TT').
      " ---------------------------------------------------------------------
      " 90: Retrieve by association Execution node
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                                                        it_key         = CORRESPONDING #( lt_trq_key MAPPING key = key )
                                                        iv_association = gc_zexec_assoc_key
                                                        iv_fill_data   = abap_true
                                              IMPORTING et_data        = dref_zexec->* ).
      " ---------------------------------------------------------------------
      " 100: Assign execution data
      " ---------------------------------------------------------------------
      ASSIGN dref_zexec->* TO <fs_fwq_exec>.
      IF xsdbool( <fs_fwq_exec> IS NOT ASSIGNED OR <fs_fwq_exec> IS INITIAL )
         = abap_true.
*        MESSAGE i005 DISPLAY LIKE 'W'.
        " leave LIST-PROCESSING.
      ENDIF.
    CATCH /bobf/cx_frw_contrct_violation.
*      MESSAGE i006 DISPLAY LIKE 'W'.
      " leave LIST-PROCESSING.
  ENDTRY.
ENDFORM.

" ---------------------------------------------------------------------
" Form: Create Form Header
" ---------------------------------------------------------------------
FORM create_form_header CHANGING cs_header TYPE ztm039s_header.
  " ---------------------------------------------------------------------
  " 10.0 Data Declaration
  " ---------------------------------------------------------------------
  DATA lf_yard_no TYPE /sapyl/e_yard_no.
  DATA dref       TYPE REF TO data.
  DATA lf_shipper TYPE string.

  CREATE DATA dref TYPE ('/SCMTMS/T_TRQ_PARTY_K').

  " ---------------------------------------------------------------------
  " 20: SET selection par.
  " ---------------------------------------------------------------------
  PERFORM set_selection_parameter.
  " ---------------------------------------------------------------------
  " 30: Get transaction mgr.
  " ---------------------------------------------------------------------
  PERFORM get_transaction_mgr.
  TRY.
      " ---------------------------------------------------------------------
      " 40: Retrieve TRQ keys
      " ---------------------------------------------------------------------
      lo_trq_srvmgr->query( EXPORTING iv_query_key            = /scmtms/if_trq_c=>sc_query-root-query_by_attributes
                                      it_selection_parameters = lt_trq_selpar
                            IMPORTING et_key                  = DATA(lt_trq_key) ).
      " ---------------------------------------------------------------------
      " 50: Get shipping party node
      " *********************************************************************
      lo_trq_srvmgr->retrieve_by_association( EXPORTING iv_node_key    = /scmtms/if_trq_c=>sc_node-root
                                                        it_key         = CORRESPONDING #( lt_trq_key MAPPING key = key )
                                                        iv_association = /scmtms/if_trq_c=>sc_association-root-party
                                                        iv_fill_data   = abap_true
                                              IMPORTING et_data        = dref->* ).
      " ---------------------------------------------------------------------
      " 60: Assign shipping parties
      " *********************************************************************
      ASSIGN dref->* TO <fs_fwq_party>.
      " ---------------------------------------------------------------------
      " 70: Get Shipper name
      " ---------------------------------------------------------------------
      IF <fs_fwq_party> IS ASSIGNED.
        SELECT FROM but000
          FIELDS partner,
                 mc_name1
          FOR ALL ENTRIES IN
          @<fs_fwq_party>
          WHERE partner
                = @<fs_fwq_party>-party_id
          INTO TABLE @DATA(lt_shipper).
      ENDIF.
      " ---------------------------------------------------------------------
      " 80: Assign Shipper partner type name
      " ---------------------------------------------------------------------
      IF line_exists( <fs_fwq_party>[ party_rco = 'Z5' ] ).
        ASSIGN <fs_fwq_party>[ party_rco = 'Z5' ] TO FIELD-SYMBOL(<fs_partner_type>).
        IF line_exists( lt_shipper[ partner = <fs_partner_type>-party_id ] ).
          lf_shipper = lf_shipper && lt_shipper[ partner = <fs_partner_type>-party_id ]-mc_name1.
        ENDIF.
      ENDIF.
      " ---------------------------------------------------------------------
      " 90: Assign Shipper name
      " *********************************************************************
      IF line_exists( <fs_fwq_party>[ party_rco = 'Z6' ] ).
        ASSIGN <fs_fwq_party>[ party_rco = 'Z6' ] TO <fs_partner_type>.
        IF line_exists( lt_shipper[ partner = <fs_partner_type>-party_id ] ).
          lf_shipper = |{ lf_shipper }/{ lt_shipper[ partner = <fs_partner_type>-party_id ]-mc_name1 }|.
        ENDIF.
      ENDIF.

    CATCH /bobf/cx_frw_contrct_violation.

  ENDTRY.
  " ---------------------------------------------------------------------
  " 100.1: IF no relevant FWO is found raise exception
  " ---------------------------------------------------------------------
  IF xsdbool( line_exists( <fs_trq_fwo>[ 1 ] ) ) = abap_false.
    MESSAGE i009 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  " ---------------------------------------------------------------------
  " 110: Assign 1 FWO since all FWO have common fields
  " ---------------------------------------------------------------------
  ASSIGN <fs_trq_fwo>[ 1 ] TO FIELD-SYMBOL(<fs_fwo>).
  " ---------------------------------------------------------------------
  " 120: Get Carrier Name
  " ---------------------------------------------------------------------
  SELECT SINGLE FROM but000
    FIELDS mc_name1
    WHERE partner = @<fs_fwo>-tsp_id
    INTO @DATA(lf_carrier_name).
  " ---------------------------------------------------------------------
  " 130: Check Destination location is a yard
  " ---------------------------------------------------------------------
  lf_yard_no = <fs_fwo>-des_loc_id.
  SELECT SINGLE FROM /sapyl/yardno
    FIELDS @abap_true
    WHERE yard_no = @lf_yard_no
    INTO @DATA(lf_yard_exists).
  " ---------------------------------------------------------------------
  " 140: Assign Header fields
  " ---------------------------------------------------------------------
  cs_header = VALUE #( carrier_id   = <fs_fwo>-tsp_id
                       carrier_name = COND #( WHEN lf_shipper IS NOT INITIAL THEN lf_shipper ELSE lf_carrier_name )
                       vessel       = <fs_fwo>-zzresource
                       direction    = s_mantyp
                       base         = COND #( WHEN lf_yard_exists = abap_true AND <fs_fwq_exec> IS NOT INITIAL THEN
                                                |{ <fs_fwo>-des_loc_id(4) }({ <fs_fwo>-des_loc_id+5(3) })|
                                              WHEN <fs_fwo>-src_loc_id IS NOT INITIAL AND lf_yard_exists = abap_false
                                               AND <fs_fwq_exec>       IS NOT INITIAL THEN
                                                |{ <fs_fwo>-src_loc_id(4) }|
                                              WHEN lf_yard_exists = abap_true AND <fs_fwq_exec> IS INITIAL THEN
                                                |{ <fs_fwo>-des_loc_id(4) }|
                                              WHEN <fs_fwo>-src_loc_id IS NOT INITIAL AND lf_yard_exists = abap_false
                                               AND <fs_fwq_exec>       IS INITIAL THEN
                                                |{ <fs_fwo>-src_loc_id(4) }| )
                       fwq_id       = |{ <fs_trq_fwq>[ 1 ]-trq_id  ALPHA = OUT }| ).
  " ---------------------------------------------------------------------
  " 150: Check if execution data exists
  " ---------------------------------------------------------------------
  IF <fs_fwq_exec> IS INITIAL.
    RETURN.
  ENDIF.
  " ---------------------------------------------------------------------
  " 160: Assign Departure Date
  " ---------------------------------------------------------------------
  IF xsdbool( line_exists( <fs_fwq_exec>[ event_code = gc_departure_event ] )  )
     = abap_true.
    ASSIGN <fs_fwq_exec>[ event_code = gc_departure_event ]
           TO FIELD-SYMBOL(<fs_departure>).
    cs_header = CORRESPONDING #( BASE ( cs_header ) <fs_departure>
     MAPPING departure = actual_date ).
  ENDIF.
  " ---------------------------------------------------------------------
  " 170: Assign Arrival Date
  " ---------------------------------------------------------------------
  IF xsdbool( line_exists( <fs_fwq_exec>[ event_code = gc_arrival_event ] )  )
     = abap_true.
    ASSIGN <fs_fwq_exec>[ event_code = gc_arrival_event ]
           TO FIELD-SYMBOL(<fs_arrival>).
    cs_header = CORRESPONDING #( BASE ( cs_header ) <fs_arrival>
     MAPPING arrival = actual_date ).
  ENDIF.
ENDFORM.

" ----------------------------------------------------------------------
" Form: Get Cargo
" ---------------------------------------------------------------------
FORM get_cargo_name USING    if_cargo_status TYPE string
                    CHANGING cf_cargo_name   TYPE string.

  SELECT SINGLE FROM ztm001t_cargostt
    FIELDS bezei
    WHERE cargo_status = @if_cargo_status
      AND
          spras        = @sy-langu
    INTO @cf_cargo_name.
ENDFORM.

" ---------------------------------------------------------------------
" Form: Create Items
" ---------------------------------------------------------------------
FORM create_form_items CHANGING ct_items TYPE ztm039_item_tt.
  " ---------------------------------------------------------------------
  " 10: Data Declaration
  " ---------------------------------------------------------------------
  DATA lt_prd      LIKE <fs_fwq_items>.
  DATA ls_manifest TYPE ztm039s_manifest.

  " ---------------------------------------------------------------------
  " 20: Get products
  " ---------------------------------------------------------------------
  lt_prd = VALUE #( FOR <fs> IN <fs_fwq_items>
                    WHERE ( item_cat = 'PRD' )
                    ( <fs> )  ).
  " ---------------------------------------------------------------------
  " 30: Repeat product count times
  " ---------------------------------------------------------------------
  DO lines( lt_prd ) TIMES.
    " ---------------------------------------------------------------------
    " 40: Get bl,cust,pol,tures_tco from parent node
    " ---------------------------------------------------------------------
    LOOP AT <fs_fwq_items> ASSIGNING FIELD-SYMBOL(<fs_item>)
         WHERE item_cat = 'PRD'.
      ASSIGN <fs_fwq_items>[ key = <fs_item>-item_parent_key ] TO FIELD-SYMBOL(<fs_parent>).
      " ---------------------------------------------------------------------
      " 50: if parent exists assign fields
      " ---------------------------------------------------------------------
      IF <fs_parent> IS ASSIGNED.
        <fs_item> = CORRESPONDING #( BASE ( <fs_item> ) <fs_parent>
                                     MAPPING zzbolnumber = zzbolnumber
                                             zzcust = zzcust
                                             zzpol = zzpol
                                             tures_tco = tures_tco
                                             EXCEPT * ).
        UNASSIGN <fs_parent>.
      ENDIF.
    ENDLOOP.
  ENDDO.
  " ---------------------------------------------------------------------
  " 60: Get Cargo information
  " ---------------------------------------------------------------------
  PERFORM get_fwo_cargo.
  " ---------------------------------------------------------------------
  " 70: Get bl,cust,pol,tures_tco from parent node
  " ---------------------------------------------------------------------
  LOOP AT <fs_fwq_products> ASSIGNING FIELD-SYMBOL(<fs_prd>).
    ASSIGN <fs_fwq_items>[ key = <fs_prd>-item_parent_key ] TO <fs_parent>.
    " ---------------------------------------------------------------------
    " 80: if parent exists assign fields
    " ---------------------------------------------------------------------
    IF <fs_parent> IS ASSIGNED.
      <fs_prd> = CORRESPONDING #( BASE ( <fs_prd> ) <fs_parent>
                                   MAPPING zzbolnumber = zzbolnumber
                                           zzcust = zzcust
                                           zzpol = zzpol
                                           tures_tco = tures_tco
                                           EXCEPT * ).
      UNASSIGN <fs_parent>.
    ENDIF.
  ENDLOOP.
  " ---------------------------------------------------------------------
  " 90 Create Items.
  " *********************************************************************

  DATA lt_trq_items TYPE /scmtms/t_trq_item_k.
  lt_trq_items = CORRESPONDING #( <fs_fwq_items> ).
  DELETE lt_trq_items WHERE item_type = 'PRD'.
*  DELETE <fs_fwq_items> WHERE item_type = 'PRD'.
  LOOP AT lt_trq_items ASSIGNING <fs_item>.
    " ---------------------------------------------------------------------
    " 100: Create empty item line
    " ---------------------------------------------------------------------

    IF xsdbool( line_exists( gt_items[ zzbolnumber = <fs_item>-zzbolnumber
                                       zzcust      = <fs_item>-zzcust
                                       zzpol       = <fs_item>-zzpol ] ) )
       = abap_false.
      APPEND INITIAL LINE TO gt_items ASSIGNING FIELD-SYMBOL(<fs_items>).
      " ---------------------------------------------------------------------
      " 110: Assign bl/cust/pol
      " ---------------------------------------------------------------------
      <fs_items> = CORRESPONDING #( <fs_item> MAPPING
                                              zzbolnumber = zzbolnumber
                                              zzcust      = zzcust
                                              zzpol       = zzpol
                                              EXCEPT * ).
    ELSE.
      <fs_items> = gt_items[ zzbolnumber = <fs_item>-zzbolnumber
                             zzcust      = <fs_item>-zzcust
                             zzpol       = <fs_item>-zzpol ].
    ENDIF.

    " ---------------------------------------------------------------------
    " 120: Unset Manifest
    " ---------------------------------------------------------------------
    CLEAR ls_manifest.
    " ---------------------------------------------------------------------
    " 130: Create Manifest
    " ---------------------------------------------------------------------
    PERFORM create_manifest USING    <fs_item>
                            CHANGING ls_manifest
                                     <fs_items>.
    " ---------------------------------------------------------------------
    " 140: Create Tures Man
    " ---------------------------------------------------------------------
    PERFORM create_tures_man USING <fs_item>
                                   ls_manifest.
    " ---------------------------------------------------------------------
    " 150: ZGC summary
    " ---------------------------------------------------------------------
    IF xsdbool( <fs_item>-item_type = 'ZGC' ) = abap_true.
      <fs_items>-tonne_car_manif     += ls_manifest-tonne_manif.
      <fs_items>-qua_car_val_man     += ls_manifest-qua_pcs_val_manif.
      <fs_items>-gro_car_val_man     += ls_manifest-gro_wei_val_manif.
      <fs_items>-gro_car_vol_val_man += ls_manifest-gro_vol_val_manif.
    ENDIF.
    " ---------------------------------------------------------------------
    " 160: Other summary
    " ---------------------------------------------------------------------
    IF xsdbool(  <fs_item>-item_type <> 'ZGC'  AND <fs_item>-item_type <> 'BLK' ) = abap_true.
      <fs_items>-tonne_manif     += ls_manifest-tonne_manif.
      <fs_items>-qua_pcs_val_man += ls_manifest-qua_pcs_val_manif.
      <fs_items>-gro_wei_val_man += ls_manifest-gro_wei_val_manif.
      <fs_items>-gro_vol_val_man += ls_manifest-gro_vol_val_manif.

    ENDIF.
  ENDLOOP.
  SORT gt_items BY zzbolnumber
                   zzcust.
  DELETE ADJACENT DUPLICATES FROM gt_items COMPARING zzbolnumber zzcust.
  " ---------------------------------------------------------------------
  " 170: Add Consignee name
  " ---------------------------------------------------------------------
  LOOP AT gt_items ASSIGNING FIELD-SYMBOL(<fs_t_items>).
    SELECT SINGLE
      FROM but000
      FIELDS mc_name1
      WHERE partner
            = @<fs_t_items>-zzcust
      INTO @<fs_t_items>-custname.
  ENDLOOP.
  " *********************************************************************
ENDFORM.

" ---------------------------------------------------------------------
" Form: Create Manifest
" ---------------------------------------------------------------------
FORM create_manifest USING    is_trq_item TYPE /scmtms/s_trq_item_k
                     CHANGING cs_manifest TYPE ztm039s_manifest
                              cs_item     TYPE ztm039s_item.

  " ---------------------------------------------------------------------
  " 10: Assign Manifest from item node
  " ---------------------------------------------------------------------
  cs_manifest = CORRESPONDING #( is_trq_item
                  MAPPING zzstorage_type_manif = zzstorage_type
                          zzstorage_section_manif = zzstorage_section
                          qua_pcs_val_manif = qua_pcs_val
                          qua_pcs_uni_manif = qua_pcs_uni
                          gro_wei_val_manif = gro_wei_val
                          gro_wei_uni_manif = gro_wei_uni
                          gro_vol_val_manif = gro_vol_val
                          gro_vol_uni_manif = gro_vol_uni
                          tures_tco_manif = tures_tco
                          EXCEPT * ).
  " ---------------------------------------------------------------------
  " 20: Assign parent node
  " ---------------------------------------------------------------------
  ASSIGN <fs_fwq_items>[ item_parent_key = is_trq_item-key ] TO FIELD-SYMBOL(<fs_parent>) ##SHADOW.
  " ---------------------------------------------------------------------
  " 30: Check item type
  " ---------------------------------------------------------------------
  CASE is_trq_item-item_type.
    " ---------------------------------------------------------------------
    " 40: When item type is ZGC
    " ---------------------------------------------------------------------
    WHEN 'ZGC'.
      IF <fs_parent> IS ASSIGNED.
        cs_manifest-qua_pcs_val_manif = <fs_parent>-qua_pcs_val.
        cs_manifest-gro_vol_val_manif = <fs_parent>-gro_vol_val.
        cs_manifest-tonne_manif       = COND #( WHEN <fs_parent>-gro_vol_val >
                                                     <fs_parent>-gro_wei_val
                                                THEN <fs_parent>-gro_vol_val
                                                ELSE <fs_parent>-gro_wei_val ).
        UNASSIGN <fs_parent>.
      ENDIF.
      " ---------------------------------------------------------------------
      " 50: When item type is BLK
      " ---------------------------------------------------------------------
    WHEN 'BLK'.
      cs_manifest-tonne_manif = COND #( WHEN cs_manifest-gro_vol_val_manif >
                                             cs_manifest-gro_wei_val_manif
                                        THEN cs_manifest-gro_vol_val_manif
                                        ELSE cs_manifest-gro_wei_val_manif ).
      " ---------------------------------------------------------------------
      " 60: When others
      " ---------------------------------------------------------------------
    WHEN OTHERS.
      cs_manifest-tonne_manif = COND #( WHEN cs_manifest-gro_vol_val_manif >
                                             cs_manifest-gro_wei_val_manif
                                        THEN cs_manifest-gro_vol_val_manif
                                        ELSE cs_manifest-gro_wei_val_manif ).
  ENDCASE.
  " ---------------------------------------------------------------------
  " 70: Create Discharged items
  " ---------------------------------------------------------------------
  PERFORM create_discharged_items USING    is_trq_item
                                  CHANGING cs_manifest
                                           cs_item.
ENDFORM.

" ---------------------------------------------------------------------
" Form: Create Tures Man
" ---------------------------------------------------------------------
FORM create_tures_man USING is_trq_item TYPE /scmtms/s_trq_item_k
                            is_manifest TYPE ztm039s_manifest.

  " ---------------------------------------------------------------------
  " 10: Create new line
  " ---------------------------------------------------------------------
  APPEND INITIAL LINE TO gt_tures_man ASSIGNING FIELD-SYMBOL(<fs_tures_man>).
  " ---------------------------------------------------------------------
  " 20: Assign Tures Man from item node
  " ---------------------------------------------------------------------
  <fs_tures_man> = VALUE #( tures_tco = is_trq_item-tures_tco
                            item_type = is_trq_item-item_type
                            qua_uni   = is_trq_item-qua_pcs_uni ).
  " ---------------------------------------------------------------------
  " 30: Assign parent node
  " ---------------------------------------------------------------------
  ASSIGN <fs_fwq_items>[ item_parent_key = is_trq_item-key ] TO FIELD-SYMBOL(<fs_parent>) ##SHADOW.
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
      <fs_tures_man>-qua_val_con = is_manifest-qua_pcs_val_manif.
  ENDCASE.
ENDFORM.

" ---------------------------------------------------------------------
" FORM: Create Discharged items
" ---------------------------------------------------------------------
FORM create_discharged_items USING    is_trq_item TYPE /scmtms/s_trq_item_k
                             CHANGING cs_manifest TYPE ztm039s_manifest
                                      cs_item     TYPE ztm039s_item.

  " ---------------------------------------------------------------------
  " 10: Data Creation
  " ---------------------------------------------------------------------
  DATA lt_all_cargoes    TYPE zyl028_cargo_summary_tt.
  DATA lt_all_containers TYPE zyl028_cargo_summary_tt.

  " ---------------------------------------------------------------------
  " 20: Check for FWO with cargo assignment
  " ---------------------------------------------------------------------
  LOOP AT <fs_fwo_cargo> ASSIGNING FIELD-SYMBOL(<fs_cargo>).
    IF <fs_cargo>-cargo_exists = abap_true.
      APPEND LINES OF <fs_cargo>-cargo_summary TO lt_all_cargoes.
      APPEND LINES OF <fs_cargo>-cargo_container TO lt_all_containers.
    ENDIF.
  ENDLOOP.
*  IF lt_all_cargoes IS INITIAL.
*    RETURN.
*  ENDIF.
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
            " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
            INTO DATA(lf_platenumber_part1)
                 " TODO: variable is assigned but never used (ABAP cleaner)
            DATA(lf_platenumber_part2).
      " ---------------------------------------------------------------------
      " 60: Check for cargo with same platenumber
      " ---------------------------------------------------------------------
      LOOP AT lt_all_cargoes ASSIGNING FIELD-SYMBOL(<fs_disch_cargo>).
        DATA lf_cargo_flag TYPE flag.
        lf_cargo_flag = abap_false.
        LOOP AT <fs_trq_fwo> ASSIGNING FIELD-SYMBOL(<fs_fwo>)
             WHERE trq_id = |{  <fs_disch_cargo>-job_order_no ALPHA = IN }|.
          ASSIGN <fs_fwo_items>[ parent_key  = <fs_fwo>-key
                                 zzbolnumber = <fs_disch_cargo>-bol_no
                                 item_id     = |{ <fs_disch_cargo>-job_order_no_item ALPHA = IN }|
                                 platenumber = is_trq_item-platenumber ] TO FIELD-SYMBOL(<fs_f_items>).
          IF <fs_f_items> IS ASSIGNED.
            lf_cargo_flag = abap_true.
            UNASSIGN <fs_f_items>.
          ENDIF.
*          if lf_cargo_flag eq abap_false.
             if <fs_disch_cargo>-bol_no eq is_trq_item-zzbolnumber.
*              and <fs_disch_cargo>-tu_number eq is_trq_item-platenumber.
              ASSIGN <fs_fwo_items>[ zzbolnumber = <fs_disch_cargo>-bol_no
                                     platenumber = is_trq_item-platenumber ] TO <fs_f_items>.
              IF <fs_f_items> IS ASSIGNED.
                select single from
                /scmtms/d_trqrot
                fields
                *
                where db_key
                = @<fs_f_items>-parent_key
                and trq_type = 'ZMLT'
                into @data(ls_zmlt).
                if ls_zmlt is not initial.
                    lf_cargo_flag = abap_true.
                    unassign <fs_f_items>.
                endif.
              endif.
              endif.
*          endif.
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
        APPEND INITIAL LINE TO cs_manifest-discharged

        " ---------------------------------------------------------------------
        " 90: Assign Discharged cargo fields
        " *********************************************************************
               ASSIGNING FIELD-SYMBOL(<fs_discharged>).
        <fs_discharged> = VALUE #(
            qua_pcs_val_disch = COND #( WHEN is_trq_item-item_type <> 'BLK' THEN <fs_disch_cargo>-product_qty_total )
            gro_wei_val_dis   = <fs_disch_cargo>-weight_to
            tonne_dis         = <fs_disch_cargo>-tonne
            tu_number_disch   = <fs_disch_cargo>-tu_number
            tu_type_disch     = <fs_disch_cargo>-tu_type
            dest_loc          = <fs_disch_cargo>-dest_location
            job_order_no_dis  = cond #(  when ls_zmlt-trq_id is initial then <fs_disch_cargo>-job_order_no else ls_zmlt-trq_id )
            pckt_desc         = <fs_disch_cargo>-product ).
        <fs_discharged> = CORRESPONDING #( BASE ( <fs_discharged> ) <fs_disch_cargo> ).
        " ---------------------------------------------------------------------
        " 100: Check fwo item node
        " *********************************************************************
        if xsdbool( ls_zmlt-trq_id is initial ) eq abap_true.
        LOOP AT <fs_trq_fwo> ASSIGNING <fs_fwo>
             WHERE trq_id =  |{  <fs_disch_cargo>-job_order_no ALPHA = IN }|
             ##NEEDED.
          " ---------------------------------------------------------------------
          " 110: Get fwo items
          " ---------------------------------------------------------------------
          READ TABLE <fs_fwo_items> ASSIGNING <fs_f_items>
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
            <fs_discharged>-zzcargo_status_dis = lf_status_name.
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
                                                          THEN <fs_discharged>-qua_pcs_val_disch )
                                    qua_val_con = COND #( WHEN <fs_disch_cargo>-mtr <> 'GC'
                                                          THEN <fs_discharged>-qua_pcs_val_disch )
                                    qua_uni     = <fs_discharged>-qua_pcs_uni_disch ).
          " ---------------------------------------------------------------------
          " 140.1 Summary for item node
          " ---------------------------------------------------------------------
          IF <fs_tures_dis>-item_type = 'ZGC'.
            cs_item-qua_car_val_dis += <fs_discharged>-qua_pcs_val_disch.
            cs_item-gro_car_val_dis += <fs_discharged>-gro_wei_val_dis.
            cs_item-tonne_car_dis   += <fs_discharged>-tonne_dis.
            cs_item-vol_car_cbm_dis += <fs_discharged>-volume_cbm.
          ELSE.
            cs_item-qua_pcs_val_dis += <fs_discharged>-qua_pcs_val_disch.
            cs_item-gro_wei_val_dis += <fs_discharged>-gro_wei_val_dis.
            cs_item-tonne_dis       += <fs_discharged>-tonne_dis.
            cs_item-vol_cbm_dis     += <fs_discharged>-volume_cbm.
          ENDIF.
        ENDLOOP.
**********************************************************************
*140.2: Handle ZMLT cargoes
**********************************************************************
        else.
            select single from
            /scmtms/d_trqitm
            fields *
            where parent_key = @ls_zmlt-db_key
            and zzbolnumber = @<fs_disch_cargo>-bol_no
            and zzmanifest_type = @s_mantyp
            into @data(ls_zmlt_trq_item).

            if ls_zmlt_trq_item is not initial.
            SELECT SINGLE FROM ztm001t_cargostt
              FIELDS bezei
              WHERE cargo_status = @ls_zmlt_trq_item-zzcargo_status
                AND spras        = @sy-langu
              INTO @lf_status_name.
            <fs_discharged>-zzcargo_status_dis = lf_status_name.
            endif.

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
                                                          THEN <fs_discharged>-qua_pcs_val_disch )
                                    qua_val_con = COND #( WHEN <fs_disch_cargo>-mtr <> 'GC'
                                                          THEN <fs_discharged>-qua_pcs_val_disch )
                                    qua_uni     = <fs_discharged>-qua_pcs_uni_disch ).
          " ---------------------------------------------------------------------
          " 140.1 Summary for item node
          " ---------------------------------------------------------------------
          IF <fs_tures_dis>-item_type = 'ZGC'.
            cs_item-qua_car_val_dis += <fs_discharged>-qua_pcs_val_disch.
            cs_item-gro_car_val_dis += <fs_discharged>-gro_wei_val_dis.
            cs_item-tonne_car_dis   += <fs_discharged>-tonne_dis.
            cs_item-vol_car_cbm_dis += <fs_discharged>-volume_cbm.
          ELSE.
            cs_item-qua_pcs_val_dis += <fs_discharged>-qua_pcs_val_disch.
            cs_item-gro_wei_val_dis += <fs_discharged>-gro_wei_val_dis.
            cs_item-tonne_dis       += <fs_discharged>-tonne_dis.
            cs_item-vol_cbm_dis     += <fs_discharged>-volume_cbm.
          ENDIF.
        endif.
        clear ls_zmlt.
        clear ls_zmlt_trq_item.
      ENDLOOP.
      " ---------------------------------------------------------------------
      " 150: When ZCN
      " ---------------------------------------------------------------------
    WHEN 'ZCN'.
      LOOP AT lt_all_containers ASSIGNING FIELD-SYMBOL(<fs_container>) WHERE tu_number = is_trq_item-platenumber.
        " ---------------------------------------------------------------------
        " 160: Create Discharged line in Manifest
        " *********************************************************************
        APPEND INITIAL LINE TO cs_manifest-discharged

        " ---------------------------------------------------------------------
        " 170: Assign Discharged cargo fields
        " *********************************************************************
               ASSIGNING <fs_discharged>.
        <fs_discharged> = VALUE #(
            qua_pcs_val_disch = COND #( WHEN is_trq_item-item_type <> 'BLK' THEN <fs_container>-product_qty_total )
            gro_wei_val_dis   = <fs_container>-weight_to
            tonne_dis         = <fs_container>-tonne
            tu_number_disch   = <fs_container>-tu_number
            tu_type_disch     = <fs_container>-tu_type
            dest_loc          = <fs_container>-dest_location
            job_order_no_dis  = <fs_container>-job_order_no
            pckt_desc         = <fs_container>-product ).
        <fs_discharged> = CORRESPONDING #( BASE ( <fs_discharged> ) <fs_container> ).
        " ---------------------------------------------------------------------
        " 180: Check fwo item node
        " *********************************************************************
        LOOP AT <fs_trq_fwo> ASSIGNING <fs_fwo>
             WHERE trq_id = |{  <fs_container>-job_order_no ALPHA = IN }|
             ##NEEDED.
          " ---------------------------------------------------------------------
          " 190: Get fwo items
          " ---------------------------------------------------------------------
          READ TABLE <fs_fwo_items> ASSIGNING <fs_f_items>
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
            <fs_discharged>-zzcargo_status_dis = lf_status_name.
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
                                                          THEN <fs_discharged>-qua_pcs_val_disch )
                                    qua_val_con = COND #( WHEN <fs_container>-mtr <> 'GC'
                                                          THEN <fs_discharged>-qua_pcs_val_disch )
                                    qua_uni     = <fs_discharged>-qua_pcs_uni_disch ).
          " ---------------------------------------------------------------------
          " 220.1 Summary for item node
          " ---------------------------------------------------------------------
          IF <fs_tures_dis>-item_type = 'ZGC'.
            cs_item-qua_car_val_dis += <fs_discharged>-qua_pcs_val_disch.
            cs_item-gro_car_val_dis += <fs_discharged>-gro_wei_val_dis.
            cs_item-tonne_car_dis   += <fs_discharged>-tonne_dis.
            cs_item-vol_car_cbm_dis += <fs_discharged>-volume_cbm.
          ELSE.
            cs_item-qua_pcs_val_dis += <fs_discharged>-qua_pcs_val_disch.
            cs_item-gro_wei_val_dis += <fs_discharged>-gro_wei_val_dis.
            cs_item-tonne_dis       += <fs_discharged>-tonne_dis.
            cs_item-vol_cbm_dis     += <fs_discharged>-volume_cbm.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
  ENDCASE.
  SORT cs_manifest-discharged BY tu_number_disch.
  APPEND cs_manifest TO cs_item-manifest_discharged.
ENDFORM.

" ---------------------------------------------------------------------
" FORM: Get Cargo
" ---------------------------------------------------------------------
FORM get_fwo_cargo.
  " ---------------------------------------------------------------------
  " 10: Create cargo not found
  " ---------------------------------------------------------------------
  DATA lf_cargo_not_found TYPE xfeld.
  DATA dref               TYPE REF TO data.

  " ---------------------------------------------------------------------
  " 20: Check all FWO for this FWQ
  " ---------------------------------------------------------------------
  LOOP AT <fs_trq_fwo> ASSIGNING FIELD-SYMBOL(<fs_fwo>).
    lf_cargo_not_found = abap_false.
    " ---------------------------------------------------------------------
    " 30: Create cargo record and assign fwo number
    " ---------------------------------------------------------------------
    CREATE DATA dref TYPE zotr_tt_cargo_details.
    IF <fs_fwo_cargo> IS NOT ASSIGNED.
      ASSIGN dref->* TO <fs_fwo_cargo>.
    ENDIF.
    APPEND INITIAL LINE TO <fs_fwo_cargo> ASSIGNING FIELD-SYMBOL(<fs_cargo>).
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
ENDFORM.

" ---------------------------------------------------------------------
" Form: Display Form
" ---------------------------------------------------------------------
FORM display_form.
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
ENDFORM.
