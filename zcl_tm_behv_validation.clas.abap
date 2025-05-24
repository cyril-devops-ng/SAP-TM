CLASS zcl_tm_behv_validation DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_frc_exceptions TYPE TABLE OF ztm_fwo_c_excpt WITH DEFAULT KEY.

    CLASS-DATA gc_trq_appl TYPE ztm_bo_application   VALUE 'FWO'.
    CLASS-DATA gc_tor_appl TYPE ztm_bo_application   VALUE 'FO'.
    CLASS-DATA fwo_cat     TYPE /scmtms/trq_category VALUE '03'.

    CLASS-METHODS pilotage_tolerance
      IMPORTING is_ctx  TYPE /bobf/s_frw_ctx_val
                it_key  TYPE /bobf/t_frw_key
                io_read TYPE REF TO /bobf/if_frw_read
                if_appl TYPE ztm_bo_application
      RAISING   zcx_tm_behv_exception.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create_bapi_ret_from_exception
      IMPORTING io_exception  TYPE REF TO cx_root
      RETURNING VALUE(rt_msg) TYPE bapiret2_t.

    CLASS-METHODS _________frc_exceptions
      RETURNING VALUE(rt_frc_exceptions) TYPE tt_frc_exceptions.

    CLASS-METHODS _________check_frc_exceptions
      IMPORTING io_read        TYPE REF TO /bobf/if_frw_read
                is_ctx         TYPE /bobf/s_frw_ctx_val
                it_key         TYPE /bobf/t_frw_key
      EXPORTING es_frc_exc     TYPE ztm_fwo_c_excpt
      RETURNING VALUE(rf_flag) TYPE flag.

    class-methods __validation_ovr
    importing if_validation_class type string
    returning value(rf_flag) type flag.

  PRIVATE SECTION.
    TYPES t_tm_charge_tol TYPE SORTED TABLE OF ztm_c_charge_tol WITH NON-UNIQUE KEY application trcharg_catcd trcharg_typecd.

    CLASS-DATA gt_tm_charge_tol        TYPE TABLE OF ztm_c_charge_tol.
    CLASS-DATA gt_sorted_tm_charge_tol TYPE t_tm_charge_tol.
    CLASS-DATA gt_trq_npa_doc_tl       TYPE TABLE OF ztm047_fwo_block.
    CLASS-DATA gt_tor_npa_doc_tl       TYPE TABLE OF ztm047_fo_block.
    CLASS-DATA gt_trq_root_data        TYPE /scmtms/t_trq_root_k.
    CLASS-DATA gt_tor_root_data        TYPE /scmtms/t_tor_root_k.
    CLASS-DATA gc_tor_root_type        TYPE tabname              VALUE '/SCMTMS/T_TOR_ROOT_K'.
    CLASS-DATA gc_trq_root_type        TYPE tabname              VALUE '/SCMTMS/T_TRQ_ROOT_K'.
    CLASS-DATA gt_frc_exceptions       TYPE tt_frc_exceptions.
ENDCLASS.


CLASS zcl_tm_behv_validation IMPLEMENTATION.
  METHOD class_constructor.
    " ---------------------------------------------------------------------
    " 10: Get Tolerance Data
    " ---------------------------------------------------------------------
    IF gt_tm_charge_tol IS INITIAL.
      SELECT FROM ztm_c_charge_tol
        FIELDS *
        WHERE status = @abap_true
        INTO TABLE @gt_tm_charge_tol.
    ENDIF.
    " ---------------------------------------------------------------------
    " 20: Get TRQ NPA Doc. Types
    " ---------------------------------------------------------------------
    IF gt_trq_npa_doc_tl IS INITIAL.
      SELECT FROM ztm047_fwo_block
        FIELDS *
        INTO TABLE @gt_trq_npa_doc_tl.
    ENDIF.
    " ---------------------------------------------------------------------
    " 30: Get TOR NPA Doc. Types
    " ---------------------------------------------------------------------
    IF gt_tor_npa_doc_tl IS INITIAL.
      SELECT FROM ztm047_fo_block
        FIELDS *
        INTO TABLE @gt_tor_npa_doc_tl.
    ENDIF.
    " ---------------------------------------------------------------------
    " 40: Get fRC exceptions
    " ---------------------------------------------------------------------
    frc_exceptions.
  ENDMETHOD.

  METHOD pilotage_tolerance.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA bo_dref TYPE REF TO data.
    FIELD-SYMBOLS <fs_tor> TYPE /scmtms/t_tor_root_k.
    FIELD-SYMBOLS <fs_trq> TYPE /scmtms/t_trq_root_k.
    DATA lf_calc_amount TYPE /scmtms/amount.

    " ---------------------------------------------------------------------
    " 20: Create Appl Data ref.
    " ---------------------------------------------------------------------
    CASE if_appl.
      WHEN gc_trq_appl.
        CREATE DATA bo_dref TYPE (gc_trq_root_type).
      WHEN gc_tor_appl.
        CREATE DATA bo_dref TYPE (gc_tor_root_type).
    ENDCASE.
    " ---------------------------------------------------------------------
    " 30: Create Node Data ref.
    " ---------------------------------------------------------------------
    io_read->retrieve( EXPORTING iv_node      = is_ctx-node_key
                                 it_key       = it_key
                                 iv_fill_data = abap_true
                       IMPORTING et_data      = bo_dref->* ).
    " ---------------------------------------------------------------------
    " 40: Assign Data ref.
    " ---------------------------------------------------------------------
    CASE if_appl.
      WHEN gc_trq_appl.
        ASSIGN bo_dref->* TO <fs_trq>.
      WHEN gc_tor_appl.
        ASSIGN bo_dref->* TO <fs_tor>.
    ENDCASE.
    " ---------------------------------------------------------------------
    " 50: Check Data ref is assigned.
    " ---------------------------------------------------------------------
    IF xsdbool( <fs_tor> IS NOT ASSIGNED AND <fs_trq> IS NOT ASSIGNED ) = abap_true.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 60: Check doc. is a pilotage document
    " ---------------------------------------------------------------------
    CASE if_appl.
      " ---------------------------------------------------------------------
      " 70: Application check: TRQ documents
      " ---------------------------------------------------------------------
      WHEN gc_trq_appl.
        " ---------------------------------------------------------------------
        " 70. 1 Check TRQ is pilotage
        " ---------------------------------------------------------------------
        ASSIGN <fs_trq>[ 1 ]-trq_type TO FIELD-SYMBOL(<Fs_trq_type>).
        IF xsdbool( line_exists( gt_trq_npa_doc_tl[ trq_type = <fs_trq_type> ] ) )
           = abap_false.
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " 70. 2 Check tolerance is set up
        " ---------------------------------------------------------------------
        IF xsdbool( line_exists( gt_tm_charge_tol[ application = gc_trq_appl ] ) )
           = abap_false.
          RAISE EXCEPTION NEW zcx_tm_behv_exception( textid         = zcx_tm_behv_exception=>tm_null_tolerance
                                                     tm_application = CONV #( gc_trq_appl )
                                                     tm_doc_type    = CONV #( <fs_trq_type> ) ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 70.3 Get charges
        " ---------------------------------------------------------------------
        /scmtms/cl_tcc_do_helper=>retrive_do_nodes(
          EXPORTING is_ctx            = VALUE #( host_bo_key        = /scmtms/if_trq_c=>sc_bo_key
                                                 host_root_node_key = /scmtms/if_trq_c=>sc_node-root )
                    it_root_key       = CORRESPONDING #( <fs_trq> MAPPING key = key )
          IMPORTING et_charge_element = DATA(lt_charge_element) ).
        " ---------------------------------------------------------------------
        " 70.4 Filter appl. specific charges
        " ---------------------------------------------------------------------
        DELETE lt_charge_element WHERE ( inactive = abap_true OR is_technical = abap_true ) AND tcet084 <> 'NPA_BILL_DIFF'.
        " ---------------------------------------------------------------------
        " Check if relevant charge exists
        " *********************************************************************
        IF lt_charge_element IS INITIAL.
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " Create Filterable charge configuration
        " *********************************************************************
        CLEAR gt_sorted_tm_charge_tol.
        gt_sorted_tm_charge_tol = CORRESPONDING #( gt_tm_charge_tol ).
        " ---------------------------------------------------------------------
        " Apply filter
        " *********************************************************************
        DATA(lt_trq_npa_tol_charges) = FILTER #( gt_sorted_tm_charge_tol WHERE application = gc_trq_appl ).
        " ---------------------------------------------------------------------
        " 70.5 Check charge with discrepancy from tolerance
        " ---------------------------------------------------------------------
        DO lines( lt_trq_npa_tol_charges ) TIMES.
          ASSIGN lt_trq_npa_tol_charges[ sy-index ] TO FIELD-SYMBOL(<fs_charge>).
          LOOP AT lt_charge_element ASSIGNING FIELD-SYMBOL(<fs_charge_element>)
               WHERE     tcet084          = <fs_charge>-trcharg_typecd
                     AND calc_amount_curr = <fs_charge>-zz_currency.
            " ---------------------------------------------------------------------
            " Get calculated amount
            " ---------------------------------------------------------------------
            lf_calc_amount = <fs_charge_element>-calc_amount.
            lf_calc_amount /= <fs_charge_element>-rate_amount.
            lf_calc_amount = abs( lf_calc_amount  ).
            " ---------------------------------------------------------------------
            " Raise exception if charge exceeds tolerance
            " ---------------------------------------------------------------------
            IF lf_calc_amount > <fs_charge>-zz_tolerance.
              RAISE EXCEPTION NEW zcx_tm_behv_exception( textid         = zcx_tm_behv_exception=>tm_tolerance_exceeded
                                                         tm_charge_type = CONV #( <fs_charge>-trcharg_typecd )
                                                         tm_doc_type    = CONV #( <fs_trq_type> )
                                                         tm_tolerance   = CONV #( <fs_charge>-zz_tolerance ) ).
            ENDIF.
          ENDLOOP.
          IF xsdbool( <fs_charge_element> IS NOT ASSIGNED AND line_exists(
                          lt_charge_element[ tcet084 = <fs_charge>-trcharg_typecd ] ) )
             = abap_true.
            " ---------------------------------------------------------------------
            " Raise exception if no charge is configured for pilotage
            " *********************************************************************
            RAISE EXCEPTION NEW zcx_tm_behv_exception( textid      = zcx_tm_behv_exception=>tm_tolerance_null_currency
                                                       tm_currency = <fs_charge_element>-calc_amount_curr ).
          ELSE.
            UNASSIGN <fs_charge_element>.
          ENDIF.
        ENDDO.

      " ---------------------------------------------------------------------
      " 80: Application check: TOR documents
      " ---------------------------------------------------------------------
      WHEN gc_tor_appl.
        " ---------------------------------------------------------------------
        " 80. 1 Check TOR is pilotage
        " ---------------------------------------------------------------------
        ASSIGN <fs_tor>[ 1 ]-tor_type TO FIELD-SYMBOL(<Fs_tor_type>).
        IF xsdbool( line_exists( gt_tor_npa_doc_tl[ tor_type = <fs_tor_type> ] ) )
           = abap_false.
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " 80. 2 Check tolerance is set up
        " ---------------------------------------------------------------------
        IF xsdbool( line_exists( gt_tm_charge_tol[ application = gc_tor_appl ] ) )
           = abap_false.
          RAISE EXCEPTION NEW zcx_tm_behv_exception( textid         = zcx_tm_behv_exception=>tm_null_tolerance
                                                     tm_application = CONV #( gc_tor_appl )
                                                     tm_doc_type    = CONV #( <fs_tor_type> ) ).
        ENDIF.
        " ---------------------------------------------------------------------
        " 80.3 Get charges
        " ---------------------------------------------------------------------
        /scmtms/cl_tcc_do_helper=>retrive_do_nodes(
          EXPORTING is_ctx            = VALUE #( host_bo_key        = /scmtms/if_tor_c=>sc_bo_key
                                                 host_root_node_key = /scmtms/if_tor_c=>sc_node-root )
                    it_root_key       = CORRESPONDING #( <fs_tor> MAPPING key = key )
          IMPORTING et_charge_element = lt_charge_element ).
        " ---------------------------------------------------------------------
        " 80.4 Filter appl. specific charges
        " ---------------------------------------------------------------------
        DELETE lt_charge_element WHERE ( inactive = abap_true OR is_technical = abap_true ) AND tcet084 <> 'NPA_BILL_DIFF'.
        IF lt_charge_element IS INITIAL.
          RETURN.
        ENDIF.
        " ---------------------------------------------------------------------
        " Create Filterable charge configuration
        " *********************************************************************
        CLEAR gt_sorted_tm_charge_tol.
        gt_sorted_tm_charge_tol = CORRESPONDING #( gt_tm_charge_tol ).
        " ---------------------------------------------------------------------
        " Apply filter
        " *********************************************************************
        DATA(lt_tor_npa_tol_charges) = FILTER #( gt_sorted_tm_charge_tol WHERE application = gc_tor_appl ).
        " ---------------------------------------------------------------------
        " 80.5 Check charge with discrepancy from tolerance
        " ---------------------------------------------------------------------
        DO lines( lt_tor_npa_tol_charges ) TIMES.
          ASSIGN lt_tor_npa_tol_charges[ sy-index ] TO <fs_charge>.
          LOOP AT lt_charge_element ASSIGNING <fs_charge_element>
               WHERE     tcet084          = <fs_charge>-trcharg_typecd
                     AND calc_amount_curr = <fs_charge>-zz_currency.
            CLEAR lf_calc_amount.
            " ---------------------------------------------------------------------
            " Get calculated amount
            " ---------------------------------------------------------------------
            lf_calc_amount = <fs_charge_element>-calc_amount.
            lf_calc_amount /= <fs_charge_element>-rate_amount.
            lf_calc_amount = abs( lf_calc_amount  ).
            " ---------------------------------------------------------------------
            " Raise exception if charge exceeds tolerance
            " ---------------------------------------------------------------------
            IF lf_calc_amount > <fs_charge>-zz_tolerance.
              RAISE EXCEPTION NEW zcx_tm_behv_exception( textid         = zcx_tm_behv_exception=>tm_tolerance_exceeded
                                                         tm_charge_type = CONV #( <fs_charge>-trcharg_typecd )
                                                         tm_doc_type    = CONV #( <fs_tor_type> )
                                                         tm_tolerance   = CONV #( <fs_charge>-zz_tolerance ) ).
            ENDIF.
          ENDLOOP.
          IF xsdbool( <fs_charge_element> IS NOT ASSIGNED AND line_exists(
                          lt_charge_element[ tcet084 = <fs_charge>-trcharg_typecd ] ) )
             = abap_true.
            " ---------------------------------------------------------------------
            " Raise exception if no charge is configured for pilotage
            " *********************************************************************
            RAISE EXCEPTION NEW zcx_tm_behv_exception( textid      = zcx_tm_behv_exception=>tm_tolerance_null_currency
                                                       tm_currency = <fs_charge_element>-calc_amount_curr ).
          ELSE.
            UNASSIGN <fs_charge_element>.
          ENDIF.
        ENDDO.
    ENDCASE.
  ENDMETHOD.

  METHOD create_bapi_ret_from_exception.
    " ---------------------------------------------------------------------
    " 10: Create BAPI return from Exception
    " ---------------------------------------------------------------------
    CALL FUNCTION 'RS_EXCEPTION_TO_BAPIRET2'
      EXPORTING i_r_exception = io_exception
      CHANGING  c_t_bapiret2  = rt_msg.
  ENDMETHOD.

  METHOD _________frc_exceptions.
    " ---------------------------------------------------------------------
    " 10: Retrieve FRC exceptions
    " *********************************************************************
    SELECT FROM ztm_fwo_c_excpt
      FIELDS
      *
      WHERE status
            = @abap_true
      INTO TABLE
      @rt_frc_exceptions.
  ENDMETHOD.

  METHOD _________check_frc_exceptions.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_root_data      TYPE /scmtms/t_trq_root_k.
    DATA lt_root_data_bi   TYPE /scmtms/t_trq_root_k.
    DATA lo_str            TYPE REF TO cl_abap_structdescr.
    DATA lf_exception_flag TYPE flag.
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
    ASSIGN lt_root_data_bi[ 1 ] TO FIELD-SYMBOL(<fs_fwo_root_bi>).
    " ---------------------------------------------------------------------
    " 60: Check for FRC Exceptions
    " ---------------------------------------------------------------------
    lo_str ?= cl_abap_structdescr=>describe_by_data( <fs_fwo_root> ).
    lf_exception_flag = abap_false.
    " ---------------------------------------------------------------------
    " 70: Do FRC check
    " ---------------------------------------------------------------------
    do_frc_exception_check lf_exception_flag.
    " ---------------------------------------------------------------------
    " 80: Set return flag
    " ---------------------------------------------------------------------
    rf_flag = lf_exception_flag.
  ENDMETHOD.

  METHOD __validation_ovr.
    SELECT SINGLE FROM ztm_frc_val_ovr
      FIELDS @abap_true
      WHERE validation_class
            = @if_validation_class
        AND override_conf
            = @abap_true
      INTO @rf_flag.
  ENDMETHOD.

ENDCLASS.
