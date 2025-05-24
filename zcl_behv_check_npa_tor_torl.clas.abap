class ZCL_BEHV_CHECK_NPA_TOR_TORL definition
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



CLASS ZCL_BEHV_CHECK_NPA_TOR_TORL IMPLEMENTATION.


  method /BOBF/IF_FRW_VALIDATION~EXECUTE.
" ---------------------------------------------------------------------
    " 10: Create Request key
    " ---------------------------------------------------------------------
    FIELD-SYMBOLS <fs_key> TYPE /bobf/conf_key.

    create_request_key.
    " ---------------------------------------------------------------------
    " 20: Try Static validation for tolerance
    " ---------------------------------------------------------------------
    TRY.
        zcl_tm_behv_validation=>pilotage_tolerance( is_ctx  = is_ctx
                                                    it_key  = it_key
                                                    io_read = io_read
                                                    if_appl = zcl_tm_behv_validation=>gc_tor_appl ).
        " ---------------------------------------------------------------------
        " 30: Raise exception
        " ---------------------------------------------------------------------
      CATCH zcx_tm_behv_exception INTO DATA(lo_tm_behv_ex).
        clear: et_failed_key, eo_message.
        eo_message = /bobf/cl_frw_factory=>get_message( ).
        DATA(lt_msg) = zcl_tm_behv_validation=>create_bapi_ret_from_exception( io_exception = lo_tm_behv_ex  ).
        /sapyl/cl_helper_base=>add_bapi2bobf( EXPORTING it_bapiret    = lt_msg
                                                        io_message    = eo_message
                                                        iv_node       = is_ctx-node_key
                                                        iv_key        = <fs_key>
                                              CHANGING  ct_failed_key = et_failed_key ).
        RETURN.
    ENDTRY.
  endmethod.
ENDCLASS.
