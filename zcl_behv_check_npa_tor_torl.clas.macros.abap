*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE data_definition.
  DATA lt_root_data    TYPE /scmtms/t_tor_root_k.
  DATA lt_root_data_bi TYPE /scmtms/t_tor_root_k.
  DATA lo_tor_srvmgr   TYPE REF TO /bobf/if_tra_service_manager.
END-OF-DEFINITION.
DEFINE x_msg_to_warning.
  LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<fs_msg>).
    IF <fs_msg>-number = &1.
      <fs_msg>-type = 'W'.
    ENDIF.
  ENDLOOP.
END-OF-DEFINITION.
DEFINE create_request_key.
  data_definition.
  io_read->retrieve( EXPORTING iv_node      = is_ctx-node_key
                               it_key       = it_key
                               iv_fill_data = abap_true
                     IMPORTING et_data      = lt_root_data ).
  IF xsdbool( line_exists( lt_root_data[ 1 ] ) )
     = abap_true.
    ASSIGN lt_root_data[ 1 ]-key TO <fs_key>.
  ENDIF.
END-OF-DEFINITION.
