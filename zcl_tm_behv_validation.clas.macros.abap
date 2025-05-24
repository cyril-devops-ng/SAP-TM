*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE frc_exceptions.
gt_frc_exceptions = zcl_tm_behv_validation=>_________frc_exceptions(  ).
end-of-definition.

define do_frc_exception_check.
  DO lines( lo_str->components ) TIMES.
    ASSIGN lo_str->components[ sy-index ]-name TO FIELD-SYMBOL(<fs_fname>).
    IF line_exists( gt_frc_exceptions[ tabname   = '/SCMTMS/D_TRQROT'
                                       fieldname = <fs_fname> ] ).
      assign gt_frc_exceptions[ tabname   = '/SCMTMS/D_TRQROT'
                                       fieldname = <fs_fname> ] to FIELD-SYMBOL(<fs_frc_exc>).
      ASSIGN COMPONENT <fs_fname> OF STRUCTURE <fs_fwo_root> TO FIELD-SYMBOL(<fs_new_value>).
      ASSIGN COMPONENT <fs_fname> OF STRUCTURE <fs_fwo_root_bi> TO FIELD-SYMBOL(<fs_old_value>).
      IF xsdbool( <fs_new_value> IS ASSIGNED AND <fs_old_value> IS ASSIGNED  ) = abap_true.
        IF xsdbool( <fs_new_value> = <fs_old_value> ) = abap_false.
          es_frc_exc = corresponding #( <fs_frc_exc> ).
          &1 = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.
END-OF-DEFINITION.
