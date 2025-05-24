CLASS zcx_tm_form_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    METHODS constructor
      IMPORTING textid       LIKE if_t100_message=>t100key OPTIONAL
                !previous    LIKE previous                 OPTIONAL
                tm_fieldname TYPE string                   OPTIONAL.

    CONSTANTS:
      BEGIN OF tm_null_manifest,
        msgid TYPE symsgid      VALUE 'ZTM_FORM_MSG',
        msgno TYPE symsgno      VALUE '010',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_null_manifest.
    CONSTANTS:
      BEGIN OF tm_record_not_found,
        msgid TYPE symsgid      VALUE 'ZTM_FORM_MSG',
        msgno TYPE symsgno      VALUE '003',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_record_not_found.
    CONSTANTS:
      BEGIN OF tm_alv_error,
        msgid TYPE symsgid      VALUE 'ZTM_FORM_MSG',
        msgno TYPE symsgno      VALUE '011',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_alv_error.
    CONSTANTS:
      BEGIN OF tm_too_many_records,
        msgid TYPE symsgid      VALUE 'ZTM_FORM_MSG',
        msgno TYPE symsgno      VALUE '012',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_too_many_records.
    CONSTANTS:
      BEGIN OF tm_form_proc_error,
        msgid TYPE symsgid      VALUE 'ZTM_FORM_MSG',
        msgno TYPE symsgno      VALUE '013',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_form_proc_error.
    CONSTANTS:
      BEGIN OF tm_invalid_bor,
        msgid TYPE symsgid      VALUE 'ZTM_FORM_MSG',
        msgno TYPE symsgno      VALUE '014',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_invalid_bor.
    CONSTANTS:
      BEGIN OF tm_null_fieldname,
        msgid TYPE symsgid      VALUE 'ZTM_FORM_MSG',
        msgno TYPE symsgno      VALUE '015',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_null_fieldname.
    CONSTANTS:
      BEGIN OF tm_field_does_not_exist,
        msgid TYPE symsgid      VALUE 'ZTM_FORM_MSG',
        msgno TYPE symsgno      VALUE '016',
        attr1 TYPE scx_attrname VALUE 'TM_FIELDNAME',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_field_does_not_exist.
    constants:
      begin of tm_null_keys,
        msgid type symsgid value 'ZTM_FORM_MSG',
        msgno type symsgno value '017',
        attr1 type scx_attrname value 'attr1',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of tm_null_keys.
    constants:
      begin of tm_null_data,
        msgid type symsgid value 'ZTM_FORM_MSG',
        msgno type symsgno value '018',
        attr1 type scx_attrname value 'attr1',
        attr2 type scx_attrname value 'attr2',
        attr3 type scx_attrname value 'attr3',
        attr4 type scx_attrname value 'attr4',
      end of tm_null_data.
    DATA tm_fieldname TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_tm_form_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    me->tm_fieldname = tm_fieldname.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
