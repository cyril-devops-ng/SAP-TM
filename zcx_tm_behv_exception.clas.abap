CLASS zcx_tm_behv_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA tm_application TYPE string.
    DATA tm_doc_type    TYPE string.
    DATA tm_charge_type TYPE string.
    DATA tm_tolerance   TYPE string.
    DATA tm_currency    TYPE waers.

    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    METHODS constructor
      IMPORTING textid         LIKE if_t100_message=>t100key OPTIONAL
                !previous      LIKE previous                 OPTIONAL
                tm_application TYPE string                   OPTIONAL
                tm_doc_type    TYPE string                   OPTIONAL
                tm_charge_type TYPE string                   OPTIONAL
                tm_tolerance   TYPE string                   OPTIONAL
                tm_currency    TYPE waers                    OPTIONAL.

    CONSTANTS:
      BEGIN OF tm_null_tolerance,
        msgid TYPE symsgid      VALUE 'ZTM_BEHV_MSG',
        msgno TYPE symsgno      VALUE '004',
        attr1 TYPE scx_attrname VALUE 'TM_APPLICATION',
        attr2 TYPE scx_attrname VALUE 'TM_DOC_TYPE',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_null_tolerance.
    CONSTANTS:
      BEGIN OF tm_tolerance_exceeded,
        msgid TYPE symsgid      VALUE 'ZTM_BEHV_MSG',
        msgno TYPE symsgno      VALUE '005',
        attr1 TYPE scx_attrname VALUE 'TM_CHARGE_TYPE',
        attr2 TYPE scx_attrname VALUE 'TM_TOLERANCE',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_tolerance_exceeded.
    CONSTANTS:
      BEGIN OF tm_tolerance_null_currency,
        msgid TYPE symsgid      VALUE 'ZTM_BEHV_MSG',
        msgno TYPE symsgno      VALUE '006',
        attr1 TYPE scx_attrname VALUE 'TM_CURRENCY',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF tm_tolerance_null_currency.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_tm_behv_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    me->tm_application = tm_application.
    me->tm_doc_type    = tm_doc_type.
    me->tm_charge_type = tm_charge_type.
    me->tm_tolerance   = tm_tolerance.
    me->tm_currency    = tm_currency.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
