class /VPCOE/CL_PCKF_MATCLASS_DAC definition
  public
  create public .

public section.

  interfaces /VPCOE/IF_PCKF_MATCLASS_DAC .

  aliases CS_CHARACTERISTIC_TYPE
    for /VPCOE/IF_PCKF_MATCLASS_DAC~CS_CHARACTERISTIC_TYPE .
  PROTECTED SECTION.

    METHODS:

      convert_to_bapi_values
        IMPORTING
          !it_characteristics  TYPE /vpcoe/if_pckf_matclass_dac=>gtyt_characteristic_value
        EXPORTING
          !et_char_values_num  TYPE tt_bapi1003_alloc_values_num
          !et_char_values_char TYPE tt_bapi1003_alloc_values_char
          !et_char_values_curr TYPE tt_bapi1003_alloc_values_curr,

      convert_from_bapi_values
        IMPORTING
          !it_char_values_num  TYPE tt_bapi1003_alloc_values_num
          !it_char_values_char TYPE tt_bapi1003_alloc_values_char
          !it_char_values_curr TYPE tt_bapi1003_alloc_values_curr
        EXPORTING
          !et_characteristics  TYPE /vpcoe/if_pckf_matclass_dac=>gtyt_characteristic_value,

      bapi_change_matclass
        IMPORTING
          !iv_material         TYPE matnr
          !iv_keydate          TYPE dats DEFAULT sy-datum
          !iv_changenum        TYPE aennr OPTIONAL
          !iv_class            TYPE klasse_d
          !it_char_values_num  TYPE tt_bapi1003_alloc_values_num
          !it_char_values_char TYPE tt_bapi1003_alloc_values_char
          !it_char_values_curr TYPE tt_bapi1003_alloc_values_curr
        EXPORTING
          !ev_failed           TYPE abap_bool
          !et_messages         TYPE /vpcoe/t_uph_msg,

      bapi_read_matclass
        IMPORTING
          !iv_material         TYPE matnr
          !iv_keydate          TYPE dats DEFAULT sy-datum
          !iv_changenum        TYPE aennr OPTIONAL
          !iv_class            TYPE klasse_d
        EXPORTING
          !et_char_values_num  TYPE tt_bapi1003_alloc_values_num
          !et_char_values_char TYPE tt_bapi1003_alloc_values_char
          !et_char_values_curr TYPE tt_bapi1003_alloc_values_curr
          !ev_failed           TYPE abap_bool
          !et_messages         TYPE /vpcoe/t_uph_msg,

      bapi_create_change_number
        IMPORTING
          !iv_change_no      TYPE aennr OPTIONAL
          !iv_valid_from     TYPE cc_datuv_bi
          !iv_auth_group     TYPE cc_aenbe OPTIONAL
          !iv_description    TYPE aetxt OPTIONAL
          !iv_commit         TYPE abap_bool
        EXPORTING
          !ev_changenum      TYPE aennr
          !ev_already_exists TYPE abap_bool
          !ev_failed         TYPE abap_bool,

      bapi_check_change_number
        IMPORTING
          !iv_changenum  TYPE aennr
        EXPORTING
          !ev_exists     TYPE abap_bool
          !ev_valid      TYPE abap_bool
          !ev_valid_from TYPE dats.

private section.
ENDCLASS.



CLASS /VPCOE/CL_PCKF_MATCLASS_DAC IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_matclass_dac~check_change_number.

    DATA: lv_aennr TYPE aennr.

    CLEAR: ev_changenum, ev_exists, ev_valid.

    lv_aennr = iv_changenum.
    IF  iv_changenum IS INITIAL AND iv_description IS NOT INITIAL.

      SELECT SINGLE aennr FROM aenr WHERE aetxt = @iv_description INTO @lv_aennr.

    ENDIF.

    IF lv_aennr IS INITIAL.
      RETURN.
    ENDIF.

    ev_changenum = lv_aennr.

    bapi_check_change_number( EXPORTING iv_changenum = lv_aennr IMPORTING ev_exists = ev_exists ev_valid = ev_valid ).

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_matclass_dac~create_change_number.

    DATA: lv_valid_from_conv TYPE cc_datuv_bi,
          lv_message         TYPE string.

    CLEAR: ev_changenum, ev_failed, et_messages.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal = iv_valid_from
      IMPORTING
        date_external = lv_valid_from_conv.

    IF sy-subrc <> 0.
      ev_failed = abap_true.
      RETURN.
    ENDIF.

    bapi_create_change_number(
      EXPORTING
        iv_change_no = iv_change_no
        iv_valid_from = lv_valid_from_conv
        iv_auth_group = iv_auth_group
        iv_description = iv_description
        iv_commit      = iv_commit
      IMPORTING
        ev_changenum = ev_changenum
        ev_failed = ev_failed
    ).

    IF ev_failed = abap_true.
      MESSAGE e070(/vpcoe/pckf) WITH COND #( WHEN iv_description IS NOT INITIAL THEN iv_description ELSE iv_change_no ) INTO lv_message.
      APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 ) TO et_messages.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_matclass_dac~read_classification.

    DATA: lv_changenumber     TYPE aennr,
          lv_keydate          TYPE dats,
          lt_char_values_num  TYPE tt_bapi1003_alloc_values_num,
          lt_char_values_char TYPE tt_bapi1003_alloc_values_char,
          lt_char_values_curr TYPE tt_bapi1003_alloc_values_curr.

    CLEAR: ev_failed, et_messages, et_characteristics.

    lv_changenumber = iv_changenum.
    lv_keydate      = iv_keydate.

    bapi_read_matclass(
      EXPORTING
        iv_material         = iv_material
        iv_keydate          = lv_keydate
        iv_changenum        = lv_changenumber
        iv_class            = iv_class
      IMPORTING
        et_char_values_num  = lt_char_values_num
        et_char_values_char = lt_char_values_char
        et_char_values_curr = lt_char_values_curr
        ev_failed           = ev_failed
        et_messages         = et_messages
    ).

    IF lt_char_values_num IS INITIAL AND
       lt_char_values_char IS INITIAL AND
       lt_char_values_curr IS INITIAL.
      RETURN.
    ENDIF.

    convert_from_bapi_values(
      EXPORTING
        it_char_values_num  = lt_char_values_num
        it_char_values_char = lt_char_values_char
        it_char_values_curr = lt_char_values_curr
      IMPORTING
        et_characteristics  = et_characteristics
    ).

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_matclass_dac~update_classification.

    DATA: lv_changenumber TYPE aennr,
          lv_keydate      TYPE dats.

    CLEAR: ev_failed, et_messages.

    lv_changenumber = iv_changenum.
    lv_keydate      = iv_keydate.

    convert_to_bapi_values(
      EXPORTING
        it_characteristics  = it_characteristics
      IMPORTING
        et_char_values_num  = DATA(lt_char_values_num)
        et_char_values_char = DATA(lt_char_values_char)
        et_char_values_curr = DATA(lt_char_values_curr)
    ).

    IF lt_char_values_num IS INITIAL AND
       lt_char_values_char IS INITIAL AND
       lt_char_values_curr IS INITIAL.
      RETURN.
    ENDIF.

    bapi_change_matclass(
      EXPORTING
        iv_material         = iv_material
        iv_keydate          = lv_keydate
        iv_changenum        = lv_changenumber
        iv_class            = iv_class
        it_char_values_num  = lt_char_values_num
        it_char_values_char = lt_char_values_char
        it_char_values_curr = lt_char_values_curr
      IMPORTING
        ev_failed           = ev_failed
        et_messages         = et_messages
    ).

    IF iv_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD bapi_change_matclass.

    DATA: lt_bapireturn       TYPE bapirettab,
          lv_objectkey        TYPE objnum,
          lt_char_values_num  TYPE tt_bapi1003_alloc_values_num,
          lt_char_values_char TYPE tt_bapi1003_alloc_values_char,
          lt_char_values_curr TYPE tt_bapi1003_alloc_values_curr,
          lv_keydate          TYPE dats.

    CLEAR: et_messages, ev_failed.

    lv_objectkey = iv_material.

    lt_char_values_num = it_char_values_num.
    lt_char_values_char = it_char_values_char.
    lt_char_values_curr = it_char_values_curr.

    lv_keydate = iv_keydate.

    IF iv_changenum IS NOT INITIAL.

      bapi_check_change_number(
       EXPORTING
         iv_changenum  = iv_changenum
       IMPORTING
         ev_valid_from = lv_keydate
     ).
    ENDIF.

    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      EXPORTING
        objectkey          = lv_objectkey
        objecttable        = 'MARA'
        classnum           = iv_class
        classtype          = '001'
        changenumber       = iv_changenum
        keydate            = lv_keydate
      TABLES
        allocvaluesnumnew  = lt_char_values_num
        allocvaluescharnew = lt_char_values_char
        allocvaluescurrnew = lt_char_values_curr
        return             = lt_bapireturn.

    IF line_exists( lt_bapireturn[ type = 'E' ] ).
      LOOP AT lt_bapireturn INTO DATA(ls_bapireturn).

        APPEND INITIAL LINE TO et_messages REFERENCE INTO DATA(lr_message).
        lr_message->msgno = ls_bapireturn-number.
        lr_message->msgid = ls_bapireturn-id.
        lr_message->msgty = ls_bapireturn-type.
        lr_message->msgv1 = ls_bapireturn-message_v1.
        lr_message->msgv2 = ls_bapireturn-message_v2.
        lr_message->msgv3 = ls_bapireturn-message_v3.
        lr_message->msgv4 = ls_bapireturn-message_v4.

        IF lr_message->msgty = 'E'.
          ev_failed = abap_true.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD bapi_check_change_number.

    DATA: ls_change_header TYPE aenr_api02.

    CLEAR: ev_exists, ev_valid.

    CHECK iv_changenum IS NOT INITIAL.

    CALL FUNCTION 'CCAP_ECN_HEADER_READ'
      EXPORTING
        change_no       = iv_changenum
      IMPORTING
        change_header   = ls_change_header
      EXCEPTIONS
        no_record_found = 1
        error           = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ev_exists = abap_true.

    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = ls_change_header-valid_from
      IMPORTING
        date_internal            = ev_valid_from
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ls_change_header-status <> '01' OR ls_change_header-deletion_mark = abap_true.
      RETURN.
    ENDIF.

    ev_valid = abap_true.

  ENDMETHOD.


  METHOD bapi_create_change_number.
* Create a material classfication change number

    DATA:
      lv_changenum      TYPE aennr,
      ls_change_header  TYPE aenr_api01,
      lv_no_commit_work TYPE abap_bool.

    CLEAR: ev_failed, ev_already_exists.

    IF iv_change_no IS NOT INITIAL.
      ls_change_header-change_no = iv_change_no.
    ENDIF.

    ls_change_header-status = '01'.
    ls_change_header-valid_from = iv_valid_from.
    ls_change_header-auth_group = iv_auth_group.
    ls_change_header-descript = iv_description.

    IF iv_commit = abap_true.
      lv_no_commit_work = abap_false.
    ELSE.
      lv_no_commit_work = abap_true.
    ENDIF.

    CALL FUNCTION 'CCAP_ECN_CREATE'
      EXPORTING
        change_header            = ls_change_header
        object_char              = VALUE aenv_api01( active = abap_true )
        object_cls               = VALUE aenv_api01( active = abap_true )
        object_cls_maint         = VALUE aenv_api01( active = abap_true )
        object_mat               = VALUE aenv_api01( active = abap_true obj_requ = abap_true mgtrec_gen = abap_true )
        fl_commit_and_wait       = abap_true
        fl_no_commit_work        = lv_no_commit_work
      IMPORTING
        change_no                = ev_changenum
      EXCEPTIONS
        change_no_already_exists = 1
        error                    = 2
        OTHERS                   = 3.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        ev_already_exists = ev_failed = abap_true.
      WHEN OTHERS.
        ev_failed = abap_true.
    ENDCASE.

  ENDMETHOD.


  METHOD bapi_read_matclass.
    DATA: lt_bapireturn       TYPE bapirettab,
          lv_objectkey        TYPE objnum,
          lv_keydate          TYPE dats.

    CLEAR: et_messages, ev_failed.

    lv_objectkey = iv_material.

    lv_keydate = iv_keydate.

    IF iv_changenum IS NOT INITIAL.

      bapi_check_change_number(
       EXPORTING
         iv_changenum  = iv_changenum
       IMPORTING
         ev_valid_from = lv_keydate
     ).
    ENDIF.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey        = lv_objectkey
        objecttable      = 'MARA'
        classnum         = iv_class
        classtype        = '001'
        keydate          = lv_keydate
        unvaluated_chars = abap_true
      TABLES
        allocvaluesnum   = et_char_values_num
        allocvalueschar  = et_char_values_char
        allocvaluescurr  = et_char_values_curr
        return           = lt_bapireturn.

    IF line_exists( lt_bapireturn[ type = 'E' ] ).
      LOOP AT lt_bapireturn INTO DATA(ls_bapireturn).

        APPEND INITIAL LINE TO et_messages REFERENCE INTO DATA(lr_message).
        lr_message->msgno = ls_bapireturn-number.
        lr_message->msgid = ls_bapireturn-id.
        lr_message->msgty = ls_bapireturn-type.
        lr_message->msgv1 = ls_bapireturn-message_v1.
        lr_message->msgv2 = ls_bapireturn-message_v2.
        lr_message->msgv3 = ls_bapireturn-message_v3.
        lr_message->msgv4 = ls_bapireturn-message_v4.

        IF lr_message->msgty = 'E'.
          ev_failed = abap_true.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD convert_from_bapi_values.

    CLEAR: et_characteristics.

    LOOP AT it_char_values_num INTO DATA(ls_char_values_num).

      APPEND INITIAL LINE TO et_characteristics REFERENCE INTO DATA(lr_characteristics).
      lr_characteristics->characteristic = ls_char_values_num-charact.
      lr_characteristics->value = ls_char_values_num-value_from.
      lr_characteristics->value_num = ls_char_values_num-value_from.
      lr_characteristics->unit_num = ls_char_values_num-unit_from.
      lr_characteristics->value_to = ls_char_values_num-value_to.
      lr_characteristics->data_type = cs_characteristic_type-num.

    ENDLOOP.

    LOOP AT it_char_values_char INTO DATA(ls_char_values_char).

      APPEND INITIAL LINE TO et_characteristics REFERENCE INTO lr_characteristics.
      lr_characteristics->characteristic = ls_char_values_char-charact.
      lr_characteristics->value = ls_char_values_char-value_char.
      lr_characteristics->value_char = ls_char_values_char-value_char.
      lr_characteristics->value_char_neutral = ls_char_values_char-value_neutral.
      lr_characteristics->data_type = cs_characteristic_type-char.
    ENDLOOP.

    LOOP AT it_char_values_curr INTO DATA(ls_char_values_curr).

      APPEND INITIAL LINE TO et_characteristics REFERENCE INTO lr_characteristics.
      lr_characteristics->characteristic = ls_char_values_curr-charact.
      lr_characteristics->value = ls_char_values_curr-value_from.
      lr_characteristics->value_curr = ls_char_values_curr-value_from.
      lr_characteristics->value_to = ls_char_values_curr-value_to.
      lr_characteristics->data_type = cs_characteristic_type-curr.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_to_bapi_values.

    CLEAR: et_char_values_num, et_char_values_char, et_char_values_curr.

    LOOP AT it_characteristics INTO DATA(ls_characteristics).

      IF ls_characteristics-value IS NOT INITIAL.
        CASE ls_characteristics-data_type.

          WHEN cs_characteristic_type-char.
            APPEND INITIAL LINE TO et_char_values_char REFERENCE INTO DATA(lr_char_values_char).
            lr_char_values_char->charact = ls_characteristics-characteristic.
            lr_char_values_char->value_char = ls_characteristics-value.
            lr_char_values_char->value_neutral = ls_characteristics-value_char_neutral.

          WHEN cs_characteristic_type-num.
            APPEND INITIAL LINE TO et_char_values_num REFERENCE INTO DATA(lr_char_values_num).
            lr_char_values_num->charact = ls_characteristics-characteristic.
            lr_char_values_num->value_from = ls_characteristics-value.
            lr_char_values_num->unit_from = ls_characteristics-unit_num.
            lr_char_values_num->value_to = ls_characteristics-value_to.

          WHEN cs_characteristic_type-curr.
            APPEND INITIAL LINE TO et_char_values_curr REFERENCE INTO DATA(lr_char_values_curr).
            lr_char_values_curr->charact = ls_characteristics-characteristic.
            lr_char_values_curr->value_from = ls_characteristics-value.
            lr_char_values_curr->value_to = ls_characteristics-value_to.

        ENDCASE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
