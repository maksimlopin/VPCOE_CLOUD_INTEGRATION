class /VPCOE/CL_UPH_TRANSFER_MAPPER definition
  public
  abstract
  create public .

public section.

  interfaces /VPCOE/IF_UPH_TRANSFER_MAPPER .

  types:
    BEGIN OF ltys_entity_error_details,
        code    TYPE string,
        message TYPE string,
        target  TYPE string,
      END OF ltys_entity_error_details .
  types:
    lty_entity_error_details TYPE STANDARD TABLE OF ltys_entity_error_details WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ltys_entity_pack_elem_error,
        code    TYPE string,
        message TYPE string,
        details TYPE lty_entity_error_details,
      END OF ltys_entity_pack_elem_error .
  types:
    BEGIN OF ltys_errorlog_api,
        error TYPE ltys_entity_pack_elem_error,
      END OF ltys_errorlog_api .
protected section.

  methods GET_ELEMENT_ID_BY_INDEX
  abstract
    importing
      !IV_INDEX type I
    returning
      value(RV_RESULT) type CHAR40 .
  methods CREATE_DEPRECATED_ATTR_MSG
    importing
      !IV_ATTRIBUTE_NAME type STRING
      !IV_ENTITY_TYPE type STRING
    changing
      !CT_MESSAGES type /VPCOE/T_UPH_MSG .
  methods CHECK_DEPRECATED_ATTRIBUTE
    importing
      !IV_ATTRIBUTE_VALUE type DATA
      !IV_ATTRIBUTE_NAME type STRING
      !IV_ENTITY_TYPE type STRING
    changing
      !CV_IS_ATTRIBUTE_SET type ABAP_BOOL
      !CT_MESSAGES type /VPCOE/T_UPH_MSG .
  methods CONVERT_DEC_TO_CHAR
    importing
      !IV_VALUE type ANY
    returning
      value(RV_CHAR_VALUE) type STRING .
private section.

  methods ENHANCE_TARGET
    importing
      !IR_PCKG_DETAIL type ref to /VPCOE/CL_UPH_TRANSFER_MAPPER=>LTYS_ENTITY_ERROR_DETAILS
    returning
      value(RV_TARGET_STR) type STRING .
ENDCLASS.



CLASS /VPCOE/CL_UPH_TRANSFER_MAPPER IMPLEMENTATION.


  method /VPCOE/IF_UPH_TRANSFER_MAPPER~CHECK_DEPRECATED_ATTRIBUTES.
  endmethod.


  METHOD /vpcoe/if_uph_transfer_mapper~evaluate_response.
    CLEAR et_messages.

    DATA ls_errorlog_api TYPE ltys_errorlog_api.

    DATA: lt_messages       TYPE /vpcoe/t_uph_msg,
          ls_message        TYPE /vpcoe/s_uph_msg,
          lt_message_detail TYPE /vpcoe/t_uph_msg_detail,
          ls_message_detail TYPE /vpcoe/s_uph_msg_detail,
          lv_message        TYPE string,
          lv_target_str     TYPE string.

    " deserialize json string json into abap structure
    DATA(lv_response) = iv_response.

    IF lv_response IS NOT INITIAL.

      /vpcoe/cl_plm_helper=>deserialize_json( EXPORTING iv_json        = lv_response
                                                        iv_pretty_name = /vpcoe/cl_plm_helper=>sc_pretty_mode-camel_case
                                              CHANGING  cs_data        = ls_errorlog_api ).
    ENDIF.

    IF ls_errorlog_api-error IS NOT INITIAL.

      DATA(ls_pckg_error) = ls_errorlog_api-error.

      lv_message =  |{ ls_pckg_error-code } { ls_pckg_error-message }|.
      cl_message_helper=>set_msg_vars_for_clike( lv_message ).

      MESSAGE e024(/vpcoe/plm) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.
      ls_message-msgty = sy-msgty.
      ls_message-msgid = sy-msgid.
      ls_message-msgno = sy-msgno.
      ls_message-msgv1 = sy-msgv1.
      ls_message-msgv2 = sy-msgv2.
      ls_message-msgv3 = sy-msgv3.
      ls_message-msgv4 = sy-msgv4.
      CLEAR ls_message-details.

      IF ls_pckg_error-details IS NOT INITIAL.

        CLEAR lt_message_detail.

        LOOP AT ls_pckg_error-details REFERENCE INTO DATA(lr_pckg_detail).

          " Parse target information
          CLEAR lv_target_str.

          IF lr_pckg_detail->target IS NOT INITIAL.
*            lv_target_str = enhance_target( lr_pckg_detail ).
          ENDIF.

          lv_message =  |{ lr_pckg_detail->code } { lr_pckg_detail->message } { lv_target_str }|.
          cl_message_helper=>set_msg_vars_for_clike( lv_message ).

          MESSAGE e025(/vpcoe/plm) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.
          ls_message_detail-msgty = sy-msgty.
          ls_message_detail-msgid = sy-msgid.
          ls_message_detail-msgno = sy-msgno.
          ls_message_detail-msgv1 = sy-msgv1.
          ls_message_detail-msgv2 = sy-msgv2.
          ls_message_detail-msgv3 = sy-msgv3.
          ls_message_detail-msgv4 = sy-msgv4.

          APPEND ls_message_detail TO lt_message_detail.
        ENDLOOP.

        ls_message-details = lt_message_detail.

      ENDIF.

      APPEND ls_message TO lt_messages.
    ENDIF.

    et_messages = lt_messages.
  ENDMETHOD.


METHOD /vpcoe/if_uph_transfer_mapper~get_entity_url_suffix.
  RETURN.
ENDMETHOD.


  method /VPCOE/IF_UPH_TRANSFER_MAPPER~PREPARE_PAYLOAD.
  endmethod.


  METHOD CHECK_DEPRECATED_ATTRIBUTE.

    IF iv_attribute_value IS NOT INITIAL AND cv_is_attribute_set <> abap_true.
      create_deprecated_attr_msg( EXPORTING iv_attribute_name = iv_attribute_name
                                            iv_entity_type    = iv_entity_type
                                  CHANGING ct_messages = ct_messages ).
      cv_is_attribute_set = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD convert_dec_to_char.

    DATA(lo_abap_typedescr) = cl_abap_typedescr=>describe_by_data( iv_value ).
    CASE lo_abap_typedescr->type_kind.
      WHEN lo_abap_typedescr->typekind_packed.
        IF iv_value IS INITIAL.
          rv_char_value = 'null'.
        ELSE.
          rv_char_value = iv_value.
        ENDIF.
      WHEN OTHERS.
        rv_char_value = iv_value.
    ENDCASE.

  ENDMETHOD.


  METHOD create_deprecated_attr_msg.
    DATA lv_msg TYPE /vpcoe/s_uph_msg.

    MESSAGE e044(/vpcoe/plm) WITH iv_attribute_name iv_entity_type INTO DATA(lv_message).

    lv_msg-msgty = 'W'.
    lv_msg-msgid = '/VPCOE/PLM'.
    lv_msg-msgno = '044'.
    lv_msg-msgv1 = sy-msgv1.
    lv_msg-msgv2 = sy-msgv2.

    INSERT lv_msg INTO TABLE ct_messages.
  ENDMETHOD.


  METHOD ENHANCE_TARGET.
*    DATA lv_string_val TYPE string.
*    DATA lv_index      TYPE i.
*
*    " if target is already named in the message, skip
*    IF ir_pckg_detail->message CS ir_pckg_detail->target. "#EC PREFER_IS_NOT
*      RETURN.
*    ENDIF.
*
*    lv_string_val = ir_pckg_detail->target.
*    rv_target_str = ir_pckg_detail->target.
*
*    FIND FIRST OCCURRENCE OF PCRE '\[\d+\]' IN lv_string_val RESULTS DATA(lv_match_res).
*    IF sy-subrc = 0.
*
*      lv_index = substring( val = lv_string_val
*                            off = lv_match_res-offset + 1
*                            len = lv_match_res-length - 2 ).
*
*      DATA(lv_element_id) = get_element_id_by_index( lv_index + 1 ).
*
*      IF lv_element_id IS NOT INITIAL.
*        rv_target_str = |{ lv_element_id } { ir_pckg_detail->target }|.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
