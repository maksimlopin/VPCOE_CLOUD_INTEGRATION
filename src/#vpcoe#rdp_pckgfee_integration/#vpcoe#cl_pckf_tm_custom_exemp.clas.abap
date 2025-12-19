CLASS /vpcoe/cl_pckf_tm_custom_exemp DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /vpcoe/if_pckf_transfer_mapper .

    TYPES:
      BEGIN OF ltys_entity_error_details,
        code    TYPE string,
        message TYPE string,
        target  TYPE string,
      END OF ltys_entity_error_details .
    TYPES:
      lty_entity_error_details TYPE STANDARD TABLE OF ltys_entity_error_details WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ltys_pckg_fee_error,
        code    TYPE string,
        message TYPE string,
        details TYPE lty_entity_error_details,
      END OF ltys_pckg_fee_error .
    TYPES:
      BEGIN OF ltys_errorlog_api,
        error TYPE ltys_pckg_fee_error,
      END OF ltys_errorlog_api .
*  types:
*    BEGIN OF ltys_pckf_api_custom_exemp,
*        customer          TYPE string,
*        report_category   TYPE string,
*        valid_from        TYPE string,
*        valid_to          TYPE string,
*        report_fraction   TYPE string,
*        exemption_percent TYPE string,
*      END OF ltys_pckf_api_custom_exemp .
    TYPES:
      ltyt_pckf_api_custom_exemp TYPE STANDARD TABLE OF /vpcoe/s_pckf_custom_exemp WITH NON-UNIQUE KEY customer .
    TYPES:
      BEGIN OF ltys_pckg_fee_custom_exemp,
        value TYPE ltyt_pckf_api_custom_exemp,
      END OF ltys_pckg_fee_custom_exemp .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS mc_pckg_fee_suffix TYPE string VALUE 'PackagingFees' ##NO_TEXT.

    METHODS remove_prefix_context
      IMPORTING
        !iv_attribute       TYPE data
        !iv_prefix          TYPE string
      RETURNING
        VALUE(rv_attribute) TYPE string .
ENDCLASS.



CLASS /VPCOE/CL_PCKF_TM_CUSTOM_EXEMP IMPLEMENTATION.


  METHOD /VPCOE/IF_PCKF_TRANSFER_MAPPER~EVALUATE_RESPONSE. "#EC CI_CYCLO "#EC CI_NOES

    CLEAR et_messages.

    DATA ls_errorlog_api TYPE ltys_errorlog_api.

    DATA: lt_messages       TYPE /vpcoe/t_uph_msg,
          ls_message        TYPE /vpcoe/s_uph_msg,
          lt_message_detail TYPE /vpcoe/t_uph_msg_detail,
          ls_message_detail TYPE /vpcoe/s_uph_msg_detail,
          lv_message        TYPE string,
          lv_string_val     TYPE string,
          lv_target_str     TYPE string,
          lv_index          TYPE i.

*deserialize json string json into abap structure
    DATA(lv_response) = iv_response .

    IF lv_response IS NOT INITIAL.

      /vpcoe/cl_common_helper=>deserialize_json( EXPORTING iv_json = lv_response
                                                           iv_pretty_name = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                                 CHANGING cs_data = ls_errorlog_api ).
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

          lv_message =  |{ lr_pckg_detail->code } { lr_pckg_detail->message }|.
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

  ENDMETHOD. "#EC CI_CYCLO       "#EC CI_NOES                    "#EC CI_NESTING


  METHOD /VPCOE/IF_PCKF_TRANSFER_MAPPER~GET_ENTITY_URL_SUFFIX.
*    rv_entity_url_suffix = mc_pckg_fee_suffix.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_transfer_mapper~parse_payload.

    DATA: ls_pckg_fee_api_payload TYPE ltys_pckg_fee_custom_exemp,
          lo_cust_exemp           TYPE REF TO /vpcoe/cl_pckf_ent_cstm_exemp,
          lt_cust_exemp           TYPE /vpcoe/t_custom_exemp_data.

    CLEAR et_entity_data.

    IF iv_payload IS NOT INITIAL.

      /vpcoe/cl_common_helper=>deserialize_json( EXPORTING iv_json          = iv_payload
                                                           iv_pretty_name   = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                                 CHANGING  cs_data          = ls_pckg_fee_api_payload ).

      LOOP AT ls_pckg_fee_api_payload-value ASSIGNING FIELD-SYMBOL(<ls_pckg_fee_api_cstm_exemp>).

        lo_cust_exemp = NEW /vpcoe/cl_pckf_ent_cstm_exemp( is_data = VALUE #( customer          = <ls_pckg_fee_api_cstm_exemp>-customer
                                                                              exemption_percent = <ls_pckg_fee_api_cstm_exemp>-exemption_percent
                                                                              report_category   = <ls_pckg_fee_api_cstm_exemp>-report_category
                                                                              report_fraction   = <ls_pckg_fee_api_cstm_exemp>-report_fraction
                                                                              valid_from        = <ls_pckg_fee_api_cstm_exemp>-valid_from
                                                                              valid_to          = <ls_pckg_fee_api_cstm_exemp>-valid_to ) ).
        APPEND lo_cust_exemp TO et_entity_data.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_transfer_mapper~prepare_payload.

    DATA: lr_entity       TYPE REF TO /vpcoe/cl_pckf_ent_cstm_exemp.

    DATA: ls_entity_data_api      TYPE /vpcoe/s_pckf_custom_exemp,
          ls_pckg_fee_api         TYPE /vpcoe/s_pckf_custom_exemp,
          ls_pckg_fee_api_payload TYPE ltys_pckg_fee_custom_exemp.

    CLEAR: rv_payload.

    LOOP AT it_entity_data INTO DATA(lr_entity_data).
      lr_entity ?= lr_entity_data.

      "do the mapping
      DATA(ls_pckg_fee_entity) = lr_entity->get_data( ).

      CLEAR ls_pckg_fee_api.

      ls_pckg_fee_api = VALUE #( customer = ls_pckg_fee_entity-customer
                                 exemption_percent = ls_pckg_fee_entity-exemption_percent
                                 report_category = ls_pckg_fee_entity-report_category
                                 report_fraction = ls_pckg_fee_entity-report_fraction
                                 valid_from = ls_pckg_fee_entity-valid_from
                                 valid_to = ls_pckg_fee_entity-valid_to ).

      APPEND ls_pckg_fee_api TO ls_pckg_fee_api_payload-value.

    ENDLOOP.

    rv_payload = /vpcoe/cl_common_helper=>serialize_json( is_data          = ls_pckg_fee_api_payload
                                                          iv_compress      = abap_true
                                                          iv_pretty_name   = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                                          iv_ts_as_iso8601 = abap_true ).

  ENDMETHOD.                                               "#EC CI_NOES


  METHOD remove_prefix_context.

    DATA lv_replace_len TYPE i.

    rv_attribute = iv_attribute.
    IF iv_prefix IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_replace) = |{ iv_prefix }:|.
    lv_replace_len = strlen( lv_replace ).

    IF strlen( iv_attribute ) < lv_replace_len.
      RETURN.
    ENDIF.

    "ensure only occurence at position 0 is replaced
    IF substring( val = iv_attribute off = 0 len = lv_replace_len ) = lv_replace.
      rv_attribute = replace( val = iv_attribute sub = lv_replace with = '' occ = 1 ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
