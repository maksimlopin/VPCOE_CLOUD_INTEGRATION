class /VPCOE/CL_PCKF_RDP_CUST_ROLE definition
  public
  inheriting from /VPCOE/CL_PCKF_PROC_BASE
  create public .

public section.

  methods CONSTRUCTOR .

  methods /VPCOE/IF_PCKF_ENTITY_PROC~DESERIALIZE_SELECTION_PARAMS
    redefinition .
  methods /VPCOE/IF_PCKF_ENTITY_PROC~PREPARE_RETRIEVE
    redefinition .
  methods /VPCOE/IF_PCKF_ENTITY_PROC~RETRIEVE_PACKAGE
    redefinition .
protected section.

  data MO_MATCLASS_DAC type ref to /VPCOE/IF_PCKF_MATCLASS_DAC .

  methods PREPARE_QUERY_PARAMETER
    redefinition .
private section.

  methods PREPARE_URL_SUFFIX
    importing
      !IV_PACK_SIZE type I
      !IV_ACT_PACK_SIZE type I
    exporting
      !EV_URI_SUFFIX type STRING
      !EV_QUERY_STRING type STRING
      !EV_QUERY_FINALIZED type ABAP_BOOL .
ENDCLASS.



CLASS /VPCOE/CL_PCKF_RDP_CUST_ROLE IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_entity_proc~deserialize_selection_params.
    CLEAR es_selection_params.

    DATA ls_selection_params TYPE /vpcoe/s_pckf_cstm_role_input.

    /vpcoe/cl_common_helper=>deserialize_json(
      EXPORTING
        iv_json = iv_json_str
      CHANGING
        cs_data = ls_selection_params ).

    es_selection_params = ls_selection_params.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_proc~prepare_retrieve.      "#EC CI_NOES

    DATA : lv_params_string         TYPE string,
           ls_input_params_frm_prot TYPE /vpcoe/s_pckf_cstm_role_input,
           lt_messages              TYPE /vpcoe/t_uph_msg,
           lr_protocol_delta        TYPE REF TO /vpcoe/pckf_prot,
           lr_protocol_selection    TYPE REF TO /vpcoe/pckf_prot.

    CLEAR: mv_prepared.

    " When the Delta load then get the last successful full load selection screen parameters from Protocol
    IF mv_upload_mode EQ /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta .

      "retrieve last protocol entries
      DATA(lo_protocol_access) = /vpcoe/cl_pckf_factory=>get_instance( )->get_protocol_access( iv_upload_entity = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role ).

      DATA(lt_protocol) = lo_protocol_access->read_from_protocol(
        iv_entity_type = mv_entity_type
        iv_only_success  = abap_true
        ).

      SORT lt_protocol BY start_timestamp DESCENDING.

      "get last successful protocol entry, might be a full or delta load: relevant for delta timestamp
      READ TABLE lt_protocol INDEX 1 REFERENCE INTO lr_protocol_delta.

      "get last successful full load protocol: relevant for selection
      READ TABLE lt_protocol REFERENCE INTO lr_protocol_selection WITH KEY upload_mode = /vpcoe/if_pckf_entity_proc~gc_upload_mode-gc_upload_mode_full.

      IF lr_protocol_selection IS INITIAL.

        MESSAGE s053(/vpcoe/common).
        APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid
                                         msgno = sy-msgno
                                         msgv1 = sy-msgv1
                                         msgv2 = sy-msgv2 ) TO lt_messages.
        mo_logger->add_messages( it_messages = lt_messages ).
        RETURN.

      ELSE.

        "deserialize parameter
        lv_params_string = lr_protocol_selection->selection.
        /vpcoe/if_pckf_entity_proc~deserialize_selection_params( EXPORTING iv_json_str = lv_params_string IMPORTING es_selection_params = ls_input_params_frm_prot ).

        "create a modifiable parameter reference
        DATA lr_parameters TYPE REF TO /VPCOE/S_PCKF_CSTM_ROLE_INPUT.
        CREATE DATA lr_parameters.
        MOVE-CORRESPONDING ls_input_params_frm_prot TO lr_parameters->*.
        mr_parameters = lr_parameters.

      ENDIF.

    ENDIF.

    "return record count and set as prepared
    rv_record_cnt = -1.

    mv_prepared = abap_true.

  ENDMETHOD.                      "#EC CI_NOES            "#EC CI_CYCLO


  METHOD /vpcoe/if_pckf_entity_proc~retrieve_package.
    "Request data package
    DATA:
      lt_messages   TYPE /vpcoe/t_uph_msg,
      lv_message    TYPE string,
      ls_payload    TYPE /vpcoe/s_jsn_cloud,
      lo_payload    TYPE REF TO /vpcoe/cl_rdp_payload_handler,
      lv_uri_suffix TYPE string,
      lv_query_str  TYPE string,
      rfc_des       TYPE rfcdest,
      url           TYPE string.

    "get a wrapper instance according upload entity
    DATA(lo_transfer_mapper) = /vpcoe/cl_pckf_factory=>get_instance(  )->get_entity_mapper( mv_entity_type ).

    prepare_url_suffix(
    EXPORTING
      iv_act_pack_size = iv_act_package
      iv_pack_size     = iv_package_size
      IMPORTING
        ev_uri_suffix   = lv_uri_suffix
        ev_query_string = lv_query_str
    ).

    IF lv_query_str IS NOT INITIAL.
      lv_uri_suffix = |{ lv_uri_suffix }{ lv_query_str }|.
    ENDIF.

    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_param_roles-rfc_des IMPORTING ev_value = rfc_des ).

    TRY.
        mo_rdp_http = NEW /vpcoe/cl_pckgfee_http( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-plm
                                                  iv_rfc_name = CONV #( rfc_des )
                                                  io_logger   = me->mo_logger
                                                  iv_url      = lv_uri_suffix
                                                   ).

      CATCH cx_oa2c INTO DATA(lx_oa2c).

        MESSAGE e011(/vpcoe/pckf) WITH lx_oa2c->get_text( ) INTO lv_message.
        mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

        RETURN.
    ENDTRY.

    mo_rdp_http->send(
    EXPORTING
      iv_body     = ``
      iv_method   = 'GET'
    IMPORTING
      ev_status   = DATA(lv_code)
      ev_reason   = DATA(lv_reason)
      es_response = DATA(ls_response_txt)
      ev_orig_response = DATA(ls_orig_response)
    ).

    mo_rdp_http->close( ).

    IF lv_code <> /vpcoe/if_plm_constants=>gc_ok_response.

      MESSAGE e020(/vpcoe/pckf) WITH lv_code lv_reason INTO lv_message.
      APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) TO lt_messages.

      mo_logger->add_messages( it_messages = lt_messages ).
      mo_logger->add_http_response( EXPORTING is_response = ls_response_txt iv_reason = lv_reason ).

    ELSE.

      "Parse payload to entity data
      lo_transfer_mapper->parse_payload(
        EXPORTING
          iv_payload = ls_orig_response
        IMPORTING
          et_entity_data = rt_entity_data ).

    ENDIF.

    "log result
    DATA(lv_entity_cnt) = lines( rt_entity_data ).
    DATA(lv_entity_dval) = VALUE dd07v(  ).
    DATA(lv_domvalue) = VALUE domvalue_l(  ).
    lv_domvalue = mv_entity_type.

    IF lv_entity_cnt = 0 AND iv_act_package > 1.
      RETURN.
    ENDIF.
    " Get the upload entity text
    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        domname  = '/VPCOE/PCKF_ENTITY'
        value    = lv_domvalue
      IMPORTING
        dd07v_wa = lv_entity_dval.

    MESSAGE s021(/vpcoe/pckf) WITH lv_entity_cnt lv_entity_dval-ddtext INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 ) ) ).

  ENDMETHOD.


  METHOD CONSTRUCTOR.
    super->constructor( ).

    mo_matclass_dac = /vpcoe/cl_pckf_factory=>get_instance(  )->get_matclass_access( ).

  ENDMETHOD.


  METHOD prepare_query_parameter.

  ENDMETHOD.


  METHOD prepare_url_suffix.
    DATA: lv_criteria      TYPE string,
          lt_context       TYPE /vpcoe/t_r_context,
          lv_proc_drctn    TYPE /vpcoe/pckf_proc_drctn,
          lv_customer_role TYPE string,
          lv_pack_size     TYPE i,
          lv_message       TYPE string.

    CLEAR: ev_query_string, ev_uri_suffix, ev_query_finalized.

    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_param_roles-cust_role_url IMPORTING ev_value = ev_uri_suffix ).
    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_param_roles-customer_role IMPORTING ev_value = lv_customer_role ).

    IF ev_uri_suffix IS INITIAL.
      MESSAGE e023(/vpcoe/pckf) INTO lv_message.
      mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).
      RETURN.
    ENDIF.

    lv_pack_size = ( iv_act_pack_size - 1 ) * iv_pack_size.
    ev_query_string = |?$filter=role eq |.

    IF lv_customer_role IS INITIAL.
      ev_query_string = |{ ev_query_string }' '|.
    ELSE.
      ev_query_string = |{ ev_query_string }'{ lv_customer_role }'|.
    ENDIF.

    ev_query_string = |{ ev_query_string }&$skip={ lv_pack_size }&$top={ iv_pack_size }|.

  ENDMETHOD.
ENDCLASS.
