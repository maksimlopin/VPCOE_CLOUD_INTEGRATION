class /VPCOE/CL_UPH_PROC_BASE definition
  public
  abstract
  create public .

public section.

  interfaces /VPCOE/IF_UPH_ENTITY_PROC .

  data MV_LOAD_ID type /VPCOE/LOAD_ID .

  methods CONSTRUCTOR .
protected section.

  data MO_LOGGER type ref to /VPCOE/IF_UPH_LOGGER .
  data MO_UPH_HTTP_UTIL type ref to /VPCOE/IF_UPH_HTTP_UTIL .
  data MO_UPL_TRANSFER_MAPPER type ref to /VPCOE/IF_UPH_TRANSFER_MAPPER .
  data MV_INITIALIZED type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data MV_PREPARED type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data MV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY .
  data MV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE .
  data MV_UPLOAD_TARGET type /VPCOE/UPLOAD_TARGET .
  data MV_UPLOAD_TARGET_VERSION type I .

  methods SERIALIZE_SELECTION_PARAMS
    importing
      !IS_PARAMETERS type ANY
    returning
      value(RV_JSON_STR) type STRING .
  methods DETERMINE_FALLBACK_MAPPER
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY
      !IV_UPLOAD_TARGET type /VPCOE/UPLOAD_TARGET
      value(IV_UPLOAD_TARGET_VERSION) type I
    returning
      value(RO_TSFR_MAPPER) type ref to /VPCOE/IF_UPH_TRANSFER_MAPPER .
  methods DETECT_UPLOAD_TARGET
    importing
      !IV_RFCDEST type RFCDEST
    exporting
      !EV_UPLOAD_TARGET type /VPCOE/UPLOAD_TARGET
      !EV_UPLOAD_TARGET_VERSION type I .
private section.

  constants LC_REPL_API_V1_PATTERN type STRING value 'api/replication/v1' ##NO_TEXT.
  constants LC_REPL_API_V2_PATTERN type STRING value 'v2' ##NO_TEXT.
  constants LC_REPL_RDP_API_PATTERN type STRING value 'api/replication/' ##NO_TEXT.
ENDCLASS.



CLASS /VPCOE/CL_UPH_PROC_BASE IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_proc~delete_from_protocol.

    DELETE FROM /vpcoe/uph_prot WHERE upload_entity = mv_upload_entity.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~deserialize_selection_params.
    "to be redefined
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~fits_param_size_into_protocol.
    rv_result = abap_true.

"Commented by Streminski to avoid limitations
    " Serialize the parameters
*    DATA(selection_parms) = serialize_selection_params( is_selection_params ).
*
*    DATA(lv_length_of_parameters) = strlen( selection_parms ).
*
*    DATA lv_protocol TYPE /vpcoe/uph_prot.
*    DESCRIBE FIELD lv_protocol-selection LENGTH DATA(lv_length_of_selection_field) IN CHARACTER MODE.
*
*    IF lv_length_of_parameters > lv_length_of_selection_field.
*      MESSAGE e043(/vpcoe/plm) INTO DATA(lv_msg_str)  ##NEEDED.
*      mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).
*      rv_result = abap_false.
*    ENDIF.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~get_upload_entity.
    rv_upload_entity = mv_upload_entity.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~get_upload_mode.
    rv_upload_mode = mv_upload_mode.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~init_processor.

    mv_upload_entity = iv_upload_entity.
    mv_upload_mode = iv_upload_mode.

    mv_initialized = abap_true.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~prepare_process.
    mv_prepared = abap_true.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~process_package.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~read_from_protocol.

    DATA: lt_protocol TYPE /vpcoe/t_uph_prot.

    CLEAR rt_protocol.

    " Get the previous successful input parameters from the Protocol table
    SELECT *
     FROM /vpcoe/uph_prot
     WHERE upload_entity = @iv_upload_entity
     ORDER BY start_timestamp DESCENDING
     INTO CORRESPONDING FIELDS OF TABLE @lt_protocol.

    LOOP AT lt_protocol REFERENCE INTO DATA(lr_protocol).
      IF iv_upload_mode IS NOT INITIAL AND lr_protocol->upload_mode NE iv_upload_mode.
        CONTINUE.
      ENDIF.

      IF iv_only_success EQ abap_true AND lr_protocol->failed = abap_true.
        CONTINUE.
      ENDIF.

      APPEND lr_protocol->* TO rt_protocol.
    ENDLOOP.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~transfer_package.
    DATA: lt_messages TYPE /vpcoe/t_uph_msg,
          lv_message  TYPE string,
          ls_payload  TYPE /vpcoe/s_jsn_cloud,
          lo_payload  TYPE REF TO /vpcoe/cl_rdp_payload_handler.

    DATA: lv_rfc_des TYPE rfcdest,
          lv_json    TYPE string.

    FIELD-SYMBOLS: <lv_rfc_des>    TYPE rfcdest,
                   <lv_uri_suffix> TYPE string,
                   <lv_api_type>   TYPE /vpcoe/de_api_type.

    "detect target api
    IF mv_upload_target IS INITIAL.
      DATA(lr_parameters) = /vpcoe/if_uph_entity_proc~get_parameters( ).
      IF lr_parameters IS BOUND.
        "get the rfc destination from selection parameter of the report
        ASSIGN lr_parameters->('RFC_DES') TO <lv_rfc_des>.
        IF sy-subrc = 0.
          lv_rfc_des = <lv_rfc_des>.
        ENDIF.
      ELSE.
        DATA(ls_parameters) = VALUE /vpcoe/s_pckg_elem_input(  ).
        lr_parameters = REF #( ls_parameters ).
      ENDIF.

      detect_upload_target(
        EXPORTING
          iv_rfcdest               = lv_rfc_des
        IMPORTING
          ev_upload_target         = mv_upload_target
          ev_upload_target_version = mv_upload_target_version ).

      IF mv_upload_target IS INITIAL OR mv_upload_target_version = 0.
        MESSAGE e011(/vpcoe/plm) WITH lv_rfc_des INTO lv_message.
        APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 ) TO lt_messages.

        mo_logger->add_messages( it_messages = lt_messages ).
        RETURN.
      ENDIF.

    ENDIF.

    "get a mapper instance according upload entity
    mo_upl_transfer_mapper = /vpcoe/cl_uph_factory=>get_instance(  )->get_pckg_elem_mapper(
                                                                     iv_upload_entity = mv_upload_entity
                                                                     iv_upload_target = mv_upload_target
                                                                     iv_upload_target_version = mv_upload_target_version ).

    IF mo_upl_transfer_mapper IS NOT BOUND.

      mo_upl_transfer_mapper = determine_fallback_mapper( iv_upload_entity = mv_upload_entity
                                                          iv_upload_target = mv_upload_target
                                                          iv_upload_target_version = mv_upload_target_version ).
    ENDIF.

    DATA(lt_deprecated_attr_messages) = mo_upl_transfer_mapper->check_deprecated_attributes( it_entity_data = it_entity_data ).
    mo_logger->add_messages( it_messages = lt_deprecated_attr_messages ).

    "wrap the entity data to json payload
    FIELD-SYMBOLS: <ls_parameters> TYPE any.
    ASSIGN lr_parameters->* TO <ls_parameters>.
    lv_json = mo_upl_transfer_mapper->prepare_payload( it_entity_data    = it_entity_data
                                                       is_parameters     = <ls_parameters>
                                                       iv_replication_id = '' ).

    " get entity  API URI
    DATA(lv_uri_suffix) = mo_upl_transfer_mapper->get_entity_url_suffix( ).

    "establish http connection (get util instance, use util instance, ...)
    DATA(lo_http_client) = mo_uph_http_util->get_http_client( iv_rfc_des    = lv_rfc_des
                                                              iv_uri_suffix = lv_uri_suffix ).

    IF lo_http_client IS BOUND.

      "execute post with payload using util
      mo_uph_http_util->post_data_to_api( EXPORTING
                                            iv_entity_data = lv_json
                                          IMPORTING
                                            ev_error_flg    = DATA(lv_resp_error)
                                            ev_response_txt = DATA(lv_response_txt)
                                            ev_reason       = DATA(lv_reason)
                                            ev_code         = DATA(lv_code) ).

      IF lv_resp_error = abap_true.
        rv_error_flg = abap_true.

        MESSAGE e020(/vpcoe/plm) WITH lv_code lv_reason INTO lv_message.
        APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid
                                         msgno = sy-msgno
                                         msgv1 = sy-msgv1
                                         msgv2 = sy-msgv2 ) TO lt_messages.

        mo_upl_transfer_mapper->evaluate_response( EXPORTING iv_response = lv_response_txt IMPORTING et_messages = DATA(lt_messages_res) ).

        APPEND LINES OF lt_messages_res TO lt_messages.

        mo_logger->add_messages( it_messages = lt_messages ).

      ELSE.

        DATA(lv_entity_cnt) = lines( it_entity_data ).
        DATA(lv_upload_entity_dval) = VALUE dd07v(  ).
        DATA(lv_domvalue) = VALUE domvalue_l(  ).
        lv_domvalue = mv_upload_entity.

        " Get the upload entity text
        CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
          EXPORTING
            domname  = '/VPCOE/UPLOAD_ENTITY'
            value    = lv_domvalue
          IMPORTING
            dd07v_wa = lv_upload_entity_dval.

        MESSAGE s026(/vpcoe/plm) WITH lv_entity_cnt lv_upload_entity_dval-ddtext INTO lv_message.
        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~write_to_protocol.
    DATA lt_messages TYPE /vpcoe/t_uph_msg.

    DATA(ls_protocol) = is_protocol.

    CONCATENATE sy-datum sy-uzeit INTO DATA(lv_timestamp).

    ls_protocol-end_timestamp = lv_timestamp .
    ls_protocol-upload_mode = mv_upload_mode.
    ls_protocol-upload_entity = mv_upload_entity.

    TRY.
        ls_protocol-uuid = cl_system_uuid=>create_uuid_x16_static(   ).
      CATCH cx_uuid_error INTO DATA(lr_exp_uuid).
        DATA(lv_exp_text) = lr_exp_uuid->get_text( ).

        " Adding the exception text to application log
        APPEND INITIAL LINE TO lt_messages ASSIGNING FIELD-SYMBOL(<fs_messages>).
        IF <fs_messages> IS ASSIGNED.
          <fs_messages>-msgty = /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_w.
          <fs_messages>-msgid = /vpcoe/cl_uph_exec_load_wrap=>gc_default_msgclass.
          <fs_messages>-msgno = 000.
          <fs_messages>-msgv1 = lv_exp_text.
        ENDIF.
        mo_logger->add_messages( it_messages = lt_messages ).

    ENDTRY.

    " Serialize the parameters
    DATA(lr_parameters) = /vpcoe/if_uph_entity_proc~get_parameters( ).
    FIELD-SYMBOLS: <params> TYPE any.
    IF lr_parameters IS BOUND.
      ASSIGN lr_parameters->* TO <params>.
      ls_protocol-selection = serialize_selection_params( <params> ).
    ENDIF.

    " Insert protocol table
    INSERT /vpcoe/uph_prot FROM ls_protocol.

  ENDMETHOD.


  METHOD constructor.

    mo_logger = /vpcoe/cl_uph_factory=>get_instance(  )->get_logger( iv_repid = sy-repid ).
    mo_uph_http_util = /vpcoe/cl_uph_factory=>get_instance(  )->get_http_util( ).

  ENDMETHOD.


  METHOD detect_upload_target.
    DATA: lv_path_prefix TYPE string.

    ev_upload_target = /vpcoe/if_uph_entity_proc=>gc_upload_target-gc_upload_target_rdp.
    ev_upload_target_version = 2.

    IF iv_rfcdest IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = iv_rfcdest
        authority_check         = abap_false
      IMPORTING
        path_prefix             = lv_path_prefix
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.

    IF sy-subrc = 0.

      lv_path_prefix = to_lower( lv_path_prefix ).

      IF lv_path_prefix CS lc_repl_rdp_api_pattern.
        ev_upload_target = /vpcoe/if_uph_entity_proc~gc_upload_target-gc_upload_target_rdp.
      ELSE.
        ev_upload_target = /vpcoe/if_uph_entity_proc~gc_upload_target-gc_upload_target_sdi.

      ENDIF.

      IF lv_path_prefix CS lc_repl_api_v1_pattern.
        ev_upload_target_version = 1.
      ELSEIF lv_path_prefix CS lc_repl_api_v2_pattern.
        ev_upload_target_version = 2.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD determine_fallback_mapper.
    CASE iv_upload_target.

      WHEN /vpcoe/if_uph_entity_proc=>gc_upload_target-gc_upload_target_rdp.

        CASE iv_upload_entity.

          WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_plm OR
               /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_rcp OR
               /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom.

            IF iv_upload_target_version = 1.
              ro_tsfr_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_cmp_v1( ).
            ELSE.
              ro_tsfr_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_cmp_v2( ).
            ENDIF.

          WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm OR
                /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl.

            IF iv_upload_target_version = 1.
              ro_tsfr_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_elem_v1( ).
            ELSE.
              ro_tsfr_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_elem_v2( ).
            ENDIF.

          WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu.

            IF iv_upload_target_version = 1.
              ro_tsfr_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_cmp_itv1( ).
            ELSE.
              ro_tsfr_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_cmp_itv2( ).
            ENDIF.

          WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_gprd.
            ro_tsfr_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_cmp_gp_2( ). "/vpcoe/cl_uph_tm_pckg_cmp_gp_v2( ).

          WHEN OTHERS.

        ENDCASE.

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD serialize_selection_params.

    CLEAR rv_json_str.

    IF is_parameters IS NOT INITIAL.

      rv_json_str = /vpcoe/cl_plm_helper=>serialize_json(
         is_data           = is_parameters
         iv_compress       = abap_true
         iv_pretty_name    = 'L' ) .

    ENDIF.
  ENDMETHOD.
ENDCLASS.
