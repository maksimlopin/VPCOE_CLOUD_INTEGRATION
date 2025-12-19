class /VPCOE/CL_PCKF_PROC_BASE definition
  public
  abstract
  create public .

public section.

  interfaces /VPCOE/IF_PCKF_ENTITY_PROC .

  data MO_LOGGER type ref to /VPCOE/IF_PCKF_LOGGER .
  class-data MV_VERSION type I .

  methods CONSTRUCTOR .
  class-methods DETERMINE_VERSION
    importing
      !IV_RFC_DEST type RFCDEST optional
    returning
      value(RV_VERSION) type I .
protected section.

  data MV_ENTITY_TYPE type /VPCOE/PCKF_ENTITY .
  data MV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE .
  data MO_RDP_HTTP type ref to /VPCOE/CL_PCKGFEE_HTTP .
  data MR_PARAMETERS type ref to DATA .
  data MV_INITIALIZED type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data MV_PREPARED type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.

  methods PREPARE_QUERY_PARAMETER
    exporting
      !EV_URI_SUFFIX type STRING
      !EV_QUERY_STRING type STRING
      !EV_QUERY_FINALIZED type ABAP_BOOL .
private section.

  constants LC_REPL_API_V1_PATTERN type STRING value '/api/packaging-report/v1/' ##NO_TEXT.
ENDCLASS.



CLASS /VPCOE/CL_PCKF_PROC_BASE IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_entity_proc~deserialize_selection_params.
    "to be redefined
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_proc~get_entity.
    rv_entity = mv_entity_type.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_proc~get_mode.
    rv_mode = mv_upload_mode.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_proc~get_parameters.
    rv_result = mr_parameters.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_proc~get_parameter_value.

    CLEAR ev_value.

    IF mr_parameters IS BOUND.

      ASSIGN mr_parameters->(iv_name) TO FIELD-SYMBOL(<fs_param>).
      IF sy-subrc = 0.
        ev_value = <fs_param>.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_proc~init_processor.

    mv_entity_type = iv_entity_type.
    mv_upload_mode = iv_upload_mode.

    IF is_parameters IS NOT INITIAL.

      "create a modifiable parameter reference
      DATA(lo_descr_ref) = cl_abap_typedescr=>describe_by_data( is_parameters ).

      CASE lo_descr_ref->absolute_name.
        WHEN '\TYPE=/VPCOE/S_PCKF_RETRIEVAL_INPUT'.
          DATA lr_parameter_ret TYPE REF TO /vpcoe/s_pckf_retrieval_input.
          CREATE DATA lr_parameter_ret.
          MOVE-CORRESPONDING is_parameters TO lr_parameter_ret->*.
          mr_parameters = lr_parameter_ret.

        WHEN '\TYPE=/VPCOE/S_PCKF_POSTING_INPUT'.
          DATA lr_parameter_pos TYPE REF TO /vpcoe/s_pckf_posting_input.
          CREATE DATA lr_parameter_pos.
          MOVE-CORRESPONDING is_parameters TO lr_parameter_pos->*.
          mr_parameters = lr_parameter_pos.

        WHEN '\TYPE=/VPCOE/S_PCKF_CSTM_ROLE_INPUT'.
          DATA lr_parameter_cus TYPE REF TO /vpcoe/s_pckf_cstm_role_input.
          CREATE DATA lr_parameter_cus.
          MOVE-CORRESPONDING is_parameters TO lr_parameter_cus->*.
          mr_parameters = lr_parameter_cus.

        WHEN '\TYPE=/VPCOE/S_PCKF_CSTM_EXEMP_INPUT'.
          DATA lr_parameter_exem TYPE REF TO /vpcoe/s_pckf_cstm_exemp_input.
          CREATE DATA lr_parameter_exem.
          MOVE-CORRESPONDING is_parameters TO lr_parameter_exem->*.
          mr_parameters = lr_parameter_exem.

        WHEN '\TYPE=/VPCOE/S_PCKF_COMP_CODE_INPUT'.
          DATA lr_parameter_code TYPE REF TO /vpcoe/s_pckf_comp_code_input.
          CREATE DATA lr_parameter_code.
          MOVE-CORRESPONDING is_parameters TO lr_parameter_code->*.
          mr_parameters = lr_parameter_code.

        WHEN OTHERS.
          RETURN.
      ENDCASE.

    ENDIF.

    IF iv_entity_type <> /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role AND
       iv_entity_type <> /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_exemp AND
       iv_entity_type <> /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_comp_code.
      "establish http util
      TRY.
          DATA: lv_message TYPE string,
                rfc_des    TYPE rfcdest,
                url        TYPE string.

          /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-rfc_des IMPORTING ev_value = rfc_des ).

          IF rfc_des IS NOT INITIAL.
            IF mo_rdp_http IS NOT BOUND.

              mo_rdp_http = NEW /vpcoe/cl_pckgfee_http( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-plm
                                                        iv_rfc_name = CONV #( rfc_des )
                                                        io_logger   = me->mo_logger ).

            ENDIF.

          ENDIF.
        CATCH cx_oa2c INTO DATA(lx_oa2c).

          MESSAGE e011(/vpcoe/pckf) WITH lx_oa2c->get_text( ) INTO lv_message.
          mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

          RETURN.
      ENDTRY.

      mv_initialized = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_proc~prepare_retrieve.
  ENDMETHOD.


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
      lv_load_id    TYPE /vpcoe/load_id.

    CHECK mv_initialized = abap_true.

    "check if preparation did already take place
    IF mv_prepared = abap_false.
      /vpcoe/if_pckf_entity_proc~prepare_retrieve( ).
    ENDIF.

    "get a wrapper instance according upload entity
    DATA(lo_transfer_mapper) = /vpcoe/cl_pckf_factory=>get_instance(  )->get_entity_mapper( mv_entity_type ).

    prepare_query_parameter(
      IMPORTING
        ev_uri_suffix   = lv_uri_suffix
        ev_query_string = lv_query_str
    ).

    IF lv_query_str IS NOT INITIAL.
      lv_uri_suffix = |{ lv_uri_suffix }?{ lv_query_str }|.
    ENDIF.

    "execute post with payload using util
    IF mo_rdp_http IS BOUND.

      mo_rdp_http->send(
        EXPORTING
          iv_body     = ``
          iv_method   = 'GET'
          iv_uri_suffix = lv_uri_suffix
          it_header_fields = VALUE #( ( name = 'Prefer' value = |maxpagesize={ iv_package_size }| ) )
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
            et_entity_data = rt_entity_data
            ev_delta_link  = DATA(lv_delta_link)
            ev_next_link   = DATA(lv_next_link)
        ).

        "Write back delte/nextLink to parameters for protocol
        /vpcoe/if_pckf_entity_proc~set_parameter_value( iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-next_link iv_value = lv_next_link ).
        /vpcoe/if_pckf_entity_proc~set_parameter_value( iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-delta_link iv_value = lv_delta_link ).

      ENDIF.
    ENDIF.

    "log result
    DATA(lv_entity_cnt) = lines( rt_entity_data ).
    DATA(lv_entity_dval) = VALUE dd07v(  ).
    DATA(lv_domvalue) = VALUE domvalue_l(  ).
    lv_domvalue = mv_entity_type.

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


  METHOD /vpcoe/if_pckf_entity_proc~set_parameter_value.

    IF mr_parameters IS BOUND.

      ASSIGN mr_parameters->(iv_name) TO FIELD-SYMBOL(<fs_param>).
      IF sy-subrc = 0.
        <fs_param> = iv_value.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_proc~transfer_package.
    "Must be implemented by custom implementation
  ENDMETHOD.


  METHOD constructor.

    mo_logger = /vpcoe/cl_pckf_factory=>get_instance(  )->get_logger( iv_repid = sy-repid ).

  ENDMETHOD.


  METHOD determine_version.
    DATA: lv_path_prefix TYPE string,
          lo_badi        TYPE REF TO /vpcoe/badi_pckf_custom,
          lv_skip        TYPE abap_bool.

    /vpcoe/cl_pckf_factory=>get_instance( )->determine_version( EXPORTING iv_rfc_dest = iv_rfc_dest
                                                                CHANGING  cv_version  = rv_version
                                                                          cv_skip     = lv_skip ).

    IF lv_skip = abap_false.
      SELECT SINGLE * FROM rfcdes INTO @DATA(ls_sm59) WHERE rfcdest = @iv_rfc_dest.
      IF sy-subrc <> 0.
        CLEAR ls_sm59.
      ENDIF.

      lv_path_prefix = ls_sm59-rfcoptions.

      rv_version = 2.

      lv_path_prefix = to_lower( lv_path_prefix ).

      IF lv_path_prefix CS lc_repl_api_v1_pattern.
        rv_version = 1.
      ENDIF.

    ENDIF.

    mv_version = rv_version.

  ENDMETHOD.


  METHOD prepare_query_parameter.

    DATA: lv_next_link     TYPE string,
          lv_delta_link    TYPE string,
          lv_cust_depend   TYPE abap_bool,
          lv_depend_scnr   TYPE string,
          lt_rep_config_id TYPE /vpcoe/t_r_report_config_id,
          lt_rep_categ_id  TYPE /vpcoe/t_r_report_categ_id,
          lv_criteria      TYPE string,
          lv_country       TYPE land1,
          lv_load_id       TYPE /vpcoe/load_id.

    CLEAR: ev_query_string, ev_uri_suffix, ev_query_finalized.

    "Prepare query string for initial load
    DATA(lo_transfer_mapper) = /vpcoe/cl_pckf_factory=>get_instance(  )->get_entity_mapper( mv_entity_type ).

    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-next_link   IMPORTING ev_value = lv_next_link ).
    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-delta_link  IMPORTING ev_value = lv_delta_link ).
*  In case of Delta link,fetch the delta token from /VPCOE/PCGFVAR table
*    IF me->mv_upload_mode = 'D'.
*      /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_param_exemption-load_id
*                                                                IMPORTING ev_value = lv_load_id ).
*
*
*      SELECT SINGLE delta_token
*         INTO lv_delta_link
*         FROM /vpcoe/pckfvar
*        WHERE load_id = lv_load_id.
*    ELSE.
*      /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-delta_link  IMPORTING ev_value = lv_delta_link ).
*    ENDIF.
    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-cust_depend IMPORTING ev_value = lv_cust_depend ).

    ev_uri_suffix = lo_transfer_mapper->get_entity_url_suffix( ).

    DATA(lv_pattern) = |/{ ev_uri_suffix }?|.

    IF mv_upload_mode EQ /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta.

      REPLACE FIRST OCCURRENCE OF lv_pattern IN lv_delta_link WITH '' IGNORING CASE.
      ev_query_string = lv_delta_link.
      ev_query_finalized = abap_true.

      RETURN.

    ELSE.
      IF lv_next_link IS NOT INITIAL.

        REPLACE FIRST OCCURRENCE OF lv_pattern IN lv_next_link WITH '' IGNORING CASE.
        ev_query_string = lv_next_link.
        ev_query_finalized = abap_true.

        RETURN.

      ENDIF.
    ENDIF.

    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-report_config_id IMPORTING ev_value = lt_rep_config_id ).
    IF lt_rep_config_id IS NOT INITIAL.

      IF ev_query_string IS NOT INITIAL.
        ev_query_string = |{ ev_query_string }&|.
      ENDIF.
      lv_criteria = lt_rep_config_id[ 1 ]-low.
      ev_query_string = |{ ev_query_string }ReportConfiguration={ cl_http_client=>escape_url( lv_criteria ) }|.
    ENDIF.

    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-report_categ_id IMPORTING ev_value = lt_rep_categ_id ).
    IF lt_rep_categ_id IS NOT INITIAL.

      IF ev_query_string IS NOT INITIAL.
        ev_query_string = |{ ev_query_string }&|.
      ENDIF.
      lv_criteria = lt_rep_categ_id[ 1 ]-low.
      ev_query_string = |{ ev_query_string }ReportCategory={ cl_http_client=>escape_url( lv_criteria ) }|.
    ENDIF.

    "Prepare query string with entity specific parameters
    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-country IMPORTING ev_value = lv_country ).
    IF lv_country IS NOT INITIAL.

      IF ev_query_string IS NOT INITIAL.
        ev_query_string = |{ ev_query_string }&|.
      ENDIF.
      lv_criteria = lv_country.
      ev_query_string = |{ ev_query_string }ReportCategoryCountry={ cl_http_client=>escape_url( lv_criteria ) }|.
    ENDIF.

    IF ev_query_string IS NOT INITIAL.
      ev_query_string = |{ ev_query_string }&|.
    ENDIF.
    lv_criteria = lv_country.

    IF lv_cust_depend = abap_true.
      lv_depend_scnr = 'true'.
    ELSE.
      lv_depend_scnr = 'false'.
    ENDIF.

    ev_query_string = |{ ev_query_string }includeCustomerRoleDependentFees={ lv_depend_scnr }|.
*    ev_query_string = |{ ev_query_string }includeCustomerRoleDependentFees=true|.

  ENDMETHOD.
ENDCLASS.
