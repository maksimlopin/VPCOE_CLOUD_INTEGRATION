class /VPCOE/CL_UPH_HTTP_UTIL definition
  public
  final
  create public .

public section.

  interfaces /VPCOE/IF_UPH_HTTP_UTIL .

  methods CONSTRUCTOR .
  PROTECTED SECTION.

    DATA mo_logger TYPE REF TO /vpcoe/if_uph_logger .

private section.

  constants MC_CONTENT_TYPE_JSON type STRING value 'application/json' ##NO_TEXT.
  constants MC_HEADER_REQUEST_METHOD type STRING value '~request_method' ##NO_TEXT.
  constants MC_HTTP_REQUEST_URI type STRING value '~request_uri' ##NO_TEXT.
  constants MC_STATUS_CODE_S type I value '204' ##NO_TEXT ##STR_NUM.
  constants MC_REQUEST_METHOD_PUT type STRING value 'PUT' ##NO_TEXT.
  constants MC_USER_AGENT type STRING value 'User-Agent' ##NO_TEXT.
  data MO_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  data MO_OAUTH_CLIENT type ref to IF_OAUTH2_CLIENT .
  data MV_ERROR_FLG type ABAP_BOOL .
  data MV_FLAG_HTTP_CLIENT type ABAP_BOOL .

  methods GET_OAUTH2_CLIENT
    importing
      !IV_RFCDEST type RFCDEST
    returning
      value(RO_OAUTH2_CLIENT) type ref to IF_OAUTH2_CLIENT .
  methods SET_OAUTH2_TOKEN
    importing
      !IV_RFC_DES type RFCDEST .
  methods CREATE_BY_URL
    importing
      !IV_DESTINATION type RFCDEST
      !IV_URI_SUFFIX type STRING
    returning
      value(RO_CLIENT) type ref to IF_HTTP_CLIENT .
  methods CREATE_BY_DESTINATION
    importing
      !IV_DESTINATION type RFCDEST
    returning
      value(RO_CLIENT) type ref to IF_HTTP_CLIENT .
  methods SET_BODY_DATA_SEND_AND_RECEIVE
    importing
      !IV_ENTITY_JSON_DATA type STRING
    exporting
      !EV_ERROR_FLG type ABAP_BOOL
      !EV_ERROR_MSG type STRING .
  methods RESPONSE_FROM_HTTP
    exporting
      !EV_ERROR_FLG type ABAP_BOOL
      !EV_RESPONSE_TXT type STRING
      !EV_REASON type STRING
      !EV_CODE type STRING .
  methods GET_USER_AGENT
    returning
      value(RV_RESULT) type STRING .
ENDCLASS.



CLASS /VPCOE/CL_UPH_HTTP_UTIL IMPLEMENTATION.


  METHOD /vpcoe/if_uph_http_util~close_connection.
*    * Close connection.
    IF mo_http_client IS BOUND .

      mo_http_client->close(
        EXCEPTIONS
          http_invalid_state = 1
          OTHERS             = 2
      ).

      CLEAR mo_http_client.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_http_util~get_http_client.

*    mo_http_client = me->create_by_destination( iv_destination = iv_rfc_des ).

    mo_http_client = me->create_by_url( iv_destination = iv_rfc_des
                                        iv_uri_suffix  = iv_uri_suffix ).

    "Set header data
    "set POST as request_method method.
    mo_http_client->request->set_header_field( name  = mc_header_request_method
                                               value =  iv_request_method ).

*    "set API suffix value as /PackagingElements/replicate
*    mo_http_client->request->set_header_field( name  = mc_http_request_uri
*                                               value = iv_uri_suffix ).

    "content type  application/json
    mo_http_client->request->set_content_type( content_type = mc_content_type_json ).

    mo_http_client->set_compression( if_http_client=>co_compress_in_all_cases ).

    mo_http_client->request->set_header_field( name  = mc_user_agent
                                               value = get_user_agent( ) ).

    IF mv_error_flg = abap_true.
      RETURN.
    ELSE.
      "get a valid Oauth token,and set it to http clinet
      IF mo_oauth_client IS NOT BOUND.
        DATA(lv_rfcdest) = iv_rfc_des.
        set_oauth2_token( iv_rfc_des = lv_rfcdest  ).
      ENDIF.

      ro_http_client = mo_http_client.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_http_util~post_data_to_api.

    CLEAR:ev_error_flg, ev_response_txt, ev_reason, ev_code.

** set body data and post data to RDP API

    IF mo_http_client IS BOUND.
* payload data to http client and
* send and receive the http request
      set_body_data_send_and_receive( EXPORTING
                                     iv_entity_json_data = iv_entity_data
                                     IMPORTING
                                      ev_error_flg = DATA(lv_error_flg)
                                      ev_error_msg = DATA(lv_error_msg) ).
* response from RDP API
      response_from_http( IMPORTING
                              ev_code = DATA(lv_resp_status_code)
                              ev_response_txt = DATA(lv_resp_txt)
                              ev_reason = DATA(lv_reason)
                              ev_error_flg = lv_error_flg ).

* set the error flag and send the response text to logger
      IF lv_error_flg = abap_true .
        ev_code = lv_resp_status_code.
        ev_response_txt = lv_resp_txt.
        ev_reason = lv_reason.
        ev_error_flg = lv_error_flg .
        IF lv_error_msg IS NOT INITIAL AND lv_reason IS INITIAL.
          ev_reason = lv_error_msg .
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


METHOD constructor.
* get the logger
  mo_logger = /vpcoe/cl_uph_factory=>get_instance(  )->get_logger( iv_repid = sy-repid ).

ENDMETHOD.


  METHOD create_by_destination.
    DATA :
     lt_messages          TYPE /vpcoe/t_uph_msg.

    " get http client for rdp api
    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = iv_destination
      IMPORTING
        client                   = ro_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6 ).

    IF sy-subrc <> 0 .
      MESSAGE e021(/vpcoe/plm) WITH iv_destination INTO DATA(lv_message).
      CLEAR lt_messages.
      APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = iv_destination ) TO lt_messages.
      mo_logger->add_messages( it_messages = lt_messages ).

      mv_error_flg = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD create_by_url.
    DATA: lt_messages      TYPE /vpcoe/t_uph_msg,
          lv_server        TYPE rfcdisplay-rfchost,
          lv_proxy_host    TYPE rfcdisplay-rfcgwhost,
          lv_proxy_service TYPE rfcdisplay-rfcgwserv.

    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = iv_destination
        authority_check         = abap_false
        bypass_buf              = abap_false
      IMPORTING
        server                  = lv_server
        proxy_host              = lv_proxy_host
        proxy_service           = lv_proxy_service
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = |https://{ lv_server }{ iv_uri_suffix }|
        proxy_host         = CONV string( lv_proxy_host )
        proxy_service      = CONV string( lv_proxy_service )
      IMPORTING
        client             = ro_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).

    IF sy-subrc <> 0 .
*      MESSAGE e021(/vpcoe/plm) WITH iv_destination INTO DATA(lv_message).
*      CLEAR lt_messages.
*      APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = iv_destination ) TO lt_messages.
*      mo_logger->add_messages( it_messages = lt_messages ).
*
*      mv_error_flg = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD get_oauth2_client.

    DATA: lt_messages      TYPE /vpcoe/t_uph_msg,
          lv_oauth_profile TYPE oa2c_profile.

    IF mo_oauth_client IS NOT BOUND.

* get OAuth2 profile name from RFC destniation config..
      CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
        EXPORTING
          destination             = iv_rfcdest
          authority_check         = abap_false
        IMPORTING
          oauth_profile           = lv_oauth_profile
        EXCEPTIONS
          authority_not_available = 1
          destination_not_exist   = 2
          information_failure     = 3
          internal_failure        = 4
          no_http_destination     = 5
          OTHERS                  = 6.

      IF sy-subrc IS INITIAL.
*create Oauth2 client
        TRY.
            mo_oauth_client = cl_oauth2_client=>create( lv_oauth_profile ).
            " Execute the Client Credentials Flow:
            " If we have no valid Access token, the oauth2 client sends a POST request
            " to the XSUAA server with the client id and client secret to get a valid
            " access token.
*            mo_oauth_client->execute_cc_flow( ).
          CATCH cx_oa2c
                cx_oa2c_kernel_too_old INTO DATA(lx_oauth_error).
*  get exception error
            DATA(lv_message2) = lx_oauth_error->get_text( ).

            MESSAGE e022(/vpcoe/plm) WITH lv_oauth_profile INTO DATA(lv_message).
            APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = lv_oauth_profile
                                                                   msgv2 = lv_message2  ) TO lt_messages.
* log the errors
            mo_logger->add_messages( it_messages = lt_messages ).
        ENDTRY.
      ELSE.
        MESSAGE e023(/vpcoe/plm) WITH iv_rfcdest INTO lv_message.
        APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = iv_rfcdest ) TO lt_messages.
        mo_logger->add_messages( it_messages = lt_messages ).
      ENDIF.
    ENDIF.

    ro_oauth2_client = mo_oauth_client.
  ENDMETHOD.


  METHOD get_user_agent.
    DATA: lv_component       TYPE string,
          lv_release_version TYPE saprelease,
          lv_ext_release     TYPE string,
          lt_comptab         TYPE STANDARD TABLE OF cvers_sdu.

    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
      EXPORTING
        iv_language      = sy-langu
      TABLES
        tt_comptab       = lt_comptab
      EXCEPTIONS
        no_release_found = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE lt_comptab WITH KEY component = 'SAPSCORE' REFERENCE INTO DATA(lr_result).
    IF sy-subrc <> 0.
      READ TABLE lt_comptab WITH KEY component = 'S4CORE' REFERENCE INTO lr_result.
    ENDIF.

    IF lr_result IS NOT INITIAL.
      lv_component       = lr_result->component.
      lv_release_version = lr_result->release.
      lv_ext_release     = lr_result->extrelease.

      SHIFT lv_ext_release LEFT DELETING LEADING '0'.

      IF lv_ext_release IS NOT INITIAL.
        lv_ext_release = '-' && lv_ext_release.
      ENDIF.

      CONCATENATE 'SAP NetWeaver Application Server (1.0;' sy-saprl ') SAP-SUS/' lv_component '' lv_release_version '' lv_ext_release ' (SIC)' INTO rv_result ##NO_TEXT .
    ENDIF.
  ENDMETHOD.


  METHOD response_from_http.

    CLEAR: ev_code,ev_error_flg, ev_response_txt, ev_reason.
*get response from RDP API
    DATA(lv_response_body) = mo_http_client->response->get_cdata( ).
*get status code
    mo_http_client->response->get_status(
      IMPORTING
        code = DATA(lv_http_rc)
        reason = DATA(lv_reason_txt) ).

    IF lv_http_rc <> mc_status_code_s.
      ev_error_flg = abap_true.
      ev_reason = lv_reason_txt.
      ev_code   = lv_http_rc.
      ev_response_txt = lv_response_body.

    ENDIF.

  ENDMETHOD.


  METHOD set_body_data_send_and_receive.

    CLEAR: ev_error_flg, ev_error_msg.

    IF mo_http_client IS BOUND.

* Set request body data
      mo_http_client->request->set_cdata(
                  data = iv_entity_json_data ).

*send the request
      mo_http_client->send(
       EXCEPTIONS
         http_communication_failure = 1                  " Communication Error
         http_invalid_state         = 2                  " Invalid state
         http_processing_failed     = 3                  " Error when processing method
         http_invalid_timeout       = 4                  " Invalid Time Entry
         OTHERS                     = 5
      ).

      IF sy-subrc <> 0.
        ev_error_flg = abap_true.
        ev_error_msg = text-002.
        RETURN.
      ENDIF.

      mo_http_client->receive(
        EXCEPTIONS
         http_communication_failure = 1                " Communication Error
         http_invalid_state         = 2                " Invalid state
         http_processing_failed     = 3                " Error when processing method
         OTHERS                     = 4
      ).
      IF sy-subrc <> 0.
        ev_error_flg = abap_true.
        ev_error_msg = text-002.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_oauth2_token.

    TYPES : BEGIN OF ty_token,
              access_token TYPE string,
            END OF ty_token.

    DATA: lo_http_client TYPE REF TO if_http_client.

    DATA: ls_token       TYPE ty_token.

    DATA: lv_response TYPE string,
          lv_status   TYPE i,
          lv_token    TYPE string,
          lv_reason   TYPE string.

*
    " Override predefined RFC Connection Name
    DATA(lv_rfc) = /vpcoe/cl_rdp_suma_helper=>get_override_rfc_id( EXPORTING iv_auth = abap_true ).
    IF lv_rfc IS INITIAL.
      lv_rfc = 'VPCOE_RDP_OAUTH'.
    ENDIF.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = lv_rfc
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6 ).
    IF sy-subrc <> 0.
      mo_logger->add_messages( VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) ) ).
      RETURN.
    ENDIF.

    IF lo_http_client IS NOT INITIAL.
      lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

      lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
      lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).
      cl_http_utility=>set_request_uri( request = lo_http_client->request
                                        uri     = '?grant_type=client_credentials' ).

      lo_http_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5 ).
      IF sy-subrc <> 0.
        mo_logger->add_messages( VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) ) ).
        RETURN.
      ENDIF.

      lo_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
      IF sy-subrc <> 0.
        mo_logger->add_messages( VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) ) ).
        RETURN.
      ENDIF.

      "  Check the response. Hopefully you get back a JSON response.
      lv_response = lo_http_client->response->get_cdata( ).

      lo_http_client->response->get_status( IMPORTING code   = lv_status
                                                      reason = lv_reason ).

      IF lv_reason = if_http_status=>reason_200.
        cl_fdt_json=>json_to_data( EXPORTING iv_json = lv_response
                                   CHANGING  ca_data = ls_token ).

        IF ls_token-access_token IS NOT INITIAL.
          "set oAuth access token in header
          CONCATENATE 'Bearer' ls_token-access_token INTO lv_token SEPARATED BY space.
          mo_http_client->request->set_header_field( name  = 'Authorization'
                                                     value = lv_token ).
        ENDIF.
      ELSE.
        MESSAGE e000(/vpcoe/common) INTO /vpcoe/cl_log=>sv_msg_text WITH lv_status ':' lv_reason.
        mo_logger->add_messages( VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) ) ).
      ENDIF.
    ENDIF.

*    DATA: lt_messages TYPE /vpcoe/t_uph_msg.
*
*    TRY.
*        " Set OAuth token
*        get_oauth2_client( iv_rfcdest = iv_rfc_des )->set_token( io_http_client = mo_http_client ).
*
*      CATCH cx_sy_ref_is_initial
*             cx_oa2c INTO DATA(lx_oauth_error).
** read the exception error's short text
*        DATA(lv_ex_error) = lx_oauth_error->get_text( ).
**add the error messages to logger.
*        MESSAGE e022(/vpcoe/plm) WITH lv_ex_error INTO DATA(lv_message).
*        APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = lv_ex_error ) TO lt_messages.
*        mo_logger->add_messages( it_messages = lt_messages ).
*    ENDTRY.

  ENDMETHOD.
ENDCLASS.
