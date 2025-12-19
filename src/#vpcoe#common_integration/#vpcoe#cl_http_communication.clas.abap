class /VPCOE/CL_HTTP_COMMUNICATION definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_json,
        elements TYPE string,
        count    TYPE i,
      END OF gty_s_json .
  types:
    gty_t_json TYPE STANDARD TABLE OF gty_s_json WITH EMPTY KEY .
  types:
    BEGIN OF gty_s_response_details,
        code     TYPE text255,
        message  TYPE text255,
        severity TYPE text255,
        target   TYPE text255,
      END OF gty_s_response_details .
  types:
    gty_t_response_details TYPE STANDARD TABLE OF gty_s_response_details WITH EMPTY KEY .
  types:
    BEGIN OF gty_s_response,
        code     TYPE text255,
        message  TYPE text255,
        severity TYPE text255,
        target   TYPE text255,
        details  TYPE gty_t_response_details,
      END OF gty_s_response .

  constants:
    BEGIN OF gc_s_http_status,
        success TYPE i VALUE '200',
        created TYPE i VALUE '201',
        ok      TYPE i VALUE '204',
      END OF gc_s_http_status .
  data MV_RFC_OAUTH type RFCDOC_D .

  methods CLOSE .
  methods CONSTRUCTOR
    importing
      !IV_API_TYPE type /VPCOE/DE_API_TYPE optional
      !IV_RFC_NAME type RFCDOC_D optional
      !IV_URL type STRING optional
      !IV_PROXY_HOST type RFCDISPLAY-RFCGWHOST optional
      !IV_PROXY_SERVICE type RFCDISPLAY-RFCGWSERV optional
      !IV_USER_AGENT type STRING optional
    raising
      CX_OA2C .
  methods GET_USER_AGENT
    returning
      value(RV_RESULT) type STRING .
*      !IV_RUN_ID type /VPCOE/DE_RUN_ID optional
  methods SEND
    importing
      !IV_BODY type STRING
      !IV_METHOD type STRING default 'PUT'
      !IV_URI_SUFFIX type STRING optional
      !IT_HEADER_FIELDS type TIHTTPNVP optional
      !IV_HTTP_VERSION type RFCDISPLAY-RFCTYPE optional
    exporting
      !EV_STATUS type I
      !EV_REASON type STRING
      !ES_RESPONSE type /VPCOE/CL_HTTP_COMMUNICATION=>GTY_S_RESPONSE
      !EV_ORIG_RESPONSE type STRING .
protected section.

  methods GET_ACCESS_TOKEN
    importing
      !IV_RFC_OAUTH type RFCDOC_D
      !IV_REQ_PARAM type STRING
    exceptions
      EX_RFC_CONNECTION
      EX_DATA_SEND
      EX_DATA_REC .
  methods GET_GZIP_CLASS
    exporting
      !EO_CLASS type ref to OBJECT .
  methods PARSE_RESPONSE
    importing
      !IV_RESPONSE type STRING
    returning
      value(RS_RESPONSE) type /VPCOE/CL_HTTP_COMMUNICATION=>GTY_S_RESPONSE .
  methods SET_HEADER
    importing
      !IV_METHOD type STRING default IF_HTTP_REQUEST=>CO_REQUEST_METHOD_POST
      !IV_HTTP_VERSION type RFCDISPLAY-RFCTYPE optional
    exporting
      !ET_BAPIRET2 type BAPIRET2_T
    returning
      value(RO_HTTP_CLIENT) type ref to IF_HTTP_CLIENT
    exceptions
      CLIENT_NOT_BOUND .
  PRIVATE SECTION.

    DATA mo_http_client TYPE REF TO if_http_client .
    DATA mv_token TYPE string .
    DATA mv_user_agent TYPE string .
ENDCLASS.



CLASS /VPCOE/CL_HTTP_COMMUNICATION IMPLEMENTATION.


  METHOD close.

    me->mo_http_client->close(
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    DATA: lv_req_param TYPE string.

    IF iv_url IS NOT INITIAL.

      cl_http_client=>create_by_url(
        EXPORTING
          url                = iv_url
          proxy_host         = CONV string( iv_proxy_host )
          proxy_service      = CONV string( iv_proxy_service )
        IMPORTING
          client             = me->mo_http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
    ELSE.

      cl_http_client=>create_by_destination(
        EXPORTING
          destination              = iv_rfc_name
        IMPORTING
          client                   = me->mo_http_client
        EXCEPTIONS
          argument_not_found       = 1
          destination_not_found    = 2
          destination_no_authority = 3
          plugin_not_active        = 4
          internal_error           = 5
          OTHERS                   = 6 ).

    ENDIF.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RAISE EXCEPTION TYPE cx_oa2c.
    ENDIF.

    lv_req_param = '?grant_type=client_credentials'.

    me->get_access_token(
      EXPORTING iv_rfc_oauth = 'VPCOE_RDP_OAUTH'
                iv_req_param = lv_req_param
      EXCEPTIONS
        ex_rfc_connection = 1
        ex_data_send      = 2
        ex_data_rec       = 3
        OTHERS            = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RAISE EXCEPTION TYPE cx_oa2c.
    ENDIF.

    me->mv_user_agent = me->get_user_agent( ).
  ENDMETHOD.


  METHOD get_access_token.

    TYPES : BEGIN OF ty_token,
              access_token TYPE string,
            END OF ty_token.

    DATA: lo_http_client TYPE REF TO if_http_client.

    DATA: ls_token       TYPE ty_token.

    DATA: lv_response TYPE string,
          lv_status   TYPE i,
          lv_reason   TYPE string.

*
    CLEAR: mv_token.

    " Override predefined RFC Connection Name
    DATA(lv_rfc) = /vpcoe/cl_rdp_suma_helper=>get_override_rfc_id( EXPORTING iv_auth = abap_true ).
    IF lv_rfc IS INITIAL.
      lv_rfc = iv_rfc_oauth.
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
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING ex_rfc_connection.
      RETURN.
    ENDIF.

    IF lo_http_client IS NOT INITIAL.
      lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

      lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
      lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).
      cl_http_utility=>set_request_uri( request = lo_http_client->request
                                        uri     = iv_req_param ).

      lo_http_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING ex_data_send.
        RETURN.
      ENDIF.

      lo_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4  RAISING ex_data_rec.
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
          mv_token = ls_token-access_token.
        ENDIF.
      ELSE.
        MESSAGE e000(/vpcoe/common) WITH lv_status ':' lv_reason RAISING ex_data_rec.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_gzip_class.

    CLEAR: eo_class.

    SELECT SINGLE *
      INTO @DATA(ls_sap_basis)
        FROM cvers
          WHERE component = 'SAP_BASIS'.

    IF sy-subrc = 0.
      IF   ls_sap_basis-release = 700 AND ls_sap_basis-extrelease >= 35 OR ls_sap_basis-release = 701 AND ls_sap_basis-extrelease >= 20 OR ls_sap_basis-release = 702 AND ls_sap_basis-extrelease >= 20
        OR ls_sap_basis-release = 710 AND ls_sap_basis-extrelease >= 22 OR ls_sap_basis-release = 711 AND ls_sap_basis-extrelease >= 17 OR ls_sap_basis-release = 730 AND ls_sap_basis-extrelease >= 18
        OR ls_sap_basis-release = 731 AND ls_sap_basis-extrelease >= 21 OR ls_sap_basis-release = 740 AND ls_sap_basis-extrelease >= 18 OR ls_sap_basis-release = 750 AND ls_sap_basis-extrelease >= 09
        OR ls_sap_basis-release = 751 AND ls_sap_basis-extrelease >= 04.
        eo_class = NEW cl_abap_gzip( ).
      ELSE.
        eo_class = NEW /vpcoe/cl_abap_gzip( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_user_agent.

    CONCATENATE 'SAP NetWeaver Application Server (1.0;' sy-saprl ') SAP-SUS/'
                /vpcoe/cl_common_helper=>gc_vp_version ' (VP)' INTO rv_result.

  ENDMETHOD.


  METHOD parse_response.
    DEFINE set_value.
      ASSIGN COMPONENT &1 OF STRUCTURE &2 TO <lv_value>.
      IF sy-subrc = 0.
        lr_value = <lv_value>.
        ASSIGN lr_value->* TO <lv_value>.
        &3 = <lv_value>.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lr_response_data TYPE REF TO data,
          lr_value         TYPE REF TO data.

    FIELD-SYMBOLS: <lv_value> TYPE any,
                   <lt_error> TYPE ANY TABLE.

    /vpcoe/cl_common_helper=>deserialize_json(
      EXPORTING
        iv_json    = iv_response
      CHANGING
        cs_data    = lr_response_data  ).

    ASSIGN lr_response_data->* TO FIELD-SYMBOL(<ls_response>).
    ASSIGN COMPONENT 'ERROR' OF STRUCTURE <ls_response> TO FIELD-SYMBOL(<ls_error_ref>).
    IF sy-subrc = 0.
      ASSIGN <ls_error_ref>->* TO FIELD-SYMBOL(<ls_error>).
      set_value 'CODE' <ls_error> rs_response-code.
      set_value 'MESSAGE' <ls_error> rs_response-message.

      ASSIGN COMPONENT 'DETAILS' OF STRUCTURE <ls_error> TO <lv_value>.
      IF sy-subrc = 0.
        lr_value = <lv_value>.
        ASSIGN lr_value->* TO <lt_error>.
        IF <lt_error> IS ASSIGNED.
          LOOP AT <lt_error> ASSIGNING FIELD-SYMBOL(<ls_error_line_ref>).
            ASSIGN <ls_error_line_ref>->* TO FIELD-SYMBOL(<ls_error_data>).
            APPEND INITIAL LINE TO rs_response-details ASSIGNING FIELD-SYMBOL(<ls_details>).
            set_value 'CODE' <ls_error_data> <ls_details>-code.
            set_value 'MESSAGE' <ls_error_data> <ls_details>-message.
            set_value 'TARGET' <ls_error_data> <ls_details>-target.
            set_value 'SEVERITY' <ls_error_data> <ls_details>-severity .
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD send.
    DATA: lv_json_gzip TYPE xstring,
          lv_code_page TYPE string VALUE `UTF-8`,
          lo_class     TYPE REF TO object,
          lt_errors    TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    CLEAR: es_response,
           ev_status,
           ev_reason,
           ev_orig_response.

    DATA(lo_http_client) = me->set_header( EXPORTING iv_method = iv_method iv_http_version = iv_http_version ).
    lo_http_client->request->set_content_type( content_type = if_rest_media_type=>gc_appl_json ).

    lo_http_client->request->set_header_field( name  = 'Content-Encoding' value = 'gzip' ).

    "set uri suffix (if provided)
    IF iv_uri_suffix IS NOT INITIAL.
      lo_http_client->request->set_header_field( name  = '~request_uri' value = iv_uri_suffix ).
    ENDIF.

    "set additional header fields (if provided)
    IF it_header_fields IS  SUPPLIED AND it_header_fields IS NOT INITIAL.
      LOOP AT it_header_fields INTO DATA(ls_header_field).
        lo_http_client->request->set_header_field( name = ls_header_field-name value = ls_header_field-value ).
      ENDLOOP.

    ENDIF.

    me->get_gzip_class( IMPORTING eo_class = lo_class ).

    DATA(lv_json_binary) = NEW cl_abap_codepage( )->convert_to( source   = iv_body
                                                                codepage = lv_code_page ).

    CALL METHOD lo_class->('COMPRESS_BINARY_WITH_HEADER')
      EXPORTING
        raw_in   = lv_json_binary
      IMPORTING
        gzip_out = lv_json_gzip.

    lo_http_client->request->set_data( data = lv_json_gzip ).
*    "for tests
*    DATA: lt_headers TYPE TIHTTPNVP.
*    lo_http_client->request->get_header_fields(
*      CHANGING
*        fields = lt_headers    " Header fields
*    ).
*    lo_http_client->request->set_header_fields( fields = lt_headers ).

    lo_http_client->send(
      EXPORTING
        timeout                    = 300
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    IF sy-subrc <> 0.
      lo_http_client->get_last_error(
        IMPORTING
          message        = DATA(lv_last_error) ).

      SPLIT lv_last_error AT cl_abap_char_utilities=>newline INTO TABLE lt_errors.

      LOOP AT lt_errors ASSIGNING FIELD-SYMBOL(<ls_error>).
        INSERT VALUE #( message = <ls_error> ) INTO TABLE es_response-details.
      ENDLOOP.

      RETURN.
    ENDIF.

    lo_http_client->receive(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      lo_http_client->get_last_error(
        IMPORTING
          message        = lv_last_error ).

      SPLIT lv_last_error AT cl_abap_char_utilities=>newline INTO TABLE lt_errors.

      LOOP AT lt_errors ASSIGNING <ls_error>.
        INSERT VALUE #( message = <ls_error> ) INTO TABLE es_response-details.
      ENDLOOP.
      RETURN.
    ENDIF.

    es_response = me->parse_response( lo_http_client->response->get_cdata( ) ).
    ev_orig_response = lo_http_client->response->get_cdata( ).
    lo_http_client->response->get_status( IMPORTING code   = ev_status
                                                    reason = ev_reason ).

  ENDMETHOD.


  METHOD set_header.
* Values
    DATA: lv_token TYPE string.

    CLEAR et_bapiret2.

    ro_http_client = me->mo_http_client.

    IF me->mo_http_client IS BOUND.
      ro_http_client->request->set_version( version = COND #( WHEN iv_http_version = '1' THEN if_http_request=>co_protocol_version_1_1
                                                              ELSE if_http_request=>co_protocol_version_1_0 ) ).
      me->mo_http_client->request->set_method( iv_method ).
      me->mo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).
      me->mo_http_client->propertytype_logon_popup = ro_http_client->co_disabled.
      me->mo_http_client->propertytype_accept_compress = ro_http_client->co_disabled.
      "set oAuth access token in header
      CONCATENATE 'Bearer' mv_token INTO lv_token SEPARATED BY space.
      me->mo_http_client->request->set_header_field( name  = 'Authorization'
                                                     value = lv_token ).

      IF me->mv_user_agent IS NOT INITIAL.
        me->mo_http_client->request->set_header_field( name  = 'User-Agent'
                                                       value = CONV #( me->mv_user_agent ) ).
      ENDIF.

      me->mo_http_client->request->if_http_entity~set_content_type( content_type = if_rest_media_type=>gc_multipart_form_data ).
      me->mo_http_client->request->if_http_entity~set_formfield_encoding( formfield_encoding = cl_http_request=>if_http_entity~co_encoding_raw ).
    ELSE.
      RAISE client_not_bound.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
