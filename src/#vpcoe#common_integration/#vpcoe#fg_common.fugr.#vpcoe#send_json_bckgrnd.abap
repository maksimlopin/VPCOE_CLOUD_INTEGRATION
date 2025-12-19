FUNCTION /vpcoe/send_json_bckgrnd.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FULL_URL) TYPE  STRING OPTIONAL
*"     VALUE(IV_RFC_NAME) TYPE  RFCDOC_D OPTIONAL
*"     VALUE(IV_JSON) TYPE  STRING OPTIONAL
*"     VALUE(IV_API_TYPE) TYPE  /VPCOE/JSN_CLOUD-API_TYPE OPTIONAL
*"     VALUE(IV_SESSION_ID) TYPE  /VPCOE/JSN_CLOUD-SESSION_ID OPTIONAL
*"     VALUE(IV_SESSION_ITEM) TYPE  /VPCOE/JSN_CLOUD-SESSION_ITEM
*"       OPTIONAL
*"     VALUE(IV_SUB_OBJECT) TYPE  /VPCOE/BALOBJ OPTIONAL
*"     VALUE(IV_COUNT) TYPE  INT4 OPTIONAL
*"     VALUE(IV_PROXY_HOST) TYPE  RFCDISPLAY-RFCGWHOST OPTIONAL
*"     VALUE(IV_PROXY_SERVICE) TYPE  RFCDISPLAY-RFCGWSERV OPTIONAL
*"     VALUE(IV_HTTP_VERSION) TYPE  RFCDISPLAY-RFCTYPE OPTIONAL
*"  EXPORTING
*"     VALUE(EV_FAILED) TYPE  XFELD
*"     VALUE(EV_REASON) TYPE  STRING
*"     VALUE(EV_STATUS) TYPE  I
*"     VALUE(ET_LOG_SUM) TYPE  /VPCOE/TT_LOG_SUM
*"  TABLES
*"      ET_MESSAGES TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  DATA: lo_badi     TYPE REF TO /vpcoe/adjust_data_retrieval,
        lt_json     TYPE /vpcoe/cl_http_communication=>gty_t_json,
        lt_mapping  TYPE /vpcoe/cl_common_helper=>gty_t_name_mappings,
        lt_messages TYPE bapiret2_t.

  CLEAR: ev_failed, ev_status, ev_reason, et_log_sum.

  DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = iv_sub_object ).
  IF iv_json IS INITIAL.
    SELECT SINGLE *
      INTO @DATA(ls_db_json)
        FROM /vpcoe/jsn_cloud
         WHERE api_type     = @iv_api_type
           AND session_id   = @iv_session_id
           AND session_item = @iv_session_item.
  ELSE.
    ls_db_json-json = iv_json.
    ls_db_json-lines_count = iv_count.
  ENDIF.

  IF sy-subrc <> 0.
    ev_failed = abap_true.
    MESSAGE e002(/vpcoe/common) WITH iv_api_type iv_session_id iv_session_item INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    lo_log->add_sy_msg( ).
    lo_log->get_messages( IMPORTING et_messages = et_messages[] ).
    RETURN.
  ENDIF.

  TRY.
      IF iv_full_url IS SUPPLIED.
        DATA(lo_rdp_http) = NEW /vpcoe/cl_http_communication( iv_api_type      = iv_api_type
                                                              iv_url           = iv_full_url
                                                              iv_proxy_host    = iv_proxy_host
                                                              iv_proxy_service = iv_proxy_service ).
      ELSEIF iv_rfc_name IS SUPPLIED.
        lo_rdp_http = NEW /vpcoe/cl_http_communication( iv_api_type  = iv_api_type
                                                         iv_rfc_name = iv_rfc_name ).
      ENDIF.
    CATCH cx_oa2c.
      MESSAGE e003(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      lo_log->add_sy_msg( ).
      lo_log->get_messages( IMPORTING et_messages = et_messages[] ).
      lo_log->save( ).
      ev_failed = abap_true.
      RETURN.
  ENDTRY.

  lo_rdp_http->send(
    EXPORTING
      iv_body         = ls_db_json-json
      iv_http_version = iv_http_version
    IMPORTING
      es_response = DATA(ls_response)
      ev_status   = ev_status
      ev_reason   = ev_reason
      ev_orig_response = DATA(lv_orig_response) ).

  IF ls_response-details IS INITIAL AND lv_orig_response IS NOT INITIAL.
    INSERT VALUE #( message = CONV #( lv_orig_response ) ) INTO TABLE ls_response-details.
  ENDIF.

  lo_log->add_http_response( EXPORTING is_response = ls_response
                                       iv_status   = ev_status
                                       iv_reason   = ev_reason
                                       iv_count    = ls_db_json-lines_count ).

  lo_log->get_messages( IMPORTING et_messages = et_messages[] ).

  ev_failed = COND #( WHEN ev_status = /vpcoe/cl_http_communication=>gc_s_http_status-ok THEN abap_false
                                                                                         ELSE abap_true ).

  et_log_sum = lo_log->mt_sum.

  lo_rdp_http->close( ).

ENDFUNCTION.
