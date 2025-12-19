FUNCTION /vpcoe/store_json_bckgrnd.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_API_TYPE) TYPE  /VPCOE/DE_API_TYPE
*"     REFERENCE(IV_SRV_GRP) TYPE  /VPCOE/DE_SERVICE_GROUP
*"     REFERENCE(IV_SRV_ID) TYPE  /VPCOE/DE_SERVICE_ID
*"     REFERENCE(IV_SESSION_ID) TYPE  /VPCOE/JSN_CLOUD-SESSION_ID
*"     REFERENCE(IV_SESSION_ITEM) TYPE  /VPCOE/JSN_CLOUD-SESSION_ITEM
*"     REFERENCE(IS_JSON) TYPE  /VPCOE/CL_RDP_HTTP=>GTY_S_JSON
*"     REFERENCE(IV_RUNID) TYPE  /VPCOE/DE_RUN_ID OPTIONAL
*"----------------------------------------------------------------------
  DATA: lo_badi    TYPE REF TO /vpcoe/adjust_data_retrieval,
        lt_json    TYPE /vpcoe/cl_rdp_http=>gty_t_json, "/vpcoe/cl_rdp_http_communic
        ls_db_json TYPE /vpcoe/jsn_cloud.

  APPEND INITIAL LINE TO lt_json ASSIGNING FIELD-SYMBOL(<ls_json>).
  <ls_json> = is_json.

  GET BADI lo_badi.
  CALL BADI lo_badi->adjust_json
    EXPORTING
      iv_api_type = iv_api_type
      iv_srv_grp  = iv_srv_grp
      iv_srv_id   = iv_srv_id
    CHANGING
      ct_json     = lt_json.

  ls_db_json = VALUE /vpcoe/jsn_cloud( api_type      = iv_api_type
                                       service_group = iv_srv_grp
                                       service_id    = iv_srv_id
                                       session_id    = iv_session_id
                                       session_item  = iv_session_item
                                       json          = <ls_json>-elements
                                       lines_count   = <ls_json>-count
                                       created       = sy-datum
                                       created_time  = sy-uzeit
                                       run_id        = iv_runid ).

  MODIFY /vpcoe/jsn_cloud FROM @ls_db_json.

  CALL FUNCTION 'DB_COMMIT'.

ENDFUNCTION.
