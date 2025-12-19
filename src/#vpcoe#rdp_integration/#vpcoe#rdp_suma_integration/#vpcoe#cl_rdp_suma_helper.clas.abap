class /VPCOE/CL_RDP_SUMA_HELPER definition
  public
  final
  create private .

public section.

  data MV_SRV_SUFFIX type STRING .

  class-methods GET_INSTANCE
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
      !IV_SRV_PRFX type STRING
    returning
      value(RO_INSTANCE) type ref to /VPCOE/CL_RDP_SUMA_HELPER .
  class-methods GET_OVERRIDE_RFC_ID
    importing
      !IV_AUTH type ABAP_BOOL optional
      !IV_ENDPOINT type ABAP_BOOL optional
    returning
      value(RV_RFC_CONNECTION) type RFCDEST .
  methods CANCEL_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IV_REPLICATION_ID type /VPCOE/DE_RUN_ID optional .
  methods CLEANUP_REPLICATIONS
    importing
      !IV_EARLIER_THAN type DATS default SY-DATUM .
  methods FINISH_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional .
  methods GET_CURRENT_RUN_ID
    returning
      value(RV_REPLICATION_ID) type /VPCOE/DE_RUN_ID .
  methods GET_HTTP_CLIENT
    importing
      !IV_RFC_DEST type RFCDEST
      !IV_PATH_PREFIX type STRING
      !IV_API_TYPE type /VPCOE/DE_API_TYPE default /VPCOE/CL_COMMON_HELPER=>SC_API_TYPE-RDP
    exporting
      !ET_BAPIRET2 type BAPIRET2_T
    returning
      value(RO_HTTP_CLIENT) type ref to /VPCOE/CL_HTTP_COMMUNICATION .
  methods START_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    returning
      value(RV_RUN_ID) type /VPCOE/DE_RUN_ID .
protected section.
private section.

  class-data ST_SUMA_HELPER type ref to /VPCOE/CL_RDP_SUMA_HELPER .
  data MO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER .
  data MV_REPLICATION_ID type /VPCOE/DE_RUN_ID .

  methods CLOSE_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IV_CANCEL type XFELD optional
      !IV_FINISH type XFELD optional
      !IV_SRV_PRFX type STRING .
  methods CONSTRUCTOR
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER .
  methods GET_SUMA_OBJECT
    exporting
      !ET_BAPIRET2 type BAPIRET2_T
    returning
      value(RV_SUMA_OBJECT) type /VPCOE/DE_OBJECT_ID .
  methods STORE_REPLICATION_ID
    importing
      !IV_REPLICATION_ID type /VPCOE/DE_RUN_ID
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID .
ENDCLASS.



CLASS /VPCOE/CL_RDP_SUMA_HELPER IMPLEMENTATION.


METHOD cancel_replication.

  IF mv_replication_id IS INITIAL AND iv_replication_id IS NOT INITIAL.
    mv_replication_id = iv_replication_id.
  ENDIF.

  me->close_replication(
    EXPORTING
      io_log      = io_log
      iv_cancel   = abap_true
      iv_srv_prfx = me->mv_srv_suffix ).

ENDMETHOD.


METHOD cleanup_replications.

  DELETE FROM /vpcoe/rdp_runid WHERE requested_on < iv_earlier_than.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

ENDMETHOD.


METHOD close_replication.
  DATA: lr_response TYPE REF TO data,
        lt_bapiret2 TYPE bapiret2_t.

  IF me->mv_replication_id IS INITIAL.
    "error TO-DO
    RETURN.
  ENDIF.

  DATA(lv_path_prefix) = COND string( WHEN iv_cancel = abap_true THEN '/api/' && iv_srv_prfx && '/v1/cancelReplication'
                                      WHEN iv_finish = abap_true THEN '/api/' && iv_srv_prfx && '/v1/finishReplication'
                                                                 ELSE '' ).
  IF lv_path_prefix IS INITIAL.
    "error TO-DO
    RETURN.
  ENDIF.

  DATA(lo_http_comm) = me->get_http_client( EXPORTING
                                              iv_rfc_dest    = CONV rfcdest( mo_rdp_helper->get_generic_rfc_name( ) )
                                              iv_path_prefix = lv_path_prefix
                                            IMPORTING
                                               et_bapiret2    = lt_bapiret2 ).

  IF io_log IS BOUND.
    io_log->add_bapiret( EXPORTING it_bapiret2_t = lt_bapiret2 ).
    IF io_log->check( ).
      IF sy-batch = space.
        io_log->display_message( ).
        io_log->save( iv_no_total = abap_true ).
      ELSE.
        io_log->save( ).
      ENDIF.
      RETURN.
    ENDIF.
  ENDIF.

  lo_http_comm->send(
    EXPORTING
      iv_body          = '{"replicationRunId": "' && me->mv_replication_id && '" }'
      iv_method        = 'POST'
    IMPORTING
      ev_status        = DATA(lv_status)
      ev_reason        = DATA(lv_reason)
      es_response      = DATA(lv_response)
      ev_orig_response = DATA(lv_orig_response) ).

  lo_http_comm->close( ).

  /vpcoe/cl_rdp_helper=>deserialize_json(
    EXPORTING
      iv_json = lv_orig_response
    CHANGING
      cs_data = lr_response ).

  ASSIGN lr_response->* TO FIELD-SYMBOL(<ls_response>).

  IF lv_status = /vpcoe/cl_http_communication=>gc_s_http_status-success.
    "Ok
    "Message TO-DO
    DELETE FROM /vpcoe/rdp_runid WHERE replication_id = me->mv_replication_id.

  ELSE.
    "Error handling
    IF io_log IS BOUND.
      ASSIGN COMPONENT 'CODE' OF STRUCTURE <ls_response> TO FIELD-SYMBOL(<lv_err_code>).
      ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <ls_response> TO FIELD-SYMBOL(<lv_err_text>).
      IF <lv_err_code> IS ASSIGNED AND <lv_err_text> IS ASSIGNED.
        io_log->add_http_response(
          EXPORTING
            is_response  = VALUE #( message = <lv_err_code> && ':' && <lv_err_text> )
            iv_status    = lv_status
            iv_reason    = lv_reason ).
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD constructor.

  me->mo_rdp_helper = io_rdp_helper.

ENDMETHOD.


METHOD finish_replication.

  me->close_replication(
    EXPORTING
      io_log      = io_log
      iv_finish   = abap_true
      iv_srv_prfx = me->mv_srv_suffix ).

ENDMETHOD.


METHOD get_current_run_id.

  rv_replication_id = me->mv_replication_id.

ENDMETHOD.


  METHOD get_http_client.
    DATA: lv_server        TYPE rfcdisplay-rfchost,
          lv_proxy_host    TYPE rfcdisplay-rfcgwhost,
          lv_proxy_service TYPE rfcdisplay-rfcgwserv,
          lt_bapiret2	     TYPE bapiret2_t.

    CLEAR: et_bapiret2.

    IF /vpcoe/cl_rdp_suma_helper=>get_override_rfc_id( EXPORTING iv_auth = abap_true ) IS NOT INITIAL
      OR /vpcoe/cl_rdp_suma_helper=>get_override_rfc_id( EXPORTING iv_endpoint = abap_true ) IS NOT INITIAL.

      MESSAGE i500(/vpcoe/common) INTO /vpcoe/cl_log=>sv_msg_text.
      /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
    ENDIF.

    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = iv_rfc_dest
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

    IF sy-subrc <> 0.
      /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
      RETURN.
    ENDIF.

    TRY.
        ro_http_client = NEW /vpcoe/cl_http_communication( iv_api_type      = iv_api_type
                                                           iv_url           = |https://{ lv_server }{ iv_path_prefix }|
                                                           iv_proxy_host    = lv_proxy_host
                                                           iv_proxy_service = lv_proxy_service ).
      CATCH cx_oa2c INTO DATA(lx_oa2c).
        MESSAGE e003(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
        MESSAGE e000(/vpcoe/common) WITH lx_oa2c->get_text( ) '' '' '' INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
    ENDTRY.

  ENDMETHOD.


METHOD get_instance.

  IF st_suma_helper IS INITIAL.
    st_suma_helper = NEW /vpcoe/cl_rdp_suma_helper( io_rdp_helper ).
    st_suma_helper->mv_srv_suffix = iv_srv_prfx.
  ENDIF.

  ro_instance = st_suma_helper.

ENDMETHOD.


METHOD get_override_rfc_id.

  DATA lv_rfc_type TYPE memoryid.

  IF iv_auth = abap_true.
    lv_rfc_type = '/VPCOE/RFC_AUTH'.
  ENDIF.

  IF iv_endpoint = abap_true.
    lv_rfc_type = '/VPCOE/RFC_API'.

  ENDIF.

  SELECT SINGLE parva
    FROM usr05
     INTO rv_rfc_connection
      WHERE bname = sy-uname
         AND parid = lv_rfc_type.

  IF sy-subrc <> 0.
    CLEAR rv_rfc_connection.
  ENDIF.

ENDMETHOD.


METHOD get_suma_object.

  CLEAR et_bapiret2.

  DATA(lv_api_type) = me->mo_rdp_helper->get_api_type( ).
  DATA(lv_srv_grp) = me->mo_rdp_helper->get_srv_grp( ).

  SELECT SINGLE *
    INTO @DATA(ls_suma_object)
      FROM /vpcoe/rdp_smsrv
        WHERE api_type    = @lv_api_type
          AND service_grp = @lv_srv_grp.

  IF sy-subrc = 0.
    rv_suma_object = ls_suma_object-obj_id.
  ELSE.
    MESSAGE e097(/vpcoe/common) WITH lv_srv_grp INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD start_replication.
  TYPES: BEGIN OF lty_s_payload,
           object           TYPE string,
           source_system_id TYPE string,
           target_solution  TYPE char6,
         END OF lty_s_payload.

  DATA: lr_response TYPE REF TO data,
        lt_bapiret2 TYPE bapiret2_t,
        ls_payload  TYPE lty_s_payload.

  IF me->mv_replication_id IS NOT INITIAL.
    rv_run_id =  me->mv_replication_id.
    RETURN.
  ENDIF.

  DATA(lo_http_comm) = me->get_http_client( EXPORTING
                                              iv_rfc_dest    = CONV rfcdest( mo_rdp_helper->get_generic_rfc_name( ) )
                                              iv_path_prefix = '/api/' && me->mv_srv_suffix && '/v1/startReplication'
                                            IMPORTING
                                               et_bapiret2    = lt_bapiret2 ).

  io_log->add_bapiret( EXPORTING it_bapiret2_t = lt_bapiret2 ).
  IF io_log->check( ).
    IF sy-batch = space.
      io_log->display_message( ).
      io_log->save( iv_no_total = abap_true ).
    ELSE.
      io_log->save( ).
    ENDIF.
    RETURN.
  ENDIF.

  ls_payload-object           = me->get_suma_object( ).
  ls_payload-source_system_id = mo_rdp_helper->get_source_id( ).
  ls_payload-target_solution  = mo_rdp_helper->get_api_type( ).

  DATA(lv_payload) = /vpcoe/cl_rdp_helper=>serialize_json(
                                                EXPORTING
                                                  is_data        = ls_payload
                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

  lo_http_comm->send(
    EXPORTING
      iv_body          = lv_payload
      iv_method        = 'POST'
    IMPORTING
      ev_status        = DATA(lv_status)
      ev_reason        = DATA(lv_reason)
      es_response      = DATA(ls_response)
      ev_orig_response = DATA(lv_orig_response) ).

  lo_http_comm->close( ).

  /vpcoe/cl_rdp_helper=>deserialize_json(
    EXPORTING
      iv_json = lv_orig_response
    CHANGING
      cs_data = lr_response ).

  ASSIGN lr_response->* TO FIELD-SYMBOL(<ls_response>).

  IF lv_status = /vpcoe/cl_http_communication=>gc_s_http_status-success.
    ASSIGN COMPONENT 'REPLICATIONRUNID' OF STRUCTURE <ls_response> TO FIELD-SYMBOL(<lv_run_id>).
    IF sy-subrc = 0.
      ASSIGN <lv_run_id>->* TO FIELD-SYMBOL(<lv_value>).
      me->mv_replication_id = <lv_value>.
      me->store_replication_id( iv_replication_id = me->mv_replication_id
                                iv_srv_grp        = mo_rdp_helper->get_srv_grp( )
                                iv_srv_id         = mo_rdp_helper->get_srv_id( ) ).
      rv_run_id = me->mv_replication_id.
    ENDIF.

  ELSE.
    "Error handling
    IF ls_response-message IS NOT INITIAL.
      io_log->add_http_response(
        EXPORTING
          is_response  = ls_response
          iv_status    = lv_status
          iv_reason    = lv_reason ).

    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD store_replication_id.
  DATA: ls_replication_id TYPE /vpcoe/rdp_runid.

  ls_replication_id-replication_id = iv_replication_id.
  ls_replication_id-grp_id         = iv_srv_grp.
  ls_replication_id-srv_id         = iv_srv_id.
  ls_replication_id-requested_by   = sy-uname.
  ls_replication_id-requested_on   = sy-datum.

  INSERT /vpcoe/rdp_runid FROM ls_replication_id.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

ENDMETHOD.
ENDCLASS.
