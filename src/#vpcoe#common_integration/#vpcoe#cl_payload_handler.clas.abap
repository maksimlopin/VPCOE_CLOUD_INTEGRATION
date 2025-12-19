class /VPCOE/CL_PAYLOAD_HANDLER definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_db_json,
        api_type           TYPE /vpcoe/jsn_cloud-api_type,
        session_id         TYPE /vpcoe/jsn_cloud-session_id,
        session_item       TYPE /vpcoe/jsn_cloud-session_item,
        failed             TYPE /vpcoe/jsn_cloud-failed,
        lines_count        TYPE /vpcoe/jsn_cloud-lines_count,
        json               TYPE /vpcoe/de_json,
        processing_started TYPE xfeld,
        processing_ended   TYPE xfeld,
        created	           TYPE dats,
        created_time       TYPE tims,
        attempts           TYPE i,
        do_wait            TYPE xfeld,
        task_id            TYPE text40,
        response_status    TYPE /vpcoe/jsn_cloud-response_status,
        response_message   TYPE /vpcoe/jsn_cloud-response_message,
        run_id             TYPE /vpcoe/de_run_id,
      END OF gty_s_db_json .
  types:
    gty_t_db_json TYPE STANDARD TABLE OF gty_s_db_json .
  types:
    gty_r_sesion_id TYPE RANGE OF /vpcoe/jsn_cloud-session_id .

  methods DECREASE_WP_COUNT
    importing
      !P_TASK type CLIKE .
  methods DELETE_PAYLOAD
    importing
      !IT_R_SESSION_IDS type STANDARD TABLE
      !IV_COMMIT_WORK type ABAP_BOOL default ABAP_FALSE .
  methods SEND_PAYLOAD
    importing
      !IO_CUST type ref to /VPCOE/CL_COMMON_HELPER
      !IT_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !IT_BAPIRET2 type BAPIRET2_T optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
      !IV_SAVE_LOG type XFELD default ABAP_TRUE
    exporting
      !EV_STATUS type I
      !EV_REASON type STRING .
  methods SEND_PAYLOAD_MULTI
    importing
      !IT_R_SESSION_IDS type /VPCOE/CL_RDP_PAYLOAD_HANDLER=>GTY_R_SESION_ID
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
    changing
      !CV_TOTAL type /VPCOE/STR_LOG_SUM-TOTAL
      !CV_TOTAL_FAILED type /VPCOE/STR_LOG_SUM-TOTAL_FAILED .
  methods SET_PAYLOAD
    importing
      !IV_COMMIT_WORK type ABAP_BOOL default ABAP_FALSE
      !IV_S_JSN_CLOUD type /VPCOE/S_JSN_CLOUD
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional .
  PROTECTED SECTION.
private section.

  data MT_DB_JSON type /VPCOE/CL_RDP_PAYLOAD_HANDLER=>GTY_T_DB_JSON .
  data MV_TASKS_RUNNING type INT4 .
  data MO_LOG type ref to /VPCOE/CL_RDP_LOG .
  data MT_PROCESSED_NOTIFICATIONS type GTY_R_SESION_ID .

  methods SAVE_TRACE_LOG
    importing
      !IS_JSON type /VPCOE/DE_JSON
      !IO_CUST type ref to /VPCOE/CL_COMMON_HELPER
      !IV_STATUS type CHAR10
      !IV_RESPONSE type STRING optional .
ENDCLASS.



CLASS /VPCOE/CL_PAYLOAD_HANDLER IMPLEMENTATION.


  METHOD decrease_wp_count.

    DATA: lt_messages TYPE bapiret2_t,
          lv_failed   TYPE xfeld,
          lv_status   TYPE i,
          lv_reason   TYPE string.

    RECEIVE RESULTS FROM FUNCTION '/VPCOE/SEND_JSON_BCKGRND'
      IMPORTING
        ev_failed             = lv_failed
        ev_reason             = lv_reason
        ev_status             = lv_status
      TABLES
        et_messages           = lt_messages
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.

    DATA(lv_sysubrc) = sy-subrc.

    READ TABLE me->mt_db_json ASSIGNING FIELD-SYMBOL(<ls_db_json>) WITH KEY task_id = p_task.
    IF sy-subrc = 0.

      <ls_db_json>-failed           = lv_failed.
      <ls_db_json>-response_status  = lv_status.
      <ls_db_json>-response_message = lv_reason.

      IF lv_sysubrc = 0.
        <ls_db_json>-processing_ended = abap_true.
        me->mo_log->add_messages_to_log( lt_messages ).
        me->mo_log->add_bapiret( EXPORTING it_bapiret2_t = lt_messages ).
      ELSE.
        <ls_db_json>-do_wait = abap_true.
        <ls_db_json>-attempts = <ls_db_json>-attempts + 1.
      ENDIF.

      <ls_db_json>-processing_started = abap_false.

      me->mv_tasks_running = me->mv_tasks_running - 1.
    ENDIF.

  ENDMETHOD.


  METHOD DELETE_PAYLOAD.

    DELETE FROM /vpcoe/jsn_cloud WHERE session_id IN it_r_session_ids
                                   AND failed = abap_true.

    IF sy-subrc = 0 AND iv_commit_work = abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD save_trace_log.

    TYPES: BEGIN OF lty_s_element,
             id TYPE string,
           END OF lty_s_element.

    TYPES: lty_t_elements TYPE STANDARD TABLE OF lty_s_element WITH EMPTY KEY.

    TYPES: BEGIN OF lty_s_json_root,
             elements TYPE lty_t_elements,
           END OF lty_s_json_root.

    DATA: ls_respons         TYPE lty_s_json_root,
          lt_trace           TYPE STANDARD TABLE OF /vpcoe/trace,
          lt_name_maping     TYPE /vpcoe/cl_common_helper=>gty_t_name_mappings,
          lv_json_field_name TYPE string.

    CASE io_cust->get_srv_id( ).
      WHEN /vpcoe/cl_rdp_helper=>sc_service_id-inventory.
        lv_json_field_name = 'product'.
      WHEN /vpcoe/cl_rdp_helper=>sc_service_id-billdocit.
        lv_json_field_name = 'billingDocument'.
      WHEN OTHERS.
        lv_json_field_name = 'id'.
    ENDCASE.


    lt_name_maping = VALUE #( ( abap = 'id' json = lv_json_field_name ) ).

    /vpcoe/cl_rdp_helper=>deserialize_json(
      EXPORTING
        iv_json          = is_json
        it_name_mappings = lt_name_maping
      CHANGING
        cs_data          = ls_respons ).

    lt_trace = VALUE #( FOR <ls_respons> IN ls_respons-elements
                       ( api_type         = io_cust->get_api_type( )
                         service_group    = io_cust->get_srv_grp( )
                         service_id       = io_cust->get_srv_id( )
                         status           = iv_status
                         response_message = CONV #( iv_response )
                         object_id        = <ls_respons>-id
                         last_sended_date = sy-datum
                         last_sended_time = sy-uzeit ) ).

    MODIFY /vpcoe/trace FROM TABLE @lt_trace.

  ENDMETHOD.


  METHOD send_payload.
    DATA: lt_bapiret2     TYPE bapiret2_t,
          lo_payload      TYPE REF TO /vpcoe/cl_rdp_payload_handler,
          ls_payload      TYPE /vpcoe/s_jsn_cloud,
          lv_session_item TYPE /vpcoe/s_jsn_cloud-session_item,
          lv_http_version TYPE rfcdisplay-rfctype.

    CLEAR: ev_status,
           ev_reason.

    io_log->add_bapiret( EXPORTING it_bapiret2_t = it_bapiret2 ).

    DATA(lo_http_comm) = io_cust->get_http_client( IMPORTING et_bapiret2     = lt_bapiret2
                                                             ev_http_version = lv_http_version ).
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

    DATA(lv_session_id) = io_cust->generate_session_id( ).

    LOOP AT it_json INTO DATA(ls_json).
      lv_session_item = lv_session_item + 1.
      MESSAGE i012(/vpcoe/common) WITH sy-tabix INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      io_log->add_msg_progress( EXPORTING iv_msg_text           = /vpcoe/cl_rdp_log=>sv_msg_text
                                          iv_save_log           = abap_false
                                          iv_add_message_to_log = abap_true ).

      lo_http_comm->send(
        EXPORTING
          iv_body         = ls_json-elements
          iv_http_version = lv_http_version
        IMPORTING
          ev_status   = ev_status
          ev_reason   = ev_reason
          es_response = DATA(ls_response) ).

      io_log->add_http_response(
        EXPORTING
          is_response = ls_response
          iv_status   = ev_status
          iv_reason   = ev_reason
          iv_count    = ls_json-count ).

      IF /vpcoe/cl_common_helper=>get_trace_enable( ) = abap_true.
        me->save_trace_log(
        EXPORTING
          is_json     = ls_json-elements
          io_cust     = io_cust
          iv_response =  COND #( WHEN ev_status <> lo_http_comm->gc_s_http_status-ok
                                   THEN ev_reason )
          iv_status   = COND #( WHEN ev_status <> lo_http_comm->gc_s_http_status-ok
                                  THEN /vpcoe/cl_common_helper=>sc_status-failed
                                  ELSE /vpcoe/cl_common_helper=>sc_status-success ) ).
      ENDIF.

      IF ev_status <> lo_http_comm->gc_s_http_status-ok.
        lo_payload = NEW /vpcoe/cl_rdp_payload_handler( ).
        ls_payload = VALUE #( api_type         = io_cust->get_api_type( )
                              service_group    = io_cust->get_srv_grp( )
                              service_id       = io_cust->get_srv_id( )
                              session_id       = lv_session_id
                              session_item     = lv_session_item
                              json             = ls_json-elements
                              lines_count      = ls_json-count
                              response_message = ev_reason
                              response_status  = ev_status
                              created          = sy-datum
                              created_time     = sy-uzeit
                              failed           = abap_true
                              repeated         = sy-datum ).
        lo_payload->set_payload( EXPORTING iv_commit_work = abap_true
                                           iv_s_jsn_cloud = ls_payload
                                           io_log         = io_log ).
      ENDIF.

    ENDLOOP.

    lo_http_comm->close( ).

    IF io_log->check( ).
      ASSIGN io_log->mt_sum[ sub_object = io_log->get_subobject( ) ] TO FIELD-SYMBOL(<ls_sum>).
      IF sy-subrc = 0.
        DATA(lv_count) = lines( it_bapiret2 ).
        <ls_sum>-total_failed = <ls_sum>-total_failed + lv_count.
      ENDIF.
    ENDIF.

    IF iv_save_log = abap_true.
      io_log->save( ).
    ENDIF.

  ENDMETHOD.


  METHOD send_payload_multi.
    DATA: lv_packages_left TYPE xfeld,
          lv_full_url      TYPE string,
          lv_no_session    TYPE abap_bool,
          lt_bapiret2      TYPE bapiret2_t.

    me->mo_log = io_log.

    SELECT api_type,
           session_id,
           session_item,
           lines_count,
           created,
           created_time,
           json
      INTO CORRESPONDING FIELDS OF TABLE @me->mt_db_json
        FROM /vpcoe/jsn_cloud
         WHERE session_id IN @it_r_session_ids.

    IF sy-subrc = 0.

      DATA(lv_wp_count) = /vpcoe/cl_rdp_helper=>get_count_of_processes( EXPORTING iv_api_type = iv_api_type
                                                                                  iv_srv_id   = iv_srv_id
                                                                                  iv_srv_grp  = io_cust->get_srv_grp( ) ).

      LOOP AT me->mt_db_json ASSIGNING FIELD-SYMBOL(<ls_db_json>).
        <ls_db_json>-task_id = 'Task-' && sy-tabix.
        cv_total = cv_total + <ls_db_json>-lines_count.
      ENDLOOP.

      CLEAR me->mv_tasks_running.

      DATA(lv_url) = io_cust->get_service_url( IMPORTING et_bapiret2 = lt_bapiret2 ).
      me->mo_log->add_bapiret( EXPORTING it_bapiret2_t = lt_bapiret2 ).

      io_cust->get_url_from_rfc(
        EXPORTING
          iv_rfc_name      = CONV #( io_cust->get_generic_rfc_name( ) )
        IMPORTING
          et_bapiret2      = lt_bapiret2
          ev_server        = DATA(lv_server)
          ev_proxy_host    = DATA(lv_proxy_host)
          ev_proxy_service = DATA(lv_proxy_service)
          ev_http_version  = DATA(lv_http_version) ).

      me->mo_log->add_bapiret( EXPORTING it_bapiret2_t = lt_bapiret2 ).
      IF me->mo_log->check( ).
        RETURN.
      ENDIF.

      lv_full_url = 'https://' && lv_server && lv_url.

      lv_packages_left = abap_true.
      DATA(lv_total_count) = lines( me->mt_db_json ).
      MESSAGE i000(/vpcoe/common) WITH `Start to send the Data` INTO me->mo_log->sv_msg_text.
      me->mo_log->add_msg_progress( EXPORTING iv_msg_text = me->mo_log->sv_msg_text ).

      WHILE lv_packages_left = abap_true.

        LOOP AT me->mt_db_json ASSIGNING <ls_db_json> WHERE processing_ended   = abap_false AND
                                                            processing_started = abap_false AND
                                                            attempts           < 5.

          WAIT FOR ASYNCHRONOUS TASKS UNTIL me->mv_tasks_running < lv_wp_count.

          IF <ls_db_json>-do_wait = abap_true.
            <ls_db_json>-do_wait = abap_false.
            WAIT UP TO 1 SECONDS.
          ENDIF.

          DATA(lv_progress) = ( 100 * sy-tabix ) DIV lv_total_count.
          DATA(lv_attempt) = <ls_db_json>-attempts + 1.
          me->mo_log->add_msg_progress( EXPORTING iv_level    = `Attempt to send the Data # ` && lv_attempt
                                                  iv_progress = lv_progress ).

          <ls_db_json>-processing_started = abap_true.

          " Check if there is free session for background task
          lv_no_session = abap_true.
          WHILE lv_no_session = abap_true.
            IF sy-index > 5.
              EXIT.
            ENDIF.
            CALL FUNCTION 'RM_FREE_SESSION_CHECK'
              EXCEPTIONS
                no_free_session = 1
                OTHERS          = 2.
            IF sy-subrc = 0.
              lv_no_session = abap_false.
            ELSE.
              WAIT UP TO 5 SECONDS.
            ENDIF.
          ENDWHILE.

          IF lv_no_session = abap_false.
            CALL FUNCTION '/VPCOE/SEND_JSON_BCKGRND'
              STARTING NEW TASK <ls_db_json>-task_id
              CALLING decrease_wp_count ON END OF TASK
              EXPORTING
                iv_full_url           = lv_full_url
                iv_proxy_host         = lv_proxy_host
                iv_proxy_service      = lv_proxy_service
                iv_api_type           = <ls_db_json>-api_type
                iv_session_id         = <ls_db_json>-session_id
                iv_session_item       = <ls_db_json>-session_item
                iv_sub_object         = io_log->get_subobject( )
                iv_http_version       = lv_http_version
              EXCEPTIONS
                resource_failure      = 1
                system_failure        = 2
                communication_failure = 3
                OTHERS                = 4.

            CASE sy-subrc.
              WHEN 0.
                me->mv_tasks_running =  me->mv_tasks_running + 1.
              WHEN 1 OR 2 OR 3.
                <ls_db_json>-processing_started = abap_false.
                WAIT UP TO 1 SECONDS.
              WHEN OTHERS.
                ASSERT 1 = 2.
            ENDCASE.
          ELSE.
            MESSAGE e092(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            " No free sessions, need to change the system parameters or decrease WP count in VPCOE customizing
            me->mo_log->add_sy_msg( ).
            RETURN.
          ENDIF.

        ENDLOOP.

        IF line_exists( me->mt_db_json[  processing_ended = abap_false ] ).
          WAIT UP TO 1 SECONDS.
        ELSE.
          lv_packages_left = abap_false.
        ENDIF.

      ENDWHILE.

      DATA(lo_payload_handler) = NEW /vpcoe/cl_rdp_payload_handler( ).

      LOOP AT me->mt_db_json ASSIGNING <ls_db_json>.

        IF /vpcoe/cl_common_helper=>get_trace_enable( ) = abap_true.
          me->save_trace_log(
            EXPORTING
              is_json     = <ls_db_json>-json
              io_cust     = io_cust
              iv_response = COND #( WHEN <ls_db_json>-failed IS NOT INITIAL
                                      THEN <ls_db_json>-response_message )
              iv_status   = COND #( WHEN <ls_db_json>-failed IS NOT INITIAL
                                      THEN /vpcoe/cl_common_helper=>sc_status-failed
                                      ELSE /vpcoe/cl_common_helper=>sc_status-success ) ).
        ENDIF.

        IF <ls_db_json>-failed IS NOT INITIAL.
          cv_total = cv_total - <ls_db_json>-lines_count.
          cv_total_failed = cv_total_failed + <ls_db_json>-lines_count.

          lo_payload_handler->set_payload(
            EXPORTING
              iv_s_jsn_cloud = VALUE #( api_type         = io_cust->get_api_type( )
                                        service_group    = io_cust->get_srv_grp( )
                                        service_id       = io_cust->get_srv_id( )
                                        session_id       = <ls_db_json>-session_id
                                        session_item     = <ls_db_json>-session_item
                                        json             = <ls_db_json>-json
                                        lines_count      = <ls_db_json>-lines_count
                                        response_message = <ls_db_json>-response_message
                                        response_status  = <ls_db_json>-response_status
                                        created          = <ls_db_json>-created
                                        created_time     = <ls_db_json>-created_time
                                        failed           = <ls_db_json>-failed
                                        repeated         = sy-datum )
              io_log         = mo_log ).

          MESSAGE e004(/vpcoe/common) WITH  iv_api_type <ls_db_json>-session_id <ls_db_json>-session_item INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          me->mo_log->add_sy_msg( ).
        ENDIF.
      ENDLOOP.

      DELETE FROM /vpcoe/jsn_cloud WHERE session_id IN it_r_session_ids
                                     AND failed = abap_false.

      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD SET_PAYLOAD.

    DATA: ls_db_json TYPE /vpcoe/jsn_cloud.

    ls_db_json = VALUE /vpcoe/jsn_cloud( api_type      = iv_s_jsn_cloud-api_type
                                         service_group = iv_s_jsn_cloud-service_group
                                         service_id    = iv_s_jsn_cloud-service_id
                                         session_id    = iv_s_jsn_cloud-session_id
                                         session_item  = iv_s_jsn_cloud-session_item
                                         json          = iv_s_jsn_cloud-json
                                         lines_count   = iv_s_jsn_cloud-lines_count
                                         created       = iv_s_jsn_cloud-created
                                         created_time  = iv_s_jsn_cloud-created_time
                                         failed        = iv_s_jsn_cloud-failed
                                         response_message = iv_s_jsn_cloud-response_message
                                         response_status = iv_s_jsn_cloud-response_status  ).

    IF iv_s_jsn_cloud-session_id NOT IN me->mt_processed_notifications OR me->mt_processed_notifications IS INITIAL.
      "send e-mail notification only once per session
      /vpcoe/cl_rdp_helper=>send_error_payload_to_email(
         EXPORTING
           is_payload = iv_s_jsn_cloud
           io_log     = io_log ).

      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_s_jsn_cloud-session_id ) INTO TABLE me->mt_processed_notifications.
    ENDIF.

    MODIFY /vpcoe/jsn_cloud FROM @ls_db_json.

    IF sy-subrc = 0 AND iv_commit_work = abap_true.
      CALL FUNCTION 'DB_COMMIT'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
