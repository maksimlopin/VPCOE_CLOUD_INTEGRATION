class /VPCOE/CL_RDP_HELPER definition
  public
  inheriting from /VPCOE/CL_COMMON_HELPER
  final
  create public .

public section.

  constants:
    BEGIN OF sc_bool,
        true  TYPE char4 VALUE 'true',
        false TYPE char5 VALUE 'false',
      END OF sc_bool .

  methods DELETE_DELTA_IDS
    importing
      !IT_R_SEL_NAME type /VPCOE/TT_R_NAME
    exporting
      !ET_BAPIRET2 type BAPIRET2_T .
  methods GET_DELTA_IDS
    exporting
      !ET_SEL_NAMES type GTY_T_SEL_NAMES .
  methods GET_DELTA_SEL_OPT
    importing
      !IV_SEL_NAME type /VPCOE/DE_SEL_NAME
    exporting
      !ET_SEL_OPT type GTY_T_SEL_OPT
      !EV_LAST_RUN type DATS .
  methods SAVE_DELTA_SEL_OPT
    importing
      !IV_SEL_NAME type /VPCOE/DE_SEL_NAME
      !IV_TEST_RUN type ABAP_BOOL
      !IT_SEL_PARAMS type GTY_T_SEL_OPT .
  class-methods SEND_ERROR_PAYLOAD_TO_EMAIL
    importing
      !IS_PAYLOAD type /VPCOE/S_JSN_CLOUD
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
      !IV_ONLY_BACKGROUND type ABAP_BOOL default 'X' .

  methods GET_GENERIC_RFC_NAME
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /VPCOE/CL_RDP_HELPER IMPLEMENTATION.


  METHOD delete_delta_ids.
    CLEAR: et_bapiret2.

    DELETE FROM /vpcoe/int_delta
          WHERE service_grp        = @me->mv_srv_grp
                AND service_id     = @me->mv_srv_id
                AND selection_name IN @it_r_sel_name .

    IF sy-subrc <> 0.
      MESSAGE e095(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      /vpcoe/cl_rdp_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
    ENDIF.
  ENDMETHOD.


  METHOD get_delta_ids.

    CLEAR: et_sel_names.

    SELECT selection_name
      FROM /vpcoe/int_delta
        INTO TABLE @et_sel_names
          WHERE service_grp = @me->mv_srv_grp
            AND service_id  = @me->mv_srv_id.

    IF sy-subrc <> 0.
      CLEAR et_sel_names.
    ENDIF.

  ENDMETHOD.


  METHOD get_delta_sel_opt.
    CLEAR: et_sel_opt,
           ev_last_run.

    SELECT SINGLE *
      FROM /vpcoe/int_delta
        INTO @DATA(ls_delta)
          WHERE service_grp    = @me->mv_srv_grp
            AND service_id     = @me->mv_srv_id
            AND selection_name = @iv_sel_name.

    IF sy-subrc = 0.
      ev_last_run = ls_delta-last_run.

      /vpcoe/cl_rdp_helper=>deserialize_json(
        EXPORTING
          iv_json = ls_delta-sel_options
        CHANGING
          cs_data = et_sel_opt ).
    ENDIF.

  ENDMETHOD.


METHOD get_generic_rfc_name.

  " Override predefined RFC Connection Name
  rv_gen_rfc_name = /vpcoe/cl_rdp_suma_helper=>get_override_rfc_id( EXPORTING iv_endpoint = abap_true ).

  IF rv_gen_rfc_name IS INITIAL.
    rv_gen_rfc_name = 'VPCOE_RDP_GENERIC'.
  ENDIF.

ENDMETHOD.


  METHOD save_delta_sel_opt.
    DATA ls_delta TYPE /vpcoe/int_delta.

    ls_delta-service_grp    = me->mv_srv_grp.
    ls_delta-service_id     = me->mv_srv_id.
    ls_delta-selection_name = iv_sel_name.
    ls_delta-last_run       = sy-datum.
    ls_delta-changed_by     = sy-uname.
    ls_delta-sel_options    = me->serialize_json( EXPORTING is_data = it_sel_params ).

    IF iv_test_run = abap_false.
      MODIFY /vpcoe/int_delta FROM ls_delta.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD send_error_payload_to_email.

    DATA: lv_subject      TYPE so_obj_des,
          lt_message_body TYPE bcsy_text,
          lv_result       TYPE boolean,
          lv_skip         TYPE boolean,
          lo_sender       TYPE REF TO if_sender_bcs VALUE IS INITIAL,
          lo_send_request TYPE REF TO cl_bcs VALUE IS INITIAL,
          lv_sender       TYPE adr6-smtp_addr,
          lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL.

    DATA: lo_badi TYPE REF TO /vpcoe/notification_processing.

    IF iv_only_background = abap_true AND sy-batch = abap_false.
      RETURN.
    ENDIF.

    GET BADI lo_badi.

    "read recipients
    SELECT *
      FROM /vpcoe/recipient
      WHERE api_type = @is_payload-api_type
       AND send_to_email        = @abap_true
       OR  send_to_internal_box = @abap_true
    INTO TABLE @DATA(lt_recipient).

    IF sy-subrc <> 0.
      MESSAGE e067(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      io_log->add_sy_msg( ).
      "add message table not maintain
      RETURN.
    ENDIF.

    "Prepare Mail Object
    CALL BADI lo_badi->notification_overwrite
      EXPORTING
        is_payload = is_payload
        io_log     = io_log
      CHANGING
        cv_skip    = lv_skip.

    IF lv_skip = abap_true.
      RETURN.
    ENDIF.

    " Message body and subject
    CALL BADI lo_badi->notification_change_body
      EXPORTING
        is_payload = is_payload
        io_log     = io_log
      CHANGING
        ct_message = lt_message_body
        cv_subject = lv_subject.

    TRY.
        DATA(lo_document) = cl_document_bcs=>create_document( i_type    = 'RAW'
                                                              i_text    = lt_message_body
                                                              i_subject = lv_subject ).
      CATCH cx_document_bcs INTO DATA(lx_document_bcs).
        io_log->add_from_exception( lx_document_bcs ).
        RETURN.
    ENDTRY.

    SELECT SINGLE e~smtp_addr AS email
      FROM usr21 AS u
        JOIN adr6 AS e ON u~persnumber = e~persnumber AND e~flgdefault = 'X'
      WHERE u~bname = @sy-uname
        INTO @lv_sender.

    IF sy-subrc <> 0.
      CLEAR lv_sender.
    ENDIF.

    TRY.
        CLASS cl_bcs DEFINITION LOAD.
        lo_send_request = cl_bcs=>create_persistent( ).
        " Pass the document to send request
        lo_send_request->set_document( lo_document ).
      CATCH cx_send_req_bcs cx_address_bcs INTO DATA(lx_bcs).
        io_log->add_from_exception( lx_bcs ).
        RETURN.
    ENDTRY.

    LOOP AT lt_recipient ASSIGNING FIELD-SYMBOL(<ls_recipient>).
      TRY.
          IF <ls_recipient>-send_from IS INITIAL.
            <ls_recipient>-send_from = lv_sender. " Get Email from Current User parameters
          ENDIF.

          CALL BADI lo_badi->notification_change_sender
            EXPORTING
              is_payload = is_payload
            CHANGING
              cv_sender  = <ls_recipient>-send_from.

          IF <ls_recipient>-send_from IS INITIAL AND <ls_recipient>-user_id IS INITIAL.
            MESSAGE e068(/vpcoe/common) WITH sy-uname && '/' && <ls_recipient>-user_email  INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            io_log->add_sy_msg( ).
            CONTINUE.
          ENDIF.

          lo_sender = cl_cam_address_bcs=>create_internet_address( <ls_recipient>-send_from ).
          lo_send_request->set_sender( EXPORTING i_sender = lo_sender ).

          IF <ls_recipient>-user_email IS NOT INITIAL
             AND <ls_recipient>-send_to_email = abap_true
             AND <ls_recipient>-send_from IS NOT INITIAL.

            lo_recipient = cl_cam_address_bcs=>create_internet_address( CONV #( <ls_recipient>-user_email ) ).
            lo_send_request->add_recipient(
              EXPORTING
                i_recipient = lo_recipient
                i_express = '' ).
          ENDIF.

          IF  <ls_recipient>-user_id IS NOT INITIAL AND <ls_recipient>-send_to_internal_box = abap_true.

            lo_recipient = cl_sapuser_bcs=>create( <ls_recipient>-user_id  ).
            lo_send_request->add_recipient(
              EXPORTING
                i_recipient = lo_recipient
                i_express = 'X' ).
          ENDIF.

        CATCH cx_send_req_bcs cx_address_bcs INTO lx_bcs.
          io_log->add_from_exception( lx_bcs ).
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    TRY.
        "Send email
        IF lo_send_request->send( i_with_error_screen = abap_true ).
          COMMIT WORK.
        ENDIF.

      CATCH cx_send_req_bcs INTO DATA(lx_bcs_req).
        io_log->add_from_exception( lx_bcs_req ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
