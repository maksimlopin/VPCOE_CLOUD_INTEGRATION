CLASS /vpcoe/cl_uph_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.           "#EC INTF_IN_CLASS     "#EC NUM_PUBLIC_ATTR
    TYPE-POOLS abap .
    INTERFACE if_ehfnd_chm_impl_c LOAD .
    INTERFACE if_ehfnd_msg_severity_c LOAD .

*  interface IF_EHFND_CHM_IMPL_C load .
*  interface IF_EHFND_MSG_SEVERITY_C load .
    INTERFACES /vpcoe/if_uph_logger .

    CONSTANTS gc_bal_object TYPE balobj_d VALUE '/VPCOE/RDP' ##NO_TEXT.
    CONSTANTS gc_bal_subobj TYPE balsubobj VALUE 'PLM' ##NO_TEXT.
    CLASS-DATA mo_surdp_logger TYPE REF TO /vpcoe/cl_uph_logger .
    CLASS-DATA gv_upload_mode_text TYPE string .

    METHODS constructor
      IMPORTING
        VALUE(iv_repid)   TYPE syrepid
        !iv_test_mode     TYPE boole_d DEFAULT abap_false
        !iv_bal_object    TYPE balobj_d DEFAULT gc_bal_object
        !iv_bal_subobject TYPE balsubobj DEFAULT gc_bal_subobj
        !iv_upload_mode   TYPE /vpcoe/upload_mode OPTIONAL
        !iv_consl_print   TYPE abap_bool OPTIONAL .
  PROTECTED SECTION.

*    DATA mo_logger TYPE REF TO cl_ehfnd_fw_logger .
*    DATA mo_log_node TYPE REF TO cl_ehfnd_fw_log_node .
    DATA mv_consl_print TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA mt_messages TYPE /vpcoe/t_uph_msg .
    DATA mv_log_handle TYPE balloghndl .
    DATA mt_msg_bapiret TYPE bapirettab .
  PRIVATE SECTION.

    CONSTANTS gc_delete_after_days TYPE i VALUE 30 ##NO_TEXT.

    METHODS write_msg_to_console
      IMPORTING
        !is_message TYPE bal_s_msg .
    METHODS add_bapiret_to_log .
ENDCLASS.



CLASS /VPCOE/CL_UPH_LOGGER IMPLEMENTATION.


  METHOD /vpcoe/if_uph_logger~add_bapi_messages.

    DATA:
      lr_bapi_message TYPE REF TO bapiret2,
      lt_messages     TYPE /vpcoe/t_uph_msg.

    CLEAR: lt_messages.

    LOOP AT it_bapi_messages REFERENCE INTO lr_bapi_message.
      APPEND INITIAL LINE TO lt_messages REFERENCE INTO DATA(lr_message).
      lr_message->msgty = lr_bapi_message->type.
      lr_message->msgid = lr_bapi_message->id.
      lr_message->msgno = lr_bapi_message->number.
      lr_message->msgv1 = lr_bapi_message->message_v1.
      lr_message->msgv2 = lr_bapi_message->message_v2.
      lr_message->msgv3 = lr_bapi_message->message_v3.
      lr_message->msgv4 = lr_bapi_message->message_v4.
    ENDLOOP.

    IF ( lt_messages IS NOT INITIAL ).
      /vpcoe/if_uph_logger~add_messages( lt_messages ).
    ENDIF.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_logger~add_http_response.

    LOOP AT is_response-details ASSIGNING FIELD-SYMBOL(<ls_details>).
      CONCATENATE <ls_details>-code ':' INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      CONCATENATE <ls_details>-target <ls_details>-message INTO /vpcoe/cl_rdp_log=>sv_msg_text SEPARATED BY space.
      INSERT VALUE #( type    = 'E'
                      message = /vpcoe/cl_rdp_log=>sv_msg_text ) INTO TABLE mt_msg_bapiret.
    ENDLOOP.

    IF is_response-message IS NOT INITIAL.
      INSERT VALUE #( type    = 'E'
                      message = is_response-message ) INTO TABLE mt_msg_bapiret.
    ENDIF.

    IF iv_reason IS NOT INITIAL.
      INSERT VALUE #( type    = 'E'
                      message = iv_reason ) INTO TABLE mt_msg_bapiret.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_logger~add_messages.

    DATA: ls_bal_msg TYPE bal_s_msg,
          ls_msg     TYPE /vpcoe/s_uph_msg.

    LOOP AT it_messages REFERENCE INTO DATA(lr_message).
      ls_bal_msg = CORRESPONDING #( lr_message->* ).

      "if details are present -> log them
      IF lr_message->details IS NOT INITIAL.

        INSERT lr_message->* INTO TABLE mt_messages.

        IF mv_consl_print = abap_true.
          write_msg_to_console( ls_bal_msg  ).

          LOOP AT lr_message->details INTO DATA(ls_det_msg).
            write_msg_to_console( ls_det_msg ).
          ENDLOOP.

        ENDIF.

      ELSE.
        MOVE-CORRESPONDING ls_bal_msg TO ls_msg.
        APPEND ls_msg TO mt_messages.
*        mo_log_node->add_message( ls_bal_msg ).

        IF mv_consl_print = abap_true.
          write_msg_to_console( ls_bal_msg ).
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_logger~commit_application_log.

    DATA: lt_messages TYPE /vpcoe/t_uph_msg.
    DATA: lt_loghandle                TYPE bal_t_logh.

    CLEAR: lt_loghandle.

    APPEND INITIAL LINE TO lt_messages ASSIGNING FIELD-SYMBOL(<lfs_message>).
    <lfs_message>-msgid = /vpcoe/cl_uph_exec_load_wrap=>gc_default_msgclass .
    <lfs_message>-msgty = COND #( WHEN
          /vpcoe/if_uph_logger~has_error_messages(  ) IS NOT INITIAL
             THEN /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_e
             WHEN /vpcoe/if_uph_logger~has_warning_messages(  ) IS NOT INITIAL
             THEN /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_w
             ELSE /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_i ).

    WRITE: sy-datum TO <lfs_message>-msgv1,
           sy-uzeit TO <lfs_message>-msgv2 .
    <lfs_message>-msgv3 = gv_upload_mode_text.
    IF iv_test_mode = abap_false.
      <lfs_message>-msgno = '016'.
    ELSEIF iv_test_mode = abap_true.
      <lfs_message>-msgno = '017'.
    ENDIF.

    /vpcoe/if_uph_logger~add_messages( it_messages = lt_messages ) .

    UNASSIGN <lfs_message>.

*     add the messages to the application log
*    mo_logger->add_nodes_to_application_log( ).
    add_bapiret_to_log( ).

    APPEND me->mv_log_handle TO lt_loghandle.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle = lt_loghandle
        i_save_all     = 'X'
      EXCEPTIONS
        log_not_found  = 0
        OTHERS         = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF ( iv_flg_commit = abap_true ).
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_logger~get_app_log_object.

*    mo_logger->get_app_log_object(
*      IMPORTING
*        ev_object    = ev_object
*        ev_subobject = ev_subobject
*      ).

  ENDMETHOD.


  METHOD /vpcoe/if_uph_logger~get_bapi_messages.

    DATA:
      lt_messages     TYPE /vpcoe/t_uph_msg,
      lr_bapi_message TYPE REF TO bapiret2.

    REFRESH: rt_bapi_messages.

    /vpcoe/if_uph_logger~get_messages(
       IMPORTING
         et_messages = lt_messages    " Table for Messages
    ).

    LOOP AT lt_messages REFERENCE INTO DATA(lr_message).
      APPEND INITIAL LINE TO rt_bapi_messages
             REFERENCE INTO lr_bapi_message.

      lr_bapi_message->type       = lr_message->msgty.
      lr_bapi_message->id         = lr_message->msgid.
      lr_bapi_message->number     = lr_message->msgno.
      lr_bapi_message->message_v1 = lr_message->msgv1.
      lr_bapi_message->message_v2 = lr_message->msgv2.
      lr_bapi_message->message_v3 = lr_message->msgv3.
      lr_bapi_message->message_v4 = lr_message->msgv4.
    ENDLOOP.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_logger~get_messages.

    et_messages = mt_messages.

*    DATA: lt_bal_msg TYPE ehfndt_fw_msg.
*
*    REFRESH: et_messages.
*
*    mo_log_node->get_messages(
*      IMPORTING
*        et_messages = lt_bal_msg
*    ).
*
*    et_messages = CORRESPONDING #( lt_bal_msg ).

  ENDMETHOD.


  METHOD /vpcoe/if_uph_logger~has_error_messages.

*    rv_flg_has_errors = mo_log_node->has_error_messages( ).
    READ TABLE  mt_messages TRANSPORTING NO FIELDS WITH KEY msgty = /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_e.
    IF sy-subrc = 0.
      rv_flg_has_errors = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_logger~has_warning_messages.

*    rv_flg_has_warnings = mo_log_node->has_warning_messages( ).

    READ TABLE  mt_messages TRANSPORTING NO FIELDS WITH KEY msgty = /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_w .
    IF sy-subrc = 0.
      rv_flg_has_warnings = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_logger~write_to_console.

    /vpcoe/if_uph_logger~get_messages(
      IMPORTING
        et_messages = DATA(lt_messages)
    ).

    LOOP AT lt_messages INTO DATA(ls_message).

      write_msg_to_console( CORRESPONDING #( ls_message ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD add_bapiret_to_log.

    LOOP AT mt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = mv_log_handle
          i_s_msg          = VALUE bal_s_msg( msgty = <ls_message>-msgty
                                              msgid = <ls_message>-msgid
                                              msgno = <ls_message>-msgno
                                              msgv1 = <ls_message>-msgv1
                                              msgv2 = <ls_message>-msgv2
                                              msgv3 = <ls_message>-msgv3
                                              msgv4 = <ls_message>-msgv4 )
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    LOOP AT  mt_msg_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle     = mv_log_handle
          i_msgty          = <ls_bapiret2>-type
          i_text           = <ls_bapiret2>-message
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.

    DATA:
      ls_log_header TYPE bal_s_log,
      lr_log_msg    TYPE REF TO bal_s_msg.

    mv_consl_print = iv_consl_print.

    ls_log_header-object     = iv_bal_object.
    ls_log_header-subobject  = iv_bal_subobject.

    ls_log_header-alprog     = iv_repid.
    ls_log_header-altcode    = sy-tcode.
    ls_log_header-aldate_del = sy-datum + gc_delete_after_days.
    ls_log_header-del_before = abap_true.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log_header
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*    mo_logger = NEW #( is_log_header = lr_log_header->* ).

* Build Log node
    CREATE DATA lr_log_msg.
    WRITE sy-datum TO lr_log_msg->msgv1.
    WRITE sy-uzeit TO lr_log_msg->msgv2.

    gv_upload_mode_text =
         /vpcoe/cl_uph_exec_load_wrap=>get_domain_fix_val_text(
         iv_domain_name = /vpcoe/cl_uph_exec_load_wrap=>gc_domname_upmode
         iv_fixed_value = iv_upload_mode ).

    lr_log_msg->msgv3 = gv_upload_mode_text.
    lr_log_msg->msgid = /vpcoe/cl_uph_exec_load_wrap=>gc_default_msgclass.
    lr_log_msg->msgty = /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_i.

    IF iv_test_mode = abap_false.
      lr_log_msg->msgno = '014'.
    ELSEIF iv_test_mode = abap_true.
      lr_log_msg->msgno = '015'.
    ENDIF.
*
*    mo_logger->create_node(
*      EXPORTING
*        is_head_message        = VALUE bal_s_msg( )
*        iv_flg_is_general_node = abap_true
*      IMPORTING
*        eo_node = mo_log_node
*    ).

    /vpcoe/if_uph_logger~add_messages( it_messages = VALUE #( ( CORRESPONDING #( lr_log_msg->* ) ) ) ).

  ENDMETHOD.


  METHOD write_msg_to_console.

    IF is_message IS NOT INITIAL.

      MESSAGE ID is_message-msgid TYPE is_message-msgty NUMBER is_message-msgno
            WITH is_message-msgv1 is_message-msgv2 is_message-msgv3 is_message-msgv4 INTO /vpcoe/cl_rdp_log=>sv_msg_text.

      WRITE: / /vpcoe/cl_rdp_log=>sv_msg_text.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 0
          text       = /vpcoe/cl_rdp_log=>sv_msg_text.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
