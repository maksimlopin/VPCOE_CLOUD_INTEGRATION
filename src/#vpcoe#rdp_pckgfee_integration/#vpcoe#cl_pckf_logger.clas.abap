CLASS /vpcoe/cl_pckf_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.           "#EC INTF_IN_CLASS     "#EC NUM_PUBLIC_ATTR
    TYPE-POOLS abap .
    INTERFACE if_ehfnd_chm_impl_c LOAD .
    INTERFACE if_ehfnd_msg_severity_c LOAD .

    INTERFACES /vpcoe/if_pckf_logger .

    CONSTANTS gc_bal_object TYPE balobj_d VALUE '/VPCOE/RDP' ##NO_TEXT.
    CONSTANTS gc_bal_subobj TYPE balsubobj VALUE 'PCKG_FEE' ##NO_TEXT.
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

    DATA mv_consl_print TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA mt_messages TYPE /vpcoe/t_uph_msg .
    DATA mv_log_handle TYPE balloghndl .
    DATA mt_msg_bapiret TYPE bapirettab .

  PRIVATE SECTION.

    CONSTANTS gc_delete_after_days TYPE i VALUE 30 ##NO_TEXT.

    METHODS write_msg_to_console
      IMPORTING
        !is_message TYPE bal_s_msg .

    METHODS add_msgtab_to_log .

ENDCLASS.



CLASS /VPCOE/CL_PCKF_LOGGER IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_logger~add_http_response.

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


  METHOD /vpcoe/if_pckf_logger~add_messages.

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

        IF mv_consl_print = abap_true.
          write_msg_to_console( ls_bal_msg ).
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_logger~commit_application_log.

    DATA: lt_messages TYPE /vpcoe/t_uph_msg.
    DATA: lt_loghandle                TYPE bal_t_logh.

    APPEND me->mv_log_handle TO lt_loghandle.

    add_msgtab_to_log( ).

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


  METHOD /vpcoe/if_pckf_logger~get_domain_fix_val_text.

    DATA : ls_dom_val TYPE dd07v,
           lv_subrc   TYPE sy-subrc,
           lv_domname TYPE domname,
           lv_value   TYPE domvalue_l.

    lv_domname = iv_domain_name.
    lv_value = iv_fixed_value.

    " Get the domain fixed value text
    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        domname  = lv_domname
        value    = lv_value
      IMPORTING
        dd07v_wa = ls_dom_val
        rc       = lv_subrc.


    IF lv_subrc IS INITIAL.
      rv_fix_val_text = ls_dom_val-ddtext.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_logger~get_messages.

    et_messages = mt_messages.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_logger~has_error_messages.

    READ TABLE  mt_messages TRANSPORTING NO FIELDS WITH KEY msgty = 'E'.
    IF sy-subrc = 0.
      rv_flg_has_errors = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_logger~has_warning_messages.

    READ TABLE  mt_messages TRANSPORTING NO FIELDS WITH KEY msgty = 'W' .
    IF sy-subrc = 0.
      rv_flg_has_warnings = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_logger~write_to_console.

    /vpcoe/if_pckf_logger~get_messages(
      IMPORTING
        et_messages = DATA(lt_messages)
    ).

    LOOP AT lt_messages INTO DATA(ls_message).

      write_msg_to_console( CORRESPONDING #( ls_message ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD add_msgtab_to_log.

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
      ls_log_header TYPE bal_s_log.

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
