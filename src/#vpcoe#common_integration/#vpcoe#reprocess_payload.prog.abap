*&---------------------------------------------------------------------*
*& Report  /VPCOE/PAYLOAD_HANDLER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/reprocess_payload.
INCLUDE /vpcoe/version.
INCLUDE /vpcoe/process_payload_top.
INCLUDE /vpcoe/process_payload_handler.
INCLUDE /vpcoe/process_payload_sel.

INITIALIZATION.

  INCLUDE /vpcoe/version_set.

START-OF-SELECTION.

  DATA: lo_payload_handler TYPE REF TO lcl_payload_handler,
        lv_payloads_count  TYPE int4,
        ls_selopt          TYPE lcl_payload_handler=>ty_s_sel_opt.

  lo_payload_handler     = NEW lcl_payload_handler( ).

  ls_selopt-api         = so_api[].
  ls_selopt-srv_grp     = so_srvg[].
  ls_selopt-srv_id      = so_srvi[].
  ls_selopt-session_id  = so_id[].
  ls_selopt-session_itm = so_itm[].
  ls_selopt-date        = so_date[].
  ls_selopt-time        = so_time[].

  lv_payloads_count = lo_payload_handler->select_payloads( ls_selopt ).

  IF p_auto = abap_true.
* Automatic Mode
    lo_payload_handler->process_auto( ).

  ELSEIF p_dele = abap_true.
* Cleanup Mode
    lo_payload_handler->process_deletion( lv_payloads_count ).

  ELSE.
* Manual Mode
    IF sy-batch = abap_true.
      MESSAGE e060(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      lo_payload_handler->mo_log->add_sy_msg( ).
    ELSE.
      lo_payload_handler->process_manual( lv_payloads_count ).
    ENDIF.
  ENDIF.

  IF sy-batch = space.
    IF p_dele = abap_true.
      lo_payload_handler->mo_log->display_message( iv_with_total = abap_false
                                                   iv_excel      = abap_true ).
    ELSE.
      IF lo_payload_handler->mv_processed = abap_true.
        lo_payload_handler->mo_log->display_message( ).
      ENDIF.
    ENDIF.
  ELSE.
    lo_payload_handler->mo_log->save( ).
  ENDIF.

  INCLUDE /vpcoe/process_payload_stato01.
  INCLUDE /vpcoe/process_payload_useri01.
  INCLUDE /vpcoe/process_payload_fillf01.
