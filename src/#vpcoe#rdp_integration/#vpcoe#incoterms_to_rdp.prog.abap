*&---------------------------------------------------------------------*
*& Report  /VPCOE/INCOTERMS_TO_RDP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/incoterms_to_rdp.

DATA: lv_ucomm    TYPE sy-ucomm,
      go_log      TYPE REF TO /vpcoe/cl_rdp_log,
      lt_file     TYPE filetable,
      lv_rc       TYPE i,
      gv_default  TYPE string VALUE /vpcoe/cl_rdp_helper=>sc_service_id-incoterms,
      gv_api_type TYPE /vpcoe/de_api_type.

INCLUDE /vpcoe/version.
SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-opt.
PARAMETERS: r_screen RADIOBUTTON GROUP req USER-COMMAND et,
            r_send   RADIOBUTTON GROUP req,
            r_excel  RADIOBUTTON GROUP req.
INCLUDE /vpcoe/i_excel_selscr.
SELECTION-SCREEN END OF BLOCK options.

INCLUDE /vpcoe/i_excel_valreq.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  lv_ucomm = sy-ucomm.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name CS 'P_FILE'.
      screen-active = COND #( WHEN r_excel = abap_true THEN 1
                                                       ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 = 'TRL'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.
  INCLUDE /vpcoe/version_set.
  INCLUDE /vpcoe/i_excel_scrout.

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_INCOTERMS' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.

START-OF-SELECTION.

  go_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-incoterms ).

  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    go_log->add_sy_msg( ).
    go_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  gv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp.
  DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                            iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                            iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-incoterms ).

  DATA(lv_mode) = lo_cust->set_mode( iv_screen = r_screen
                                     iv_excel  = r_excel ).

  DATA(lo_rdp_api) = /vpcoe/cl_rdp_api_instance=>get_instance( ).

  DATA(lo_config_data) = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                              iv_mode       = lv_mode
                                                              io_log        = go_log ).

  IF r_excel = abap_true AND p_file IS INITIAL.
    MESSAGE e064(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    go_log->add_sy_msg( ).

    go_log->display_message( iv_with_total = abap_false iv_excel = abap_true ).
    go_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  lo_config_data->get_incoterms(
    IMPORTING
      et_incoterms    = DATA(gt_incoterms)
      et_json         = DATA(gt_json) ).

  CASE lv_mode.
    WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
      IF lines( gt_incoterms ) >= /vpcoe/cl_rdp_helper=>gc_display_amount.
        MESSAGE i065(/vpcoe/common).
        DELETE gt_incoterms FROM /vpcoe/cl_rdp_helper=>gc_display_amount + 1.
      ENDIF.

      lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_incoterms
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-incoterms ).

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
      lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                      it_json   = gt_json
                                      io_cust   = lo_cust ).

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
      IF p_file IS NOT INITIAL.
        IF sy-batch = abap_true AND r_server = abap_false.
          MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          go_log->add_sy_msg( ).
        ELSE.
          lo_config_data->download_excel(
            EXPORTING
              iv_file_path = CONV #( p_file )
              iv_save_background = COND #( WHEN r_server = abap_true THEN abap_true
                                                               ELSE abap_false )
              iv_incoterms = abap_true
              io_log = go_log ).

          IF NOT go_log->check( ).
            MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            go_log->add_sy_msg( ).
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE e064(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        go_log->add_sy_msg( ).
      ENDIF.

      go_log->display_message( iv_with_total = abap_false iv_excel = abap_true ).
      go_log->save( iv_no_total = abap_true ).
  ENDCASE.
