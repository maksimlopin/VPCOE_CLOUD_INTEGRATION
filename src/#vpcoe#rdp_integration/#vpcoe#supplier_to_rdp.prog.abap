*&---------------------------------------------------------------------*
*& Report  /VPCOE/SUPPLIER_TO_RDP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/supplier_to_rdp.
TABLES: lfa1, cdhdr, lfm1, lfb1.

DATA:
  gt_bapiret2      TYPE bapiret2_t,
  gt_supplier      TYPE STANDARD TABLE OF /vpcoe/cl_rdp_supplier_data=>gty_s_supplier_jsn,
  lv_ucomm         TYPE sy-ucomm,
  gv_api_type      TYPE /vpcoe/de_api_type,
  gs_sel_opt       TYPE /vpcoe/s_selopt_supplier,
  gv_supplier_role TYPE /vpcoe/de_supplier_role,
  gv_default       TYPE string VALUE /vpcoe/cl_rdp_helper=>sc_service_id-supplier.

INCLUDE /vpcoe/supplier_trans_selscr.

START-OF-SELECTION.

  DATA(lv_mode) = /vpcoe/cl_rdp_helper=>set_mode( iv_screen = r_screen
                                                  iv_excel  = r_excel ).

  DATA(go_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-supplier
                                        iv_mode       = lv_mode ).

  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    go_log->add_sy_msg( ).
    go_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  gv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp.

  DATA(go_supplier_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                                     iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-supplier
                                                     iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-supplier ).

  DATA(lo_rdp_api) = /vpcoe/cl_rdp_api_instance=>get_instance( ).
  DATA(go_supplier_data) = lo_rdp_api->get_supplier_handler( io_rdp_helper = go_supplier_cust
                                                             io_log        = go_log
                                                             iv_mode       = lv_mode ).


  gs_sel_opt-supplier_id = so_id[].
  gs_sel_opt-country     = so_cntry[].
  gs_sel_opt-region      = so_reg[].
  gs_sel_opt-crt_date    = so_crt[].
  gs_sel_opt-chng_date   = so_chng[].
  gs_sel_opt-ekorg       = so_ekorg[].
  gs_sel_opt-ktokk       = so_ktokk[].
  gs_sel_opt-bukrs       = so_bukrs[].

ENHANCEMENT-POINT /vpcoe/supplier_set_data SPOTS /vpcoe/enh_supplier .

  IF p_delta = abap_true.
    go_supplier_data->get_supplier_delta(
     EXPORTING
       iv_chng_pointer_id = p_chgpid
       is_sel_opt         = gs_sel_opt
       io_log             = go_log
     IMPORTING
       et_supplier        = DATA(gt_supplier_sort)
       et_json            = DATA(gt_json)
       es_supplier_tables = DATA(gs_supplier_tables) ).

  ELSE.
    go_supplier_data->get_supplier(
      EXPORTING
        iv_delta           = p_delta
        is_sel_opt         = gs_sel_opt
        io_log             = go_log
      IMPORTING
        et_supplier        = gt_supplier_sort
        et_json            = gt_json
        es_supplier_tables = gs_supplier_tables ).

  ENDIF.

  CASE lv_mode.
    WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
      gt_supplier = gt_supplier_sort.
      IF lines( gt_supplier ) > /vpcoe/cl_rdp_helper=>gc_display_amount.
        DELETE gt_supplier FROM /vpcoe/cl_rdp_helper=>gc_display_amount.
        MESSAGE i065(/vpcoe/common).
      ENDIF.
      go_supplier_cust->display_selected_data(
        EXPORTING
          it_deep_data     = gt_supplier
          iv_lable         = /vpcoe/cl_rdp_helper=>sc_service_id-supplier
          it_columns_title = VALUE #( ( column = 'IS_MARKED_FOR_DELETION' text = 'Deletion Indicator'(del) ) ) ).

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
      IF sy-batch = abap_true AND r_server = abap_false.
        MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        go_log->add_sy_msg( ).
      ELSE.
        IF p_file IS NOT INITIAL.
          go_supplier_data->download_excel(
            EXPORTING
              is_supplier_tables  = gs_supplier_tables
              iv_file_path        = CONV #( p_file )
              iv_save_background  = COND #( WHEN r_server = abap_true THEN abap_true
                                                                      ELSE abap_false )
              io_log = go_log ).
          IF NOT go_log->check( ).
            MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            go_log->add_sy_msg( ).
          ENDIF.
        ELSE.
          MESSAGE e064(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          go_log->add_sy_msg( ).
        ENDIF.
      ENDIF.

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
      IF gt_json IS INITIAL.
        MESSAGE i015(/vpcoe/common).
        go_log->add_sy_msg( ).
      ELSE.

        NEW /vpcoe/cl_rdp_payload_handler( )->send_payload(
          EXPORTING
            io_cust     = go_supplier_cust
            it_json     = gt_json
            io_log      = go_log
          IMPORTING
            ev_status = DATA(lv_status) ).

        IF lv_status = /vpcoe/cl_rdp_http=>gc_s_http_status-ok.
          go_supplier_data->change_status( EXPORTING iv_test_run = COND #( WHEN r_send = abap_true THEN abap_false ELSE abap_true ) ).
        ENDIF.

      ENDIF.
  ENDCASE.
  IF lo_rdp_api->is_suma_api_enabled( go_supplier_cust ).
    go_supplier_data->close_replication( go_log ).
  ENDIF.
  IF lv_mode <> /vpcoe/cl_rdp_helper=>sc_mode-screen.
    go_log->display_message( iv_with_total = COND #( WHEN r_send = abap_true THEN abap_true
                                                                             ELSE abap_false )
                             iv_excel = r_excel ).
    go_log->save( iv_no_total = abap_true ).
  ENDIF.
