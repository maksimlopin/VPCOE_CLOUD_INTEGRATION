*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_CUSTOMER_TO_CLOUD_GEN
*&---------------------------------------------------------------------*

INCLUDE /vpcoe/i_excel_valreq.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF screen-name CS 'P_CHGPID'.
      screen-input = COND #( WHEN p_delta = abap_true THEN 1
                                                      ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_FILE'.
      screen-active = COND #( WHEN r_excel = abap_true THEN 1
                                                    ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 = 'TRL'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

  INCLUDE /vpcoe/i_excel_scrout.

  INCLUDE /vpcoe/version_set.

ENHANCEMENT-POINT /vpcoe/customer_sel_scr_out SPOTS /vpcoe/enh_customer .

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_CUSTOMER' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.

  go_payload_handler = NEW #( ).

START-OF-SELECTION.

  DATA(lv_mode) = /vpcoe/cl_rdp_helper=>set_mode( iv_screen = r_screen
                                                  iv_excel  = r_excel ).
  DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-customer
                                        iv_mode       = lv_mode ).

  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    lo_log->add_sy_msg( ).
    lo_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  CLEAR gv_runid.

  IF pgen = abap_false AND pext = abap_false.
    IF sy-batch = space.
      MESSAGE i029(/vpcoe/common).
    ELSE.
      MESSAGE i029(/vpcoe/common) INTO DATA(ls_msg) ##NEEDED.
    ENDIF.
    lo_log->add_sy_msg( ).
    lo_log->save( ).
  ENDIF.
  gv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp.
  DATA(go_log_ext) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-customer_ext  ).

  DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                            iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
                                            iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer ).

*  DATA(lo_customer_data) = NEW /vpcoe/cl_rdp_customer_data( iv_api_type     = gv_api_type
*                                                            iv_package_size = lo_cust->get_package_size( )
*                                                            iv_source       = /vpcoe/cl_rdp_helper=>get_source_id( )
*                                                            iv_mode         = lv_mode ).


  DATA(lo_rdp_api) = /vpcoe/cl_rdp_api_instance=>get_instance( ).
  DATA(lo_customer_data) = lo_rdp_api->get_customer_handler( io_rdp_helper = lo_cust iv_mode = lv_mode ).
  DATA(gv_url) = lo_cust->get_service_url( IMPORTING et_bapiret2 = gt_bapiret2 ).

  IF lo_log->check( ).
    lo_log->add_bapiret( EXPORTING it_bapiret2_t = gt_bapiret2 ).
    IF sy-batch = space.
      lo_log->display_message( ).
    ENDIF.
    lo_log->save( ).
    RETURN.
  ENDIF.

  lo_log->add_bapiret( EXPORTING it_bapiret2_t = gt_bapiret2 ).

  " Get Customer
  gs_sel_opt-customer_id = so_id[].
  gs_sel_opt-country     = so_cntry[].
  gs_sel_opt-region      = so_reg[].
  gs_sel_opt-crt_date    = so_crt[].
  gs_sel_opt-chng_date   = so_chng[].
  gs_sel_opt-vkorg       = so_vkorg[].
  gs_sel_opt-ktokd       = so_ktotd[].
  gs_sel_opt-bukrs       = so_bukrs[].

  gs_sel_opt_ext = CORRESPONDING #( gs_sel_opt ).
  gs_sel_opt_ext-delta_only = p_delta.
  gs_sel_opt_ext-cust_num   = so_id[].
  gs_sel_opt_ext-role       = so_role[].

ENHANCEMENT-POINT /vpcoe/customer_set_data SPOTS /vpcoe/enh_customer .

  IF p_delta = abap_true.
    lo_customer_data->get_customer_delta( EXPORTING iv_chng_pointer_id = p_chgpid
                                                    io_log             = lo_log
                                                    is_sel_opt      = gs_sel_opt
                                          IMPORTING et_json         = DATA(gt_json)
                                                    et_customer     = DATA(gt_customer_sort)
                                                    et_json_del     = DATA(gt_json_del)
                                                    et_customer_del = DATA(gt_customer_sort_del) ).
  ELSE.
    lo_customer_data->get_customer_all( EXPORTING
                                          is_sel_opt     = gs_sel_opt
                                          io_log         = lo_log
                                        IMPORTING
                                          et_json        = gt_json
                                          et_customer    = gt_customer_sort
                                          et_json_del    = gt_json_del
                                          et_customer_del = gt_customer_sort_del ).

  ENDIF.

  IF pext = abap_true.
    lo_customer_data->get_customer_ext(
      EXPORTING
        is_sel_opt     = gs_sel_opt_ext
        it_customer    = gt_customer_sort
        io_log         = lo_log
      IMPORTING
        et_customer_ext = DATA(gt_customer_ext)
        et_json         = DATA(gt_json_ext) ).
  ENDIF.

  CASE lv_mode.
    WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
      DATA(lo_sc) = NEW cl_gui_splitter_container( parent                  = cl_gui_container=>screen0
                                                   no_autodef_progid_dynnr = abap_true
                                                   columns                 = COND #( WHEN pext = abap_true
                                                                                      THEN 2
                                                                                       ELSE 1 )
                                                   rows                    = 1 ).

      gt_customer = gt_customer_sort.
      APPEND LINES OF gt_customer_sort_del TO gt_customer.

      IF pgen = abap_true.
        IF lines( gt_customer ) >= /vpcoe/cl_rdp_helper=>gc_display_amount.
          MESSAGE i065(/vpcoe/common).
          DELETE gt_customer FROM /vpcoe/cl_rdp_helper=>gc_display_amount + 1.
        ENDIF.
        lo_cust->display_selected_data(
          EXPORTING
              it_deep_data     = gt_customer
              it_columns_title = VALUE #( ( column = 'IS_MARKED_FOR_DELETION' text = 'Deletion Indicator'(TC1) )
                                          ( column = 'IS_BUSINESS_PURPOSE_COMPLETED' text = 'PurposeComplete Flag'(TC2) ) )
              iv_lable         = /vpcoe/cl_rdp_helper=>sc_service_id-customer
              is_splitter      = VALUE #( sc     = lo_sc
                                          row    = 1
                                          column = 1  ) ).
      ENDIF.

      IF pext = abap_true.
        IF lines( gt_customer_ext ) >= /vpcoe/cl_rdp_helper=>gc_display_amount.
          IF lines( gt_customer ) < /vpcoe/cl_rdp_helper=>gc_display_amount. "To avoid double message
            MESSAGE i065(/vpcoe/common).
          ENDIF.
          DELETE gt_customer_ext FROM /vpcoe/cl_rdp_helper=>gc_display_amount + 1.
        ENDIF.

        lo_cust->display_selected_data(
          EXPORTING
            it_deep_data = gt_customer_ext
            iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-customer_ext
            is_splitter = VALUE #( sc     = lo_sc
                                   row    = 1
                                   column = 2  ) ).
      ENDIF.
      WRITE: ' '.

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
      IF p_file IS INITIAL.
        MESSAGE e064(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        IF pext = abap_true.
          go_log_ext->add_sy_msg( ).
        ENDIF.
        IF pgen = abap_true.
          lo_log->add_sy_msg( ).
        ENDIF.
      ELSEIF r_server = abap_false AND sy-batch = abap_true.
        MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        IF pext = abap_true.
          go_log_ext->add_sy_msg( ).
        ENDIF.
        IF pgen = abap_true.
          lo_log->add_sy_msg( ).
        ENDIF.
      ELSE.
        DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path = CONV #( p_file )
                                                       iv_save_background = COND #( WHEN r_server = abap_true
                                                                                        THEN abap_true
                                                                                          ELSE abap_false )   ).
        IF pgen = abap_true.
          lo_customer_data->download_excel(
            EXPORTING
              iv_service_id = /vpcoe/cl_rdp_helper=>sc_service_id-customer
              iv_file_path  = CONV #( p_file )
              iv_save_background = COND #( WHEN r_server = abap_true THEN abap_true
                                                                ELSE abap_false )
              io_log = lo_log ).
          IF NOT lo_log->check( ).
            MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            lo_log->add_sy_msg( ).
          ENDIF.
        ENDIF.

        IF pext = abap_true.
          SPLIT p_file AT '.xls' INTO DATA(lv_name) DATA(lv_extension).
          lo_customer_data->download_excel(
                      EXPORTING
                        iv_service_id = /vpcoe/cl_rdp_helper=>sc_service_id-customer_ext
                        iv_file_path  = |{ lv_name }_ExtensionData.xls|
                        iv_save_background = COND #( WHEN r_server = abap_true THEN abap_true
                                                                          ELSE abap_false )
                        io_log = go_log_ext ).
          IF NOT go_log_ext->check( ).
            MESSAGE i069(/vpcoe/common) WITH |{ lv_name }_ExtensionData.xls| INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            go_log_ext->add_sy_msg( ).
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
      IF pgen = abap_true.
        IF gt_json IS INITIAL AND gt_json_del IS INITIAL.
          MESSAGE i015(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          lo_log->add_sy_msg( ).
        ELSE.
          go_payload_handler->send_payload(
            EXPORTING
              io_cust     = lo_cust
              it_json     = gt_json
              iv_save_log = abap_false
              io_log      = lo_log
            IMPORTING
              ev_status   = DATA(lv_status) ).

          IF gt_json_del IS NOT INITIAL.
            MESSAGE i070(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            lo_log->add_sy_msg( ).
            go_payload_handler->send_payload(
              EXPORTING
                io_cust     = lo_cust
                it_json     = gt_json_del
                iv_save_log = abap_false
                io_log      = lo_log
              IMPORTING
                ev_status   = lv_status ).

          ENDIF.

          IF lv_status =  /vpcoe/cl_rdp_http=>gc_s_http_status-ok.
            lo_customer_data->change_status( EXPORTING iv_test_run =  COND #( WHEN r_send = abap_true THEN abap_false
                                                                                                      ELSE abap_true ) ).
          ENDIF.

        ENDIF.
      ENDIF.
      IF pext = abap_true.
        IF gt_json_ext IS INITIAL.
          MESSAGE i015(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          go_log_ext->add_sy_msg( ).
        ELSE.

          DATA(go_cust_ext) = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                                        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
                                                        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer_ext ).

          go_payload_handler->send_payload(
            EXPORTING
              io_cust     = go_cust_ext
              it_json     = gt_json_ext
              iv_save_log = abap_false
              io_log      = go_log_ext ).

        ENDIF.
      ENDIF.
  ENDCASE.
  IF lo_rdp_api->is_suma_api_enabled( lo_cust ).
    lo_customer_data->close_replication( lo_log ).
  ENDIF.
  IF r_send  = abap_true OR r_excel = abap_true.
    lo_log->display_message( iv_with_total = COND #( WHEN r_send = abap_true THEN abap_true
                                                                             ELSE abap_false )
                             iv_excel      = r_excel ).

    lo_log->save( iv_no_total = abap_true ).
    go_log_ext->display_message( iv_with_total = COND #( WHEN r_send = abap_true THEN abap_true
                                                                                 ELSE abap_false )
                                 iv_excel      = r_excel ).
    go_log_ext->save( iv_no_total = abap_true ).
  ENDIF.
