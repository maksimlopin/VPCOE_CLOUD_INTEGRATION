*&---------------------------------------------------------------------*
*& Report  /vpcoe/org_data_to_rdp
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/org_data_to_rdp.

DATA: gt_json	    TYPE /vpcoe/cl_rdp_http=>gty_t_json,
      lv_ucomm    TYPE sy-ucomm,
      gv_api_type TYPE /vpcoe/de_api_type,
      gv_error    TYPE flag VALUE abap_false,
      gv_default  TYPE string VALUE /vpcoe/cl_rdp_helper=>sc_service_id-org.

CONSTANTS: gv_cmd_all_const TYPE char8 VALUE 'CMD_ALL',
           gc_et            TYPE char2 VALUE 'ET'.

INCLUDE /vpcoe/version.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS:
  r_screen RADIOBUTTON GROUP req USER-COMMAND et,
  r_send   RADIOBUTTON GROUP req,
  r_excel  RADIOBUTTON GROUP req.
INCLUDE /vpcoe/i_excel_selscr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK ball WITH FRAME TITLE text-004.
PARAMETERS:
  pall     TYPE xfeld AS CHECKBOX USER-COMMAND cmd_all. "Select all
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  pccode TYPE xfeld AS CHECKBOX,                          "Company Code
  pplant TYPE xfeld AS CHECKBOX,                          "Plant
  psorg  TYPE xfeld AS CHECKBOX,                          "Sales Organization
  pdistr TYPE xfeld AS CHECKBOX,                          "Distribution Channel
  pdvsn  TYPE xfeld AS CHECKBOX,                          "Division
  psalar TYPE xfeld AS CHECKBOX.                          "Sales Area
SELECTION-SCREEN END OF BLOCK ball.

INCLUDE /vpcoe/i_excel_valreq.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN gv_cmd_all_const.
      pccode = psorg = pplant = pdistr = pdvsn = psalar = pall.
    WHEN gc_et.
      pccode = psorg = pplant = pdistr = pdvsn = psalar = pall = abap_true.
  ENDCASE .

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name CS 'PRDP'.
      screen-active = 1.
      MODIFY SCREEN.

    ELSEIF screen-name CS 'PSORG'
        OR screen-name CS 'PDISTR'
        OR screen-name CS 'PDVSN'
        OR screen-name CS 'PSALAR'.

      screen-input = COND #( WHEN r_excel = abap_true THEN 0
                                                      ELSE 1 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'PCCODE'
        OR screen-name CS 'PPLANT'
        OR screen-name CS 'PALL'.

      screen-input = COND #( WHEN r_excel = abap_true THEN 0
                                                      ELSE 1 ).
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
  INCLUDE /vpcoe/version_set.
  INCLUDE /vpcoe/i_excel_scrout.

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_ORG_DATA' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.


START-OF-SELECTION.

  IF pccode = abap_false AND pplant = abap_false AND psorg = abap_false AND pdistr = abap_false AND pdvsn = abap_false AND psalar = abap_false.
    gv_error = abap_true.
  ENDIF.

  DATA(lv_mode) = /vpcoe/cl_rdp_helper=>set_mode( iv_screen = r_screen
                                                  iv_excel  = r_excel ).

  DATA(go_log)  = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-organization
                                         iv_mode       = lv_mode ).

  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    go_log->add_sy_msg( ).
    go_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  IF gv_error = abap_true.
    IF sy-batch = space.
      MESSAGE i028(/vpcoe/common).
    ELSE.
      MESSAGE i028(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
      go_log->save( ).
      RETURN.
    ENDIF.
  ENDIF.

  gv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp.

  DATA(lo_rdp_api) = /vpcoe/cl_rdp_api_instance=>get_instance( ).

  "Company Code
  IF pccode = abap_true.
    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                              iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                              iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-company_code ).

    DATA(lo_org_data) = lo_rdp_api->get_org_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_org_data->get_company_code( EXPORTING io_cust     = lo_cust
                                   IMPORTING et_json     = gt_json
                                             et_company_code = DATA(gt_company_code) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_org_data->send( EXPORTING io_cust   = lo_cust
                                     it_json   = gt_json ).
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
          EXPORTING
            it_deep_data = gt_company_code
            iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-company_code ).
    ENDCASE.
  ENDIF.

  "Sales Organization
  IF psorg = abap_true.
    "only rdp
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-sales_organization ).

    lo_org_data = lo_rdp_api->get_org_data_handler( io_rdp_helper = lo_cust
                                                    iv_mode       = lv_mode
                                                    io_log        = go_log ).

    lo_org_data->get_sales_org( EXPORTING io_cust      = lo_cust
                                IMPORTING et_json      = gt_json
                                          et_sales_org = DATA(gt_sales_org) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
          EXPORTING
            it_deep_data = gt_sales_org
            iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-sales_organization ).
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_org_data->send( EXPORTING io_cust = lo_cust
                                     it_json = gt_json ).
    ENDCASE.
  ENDIF.

  "Plant
  IF pplant = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp   = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-plant ).

    lo_org_data = lo_rdp_api->get_org_data_handler( io_rdp_helper = lo_cust
                                                    iv_mode       = lv_mode
                                                    io_log        = go_log ).

    lo_org_data->get_plant( EXPORTING io_cust  = lo_cust
                            IMPORTING et_json  = gt_json
                                      et_plant = DATA(gt_plant)
                                      et_bapiret2 = DATA(gt_message) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_org_data->send( EXPORTING io_cust   = lo_cust
                                     it_json   = gt_json
                                     it_bapiret2 = gt_message ).
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_plant
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-plant ).
    ENDCASE.
  ENDIF.

  "Distribution Channel
  IF pdistr = abap_true."AND prdp = abap_true.
    "only rdp
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-distribution_channel ).

    lo_org_data = lo_rdp_api->get_org_data_handler( io_rdp_helper = lo_cust
                                                    iv_mode       = lv_mode
                                                    io_log        = go_log ).

    lo_org_data->get_distr_channel( EXPORTING io_cust          = lo_cust
                                    IMPORTING et_json          = gt_json
                                              et_distr_channel = DATA(gt_distr_channel) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_distr_channel
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-distribution_channel ).
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_org_data->send( EXPORTING io_cust = lo_cust
                                     it_json = gt_json ).
    ENDCASE.
  ENDIF.

  "Division
  IF pdvsn = abap_true." AND prdp = abap_true.
    "only rpd
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-division ).

    lo_org_data = lo_rdp_api->get_org_data_handler( io_rdp_helper = lo_cust
                                                    iv_mode       = lv_mode
                                                    io_log        = go_log ).

    lo_org_data->get_division( EXPORTING io_cust     = lo_cust
                               IMPORTING et_json     = gt_json
                                         et_division = DATA(gt_division) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
          EXPORTING
            it_deep_data = gt_division
            iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-division ).
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_org_data->send( EXPORTING io_cust = lo_cust
                                     it_json = gt_json ).
    ENDCASE.
  ENDIF.

  "Sales Area
  IF psalar = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-sales_area ).

    lo_org_data = lo_rdp_api->get_org_data_handler( io_rdp_helper = lo_cust
                                                    iv_mode       = lv_mode
                                                    io_log        = go_log ).

    lo_org_data->get_sales_area( EXPORTING io_cust       = lo_cust
                                 IMPORTING et_json       = gt_json
                                           et_sales_area = DATA(gt_salea_area) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_org_data->send( EXPORTING io_cust   = lo_cust
                                     it_json   = gt_json ).
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
          EXPORTING
            it_deep_data = gt_salea_area
            iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-sales_area ).
    ENDCASE.
  ENDIF.

  IF lv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send.
    " Close Replication
    IF lo_rdp_api->is_suma_api_enabled( lo_cust ).
      DATA(lo_suma_helper) = /vpcoe/cl_rdp_suma_helper=>get_instance( EXPORTING io_rdp_helper = lo_cust
                                                                                iv_srv_prfx   = 'org_data' ).
      " Close Replication
      IF go_log IS BOUND AND go_log->check( ).
        lo_suma_helper->cancel_replication( io_log = go_log ).
      ELSE.
        lo_suma_helper->finish_replication( io_log = go_log ).
      ENDIF.
    ENDIF.

  ENDIF.

  IF r_excel = abap_true.
    IF r_server = abap_false AND sy-batch = abap_true.
      MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
    ELSE.
      IF p_file IS NOT INITIAL.
        lo_org_data->download_excel( iv_file_path = CONV #( p_file )
                                     iv_save_background = COND #( WHEN r_server = abap_true THEN abap_true
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

    go_log->display_message( iv_with_total = abap_true iv_excel = abap_true ).
    IF NOT go_log->check( ).
      MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
    ENDIF.
    go_log->save( iv_no_total = abap_true ).
  ENDIF.
*  ENDDO.
