*&---------------------------------------------------------------------*
*& Report  /VPCOE/CONFIGURARION_TO_RDP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/configuration_to_rdp.

CONSTANTS: gc_cmd_all_const TYPE char8 VALUE 'CMD_ALL',
           gc_mode          TYPE char10 VALUE 'MODE'.

DATA: lv_ucomm    TYPE sy-ucomm,
      gt_json     TYPE /vpcoe/cl_rdp_http=>gty_t_json,
      gv_api_type TYPE /vpcoe/de_api_type,
      gv_error    TYPE flag VALUE abap_false,
      go_log      TYPE REF TO /vpcoe/cl_rdp_log,
      gv_default  TYPE string.

FIELD-SYMBOLS: <ls_sel_opt_id> TYPE /vpcoe/de_sel_name.

INCLUDE /vpcoe/version.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:
  r_screen RADIOBUTTON GROUP req USER-COMMAND mode,
  r_send   RADIOBUTTON GROUP req,
  r_excel  RADIOBUTTON GROUP req.
INCLUDE /vpcoe/i_excel_selscr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK ball WITH FRAME TITLE text-002.
PARAMETERS:
pall     TYPE xfeld AS CHECKBOX USER-COMMAND cmd_all. "Select all
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  puomd   TYPE xfeld AS CHECKBOX,                      "Unit of Measure Dimensions
  puom    TYPE xfeld AS CHECKBOX,                      "Unit of Measure
  puomiso TYPE xfeld AS CHECKBOX,                      "Unit of Measure ISO Code
  pcurr   TYPE xfeld AS CHECKBOX,                      "Currency
  pcntr   TYPE xfeld AS CHECKBOX,                      "Country
  prgn    TYPE xfeld AS CHECKBOX,                      "Region
  pprdty  TYPE xfeld AS CHECKBOX,                      "Product Type
  prpdgr  TYPE xfeld AS CHECKBOX,                      "Product Group
  prpdhr  TYPE xfeld AS CHECKBOX,                      "Product Hierarcy
  pddt    TYPE xfeld AS CHECKBOX,                      "Delivery Document Type
  pddic   TYPE xfeld AS CHECKBOX,                      "Delivery Document Item Category
  pmvmt   TYPE xfeld AS CHECKBOX.                      "Movement Type
SELECTION-SCREEN END OF BLOCK ball.

SELECTION-SCREEN BEGIN OF SCREEN 200.
SELECTION-SCREEN PUSHBUTTON 15(10) but1 USER-COMMAND def.
SELECTION-SCREEN END OF SCREEN 200.

INCLUDE /vpcoe/i_excel_valreq.

AT SELECTION-SCREEN.

  CASE sy-ucomm.

    WHEN gc_cmd_all_const.
      puomd = puom = puomiso = pcurr = pcntr = prgn = pprdty = prpdgr = pddt = pddic = pmvmt = prpdhr = pall.
    WHEN gc_mode.
      IF r_excel = abap_true.
        puomd = puom = puomiso = pcurr = pcntr = prgn = pprdty = prpdgr = pddt = pddic = pmvmt = prpdhr = pall = abap_true.
      ENDIF.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF screen-name CS 'PRDP'.
      "check rdp exist
      screen-active = 1.
      MODIFY SCREEN.

    ELSEIF screen-name CS 'PUOMISO'
        OR screen-name CS 'PDDT'
        OR screen-name CS 'PDDIC'
        OR screen-name CS 'PMVMT'
        OR screen-name CS 'PCURR'
        OR screen-name CS 'PRPDHR'.

      screen-input = COND #( WHEN r_excel = abap_true THEN 0
                                                      ELSE 1 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'PUOMD'
        OR screen-name CS 'PUOM'
        OR screen-name CS 'PRGN'
        OR screen-name CS 'PCNTR'
        OR screen-name CS 'PPRDTY'
        OR screen-name CS 'PRPDGR'
        OR screen-name CS 'PALL'.

      screen-input = COND #( WHEN r_excel = abap_true THEN 0
                                                      ELSE 1 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'R_SCREEN'
        OR screen-name CS 'R_SEND'
        OR screen-name CS 'R_EXCEL'.

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
                                                      ELSE '/VPCOE/RDP_CONF' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.

  go_log  = NEW #( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-configuration ).
  gv_default = CONV #( /vpcoe/cl_rdp_helper=>sc_service_id-conf ).

START-OF-SELECTION.

  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    go_log->add_sy_msg( ).
    go_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  IF puomd = abap_false AND puom = abap_false AND puomiso = abap_false AND pcurr = abap_false AND pcntr = abap_false AND prgn = abap_false
     AND pprdty = abap_false AND  prpdgr = abap_false AND pddt = abap_false AND pddic = abap_false AND pmvmt = abap_false AND prpdhr = abap_false.
    gv_error = abap_true.
  ENDIF.

  IF gv_error = abap_true.
    IF sy-batch = space.
      MESSAGE i028(/vpcoe/common).
    ELSE.
      MESSAGE i028(/vpcoe/common) INTO DATA(lv_message).
      go_log->add_sy_msg( ).
    ENDIF.
    IF r_send = abap_true.
      go_log->save( ).
    ENDIF.
    RETURN.
  ENDIF.

  IF lv_ucomm = 'CBAC'.
    RETURN.
  ENDIF.

  gv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp.

  DATA(lv_mode) = /vpcoe/cl_rdp_helper=>set_mode( iv_screen = r_screen
                                                  iv_excel  = r_excel ).

  DATA(lo_rdp_api) = /vpcoe/cl_rdp_api_instance=>get_instance( ).

*  Unit of Measure Dimensions
  IF puomd = abap_true.
    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                              iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                              iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-uom_dimension ).

    DATA(lo_config_data) = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                                iv_mode       = lv_mode
                                                                io_log        = go_log ).

    lo_config_data->get_uom_dimension( IMPORTING et_json  = gt_json
                                                 et_uom_d = DATA(gt_uom_d) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data     = gt_uom_d
          it_columns_title = VALUE #( ( column = 'HAS_UNITSWITH_TEMPERATURE_SPEC' text = 'Unit. Temperature' )
                                      ( column = 'HAS_UNITSWITH_PRESSURE_SPEC' text = 'Unit Switch Pressure' ) )
          iv_lable         = /vpcoe/cl_rdp_helper=>sc_service_id-uom_dimension ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.

        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).

    ENDCASE.

  ENDIF.

*  Unit of Measure
  IF puom = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-uom ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_uom( IMPORTING et_json = gt_json
                                       et_uom  = DATA(gt_uom) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_uom
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-uom ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).

    ENDCASE.
  ENDIF.

*  Unit of Measure ISO Code
  IF puomiso = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                                iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                                iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-uom_code ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_uom_iso_code( IMPORTING et_json    = gt_json
                                                et_iso_uom = DATA(gt_iso_uom) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_iso_uom
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-uom_code ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).

    ENDCASE.
  ENDIF.

*  Currency
  IF pcurr = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-currency ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_currency( IMPORTING et_json     = gt_json
                                            et_currency = DATA(gt_currency) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_currency
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-currency ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).

    ENDCASE.
  ENDIF.

*  Country
  IF pcntr = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-country ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_country( IMPORTING et_json    = gt_json
                                           et_country = DATA(gt_country) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
          EXPORTING
            it_deep_data     = gt_country
            it_columns_title = VALUE #( ( column = 'ISEUROPEANUNIONMEMBER' text = 'EU Member' ) )
            iv_lable         = /vpcoe/cl_rdp_helper=>sc_service_id-country ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).

    ENDCASE.
  ENDIF.

*  Region
  IF prgn = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-region ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_region( IMPORTING et_json   = gt_json
                                          et_region = DATA(gt_region) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_region
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-region ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).

    ENDCASE.
  ENDIF.

*  Product Type
  IF pprdty = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-product_type ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_product_type( IMPORTING et_json         = gt_json
                                                et_product_type = DATA(gt_product_type) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_product_type
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-product_type ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).

    ENDCASE.
  ENDIF.

*  Product Group
  IF prpdgr = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-product_group ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_product_group( IMPORTING et_json          = gt_json
                                                 et_product_group = DATA(gt_product_group) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_product_group
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-product_group ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).
    ENDCASE.
  ENDIF.

*  Product Hierarchy
  IF prpdhr = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-product_hierarchy ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_product_hierarchy( IMPORTING et_json              = gt_json
                                                     et_product_hierarchy = DATA(gt_product_hierarchy) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_product_hierarchy
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-product_hierarchy ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).
    ENDCASE.
  ENDIF.

*  Delivery Document Type
  IF pddt = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-delivery_doc_type ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_dlvr_doc_type( IMPORTING et_json          = gt_json
                                                 et_dlvr_doc_type = DATA(gt_dlvr_doc_type) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_dlvr_doc_type
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-delivery_doc_type ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).
    ENDCASE.
  ENDIF.

*  Delivery Document Item Category
  IF pddic = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-delivery_doc_item_cat ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_dlvr_doc_item_cat( IMPORTING et_json              = gt_json
                                                     et_dlvr_doc_item_cat = DATA(gt_dlvr_doc_item_cat) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_dlvr_doc_item_cat
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-delivery_doc_item_cat ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).
    ENDCASE.
  ENDIF.

*  Movement Type
  IF pmvmt = abap_true.
    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                        iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                        iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-movement_type ).

    lo_config_data = lo_rdp_api->get_config_data_handler( io_rdp_helper = lo_cust
                                                          iv_mode       = lv_mode
                                                          io_log        = go_log ).

    lo_config_data->get_movement_type( IMPORTING et_json          = gt_json
                                                 et_movement_type = DATA(gt_movement_type) ).
    CASE lv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
        lo_cust->display_selected_data(
        EXPORTING
          it_deep_data = gt_movement_type
          iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-movement_type ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        lo_config_data->send( EXPORTING iv_srv_id = lo_cust->get_srv_id( )
                                        it_json   = gt_json
                                        io_cust   = lo_cust ).
    ENDCASE.
  ENDIF.

  IF lv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send.
    " Close Replication
    IF lo_rdp_api->is_suma_api_enabled( lo_cust ).
      DATA(lo_suma_helper) = /vpcoe/cl_rdp_suma_helper=>get_instance( EXPORTING io_rdp_helper = lo_cust
                                                                                iv_srv_prfx   = 'config' ).
      " Close Replication
      IF go_log IS BOUND AND go_log->check( ).
        lo_suma_helper->cancel_replication( io_log = go_log ).
      ELSE.
        lo_suma_helper->finish_replication( io_log = go_log ).
      ENDIF.
    ENDIF.

  ENDIF.

  IF r_excel = abap_true.
    IF p_file IS NOT INITIAL.
      IF r_server = abap_false AND sy-batch = abap_true.
        MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        go_log->add_sy_msg( ).
        go_log->save( iv_no_total = abap_true ).
        RETURN.
      ENDIF.
      lo_config_data->download_excel( iv_file_path = CONV #( p_file )
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
    go_log->display_message( iv_with_total = abap_true iv_excel = abap_true ).
    IF NOT go_log->check( ).
      MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
    ENDIF.
    go_log->save( iv_no_total = abap_true ).
  ENDIF.
