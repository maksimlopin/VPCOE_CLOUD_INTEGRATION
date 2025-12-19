*&---------------------------------------------------------------------*
*& Report  /VPCOE/DELIVERY_TO_RDP_BCKGRND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/delivery_to_rdp.

TABLES: adrc, likp, lips, t001w, sscrfields, vbrk, vbrp, rbkp, rseg.

DATA: lv_ucomm         TYPE sy-ucomm,
      go_cust          TYPE REF TO /vpcoe/cl_rdp_helper,
      gt_bapiret2      TYPE bapiret2_t,
      lv_scr_fld_state TYPE screen-input,
      ls_sel_opt       TYPE /vpcoe/s_selopt_delivery,
      lt_sel_params    TYPE /vpcoe/cl_rdp_helper=>gty_t_sel_opt,
      lt_sel_name      TYPE /vpcoe/cl_rdp_helper=>gty_t_sel_names,
      lt_r_sel_name    TYPE /vpcoe/tt_r_name,
      lv_status        TYPE i,
      lv_reason        TYPE string,
      lv_answer        TYPE char3,
      lt_selid_vrm     TYPE vrm_values,
      lv_question      TYPE char255,
      lv_option        TYPE char2,
      lt_delivery_xls  TYPE /vpcoe/t_delivery_hdr,
      gv_default       TYPE string VALUE /vpcoe/cl_rdp_helper=>sc_service_id-delivery,
      gt_billdi        TYPE /vpcoe/t_billdi_data.

FIELD-SYMBOLS:  <lv_value>      TYPE any,
                <lt_value>      TYPE STANDARD TABLE,
                <ls_sel_opt_id> TYPE /vpcoe/de_sel_name.

INCLUDE /vpcoe/delivery_to_rdp_selscr.

START-OF-SELECTION.
  DATA(lv_mode) = go_cust->set_mode( iv_screen = r_screen
                                     iv_excel  = r_excel ).
  DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-delivery
                                        iv_mode       = lv_mode ).
  DATA(lo_log_bildoc) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-billdocit
                                               iv_mode       = lv_mode ).

  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    lo_log->add_sy_msg( ).
    lo_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  IF r_send <> abap_true.
    p_sbild = abap_false.
    p_sdlv = abap_false.
  ENDIF.

  IF p_delta = abap_true AND p_pselid IS INITIAL.
    IF sy-batch = space.
      MESSAGE i074(/vpcoe/common).
    ELSE.
      MESSAGE i074(/vpcoe/common) INTO DATA(lv_message).
      lo_log->add_sy_msg( ).
    ENDIF.
    IF r_send = abap_true.
      lo_log->save( ).
    ENDIF.
    RETURN.
  ENDIF.

  IF p_sndc = abap_false AND p_sdlv = abap_false AND p_sbild = abap_false AND r_send = abap_true.
    IF sy-batch = space.
      MESSAGE i047(/vpcoe/common) DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
      MESSAGE i047(/vpcoe/common) INTO lv_message.
      lo_log->add_sy_msg( ).
      lo_log->save( ).
      RETURN.
    ENDIF.
  ENDIF.

  IF lv_ucomm = 'CBAC'.
    RETURN.
  ENDIF.

*  DATA(lo_delivery_cust_data) = NEW /vpcoe/cl_rdp_delivery_data( iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                 iv_package_size = go_cust->get_package_size( )
*                                                                 iv_source       = /vpcoe/cl_rdp_helper=>get_source_id( )
*                                                                 iv_mode         = lv_mode
*                                                                 io_log          = lo_log ).
**
*  DATA(lo_cust_customer) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                     iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
*                                                     iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer ).
*,
  DATA(lo_cust_customer) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                      iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
                                                      iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer ).
  DATA(lo_customer_data) = NEW /vpcoe/cl_rdp_customer_data( iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                            iv_package_size = lo_cust_customer->get_package_size( )
                                                            iv_source       = /vpcoe/cl_rdp_helper=>get_source_id( ) ).

  DATA(lo_rdp_api) = /vpcoe/cl_rdp_api_instance=>get_instance( ).
  DATA(lo_delivery_cust_data) = lo_rdp_api->get_delivery_handler( io_rdp_helper = lo_cust_customer
                                                                  io_log        = lo_log
                                                                  iv_mode =  lv_mode ).

  ls_sel_opt-document_type              = so_type[].
  ls_sel_opt-sales_org                  = so_org[].
  ls_sel_opt-ship_to_party              = so_kunnr[].
  ls_sel_opt-country                    = so_cntry[].
  ls_sel_opt-region                     = so_reg[].
  ls_sel_opt-crt_date                   = so_crt[].
  ls_sel_opt-chng_date                  = so_chng[].
  ls_sel_opt-division                   = so_divis[].
  ls_sel_opt-category                   = so_categ[].
  ls_sel_opt-distribution               = so_distr[].
  ls_sel_opt-vbeln                      = so_vbeln[].
  ls_sel_opt-actual_goods_movement_date = so_wadat[].
  ls_sel_opt-plnt_country               = so_pcntr[].
  ls_sel_opt-matnr                      = so_matnr[].
  ls_sel_opt-billdoc                    = so_bildi[].
  ls_sel_opt-bdocdate                   = so_bdat[].
  ls_sel_opt-bsddoccat                  = so_bsddc[].
  ls_sel_opt-billdi_country             = so_cntrb[].
  ls_sel_opt-get_billdi                 = p_bildi.
  ls_sel_opt-send_delivery              = p_sdlv.
  ls_sel_opt-send_bil_doc               = p_sbild.

ENHANCEMENT-POINT /vpcoe/delivery_set_data SPOTS /vpcoe/enh_delivery_bck .

  CASE lv_mode.
    WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
      DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path = CONV #( p_file )
                                                     iv_save_background = COND #( WHEN r_server = abap_true THEN abap_true
                                                                                                             ELSE abap_false ) ).

      IF p_file IS NOT INITIAL.
        IF r_server = abap_false AND sy-batch = abap_true.
          MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          lo_log->add_sy_msg( ).
        ELSE.
          lo_delivery_cust_data->get_data_for_xls(
            EXPORTING
              is_sel_opt       = ls_sel_opt
              io_log           = lo_log
              io_log_bil_doc   = lo_log_bildoc
            IMPORTING
              et_delivery      = DATA(lt_delivery_doc)
              et_delivery_item = DATA(lt_items)
              et_billdi        = gt_billdi ).

          lo_delivery_cust_data->get_excel_header(
            IMPORTING
              ev_header      = DATA(lv_header)
              ev_item        = DATA(lv_header_itm)
              ev_code_list   = DATA(lv_code_list)
              et_header_itm  = DATA(lt_header_itm)
              ev_header_list = DATA(lv_header_list)
              et_header      = DATA(lt_header)
              et_header_list = DATA(lt_header_list)
              et_code_list   = DATA(lt_code_list)
              et_code_list_deliveryitem   = DATA(lt_code_list_deliveryitem)
              ev_code_list_deliveryitem   = DATA(lv_code_list_deliveryitem)
              ev_header_list_deliveryitem = DATA(lv_header_list_deliveryitem) ).

          lo_xls_file->execute( iv_name = lv_header ).
          lo_xls_file->fill_data( it_item_tab = lt_delivery_doc
                                  it_title    = lt_header ).

          MOVE-CORRESPONDING lt_delivery_doc TO lt_delivery_xls.

          lo_xls_file->execute( iv_name = lv_header_itm ).
          lo_xls_file->fill_data( it_item_tab = lt_items
                                  it_title    = lt_header_itm ).

          lo_xls_file->execute( iv_name     = lv_code_list ).
          lo_xls_file->fill_data( it_item_tab = lt_code_list
                                  it_title    = lt_header_list
                                  iv_code_list = lv_header_list
                                  iv_style     = /vpcoe/cl_xls_handler=>sc_styles-code_list_style ).

          lo_xls_file->execute( iv_name     = lv_code_list_deliveryitem ).
          lo_xls_file->fill_data( it_item_tab  = lt_code_list_deliveryitem
                                  it_title     = lt_header_list
                                  iv_code_list = lv_header_list_deliveryitem
                                  iv_style     = /vpcoe/cl_xls_handler=>sc_styles-code_list_style ).

          lo_xls_file->save_xls_file( lo_log ).

          IF NOT lo_log->check( ).
            MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            lo_log->add_sy_msg( ).
          ENDIF.

          IF p_bildi = abap_true.
            SPLIT p_file AT 'Delivery.xls' INTO DATA(lv_name) DATA(lv_invoice).

            DATA(lo_xls_file_billdoc) = NEW /vpcoe/cl_xls_handler( iv_path            = |{ lv_name }BillingDocumentItem.xls|
                                                                   iv_save_background = COND #( WHEN r_server = abap_true
                                                                                                THEN abap_true
                                                                                                ELSE abap_false )   ).
            /vpcoe/cl_rdp_billdocit_data=>download_excel(
               EXPORTING
                 io_log         = lo_log_bildoc
                 it_bill_doc    = gt_billdi
               CHANGING
                 co_xls_handler = lo_xls_file_billdoc ).

            lo_xls_file_billdoc->save_xls_file( lo_log_bildoc ).
            IF NOT lo_log_bildoc->check( ).
              MESSAGE i069(/vpcoe/common) WITH |{ lv_name }BillingDocumentItem.xls| INTO /vpcoe/cl_rdp_log=>sv_msg_text.
              lo_log_bildoc->add_sy_msg( ).
            ENDIF.
            lo_log_bildoc->display_message( iv_with_total = abap_true iv_excel = abap_true ).
            lo_log_bildoc->save( iv_no_total = abap_true ).
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE e064(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        lo_log->add_sy_msg( ).
      ENDIF.

      lo_log->display_message( iv_with_total = abap_false iv_excel = abap_true ).
      lo_log->save( iv_no_total = abap_true ).

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen
      OR /vpcoe/cl_rdp_helper=>sc_mode-send.

      IF lv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND p_sndc = abap_true.
        go_cust->get_service_url( IMPORTING et_bapiret2 = gt_bapiret2 ).
        lo_log->add_bapiret( EXPORTING it_bapiret2_t = gt_bapiret2 ).

        DATA(lv_customer_url) = lo_cust_customer->get_service_url( IMPORTING et_bapiret2 = gt_bapiret2 ).
        lo_log->add_bapiret( EXPORTING it_bapiret2_t = gt_bapiret2 ).

        IF lo_log->check( ).
          IF sy-batch = space.
            lo_log->display_message( iv_with_total = abap_false ).
          ENDIF.
          lo_log->save( iv_no_total = abap_true ).
          RETURN.
        ENDIF.

        lo_customer_data->get_customer_delta(
           EXPORTING
             iv_chng_pointer_id = p_cchp
             io_log             = lo_log
           IMPORTING
             et_customer        = DATA(lt_changed_customer) ).

        lo_delivery_cust_data->set_changed_customer( lt_changed_customer ).
      ENDIF.

      lo_delivery_cust_data->get_delivery(
        EXPORTING
          iv_source      = /vpcoe/cl_rdp_helper=>get_source_id( )
          io_cust        = go_cust
          is_sel_opt     = ls_sel_opt
          io_log         = lo_log
          io_log_billdoc = lo_log_bildoc
        IMPORTING
          ev_failed      = DATA(lv_failed)
          et_delivery    = DATA(lt_delivery)
          ev_no_data     = DATA(lv_no_data)
          et_billdi      = gt_billdi
        CHANGING
          ct_total_count = lo_log->mt_sum
          ct_total_count_billdoc = lo_log_bildoc->mt_sum ).

      CASE lv_mode.
        WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
          IF lines( lt_delivery ) >= /vpcoe/cl_rdp_helper=>gc_display_amount.
            MESSAGE i065(/vpcoe/common).
            DELETE lt_delivery FROM /vpcoe/cl_rdp_helper=>gc_display_amount + 1.
          ENDIF.

          go_cust->display_selected_data(
              EXPORTING
                  it_deep_data = lt_delivery
                  iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-delivery
                  it_columns_title = VALUE #( ( column = 'SHIP_TO_PARTY' text = 'Ship to Party' )
                                              ( column = 'SOLD_TO_PARTY' text = 'Sold to Party' ) ) ).
          IF p_bildi = abap_true.
            go_cust->display_selected_data(
              EXPORTING
                it_deep_data = gt_billdi
                iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-billdocit ).
          ENDIF.
        WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
          IF p_sdlv = abap_true.
            IF lv_no_data = abap_true.
              MESSAGE i015(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
              lo_log->add_sy_msg( ).
            ENDIF.
            IF lv_failed = abap_false.
              INSERT VALUE #( field = 'SO_TYPE'  value = REF #( so_type[] ) )  INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_ORG'   value = REF #( so_org[] ) )   INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_KUNNR' value = REF #( so_kunnr[] ) ) INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_CNTRY' value = REF #( so_cntry[] ) )   INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_REG'   value = REF #( so_reg[] ) )     INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_CATEG' value = REF #( so_categ[] ) )   INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_DISTR' value = REF #( so_distr[] ) )   INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_DIVIS' value = REF #( so_divis[] ) )   INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_WADAT' value = REF #( so_wadat[] ) )   INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_PCNTR' value = REF #( so_pcntr[] ) )  INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_VBELN' value = REF #( so_vbeln[] ) )  INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'P_SNDC'   value = REF #( p_sndc ) ) INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'P_CCHP'   value = REF #( p_cchp ) ) INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_CNTRB' value = REF #( so_cntrb[] ) ) INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_BILDI' value = REF #( so_bildi[] ) ) INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_BDAT'  value = REF #( so_bdat[] ) ) INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'SO_BSDDC' value = REF #( so_bsddc[] ) ) INTO TABLE lt_sel_params.
              INSERT VALUE #( field = 'P_BILDI' value = REF #( p_bildi ) ) INTO TABLE lt_sel_params.

ENHANCEMENT-POINT /vpcoe/delivery_save_sel_opt SPOTS /vpcoe/enh_delivery_bck .

              go_cust->save_delta_sel_opt( iv_sel_name = COND #( WHEN p_delta = abap_true THEN p_pselid
                                                                                          ELSE p_cselid )
                                           iv_test_run = abap_false
                                           it_sel_params  = lt_sel_params ).
            ELSE.
              MESSAGE i015(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
              lo_log->add_sy_msg( ).
            ENDIF.
          ENDIF.
          IF lo_rdp_api->is_suma_api_enabled( lo_cust_customer ).
            lo_delivery_cust_data->close_replication( lo_log ).
          ENDIF.

          lo_log->display_message(
            EXPORTING iv_test_mode = COND #( WHEN r_send = abap_true THEN abap_false ELSE abap_true ) ).
          lo_log->save( ).

          IF p_bildi = abap_true.
            lo_log_bildoc->display_message(
              EXPORTING iv_test_mode = COND #( WHEN r_send = abap_true THEN abap_false ELSE abap_true ) ).

            lo_log_bildoc->save( ).
          ENDIF.

      ENDCASE.
  ENDCASE.
