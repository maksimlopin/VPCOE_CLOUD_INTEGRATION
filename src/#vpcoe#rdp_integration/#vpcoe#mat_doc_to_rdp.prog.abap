*&---------------------------------------------------------------------*
*& Report  /VPCOE/MAT_DOC_TO_RDP_BCKGRND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/mat_doc_to_rdp.
INCLUDE /vpcoe/mat_doc_to_rdp_top.
INCLUDE /vpcoe/mat_doc_to_rdp_selscr.

START-OF-SELECTION.

  DATA(lv_mode) = go_cust->set_mode( iv_screen = r_screen
                                     iv_excel  = r_excel ).

  DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc
                                        iv_mode       = lv_mode ).
  DATA(lo_log_billdoc) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-billdocit
                                                iv_mode       = lv_mode ).
  DATA(lo_log_suppnv) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-supinvi
                                               iv_mode       = lv_mode ).

  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    lo_log->add_sy_msg( ).
    lo_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  IF r_screen = abap_true OR r_excel = abap_true.
    p_supis = abap_false.
    p_bilds = abap_false.
    p_mtrl = abap_false.
  ENDIF.

  IF p_gr_pr = abap_false
     AND p_gi_pr = abap_false
     AND p_gr_st = abap_false
     AND p_gi_st = abap_false
     AND p_gr_poi = abap_false
     AND p_gr_po = abap_false
     AND p_gi_soe = abap_false
     AND p_gi_so = abap_false
     AND p_gr_std = abap_false
     AND p_gi_std = abap_false.
    IF sy-batch = space.
      MESSAGE i028(/vpcoe/common) DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE i028(/vpcoe/common) INTO DATA(lv_message).
      lo_log->add_sy_msg( ).
      lo_log->save( ).
      RETURN.
    ENDIF.
  ENDIF.

  IF p_delta = abap_true AND p_pselid IS INITIAL.
    IF sy-batch = space.
      MESSAGE i074(/vpcoe/common).
    ELSE.
      MESSAGE i074(/vpcoe/common) INTO lv_message.
      lo_log->add_sy_msg( ).
    ENDIF.
    IF r_send = abap_true.
      lo_log->save( ).
    ENDIF.
    RETURN.
  ENDIF.

  DATA(lo_mat_doc_data) = NEW /vpcoe/cl_rdp_material_doc( iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                          iv_package_size = go_cust->get_package_size( )
                                                          iv_source       = go_cust->get_source_id( ) ).

  IF lv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send.
    IF p_supinv = abap_false AND p_bildi = abap_false AND p_mtrl = abap_false.
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
    DATA(gv_url) = go_cust->get_service_url( IMPORTING et_bapiret2 = gt_bapiret2 ).
    IF line_exists( gt_bapiret2[ type = 'E' ] ).
      lo_log->add_bapiret( EXPORTING it_bapiret2_t = gt_bapiret2 ).
      IF sy-batch = space.
        lo_log->display_message( ).
      ELSE.
        lo_log->save( ).
      ENDIF.
      RETURN.
    ENDIF.
  ENDIF.

  gt_variants = VALUE #(
  ( code = lo_mat_doc_data->sc_variants-gr_prod   variant = p_gr_pr )
  ( code = lo_mat_doc_data->sc_variants-gi_prod   variant = p_gi_pr )
  ( code = lo_mat_doc_data->sc_variants-gr_st_imp variant = p_gr_st )
  ( code = lo_mat_doc_data->sc_variants-gi_st_exp variant = p_gi_st )
  ( code = lo_mat_doc_data->sc_variants-gr_po_imp variant = p_gr_poi )
  ( code = lo_mat_doc_data->sc_variants-gr_po     variant = p_gr_po )
  ( code = lo_mat_doc_data->sc_variants-gi_so_exp variant = p_gi_soe )
  ( code = lo_mat_doc_data->sc_variants-gi_so     variant = p_gi_so )
  ( code = lo_mat_doc_data->sc_variants-gi_st     variant = p_gi_std )
  ( code = lo_mat_doc_data->sc_variants-gr_st     variant = p_gr_std ) ).

  gv_global_no_data = abap_true.

  gs_sel_opt-country                = so_cntry[].
  gs_sel_opt-posting_date           = so_post[].
  gs_sel_opt-material_doc_year      = so_year[].
  gs_sel_opt-werks                  = so_werks[].
  gs_sel_opt-matdocid               = so_mdoc[].
  gs_sel_opt-movement_type          = so_type[].
  gs_sel_opt-matnr                  = so_matnr[].
  gs_sel_opt-get_billdi             = p_bildi.
  gs_sel_opt-get_supinvi            = p_supinv.
  gs_sel_opt-mtart                  = so_mtart[].
  gs_sel_opt-vkorg                  = so_vkorg[].
  gs_sel_opt-billdoc                = so_bildi[].
  gs_sel_opt-bdocdate               = so_bdat[].
  gs_sel_opt-bsddoccat              = so_bsddc[].
  gs_sel_opt-mat_doc                = so_mdoc[].
  gs_sel_opt-supplier_invoice       = so_belnr[].
  gs_sel_opt-supplier_invoice_item  = so_buzei[].
  gs_sel_opt-fiscal_year            = so_gjahr[].
  gs_sel_opt-billdi_country         = so_cntrb[].
  gs_sel_opt-supinvi_country        = so_cntri[].

ENHANCEMENT-POINT /vpcoe/mat_doc_set_data SPOTS /vpcoe/enh_mat_dock_bck .
  IF lv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
    SPLIT p_file AT 'Material Documents.xls' INTO DATA(lv_name) DATA(lv_invoice).

    DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path            = CONV #( p_file )
                                                   iv_save_background = COND #( WHEN r_server = abap_true
                                                                                   THEN abap_true
                                                                                    ELSE abap_false )   ).
    DATA(lo_xls_file_inv) = NEW /vpcoe/cl_xls_handler( iv_path            = |{ lv_name }SupplierInvoiceItem.xls|
                                                       iv_save_background = COND #( WHEN r_server = abap_true
                                                                                    THEN abap_true
                                                                                    ELSE abap_false )   ).
    DATA(lo_xls_file_billdoc) = NEW /vpcoe/cl_xls_handler( iv_path            = |{ lv_name }BillingDocumentItem.xls|
                                                           iv_save_background = COND #( WHEN r_server = abap_true
                                                                                        THEN abap_true
                                                                                        ELSE abap_false )   ).
    IF r_server = abap_false AND sy-batch = abap_true.
      MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      lo_log->add_sy_msg( ).
      lo_log->save( iv_no_total = abap_true ).
      RETURN.
    ENDIF.

  ENDIF.

  LOOP AT gt_variants ASSIGNING FIELD-SYMBOL(<ls_varinat>).
    IF <ls_varinat>-variant = abap_true.
      lo_mat_doc_data->get_material_document(
        EXPORTING
          io_cust              = go_cust
          io_log               = lo_log
          is_sel_opt           = gs_sel_opt
          iv_code              = <ls_varinat>-code
          iv_mode              = lv_mode
          io_log_billdoc       = lo_log_billdoc
          io_log_supp_inv      = lo_log_suppnv
          iv_send              = p_mtrl
          iv_send_mat_doc      = p_mtrl
          iv_send_bil_doc      = p_bilds
          iv_send_sup_inv      = p_supis
        IMPORTING
          ev_failed            = DATA(lv_failed)
          ev_no_data           = DATA(lv_no_data)
          et_material_document = gt_material_document
          et_billdi            = gt_billdi
          et_supinvi           = gt_supinvi
        CHANGING
          ct_total_count         = lo_log->mt_sum
          ct_total_count_billdoc = lo_log_billdoc->mt_sum
          ct_total_count_suppinv = lo_log_suppnv->mt_sum ).

      CASE lv_mode.
        WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
          INSERT LINES OF gt_material_document INTO TABLE gt_material_document_all.
          IF lines( gt_material_document_all ) > go_cust->gc_display_amount.
            MESSAGE i065(/vpcoe/common).
            DELETE gt_material_document_all FROM go_cust->gc_display_amount.
            gv_global_no_data = abap_false.
            EXIT.
          ENDIF.

          INSERT LINES OF gt_supinvi INTO TABLE gt_supinvi_all.
          INSERT LINES OF gt_billdi  INTO TABLE gt_billdi_all.
        WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
          IF p_file IS NOT INITIAL.
            lo_mat_doc_data->download_excel(
              EXPORTING
                iv_file_path = CONV #( p_file )
                it_mat_doc   = gt_material_document
                iv_code      = <ls_varinat>-code
              CHANGING
                co_xls_handler = lo_xls_file ).
          ELSE.
            MESSAGE e064(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            lo_log->add_sy_msg( ).
          ENDIF.

      ENDCASE.

      "if we have variant where data exist lv_global_no_data = abap_false.
      IF gv_global_no_data = abap_true.
        gv_global_no_data = lv_no_data.
      ENDIF.
    ENDIF.

  ENDLOOP.

  CASE lv_mode.
    WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
      IF p_file IS NOT INITIAL.
        IF p_bildi = abap_true.
          /vpcoe/cl_rdp_billdocit_data=>download_excel(
            EXPORTING
              io_log = lo_log_billdoc
              it_bill_doc = gt_billdi
            CHANGING
              co_xls_handler = lo_xls_file_billdoc ).

          lo_xls_file_billdoc->save_xls_file( lo_log_billdoc ).

          IF NOT lo_log_billdoc->check( ).
            MESSAGE i069(/vpcoe/common) WITH |{ lv_name }BillingDocumentItem.xls| INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            lo_log_billdoc->add_msg_progress( EXPORTING iv_msg_text = /vpcoe/cl_rdp_log=>sv_msg_text ).
          ENDIF.
        ENDIF.
        IF p_supinv = abap_true.
          /vpcoe/cl_rdp_supinvi_data=>download_excel(
            EXPORTING
              io_log = lo_log_suppnv
              it_data = gt_supinvi
            CHANGING
              co_xls_handler = lo_xls_file_inv ).

          lo_xls_file_inv->save_xls_file( lo_log_suppnv ).

          IF NOT lo_log_suppnv->check( ).
            MESSAGE i069(/vpcoe/common) WITH |{ lv_name }SupplierInvoiceItem.xls| INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            lo_log_suppnv->add_msg_progress( EXPORTING iv_msg_text = /vpcoe/cl_rdp_log=>sv_msg_text ).
          ENDIF.

        ENDIF.
        lo_xls_file->save_xls_file( lo_log ).
        IF NOT lo_log->check( ).
          MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          lo_log->add_msg_progress( EXPORTING iv_msg_text = /vpcoe/cl_rdp_log=>sv_msg_text ).
        ENDIF.
      ENDIF.

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.

      IF gv_global_no_data = abap_true.
        MESSAGE i015(/vpcoe/common) INTO DATA(gv_msg).
        lo_log->add_sy_msg( ).

      ELSE.
        go_cust->display_selected_data(
          EXPORTING
            it_deep_data       = gt_material_document_all
            iv_lable           = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
            it_exclude_columns = VALUE #( ( 'LGORT' )
                                          ( 'CUSTOMER_REGION' )
                                          ( 'STORAGE_LOCATION' )
                                          ( 'ADDRESS' )
                                          ( 'VENDOR_ACC_NUMBER' )
                                          ( 'CATEGORY' )
                                          ( 'PLANT_REGION' )
                                          ( 'ISSUING_OR_RECEIVING_PLNT_REG' )
                                          ( 'ISSUING_OR_RECEIVING_PLNT_CNTR' )
                                          ( 'PLANT_COUNTRY' )
                                          ( 'SUPPLIER_COUNTRY' )
                                          ( 'CUSTOMER_COUNTRY' ) )
            it_columns_title  = VALUE #( ( column = 'IS_CANCELLED' text = 'Cancelled' ) ) ).
        IF p_bildi = abap_true.
          go_cust->display_selected_data(
            EXPORTING
              it_deep_data = gt_billdi_all
              iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-billdocit ).
        ENDIF.
        IF p_supinv = abap_true.
*          IF sy-uname = 'C5332659'.
*            DATA lt_exclude_columns TYPE /vpcoe/cl_common_helper=>gty_t_column_names.
*            lt_exclude_columns = VALUE #( ( 'REVERSAL_DOCUMENT' ) ( 'REVERSAL_DOCUMENR_YEAR' ) ).
*          ENDIF.
          go_cust->display_selected_data(
            EXPORTING
              it_deep_data = gt_supinvi_all
              iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-supinvi ).
        ENDIF.
      ENDIF.


    WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
      IF p_mtrl = abap_true.
        IF gv_global_no_data = abap_true.
          MESSAGE i015(/vpcoe/common) INTO gv_msg.
          lo_log->add_sy_msg( ).
        ELSEIF lv_failed = abap_false.
          INSERT VALUE #( field = 'SO_CNTRY' value = REF #( so_cntry[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_POST'  value = REF #( so_post[] ) )  INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_YEAR'  value = REF #( so_year[] ) )  INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_MDOC' value = REF #( so_mdoc[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_WERKS' value = REF #( so_werks[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_TYPE'  value = REF #( so_type[] ) )  INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_MATNR'  value = REF #( so_matnr[] ) )  INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_MTART'  value = REF #( so_mtart[] ) )  INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_VKORG'  value = REF #( so_vkorg[] ) )  INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GR_PR'  value = REF #( p_gr_pr ) )    INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GI_PR'  value = REF #( p_gi_pr ) )    INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GR_ST'  value = REF #( p_gr_st ) )    INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GI_ST'  value = REF #( p_gi_st ) )    INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GR_POI' value = REF #( p_gr_poi ) )   INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GR_PO'  value = REF #( p_gr_po ) )    INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GI_SOE' value = REF #( p_gi_soe ) )   INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GI_SO'  value = REF #( p_gi_so ) )    INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GI_STD' value = REF #( p_gi_std ) )   INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_GR_STD' value = REF #( p_gr_std ) )   INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_BILDI' value = REF #( p_bildi ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_SUPINV' value = REF #( p_supinv ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_BILDI' value = REF #( so_bildi[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_BDAT' value = REF #( so_bdat[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_BSDDC' value = REF #( so_bsddc[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_MDOC' value = REF #( so_mdoc[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_BELNR' value = REF #( so_belnr[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_BUZEI' value = REF #( so_buzei[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_GJAHR' value = REF #( so_gjahr[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_CNTRB' value = REF #( so_cntrb[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'SO_CNTRI' value = REF #( so_cntri[] ) ) INTO TABLE gt_sel_params.
          INSERT VALUE #( field = 'P_MTRL'   value = REF #( p_mtrl ) )     INTO TABLE gt_sel_params.

ENHANCEMENT-POINT /vpcoe/mat_doc_save_sel_opt SPOTS /vpcoe/enh_mat_dock_bck .

          go_cust->save_delta_sel_opt( iv_sel_name = COND #( WHEN p_delta = abap_true THEN p_pselid
                                                                                      ELSE p_cselid )
                                       iv_test_run = r_screen
                                       it_sel_params  = gt_sel_params ).
        ENDIF.
      ENDIF.
  ENDCASE.

  IF lv_mode = /vpcoe/cl_rdp_helper=>sc_mode-screen.
    IF lo_log->check( ) OR gv_global_no_data = abap_true.
      lo_log->messages_to_display( ).
    ENDIF.
  ELSE.

    lo_log->display_message( iv_with_total = p_mtrl
                             iv_excel      = r_excel ).
    IF p_bildi = abap_true.
      lo_log_billdoc->display_message( iv_with_total = p_bilds
                                       iv_excel      = r_excel ).
    ENDIF.
    IF p_supinv = abap_true.
      lo_log_suppnv->display_message( iv_with_total = p_supis
                                      iv_excel      = r_excel ).
    ENDIF.

    IF p_mtrl = abap_true.
      lo_log->save( iv_no_total = COND #( WHEN r_send = abap_true THEN abap_false
                                                                  ELSE abap_true ) ).
    ENDIF.
    IF p_bildi = abap_true.
      lo_log_billdoc->save( iv_no_total = COND #( WHEN r_send = abap_true THEN abap_false
                                                                          ELSE abap_true ) ).
    ENDIF.
    IF p_supinv = abap_true.
      lo_log_suppnv->save( iv_no_total = COND #( WHEN r_send = abap_true THEN abap_false
                                                                         ELSE abap_true ) ).
    ENDIF.

    IF sy-batch = abap_true.
      IF lo_log->check( ) OR lo_log_billdoc->check( ) OR lo_log_suppnv->check( ).
        MESSAGE e091(/vpcoe/common).
      ENDIF.
    ENDIF.

  ENDIF.
