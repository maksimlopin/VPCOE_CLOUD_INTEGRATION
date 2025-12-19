*&---------------------------------------------------------------------*
*& Report  /VPCOE/CONFIGURARION_TO_RDP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/product_to_rdp.

TABLES: mara, marc, mvke.
DATA: lv_ucomm               TYPE sy-ucomm,
      gt_bapiret2            TYPE bapiret2_t,
      gt_bapiret2_ext        TYPE bapiret2_t,
      gt_product             TYPE STANDARD TABLE OF /vpcoe/cl_rdp_product_data=>gty_s_product_combined,
      gt_display_product     TYPE STANDARD TABLE OF /vpcoe/cl_rdp_product_data=>gty_s_product_combined_base,
      gv_api_type            TYPE /vpcoe/de_api_type,
      gv_product_type_harmon TYPE /vpcoe/de_product_type_harmon,
      gv_de_product_content  TYPE /vpcoe/de_product_content,
      gv_valid_from          TYPE /vpcoe/de_valid_from,
      gs_sel_opt             TYPE /vpcoe/s_selopt_product,
      gs_sel_opt_ext         TYPE /vpcoe/cl_rdp_product_data=>gty_prd_ext_sel_opt,
      gv_default             TYPE string VALUE /vpcoe/cl_rdp_helper=>sc_service_id-product.

INCLUDE /vpcoe/product_trans_selscr.

START-OF-SELECTION.

  gv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp.
  DATA(go_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = gv_api_type
                                            iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-product
                                            iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-product ).

  DATA(lo_rdp_api) = /vpcoe/cl_rdp_api_instance=>get_instance( ).

  DATA(lv_mode) = go_cust->set_mode( iv_screen = r_screen
                                     iv_excel  = r_excel ).

  DATA(go_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-product
                                        iv_mode       = lv_mode ).

  DATA(go_product_data) = /vpcoe/cl_rdp_api_instance=>get_instance( )->get_product_handler(
                                                         io_rdp_helper = go_cust
                                                         io_log        = go_log
                                                         iv_mode       = lv_mode ).


  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    go_log->add_sy_msg( ).
    go_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  IF lv_ucomm = 'CBAC'.
    RETURN.
  ENDIF.


  DATA(go_log_ext) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-product_ext ).
  IF pgen = abap_false AND pext = abap_false.
    IF sy-batch = space.
      MESSAGE i029(/vpcoe/common).
    ELSE.
      MESSAGE i029(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
      go_log->save( ).
    ENDIF.
  ENDIF.

  gs_sel_opt-matnr     = so_nmbr[].
  gs_sel_opt-type      = so_type[].
  gs_sel_opt-grp       = so_grp[].
  gs_sel_opt-chng_date = so_chng[].
  gs_sel_opt-vkorg     = so_vkorg[].
  gs_sel_opt-werks     = so_werks[].
  gs_sel_opt-mstae     = so_mstae[].
  gs_sel_opt-datab     = so_vlfr[].
  gs_sel_opt-mstde    =  so_mstde[].

  gs_sel_opt_ext = CORRESPONDING #( gs_sel_opt ).
  gs_sel_opt_ext-delta_only          = p_delta.
  gs_sel_opt_ext-valid_from          = so_vlfr[].
  gs_sel_opt_ext-product_type_harmon = so_prth[].
  gs_sel_opt_ext-product_content     = so_prcnt[].

ENHANCEMENT-POINT /vpcoe/product_trans_set_data SPOTS /vpcoe/enh_product_trans .

  IF p_delta = abap_true.
    go_product_data->get_product_delta(
      EXPORTING
        is_sel_opt          = gs_sel_opt
        iv_chng_pointer_id  = p_chgpid
        iv_gen              = pgen
      IMPORTING
        et_json             = DATA(gt_json)
        et_product          = DATA(gt_product_sort)
        es_product_tables   = DATA(gs_product_tables) ).
  ELSE.
    go_product_data->get_product(
      EXPORTING
        is_sel_opt        = gs_sel_opt
        iv_gen            = pgen
      IMPORTING
        et_product        = gt_product_sort
        es_product_tables = gs_product_tables ).
  ENDIF.

  IF pext = abap_true.
    go_product_data->get_product_ext(
      EXPORTING
        is_sel_opt      = gs_sel_opt_ext
        it_product      = gt_product_sort
        io_log          = go_log_ext
      IMPORTING
        et_product_ext  = DATA(gt_product_ext)
      CHANGING
        cs_product_tables = gs_product_tables ).
  ENDIF.

  CASE lv_mode.
    WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
      IF ( gt_product_sort IS NOT INITIAL OR gt_product_ext IS NOT INITIAL ).
        DATA(lo_sc) = NEW cl_gui_splitter_container( parent = cl_gui_container=>screen0
                                                     no_autodef_progid_dynnr = abap_true
                                                     columns = COND #( WHEN pext = abap_true
                                                                         THEN 2
                                                                         ELSE 1 )
                                                     rows = 1 ).
        gt_product = gt_product_sort.
        IF pgen = abap_true.
          IF lines( gt_product ) >= /vpcoe/cl_rdp_helper=>gc_display_amount.
            MESSAGE i065(/vpcoe/common).
            DELETE gt_product FROM /vpcoe/cl_rdp_helper=>gc_display_amount + 1.
          ENDIF.
          gt_display_product = CORRESPONDING #( gt_product ).
          go_cust->display_selected_data(
            EXPORTING
              it_deep_data = gt_display_product
              iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-product
              is_splitter = VALUE #( sc     = lo_sc
                                     row    = 1
                                     column = 1  ) ).
        ENDIF.

        IF pext = abap_true.
          IF lines( gt_product_ext ) >= /vpcoe/cl_rdp_helper=>gc_display_amount.
            IF lines( gt_product ) < /vpcoe/cl_rdp_helper=>gc_display_amount. "To avoid double message
              MESSAGE i065(/vpcoe/common).

            ENDIF.
            DELETE gt_product_ext FROM /vpcoe/cl_rdp_helper=>gc_display_amount + 1.
          ENDIF.
          go_cust->display_selected_data(
           EXPORTING
              it_deep_data = gt_product_ext
              iv_lable     = /vpcoe/cl_rdp_helper=>sc_service_id-product_ext
              it_columns_title = VALUE #( ( column = 'IS_HAZARDOUS_CLP_EU' text = 'Is.Hzrd.EU'(020) )
                                          ( column = 'HAS_NO_PACKAGING'    text = 'Has No Packaging'(021) ) )
              is_splitter  = VALUE #( sc     = lo_sc
                                      row    = 1
                                      column = 2  ) ).
        ENDIF.
        WRITE: ' '.

      ELSE.
        MESSAGE i015(/vpcoe/common).
        go_log->add_sy_msg( ).
      ENDIF.

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
      IF p_file IS INITIAL.
        MESSAGE e064(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        IF pext = abap_true.
          go_log_ext->add_sy_msg( ).
        ENDIF.
        IF pgen = abap_true.
          go_log->add_sy_msg( ).
        ENDIF.
      ELSEIF r_server = abap_false AND sy-batch = abap_true.
        MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        IF pext = abap_true.
          go_log_ext->add_sy_msg( ).
        ENDIF.
        IF pgen = abap_true.
          go_log->add_sy_msg( ).
        ENDIF.
      ELSE.
        IF pgen = abap_true.
          go_product_data->download_excel(
            EXPORTING
              is_product_tables = gs_product_tables
              iv_file_path      = CONV #( p_file )
              iv_save_background = COND #( WHEN r_server = abap_true THEN abap_true
                                                                    ELSE abap_false ) ).
          IF NOT go_log->check( ).
            MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            go_log->add_sy_msg( ).
          ENDIF.
        ENDIF.
        IF  pext = abap_true.
          SPLIT p_file AT '.xls' INTO DATA(lv_name) DATA(lv_extension).
          DATA(lv_file_ext) = |{ lv_name }_ExtensionData.xls|.
          go_product_data->download_excel(
          EXPORTING
            iv_service_id     = /vpcoe/cl_rdp_helper=>sc_service_id-product_ext
            is_product_tables = gs_product_tables
            iv_file_path      = lv_file_ext
            iv_save_background = COND #( WHEN r_server = abap_true THEN abap_true
                                                                    ELSE abap_false ) ).
          IF NOT go_log_ext->check( ).
            MESSAGE i069(/vpcoe/common) WITH |{ lv_name }_ExtensionData.xls| INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            go_log_ext->add_sy_msg( ).
          ENDIF.
        ENDIF.
      ENDIF.

  ENDCASE.

  IF r_screen = abap_false.
    go_log->display_message( iv_with_total = COND #( WHEN r_send = abap_true THEN abap_true
                                                                             ELSE abap_false )
                             iv_excel = r_excel ).
    IF pext = abap_true.
      go_log_ext->display_message( iv_with_total = COND #( WHEN r_send = abap_true THEN abap_true
                                                                                   ELSE abap_false )
                                   iv_excel = r_excel ).
    ENDIF.
    go_log->save( iv_no_total = COND #( WHEN r_send = abap_true THEN abap_false
                                                                 ELSE abap_true ) ).
    IF pext = abap_true.
      go_log_ext->save( iv_no_total =  COND #( WHEN r_send = abap_true THEN abap_false
                                                                        ELSE abap_true )  ).
    ENDIF.
  ENDIF.
