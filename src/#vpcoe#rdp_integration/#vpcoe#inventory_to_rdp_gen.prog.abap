*&---------------------------------------------------------------------*
*&  Include           /VPCOE/INVENTORY_TO_RDP_GEN
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lv_last_day_in_month TYPE budat,
        lv_continue          TYPE c,
        lv_day_in            TYPE sy-datum.

  DATA(lv_mode) = go_cust->set_mode( iv_screen = r_screen
                                     iv_excel  = r_excel ).

  DATA(go_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-inventory
                                        iv_mode       = lv_mode ).

  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    go_log->add_sy_msg( ).
    go_log->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  IF p_delta = abap_true AND p_pselid IS INITIAL.
    IF sy-batch = space.
      MESSAGE i074(/vpcoe/common).
    ELSE.
      MESSAGE i074(/vpcoe/common) INTO DATA(lv_message).
      go_log->add_sy_msg( ).
    ENDIF.
    IF r_send = abap_true.
      go_log->save( ).
    ENDIF.
    RETURN.
  ENDIF.

  IF r_excel = abap_true.
    IF p_file IS INITIAL.
      MESSAGE e064(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
    ENDIF.
  ENDIF.

  IF so_co_co[] IS INITIAL
     AND so_plant[] IS INITIAL
     AND so_pro[] IS INITIAL
     AND so_year[] IS INITIAL
     AND so_month[] IS INITIAL.
    IF sy-batch = space.
      MESSAGE w032(/vpcoe/common) DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE w032(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
    ENDIF.
    IF r_send = abap_true.
      go_log->save( ).
    ENDIF.
    RETURN.
  ENDIF.

  IF p_delta IS INITIAL AND ( so_year[] IS INITIAL OR so_month[] IS INITIAL ) OR p_delta IS NOT INITIAL AND ( so_year[] IS INITIAL OR so_month[] IS INITIAL ).
    IF sy-batch = space.
      MESSAGE i109(/vpcoe/common) DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE e109(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
    ENDIF.
    IF r_send = abap_true.
      go_log->save( ).
    ENDIF.
    RETURN.
  ENDIF.

  IF so_month-low < 1 OR so_month-low > 12.
    IF sy-batch = space.
      MESSAGE i110(/vpcoe/common) DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE e110(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
    ENDIF.
    IF r_send = abap_true.
      go_log->save( ).
    ENDIF.
    RETURN.

  ELSE.

    IF so_year-low = sy-datum(4) AND so_month-low = sy-datum+4(2).
      lv_day_in = so_year-low && so_month-low && '01'.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = lv_day_in "sy-datum
        IMPORTING
          last_day_of_month = lv_last_day_in_month
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.
    ENDIF.

    IF sy-batch = abap_false.
      IF sy-subrc <> 0
          OR ( lv_last_day_in_month IS NOT INITIAL AND lv_last_day_in_month+6(2) <> sy-datum+6(2) )
          OR ( so_month-low > sy-datum+4(2) AND so_year-low = sy-datum(4) ).
        MESSAGE i066(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = ' '
            text_question         = /vpcoe/cl_rdp_log=>sv_msg_text
            icon_button_1         = 'ICON_OKAY'
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = lv_continue
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0 OR lv_continue <> '1'.
          MESSAGE e072(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          go_log->add_sy_msg( ).
          go_log->save( iv_no_total = abap_true ).
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

  IF p_sbbst = abap_true AND so_sobkz IS INITIAL.
    IF sy-batch = space.
      MESSAGE i033(/vpcoe/common) DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE e033(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
    ENDIF.
    IF r_send = abap_true.
      go_log->save( ).
    ENDIF.
    RETURN.
  ENDIF.

  IF so_co_co IS INITIAL.
    so_co_co[] = /vpcoe/cl_rdp_inventory=>get_company_code( EXPORTING it_r_plant   = so_plant[]
                                                                      it_r_product = so_pro[] ).
  ENDIF.

  DATA(go_inventory) = NEW /vpcoe/cl_rdp_inventory( iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                    iv_package_size = go_cust->get_package_size( )
                                                    iv_source       = /vpcoe/cl_rdp_helper=>get_source_id( )
                                                    iv_mode         = lv_mode ).

  DATA(ls_sel_op) = VALUE /vpcoe/s_selopt_inventory( co_code     = so_co_co[]
                                                     month       = so_month[]
                                                     plant       = so_plant[]
                                                     prod        = so_pro[]
                                                     year        = so_year[]
                                                     sobkz_range = so_sobkz[]
                                                     lgbst       = p_lgbst
                                                     sbbst       = p_sbbst
                                                     from_arc    = p_arc
                                                     arc_idx     = p_arcidx
                                                     trans       = p_trans
                                                     xtram       = p_xtram
                                                     xnlcc       = p_xnlcc
                                                     xelik       = p_xelik ).

ENHANCEMENT-POINT /vpcoe/inventory_set_data SPOTS /vpcoe/enh_inventory_rdp .

  go_inventory->get_inventory_stocks(
     EXPORTING
       io_log         = go_log
       io_cust        = go_cust
       iv_test_run    = r_screen
       is_sel_opt     = ls_sel_op
       iv_path       = CONV #( p_file )
       iv_save_background = COND #( WHEN r_server = abap_true THEN abap_true
                                                                ELSE abap_false )
     IMPORTING
       ev_failed      = DATA(gv_failed)
       ev_no_data     = DATA(gv_no_data)
     CHANGING
       cv_total_count = gv_sum ).

  IF gv_no_data = abap_true.
    go_log->remove_info( ).
    MESSAGE i015(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    go_log->add_sy_msg( ).

  ELSEIF gv_failed = abap_false.
    INSERT VALUE #( field = 'SO_PLANT' value = REF #( so_plant[] ) ) INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'SO_PRO' value = REF #( so_pro[] ) ) INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'SO_CO_CO' value = REF #( so_co_co[] ) ) INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'SO_YEAR' value = REF #( so_year[] ) ) INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'SO_MONTH' value = REF #( so_month[] ) ) INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'SO_SOBKZ' value = REF #( so_sobkz[] ) ) INTO TABLE gt_sel_params.

    DATA(lv_lgbst) = CONV xfeld( p_lgbst ).
    DATA(lv_sbbst) = CONV xfeld( p_sbbst ).
    DATA(lv_trans) = CONV xfeld( p_trans ).
    INSERT VALUE #( field = 'P_LGBST'  value = REF #( lv_lgbst ) )    INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'P_SBBST'  value = REF #( lv_sbbst ) )    INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'P_TRANS'  value = REF #( lv_trans ) )    INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'P_XTRAM'  value = REF #( p_xtram ) )    INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'P_XNLCC'  value = REF #( p_xnlcc ) )    INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'P_XELIK'  value = REF #( p_xelik ) )    INTO TABLE gt_sel_params.
    DATA(lv_arc) = CONV xfeld( p_arc ).
    INSERT VALUE #( field = 'P_ARC'  value = REF #( lv_arc ) )        INTO TABLE gt_sel_params.
    INSERT VALUE #( field = 'P_ARCIDX'  value = REF #( p_arcidx ) )   INTO TABLE gt_sel_params.

ENHANCEMENT-POINT /vpcoe/inventory_save_sel_opt SPOTS /vpcoe/enh_inventory_rdp .

    go_cust->save_delta_sel_opt( iv_sel_name = COND #( WHEN p_delta = abap_true THEN p_pselid
                                                                                ELSE p_cselid )
                                 iv_test_run = COND #( WHEN r_send = abap_true THEN abap_false ELSE abap_true )
                                 it_sel_params  = gt_sel_params ).

  ENDIF.

  IF lv_mode = /vpcoe/cl_rdp_helper=>sc_mode-screen.
    IF go_log->check( ) = abap_false.
      DATA(gt_inventory_result) = go_inventory->get_inventory_results(  ).

      IF lines( gt_inventory_result ) > go_cust->gc_display_amount.
        MESSAGE i065(/vpcoe/common).
        DELETE gt_inventory_result FROM go_cust->gc_display_amount.
      ENDIF.
      go_cust->display_selected_data( it_deep_data = gt_inventory_result
                                      it_exclude_columns = VALUE #( ( 'PRODUCTGROUP' ) ) ).
    ELSE.
      IF sy-batch = space.
        go_log->display_message( iv_with_total = abap_false ).
      ENDIF.
    ENDIF.
  ELSE.
    IF lv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document AND gv_no_data = abap_false.
      MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      go_log->add_sy_msg( ).
    ENDIF.
    go_log->display_message( iv_with_total = r_send iv_excel = r_excel ).

    go_log->save( iv_no_total = COND #( WHEN r_send = abap_true THEN abap_false
                                                                  ELSE abap_true ) ).
  ENDIF.
