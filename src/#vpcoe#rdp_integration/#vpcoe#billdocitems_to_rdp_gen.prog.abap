*&---------------------------------------------------------------------*
*&  Include           /VPCOE/BILLDOCITEMS_TO_RDP_GEN
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF r_del = abap_true.
    NEW /vpcoe/cl_rdp_billdocit_data( /vpcoe/cl_rdp_helper=>set_mode( iv_screen = r_screen
                                                                      iv_excel  = abap_false  ) )->delete_expired( ).
    RETURN.
  ENDIF.

  DATA(lv_mode) = /vpcoe/cl_rdp_helper=>set_mode( iv_screen = r_screen
                                                  iv_excel  = abap_false ).

  DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                            iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-billdocit
                                            iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-billdocit ).

  DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-billdocit
                                        iv_mode       = lv_mode ).

  DATA(lo_billdi_data) = NEW /vpcoe/cl_rdp_billdocit_data( lv_mode ).

  lo_billdi_data->process_billing_doc_items(
    EXPORTING
      iv_service_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
      iv_send         = COND #( WHEN lv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send THEN abap_true
                                                                                  ELSE abap_false )
      is_sel_opt      = VALUE #( billdoc        = so_mdoc[]
                                 bdocdate       = so_bdat[]
                                 bsddoccat      = so_bsddc[]
                                 billdi_country = so_cntrb[]
                                 billdoc_session_id = so_sesid[] )
      io_log          = lo_log
    IMPORTING
      et_billdi       = DATA(lt_billdi)
    CHANGING
      ct_total_count_billdoc = lo_log->mt_sum ).


  CASE lv_mode.
    WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
      IF sy-batch = space.
        lo_log->display_message( ).
      ENDIF.

      lo_log->save( ).

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
      lo_cust->display_selected_data( it_deep_data = lt_billdi ).

  ENDCASE.
