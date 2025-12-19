*&---------------------------------------------------------------------*
*&  Include           /VPCOE/SUPPINVITEMS_TO_RDP_GEN
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  IF r_del = abap_true.
    NEW /vpcoe/cl_rdp_supinvi_data( /vpcoe/cl_rdp_helper=>set_mode( iv_screen = r_screen
                                                                    iv_excel  = abap_false  ) )->delete_expired( ).
    RETURN.
  ENDIF.

  DATA(lv_mode) = /vpcoe/cl_rdp_helper=>set_mode( iv_screen = r_screen
                                                  iv_excel  = abap_false ).

  DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                            iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-supinvi
                                            iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-supinvi ).

  DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-supinvi
                                        iv_mode       = lv_mode ).

  DATA(lo_billdi_data) = NEW /vpcoe/cl_rdp_supinvi_data( lv_mode ).

  lo_billdi_data->process_supplier_inv_items(
    EXPORTING
      is_sel_opt      = VALUE #( supplier_invoice   = so_sinvi[]
                                 supinvi_country    = so_cntri[]
                                 mat_doc            = so_mdoc[]
                                 supinvi_session_id = so_sesid[] )
      io_log          = lo_log
    IMPORTING
      et_supinvi      = DATA(lt_supinvi)
      CHANGING
        ct_total_count_suppinv = lo_log->mt_sum ).


  CASE lv_mode.
    WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
      lo_log->display_message( ).
      lo_log->save( ).

    WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
      lo_cust->display_selected_data( it_deep_data = lt_supinvi ).

  ENDCASE.
