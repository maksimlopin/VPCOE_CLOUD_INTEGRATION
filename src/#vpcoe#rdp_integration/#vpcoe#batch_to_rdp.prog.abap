*&---------------------------------------------------------------------*
*& Report  /VPCOE/BATCH_TO_RDP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/batch_to_rdp.

TABLES: mch1, mcha, mara, cabn, klah.
DATA: lv_ucomm   TYPE sy-ucomm,
      gt_batch   TYPE /vpcoe/cl_rdp_batch_data=>gty_t_batch,
      gs_sel_opt TYPE /vpcoe/s_selopt_batch,
      gv_status  TYPE i,
      gv_default TYPE string VALUE /vpcoe/cl_common_helper=>sc_service_id-packaging.

INCLUDE /vpcoe/version.
SELECTION-SCREEN BEGIN OF BLOCK delta WITH FRAME TITLE text-dlr.
PARAMETERS: p_delta  TYPE xfeld AS CHECKBOX USER-COMMAND usrdelta,
            p_chgpid TYPE edi_mestyp DEFAULT '/VPCOE/BATCH'.
SELECTION-SCREEN END OF BLOCK delta.
SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-opt.
PARAMETERS: r_screen RADIOBUTTON GROUP req USER-COMMAND et,
            r_send   RADIOBUTTON GROUP req,
            r_excel  RADIOBUTTON GROUP req.
INCLUDE /vpcoe/i_excel_selscr.
SELECTION-SCREEN END OF BLOCK options.

SELECTION-SCREEN BEGIN OF BLOCK seldata WITH FRAME TITLE text-slp.
SELECT-OPTIONS:
  so_charg FOR mch1-charg, "Display id
  so_matnr FOR mara-matnr, "Material
  so_hsdat FOR mch1-hsdat, "Date
  so_werks FOR mcha-werks, "Plant
  so_ersda FOR mch1-ersda. "Created On
PARAMETERS:
  p_class TYPE klah-class,
  p_charc TYPE cabn-atnam.
SELECTION-SCREEN END OF BLOCK seldata.

ENHANCEMENT-POINT /vpcoe/batch_sel_scr SPOTS /vpcoe/enh_batch.

INCLUDE /vpcoe/i_excel_valreq.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  lv_ucomm = sy-ucomm.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'P_CHGPID'.
      screen-input = COND #( WHEN p_delta = abap_true THEN 1
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

ENHANCEMENT-POINT /vpcoe/batch_scr_out SPOTS /vpcoe/enh_batch .

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_BATCH' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.

  "default class and charecteristic
  p_class = 'ZBCL_BATCH_PACKCOM'.
  p_charc = 'ZRDP_BATCH_PACKCOMP'.

START-OF-SELECTION.

  IF lv_ucomm = 'CBAC'.
    RETURN.
  ENDIF.

  DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                            iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-batch
                                            iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-packaging ).

  DATA(lo_rdp_api) = /vpcoe/cl_rdp_api_instance=>get_instance( ).
  DATA(lo_batch_data) = lo_rdp_api->get_batch_handler( lo_cust ).

  DATA(lv_mode) = lo_cust->set_mode( iv_screen = r_screen
                                     iv_excel  = r_excel ).

  DATA(lo_log_batch)  = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-packaging
                                               iv_mode       = lv_mode ).

  IF r_screen = abap_true AND sy-batch = abap_true.
    MESSAGE e014(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    lo_log_batch->add_sy_msg( ).
    lo_log_batch->save( iv_no_total = abap_true ).
    RETURN.
  ENDIF.

  gs_sel_opt-display_id = so_charg[].
  gs_sel_opt-plant      = so_werks[].
  gs_sel_opt-date       = so_hsdat[].
  gs_sel_opt-material   = so_matnr[].
  gs_sel_opt-ersda      = so_ersda[].
  gs_sel_opt-characteristic = p_charc.
  gs_sel_opt-class          = p_class.

ENHANCEMENT-POINT /vpcoe/batch_set_data SPOTS /vpcoe/enh_batch .

  IF p_delta = abap_true.
    lo_batch_data->get_batch_delta(
    EXPORTING
      iv_chng_pointer_id = p_chgpid
      is_sel_opt         = gs_sel_opt
      io_log             = lo_log_batch
    IMPORTING
      et_json            = DATA(lt_json)
      et_batch           = gt_batch ).

  ELSE.
    lo_batch_data->get_batch(
      EXPORTING
        is_sel_opt = gs_sel_opt
        iv_mode    = lo_cust->set_mode( iv_screen = r_screen
                                        iv_excel = r_excel )
        io_log     = lo_log_batch
      IMPORTING
        et_json   = lt_json
        et_batch  = gt_batch ).
  ENDIF.

  CASE lv_mode.
    WHEN /vpcoe/cl_common_helper=>sc_mode-screen.
      IF lines( gt_batch ) >= /vpcoe/cl_common_helper=>gc_display_amount.
        MESSAGE i065(/vpcoe/common).
        DELETE gt_batch FROM /vpcoe/cl_rdp_helper=>gc_display_amount + 1.
      ENDIF.
      lo_cust->display_selected_data(
        EXPORTING
              it_deep_data = gt_batch
              iv_lable     = /vpcoe/cl_common_helper=>sc_service_id-packaging ).

    WHEN /vpcoe/cl_common_helper=>sc_mode-document.
      DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path            = CONV #( p_file )
                                                     iv_save_background = COND #( WHEN r_server = abap_true THEN abap_true
                                                                                                             ELSE abap_false ) ).
      IF p_file IS NOT INITIAL.
        IF r_server = abap_false AND sy-batch = abap_true.
          MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          lo_log_batch->add_sy_msg( ).
          lo_log_batch->save( iv_no_total = abap_true ).
          RETURN.
        ENDIF.
        lo_batch_data->get_header_excel(
          IMPORTING
            ev_header = DATA(lv_header)
            et_header = DATA(lt_header) ).

        lo_xls_file->execute( iv_name = lv_header ).

        lo_xls_file->fill_data( it_title        = lt_header
                                it_item_tab     = gt_batch
                                iv_start_column = 1
                                iv_start_row    = 1 ).

        lo_xls_file->save_xls_file( lo_log_batch ).

        IF NOT lo_log_batch->check( ).
          MESSAGE i069(/vpcoe/common) WITH p_file INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          lo_log_batch->add_sy_msg( ).
        ENDIF.
      ELSE.
        MESSAGE e064(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        lo_log_batch->add_sy_msg( ).
      ENDIF.

      lo_log_batch->display_message( iv_with_total = abap_false iv_excel = abap_true ).

      lo_log_batch->save( iv_no_total = abap_true ).

    WHEN /vpcoe/cl_common_helper=>sc_mode-send.
      IF lt_json IS INITIAL.
        MESSAGE i015(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        lo_log_batch->add_sy_msg( ).
        lo_log_batch->display_message( ).
        lo_log_batch->save( iv_no_total = abap_true ).
        RETURN.

      ELSE.

        IF NOT lo_log_batch->check( ).
          NEW /vpcoe/cl_rdp_payload_handler( )->send_payload(
            EXPORTING
              io_cust     = lo_cust
              it_json     = lt_json
              io_log      = lo_log_batch
              iv_save_log = abap_true
            IMPORTING
              ev_status   = gv_status ).

          IF gv_status = /vpcoe/cl_rdp_http=>gc_s_http_status-ok.
            lo_batch_data->change_status( EXPORTING iv_test_run = COND #( WHEN r_send = abap_true
                                                                              THEN abap_false
                                                                              ELSE abap_true ) ).
          ENDIF.

          IF lo_rdp_api->is_suma_api_enabled( lo_cust ).
            lo_batch_data->close_replication( lo_log_batch ).
          ENDIF.
        ENDIF.
      ENDIF.

      lo_log_batch->display_message( ).
      lo_log_batch->save( iv_no_total = abap_true ).

  ENDCASE.
