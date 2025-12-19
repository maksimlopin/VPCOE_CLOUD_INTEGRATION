*&---------------------------------------------------------------------*
*&  Include           /VPCOE/MAT_DOC_RO_RDP_SELSCR
*&---------------------------------------------------------------------*
INCLUDE /vpcoe/mat_doc_to_rdp_scr_nrml.

AT SELECTION-SCREEN.
  INCLUDE /vpcoe/i_delta_ss_hndlr.

  IF p_delta = abap_true AND sscrfields-ucomm = 'CMD_SELID'.
    go_cust->get_delta_sel_opt(
      EXPORTING
        iv_sel_name = p_pselid
      IMPORTING
        et_sel_opt  = gt_sel_params
        ev_last_run = DATA(lv_last_run) ).

    LOOP AT gt_sel_params ASSIGNING FIELD-SYMBOL(<ls_sel_params>).
      CASE <ls_sel_params>-field.
        WHEN 'SO_CNTRY' OR 'SO_YEAR' OR 'SO_WERKS' OR 'SO_MDOC' OR 'SO_POST' OR 'SO_TYPE' OR 'SO_MATNR' OR 'SO_BILDI' OR 'SO_BDAT' OR
          'SO_BSDDC' OR 'SO_BELNR' OR 'SO_BUZEI' OR 'SO_GJAHR' OR 'SO_CNTRB' OR 'SO_CNTRI' OR 'SO_MTART' OR 'SO_VKORG'.
          ASSIGN <ls_sel_params>-value->* TO <gt_value>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          CASE <ls_sel_params>-field.
            WHEN 'SO_WERKS'.
              CLEAR so_werks[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_werks[] ).
            WHEN 'SO_CNTRY'.
              CLEAR so_cntry[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_cntry[] ).
            WHEN  'SO_YEAR'.
              CLEAR so_year[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_year[] ).
            WHEN 'SO_MDOC'.
              CLEAR so_mdoc[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_mdoc[] ).
            WHEN 'SO_POST'.
              CLEAR so_post[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_post[] ).
            WHEN 'SO_TYPE'.
              CLEAR so_type[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_type[] ).
            WHEN 'SO_MATNR'.
              CLEAR so_matnr[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_matnr[] ).
            WHEN 'SO_MTART'.
              CLEAR so_mtart[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_mtart[] ).
            WHEN 'SO_VKORG'.
              CLEAR so_vkorg[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_vkorg[] ).
            WHEN 'SO_BILDI'.
              CLEAR so_bildi[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_bildi[] ).
            WHEN 'SO_BDAT'.
              CLEAR so_bdat[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_bdat[] ).
            WHEN 'SO_BSDDC'.
              CLEAR so_bsddc[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_bsddc[] ).
            WHEN 'SO_BELNR'.
              CLEAR so_belnr[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_belnr[] ).
            WHEN 'SO_BUZEI'.
              CLEAR so_buzei[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_buzei[] ).
            WHEN 'SO_GJAHR'.
              CLEAR so_gjahr[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_gjahr[] ).
            WHEN 'SO_CNTRB'.
              CLEAR so_cntrb[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_cntrb[] ).
            WHEN 'SO_CNTRI'.
              CLEAR so_cntri[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <gt_value>
                CHANGING
                  ct_so     = so_cntri[] ).

          ENDCASE.

        WHEN  'P_GR_PR' OR 'P_GI_PR' OR 'P_GR_ST' OR 'P_GI_ST' OR 'P_GR_POI' OR 'P_GR_PO' OR 'P_GI_SOE' OR 'P_GI_SO' OR 'P_GR_STD' OR 'P_GI_STD'
              OR 'P_BILDI' OR 'P_SUPINV' OR 'P_MTRL' OR 'P_BILDS' OR 'P_SUPIS'.
          ASSIGN <ls_sel_params>-value->* TO <gv_value>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          CASE <ls_sel_params>-field.
            WHEN  'P_GR_PR'.
              p_gr_pr = <gv_value>.
            WHEN  'P_GI_PR'.
              p_gi_pr = <gv_value>.
            WHEN  'P_GR_ST'.
              p_gr_st = <gv_value>.
            WHEN  'P_GI_ST'.
              p_gi_st  = <gv_value>.
            WHEN  'P_GR_POI'.
              p_gr_poi = <gv_value>.
            WHEN  'P_GR_PO'.
              p_gr_po = <gv_value>.
            WHEN  'P_GI_SOE'.
              p_gi_soe = <gv_value>.
            WHEN  'P_GI_SO'.
              p_gi_so = <gv_value>.
            WHEN  'P_GI_STD'.
              p_gi_std = <gv_value>.
            WHEN  'P_GR_STD'.
              p_gr_std = <gv_value>.
            WHEN  'P_BILDI'.
              p_bildi = <gv_value>.
            WHEN  'P_SUPINV'.
              p_supinv = <gv_value>.
            WHEN  'P_BILDS'.
              p_bilds = <gv_value>.
            WHEN  'P_SUPIS'.
              p_supis = <gv_value>.
            WHEN  'P_MTRL'.
              p_mtrl = <gv_value>.
          ENDCASE.
      ENDCASE.
    ENDLOOP.

    IF lv_last_run IS NOT INITIAL.
      so_post[] = VALUE #( ( sign = 'I' option = 'BT' low = lv_last_run high = sy-datum ) ).
    ENDIF.

ENHANCEMENT-POINT /vpcoe/mat_doc_bck_sel_scr_dlt SPOTS /vpcoe/enh_mat_dock_bck .

  ENDIF.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  INCLUDE /vpcoe/i_excel_valreq.

  lv_ucomm = sy-ucomm.

AT SELECTION-SCREEN OUTPUT.
  btn_delc = text-016."'Delete from Previous Selection ID'.
  btn_dela = text-017."'Delete all enteries'.

  IF lv_ucomm = 'BILDI'.
    IF p_bildi = abap_true.
      p_bilds = abap_true.
    ELSE.
      p_bilds = abap_false.
    ENDIF.
  ELSEIF lv_ucomm = 'BLDS'.
    IF p_bilds = abap_true.
      p_bildi = abap_true.
    ELSE.
      p_bildi = abap_false.
    ENDIF.
  ELSEIF lv_ucomm = 'SUPINVI'.
    IF p_supinv = abap_true.
      p_supis = abap_true.
    ELSE.
      p_supis = abap_false.
    ENDIF.
  ELSEIF lv_ucomm = 'SUPL'.
    IF p_supis = abap_true.
      p_supinv = abap_true.
    ELSE.
      p_supinv = abap_false.
    ENDIF.
  ENDIF.

  gv_scr_fld_state = COND #( WHEN p_delta = abap_true THEN 0
                                                      ELSE 1 ).

  LOOP AT SCREEN.
    IF screen-name CS 'P_GI_STD'
       OR screen-name CS 'P_GR_STD'.
      screen-input = gv_scr_fld_state.
      MODIFY SCREEN.
    ELSEIF screen-name CS 'SO_CNTRY'
      OR screen-name CS 'SO_YEAR'
      OR screen-name CS 'SO_WERKS'
      OR screen-name CS 'SO_MDOC'
      OR screen-name CS 'SO_TYPE'
      OR screen-name CS 'SO_MATNR'
      OR screen-name CS 'SO_MTART'
      OR screen-name CS 'SO_VKORG'
      OR screen-name CS 'P_GR_PR'
      OR screen-name CS 'P_GI_PR'
      OR screen-name CS 'P_GR_ST'
      OR screen-name CS 'P_GI_ST'
      OR screen-name CS 'P_GR_POI'
      OR screen-name CS 'P_GR_PO'
      OR screen-name CS 'P_GI_SOE'
      OR screen-name CS 'P_GI_SO'
      OR screen-name CS 'SO_CNTRB'
      OR screen-name CS 'SO_BILDI'
      OR screen-name CS 'SO_BDAT'
      OR screen-name CS 'SO_BSDDC'
      OR screen-name CS 'SO_CNTRI'
      OR screen-name CS 'SO_BELNR'
      OR screen-name CS 'SO_BUZEI'
      OR screen-name CS 'SO_CCODE'
      OR screen-name CS 'SO_PLANT'
      OR screen-name CS 'SO_GJAHR'.
      screen-input = gv_scr_fld_state.
      MODIFY SCREEN.
    ELSEIF screen-name CS 'SO_CNTRB'.
      screen-input = 0.
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_CSELID'.
      screen-active = COND #( WHEN p_delta = abap_true THEN 0
                                                       ELSE 1 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_PSELID'.
      screen-active = COND #( WHEN p_delta = abap_true THEN 1
                                                       ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_FILE'.
      screen-active = COND #( WHEN r_excel = abap_true THEN 1
                                                    ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_MTRL' OR
           screen-name CS 'P_BILDS' OR
           screen-name CS 'P_SUPIS' .
      screen-active = COND #( WHEN  r_send = abap_true  THEN 1
                                                         ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    CASE screen-group1.
      WHEN 'TRL'.
        screen-intensified = '1'.
        MODIFY SCREEN.
      WHEN 'OFF'.
        screen-active = '0'.
        MODIFY SCREEN.
      WHEN 'BDI'.
        screen-active = COND #( WHEN p_bildi = abap_true THEN 1
                                                         ELSE 0 ).
        MODIFY SCREEN.
      WHEN 'SUP'.
        screen-active = COND #( WHEN p_supinv = abap_true THEN 1
                                                         ELSE 0 ).
        MODIFY SCREEN.
      WHEN 'SOP'.
        screen-active = COND #( WHEN  r_send = abap_true  THEN 1
                                                          ELSE 0 ).
        MODIFY SCREEN.

    ENDCASE.
  ENDLOOP.
  INCLUDE /vpcoe/i_delta_ss_out.
  INCLUDE /vpcoe/version_set.
  INCLUDE /vpcoe/i_excel_scrout.

  "Since they are OFF
  CLEAR: p_gi_pr,
         p_gi_soe,
         p_gi_so.

ENHANCEMENT-POINT /vpcoe/mat_doc_bck_sel_scr_out SPOTS /vpcoe/enh_mat_dock_bck .

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_MAT_DOC' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.
  IF p_cselid IS INITIAL.
    p_cselid = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc && '-' && /vpcoe/cl_rdp_helper=>sc_service_id-material_doc && '-' && sy-datum.
  ENDIF.
  go_cust = NEW #( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                   iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
                   iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).

  go_cust->get_delta_ids( IMPORTING et_sel_names = DATA(lt_sel_opt) ).
  LOOP AT lt_sel_opt ASSIGNING FIELD-SYMBOL(<ls_mat_doc>).
    INSERT VALUE #( key  = <ls_mat_doc>
                    text = <ls_mat_doc>  ) INTO TABLE gt_selid_vrm.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_PSELID'
      values          = gt_selid_vrm
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    CLEAR gt_selid_vrm.
  ENDIF.
