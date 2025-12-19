*&---------------------------------------------------------------------*
*&  Include           /VPCOE/DELIVERY_TO_RDP_SELSCR
*&---------------------------------------------------------------------*
INCLUDE /vpcoe/version.
INCLUDE /vpcoe/i_delta_ss.
SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-005.
PARAMETERS:
  p_cselid TYPE /vpcoe/de_sel_name.
PARAMETERS: r_screen RADIOBUTTON GROUP req USER-COMMAND et,
            r_send   RADIOBUTTON GROUP req.

SELECTION-SCREEN BEGIN OF BLOCK send_opt.
"Send Deliv.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(30) text-psd FOR FIELD p_sdlv MODIF ID sop.
SELECTION-SCREEN POSITION 3.
PARAMETERS: p_sdlv   TYPE xfeld AS CHECKBOX DEFAULT 'X' USER-COMMAND dlvr MODIF ID sop.
SELECTION-SCREEN END OF LINE.
"Send BillDoc
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(30) text-psb FOR FIELD p_sbild MODIF ID sop.
SELECTION-SCREEN POSITION 3.
PARAMETERS: p_sbild  TYPE xfeld AS CHECKBOX USER-COMMAND blds MODIF ID sop.
SELECTION-SCREEN END OF LINE.
"Send Customers
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(30) text-pcr FOR FIELD p_sndc MODIF ID cst."sop.
SELECTION-SCREEN POSITION 3.
PARAMETERS: p_sndc TYPE xfeld AS CHECKBOX USER-COMMAND sendcust MODIF ID cst."sop.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 6(30) text-pcp FOR FIELD p_cchp MODIF ID scp.
PARAMETERS: p_cchp TYPE edi_mestyp DEFAULT '/VPCOE/CUSTOMER' MODIF ID scp.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK send_opt.

PARAMETERS: r_excel  RADIOBUTTON GROUP req.
INCLUDE /vpcoe/i_excel_selscr.
SELECTION-SCREEN END OF BLOCK options.

INCLUDE /vpcoe/billdi.

SELECTION-SCREEN BEGIN OF BLOCK seldata WITH FRAME TITLE text-006.
SELECT-OPTIONS:
  so_type  FOR likp-lfart, "DeliveryDocumentType
  so_org   FOR likp-vkorg, "Sales Organization
  so_kunnr FOR likp-kunnr, "CustomerId (ShipToParty)
  so_cntry FOR adrc-country NO INTERVALS, "Country
  so_reg   FOR adrc-region, "Region
  so_categ FOR lips-pstyv, "itemCategory
  so_distr FOR lips-vtweg, "distributionChannel
  so_divis FOR lips-spart, "division
  so_vbeln FOR likp-vbeln,
  so_wadat FOR likp-wadat_ist, "actual goods movement date.
  so_pcntr FOR t001w-land1, "Delivery Plant Country
  so_matnr FOR lips-matnr.
SELECTION-SCREEN END OF BLOCK seldata.

SELECTION-SCREEN BEGIN OF BLOCK seldates WITH FRAME TITLE text-008.
SELECT-OPTIONS:
  so_crt   FOR likp-erdat."Creation Date
SELECTION-SCREEN BEGIN OF LINE. SELECTION-SCREEN COMMENT (10) text-009. SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS:
  so_chng  FOR likp-aedat. "Change Date
SELECTION-SCREEN END OF BLOCK seldates.

ENHANCEMENT-POINT /vpcoe/delivery_sel_scr SPOTS /vpcoe/enh_delivery_bck .

AT SELECTION-SCREEN.
  INCLUDE /vpcoe/i_delta_ss_hndlr.

  IF p_delta = abap_true AND sscrfields-ucomm = 'CMD_SELID'.
    go_cust->get_delta_sel_opt(
      EXPORTING
        iv_sel_name = p_pselid
      IMPORTING
        et_sel_opt  = lt_sel_params
        ev_last_run = DATA(lv_last_run) ).

    LOOP AT lt_sel_params ASSIGNING FIELD-SYMBOL(<ls_sel_params>).
      CASE <ls_sel_params>-field.

        WHEN 'SO_TYPE' OR 'SO_ORG' OR 'SO_KUNNR' OR 'SO_CNTRY' OR 'SO_REG' OR 'SO_CATEG'
                       OR 'SO_DISTR' OR 'SO_DIVIS' OR 'SO_VBELN' OR 'SO_WADAT' OR 'SO_PCNTR' OR 'SO_MATNR' OR 'SO_CNTRB' OR 'SO_BILDI'
                       OR 'SO_BDAT' OR 'SO_BSDDC'.
          ASSIGN <ls_sel_params>-value->* TO <lt_value>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          CASE <ls_sel_params>-field.
            WHEN 'SO_CNTRB'.
              CLEAR so_cntrb[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_cntrb[] ).
            WHEN 'SO_BILDI'.
              CLEAR so_bildi[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_bildi[] ).
            WHEN 'SO_BDAT'.
              CLEAR so_bdat[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_bdat[] ).
            WHEN 'SO_BSDDC'.
              CLEAR so_bsddc[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_bsddc[] ).
            WHEN 'SO_TYPE'.
              CLEAR so_type[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_type[] ).

            WHEN 'SO_ORG'.
              CLEAR so_org[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_org[] ).

            WHEN  'SO_KUNNR'.
              CLEAR so_kunnr[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_kunnr[] ).

            WHEN 'SO_CNTRY'.
              CLEAR so_cntry[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_cntry[] ).

            WHEN 'SO_REG'.
              CLEAR so_reg[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_reg[] ).

            WHEN 'SO_CATEG'.
              CLEAR so_categ[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_categ[] ).

            WHEN 'SO_DISTR'.
              CLEAR so_distr[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_distr[] ).

            WHEN 'SO_DIVIS'.
              CLEAR so_divis[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_divis[] ).

            WHEN 'SO_WADAT'.
              CLEAR so_wadat[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_wadat[] ).

            WHEN 'SO_VBELN'.
              CLEAR so_vbeln[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_vbeln[] ).

            WHEN 'SO_PCNTR'.
              CLEAR so_pcntr[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_pcntr[] ).

            WHEN 'SO_MATNR'.
              CLEAR so_matnr[].
              go_cust->add_sel_opt(
                EXPORTING
                  it_so_gen = <lt_value>
                CHANGING
                  ct_so     = so_matnr[] ).

          ENDCASE.

        WHEN  'P_SNDC' OR 'P_CCHP' OR 'P_BILDI' OR 'P_SDLV' OR 'P_SBILD'.
          ASSIGN <ls_sel_params>-value->* TO <lv_value>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          CASE <ls_sel_params>-field.
            WHEN  'P_SNDC'.
              p_sndc = <lv_value>.
            WHEN  'P_CCHP'.
              p_cchp = <lv_value>.
            WHEN 'P_SDLV'.
              p_sdlv = <lv_value>.
            WHEN 'P_BILDI'.
              p_bildi = <lv_value>.
            WHEN 'P_SBILD'.
              p_sbild = <lv_value>.
          ENDCASE.
      ENDCASE.
    ENDLOOP.

    IF lv_last_run IS NOT INITIAL.
      so_crt[] = VALUE #( ( sign = 'I' option = 'BT' low = lv_last_run high = sy-datum ) ).
      so_chng[] = VALUE #( ( sign = 'I' option = 'BT' low = lv_last_run high = sy-datum ) ).
    ENDIF.

ENHANCEMENT-POINT /vpcoe/delivery_sel_scr_delta SPOTS /vpcoe/enh_delivery_bck .

  ENDIF.

AT SELECTION-SCREEN ON EXIT-COMMAND.

  INCLUDE /vpcoe/i_excel_valreq.
  lv_ucomm = sy-ucomm.

AT SELECTION-SCREEN OUTPUT.
  btn_delc = text-010."'Delete from Previous Selection ID'.
  btn_dela = text-011."'Delete all enteries'.

  IF lv_ucomm = 'BILDI'.
    IF p_bildi = abap_true.
      p_sbild = abap_true.
    ELSE.
      p_sbild = abap_false.
    ENDIF.
  ELSEIF lv_ucomm = 'BLDS'.
    IF p_sbild = abap_true.
      p_bildi = abap_true.
    ELSE.
      p_bildi = abap_false.
    ENDIF.
  ENDIF.

  lv_scr_fld_state = COND #( WHEN p_delta = abap_true THEN 0
                                                      ELSE 1 ).
  LOOP AT SCREEN.
    IF screen-name CS 'SO_TYPE' OR
       screen-name CS 'SO_ORG' OR
       screen-name CS 'SO_KUNNR' OR
       screen-name CS 'SO_CNTRY' OR
       screen-name CS 'SO_REG' OR
       screen-name CS 'SO_CATEG' OR
       screen-name CS 'SO_DISTR' OR
       screen-name CS 'SO_DIVIS' OR
       screen-name CS 'P_SNDC' OR
       screen-name CS 'SO_WADAT' OR
       screen-name CS 'SO_VBELN' OR
       screen-name CS 'SO_MATNR' OR
       screen-name CS 'SO_CNTR' OR
       screen-name CS 'SO_BILDI' OR
       screen-name CS 'SO_BDAT' OR
       screen-name CS 'SO_PCNTR' OR
       screen-name CS 'SO_BSDDC'.
      screen-input = lv_scr_fld_state.
      MODIFY SCREEN.
    ELSEIF screen-name CS 'SO_CNTRB'.
      screen-input = 0.
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_CSELID'.
      screen-input = screen-active = COND #( WHEN p_delta = abap_true THEN 0
                                                                      ELSE 1 ).

      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_PSELID'.
      screen-active = COND #( WHEN p_delta = abap_true THEN 1
                                                       ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_CCHP'.
      screen-active = COND #( WHEN p_sndc = abap_true THEN 1
                                                      ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_SBILD' OR
           screen-name CS 'P_SDLV' .
      screen-active = COND #( WHEN  r_send = abap_true  THEN 1
                                                         ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    CASE screen-group1.
      WHEN 'TRL'.
        screen-intensified = '1'.
        MODIFY SCREEN.

      WHEN 'BDI'.
        screen-active = COND #( WHEN p_bildi = abap_true THEN 1
                                                         ELSE 0 ).
        MODIFY SCREEN.

      WHEN 'SOP'.
        screen-active = COND #( WHEN r_send = abap_true THEN 1
                                                        ELSE 0 ).
        MODIFY SCREEN.

      WHEN 'CST'.
        screen-active = COND #( WHEN r_send = abap_true AND p_sdlv = abap_true THEN 1
                                                                               ELSE 0 ).
        IF r_send = abap_false OR p_sdlv = abap_false.
          p_sndc = abap_false.
        ENDIF.
        MODIFY SCREEN.

      WHEN 'SCP'.
        screen-active = COND #( WHEN p_sndc = abap_true AND r_send = abap_true
                                                        THEN 1
                                                        ELSE 0 ).
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

  INCLUDE /vpcoe/i_delta_ss_out.
  INCLUDE /vpcoe/version_set.
  INCLUDE /vpcoe/i_excel_scrout.

ENHANCEMENT-POINT /vpcoe/delivery_selscr_out SPOTS /vpcoe/enh_delivery_bck .

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_DELIVERY' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.
  IF p_cselid IS INITIAL.
    p_cselid = /vpcoe/cl_rdp_helper=>sc_grp_id-delivery && '-' && /vpcoe/cl_rdp_helper=>sc_service_id-delivery && '-' && sy-datum.
  ENDIF.
  go_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                              iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-delivery
                                              iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-delivery ).

  go_cust->get_delta_ids( IMPORTING et_sel_names = lt_sel_name ).
  LOOP AT lt_sel_name ASSIGNING <ls_sel_opt_id>.
    INSERT VALUE #( key  = <ls_sel_opt_id>
                    text = <ls_sel_opt_id>  ) INTO TABLE lt_selid_vrm.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_PSELID'
      values          = lt_selid_vrm
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    CLEAR lt_selid_vrm.
  ENDIF.
