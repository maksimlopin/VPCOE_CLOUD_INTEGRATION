*&---------------------------------------------------------------------*
*&  Include           /VPCOE/SUPPLIER_TRANS_SELSCR
*&---------------------------------------------------------------------*
INCLUDE /vpcoe/version.
*INCLUDE /vpcoe/barea.
SELECTION-SCREEN BEGIN OF BLOCK delta WITH FRAME TITLE text-drl.
PARAMETERS: p_delta  TYPE xfeld AS CHECKBOX USER-COMMAND usrdelta,
            p_chgpid TYPE edi_mestyp DEFAULT '/VPCOE/SUPPLIER'.
SELECTION-SCREEN END OF BLOCK delta.
SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-opt.
PARAMETERS: r_screen RADIOBUTTON GROUP req USER-COMMAND et,
            r_send   RADIOBUTTON GROUP req,
            r_excel  RADIOBUTTON GROUP req.
INCLUDE /vpcoe/i_excel_selscr.
SELECTION-SCREEN END OF BLOCK options.

SELECTION-SCREEN BEGIN OF BLOCK seldata WITH FRAME TITLE text-spr.
SELECT-OPTIONS:
  so_id    FOR lfa1-lifnr, "Supplier Id
  so_bukrs FOR lfb1-bukrs, " Supplier Company Code
  so_cntry FOR lfa1-land1, "Country
  so_reg   FOR lfa1-regio, "Region
  so_crt   FOR cdhdr-udate,"Creation Date
  so_chng  FOR cdhdr-udate,"Change Date
  so_ekorg FOR lfm1-ekorg,
  so_ktokk FOR lfa1-ktokk.
SELECTION-SCREEN END OF BLOCK seldata.

ENHANCEMENT-POINT /vpcoe/supplier_sel_scr SPOTS /vpcoe/enh_supplier .

INCLUDE /vpcoe/i_excel_valreq.

AT SELECTION-SCREEN OUTPUT.

*  IF p_delta = abap_true.
*    CLEAR: so_id[],
*           so_cntry[],
*           so_reg[],
*           so_bukrs[].
*  ENDIF.

  LOOP AT SCREEN.

    IF screen-name CS 'P_CHGPID'.
      screen-input = COND #( WHEN p_delta = abap_true THEN 1
                                                      ELSE 0 ).

      MODIFY SCREEN.

*    ELSEIF screen-name CS 'SO_ID' OR
*           screen-name CS 'SO_CNTRY' OR
*           screen-name CS 'SO_REG' OR
*           screen-name CS 'SO_EKORG' OR
*           screen-name CS 'SO_KTOKK' OR
*           screen-name CS 'SO_BUKRS'.
*      screen-input = COND #( WHEN p_delta = abap_true THEN 0
*                                                      ELSE 1 ).
*
*      MODIFY SCREEN.

    ENDIF.
    IF screen-group1 = 'TRL'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  INCLUDE /vpcoe/version_set.
  INCLUDE /vpcoe/i_excel_scrout.

ENHANCEMENT-POINT /vpcoe/supplier_sel_scr_out SPOTS /vpcoe/enh_supplier .

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_SUPPLIER' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.
