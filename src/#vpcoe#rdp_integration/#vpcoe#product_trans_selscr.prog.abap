*&---------------------------------------------------------------------*
*&  Include           /VPCOE/PRODUCT_TRANS_SELSCR
*&---------------------------------------------------------------------*
INCLUDE /vpcoe/version.
SELECTION-SCREEN BEGIN OF BLOCK delta WITH FRAME TITLE text-drl.
PARAMETERS: p_delta  TYPE xfeld AS CHECKBOX USER-COMMAND usrdelta,
            p_chgpid TYPE edi_mestyp DEFAULT '/VPCOE/PRODUCT'.
SELECTION-SCREEN END OF BLOCK delta.
SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-opt.
PARAMETERS: r_screen RADIOBUTTON GROUP req USER-COMMAND et,
            r_send   RADIOBUTTON GROUP req,
            r_excel  RADIOBUTTON GROUP req.
SELECTION-SCREEN END OF BLOCK options.
INCLUDE /vpcoe/i_excel_selscr.

INCLUDE /vpcoe/extension_sel_scr.

SELECTION-SCREEN BEGIN OF BLOCK seldata WITH FRAME TITLE text-gen.
SELECT-OPTIONS:
  so_nmbr    FOR mara-matnr, "Product
  so_type    FOR mara-mtart, "Product Type
  so_grp     FOR mara-matkl, "Product Group
  so_chng    FOR mara-laeda, "Change Date
  so_vlfr    FOR mara-datab, "Valid From
  so_vkorg   FOR mvke-vkorg, " Sales Organization
  so_werks   FOR marc-werks, " Plant
  so_mstae   FOR mara-mstae, " Plant Status
  so_mstde  FOR mara-mstde NO INTERVALS. " Valid From Plant Status
SELECTION-SCREEN END OF BLOCK seldata.

ENHANCEMENT-POINT /vpcoe/product_trans_sel_scr SPOTS /vpcoe/enh_product_trans .

SELECTION-SCREEN BEGIN OF BLOCK extdata WITH FRAME TITLE text-ext.
SELECT-OPTIONS:
  so_prcnt FOR gv_de_product_content NO DATABASE SELECTION,  "Product Content
  so_prth  FOR gv_product_type_harmon NO DATABASE SELECTION. "Product Type Harmonized
SELECTION-SCREEN END OF BLOCK extdata.

INCLUDE /vpcoe/i_excel_valreq.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF screen-name CS 'P_CHGPID'.
      screen-input = COND #( WHEN p_delta = abap_true THEN 1
                                                      ELSE 0 ).

      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 = 'TRL'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  INCLUDE /vpcoe/version_set.
  INCLUDE /vpcoe/i_excel_scrout.

ENHANCEMENT-POINT /vpcoe/product_sel_scr_out SPOTS /vpcoe/enh_product_trans .

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_PRODUCT' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.
