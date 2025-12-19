*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_CUSTOMER_TO_CLOUD_SEL
*&---------------------------------------------------------------------*
INCLUDE /vpcoe/version.
SELECTION-SCREEN BEGIN OF BLOCK delta WITH FRAME TITLE text-dlr.
PARAMETERS: p_delta  TYPE xfeld AS CHECKBOX USER-COMMAND usrdelta,
            p_chgpid TYPE edi_mestyp DEFAULT '/VPCOE/CUSTOMER'.
SELECTION-SCREEN END OF BLOCK delta.
SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-opt.
PARAMETERS: r_screen RADIOBUTTON GROUP req USER-COMMAND et,
            r_send   RADIOBUTTON GROUP req,
            r_excel  RADIOBUTTON GROUP req.
INCLUDE /vpcoe/i_excel_selscr.
SELECTION-SCREEN END OF BLOCK options.

INCLUDE /vpcoe/extension_sel_scr.

SELECTION-SCREEN BEGIN OF BLOCK seldata WITH FRAME TITLE text-005.
SELECT-OPTIONS:
  so_id    FOR kna1-kunnr, " CustomerId
  so_bukrs FOR knb1-bukrs, " Company Code
  so_cntry FOR kna1-land1, " Country
  so_reg   FOR kna1-regio, " Region
  so_crt   FOR cdhdr-udate," Creation Date
  so_chng  FOR cdhdr-udate," Change Date
  so_vkorg FOR knvv-vkorg,
  so_ktotd FOR kna1-ktokd.
SELECTION-SCREEN END OF BLOCK seldata.

ENHANCEMENT-POINT /vpcoe/customer_sel_scr SPOTS /vpcoe/enh_customer .

SELECTION-SCREEN BEGIN OF BLOCK extdata WITH FRAME TITLE text-ext.
SELECT-OPTIONS:
  so_role  FOR gv_customer_role NO DATABASE SELECTION.
SELECTION-SCREEN END OF BLOCK extdata.
