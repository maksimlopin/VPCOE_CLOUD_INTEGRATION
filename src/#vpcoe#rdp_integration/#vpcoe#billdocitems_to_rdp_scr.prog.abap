*&---------------------------------------------------------------------*
*&  Include           /VPCOE/BILLDOCITEMS_TO_RDP_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK seldata WITH FRAME TITLE text-001.
SELECT-OPTIONS:
 so_bildi FOR vbrk-vbeln MODIF ID bdi,
 so_bdat  FOR vbrk-fkdat MODIF ID bdi,
 so_bsddc FOR vbrp-vgtyp MODIF ID bdi,
  so_cntrb FOR t001w-land1 MODIF ID bdi DEFAULT 'ES'.
SELECTION-SCREEN END OF BLOCK seldata.

SELECTION-SCREEN BEGIN OF BLOCK matdoc WITH FRAME TITLE text-001.
SELECT-OPTIONS:
   so_mdoc  FOR mseg-mblnr,
   so_sesid FOR /vpcoe/billdi-session_id.
SELECTION-SCREEN END OF BLOCK matdoc.

SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-opt.
PARAMETERS: r_screen RADIOBUTTON GROUP req USER-COMMAND et,
            r_send   RADIOBUTTON GROUP req,
            r_del    RADIOBUTTON GROUP req.
SELECTION-SCREEN END OF BLOCK options.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'TRL'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  INCLUDE /vpcoe/version_set.

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_BILLDI' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.
