*&---------------------------------------------------------------------*
*&  Include           /VPCOE/UPH_PCKG_MCL_SLSCRN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME  TITLE text-005.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME  TITLE text-005.
" Full or Delta mode radiobuttons
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11) text-002 FOR FIELD p_iload.
PARAMETERS p_iload TYPE char1 RADIOBUTTON GROUP r2 DEFAULT 'X' USER-COMMAND fcode1.
SELECTION-SCREEN COMMENT 31(10) text-003 FOR FIELD p_dload.
PARAMETERS p_dload TYPE char1 RADIOBUTTON GROUP r2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

" Test mode
PARAMETERS p_test TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.
PARAMETERS:
  p_source TYPE /vpcoe/uph_source_id LOWER CASE VISIBLE LENGTH 36 MODIF ID m2,
  p_rfcdes TYPE /vpcoe/uph_rfcdest VISIBLE LENGTH 16 MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.

SELECT-OPTIONS s_mat     FOR lv_material        MODIF ID m1.
SELECT-OPTIONS s_mattyp  FOR lv_material_type   MODIF ID m1 MATCHCODE OBJECT h_t134.
SELECT-OPTIONS s_matgrp  FOR lv_material_group  MODIF ID m1 MATCHCODE OBJECT h_t023.
SELECT-OPTIONS s_chdate  FOR sy-datum           MODIF ID m1.
SELECT-OPTIONS s_valdat  FOR sy-datum           MODIF ID m3 NO-EXTENSION DEFAULT '00010101' TO '99991231'.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_iload IS NOT INITIAL.
      IF screen-group1 = gc_screen_grp1_m1.
        screen-input = 1.
      ENDIF.
    ELSEIF p_dload IS NOT INITIAL.
      IF    screen-group1 = gc_screen_grp1_m1
         OR screen-group1 = gc_screen_grp1_m2.
        screen-input  = 0.
        screen-active = 0.
      ENDIF.
    ENDIF.
    IF screen-group1 = gc_screen_grp1_m3.
      IF lv_display_validity = abap_true AND p_iload IS NOT INITIAL.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.

  IF sy-ucomm = gc_ucomm_display_validity.
    lv_display_validity = abap_true.
  ENDIF.

  IF sy-ucomm = gc_ucomm_onli.

    " Perform mandatory field checks
    IF p_source IS INITIAL AND p_dload IS INITIAL.
      MESSAGE s027(/vpcoe/plm).
      CLEAR sscrfields.
      STOP.
    ENDIF.

    IF p_rfcdes IS INITIAL AND p_dload IS INITIAL.
      MESSAGE s032(/vpcoe/plm).
      CLEAR sscrfields.
      STOP.
    ENDIF.

    IF s_valdat IS INITIAL AND p_dload IS INITIAL.
      MESSAGE s041(/vpcoe/plm).
      CLEAR : sscrfields.
      STOP.
    ENDIF.

    " If not running in background: Check if no selection criteria is given and issue warning
    IF     sy-batch = abap_false AND p_iload = abap_true
       AND (     s_mat    IS INITIAL
             AND s_mattyp IS INITIAL
             AND s_matgrp IS INITIAL
             AND s_chdate IS INITIAL ).

      MESSAGE s000(/vpcoe/plm) INTO DATA(lv_msg_txt).

      DATA lv_answer TYPE char1.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = lv_msg_txt
          display_cancel_button = ''
        IMPORTING
          answer                = lv_answer.

      IF lv_answer <> '1'.
        CLEAR sy-ucomm.
        STOP.
      ENDIF.
    ENDIF.

  ENDIF.
