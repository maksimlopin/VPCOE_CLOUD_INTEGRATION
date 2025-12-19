*&---------------------------------------------------------------------*
*&  Include           /VPCOE/PCKF_SEL_SCR_CSTM_EXMP
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME  TITLE text-001.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME  TITLE text-001.

"Full or Delta mode radiobuttons
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11) text-002 FOR FIELD p_iload .
PARAMETERS : p_iload TYPE char1 RADIOBUTTON GROUP r2 DEFAULT 'X' USER-COMMAND fcode1 .
SELECTION-SCREEN COMMENT 31(10) text-003 FOR FIELD p_dload .
PARAMETERS : p_dload TYPE char1 RADIOBUTTON GROUP r2 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

"Test mode
PARAMETERS p_test TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
PARAMETERS: p_cstm TYPE char40 VISIBLE LENGTH 40 .
PARAMETERS: p_ctgr TYPE char40 VISIBLE LENGTH 40 .
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN OUTPUT.

  IF p_dload = abap_true.
    CLEAR: p_cstm, p_ctgr.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name CS 'P_CSTM' OR screen-name CS 'P_CTGR'.
      screen-input = COND #( WHEN p_dload = abap_true THEN 0
                                                      ELSE 1 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
