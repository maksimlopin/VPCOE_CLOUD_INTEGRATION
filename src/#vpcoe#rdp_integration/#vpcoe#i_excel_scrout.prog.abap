*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_EXCEL_OUT
*&---------------------------------------------------------------------*

  LOOP AT SCREEN.
    IF screen-name CS 'R_LOCAL'.
      screen-active = COND #( WHEN  r_excel = abap_true THEN 1
                                                       ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'R_SERVER'.
      screen-active = COND #( WHEN  r_excel = abap_true THEN 1
                                                       ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_FILE'.
      screen-active = COND #( WHEN  r_excel = abap_true  THEN 1
                                                         ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF lv_ucomm = 'FILETRG'.
    CLEAR: p_file, lv_ucomm.
  ENDIF.
