*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_DELTA_SS_OUT
*&---------------------------------------------------------------------*
  LOOP AT SCREEN.
    IF screen-name CS 'BTN_DELC'.
      screen-active = COND #( WHEN p_delta = abap_true THEN 1
                                                       ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'BTN_DELA'.
      screen-active = COND #( WHEN p_delta = abap_true THEN 1
                                                       ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
