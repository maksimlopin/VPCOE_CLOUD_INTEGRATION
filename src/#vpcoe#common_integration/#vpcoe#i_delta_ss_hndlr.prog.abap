*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_DELTA_SS_HNDLR
*&---------------------------------------------------------------------*
IF sscrfields-ucomm = 'CMD_ONE' OR sscrfields-ucomm = 'CMD_ALL'.
  IF p_pselid IS NOT INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Please confirm the deletion.'(ttl)
        text_question         = COND text255( WHEN sscrfields-ucomm = 'CMD_ONE' THEN 'Selected ID will be deleted. Continue?'(dcu)
                                                                                ELSE 'All Selection IDs will be deleted. Continue?'(dal) )
        text_button_1         = text-yes
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = text-no1
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = space
        popup_type            = 'ICON_MESSAGE_WARNING'
      IMPORTING
        answer                = lv_answer.

    lt_r_sel_name = VALUE #( ( sign   = 'I'
                               option = COND #( WHEN sscrfields-ucomm = 'CMD_ONE' THEN 'EQ'
                                                                                  ELSE 'NE' )
                               low    = p_pselid ) ).
    IF lv_answer = '1'.
      CLEAR: lt_selid_vrm, p_pselid, lt_sel_name.
      go_cust->delete_delta_ids( EXPORTING it_r_sel_name = lt_r_sel_name
                                 IMPORTING et_bapiret2   = gt_bapiret2 ).
      IF line_exists( gt_bapiret2[ type = 'E' ] ).
        MESSAGE e095(/vpcoe/common).
        RETURN.
      ENDIF.

      go_cust->get_delta_ids( IMPORTING et_sel_names = lt_sel_name ).

      LOOP AT lt_sel_name ASSIGNING FIELD-SYMBOL(<ls_sel_opt>).
        INSERT VALUE #( key  = <ls_sel_opt>
                        text = <ls_sel_opt>  ) INTO TABLE lt_selid_vrm.
      ENDLOOP.

      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id              = 'P_PSELID'
          values          = lt_selid_vrm
        EXCEPTIONS
          id_illegal_name = 1
          OTHERS          = 2.

      MESSAGE s096(/vpcoe/common).
    ELSE.
      MESSAGE w095(/vpcoe/common).
    ENDIF.

  ELSE.
    MESSAGE i005(/vpcoe/common) DISPLAY LIKE 'W'.

  ENDIF.

ENDIF.
