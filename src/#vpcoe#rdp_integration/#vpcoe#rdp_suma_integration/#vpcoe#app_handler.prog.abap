*&---------------------------------------------------------------------*
*&  Include           /VPCOE/APP_HANDLER
*&---------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    CLEAR: mt_rows, mt_fcat, ms_layout.
  ENDMETHOD.

  METHOD run.
    CALL SCREEN 100.
  ENDMETHOD.

  METHOD pbo_0100.

    IF mo_container IS INITIAL.
      mo_container = NEW #( container_name = 'CC_ALV' ).
      mo_grid = NEW #( i_parent = mo_container ).

      build_fcat( ).
      ms_layout-zebra      = abap_true.
      ms_layout-sel_mode = 'A'.

      DATA(lo_evt) = NEW lcl_events( me ).
      SET HANDLER lo_evt->on_toolbar  FOR mo_grid.
      SET HANDLER lo_evt->on_user_cmd FOR mo_grid.

      read_data( ).

      mo_grid->set_table_for_first_display(
        EXPORTING
          is_layout       = ms_layout
        CHANGING
          it_outtab       = mt_rows
          it_fieldcatalog = mt_fcat
      ).
      mo_grid->set_toolbar_interactive( ).
    ENDIF.
  ENDMETHOD.

  METHOD pai_0100.
    CASE sy-ucomm.
      WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD on_toolbar.

    e_object->mt_toolbar =  VALUE #(
                                      ( function  =  c_fcode_del_sel
                                       icon      =  icon_delete_row
                                       text      =  'Delete selected'
                                       quickinfo =  'Delete selected rows'
                                       disabled  = ' ' )
                                     ( function  = c_fcode_del_all
                                       icon      = icon_delete
                                       text      = 'Delete all (filtered)'
                                       quickinfo = 'Delete all rows from current selection'
                                       disabled  = ' ' )
                                     ( function  = c_fcode_refresh
                                       icon      = icon_refresh
                                       text      = 'Refresh'
                                       quickinfo = 'Reload data by selection screen'
                                       disabled  = ' ' ) ).

  ENDMETHOD.

  METHOD on_user_cmd.
    CASE e_ucomm.
      WHEN c_fcode_del_sel.
        delete_selected( ).
      WHEN c_fcode_del_all.
        delete_all( ).
      WHEN c_fcode_refresh.
        read_data( ).
        alv_refresh( ).
    ENDCASE.
  ENDMETHOD.

  METHOD build_fcat.
    CLEAR mt_fcat.
    add_fcat( iv_field = 'MARK'           iv_checkbox = abap_true  iv_key = abap_false iv_text = 'Sel.'            iv_len = 1  ).
    add_fcat( iv_field = 'REPLICATION_ID' iv_checkbox = abap_false iv_key = abap_false iv_text = 'Run ID'         iv_len = 36 ).
    add_fcat( iv_field = 'GRP_ID'         iv_checkbox = abap_false iv_key = abap_false iv_text = 'Service Group'  iv_len = 10 ).
    add_fcat( iv_field = 'SRV_ID'         iv_checkbox = abap_false iv_key = abap_false iv_text = 'Service ID'     iv_len = 40 ).
    add_fcat( iv_field = 'REQUESTED_ON'   iv_checkbox = abap_false iv_key = abap_false iv_text = 'Date'           iv_len = 8  ).
    add_fcat( iv_field = 'REQUESTED_BY'   iv_checkbox = abap_false iv_key = abap_false iv_text = 'User'           iv_len = 12 ).
  ENDMETHOD.

  METHOD add_fcat.
    DATA ls_fcat TYPE lvc_s_fcat.
    CLEAR ls_fcat.
    ls_fcat-fieldname = iv_field.
    ls_fcat-scrtext_m = iv_text.
    ls_fcat-outputlen = iv_len.
    IF iv_checkbox = abap_true.
      ls_fcat-checkbox = abap_true.
      ls_fcat-edit     = abap_true.
    ENDIF.
    IF iv_key = abap_true.
      ls_fcat-key = abap_true.
    ENDIF.
    APPEND ls_fcat TO mt_fcat.
  ENDMETHOD.

  METHOD read_data.
    CLEAR mt_rows.
    SELECT replication_id
           grp_id
           srv_id
           requested_on
           requested_by
      FROM /vpcoe/rdp_runid
      INTO CORRESPONDING FIELDS OF TABLE mt_rows
      WHERE requested_on IN s_date
        AND requested_by IN s_user.

  ENDMETHOD.

  METHOD alv_refresh.
    IF mo_grid IS BOUND.
      mo_grid->refresh_table_display(
        EXPORTING
          is_stable = VALUE lvc_s_stbl( row = 'X' col = 'X' )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD confirm.
    DATA lv_answer TYPE c.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmation'
        text_question         = iv_text
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        display_cancel_button = space
      IMPORTING
        answer                = lv_answer.
    rv_ok = xsdbool( lv_answer = '1' ).
  ENDMETHOD.

  METHOD delete_selected.

    DATA lt_del TYPE STANDARD TABLE OF /vpcoe/rdp_runid.

    IF confirm( CONV #( text-004 ) ) = abap_false.
      RETURN.
    ENDIF.

    DELETE mt_rows WHERE mark = abap_false.

    IF mt_rows IS INITIAL.
      MESSAGE i005(/vpcoe/common).
      RETURN.
    ENDIF.

    do_delete( mt_rows ).

  ENDMETHOD.

  METHOD delete_all.
    IF confirm( CONV #( text-005 ) ) = abap_false.
      RETURN.
    ENDIF.
    do_delete( mt_rows ).
  ENDMETHOD.

  METHOD do_delete.
    DATA lt_del TYPE STANDARD TABLE OF /vpcoe/rdp_runid.

    lt_del = CORRESPONDING #( it_del ).
    LOOP AT lt_del ASSIGNING FIELD-SYMBOL(<ls_grp>) GROUP BY ( grp_id = <ls_grp>-grp_id ).
      LOOP AT GROUP <ls_grp> ASSIGNING FIELD-SYMBOL(<ls_srv>) GROUP BY ( srv_id = <ls_srv>-srv_id ).

        DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper(
          iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
          iv_srv_grp  = <ls_grp>-grp_id
          iv_srv_id   = <ls_srv>-srv_id ).

        LOOP AT GROUP <ls_srv> ASSIGNING FIELD-SYMBOL(<ls_del>).
          /vpcoe/cl_rdp_suma_helper=>get_instance( io_rdp_helper = lo_cust
                                                   iv_srv_prfx   = CONV #( <ls_srv>-srv_id ) )->cancel_replication(
            EXPORTING
              iv_replication_id = <ls_del>-replication_id ).
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    DELETE /vpcoe/rdp_runid FROM TABLE @lt_del.
    COMMIT WORK.
    MESSAGE i096(/vpcoe/common).
    read_data( ).
    alv_refresh( ).

  ENDMETHOD.
ENDCLASS.
