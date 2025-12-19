*&---------------------------------------------------------------------*
*&  Include           /VPCOE/TRACE_MAINT_HANDLER
*&---------------------------------------------------------------------*
CLASS lcl_trace_maint DEFINITION.
  PUBLIC SECTION.

    DATA: mt_delete_trace TYPE STANDARD TABLE OF /vpcoe/trace.

    METHODS:
      constructor,

      delete_entries,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_trace_maint IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD handle_toolbar.

    INSERT VALUE #( function  = 'DEL'
                    icon      = icon_remove
                    text      = text-003
                    quickinfo = 'Delete'
                    disabled  = ' ' ) INTO TABLE e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'DEL'.
        go_grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).
        go_grid->set_toolbar_interactive( ).

        LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<ls_row>).
          mt_delete_trace = VALUE #( BASE mt_delete_trace ( gt_trace[ <ls_row>-index ] ) ).
          DELETE gt_trace INDEX <ls_row>-index.
        ENDLOOP.

        me->delete_entries( ).
        go_grid->refresh_table_display( ).

    ENDCASE.
  ENDMETHOD.

  METHOD delete_entries.
    DATA: lv_answer TYPE char1.

    DATA(lv_count) = lines( mt_delete_trace ).
    IF lv_count <= 0.
      MESSAGE i013(/vpcoe/common).
      RETURN.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Information'
        text_question         = |Delete { lv_count } entries - are you sure?|
*        text_button_1         = 'Yes'
*        text_button_2         = 'No'
        default_button        = '2'
        display_cancel_button = 'X'
      IMPORTING
        answer                = lv_answer.

    IF lv_answer <> '1'.
      MESSAGE i008(/vpcoe/common).
      LEAVE PROGRAM.
    ENDIF.

    DELETE /vpcoe/trace FROM TABLE @mt_delete_trace.
    COMMIT WORK.
    MESSAGE i096(/vpcoe/common).
    CLEAR: mt_delete_trace.

  ENDMETHOD.

ENDCLASS.
