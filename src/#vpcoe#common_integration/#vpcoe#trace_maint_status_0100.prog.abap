*&---------------------------------------------------------------------*
*&  Include           /VPCOE/TRACE_MAINT_STATUS_0100
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MAIN100'.

  IF go_container IS INITIAL.

    go_container = NEW #( container_name = 'CC_ALV' ).

    go_grid = NEW #( i_parent = go_container ).

    PERFORM build_fieldcatalog.

    gs_layout-zebra = 'X'.
    gs_layout-sel_mode = 'A'.

    LOOP AT gt_trace ASSIGNING FIELD-SYMBOL(<ls_trace>).
      <ls_trace>-object_id = shift_left( val = <ls_trace>-object_id sub = '0' ).
    ENDLOOP.

    go_grid->set_table_for_first_display(
        EXPORTING is_layout       = gs_layout
         CHANGING it_outtab       = gt_trace
                  it_fieldcatalog = gt_fieldcat ).

    SET HANDLER lo_trace_maint_hendler->handle_toolbar FOR go_grid.
    SET HANDLER lo_trace_maint_hendler->handle_user_command FOR go_grid.
    go_grid->set_toolbar_interactive( ).

  ENDIF.

ENDMODULE.
