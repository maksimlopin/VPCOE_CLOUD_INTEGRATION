*----------------------------------------------------------------------*
***INCLUDE /VPCOE/PROCESS_PAYLOAD_STATO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '/VPCOE/STANDART'.
  SET TITLEBAR '/VPCOE/TITLE'.

  IF lo_custom_container IS INITIAL.

    CREATE OBJECT lo_custom_container EXPORTING container_name = lv_container.
    CREATE OBJECT lo_table EXPORTING i_parent = lo_custom_container.

    lv_fun = cl_gui_alv_grid=>mc_fc_maximum.
    APPEND lv_fun TO lt_fun.
    lv_fun = cl_gui_alv_grid=>mc_fc_minimum.
    APPEND lv_fun TO lt_fun.
    lv_fun = cl_gui_alv_grid=>mc_fc_subtot.
    APPEND lv_fun TO lt_fun.
    lv_fun = cl_gui_alv_grid=>mc_fc_sum.
    APPEND lv_fun TO lt_fun.
    lv_fun = cl_gui_alv_grid=>mc_fc_average.
    APPEND lv_fun TO lt_fun.
    lv_fun = cl_gui_alv_grid=>mc_fc_print_back.
    APPEND lv_fun TO lt_fun.
    lv_fun = cl_gui_alv_grid=>mc_fc_detail.
    APPEND lv_fun TO lt_fun.
    lv_fun = cl_gui_alv_grid=>mc_mb_view.
    APPEND lv_fun TO lt_fun.
    lv_fun = cl_gui_alv_grid=>mc_fc_graph.
    APPEND lv_fun TO lt_fun.
    lv_fun = cl_gui_alv_grid=>mc_fc_info.
    APPEND lv_fun TO lt_fun.

    ls_layout-zebra = 'X'.
    ls_layout-sel_mode = 'A'.

    PERFORM fill_column.

    lo_table->set_table_for_first_display( EXPORTING is_layout            = ls_layout
                                                     it_toolbar_excluding = lt_fun
                                           CHANGING  it_fieldcatalog      = ls_fieldcatalog
                                                     it_outtab            = lo_payload_handler->mt_json_selected ).

    SET HANDLER lo_payload_handler->handle_user_command FOR lo_table.
    SET HANDLER lo_payload_handler->handle_toolbar FOR lo_table.
    lo_table->set_toolbar_interactive( ).
    cl_gui_control=>set_focus( EXPORTING control = lo_table ).
  ELSE.
    lo_table->refresh_table_display( ).
  ENDIF.
ENDMODULE.
