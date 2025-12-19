*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_EXCEL_FILE
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: lt_file TYPE filetable,
        lv_rc   TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      initial_directory = 'C:\'
      default_filename  = lv_default_name
      default_extension = 'xls'
    CHANGING
      file_table        = lt_file
      rc                = lv_rc.

  p_file = VALUE #( lt_file[ 1 ]-filename OPTIONAL ).
