*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_COMMAND
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  IF r_local = abap_true.
    DATA: lt_file TYPE filetable,
          lv_rc   TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
       EXPORTING
         initial_directory = 'C:\'
         default_filename  = gv_default
         default_extension = 'xls'
       CHANGING
         file_table        = lt_file
         rc                = lv_rc ).

    p_file = VALUE #( lt_file[ 1 ]-filename OPTIONAL ).
  ELSE.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = p_file
        filemask         = '*'
      IMPORTING
        serverfile       = p_file
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
