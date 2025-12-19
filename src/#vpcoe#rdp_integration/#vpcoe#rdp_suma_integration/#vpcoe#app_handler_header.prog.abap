*&---------------------------------------------------------------------*
*&  Include           /VPCOE/APP_HANDLER
*&---------------------------------------------------------------------*
CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      run,
      pbo_0100,
      pai_0100,
      on_toolbar  IMPORTING e_object      TYPE REF TO cl_alv_event_toolbar_set
                            e_interactive TYPE char01,
      on_user_cmd IMPORTING e_ucomm TYPE syucomm.

  PRIVATE SECTION.
    CONSTANTS: c_fcode_del_sel TYPE syucomm VALUE 'DEL_SEL',
               c_fcode_del_all TYPE syucomm VALUE 'DEL_ALL',
               c_fcode_refresh TYPE syucomm VALUE 'REFRESH'.

    DATA: mt_rows      TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY,
          mo_container TYPE REF TO cl_gui_custom_container,
          mo_grid      TYPE REF TO cl_gui_alv_grid,
          mt_fcat      TYPE lvc_t_fcat,
          ms_layout    TYPE lvc_s_layo.

    METHODS:
      build_fcat,
      add_fcat       IMPORTING iv_field    TYPE lvc_fname
                               iv_checkbox TYPE abap_bool
                               iv_key      TYPE abap_bool
                               iv_text     TYPE scrtext_m
                               iv_len      TYPE lvc_outlen,
      read_data,
      alv_refresh,
      confirm        IMPORTING iv_text      TYPE string
                     RETURNING VALUE(rv_ok) TYPE abap_bool,
      delete_selected,
      delete_all,
      do_delete      IMPORTING it_del TYPE STANDARD TABLE.
ENDCLASS.
