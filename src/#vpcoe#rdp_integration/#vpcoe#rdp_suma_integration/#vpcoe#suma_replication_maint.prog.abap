*&---------------------------------------------------------------------*
*& Report  /VPCOE/TRACE_MAINTENANCE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/suma_replication_maint.

INCLUDE /vpcoe/version.

*INCLUDE /vpcoe/suma_rep_maint_handler.

TABLES: /vpcoe/rdp_runid.

TYPES: BEGIN OF ty_row,
         mark    TYPE abap_bool,
         replication_id TYPE /vpcoe/de_run_id,
         grp_id         TYPE /vpcoe/de_service_group,
         srv_id         TYPE /vpcoe/de_service_id,
         requested_on   TYPE dats,
         requested_by   TYPE uname,
       END OF ty_row.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_date FOR /vpcoe/rdp_runid-requested_on,
                s_user FOR /vpcoe/rdp_runid-requested_by.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

  LOOP AT SCREEN.
    IF screen-group1 = 'TRL'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


  INCLUDE /vpcoe/app_handler_header.

CLASS lcl_events DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_app TYPE REF TO lcl_app,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_user_cmd FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
  PRIVATE SECTION.
    DATA mo_app TYPE REF TO lcl_app.
ENDCLASS.

INCLUDE /vpcoe/app_handler.

CLASS lcl_events IMPLEMENTATION.
  METHOD constructor.
    mo_app = io_app.
  ENDMETHOD.

  METHOD on_toolbar.
    mo_app->on_toolbar( e_object = e_object e_interactive = e_interactive ).
  ENDMETHOD.

  METHOD on_user_cmd.
    mo_app->on_user_cmd( e_ucomm = e_ucomm ).
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  DATA(go_app) = NEW lcl_app( ).

START-OF-SELECTION.
  go_app->run( ).

  INCLUDE /vpcoe/suma_rep_m_status_0100.
  INCLUDE /vpcoe/suma_rep_main_usr_0100.
