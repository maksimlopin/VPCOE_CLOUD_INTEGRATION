*&---------------------------------------------------------------------*
*& Report  /VPCOE/TRACE_MAINTENANCE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/trace_maintenance.

INCLUDE /vpcoe/version.
TABLES: /vpcoe/trace, /vpcoe/rdp_srvid.


DATA: gt_trace     TYPE TABLE OF /vpcoe/trace WITH DEFAULT KEY,
      gs_trace     LIKE LINE OF gt_trace,
      go_container TYPE REF TO cl_gui_custom_container,
      go_grid      TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fieldcat  TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo,
      gt_sel_rows  TYPE lvc_t_row WITH HEADER LINE,
      gv_count     TYPE int4,
      gt_api       TYPE STANDARD TABLE OF vrm_value WITH DEFAULT KEY,
      gt_status    TYPE STANDARD TABLE OF vrm_value WITH DEFAULT KEY.

INCLUDE /vpcoe/trace_maint_handler.
DATA: lo_trace_maint_hendler TYPE REF TO lcl_trace_maint.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_api TYPE /vpcoe/rdp_srvid-api_type AS LISTBOX VISIBLE LENGTH 10 OBLIGATORY.
SELECT-OPTIONS:
         s_group        FOR /vpcoe/rdp_srvid-service_grp,
         s_sid          FOR /vpcoe/rdp_srvid-service_id,
         s_oid          FOR /vpcoe/trace-object_id,
         s_date         FOR /vpcoe/trace-last_sended_date,
         s_time         FOR /vpcoe/trace-last_sended_time.
PARAMETERS: p_status TYPE /vpcoe/de_status AS LISTBOX VISIBLE LENGTH 30 OBLIGATORY LOWER CASE.
PARAMETERS: p_del TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

  LOOP AT SCREEN.
    IF screen-group1 = 'TRL'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

INITIALIZATION.

  INCLUDE /vpcoe/version_set.

  SELECT *
   FROM /vpcoe/api_type
   INTO TABLE @DATA(lt_api).

  gt_api = VALUE #( FOR <ls_api> IN lt_api ( key = <ls_api>-api_type text = <ls_api>-api_type ) ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_API'
      values          = gt_api
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc = 0.
    p_api = /vpcoe/cl_rdp_helper=>sc_api_type-rdp.
  ENDIF.

  gt_status = VALUE #( ( key = /vpcoe/cl_common_helper=>sc_status-success text = /vpcoe/cl_common_helper=>sc_status-success )
                       ( key = /vpcoe/cl_common_helper=>sc_status-failed text = /vpcoe/cl_common_helper=>sc_status-failed )  ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_STATUS'
      values          = gt_status
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc = 0.
    p_status = /vpcoe/cl_common_helper=>sc_status-success.
  ENDIF.

START-OF-SELECTION.

  lo_trace_maint_hendler = NEW #( ).

  SELECT COUNT(*)
    FROM /vpcoe/trace
    INTO @gv_count
    WHERE object_id        IN @s_oid
      AND service_group    IN @s_group
      AND service_id       IN @s_sid
      AND last_sended_date IN @s_date
      AND last_sended_time IN @s_time
      AND api_type          = @p_api
      AND status            = @p_status.

  IF sy-subrc <> 0.
    MESSAGE i015(/vpcoe/common).
    RETURN.
  ENDIF.

  IF gv_count > 50000.
    MESSAGE i094(/vpcoe/common) WITH gv_count.
    RETURN.
  ENDIF.

  SELECT *
    FROM /vpcoe/trace
    INTO CORRESPONDING FIELDS OF TABLE @gt_trace
    WHERE object_id        IN @s_oid
      AND service_group    IN @s_group
      AND service_id       IN @s_sid
      AND last_sended_date IN @s_date
      AND last_sended_time IN @s_time
      AND api_type          = @p_api
      AND status            = @p_status.

  IF sy-subrc <> 0.
    MESSAGE i015(/vpcoe/common).
    RETURN.
  ENDIF.

  IF p_del = abap_true.
    lo_trace_maint_hendler->mt_delete_trace = gt_trace.
    lo_trace_maint_hendler->delete_entries( ).
    RETURN.
  ENDIF.

  CALL SCREEN 100.

  INCLUDE /vpcoe/trace_maint_status_0100.
  INCLUDE /vpcoe/trace_maint_usr_0100.
  INCLUDE /vpcoe/trace_maint_fillf01.
