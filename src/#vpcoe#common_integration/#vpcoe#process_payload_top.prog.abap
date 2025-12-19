*&---------------------------------------------------------------------*
*&  Include           /VPCOE/REPROCESS_PAYLOAD_TOP
*&---------------------------------------------------------------------*
TABLES: /vpcoe/api_type, /vpcoe/rdp_srvgr, /vpcoe/rdp_srvid.

DATA: lt_rows             TYPE lvc_t_row,
      lt_fun              TYPE ui_functions,

      ls_fieldcatalog     TYPE lvc_s_fcat OCCURS 0,
      ls_layout           TYPE lvc_s_layo,

      lo_custom_container TYPE REF TO cl_gui_custom_container,
      lo_table            TYPE REF TO cl_gui_alv_grid,
      lo_cust             TYPE REF TO /vpcoe/cl_common_helper,

      lt_bapiret2         TYPE bapiret2_t,
      lv_session_id       TYPE /vpcoe/session_id,
      lv_session_item     TYPE /vpcoe/jsn_cloud-session_item,
      lv_fcat             LIKE LINE OF ls_fieldcatalog,
      lv_date             TYPE dats,
      lv_time             TYPE tims,
      lv_container        TYPE scrfname VALUE 'CC_CONTAINER_GRID',
      lv_fun              TYPE ui_func,

      ls_interface        TYPE shlp_descr-interface.
