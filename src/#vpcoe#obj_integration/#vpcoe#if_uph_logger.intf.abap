INTERFACE /vpcoe/if_uph_logger
  PUBLIC .


  METHODS add_messages
    IMPORTING
      !it_messages TYPE /vpcoe/t_uph_msg .
  METHODS get_messages
    EXPORTING
      !et_messages TYPE /vpcoe/t_uph_msg .
  METHODS commit_application_log
    IMPORTING
      !iv_test_mode  TYPE boole_d DEFAULT abap_false
      !iv_flg_commit TYPE abap_bool DEFAULT abap_false .
  METHODS get_app_log_object
    EXPORTING
      !ev_object    TYPE balobj_d
      !ev_subobject TYPE balsubobj .
  METHODS has_error_messages
    RETURNING
      VALUE(rv_flg_has_errors) TYPE abap_bool .
  METHODS has_warning_messages
    RETURNING
      VALUE(rv_flg_has_warnings) TYPE abap_bool .
  METHODS add_bapi_messages
    IMPORTING
      !it_bapi_messages TYPE bapiret2_t .
  METHODS get_bapi_messages
    RETURNING
      VALUE(rt_bapi_messages) TYPE bapiret2_t .
  METHODS write_to_console .
  METHODS add_http_response
    IMPORTING
      !is_response TYPE /vpcoe/cl_plm_http=>gty_s_response
      !iv_reason   TYPE string OPTIONAL .
ENDINTERFACE.
