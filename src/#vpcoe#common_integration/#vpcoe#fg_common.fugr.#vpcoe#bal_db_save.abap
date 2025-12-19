FUNCTION /vpcoe/bal_db_save.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_LOG_HANDLE) TYPE  BAL_T_LOGH
*"----------------------------------------------------------------------

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = it_log_handle
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc = 0.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.

ENDFUNCTION.
