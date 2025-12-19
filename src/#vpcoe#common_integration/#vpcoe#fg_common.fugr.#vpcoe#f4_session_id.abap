FUNCTION /vpcoe/f4_session_id.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  CASE callcontrol-step.
    WHEN 'DISP'.
      SORT record_tab.
      DELETE ADJACENT DUPLICATES FROM record_tab.
  ENDCASE.

ENDFUNCTION.
