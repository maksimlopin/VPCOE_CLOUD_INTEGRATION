interface /VPCOE/IF_XLS_PROCESSING
  public .


  interfaces IF_BADI_INTERFACE .

  methods ADJUST_FILE_SAVING
    importing
      !IV_BACKGROUND type ABAP_BOOL
      !IO_LOG type ref to /VPCOE/CL_rdp_LOG optional
    exporting
      !EV_SKIP_STANDARD_PROCESS type ABAP_BOOL
    changing
      !CV_FILE_PATH type STRING
      !CT_XLS_DATA type SOLIX_TAB .
endinterface.
