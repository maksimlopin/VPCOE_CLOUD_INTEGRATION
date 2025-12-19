interface /VPCOE/IF_UPH_REPORT
  public .


  constants GC_DEFAULT_RFC_DESTINATION type RFCDEST value 'RDP_REPL_API' ##NO_TEXT.

  methods DERIVE_RFC_TARGET_DESTINATION
    returning
      value(RV_VALUE) type RFCDEST .
  methods DERIVE_UPLOAD_MODE
    importing
      !IC_INITIAL_LOAD type C1
      !IC_DELTA_LOAD type C1
    returning
      value(RV_VALUE) type /VPCOE/UPLOAD_MODE .
  methods EXECUTE_UPLOAD
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IS_PARAMETERS type ANY optional
      !IV_TEST type ABAP_BOOL optional .
endinterface.
