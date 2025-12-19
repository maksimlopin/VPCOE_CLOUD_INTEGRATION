interface /VPCOE/IF_UPH_EXEC_LOAD_WRAP
  public .


  methods EXECUTE_UPLOAD
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IS_PARAMETERS type ANY optional
      !IV_TEST type /VPCOE/EHFND_BOOL optional .
endinterface.
