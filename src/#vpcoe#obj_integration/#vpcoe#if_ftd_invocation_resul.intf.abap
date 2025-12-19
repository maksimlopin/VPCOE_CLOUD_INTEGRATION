interface /VPCOE/IF_FTD_INVOCATION_RESUL
  public .


  methods GET_OUTPUT_CONFIGURATION
    returning
      value(RESULT) type ref to /VPCOE/IF_FTD_OUTPUT_CONFIG .
  methods RAISE_EXCEPTION
    importing
      !EXCEPTION type ref to CX_ROOT .
  methods RAISE_CLASSIC_EXCEPTION
    importing
      !CLASSIC_EXCEPTION type ABAP_EXCPNAME .
endinterface.
