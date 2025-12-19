interface /VPCOE/IF_FTD_OUTPUT_CONFIG
  public .


  methods SET_EXPORTING_PARAMETER
    importing
      !NAME type ABAP_PARMNAME
      !VALUE type ANY
    returning
      value(SELF) type ref to /VPCOE/IF_FTD_OUTPUT_CONFIG .
  methods SET_CHANGING_PARAMETER
    importing
      !NAME type ABAP_PARMNAME
      !VALUE type ANY
    returning
      value(SELF) type ref to /VPCOE/IF_FTD_OUTPUT_CONFIG .
  methods SET_TABLE_PARAMETER
    importing
      !NAME type ABAP_PARMNAME
      !VALUE type ANY
    returning
      value(SELF) type ref to /VPCOE/IF_FTD_OUTPUT_CONFIG .
endinterface.
