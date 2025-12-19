interface /VPCOE/IF_FTD_INPUT_CONFIGURAT
  public .


  methods SET_IMPORTING_PARAMETER
    importing
      !NAME type ABAP_PARMNAME
      !VALUE type ANY
    returning
      value(SELF) type ref to /VPCOE/IF_FTD_INPUT_CONFIGURAT .
  methods SET_CHANGING_PARAMETER
    importing
      !NAME type ABAP_PARMNAME
      !VALUE type ANY
    returning
      value(SELF) type ref to /VPCOE/IF_FTD_INPUT_CONFIGURAT .
  methods SET_TABLE_PARAMETER
    importing
      !NAME type ABAP_PARMNAME
      !VALUE type ANY
    returning
      value(SELF) type ref to /VPCOE/IF_FTD_INPUT_CONFIGURAT .
endinterface.
