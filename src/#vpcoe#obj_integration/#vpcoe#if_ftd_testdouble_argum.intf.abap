interface /VPCOE/IF_FTD_TESTDOUBLE_ARGUM
  public .


  methods GET_NAME
    returning
      value(RESULT) type ABAP_PARMNAME .
  methods GET_VALUE
    exporting
      !VALUE type DATA
    returning
      value(RESULT) type ref to DATA .
  methods GET_KIND
    returning
      value(RESULT) type ABAP_PARMKIND .
  methods IS_SUPPLIED
    returning
      value(RESULT) type ABAP_BOOL .
endinterface.
