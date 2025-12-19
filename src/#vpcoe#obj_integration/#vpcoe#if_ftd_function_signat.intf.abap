interface /VPCOE/IF_FTD_FUNCTION_SIGNAT
  public .


  methods GET_IMPORTING_PARAMETERS
    returning
      value(RESULT) type RSFB_IMP .
  methods GET_EXPORTING_PARAMETERS
    returning
      value(RESULT) type RSFB_EXP .
  methods GET_CHANGING_PARAMETERS
    returning
      value(RESULT) type RSFB_CHA .
  methods GET_TABLE_PARAMETERS
    returning
      value(RESULT) type RSFB_TBL .
  methods GET_CLASSIC_EXCEPTIONS
    returning
      value(RESULT) type RSFB_EXC .
  methods GET_CLASS_BASED_EXCEPTIONS
    returning
      value(RESULT) type RSFB_EXC .
endinterface.
