interface /VPCOE/IF_FTD_BEHAVIOR_VERIFIC
  public .


  methods IS_CALLED_TIMES
    importing
      !TIMES type I .
  methods IS_NEVER_CALLED .
  methods IS_CALLED_ONCE .
  methods IS_CALLED_AT_LEAST_ONCE .
  methods IS_CALLED_AT_LEAST
    importing
      !TIMES type I .
  methods IS_CALLED_AT_MOST_ONCE .
  methods IS_CALLED_AT_MOST
    importing
      !TIMES type I .
endinterface.
