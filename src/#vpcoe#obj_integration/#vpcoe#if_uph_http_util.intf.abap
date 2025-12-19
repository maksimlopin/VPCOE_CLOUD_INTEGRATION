interface /VPCOE/IF_UPH_HTTP_UTIL
  public .


  methods GET_HTTP_CLIENT
    importing
      !IV_RFC_DES type RFCDEST
      !IV_URI_SUFFIX type STRING
      !IV_REQUEST_METHOD type STRING default 'PUT'
    returning
      value(RO_HTTP_CLIENT) type ref to IF_HTTP_CLIENT .
  methods POST_DATA_TO_API
    importing
      !IV_ENTITY_DATA type STRING
    exporting
      !EV_ERROR_FLG type BOOLE_D
      !EV_RESPONSE_TXT type STRING
      !EV_REASON type STRING
      !EV_CODE type STRING .
  methods CLOSE_CONNECTION .
endinterface.
