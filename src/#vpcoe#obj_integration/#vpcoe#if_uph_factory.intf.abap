interface /VPCOE/IF_UPH_FACTORY
  public .


  methods GET_ENTITY_PROCESSOR default ignore
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IS_PARAMETERS type ANY optional
    returning
      value(RO_PROCESSOR) type ref to /VPCOE/IF_UPH_ENTITY_PROC .
  methods GET_CONFIG_BADI default ignore
    returning
      value(RO_BADI) type ref to /VPCOE/BADI_UPH_CUSTOM .
  methods GET_LOGGER default ignore
    importing
      !IV_REPID type SYREPID default SY-REPID
      !IV_TEST_MODE type BOOLE_D default ABAP_FALSE
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE optional
    returning
      value(RO_LOGGER) type ref to /VPCOE/IF_UPH_LOGGER .
  methods GET_HTTP_UTIL default ignore
    returning
      value(RO_UPH_HTTP_UTIL) type ref to /VPCOE/IF_UPH_HTTP_UTIL .
  methods GET_PCKG_ELEM_MAPPER default ignore
    importing
      value(IV_UPLOAD_ENTITY) type /VPCOE/UPLOAD_ENTITY
      !IV_UPLOAD_TARGET type /VPCOE/UPLOAD_TARGET default /VPCOE/IF_UPH_ENTITY_PROC=>GC_UPLOAD_TARGET-GC_UPLOAD_TARGET_RDP
      !IV_UPLOAD_TARGET_VERSION type I default 1
    returning
      value(RO_TSFR_MAPPER) type ref to /VPCOE/IF_UPH_TRANSFER_MAPPER .
  methods GET_TARGET_DESTINATION default ignore
    exporting
      !EV_ENDPOINT_DEST type RFCDEST .
  methods GET_CHARACTERISTIC_DETAILS
    returning
      value(RO_RESULT) type ref to /VPCOE/IF_UPH_CHARACT_DETAILS .
endinterface.
