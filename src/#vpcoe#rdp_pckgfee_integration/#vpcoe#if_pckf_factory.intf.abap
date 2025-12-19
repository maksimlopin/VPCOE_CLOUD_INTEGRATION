interface /VPCOE/IF_PCKF_FACTORY
  public .


  methods GET_ENTITY_PROCESSOR
    importing
      !IV_ENTITY_TYPE type /VPCOE/PCKF_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IS_PARAMETERS type ANY optional
    returning
      value(RO_PROCESSOR) type ref to /VPCOE/IF_PCKF_ENTITY_PROC .
  methods GET_LOGGER
    importing
      !IV_REPID type SYREPID default SY-REPID
      !IV_TEST_MODE type ABAP_BOOL default ABAP_FALSE
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE optional
    returning
      value(RO_LOGGER) type ref to /VPCOE/IF_PCKF_LOGGER .
  methods GET_HTTP_UTIL
    importing
      !IV_API_TYPE type /VPCOE/DE_API_TYPE optional
      !IV_RFC_NAME type RFCDOC_D optional
      !IV_URL type STRING optional
    returning
      value(RO_HTTP_UTIL) type ref to /VPCOE/CL_PLM_HTTP
    raising
      CX_OA2C .
  methods GET_ENTITY_MAPPER
    importing
      value(IV_UPLOAD_ENTITY) type /VPCOE/PCKF_ENTITY
    returning
      value(RO_TSFR_MAPPER) type ref to /VPCOE/IF_PCKF_TRANSFER_MAPPER .
  methods GET_TARGET_DESTINATION
    exporting
      !EV_ENDPOINT_DEST type RFCDEST .
  methods GET_ENTITY_CACHE
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/PCKF_ENTITY optional
    returning
      value(RO_ENTITY_CACHE) type ref to /VPCOE/IF_PCKF_CACHE .
  methods GET_CONFIG_BADI
    returning
      value(RO_BADI) type ref to /VPCOE/BADI_PCKF_CUSTOM .
  methods GET_PROTOCOL_ACCESS
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/PCKF_ENTITY optional
    returning
      value(RO_RESULT) type ref to /VPCOE/IF_PCKF_PROTOCOL .
  methods GET_MATCLASS_ACCESS
    returning
      value(RO_RESULT) type ref to /VPCOE/IF_PCKF_MATCLASS_DAC .
  methods DETERMINE_VERSION
    importing
      !IV_RFC_DEST type RFCDEST optional
    changing
      !CV_VERSION type I
      !CV_SKIP type ABAP_BOOL .
endinterface.
