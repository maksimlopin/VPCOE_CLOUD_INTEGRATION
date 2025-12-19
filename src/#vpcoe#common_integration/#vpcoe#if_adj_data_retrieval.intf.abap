interface /VPCOE/IF_ADJ_DATA_RETRIEVAL
  public .


  interfaces IF_BADI_INTERFACE .

  methods ADJUST_DATA_RETRIEVAL default ignore
    importing
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IS_SEL_OPT type DATA optional
      !IV_LEVEL type /VPCOE/LEVEL optional
      !IV_CODE type /VPCOE/DE_MAT_DOC_VARIANTS optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IV_MODE type /VPCOE/DE_MODE optional
      !IV_SESSION_ID type RAW16 optional
    changing
      !CT_DATA type DATA .
  methods ADJUST_JSON default ignore
    importing
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IV_LEVEL type /VPCOE/LEVEL optional
    changing
      !CT_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods ADJUST_MAPPING default ignore
    importing
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IS_DATA type DATA
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    changing
      !CS_DATA type DATA .
  methods ADJUST_TEXT_MAPPING default ignore
    importing
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IT_DATA type DATA
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    changing
      !CT_DATA type ANY TABLE .
  methods GET_EXT_DATA default ignore
    importing
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IS_SEL_OPT type DATA
      !IT_DATA type DATA optional
      !IV_MODE type /VPCOE/DE_MODE optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    changing
      !CT_EXT_DATA type DATA .
  methods SKIP_SELECTION default ignore
    importing
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_LEVEL type /VPCOE/LEVEL optional
      !IS_SEL_OPT type DATA optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IV_MODE type /VPCOE/DE_MODE optional
      !IV_CODE type /VPCOE/DE_MAT_DOC_VARIANTS optional
    changing
      !CV_SKIP type ABAP_BOOL .
  methods ADJUST_BUILD_WHERE_FOR_VARIANT default ignore
    importing
      !IV_CODE type /VPCOE/DE_VARIANTS
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
      !IV_MODE type /VPCOE/DE_MODE
    changing
      !CV_WHERE type STRING .
  methods DEFINE_MOVEMENT_TYPE default ignore
    importing
      !IV_CODE type /VPCOE/DE_MAT_DOC_VARIANTS
    changing
      !CT_MOVEMENT_TYPES type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MOVEMENT_TYPES
      !CT_STOCK_CHANGE_CAT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MOVEMENT_TYPES .
  methods GET_DB_CONNECTION default ignore
    importing
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IS_SEL_OPT type DATA optional
      !IV_LEVEL type /VPCOE/LEVEL optional
    changing
      !RV_DB_CONNECTION type DBCON_NAME .
endinterface.
