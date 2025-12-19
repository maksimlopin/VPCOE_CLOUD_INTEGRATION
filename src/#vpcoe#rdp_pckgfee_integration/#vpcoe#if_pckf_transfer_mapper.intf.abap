interface /VPCOE/IF_PCKF_TRANSFER_MAPPER
  public .


  methods PARSE_PAYLOAD
    importing
      !IV_PAYLOAD type STRING
    exporting
      !EV_NEXT_LINK type STRING
      !EV_DELTA_LINK type STRING
      value(ET_ENTITY_DATA) type /VPCOE/T_PCKF_ENTITY_DATA .
  methods PREPARE_PAYLOAD
    importing
      !IT_ENTITY_DATA type /VPCOE/T_PCKF_ENTITY_DATA
      !IS_PARAMETERS type ANY optional
    returning
      value(RV_PAYLOAD) type STRING .
  methods GET_ENTITY_URL_SUFFIX
    returning
      value(RV_ENTITY_URL_SUFFIX) type STRING .
  methods EVALUATE_RESPONSE
    importing
      !IV_RESPONSE type STRING
    exporting
      !ET_MESSAGES type /VPCOE/T_UPH_MSG .
endinterface.
