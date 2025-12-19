interface /VPCOE/IF_CUSTOM_ROLE_CACHE
  public .


  methods CLEAR
    importing
      !IV_ENTITY_TYPE type /VPCOE/PCKF_ENTITY
      !IV_REF_PROT_UUID type /VPCOE/UPH_PROT_UUID optional .
  methods DELETE_ENTITY
    importing
      !IV_UUID type /VPCOE/PCKF_ENTITY_UUID .
  methods GET_ENTITIES
    importing
      !IV_ENTITY_TYPE type /VPCOE/PCKF_ENTITY
      !IV_REF_PROT_UUID type /VPCOE/UPH_PROT_UUID optional
      !IV_FAILED_ONLY type ABAP_BOOL optional
    returning
      value(RV_RESULT) type /VPCOE/T_CUSTOM_ROLE_DATA .
  methods SET_ENTITIES
    importing
      !IT_ENTITIES type /VPCOE/T_PCKF_ENTITY_DATA
      !IV_REF_PROT_UUID type /VPCOE/UPH_PROT_UUID optional .
  methods GET_REFERENCES
    importing
      !IV_ENTITY_TYPE type /VPCOE/PCKF_ENTITY
    returning
      value(RT_RESULT) type /VPCOE/T_PCKF_CACHE_REFERENCE .
  methods SET_FAILED
    importing
      !IV_UUID type /VPCOE/PCKF_ENTITY_UUID .
endinterface.
