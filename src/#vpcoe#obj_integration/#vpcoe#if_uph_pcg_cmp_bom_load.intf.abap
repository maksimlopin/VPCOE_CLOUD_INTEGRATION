interface /VPCOE/IF_UPH_PCG_CMP_BOM_LOAD
  public .


  interfaces /VPCOE/IF_UPH_REPORT .

  types:
    ty_material_range        TYPE RANGE OF matnr .
  types:
    ty_material_type_range   TYPE RANGE OF mtart .
  types:
    ty_plant_range           TYPE RANGE OF werks_d .
  types:
    ty_bom_usage_range       TYPE RANGE OF stlan .
  types:
    ty_alternative_bom_range TYPE RANGE OF stlal .
  types:
    ty_change_date_range     TYPE RANGE OF sy-datum .

  constants GC_DEFAULT_RFC_DESTINATION type RFCDEST value 'VPCOE_RDP_PLM' ##NO_TEXT.
  constants GC_TCODE type SYST_TCODE value '/VPCOE/UPH_PC_BOM' ##NO_TEXT.

  methods IS_AUTHORIZED
    returning
      value(RV_VALUE) type ABAP_BOOL .
  methods DERIVE_INPUT_FROM_PARAMETERS
    importing
      !IV_SOURCE_ID type /VPCOE/UPH_SOURCE_ID
      !IV_RFC_DESTINATION type /VPCOE/UPH_RFCDEST
      !IV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE
      !IV_PATH_PREFIX type STRING
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_MATERIAL_RANGE type TY_MATERIAL_RANGE
      !IV_MATERIAL_TYPE_RANGE type TY_MATERIAL_TYPE_RANGE
      !IV_PLANT_RANGE type TY_PLANT_RANGE
      !IV_BOM_USAGE_RANGE type TY_BOM_USAGE_RANGE
      !IV_ALTERNATIVE_BOM_RANGE type TY_ALTERNATIVE_BOM_RANGE
      !IV_BOM_STATUS type STLST
      !IV_BOM_VALIDITY_DATE type SY-DATUM
      !IV_BOM_CHANGE_DATE_RANGE type TY_CHANGE_DATE_RANGE
      !IV_MAX_EXPLOSION_LEVEL type CS_MAXST
      !IV_LOAD_ID type /VPCOE/LOAD_ID optional
    returning
      value(RV_VALUE) type /VPCOE/S_PCKG_BOM_INPUT .
endinterface.
