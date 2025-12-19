interface /VPCOE/IF_UPH_PCKG_MCL_LOAD
  public .


  interfaces /VPCOE/IF_UPH_REPORT .

  types:
    ty_material_range       TYPE RANGE OF matnr .
  types:
    ty_material_type_range  TYPE RANGE OF mtart .
  types:
    ty_material_group_range TYPE RANGE OF matkl .
  types:
    ty_change_date_range    TYPE RANGE OF sy-datum .
  types:
    ty_validity_range       TYPE RANGE OF sy-datum .

  constants GC_DEFAULT_RFC_DESTINATION type RFCDEST value 'VPCOE_RDP_PLM_PKG_ELMNT' ##NO_TEXT.
  constants GC_TCODE type SYST_TCODE value '/VPCOE/UPH_PE_MCL' ##NO_TEXT.

  methods DERIVE_INPUT_FROM_PARAMETERS
    importing
      !IV_SOURCE_ID type /VPCOE/UPH_SOURCE_ID
      !IV_RFC_DESTINATION type /VPCOE/UPH_RFCDEST
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_PATH_PREFIX type STRING
      !IV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE
      !IV_MATERIAL_RANGE type TY_MATERIAL_RANGE
      !IV_MATERIAL_TYPE_RANGE type TY_MATERIAL_TYPE_RANGE
      !IV_MATERIAL_GROUP_RANGE type TY_MATERIAL_GROUP_RANGE
      !IV_CHANGE_DATE_RANGE type TY_CHANGE_DATE_RANGE
      !IV_VALIDITY_DATE_RANGE type TY_VALIDITY_RANGE
    returning
      value(RV_VALUE) type /VPCOE/S_PCKG_MATCLAS_INPUT .
endinterface.
