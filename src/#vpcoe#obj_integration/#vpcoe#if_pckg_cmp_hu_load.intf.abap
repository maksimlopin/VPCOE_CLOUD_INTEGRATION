interface /VPCOE/IF_PCKG_CMP_HU_LOAD
  public .


  interfaces /VPCOE/IF_UPH_REPORT .

  types:
    ty_document_type_range      TYPE RANGE OF lfart .
  types:
    ty_sales_org_range          TYPE RANGE OF vkorg .
  types:
    ty_country_range            TYPE RANGE OF land1 .
  types:
    ty_division_range           TYPE RANGE OF spart .
  types:
    ty_category_range           TYPE RANGE OF pstyv .
  types:
    ty_distribution_range       TYPE RANGE OF vtweg .
  types:
    ty_sddoc_range              TYPE RANGE OF vbeln .
  types:
    ty_act_goods_mvt_date_range TYPE RANGE OF wadat_ist .
  types:
    ty_plant_country_range      TYPE RANGE OF land1 .
  types:
    ty_material_range           TYPE RANGE OF matnr .
  types:
    ty_material_type_range      TYPE RANGE OF mtart .
  types:
    ty_material_group_range     TYPE RANGE OF matkl .
  types:
    ty_plant_range              TYPE RANGE OF werks_d .
  types:
    ty_ship_to_party_range      TYPE RANGE OF kunwe .
  types:
    BEGIN OF gty_s_hu_input,
           source_id          TYPE /vpcoe/source_id,
           rfc_des            TYPE /vpcoe/uph_rfcdest,
           document_type      TYPE ty_document_type_range,
           sales_org          TYPE ty_sales_org_range,
           country            TYPE ty_country_range,
           division           TYPE ty_division_range,
           category           TYPE ty_category_range,
           distribution       TYPE ty_distribution_range,
           sddoc              TYPE ty_sddoc_range,
           act_goods_mvt_date TYPE ty_act_goods_mvt_date_range,
           plant_country      TYPE ty_plant_country_range,
           material           TYPE /vpcoe/tt_r_matnr,
           material_type      TYPE /vpcoe/tt_r_mtart,
           material_group     TYPE /vpcoe/t_mat_group_range,
           plant              TYPE /vpcoe/t_uph_plant_range,
           ship_to_party      TYPE ty_ship_to_party_range,
         END OF gty_s_hu_input .

  constants GC_TCODE type SYST_TCODE value '/VPCOE/HU' ##NO_TEXT.

  methods IS_AUTHORIZED
    returning
      value(RV_VALUE) type ABAP_BOOL .
  methods DERIVE_INPUT_FROM_PARAMETERS
    importing
      !IV_SOURCE_ID type /VPCOE/SOURCE_ID
      !IV_RFC_DESTINATION type /VPCOE/UPH_RFCDEST
      !IV_DOCUMENT_TYPE_RANGE type TY_DOCUMENT_TYPE_RANGE
      !IV_SALES_ORG_RANGE type TY_SALES_ORG_RANGE
      !IV_COUNTRY_RANGE type TY_COUNTRY_RANGE
      !IV_DIVISION_RANGE type TY_DIVISION_RANGE
      !IV_CATEGORY_RANGE type TY_CATEGORY_RANGE
      !IV_DISTRIBUTION_RANGE type TY_DISTRIBUTION_RANGE
      !IV_SDDOC_RANGE type TY_SDDOC_RANGE
      !IV_ACT_GOODS_MVT_DATE_RANGE type TY_ACT_GOODS_MVT_DATE_RANGE
      !IV_PLANT_COUNTRY_RAGE type TY_PLANT_COUNTRY_RANGE
      !IV_MATERIAL_RANGE type TY_MATERIAL_RANGE
      !IV_MATERIAL_TYPE_RANGE type TY_MATERIAL_TYPE_RANGE
      !IV_MATERIAL_GROUP_RANGE type TY_MATERIAL_GROUP_RANGE
      !IV_PLANT_RANGE type TY_PLANT_RANGE
      !IV_SHIP_TO_PARTY_RANGE type TY_SHIP_TO_PARTY_RANGE
    returning
      value(RV_VALUE) type GTY_S_HU_INPUT .
endinterface.
