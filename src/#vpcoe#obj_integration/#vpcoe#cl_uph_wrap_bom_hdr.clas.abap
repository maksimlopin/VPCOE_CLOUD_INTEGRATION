class /VPCOE/CL_UPH_WRAP_BOM_HDR definition
  public
  final
  create public .

public section.                                      "#EC INTF_IN_CLASS

  methods CONSTRUCTOR
    importing
      !IS_HEADER_MATERIAL type CSTMAT
      !IR_ITEMS type ref to /VPCOE/UPH_WRAP_BOM_ITEM optional
      !IS_MATERIAL_GENERAL type BAPIMATDOA optional
      !IS_MATERIAL_PLANT type BAPIMATDOC optional
      !IS_MATERIAL_VALUATION type BAPIMATDOBEW optional .
      "! Set a list of BOM items
  methods SET_ITEMS
    importing
      !IR_ITEMS type ref to /VPCOE/UPH_WRAP_BOM_ITEM .
      "! Get a list of BOM items
  methods GET_ITEMS
    returning
      value(RV_ITEMS) type /VPCOE/UPH_WRAP_BOM_ITEM .
      "! Get the count of items available
  methods GET_ITEM_COUNT
    returning
      value(RV_COUNT) type I .
      "! Get the material BOM material
  methods GET_MATERIAL
    returning
      value(RV_MATERIAL) type MATNR .
      "! Get the material description
  methods GET_MATERIAL_DESCRIPTION
    returning
      value(RV_DESCRIPTION) type MAKTX .
      "! Get item material group
  methods GET_MATERIAL_GROUP
    returning
      value(RV_MATERIAL_GROUP) type MATKL .
      "! Get item material type
  methods GET_MATERIAL_TYPE
    returning
      value(RV_MATERIAL_TYPE) type MTART .
      "! Get the material base unit of measure
  methods GET_MATERIAL_BASE_UOM
    returning
      value(RV_UOM) type MEINS .
      "! Get the material BOM plant
  methods GET_PLANT
    returning
      value(RV_PLANT) type WERKS_D .
      "! Get the BOM number
  methods GET_BOM_NUMBER
    returning
      value(RV_NUMBER) type STNUM .
      "! Get the BOM alternative number
  methods GET_BOM_ALTERNATIVE_NUMBER
    returning
      value(RV_ALTERNATIVE_NUMBER) type STALT .
      "! Get the BOM alternative text
  methods GET_BOM_ALTERNATIVE_TEXT
    returning
      value(RV_ALTERNATIVE_TEXT) type STKTX .
      "! Get the BOM usage
  methods GET_BOM_USAGE
    returning
      value(RV_USAGE) type STLAN .
      "! Get the BOM version
  methods GET_BOM_VERSION
    returning
      value(RV_VERSION) type /VPCOE/CS_VERSN .
      "! Get the BOM status
  methods GET_BOM_STATUS
    returning
      value(RV_STATUS) type STLST .
      "! Get BOM valid from
  methods GET_VALID_FROM
    returning
      value(RV_VALID_FROM) type DATUV .
      "! Get BOM valid to
  methods GET_VALID_TO
    returning
      value(RV_VALID_TO) type DATUB .
      "! Get the BOM change number
  methods GET_CHANGE_NUMBER
    returning
      value(RV_NUMBER) type AENNR .
      "! Get the base amount
  methods GET_BASE_AMOUNT
    returning
      value(RV_AMOUNT) type BASMN .
      "! Get the base amount unit of measure
  methods GET_BASE_UOM
    returning
      value(RV_UOM) type BASME .
      "! Get delete indicator set in the header table.
  methods IS_MARKED_DELETED
    returning
      value(RV_MARK_DELETED) type ABAP_BOOL .
      "! Provides the internal data structures for external consumption.
  methods GET_INTERNAL_DATA
    exporting
      !ES_HEADER_MATERIAL type CSTMAT
      !ES_MATERIAL_GENERAL type BAPIMATDOA
      !ES_MATERIAL_PLANT type BAPIMATDOC
      !ES_MATERIAL_VALUATION type BAPIMATDOBEW .
  PROTECTED SECTION.
private section.

  data MS_HEADER_MATERIAL type /VPCOE/CSTMAT .
  data MR_ITEMS type ref to /VPCOE/UPH_WRAP_BOM_ITEM .
  data MS_MATERIAL_GENERAL type BAPIMATDOA .
  data MS_MATERIAL_PLANT type BAPIMATDOC .
  data MS_MATERIAL_VALUATION type BAPIMATDOBEW .
ENDCLASS.



CLASS /VPCOE/CL_UPH_WRAP_BOM_HDR IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ms_header_material = is_header_material.
    ms_material_general = is_material_general.
    ms_material_plant = is_material_plant.
    ms_material_valuation = is_material_valuation.

    mr_items = ir_items.

  ENDMETHOD.


  METHOD GET_BASE_AMOUNT.
    rv_amount = ms_header_material-bmeng.
  ENDMETHOD.


  METHOD GET_BASE_UOM.
    rv_uom = ms_header_material-bmein.
  ENDMETHOD.


  METHOD GET_BOM_ALTERNATIVE_NUMBER.
    rv_alternative_number = ms_header_material-stlal.
  ENDMETHOD.


  METHOD GET_BOM_ALTERNATIVE_TEXT.
    rv_alternative_text = ms_header_material-stktx.
  ENDMETHOD.


  METHOD GET_BOM_NUMBER.
    rv_number = ms_header_material-stlnr.
  ENDMETHOD.


  METHOD GET_BOM_STATUS.
    rv_status = ms_header_material-stlst.
  ENDMETHOD.


  METHOD GET_BOM_USAGE.
    rv_usage = ms_header_material-stlan.
  ENDMETHOD.


  METHOD GET_BOM_VERSION.
    rv_version = ms_header_material-bom_versn.
  ENDMETHOD.


  METHOD GET_CHANGE_NUMBER.
    rv_number = ms_header_material-aennr.
  ENDMETHOD.


  METHOD GET_INTERNAL_DATA.
* Provides the internal data structures for external consumption

    CLEAR: es_header_material, es_material_general, es_material_plant, es_material_valuation.

    es_header_material = ms_header_material.
    es_material_general = ms_material_general.
    es_material_plant = ms_material_plant.
    es_material_valuation = ms_material_valuation.

  ENDMETHOD.


  METHOD GET_ITEMS.

    IF mr_items IS BOUND.
      rv_items = mr_items->*.
    ENDIF.

  ENDMETHOD.


  METHOD GET_ITEM_COUNT.

    IF mr_items IS BOUND.
      rv_count = lines( mr_items->* ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_MATERIAL.
    rv_material = ms_header_material-matnr.
  ENDMETHOD.


  METHOD GET_MATERIAL_BASE_UOM.
    rv_uom = ms_material_general-base_uom.
  ENDMETHOD.


  METHOD GET_MATERIAL_DESCRIPTION.
    rv_description = ms_header_material-maktx.
  ENDMETHOD.


  METHOD GET_MATERIAL_GROUP.

    rv_material_group = ms_material_general-matl_group.

  ENDMETHOD.


  METHOD GET_MATERIAL_TYPE.

    rv_material_type = ms_material_general-matl_type.

  ENDMETHOD.


  METHOD GET_PLANT.
    rv_plant = ms_header_material-werks.
  ENDMETHOD.


  METHOD GET_VALID_FROM.
    rv_valid_from = ms_header_material-datuv.
  ENDMETHOD.


  METHOD GET_VALID_TO.
    rv_valid_to = ms_header_material-datub.
  ENDMETHOD.


  METHOD IS_MARKED_DELETED.

    rv_mark_deleted = ms_header_material-loekz.

  ENDMETHOD.


  METHOD SET_ITEMS.
    mr_items = ir_items.
  ENDMETHOD.
ENDCLASS.
