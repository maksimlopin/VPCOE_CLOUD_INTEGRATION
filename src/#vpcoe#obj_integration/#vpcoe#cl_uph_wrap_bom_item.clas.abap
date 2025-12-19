class /VPCOE/CL_UPH_WRAP_BOM_ITEM definition
  public
  final
  create public .

public section.                                      "#EC INTF_IN_CLASS

  methods CONSTRUCTOR
    importing
      !IS_BOM_ITEM_DATA type STPOX
      !IR_ITEMS type ref to /VPCOE/UPH_WRAP_BOM_ITEM optional .
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
      "! Get the material item plant
  methods GET_PLANT
    returning
      value(RV_PLANT) type WERKS_D .
      "! Get the item material
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
      "! Get the BOM number
  methods GET_BOM_NUMBER
    returning
      value(RV_NUMBER) type STNUM .
      "! Get the BOM alternative number
  methods GET_BOM_ALTERNATIVE_NUMBER
    returning
      value(RV_ALTERNATIVE_NUMBER) type STALT .
      "! Get the BOM usage
  methods GET_BOM_USAGE
    returning
      value(RV_USAGE) type STLAN .
      "! Get the BOM base amount
  methods GET_BOM_BASE_AMOUNT
    returning
      value(RV_AMOUNT) type BASMN .
      "! Get the BOM base unit of measure
  methods GET_BOM_BASE_UOM
    returning
      value(RV_UOM) type BASME .
      "! Get item valid from
  methods GET_ITEM_VALID_FROM
    returning
      value(RV_VALID_FROM) type DATUV .
      "! Get item valid to
  methods GET_ITEM_VALID_TO
    returning
      value(RV_VALID_TO) type DATUB .
      "! Get the item change number
  methods GET_ITEM_CHANGE_NUMBER
    returning
      value(RV_NUMBER) type AENNR .
      "! Get the item amount
  methods GET_ITEM_AMOUNT
    returning
      value(RV_AMOUNT) type BASMN .
      "! Get the item amount unit of measure
  methods GET_ITEM_UOM
    returning
      value(RV_UOM) type BASME .
      "! Get item number
  methods GET_ITEM_NUMBER
    returning
      value(RV_NUMBER) type SPOSN .
      "! Get item category
  methods GET_ITEM_CATEGORY
    returning
      value(RV_CATEGORY) type PSITP .
  methods GET_ITEM_SORT_STRING
    returning
      value(RV_SORT_STRING) type SORTP .
      "! Get all internal data
  methods GET_INTERNAL_DATA
    returning
      value(RV_ITEM_DATA) type STPOX .
  methods IS_ALTERNATIVE_ITEM
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods GET_ALTERNATIVE_ITEM_GROUP
    returning
      value(RV_ALT_ITEM_GROUP) type CS_ALPGR .
  methods GET_ALTERNATIVE_ITEM_ORDER
    returning
      value(RV_ALT_ITEM_ORDER) type CS_ALPRF .
  methods GET_ALTERNATIVE_ITEM_STRATEGY
    returning
      value(RV_ALT_ITEM_STRATEGY) type CS_ALPST .
  methods GET_ALT_ITEM_USAGE_PROBABILITY
    returning
      value(RV_ALT_ITEM_USAGE_PROB) type EWAHR .
  methods GET_ITEM_CALCULATED_AMOUNT
    returning
      value(RV_CALCULATED_AMOUNT) type CS_E_MNGKO .
  methods GET_BATCH_CLFN_DATA
    returning
      value(RV_BATCH_CLFN) type ref to /VPCOE/CL_UPH_WRAP_MCL .
  methods SET_BATCH_CLFN_DATA
    importing
      !IR_BATCH_CLFN type ref to /VPCOE/CL_UPH_WRAP_MCL .
  PROTECTED SECTION.
private section.

  data MS_BOM_ITEM_DATA type STPOX .
  data MR_ITEMS type ref to /VPCOE/UPH_WRAP_BOM_ITEM .
  data MR_BATCH_CLFN type ref to /VPCOE/CL_UPH_WRAP_MCL .
ENDCLASS.



CLASS /VPCOE/CL_UPH_WRAP_BOM_ITEM IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ms_bom_item_data = is_bom_item_data.

    IF ir_items IS BOUND.
      mr_items = ir_items.
    ENDIF.

  ENDMETHOD.


  METHOD get_alternative_item_group.
    rv_alt_item_group = ms_bom_item_data-alpgr.
  ENDMETHOD.


  METHOD get_alternative_item_order.
    rv_alt_item_order = ms_bom_item_data-alprf.
  ENDMETHOD.


  METHOD get_alternative_item_strategy.
    rv_alt_item_strategy = ms_bom_item_data-alpst.
  ENDMETHOD.


  METHOD get_alt_item_usage_probability.
    rv_alt_item_usage_prob = ms_bom_item_data-ewahr.
  ENDMETHOD.


  METHOD get_batch_clfn_data.
    rv_batch_clfn = mr_batch_clfn.
  ENDMETHOD.


  METHOD GET_BOM_ALTERNATIVE_NUMBER.
    rv_alternative_number = ms_bom_item_data-xtlal.
  ENDMETHOD.


  METHOD GET_BOM_BASE_AMOUNT.
    rv_amount = ms_bom_item_data-xmeng.
  ENDMETHOD.


  METHOD GET_BOM_BASE_UOM.
    rv_uom = ms_bom_item_data-xmein.
  ENDMETHOD.


  METHOD GET_BOM_NUMBER.
    rv_number = ms_bom_item_data-xtlnr.
  ENDMETHOD.


  METHOD GET_BOM_USAGE.
    rv_usage = ms_bom_item_data-xtlan.
  ENDMETHOD.


  METHOD GET_INTERNAL_DATA.
* Provides the internal data structures for external consumption
    rv_item_data = ms_bom_item_data.
  ENDMETHOD.


  METHOD GET_ITEMS.

    IF mr_items IS BOUND.
      rv_items = mr_items->*.
    ENDIF.

  ENDMETHOD.


  METHOD GET_ITEM_AMOUNT.
    rv_amount = ms_bom_item_data-mngko.
  ENDMETHOD.


  METHOD get_item_calculated_amount.
    rv_calculated_amount = ms_bom_item_data-mngko.
  ENDMETHOD.


  METHOD GET_ITEM_CATEGORY.
    rv_category = ms_bom_item_data-postp.
  ENDMETHOD.


  METHOD GET_ITEM_CHANGE_NUMBER.
    rv_number = ms_bom_item_data-aennr.
  ENDMETHOD.


  METHOD GET_ITEM_COUNT.

    IF mr_items IS BOUND.
      rv_count = lines( mr_items->* ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_ITEM_NUMBER.
    rv_number = ms_bom_item_data-posnr.
  ENDMETHOD.


  METHOD get_item_sort_string.
    rv_sort_string = ms_bom_item_data-sortf.
  ENDMETHOD.


  METHOD GET_ITEM_UOM.
    rv_uom = ms_bom_item_data-meins.
  ENDMETHOD.


  METHOD GET_ITEM_VALID_FROM.
    rv_valid_from = ms_bom_item_data-datuv.
  ENDMETHOD.


  METHOD GET_ITEM_VALID_TO.
    rv_valid_to = ms_bom_item_data-datub.
  ENDMETHOD.


  METHOD GET_MATERIAL.
    rv_material = ms_bom_item_data-idnrk.
  ENDMETHOD.


  METHOD GET_MATERIAL_BASE_UOM.
    rv_uom = ms_bom_item_data-mmein.
  ENDMETHOD.


  METHOD GET_MATERIAL_DESCRIPTION.
    rv_description = ms_bom_item_data-ojtxp.
  ENDMETHOD.


  METHOD get_material_group.
     rv_material_group = ms_bom_item_data-itmmk.
  ENDMETHOD.


  METHOD GET_MATERIAL_TYPE.
    rv_material_type = ms_bom_item_data-mtart.
  ENDMETHOD.


  METHOD GET_PLANT.
    rv_plant = ms_bom_item_data-werks.
  ENDMETHOD.


  METHOD is_alternative_item.
    IF ms_bom_item_data-alpos = abap_true.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD set_batch_clfn_data.
    mr_batch_clfn = ir_batch_clfn.
  ENDMETHOD.


  METHOD SET_ITEMS.
    mr_items = ir_items.
  ENDMETHOD.
ENDCLASS.
