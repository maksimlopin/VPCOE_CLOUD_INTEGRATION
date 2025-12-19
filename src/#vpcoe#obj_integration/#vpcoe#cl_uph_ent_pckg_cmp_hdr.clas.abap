class /VPCOE/CL_UPH_ENT_PCKG_CMP_HDR definition
  public
  final
  create public .

public section.       "#EC INTF_IN_CLASS

  interfaces /VPCOE/IF_UPH_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_CMP_HDR_DATA type /VPCOE/S_UPH_ENT_PACK_CMP_HDR
      !IT_CMP_ITEM_DATA type /VPCOE/T_UPH_ENTITY_DATA
      !IT_PRODUCTS type /VPCOE/T_UPH_ENTITY_DATA
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_CMP_HDR_DATA) type /VPCOE/S_UPH_ENT_PACK_CMP_HDR .
  methods GET_ITEMS
    returning
      value(RT_CMP_ITEM_DATA) type /VPCOE/T_UPH_ENTITY_DATA .
  methods GET_PRODUCTS
    returning
      value(RT_PRODUCTS) type /VPCOE/T_UPH_ENTITY_DATA .
  methods SET_ITEMS
    importing
      !IT_CMP_ITEM_DATA type /VPCOE/UPH_ENTITY_DATA .
protected section.
private section.

  data MS_HDR_DATA type /VPCOE/S_UPH_ENT_PACK_CMP_HDR .
  data MT_ITEM_DATA type /VPCOE/T_UPH_ENTITY_DATA .
  data MT_PRODUCTS type /VPCOE/T_UPH_ENTITY_DATA .
  data MV_DELETED type ABAP_BOOL .
ENDCLASS.



CLASS /VPCOE/CL_UPH_ENT_PCKG_CMP_HDR IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_data~is_marked_deleted.
        rv_mark_deleted = mv_deleted.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_data~to_console.
  ENDMETHOD.


  METHOD CONSTRUCTOR.

    ms_hdr_data = is_cmp_hdr_data.
    mt_item_data = it_cmp_item_data.
    mt_products = it_products.
    mv_deleted = iv_deleted.

  ENDMETHOD.


  method GET_DATA.
    rs_cmp_hdr_data = ms_hdr_data.
  endmethod.


  method GET_ITEMS.
     rt_cmp_item_data = mt_item_data.
  endmethod.


  method GET_PRODUCTS.
    rt_products = mt_products.
  endmethod.


  method SET_ITEMS.
    mt_item_data = it_cmp_item_data.
  endmethod.
ENDCLASS.
