class /VPCOE/CL_UPH_ENT_PCKG_CMP_ITM definition
  public
  final
  create public .

public section.                       "#EC INTF_IN_CLASS

  interfaces /VPCOE/IF_UPH_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_UPH_ENT_PACK_CMP_ITEM
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_ITEM_DATA) type /VPCOE/S_UPH_ENT_PACK_CMP_ITEM .
  methods SET_DATA
    importing
      !IS_DATA type /VPCOE/S_UPH_ENT_PACK_CMP_ITEM
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_SUBITEMS
    returning
      value(RT_CMP_SUBITEM_DATA) type /VPCOE/T_UPH_ENTITY_DATA .
  methods SET_SUBITEMS
    importing
      !IT_CMP_SUBITEM_DATA type /VPCOE/T_UPH_ENTITY_DATA .
  PROTECTED SECTION.
private section.

  data MS_DATA type /VPCOE/S_UPH_ENT_PACK_CMP_ITEM .
  data MV_DELETED type ABAP_BOOL .
  data MT_SUBITEM_DATA type /VPCOE/T_UPH_ENTITY_DATA .
ENDCLASS.



CLASS /VPCOE/CL_UPH_ENT_PCKG_CMP_ITM IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_data~is_marked_deleted.
        rv_mark_deleted = mv_deleted.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_data~to_console.

  ENDMETHOD.


  METHOD CONSTRUCTOR.
    ms_data = is_data.
    mv_deleted = iv_deleted.
  ENDMETHOD.


  METHOD GET_DATA.
    rs_item_data = ms_data.
  ENDMETHOD.


  METHOD get_subitems.
    rt_cmp_subitem_data = mt_subitem_data.
  ENDMETHOD.


  METHOD set_data.
    ms_data = is_data.
    mv_deleted = iv_deleted.
  ENDMETHOD.


  METHOD set_subitems.
    mt_subitem_data = it_cmp_subitem_data.
  ENDMETHOD.
ENDCLASS.
