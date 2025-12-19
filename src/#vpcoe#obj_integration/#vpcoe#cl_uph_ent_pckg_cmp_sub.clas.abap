class /VPCOE/CL_UPH_ENT_PCKG_CMP_SUB definition
  public
  final
  create public .

public section.                                      "#EC INTF_IN_CLASS

  interfaces /VPCOE/IF_UPH_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_ENT_PACK_CMP_SUBITM
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_ITEM_DATA) type /VPCOE/S_ENT_PACK_CMP_SUBITM .
  PROTECTED SECTION.
private section.

  data MS_DATA type /VPCOE/S_ENT_PACK_CMP_SUBITM .
  data MV_DELETED type ABAP_BOOL .
ENDCLASS.



CLASS /VPCOE/CL_UPH_ENT_PCKG_CMP_SUB IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_data~is_marked_deleted.
    rv_mark_deleted = mv_deleted.
  ENDMETHOD.


  method /VPCOE/IF_UPH_ENTITY_DATA~TO_CONSOLE.
  endmethod.


  METHOD CONSTRUCTOR.
    ms_data = is_data.
    mv_deleted = iv_deleted.
  ENDMETHOD.


  METHOD GET_DATA.
    rs_item_data = ms_data.
  ENDMETHOD.
ENDCLASS.
