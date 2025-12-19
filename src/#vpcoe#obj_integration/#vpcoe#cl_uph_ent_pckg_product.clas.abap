class /VPCOE/CL_UPH_ENT_PCKG_PRODUCT definition
  public
  final
  create public .

public section.                                      "#EC INTF_IN_CLASS

  interfaces /VPCOE/IF_UPH_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_UPH_ENT_PACK_PROD
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_PRODUCT_DATA) type /VPCOE/S_UPH_ENT_PACK_PROD .
  PROTECTED SECTION.
private section.

  data MS_DATA type /VPCOE/S_UPH_ENT_PACK_PROD .
  data MV_DELETED type ABAP_BOOL .
ENDCLASS.



CLASS /VPCOE/CL_UPH_ENT_PCKG_PRODUCT IMPLEMENTATION.


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
    rs_product_data = ms_data.
  ENDMETHOD.
ENDCLASS.
