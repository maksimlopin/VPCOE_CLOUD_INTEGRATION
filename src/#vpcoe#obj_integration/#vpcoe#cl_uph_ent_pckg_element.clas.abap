class /VPCOE/CL_UPH_ENT_PCKG_ELEMENT definition
  public
  final
  create public .

public section.                                      "#EC INTF_IN_CLASS

  interfaces /VPCOE/IF_UPH_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_UPH_ENT_PACK_ELEM
      !IT_FRACTIONS type /VPCOE/T_UPH_ENTITY_DATA
      !IT_PRODUCTS type /VPCOE/T_UPH_ENTITY_DATA
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_DATA) type /VPCOE/S_UPH_ENT_PACK_ELEM .
  methods GET_FRACTIONS
    returning
      value(RT_FRACTIONS) type /VPCOE/T_UPH_ENTITY_DATA .
  methods GET_PRODUCTS
    returning
      value(RT_PRODUCTS) type /VPCOE/T_UPH_ENTITY_DATA .
  PROTECTED SECTION.
private section.

  data MS_DATA type /VPCOE/S_UPH_ENT_PACK_ELEM .
  data MT_FRACTIONS type /VPCOE/T_UPH_ENTITY_DATA .
  data MT_PRODUCTS type /VPCOE/T_UPH_ENTITY_DATA .
  data MV_DELETED type ABAP_BOOL .
ENDCLASS.



CLASS /VPCOE/CL_UPH_ENT_PCKG_ELEMENT IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_data~is_marked_deleted.
     rv_mark_deleted = mv_deleted.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_data~to_console.
  ENDMETHOD.


  METHOD CONSTRUCTOR.

    ms_data = is_data.
    mt_fractions = it_fractions.
    mt_products = it_products.
    mv_deleted = iv_deleted.

  ENDMETHOD.


  METHOD GET_DATA.
    rs_data = ms_data.
  ENDMETHOD.


  METHOD GET_FRACTIONS.
    rt_fractions = mt_fractions.
  ENDMETHOD.


  METHOD GET_PRODUCTS.
    rt_products = mt_products.
  ENDMETHOD.
ENDCLASS.
