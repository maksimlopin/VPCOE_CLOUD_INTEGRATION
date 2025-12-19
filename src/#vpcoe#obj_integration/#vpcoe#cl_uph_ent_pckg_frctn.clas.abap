class /VPCOE/CL_UPH_ENT_PCKG_FRCTN definition
  public
  final
  create public .

public section.                                      "#EC INTF_IN_CLASS

  interfaces /VPCOE/IF_UPH_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_UPH_ENT_PACK_FRAC
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_FRACTION_DATA) type /VPCOE/S_UPH_ENT_PACK_FRAC .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_data TYPE /vpcoe/s_uph_ent_pack_frac .
    DATA mv_deleted TYPE abap_bool .
ENDCLASS.



CLASS /VPCOE/CL_UPH_ENT_PCKG_FRCTN IMPLEMENTATION.


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
    rs_fraction_data = ms_data.
  ENDMETHOD.
ENDCLASS.
