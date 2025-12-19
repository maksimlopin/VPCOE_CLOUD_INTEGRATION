class /VPCOE/CL_ENT_PCKG_FRACTION definition
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
private section.

  data MS_DATA type /VPCOE/S_UPH_ENT_PACK_FRAC .
  data MV_DELETED type ABAP_BOOL .
ENDCLASS.



CLASS /VPCOE/CL_ENT_PCKG_FRACTION IMPLEMENTATION.


  method /VPCOE/IF_UPH_ENTITY_DATA~IS_MARKED_DELETED.
    rv_mark_deleted = mv_deleted.
  endmethod.


  method /VPCOE/IF_UPH_ENTITY_DATA~TO_CONSOLE.


  endmethod.


  METHOD CONSTRUCTOR.
    ms_data = is_data.
    mv_deleted = iv_deleted.
  ENDMETHOD.


  METHOD GET_DATA.
    rs_fraction_data = ms_data.
  ENDMETHOD.
ENDCLASS.
