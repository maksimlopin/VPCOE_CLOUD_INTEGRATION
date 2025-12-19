class /VPCOE/CL_UPH_ENT_PCKG_FUNC definition
  public
  final
  create public .

public section.

  interfaces /VPCOE/IF_UPH_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_UPH_ENT_PACK_FUNC
      !IV_DELETED type ABAP_BOOL optional .
  methods GET_DATA
    returning
      value(RS_FUNCTION_DATA) type /VPCOE/S_UPH_ENT_PACK_FUNC .
protected section.
private section.

  data MS_DATA type /VPCOE/S_UPH_ENT_PACK_FUNC .
  data MV_DELETED type ABAP_BOOL .
ENDCLASS.



CLASS /VPCOE/CL_UPH_ENT_PCKG_FUNC IMPLEMENTATION.


  method /VPCOE/IF_UPH_ENTITY_DATA~IS_MARKED_DELETED.
    rv_mark_deleted = mv_deleted.
  endmethod.


  method /VPCOE/IF_UPH_ENTITY_DATA~TO_CONSOLE.
  endmethod.


  method CONSTRUCTOR.
    ms_data = is_data.
    mv_deleted = iv_deleted.
  endmethod.


  method GET_DATA.
    rs_function_data = ms_data.
  endmethod.
ENDCLASS.
