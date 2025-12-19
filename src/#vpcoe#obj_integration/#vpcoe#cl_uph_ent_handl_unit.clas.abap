class /VPCOE/CL_UPH_ENT_HANDL_UNIT definition
  public
  final
  create public .

public section.                                      "#EC INTF_IN_CLASS

  interfaces /VPCOE/IF_UPH_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_HANDLING_UNIT_PRD
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_DATA) type /VPCOE/S_HANDLING_UNIT_PRD .
  PROTECTED SECTION.
private section.

  data MS_DATA type /VPCOE/S_HANDLING_UNIT_PRD .
  data MV_DELETED type ABAP_BOOL .
ENDCLASS.



CLASS /VPCOE/CL_UPH_ENT_HANDL_UNIT IMPLEMENTATION.


  METHOD /VPCOE/IF_UPH_ENTITY_DATA~IS_MARKED_DELETED.
     rv_mark_deleted = mv_deleted.
  ENDMETHOD.


  METHOD /VPCOE/IF_UPH_ENTITY_DATA~TO_CONSOLE.
  ENDMETHOD.


  METHOD CONSTRUCTOR.

    ms_data = is_data.
    mv_deleted = iv_deleted.

  ENDMETHOD.


  METHOD GET_DATA.
    rs_data = ms_data.
  ENDMETHOD.
ENDCLASS.
