class /VPCOE/CL_RDP_ORG_DATA_OBJ_SM definition
  public
  inheriting from /VPCOE/CL_RDP_ORG_DATA_OBJ
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_RDP_HELPER=>SC_MODE-SEND
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
protected section.

  data MO_SUMA_HELPER type ref to /VPCOE/CL_RDP_SUMA_HELPER .
  data MV_RUN_ID type /VPCOE/DE_RUN_ID .
  data MO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER .

  methods CREATE_JSON
    redefinition .
private section.
ENDCLASS.



CLASS /VPCOE/CL_RDP_ORG_DATA_OBJ_SM IMPLEMENTATION.


METHOD constructor.

  super->constructor(
    EXPORTING
      iv_api_type = iv_api_type
      iv_mode     = iv_mode
      io_log      = io_log ).

  IF iv_mode = /vpcoe/cl_common_helper=>sc_mode-send.
    mo_rdp_helper = io_rdp_helper.
    me->mo_suma_helper = /vpcoe/cl_rdp_suma_helper=>get_instance( io_rdp_helper = me->mo_rdp_helper
                                                                  iv_srv_prfx   = 'org_data' ).

    me->mo_suma_helper->start_replication( EXPORTING io_log = io_log ).
    me->mv_run_id = me->mo_suma_helper->get_current_run_id( ).
  ENDIF.


ENDMETHOD.


  METHOD create_json.

    TYPES: BEGIN OF lty_s_source_data,
             replication_run_id TYPE  /vpcoe/de_run_id,
             elements           TYPE REF TO data.
    TYPES: END OF lty_s_source_data.

    DATA: ls_source_data TYPE lty_s_source_data,
          lv_json        TYPE string.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = is_json-elements.

    lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_source_data
                                                              iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

    INSERT VALUE #( elements = lv_json
                    count    = iv_lines ) INTO TABLE ct_json.

  ENDMETHOD.
ENDCLASS.
