class /VPCOE/CL_RDP_BATCH_DATA_SM definition
  public
  inheriting from /VPCOE/CL_RDP_BATCH_DATA
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER .

  methods BUILD_JSON
    redefinition .
  methods CLOSE_REPLICATION
    redefinition .
protected section.
private section.

  data MO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER .
  data MT_CPI type /VPCOE/CL_RDP_DELIVERY_DATA=>GTY_T_CPI .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_CHNG_POINTER_ID type EDI_MESTYP .
  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SOURCE type STRING .
  data MO_SUMA_HELPER type ref to /VPCOE/CL_RDP_SUMA_HELPER .
ENDCLASS.



CLASS /VPCOE/CL_RDP_BATCH_DATA_SM IMPLEMENTATION.


METHOD build_json.
  DATA: ls_source_data TYPE /vpcoe/str_batch_json_sm.

  me->mo_suma_helper->start_replication( EXPORTING io_log = io_log ).

  ls_source_data-replication_run_id = me->mo_suma_helper->get_current_run_id( ).
  IF ls_source_data-replication_run_id IS INITIAL.
    RETURN.
  ENDIF.

  ls_source_data-elements = it_batch_data.

  rs_json-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                              EXPORTING
                                                is_data        = ls_source_data
                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

  rs_json-count = lines( it_batch_data ).

  REPLACE ALL OCCURRENCES OF ':"0000-00-00"' IN rs_json-elements WITH ': null' ##no_text.
  REPLACE ALL OCCURRENCES OF ':""' IN rs_json-elements WITH ': null' ##no_text.

ENDMETHOD.


METHOD close_replication.

  " Close Replication
  IF io_log IS BOUND AND io_log->check( ).
    me->mo_suma_helper->cancel_replication( io_log = io_log ).
  ELSE.
    me->mo_suma_helper->finish_replication( io_log = io_log ).
  ENDIF.

ENDMETHOD.


  METHOD constructor.

    super->constructor(
      EXPORTING
        iv_package_size = io_rdp_helper->get_package_size( )
        iv_source       = io_rdp_helper->get_source_id( )
        iv_api_type     = io_rdp_helper->get_api_type( ) ).

    me->mo_rdp_helper = io_rdp_helper.

    me->mo_suma_helper = /vpcoe/cl_rdp_suma_helper=>get_instance( io_rdp_helper = me->mo_rdp_helper
                                                                  iv_srv_prfx   = 'batch' ).

  ENDMETHOD.
ENDCLASS.
