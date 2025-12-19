class /VPCOE/CL_RDP_SUPPLIER_DATA_SM definition
  public
  inheriting from /VPCOE/CL_RDP_SUPPLIER_DATA
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .

  methods BUILD_JSON
    redefinition .
  methods CLOSE_REPLICATION
    redefinition .
protected section.
PRIVATE SECTION.

  DATA mo_rdp_helper TYPE REF TO /vpcoe/cl_rdp_helper .
  DATA mt_cpi TYPE /vpcoe/cl_rdp_delivery_data=>gty_t_cpi .
  DATA mv_api_type TYPE /vpcoe/de_api_type .
  DATA mv_chng_pointer_id TYPE edi_mestyp  .
  DATA mv_package_size TYPE /vpcoe/de_package_size .
  DATA mv_source TYPE string .
  DATA mo_suma_helper TYPE REF TO /vpcoe/cl_rdp_suma_helper .
ENDCLASS.



CLASS /VPCOE/CL_RDP_SUPPLIER_DATA_SM IMPLEMENTATION.


  METHOD build_json.
    DATA: lt_supplier_pack TYPE gty_t_supplier_jsn,
          lt_supplier	     TYPE gty_t_supplier_jsn,
          ls_source_data   TYPE /vpcoe/str_supplier_json_sm,
          ls_json          TYPE /vpcoe/cl_rdp_http=>gty_s_json.

    CLEAR : et_json ,ls_source_data .

    me->mo_suma_helper->start_replication( EXPORTING io_log = io_log ).

    ls_source_data-replication_run_id = me->mo_suma_helper->get_current_run_id( ).
    IF ls_source_data-replication_run_id IS INITIAL.
      RETURN.
    ENDIF.


    IF me->mv_package_size IS INITIAL.
      lt_supplier_pack = it_supplier.
    ELSE.
      lt_supplier = it_supplier.
      LOOP AT lt_supplier ASSIGNING FIELD-SYMBOL(<ls_supplier>).
        INSERT <ls_supplier> INTO TABLE lt_supplier_pack.

        IF lines( lt_supplier_pack ) = me->mv_package_size.
          ls_source_data-elements = lt_supplier_pack.
          APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          <ls_json>-count = lines( lt_supplier_pack ).
          <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                                        EXPORTING is_data        = ls_source_data
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
          REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
          CLEAR lt_supplier_pack.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lt_supplier_pack IS NOT INITIAL.
       ls_source_data-elements = lt_supplier_pack.
          APPEND INITIAL LINE TO et_json ASSIGNING <ls_json>.
          <ls_json>-count = lines( lt_supplier_pack ).
          <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                                        EXPORTING is_data        = ls_source_data
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
          REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
          CLEAR lt_supplier_pack.
    ENDIF.
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
       iv_api_type     = io_rdp_helper->get_api_type( )
       io_log          = io_log ).

    me->mo_rdp_helper = io_rdp_helper.

    me->mo_suma_helper = /vpcoe/cl_rdp_suma_helper=>get_instance( io_rdp_helper = me->mo_rdp_helper
                                                                  iv_srv_prfx   = 'batch' ).
  ENDMETHOD.
ENDCLASS.
