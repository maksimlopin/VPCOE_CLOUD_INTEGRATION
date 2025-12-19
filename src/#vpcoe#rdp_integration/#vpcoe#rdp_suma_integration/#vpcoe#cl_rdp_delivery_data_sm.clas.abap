class /VPCOE/CL_RDP_DELIVERY_DATA_SM definition
  public
  inheriting from /VPCOE/CL_RDP_DELIVERY_DATA
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND .

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
  data MV_MODE type /VPCOE/DE_MODE value 1 ##NO_TEXT.
ENDCLASS.



CLASS /VPCOE/CL_RDP_DELIVERY_DATA_SM IMPLEMENTATION.


  METHOD build_json.
    DATA: lo_badi        TYPE REF TO /vpcoe/adjust_data_retrieval,
          ls_source_data TYPE /vpcoe/str_delivery_json_sm,
          lt_json        TYPE /vpcoe/cl_rdp_http=>gty_t_json.


    CLEAR: es_json,
           et_delivery.

    GET BADI lo_badi.

    me->mo_suma_helper->start_replication( EXPORTING io_log = io_log ).
    ls_source_data-replication_run_id = me->mo_suma_helper->get_current_run_id( ).
    IF ls_source_data-replication_run_id IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_delivery_hdr ASSIGNING FIELD-SYMBOL(<ls_delivery>).
      IF NOT line_exists( it_delivery_itm[ vbeln = <ls_delivery>-id ] ).
        CONTINUE.
      ENDIF.
      INSERT VALUE #( id                            = <ls_delivery>-id
                      type                          = <ls_delivery>-type
                      sales_organization            = <ls_delivery>-sales_organization
                      ship_to_party                 = <ls_delivery>-ship_to_party
                      ship_to_country               = <ls_delivery>-ship_to_country
                      ship_to_region                = <ls_delivery>-ship_to_region
                      actual_goods_movement_date    = <ls_delivery>-actual_goods_movement_date
                      incoterms                     = <ls_delivery>-incoterms
                      overall_goods_movement_status = <ls_delivery>-overall_goods_movement_status
                      sold_to_party                 = <ls_delivery>-sold_to_party
                      items                         = VALUE #( FOR <ls_items> IN it_delivery_itm
                                                                  WHERE ( vbeln = <ls_delivery>-id )
                                                                   ( CORRESPONDING #( <ls_items> ) ) ) )
               INTO TABLE et_delivery.
    ENDLOOP.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
        iv_level    = /vpcoe/cl_common_helper=>sc_level-build_json
        iv_api_type = me->mv_api_type
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        ct_data     = et_delivery.

    IF et_delivery IS INITIAL.
      RETURN.
    ENDIF.

    IF mv_mode = /vpcoe/cl_common_helper=>sc_mode-send.
      ls_source_data-elements = et_delivery.
      APPEND INITIAL LINE TO lt_json ASSIGNING FIELD-SYMBOL(<ls_json>).

      <ls_json>-elements = /vpcoe/cl_common_helper=>serialize_json( EXPORTING is_data        = ls_source_data
                                                                              iv_pretty_name = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case ).
      <ls_json>-count = lines( et_delivery ).

      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null'.
      REPLACE ALL OCCURRENCES OF ':"0000-00-00"' IN <ls_json>-elements WITH ': null'.

      CALL BADI lo_badi->adjust_json
        EXPORTING
          iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
          iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
          iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
          io_log      = io_log
        CHANGING
          ct_json     = lt_json.

      es_json = <ls_json>.
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
                                                                  iv_srv_prfx   = 'delivery' ).
  ENDMETHOD.
ENDCLASS.
