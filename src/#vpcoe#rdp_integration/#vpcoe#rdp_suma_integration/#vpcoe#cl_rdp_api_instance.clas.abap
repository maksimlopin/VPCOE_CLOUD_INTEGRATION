class /VPCOE/CL_RDP_API_INSTANCE definition
  public
  create public .

public section.

  methods GET_BILLDOCITEMS_HANDLER
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
      !IV_MODE type /VPCOE/DE_MODE
    returning
      value(RO_HANDLER) type ref to /VPCOE/CL_RDP_BILLDOCIT_DATA .
  methods GET_BATCH_HANDLER
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
    returning
      value(RO_HANDLER) type ref to /VPCOE/CL_RDP_BATCH_DATA .
  methods GET_API_VERSION
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
    returning
      value(RV_API_VERSION) type /VPCOE/RDP_VERSION .
  methods IS_SUMA_API_ENABLED
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
    returning
      value(RV_SUMA_IN_USE) type ABAP_BOOL .
  methods GET_ORG_DATA_HANDLER
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER optional
      !IV_MODE type /VPCOE/DE_MODE
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
    returning
      value(RO_HANDLER) type ref to /VPCOE/CL_RDP_ORG_DATA_OBJ .
  methods GET_CONFIG_DATA_HANDLER
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER optional
      !IV_MODE type /VPCOE/DE_MODE
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
    returning
      value(RO_HANDLER) type ref to /VPCOE/CL_RDP_CONFIG_OBJ .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to /VPCOE/CL_RDP_API_INSTANCE .
  methods GET_CUSTOMER_HANDLER
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
      !IV_MODE type /VPCOE/DE_MODE
    returning
      value(RO_HANDLER) type ref to /VPCOE/CL_RDP_CUSTOMER_DATA .
  methods GET_SUPPLIER_HANDLER
    importing
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
      !IV_MODE type /VPCOE/DE_MODE
    returning
      value(RO_HANDLER) type ref to /VPCOE/CL_RDP_SUPPLIER_DATA .
  methods GET_PRODUCT_HANDLER
    importing
      !IV_MODE type /VPCOE/DE_MODE
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
    returning
      value(RO_HANDLER) type ref to /VPCOE/CL_RDP_PRODUCT_DATA .
  methods GET_DELIVERY_HANDLER
    importing
      !IV_MODE type /VPCOE/DE_MODE
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
    returning
      value(RO_HANDLER) type ref to /VPCOE/CL_RDP_DELIVERY_DATA .
  methods GET_SUPPLIER_INV_HANDLER
    importing
      !IV_MODE type /VPCOE/DE_MODE
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER
    returning
      value(RO_HANDLER) type ref to /VPCOE/CL_RDP_SUPINVI_DATA .
protected section.

  class-data SO_INSTANCE type ref to /VPCOE/CL_RDP_API_INSTANCE .
  data MV_SUMA_API_EXIST type ABAP_BOOL .
private section.
ENDCLASS.



CLASS /VPCOE/CL_RDP_API_INSTANCE IMPLEMENTATION.


METHOD get_api_version.

  "Determine API version based on Endpoint

  DATA(lv_endpoint) = io_rdp_helper->get_service_url( ).

  IF lv_endpoint IS NOT INITIAL AND NOT ( lv_endpoint CS `/api/exemptions/` OR lv_endpoint CS `/api/replication` ).

    rv_api_version = /vpcoe/cl_rdp_helper=>gc_api_version-suma.

  ELSE.

    rv_api_version = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

  ENDIF.

ENDMETHOD.


METHOD get_batch_handler.

  IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

    ro_handler = NEW /vpcoe/cl_rdp_batch_data( iv_api_type     = io_rdp_helper->get_api_type( )
                                               iv_package_size = io_rdp_helper->get_package_size( )
                                               iv_source       = io_rdp_helper->get_source_id( ) ).

  ELSE.

    ro_handler = NEW /vpcoe/cl_rdp_batch_data_sm( io_rdp_helper ).
    me->mv_suma_api_exist = abap_true.

  ENDIF.

ENDMETHOD.


METHOD get_billdocitems_handler.

  IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

    ro_handler = NEW /vpcoe/cl_rdp_billdocit_data( iv_mode ).

  ELSE.

    ro_handler = NEW /vpcoe/cl_rdp_billdocit_datasm( iv_mode ).
    me->mv_suma_api_exist = abap_true.

  ENDIF.

ENDMETHOD.


  METHOD get_config_data_handler.

    IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

      ro_handler = NEW /vpcoe/cl_rdp_config_obj( io_rdp_helper = io_rdp_helper
                                                 iv_api_type = io_rdp_helper->get_api_type( )
                                                 iv_mode     = iv_mode
                                                 io_log      = io_log ).

    ELSE.

      ro_handler = NEW /vpcoe/cl_rdp_config_obj_sm( iv_api_type = io_rdp_helper->get_api_type( )
                                                    io_rdp_helper = io_rdp_helper
                                                    iv_mode     = iv_mode
                                                    io_log      = io_log ).

      me->mv_suma_api_exist = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD get_customer_handler.

    IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

      ro_handler = NEW /vpcoe/cl_rdp_customer_data( iv_api_type     = io_rdp_helper->get_api_type( )
                                                    iv_package_size = io_rdp_helper->get_package_size( )
                                                    iv_source       = io_rdp_helper->get_source_id( )
                                                    iv_mode         = iv_mode ).

    ELSE.

      ro_handler = NEW /vpcoe/cl_rdp_customer_data_sm( io_rdp_helper = io_rdp_helper ).
      me->mv_suma_api_exist = abap_true.

    ENDIF.
  ENDMETHOD.


  METHOD get_delivery_handler.
    IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

      ro_handler = NEW /vpcoe/cl_rdp_delivery_data( iv_api_type     = io_rdp_helper->get_api_type( )
                                                   iv_package_size = io_rdp_helper->get_package_size( )
                                                   iv_source       = io_rdp_helper->get_source_id( )
                                                   iv_mode         = iv_mode
                                                   io_log          = io_log ).

    ELSE.
      ro_handler = NEW /vpcoe/cl_rdp_delivery_data_sm( io_rdp_helper = io_rdp_helper
                                                      iv_mode       = iv_mode
                                                      io_log        = io_log ).
      me->mv_suma_api_exist = abap_true.
    ENDIF.
  ENDMETHOD.


METHOD get_instance.

  IF /vpcoe/cl_rdp_api_instance=>so_instance IS NOT BOUND.
    /vpcoe/cl_rdp_api_instance=>so_instance = NEW #( ).
  ENDIF.

  ro_instance = so_instance.

ENDMETHOD.


  METHOD get_org_data_handler.

    IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

      ro_handler = NEW /vpcoe/cl_rdp_org_data_obj( iv_api_type = io_rdp_helper->get_api_type( )
                                                   iv_mode     = iv_mode
                                                   io_log      = io_log ).

    ELSE.

      ro_handler = NEW /vpcoe/cl_rdp_org_data_obj_sm( iv_api_type = io_rdp_helper->get_api_type( )
                                                      io_rdp_helper = io_rdp_helper
                                                      iv_mode     = iv_mode
                                                      io_log      = io_log ).

      me->mv_suma_api_exist = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD get_product_handler.
    IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

      ro_handler = NEW /vpcoe/cl_rdp_product_data( iv_api_type     = io_rdp_helper->get_api_type( )
                                                   iv_package_size = io_rdp_helper->get_package_size( )
                                                   iv_source       = io_rdp_helper->get_source_id( )
                                                   iv_mode         = iv_mode
                                                   io_log          = io_log ).

    ELSE.
      ro_handler = NEW /vpcoe/cl_rdp_product_data_sm( io_rdp_helper = io_rdp_helper
                                                      iv_mode       = iv_mode
                                                      io_log        = io_log ).
      me->mv_suma_api_exist = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_supplier_handler.
    IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

      ro_handler = NEW /vpcoe/cl_rdp_supplier_data( iv_api_type     = io_rdp_helper->get_api_type( )
                                                    iv_package_size = io_rdp_helper->get_package_size( )
                                                    iv_source       = io_rdp_helper->get_source_id( )
                                                    io_log          = io_log
                                                    iv_mode         = iv_mode ).

    ELSE.

      ro_handler = NEW /vpcoe/cl_rdp_supplier_data_sm( io_rdp_helper = io_rdp_helper
                                                       io_log        = io_log ).
      me->mv_suma_api_exist = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_supplier_inv_handler.
    IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-rdp.

      ro_handler = NEW /vpcoe/cl_rdp_supinvi_data( iv_mode ).

    ELSE.
      ro_handler = NEW /vpcoe/cl_rdp_supinvi_data_sm( iv_mode ).
      me->mv_suma_api_exist = abap_true.
    ENDIF.
  ENDMETHOD.


METHOD is_suma_api_enabled.

  IF me->get_api_version( io_rdp_helper ) = /vpcoe/cl_rdp_helper=>gc_api_version-suma OR me->mv_suma_api_exist = abap_true.
    rv_suma_in_use = abap_true.
  ELSE.
    rv_suma_in_use = abap_false.
  ENDIF.

ENDMETHOD.
ENDCLASS.
