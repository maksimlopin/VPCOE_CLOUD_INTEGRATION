class /VPCOE/CL_RDP_CUSTOMER_DATA_SM definition
  public
  inheriting from /VPCOE/CL_RDP_CUSTOMER_DATA
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



CLASS /VPCOE/CL_RDP_CUSTOMER_DATA_SM IMPLEMENTATION.


  METHOD build_json.
    DATA: ls_source_data   TYPE /vpcoe/str_customer_json_sm,
          ls_json          TYPE /vpcoe/cl_rdp_http=>gty_s_json,
          lt_customer_pack TYPE gty_t_customer,
          lt_customer	     TYPE gty_t_customer.

    CLEAR: et_json, ls_source_data .

    me->mo_suma_helper->start_replication( EXPORTING io_log = io_log ).

    ls_source_data-replication_run_id = me->mo_suma_helper->get_current_run_id( ).
    IF ls_source_data-replication_run_id IS INITIAL.
      RETURN.
    ENDIF.



    IF me->mv_package_size IS INITIAL.
      lt_customer_pack = it_customer.
    ELSE.
      lt_customer = it_customer.
      LOOP AT lt_customer ASSIGNING FIELD-SYMBOL(<ls_cusotmer>).
        <ls_cusotmer>-tax_numbers = VALUE #( FOR <ls_tax_number> IN it_tax_number WHERE ( cust_num = <ls_cusotmer>-id )
                                            ( tax_number      =  COND #( WHEN <ls_tax_number>-tax_number IS NOT INITIAL
                                                                              THEN <ls_tax_number>-tax_number
                                                                               ELSE <ls_tax_number>-tax_number_xl )
                                              tax_number_type = <ls_tax_number>-tax_number_type ) ).

        INSERT <ls_cusotmer> INTO TABLE lt_customer_pack.

        IF lines( lt_customer_pack ) = me->mv_package_size.
          ls_source_data-elements = lt_customer_pack.
          APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          <ls_json>-count = lines( lt_customer_pack ).
          <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                                        EXPORTING is_data        = ls_source_data
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

          REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
          CLEAR lt_customer_pack.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lt_customer_pack IS NOT INITIAL.
      ls_source_data-elements = lt_customer_pack.
      APPEND INITIAL LINE TO et_json ASSIGNING <ls_json>.
      <ls_json>-count = lines( lt_customer_pack ).
      <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                                    EXPORTING is_data        = ls_source_data
                                                              iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
      REPLACE ALL OCCURRENCES OF ':"f"' IN <ls_json>-elements WITH ': false' ##no_text.
      REPLACE ALL OCCURRENCES OF ':"t"' IN <ls_json>-elements WITH ': true' ##no_text.
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
       iv_api_type     = io_rdp_helper->get_api_type( ) ).

    me->mo_rdp_helper = io_rdp_helper.

    me->mo_suma_helper = /vpcoe/cl_rdp_suma_helper=>get_instance( io_rdp_helper = me->mo_rdp_helper
                                                                  iv_srv_prfx   = 'batch' ).
  ENDMETHOD.
ENDCLASS.
