class /VPCOE/TD_UPH_FACTORY definition
  public
  final
  create public
  for testing .

public section.

  interfaces /vpcoe/IF_UPH_FACTORY .

  aliases GET_CONFIG_BADI
    for /vpcoe/IF_UPH_FACTORY~GET_CONFIG_BADI .
  aliases GET_ENTITY_PROCESSOR
    for /vpcoe/IF_UPH_FACTORY~GET_ENTITY_PROCESSOR .
  aliases GET_HTTP_UTIL
    for /vpcoe/IF_UPH_FACTORY~GET_HTTP_UTIL .
  aliases GET_LOGGER
    for /vpcoe/IF_UPH_FACTORY~GET_LOGGER .
  aliases GET_PCKG_ELEM_MAPPER
    for /vpcoe/IF_UPH_FACTORY~GET_PCKG_ELEM_MAPPER .
  aliases GET_TARGET_DESTINATION
    for /vpcoe/IF_UPH_FACTORY~GET_TARGET_DESTINATION .

  methods CONSTRUCTOR .
  methods SET_ENTITY_PROCESSOR
    importing
      !IO_DOUBLE type ref to /vpcoe/IF_UPH_ENTITY_PROC .
  methods SET_LOGGER
    importing
      !IO_DOUBLE type ref to /vpcoe/IF_UPH_LOGGER .
  methods SET_HTTP_UTIL
    importing
      !IO_DOUBLE type ref to /vpcoe/IF_UPH_HTTP_UTIL .
  methods SET_PCKG_ELEM_MAPPER
    importing
      !IO_DOUBLE type ref to /vpcoe/IF_UPH_TRANSFER_MAPPER .
  methods SET_TARGET_DESTINATION
    importing
      !IV_TARGET_DESTINATION type RFCDEST .
  PRIVATE SECTION.

    DATA mo_surdp_logger TYPE REF TO /vpcoe/if_uph_logger .
    DATA mo_badi_uph_custom TYPE REF TO /vpcoe/badi_uph_custom .
    DATA mo_upl_proc_pckg_plm TYPE REF TO /vpcoe/if_uph_entity_proc .
    DATA mo_uph_http_util TYPE REF TO /vpcoe/if_uph_http_util .
    DATA mo_upl_transfer_mapper TYPE REF TO /vpcoe/if_uph_transfer_mapper .
    DATA mv_target_destination TYPE rfcdest.
ENDCLASS.



CLASS /VPCOE/TD_UPH_FACTORY IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    mo_surdp_logger ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_UPH_LOGGER' ).
    mo_upl_proc_pckg_plm ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_UPH_ENTITY_PROC' ).
    mo_uph_http_util ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_UPH_HTTP_UTIL' ).
    mo_upl_transfer_mapper ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_UPH_TRANSFER_MAPPER' ).

  ENDMETHOD.


  METHOD GET_CONFIG_BADI.
    ro_badi = mo_badi_uph_custom.
  ENDMETHOD.


  METHOD GET_ENTITY_PROCESSOR.
    ro_processor = mo_upl_proc_pckg_plm.
  ENDMETHOD.


  METHOD GET_HTTP_UTIL.
    ro_uph_http_util = mo_uph_http_util.
  ENDMETHOD.


  METHOD GET_LOGGER.
    ro_logger = mo_surdp_logger.
  ENDMETHOD.


  METHOD GET_PCKG_ELEM_MAPPER.
    ro_tsfr_mapper = mo_upl_transfer_mapper.
  ENDMETHOD.


  METHOD GET_TARGET_DESTINATION.
    ev_endpoint_dest = mv_target_destination.
  ENDMETHOD.


  METHOD SET_ENTITY_PROCESSOR.
    mo_upl_proc_pckg_plm = io_double.
  ENDMETHOD.


  METHOD SET_HTTP_UTIL.
    mo_uph_http_util = io_double.
  ENDMETHOD.


  METHOD SET_LOGGER.
    mo_surdp_logger = io_double.
  ENDMETHOD.


  METHOD SET_PCKG_ELEM_MAPPER.
    mo_upl_transfer_mapper = io_double.
  ENDMETHOD.


  METHOD SET_TARGET_DESTINATION.
    mv_target_destination = iv_target_destination.
  ENDMETHOD.
ENDCLASS.
