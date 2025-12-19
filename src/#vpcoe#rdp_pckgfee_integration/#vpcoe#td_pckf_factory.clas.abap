CLASS /vpcoe/td_pckf_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING .

  PUBLIC SECTION.

    INTERFACES /vpcoe/if_pckf_factory .

    ALIASES get_config_badi
      FOR /vpcoe/if_pckf_factory~get_config_badi .
    ALIASES get_entity_processor
      FOR /vpcoe/if_pckf_factory~get_entity_processor .
    ALIASES get_http_util
      FOR /vpcoe/if_pckf_factory~get_http_util .
    ALIASES get_logger
      FOR /vpcoe/if_pckf_factory~get_logger .
    ALIASES get_entity_mapper
      FOR /vpcoe/if_pckf_factory~get_entity_mapper .
    ALIASES get_target_destination
      FOR /vpcoe/if_pckf_factory~get_target_destination .
    ALIASES get_entity_cache
      FOR /vpcoe/if_pckf_factory~get_entity_cache .
    ALIASES get_matclass_access
      FOR /vpcoe/if_pckf_factory~get_matclass_access.

    METHODS constructor .

    METHODS set_entity_processor
      IMPORTING
        !io_double TYPE REF TO /vpcoe/if_pckf_entity_proc .

    METHODS set_logger
      IMPORTING
        !io_double TYPE REF TO /vpcoe/if_pckf_logger .
    METHODS set_http_util
      IMPORTING
        !io_double TYPE REF TO /vpcoe/cl_plm_http.
    METHODS set_entity_mapper
      IMPORTING
        !io_double TYPE REF TO /vpcoe/if_pckf_transfer_mapper .
    METHODS set_target_destination
      IMPORTING
        !iv_target_destination TYPE rfcdest .
  PRIVATE SECTION.

    DATA mo_pckf_logger TYPE REF TO /vpcoe/if_pckf_logger .
    DATA mo_badi_pckf_custom TYPE REF TO /vpcoe/badi_pckf_custom .
    DATA mo_ent_proc TYPE REF TO /vpcoe/if_pckf_entity_proc .
    DATA mo_pckf_http_util TYPE REF TO /vpcoe/cl_plm_http.
    DATA mo_transfer_mapper TYPE REF TO /vpcoe/if_pckf_transfer_mapper .
    DATA mv_target_destination TYPE rfcdest.
    DATA mo_entity_cache TYPE REF TO /vpcoe/if_pckf_cache .
    DATA mo_protocol TYPE REF TO /vpcoe/if_pckf_protocol.
    DATA mo_matclass_dac TYPE REF TO /vpcoe/if_pckf_matclass_dac.
ENDCLASS.



CLASS /VPCOE/TD_PCKF_FACTORY IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_factory~get_entity_cache.
    ro_entity_cache = mo_entity_cache.
  ENDMETHOD.


  METHOD get_entity_mapper.
    ro_tsfr_mapper = mo_transfer_mapper.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_matclass_access.
    ro_result = mo_matclass_dac.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_protocol_access.
    ro_result = mo_protocol.
  ENDMETHOD.


  METHOD constructor.

    mo_pckf_logger ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_PCKF_LOGGER' ).
    mo_ent_proc ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_PCKF_ENTITY_PROC' ).
    mo_transfer_mapper ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_PCKF_TRANSFER_MAPPER' ).
    mo_entity_cache ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_PCKF_CACHE' ).
    mo_protocol ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_PCKF_PROTOCOL' ).
    mo_matclass_dac ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_PCKF_MATCLASS_DAC' ).

    "not mockable because of missing interfaces
    "mo_pckf_http_util ?= cl_abap_testdouble=>create( object_name = '/VPCOE/CL_HTTP_COMMUNICATION' ).
    "mo_badi_pckf_custom ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_PCKF_CUSTOM' ).

  ENDMETHOD.


  METHOD get_config_badi.
    ro_badi = mo_badi_pckf_custom.
  ENDMETHOD.


  METHOD get_entity_processor.
    ro_processor = mo_ent_proc.

    IF is_parameters IS NOT INITIAL.

      ro_processor->init_processor(
        EXPORTING
          iv_entity_type = mo_ent_proc->get_entity( )
          iv_upload_mode = mo_ent_proc->get_mode( )
          is_parameters  = is_parameters
      ).
    ENDIF.

  ENDMETHOD.


  METHOD get_http_util.
    ro_http_util = mo_pckf_http_util.
  ENDMETHOD.


  METHOD get_logger.
    ro_logger = mo_pckf_logger.
  ENDMETHOD.


  METHOD get_target_destination.
    ev_endpoint_dest = mv_target_destination.
  ENDMETHOD.


  METHOD set_entity_mapper.
    mo_transfer_mapper = io_double.
  ENDMETHOD.


  METHOD set_entity_processor.
    mo_ent_proc = io_double.
  ENDMETHOD.


  METHOD set_http_util.
    mo_pckf_http_util = io_double.
  ENDMETHOD.


  METHOD set_logger.
    mo_pckf_logger = io_double.
  ENDMETHOD.


  METHOD set_target_destination.
    mv_target_destination = iv_target_destination.
  ENDMETHOD.
ENDCLASS.
