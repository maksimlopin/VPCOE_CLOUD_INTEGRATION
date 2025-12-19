class /VPCOE/CL_UPH_FACTORY definition
  public
  final
  create private

  global friends /VPCOE/TH_UPH_FACTORY_INJECTOR .

public section.                                      "#EC INTF_IN_CLASS

  interfaces /VPCOE/IF_UPH_FACTORY .

  aliases GET_CONFIG_BADI
    for /VPCOE/IF_UPH_FACTORY~GET_CONFIG_BADI .
  aliases GET_ENTITY_PROCESSOR
    for /VPCOE/IF_UPH_FACTORY~GET_ENTITY_PROCESSOR .
  aliases GET_HTTP_UTIL
    for /VPCOE/IF_UPH_FACTORY~GET_HTTP_UTIL .
  aliases GET_LOGGER
    for /VPCOE/IF_UPH_FACTORY~GET_LOGGER .
  aliases GET_PCKG_ELEM_MAPPER
    for /VPCOE/IF_UPH_FACTORY~GET_PCKG_ELEM_MAPPER .
  aliases GET_TARGET_DESTINATION
    for /VPCOE/IF_UPH_FACTORY~GET_TARGET_DESTINATION .

  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to /VPCOE/IF_UPH_FACTORY .
  PROTECTED SECTION.

private section.

  class-data MO_INSTANCE type ref to /VPCOE/IF_UPH_FACTORY .
  data MO_SURDP_LOGGER type ref to /VPCOE/IF_UPH_LOGGER .
  data MO_BADI_UPH_CUSTOM type ref to /VPCOE/BADI_UPH_CUSTOM .
  data MO_UPL_PROC_PCKG_PLM type ref to /VPCOE/IF_UPH_ENTITY_PROC .
  data MO_UPH_HTTP_UTIL type ref to /VPCOE/IF_UPH_HTTP_UTIL .
  data MO_UPL_TRANSFER_MAPPER type ref to /VPCOE/IF_UPH_TRANSFER_MAPPER .
  data MO_UPH_CHARACT_DETAILS type ref to /VPCOE/IF_UPH_CHARACT_DETAILS .
  data MO_BADI_UPH_WRAPPER type ref to LIF_LOCAL_BADI_WRAPPER .
*  data MO_BADI_UPH_WRAPPER type ref to LIF_LOCAL_BADI_WRAPPER .
*  data MO_BADI_WRAPPER type ref to lif_local_badi_wrapper .
ENDCLASS.



CLASS /VPCOE/CL_UPH_FACTORY IMPLEMENTATION.


  METHOD /vpcoe/if_uph_factory~get_characteristic_details.
    IF mo_uph_charact_details IS NOT BOUND.
      mo_uph_charact_details = NEW /vpcoe/cl_charact_details( ).
    ENDIF.
    ro_result = mo_uph_charact_details.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_factory~get_config_badi.
    IF mo_badi_uph_wrapper IS BOUND.
      ro_badi = mo_badi_uph_wrapper->get_config_badi( ).
    ENDIF.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_factory~get_entity_processor.

    "Call custom BADI to retrieve entity processor instance
    IF mo_badi_uph_wrapper IS BOUND.

      DATA(lo_instance_proc) = mo_badi_uph_wrapper->get_entity_processor(
              iv_upload_entity = iv_upload_entity
              iv_upload_mode   = iv_upload_mode
              is_parameters    = is_parameters
     ).

      "initialize processor
      IF lo_instance_proc IS BOUND.
        lo_instance_proc->init_processor( iv_upload_entity = iv_upload_entity iv_upload_mode = iv_upload_mode is_parameters = is_parameters ).
      ENDIF.

      mo_upl_proc_pckg_plm = lo_instance_proc.

    ENDIF.

    ro_processor = mo_upl_proc_pckg_plm.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_factory~get_http_util.
    IF mo_uph_http_util IS NOT BOUND.
      mo_uph_http_util = NEW /vpcoe/cl_uph_http_util( ).
    ENDIF.
    ro_uph_http_util = mo_uph_http_util.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_factory~get_logger.
    IF mo_surdp_logger IS NOT BOUND.

      mo_surdp_logger = NEW /vpcoe/cl_uph_logger(
        iv_repid = iv_repid
        iv_test_mode = iv_test_mode
        iv_upload_mode = iv_upload_mode
        iv_consl_print = abap_true
      ) .

    ENDIF.
    ro_logger = mo_surdp_logger.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_factory~get_pckg_elem_mapper.
*    IF mo_upl_transfer_mapper IS NOT BOUND.
*      CASE iv_upload_entity.
*
*        WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm.
*          mo_upl_transfer_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_elem_v1( ).
*
*        WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_plm.
*          mo_upl_transfer_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_cmp_v1( ).
*
*        WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_rcp.
*          mo_upl_transfer_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_cmp_v1( ).
*
*        WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom.
*          mo_upl_transfer_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_cmp_v1( ).
*
*        WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl.
*          mo_upl_transfer_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_elem_v1( ).
*
*        WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu.
*          mo_upl_transfer_mapper ?= NEW /vpcoe/cl_pckg_cmp_it_v1( ).
*
*        WHEN OTHERS.
*      ENDCASE.
*    ENDIF.
*    ro_tsfr_mapper = mo_upl_transfer_mapper.
    IF mo_badi_uph_wrapper IS BOUND.

      ro_tsfr_mapper = mo_badi_uph_wrapper->get_entity_mapper( iv_upload_entity = iv_upload_entity ).
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    IF mo_instance IS NOT BOUND.

      DATA(lo_instance) = NEW /vpcoe/cl_uph_factory( ).

      lo_instance->mo_badi_uph_wrapper = NEW lcl_local_badi_wrapper(  ).

      mo_instance ?= lo_instance.
    ENDIF.

    ro_instance = mo_instance.
  ENDMETHOD.


  METHOD get_target_destination.
  ENDMETHOD.
ENDCLASS.
