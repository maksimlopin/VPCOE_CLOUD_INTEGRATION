class /VPCOE/CL_PCKF_FACTORY definition
  public
  final
  create private

  global friends /VPCOE/TH_PCKF_FACTORY_INJECT .

public section.                                      "#EC INTF_IN_CLASS

  interfaces /VPCOE/IF_PCKF_FACTORY .

  aliases GET_CONFIG_BADI
    for /VPCOE/IF_PCKF_FACTORY~GET_CONFIG_BADI .
  aliases GET_ENTITY_CACHE
    for /VPCOE/IF_PCKF_FACTORY~GET_ENTITY_CACHE .
  aliases GET_ENTITY_MAPPER
    for /VPCOE/IF_PCKF_FACTORY~GET_ENTITY_MAPPER .
  aliases GET_ENTITY_PROCESSOR
    for /VPCOE/IF_PCKF_FACTORY~GET_ENTITY_PROCESSOR .
  aliases GET_HTTP_UTIL
    for /VPCOE/IF_PCKF_FACTORY~GET_HTTP_UTIL .
  aliases GET_LOGGER
    for /VPCOE/IF_PCKF_FACTORY~GET_LOGGER .
  aliases GET_MATCLASS_ACCESS
    for /VPCOE/IF_PCKF_FACTORY~GET_MATCLASS_ACCESS .
  aliases GET_TARGET_DESTINATION
    for /VPCOE/IF_PCKF_FACTORY~GET_TARGET_DESTINATION .

  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to /VPCOE/IF_PCKF_FACTORY .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA mo_instance TYPE REF TO /vpcoe/if_pckf_factory .
    DATA mo_logger TYPE REF TO /vpcoe/if_pckf_logger .
    DATA mo_badi_pckf_custom TYPE REF TO /vpcoe/badi_pckf_custom .
    DATA mo_http_util TYPE REF TO /vpcoe/cl_plm_http.
    DATA mo_entity_cache TYPE REF TO /vpcoe/if_pckf_cache.
    DATA mo_protocol TYPE REF TO /vpcoe/if_pckf_protocol.
    DATA mo_matclass_dac TYPE REF TO /vpcoe/if_pckf_matclass_dac.
ENDCLASS.



CLASS /VPCOE/CL_PCKF_FACTORY IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_factory~determine_version.
    "Call custom BADI to retrieve entity processor instance
    IF mo_badi_pckf_custom IS BOUND.

      TRY.
          CALL BADI mo_badi_pckf_custom->determine_version
            EXPORTING
              iv_rfc_dest = iv_rfc_dest
            CHANGING
              cv_skip    = cv_skip
              cv_version = cv_version.

        CATCH cx_badi_initial_reference   ##NO_HANDLER. "#EC EMPTY_CATCH
          "even if its blank its handled in the selection screen output
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_config_badi.
    ro_badi = mo_badi_pckf_custom.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_entity_cache.

    IF mo_entity_cache IS NOT BOUND.

      CASE iv_upload_entity.

        WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee.
          mo_entity_cache = NEW /vpcoe/cl_pckf_cache( ).

        WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role OR
             /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_exemp OR
             /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_comp_code.
          mo_entity_cache = NEW /vpcoe/cl_cstm_role_cache( ).

        WHEN OTHERS.
          mo_entity_cache = NEW /vpcoe/cl_pckf_cache( ).
      ENDCASE.
    ENDIF.

    ro_entity_cache = mo_entity_cache.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_entity_mapper.

    CASE iv_upload_entity.

      WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee.

        IF /vpcoe/cl_pckf_proc_base=>mv_version = 2.
          ro_tsfr_mapper ?= NEW /vpcoe/cl_pckf_tm_pckg_fee_v2( ).
        ELSE.
          ro_tsfr_mapper ?= NEW /vpcoe/cl_pckf_tm_pckg_fee_v1( ).
        ENDIF.

      WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role.
        ro_tsfr_mapper ?= NEW /vpcoe/cl_pckf_tm_custom_role( ).

      WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_exemp.
        ro_tsfr_mapper ?= NEW /vpcoe/cl_pckf_tm_custom_exemp( ).

      WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_comp_code.
        ro_tsfr_mapper ?= NEW /vpcoe/cl_pckf_tm_comp_code_ex( ).

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_entity_processor.

    "Call custom BADI to retrieve entity processor instance
    TRY.
        CALL BADI mo_badi_pckf_custom->get_entity_processor
          EXPORTING
            iv_entity_type = iv_entity_type
            iv_upload_mode = iv_upload_mode
            is_parameters  = is_parameters
          RECEIVING
            ro_processor   = DATA(lo_instance_proc).

        "initialize processor
        IF lo_instance_proc IS BOUND.
          ro_processor = lo_instance_proc.
        ENDIF.

      CATCH cx_badi_initial_reference   ##NO_HANDLER.  "#EC EMPTY_CATCH
        "should not happen, since badi reference is checked as bound
    ENDTRY.

    IF lo_instance_proc IS NOT BOUND.

      CASE iv_entity_type.

        WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee.
          ro_processor = NEW /vpcoe/cl_pckf_proc_pckg_fee( ).

        WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role.
          ro_processor = NEW /vpcoe/cl_pckf_rdp_cust_role( ).

        WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_exemp.
          ro_processor = NEW /vpcoe/cl_pckf_rdp_cust_exemp( ).

        WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_comp_code.
          ro_processor = NEW /vpcoe/cl_pckf_rdp_comp_code( ).

        WHEN OTHERS.
      ENDCASE.

    ENDIF.

    ro_processor->init_processor( iv_entity_type = iv_entity_type
                                  iv_upload_mode = iv_upload_mode
                                  is_parameters  = is_parameters ).

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_http_util.
    IF mo_http_util IS NOT BOUND.
      mo_http_util = NEW /vpcoe/cl_plm_http( iv_api_type = iv_api_type
                                             iv_rfc_name = iv_rfc_name
                                             iv_url      = iv_url ).
    ENDIF.
    ro_http_util = mo_http_util.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_logger.
    IF mo_logger IS NOT BOUND.

      mo_logger = NEW /vpcoe/cl_pckf_logger(
        iv_repid = iv_repid
        iv_test_mode = iv_test_mode
        iv_upload_mode = iv_upload_mode
        iv_consl_print = abap_true
      ).

    ENDIF.
    ro_logger = mo_logger.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_matclass_access.

    IF mo_matclass_dac IS NOT BOUND.

      mo_matclass_dac = NEW /vpcoe/cl_pckf_matclass_dac( ).

    ENDIF.

    ro_result = mo_matclass_dac.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_factory~get_protocol_access.

    IF mo_protocol IS NOT BOUND.
      CASE iv_upload_entity.

        WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee.
          mo_protocol = NEW /vpcoe/cl_pckf_protocol( ).

        WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role OR /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_exemp
          OR /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_comp_code.
          mo_protocol = NEW /vpcoe/cl_customer_protocol( ).

        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    ro_result = mo_protocol.

  ENDMETHOD.


  METHOD get_instance.

    IF mo_instance IS NOT BOUND.

      DATA(lo_instance) = NEW /vpcoe/cl_pckf_factory( ).

      TRY.
          GET BADI lo_instance->mo_badi_pckf_custom.
        CATCH cx_badi_not_implemented  ##NO_HANDLER.   "#EC EMPTY_CATCH
          "badi implementation error will be handled later
      ENDTRY.

      mo_instance ?= lo_instance.
    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD get_target_destination.
    "Call custom BADI to retrieve entity processor instance
    IF mo_badi_pckf_custom IS BOUND.

      TRY.
          CALL BADI mo_badi_pckf_custom->get_target_endpoint_dest
            IMPORTING
              ev_endpoint_dest = DATA(lo_rfcdest).

        CATCH cx_badi_initial_reference   ##NO_HANDLER. "#EC EMPTY_CATCH
          "even if its blank its handled in the selection screen output
      ENDTRY.
    ENDIF.

    ev_endpoint_dest = lo_rfcdest.

  ENDMETHOD.
ENDCLASS.
