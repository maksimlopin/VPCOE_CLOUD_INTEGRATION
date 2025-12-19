INTERFACE /vpcoe/if_pckf_cache
  PUBLIC .

  METHODS clear
    IMPORTING
      !iv_entity_type   TYPE /vpcoe/pckf_entity
      !iv_ref_prot_uuid TYPE /vpcoe/uph_prot_uuid OPTIONAL.

  METHODS delete_entity
    IMPORTING
      !iv_uuid TYPE /vpcoe/pckf_entity_uuid .

  METHODS get_entities
    IMPORTING
      !iv_entity_type      TYPE /vpcoe/pckf_entity
      !iv_report_config_id TYPE /vpcoe/pckf_report_config_id OPTIONAL
      !iv_ref_prot_uuid    TYPE /vpcoe/uph_prot_uuid OPTIONAL
      !iv_failed_only      TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(rv_result)     TYPE /vpcoe/t_pckf_entity_data .

  METHODS set_entities
    IMPORTING
      !it_entities      TYPE /vpcoe/t_pckf_entity_data
      !iv_ref_prot_uuid TYPE /vpcoe/uph_prot_uuid OPTIONAL.

  METHODS get_references
    IMPORTING
      !iv_entity_type  TYPE /vpcoe/pckf_entity
    RETURNING
      VALUE(rt_result) TYPE /vpcoe/t_pckf_cache_reference.

  METHODS set_failed
    IMPORTING
      !iv_uuid TYPE /vpcoe/pckf_entity_uuid .

ENDINTERFACE.
