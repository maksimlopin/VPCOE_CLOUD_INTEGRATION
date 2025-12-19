INTERFACE /vpcoe/if_pckf_entity_data
  PUBLIC .


  CONSTANTS:
    BEGIN OF gc_entities,
      gc_ent_pckg_fee   TYPE /vpcoe/pckf_entity VALUE 'PCKG_FEE',
      gc_ent_cstm_role  TYPE /vpcoe/pckf_entity VALUE 'CSTM_ROLE',
      gc_ent_cstm_exemp TYPE /vpcoe/pckf_entity VALUE 'CSTM_EXEMP',
      gc_ent_comp_code  TYPE /vpcoe/pckf_entity VALUE 'COMP_CODE',
    END OF gc_entities .

  "! Converts json data to an entity instance
  CLASS-METHODS json_to_entity
    IMPORTING
      !iv_json        TYPE string
    EXPORTING
      !et_entity_data TYPE /vpcoe/t_pckf_entity_data
      !ev_next_link   TYPE string
      !ev_delta_link  TYPE string.

  "! Returns if the entity data is marked as deleted.
  METHODS is_marked_deleted
    RETURNING
      VALUE(rv_mark_deleted) TYPE abap_bool .
  METHODS to_console .
  "! Converts entity data to json
  METHODS entity_to_json
    RETURNING
      VALUE(rv_json) TYPE string .

  METHODS get_uuid
    RETURNING
      VALUE(rv_uuid) TYPE /vpcoe/pckf_entity_uuid .
  METHODS set_uuid
    IMPORTING
      !iv_uuid TYPE /vpcoe/pckf_entity_uuid .
  METHODS get_entity_type
    RETURNING
      VALUE(rv_entity_type) TYPE /vpcoe/pckf_entity .
  METHODS get_report_config_id
    RETURNING
      VALUE(rv_report_config_id) TYPE /vpcoe/pckf_report_config_id .

  METHODS is_failed
    RETURNING
      VALUE(rv_failed) TYPE abap_bool.

  METHODS set_failed
    IMPORTING
      !iv_failed TYPE abap_bool.

  METHODS get_messages
    RETURNING VALUE(rv_result) TYPE tab_bdcmsgcoll.

  METHODS set_messages
    IMPORTING
      !it_messages TYPE tab_bdcmsgcoll.

ENDINTERFACE.
