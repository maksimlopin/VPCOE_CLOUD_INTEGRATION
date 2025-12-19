INTERFACE /vpcoe/if_uph_transfer_mapper
  PUBLIC .

  METHODS prepare_payload
    IMPORTING
      !it_entity_data    TYPE /vpcoe/t_uph_entity_data
      !is_parameters     TYPE any
      !iv_replication_id TYPE string OPTIONAL
    RETURNING
      VALUE(rv_payload)  TYPE string .

  METHODS get_entity_url_suffix
    RETURNING
      VALUE(rv_entity_url_suffix) TYPE string .

  METHODS evaluate_response
    IMPORTING
      !iv_response    TYPE string
      !it_entity_data TYPE /vpcoe/t_uph_entity_data OPTIONAL
    EXPORTING
      !et_messages    TYPE /vpcoe/t_uph_msg.

  METHODS check_deprecated_attributes
    IMPORTING
      !it_entity_data    TYPE /vpcoe/t_uph_entity_data
    RETURNING
      VALUE(rt_messages) TYPE /vpcoe/t_uph_msg.

ENDINTERFACE.
