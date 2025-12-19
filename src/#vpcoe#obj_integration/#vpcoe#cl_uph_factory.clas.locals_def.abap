*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
INTERFACE lif_local_badi_wrapper.

  METHODS get_config_badi
    RETURNING
      VALUE(ro_badi) TYPE REF TO /vpcoe/badi_uph_custom.

  METHODS get_entity_processor
    IMPORTING
      !iv_upload_entity   TYPE /vpcoe/upload_entity
      !iv_upload_mode     TYPE /vpcoe/upload_mode
      !is_parameters      TYPE any
    RETURNING
      VALUE(ro_processor) TYPE REF TO /vpcoe/if_uph_entity_proc.

  METHODS get_entity_mapper
    IMPORTING
      !iv_upload_entity TYPE /vpcoe/upload_entity
    RETURNING
      VALUE(ro_mapper)  TYPE REF TO /vpcoe/if_uph_transfer_mapper.

  METHODS get_target_endpoint_dest
    EXPORTING
      !ev_endpoint_dest TYPE rfcdest .

ENDINTERFACE.

CLASS lcl_local_badi_wrapper DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES lif_local_badi_wrapper.

    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_badi_uph_custom TYPE REF TO /vpcoe/badi_uph_custom .

ENDCLASS.
