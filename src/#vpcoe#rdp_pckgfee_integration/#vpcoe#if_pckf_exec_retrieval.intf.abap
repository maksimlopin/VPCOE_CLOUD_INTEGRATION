INTERFACE /vpcoe/if_pckf_exec_retrieval
  PUBLIC .

  CONSTANTS:
    gc_default_rfc_destination TYPE rfcdest VALUE 'VPCOE_RDP_PCKF'.

  METHODS execute_retrieval
    IMPORTING
      !iv_upload_mode TYPE /vpcoe/upload_mode
      !is_parameters  TYPE any OPTIONAL
      !iv_test        TYPE /vpcoe/ehfnd_bool OPTIONAL .

  METHODS derive_rfc_target_destination
    RETURNING VALUE(rv_result) TYPE rfcdest.

ENDINTERFACE.
