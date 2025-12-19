INTERFACE /vpcoe/if_ehfnd_ehssub_proxy
  PUBLIC .

  CLASS-METHODS get_next_subid
    IMPORTING
      !iv_spec_type TYPE esesubcat OPTIONAL
    EXPORTING
      !ev_subid     TYPE esesubid
      !ev_error_ind TYPE boole_d
      !es_error_msg TYPE symsg .
ENDINTERFACE.
