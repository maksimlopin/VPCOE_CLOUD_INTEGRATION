INTERFACE /vpcoe/if_pckf_exec_posting
  PUBLIC .


  METHODS execute_posting
    IMPORTING
      !is_parameters TYPE any OPTIONAL
      !iv_test       TYPE /vpcoe/ehfnd_bool OPTIONAL .
ENDINTERFACE.
