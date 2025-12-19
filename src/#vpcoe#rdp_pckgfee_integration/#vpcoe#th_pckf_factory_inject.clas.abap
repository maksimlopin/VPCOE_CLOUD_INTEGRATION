CLASS /vpcoe/th_pckf_factory_inject DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING .

  PUBLIC SECTION.

    CLASS-METHODS inject_factory_double
      IMPORTING
        !io_double TYPE REF TO /vpcoe/if_pckf_factory .
ENDCLASS.



CLASS /VPCOE/TH_PCKF_FACTORY_INJECT IMPLEMENTATION.


  METHOD inject_factory_double.

    IF io_double IS BOUND.
      /vpcoe/cl_pckf_factory=>mo_instance = io_double.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
