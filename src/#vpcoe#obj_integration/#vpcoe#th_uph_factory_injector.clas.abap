class /VPCOE/TH_UPH_FACTORY_INJECTOR definition
  public
  final
  create public
  for testing .

public section.

  class-methods INJECT_FACTORY_DOUBLE
    importing
      !IO_DOUBLE type ref to /VPCOE/IF_UPH_FACTORY .
ENDCLASS.



CLASS /VPCOE/TH_UPH_FACTORY_INJECTOR IMPLEMENTATION.


  METHOD INJECT_FACTORY_DOUBLE.

    IF io_double IS BOUND.
      /vpcoe/cl_uph_factory=>mo_instance = io_double.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
