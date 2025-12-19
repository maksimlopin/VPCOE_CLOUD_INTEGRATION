CLASS /vpcoe/cl_uph_report_base DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /vpcoe/if_uph_report .

    METHODS constructor
      IMPORTING
        !io_exec_load_wrapper TYPE REF TO /vpcoe/if_uph_exec_load_wrap OPTIONAL
        !io_surdp_uph_factory TYPE REF TO /vpcoe/if_uph_factory OPTIONAL .
  PROTECTED SECTION.

    DATA mr_exec_load_wrapper TYPE REF TO /vpcoe/if_uph_exec_load_wrap .
    DATA mr_uph_factory TYPE REF TO /vpcoe/if_uph_factory .
  PRIVATE SECTION.
ENDCLASS.



CLASS /VPCOE/CL_UPH_REPORT_BASE IMPLEMENTATION.


  METHOD /vpcoe/if_uph_report~derive_rfc_target_destination.
    mr_uph_factory->get_target_destination( IMPORTING ev_endpoint_dest = DATA(lv_target_destination) ).

    IF lv_target_destination IS INITIAL.
      rv_value = /vpcoe/if_uph_report~gc_default_rfc_destination.
    ELSE.
      rv_value = lv_target_destination.
    ENDIF.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_report~derive_upload_mode.
    IF ic_initial_load IS NOT INITIAL.
      rv_value = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.
    ELSEIF ic_delta_load IS NOT INITIAL.
      rv_value = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
    ELSE.
      rv_value = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.
    ENDIF.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_report~execute_upload.
    mr_exec_load_wrapper->execute_upload( iv_upload_entity = iv_upload_entity
                                          iv_upload_mode   = iv_upload_mode
                                          is_parameters    = is_parameters
                                          iv_test          = iv_test ).
  ENDMETHOD.


  METHOD constructor.
    IF io_exec_load_wrapper IS INITIAL.
      mr_exec_load_wrapper = NEW /vpcoe/cl_uph_exec_load_wrap( ).
    ELSE.
      mr_exec_load_wrapper = io_exec_load_wrapper.
    ENDIF.

    IF io_surdp_uph_factory IS INITIAL.
      mr_uph_factory = /vpcoe/cl_uph_factory=>get_instance( ).
    ELSE.
      mr_uph_factory = io_surdp_uph_factory.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
