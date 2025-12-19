CLASS /vpcoe/tcl_uph_factory DEFINITION FOR TESTING
  FINAL
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA f_cut TYPE REF TO /vpcoe/if_uph_factory.  "class under test

    METHODS:
      setup,
      get_config_badi FOR TESTING,
      get_entity_processor FOR TESTING,
      get_instance FOR TESTING,
      get_http_util FOR TESTING,
      get_pckg_elem_mapper FOR TESTING,
      get_logger FOR TESTING,
      teardown.

ENDCLASS.       "tcl_Susrdp_Uph_Factory

CLASS /vpcoe/tcl_uph_factory IMPLEMENTATION.

  METHOD setup.

*   Instantiate private class method
    f_cut = /vpcoe/cl_uph_factory=>get_instance( ).

  ENDMETHOD.

  METHOD get_config_badi.

    DATA lro_badi TYPE REF TO /vpcoe/badi_uph_custom.

    lro_badi = f_cut->get_config_badi( ).

    IF lro_badi IS INITIAL.
      cl_abap_unit_assert=>assert_initial( lro_badi ).
    ELSE.
      cl_abap_unit_assert=>assert_not_initial( lro_badi ).
    ENDIF.
    FREE lro_badi.

  ENDMETHOD.

  METHOD get_entity_processor.

    DATA ls_input_values TYPE /vpcoe/s_pckg_elem_input.
    DATA lro_processor TYPE REF TO /vpcoe/if_uph_entity_proc.

    ls_input_values-spec_radio_btn = abap_true.
    APPEND INITIAL LINE TO ls_input_values-subid ASSIGNING FIELD-SYMBOL(<lfs_subid>).
    <lfs_subid>-sign = 'I'.
    <lfs_subid>-option = 'EQ'.
    <lfs_subid>-low = 'RS1_RDP_1'.

    lro_processor = f_cut->get_entity_processor( iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm
                                                iv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
                                                is_parameters = ls_input_values ).

    IF lro_processor IS INITIAL.
      cl_abap_unit_assert=>assert_initial( lro_processor ).
    ELSE.
      cl_abap_unit_assert=>assert_not_initial( lro_processor ).
    ENDIF.

    FREE lro_processor.

  ENDMETHOD.

  METHOD get_instance.

    DATA lro_instance TYPE REF TO /vpcoe/if_uph_factory.

    lro_instance = /vpcoe/cl_uph_factory=>get_instance(  ).
    cl_abap_unit_assert=>assert_not_initial( lro_instance ).
    FREE lro_instance.

  ENDMETHOD.

  METHOD get_logger.

    DATA iv_repid TYPE syrepid.
    DATA iv_test_mode TYPE boole_d.
    DATA ro_logger TYPE REF TO /vpcoe/if_uph_logger.

    ro_logger = f_cut->get_logger( iv_repid = iv_repid iv_test_mode = iv_test_mode  ).


    cl_abap_unit_assert=>assert_not_initial( ro_logger ).
    FREE ro_logger.

  ENDMETHOD.

  METHOD get_http_util.

    DATA ro_uph_http_util TYPE REF TO  /vpcoe/if_uph_http_util.

    ro_uph_http_util = f_cut->get_http_util(  ).

    IF ro_uph_http_util IS INITIAL.
      cl_abap_unit_assert=>assert_initial( ro_uph_http_util ).
    ELSE.
      cl_abap_unit_assert=>assert_not_initial( ro_uph_http_util ).
    ENDIF.

  ENDMETHOD.

  METHOD get_pckg_elem_mapper.

    DATA iv_upload_entity TYPE /vpcoe/upload_entity.
    DATA ro_tsfr_mapper TYPE REF TO /vpcoe/if_uph_transfer_mapper.

    ro_tsfr_mapper = f_cut->get_pckg_elem_mapper( iv_upload_entity ).

    ro_tsfr_mapper = f_cut->get_pckg_elem_mapper( iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm ).

    IF ro_tsfr_mapper IS INITIAL.
      cl_abap_unit_assert=>assert_initial( ro_tsfr_mapper ).
    ELSE.
      cl_abap_unit_assert=>assert_not_initial( ro_tsfr_mapper ).
    ENDIF.

    FREE ro_tsfr_mapper.

  ENDMETHOD.

  METHOD teardown.

    FREE: f_cut.

  ENDMETHOD.

ENDCLASS.
