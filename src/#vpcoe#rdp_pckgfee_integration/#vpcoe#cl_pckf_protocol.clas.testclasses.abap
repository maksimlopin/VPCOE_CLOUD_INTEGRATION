CLASS /vpcoe/ltcl_pckf_protocol DEFINITION DEFERRED.
CLASS /vpcoe/cl_pckf_protocol DEFINITION LOCAL FRIENDS /vpcoe/ltcl_pckf_protocol.

CLASS /vpcoe/ltcl_pckf_protocol DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
          mo_cut TYPE REF TO /vpcoe/if_pckf_protocol.  "class under test

    METHODS:
      setup,
      teardown,
      write_to_protocol FOR TESTING,
      read_from_protocol FOR TESTING,
      delete_from_protocol FOR TESTING.

ENDCLASS.

CLASS /vpcoe/ltcl_pckf_protocol IMPLEMENTATION.

  METHOD setup.

    DATA(lo_factory_double) = NEW /vpcoe/td_pckf_factory(  ).
    /vpcoe/th_pckf_factory_inject=>inject_factory_double( io_double = lo_factory_double ).

    mo_cut ?= NEW /vpcoe/cl_pckf_protocol( ).

  ENDMETHOD.

  METHOD teardown.

    "rollback any changes, since no OSQL double could be used
    ROLLBACK WORK.

  ENDMETHOD.

  METHOD write_to_protocol.

    DATA: ls_protocol TYPE /vpcoe/pckf_prot,
          ls_params   TYPE string.

    "Given
    ls_params = 'SEL_PARAM'.

    "When
    mo_cut->write_to_protocol( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
                                                          iv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
                                                          is_protocol = ls_protocol
                                                          is_selection_params = ls_params ).
    "Then
    DATA(lt_proto_data) = mo_cut->read_from_protocol( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
                                                         iv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
                                                         iv_only_success  = abap_true ).

    cl_abap_unit_assert=>assert_not_initial( lt_proto_data ).

    DATA(lv_lines) = lines( lt_proto_data ).
    cl_abap_unit_assert=>assert_equals( act = lt_proto_data[ lv_lines ]-selection exp = '"SEL_PARAM"' ).

  ENDMETHOD.

  METHOD delete_from_protocol.

    mo_cut->delete_from_protocol( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).

  ENDMETHOD.

  METHOD read_from_protocol .

    DATA:
      lv_entity_type   TYPE /vpcoe/upload_entity VALUE /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee,
      lv_upload_mode   TYPE /vpcoe/upload_mode VALUE 'F',
      lv_trnsfr_failed TYPE abap_bool.

    DATA(lt_proto_data) = mo_cut->read_from_protocol( iv_entity_type = lv_entity_type
                                                         iv_upload_mode = lv_upload_mode
                                                         iv_only_success  = abap_true ).

    lv_entity_type = ''.
    lt_proto_data = mo_cut->read_from_protocol( iv_entity_type = lv_entity_type
                                                         iv_upload_mode = lv_upload_mode
                                                         iv_only_success  = abap_true ).
    lv_entity_type = 'PE_PLM'.
    lv_upload_mode = 'D'.
    lt_proto_data = mo_cut->read_from_protocol( iv_entity_type = lv_entity_type
                                                              iv_upload_mode = lv_upload_mode
                                                              iv_only_success  = abap_true ).

  ENDMETHOD.

ENDCLASS.
