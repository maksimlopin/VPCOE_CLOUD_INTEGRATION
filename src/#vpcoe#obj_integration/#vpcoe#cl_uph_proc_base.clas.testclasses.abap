CLASS /vpcoe/ltcl_uph_proc_ip DEFINITION FOR TESTING INHERITING FROM /vpcoe/cl_uph_proc_base.

  PUBLIC SECTION.
    METHODS: constructor.

ENDCLASS.

CLASS /vpcoe/ltcl_uph_proc_ip IMPLEMENTATION.

  METHOD constructor.
    super->constructor(  ).
  ENDMETHOD.

ENDCLASS.

CLASS /vpcoe/ltcl_surdp_uph_proc DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: lt_entity_data    TYPE /vpcoe/t_uph_entity_data,
          mv_upload_entity  TYPE /vpcoe/upload_entity,
          mv_upload_mode    TYPE /vpcoe/upload_mode,
          mo_cut            TYPE REF TO /vpcoe/ltcl_uph_proc_ip.  "class under test
*          mo_sub_proxy_mock TYPE REF TO cl_ehfnd_ehssub_proxy.

    METHODS:
      setup,
      test_init_processor FOR TESTING,
      get_upload_entity   FOR TESTING,
      get_upload_mode FOR TESTING,
      prepare_process FOR TESTING,
      transfer_package FOR TESTING,
      write_to_protocol FOR TESTING,
      read_from_protocol FOR TESTING,
      process_package FOR TESTING,
      delete_from_protocol FOR TESTING,
      deserialize_selection_params FOR TESTING.

ENDCLASS.

CLASS /vpcoe/ltcl_surdp_uph_proc IMPLEMENTATION.

  METHOD setup.

    mo_cut = NEW /vpcoe/ltcl_uph_proc_ip( ).

  ENDMETHOD.

  METHOD test_init_processor.

    "Given
    CHECK mo_cut IS BOUND.

    "When
    mo_cut->/vpcoe/if_uph_entity_proc~init_processor(
        iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm
        iv_upload_mode   = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).

    "Then
    cl_abap_unit_assert=>assert_equals( act = mo_cut->/vpcoe/if_uph_entity_proc~get_upload_entity(  ) exp = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm ).
    cl_abap_unit_assert=>assert_equals( act = mo_cut->/vpcoe/if_uph_entity_proc~get_upload_mode(  ) exp = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).

  ENDMETHOD.

  METHOD get_upload_entity.
    "Given
    CHECK mo_cut IS BOUND.
    mo_cut->/vpcoe/if_uph_entity_proc~init_processor(
       iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm
       iv_upload_mode   = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta
   ).
    DATA(lv_upload_entity) = mo_cut->/vpcoe/if_uph_entity_proc~get_upload_entity( ).

    cl_abap_unit_assert=>assert_equals(
       act   = lv_upload_entity
       exp   = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm
     ).

  ENDMETHOD.


  METHOD get_upload_mode.
    DATA(lv_upload_mode) = mo_cut->/vpcoe/if_uph_entity_proc~get_upload_mode( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_upload_mode
      exp   =  ''
    ).
  ENDMETHOD.

  METHOD prepare_process.
    DATA(lv_record_cnt) = mo_cut->/vpcoe/if_uph_entity_proc~prepare_process( ).
  ENDMETHOD.

  METHOD transfer_package.
*   DATA:it_entity_data TYPE surdpt_uph_entity_data.
*   mo_cut->if_surdp_uph_entity_proc~transfer_package( it_entity_data ).
  ENDMETHOD.

  METHOD write_to_protocol.
    DATA:ls_protocol TYPE /vpcoe/uph_prot.
    mo_cut->/vpcoe/if_uph_entity_proc~write_to_protocol( ls_protocol ).
  ENDMETHOD.

  METHOD process_package.
    lt_entity_data = mo_cut->/vpcoe/if_uph_entity_proc~process_package(  ).
  ENDMETHOD.

  METHOD delete_from_protocol.
    mo_cut->/vpcoe/if_uph_entity_proc~delete_from_protocol( ).
  ENDMETHOD.

  METHOD deserialize_selection_params.
    DATA:lv_json_str TYPE string.
    mo_cut->/vpcoe/if_uph_entity_proc~deserialize_selection_params( lv_json_str ).
  ENDMETHOD.

  METHOD  read_from_protocol .

    DATA:iv_upload_entity TYPE /vpcoe/upload_entity VALUE 'PE_PLM',
         iv_upload_mode   TYPE /vpcoe/upload_mode VALUE 'F',
         iv_trnsfr_failed TYPE abap_bool.

    DATA(lt_proto_data) = mo_cut->/vpcoe/if_uph_entity_proc~read_from_protocol( iv_upload_entity = iv_upload_entity
                                                         iv_upload_mode = iv_upload_mode
                                                         iv_only_success  = 'X').

    iv_upload_entity = ''.
    lt_proto_data = mo_cut->/vpcoe/if_uph_entity_proc~read_from_protocol( iv_upload_entity = iv_upload_entity
                                                         iv_upload_mode = iv_upload_mode
                                                         iv_only_success  = 'X').
    iv_upload_entity = 'PE_PLM'.
    iv_upload_mode = 'D'.
    lt_proto_data = mo_cut->/vpcoe/if_uph_entity_proc~read_from_protocol( iv_upload_entity = iv_upload_entity
                                                              iv_upload_mode = iv_upload_mode
                                                              iv_only_success  = 'X').



  ENDMETHOD.




ENDCLASS.
