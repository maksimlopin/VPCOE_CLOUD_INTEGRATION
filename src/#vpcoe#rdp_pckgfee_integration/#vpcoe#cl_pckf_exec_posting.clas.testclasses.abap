CLASS tcl_pckf_proc_pckg_fee DEFINITION FOR TESTING
  INHERITING FROM /vpcoe/cl_pckf_proc_pckg_fee.

  PUBLIC SECTION.

    DATA:
          mv_transfer_called TYPE i.

    METHODS:
      constructor,
      /vpcoe/if_pckf_entity_proc~transfer_package REDEFINITION,

      get_transfered_called
        RETURNING VALUE(rv_result) TYPE i.

ENDCLASS.

CLASS tcl_pckf_proc_pckg_fee IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD /vpcoe/if_pckf_entity_proc~transfer_package.
* Test transfer
    ADD 1 TO mv_transfer_called.

  ENDMETHOD.

  METHOD get_transfered_called.
    rv_result = mv_transfer_called.
  ENDMETHOD.

ENDCLASS.


CLASS tcl_pckf_exec_posting DEFINITION DEFERRED.
CLASS /vpcoe/cl_pckf_exec_posting DEFINITION LOCAL FRIENDS tcl_pckf_exec_posting.

CLASS tcl_pckf_exec_posting DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mo_cut TYPE REF TO /vpcoe/cl_pckf_exec_posting.  "class under test

    METHODS: setup.
    METHODS: teardown.

    METHODS:
      post_entity_changes_sc0 FOR TESTING,
      post_entity_changes_sc1 FOR TESTING.

ENDCLASS.

CLASS tcl_pckf_exec_posting IMPLEMENTATION.

  METHOD setup.

    DATA(lo_factory_double) = NEW /vpcoe/td_pckf_factory(  ).
    DATA(lo_ent_proc_double) = NEW tcl_pckf_proc_pckg_fee( ).
    lo_factory_double->set_entity_processor( io_double = lo_ent_proc_double ).

    /vpcoe/th_pckf_factory_inject=>inject_factory_double( io_double = lo_factory_double ).

    mo_cut = NEW /vpcoe/cl_pckf_exec_posting( ).

  ENDMETHOD.

  METHOD teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD post_entity_changes_sc0.
* Scenario 0: Small amount of packaging fees and parameters with package size given

    "Given
    DATA: lt_ent_pckg_fee      TYPE /vpcoe/t_pckf_entity_data,
          ls_ent_pckg_fee_data TYPE /vpcoe/s_pckf_ent_pckg_fee.

    DATA lo_ent_processor TYPE REF TO tcl_pckf_proc_pckg_fee.

    lo_ent_processor ?= /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_processor(
       EXPORTING
         iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
         iv_upload_mode = 'F'
    ).

    ls_ent_pckg_fee_data = VALUE #( product = 'P001' reportconfiguration = 'REPCFG_1' ).
    DATA(lr_entity) = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_pckg_fee_data ).
    APPEND lr_entity TO lt_ent_pckg_fee.

    ls_ent_pckg_fee_data = VALUE #( product = 'P002' reportconfiguration = 'REPCFG_1' ).
    lr_entity = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_pckg_fee_data ).
    APPEND lr_entity TO lt_ent_pckg_fee.

    DATA(lo_cache) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_cache( ).

    cl_abap_testdouble=>configure_call( double = lo_cache )->set_parameter( name = 'iv_entity_type' value = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee )->returning( value = VALUE /vpcoe/t_pckf_cache_reference( (
      entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
      ref_prot_uuid = '0815'
      rec_cnt = 2
    ) ) ).
    lo_cache->get_references( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).

    cl_abap_testdouble=>configure_call( double = lo_cache )->returning( value = lt_ent_pckg_fee ).
    lo_cache->get_entities( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee iv_ref_prot_uuid = '0815' ).

    "When
    mo_cut->post_entity_changes(
      EXPORTING
        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
        is_parameters = VALUE /vpcoe/s_pckf_posting_input( package_size = 1 )
        iv_test        = abap_false
    ).

    "Then
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act  =  lo_ent_processor->get_transfered_called( )
        exp  =  2
    ).

    cl_abap_testdouble=>verify_expectations( double = lo_cache ).

  ENDMETHOD.

  METHOD post_entity_changes_sc1.
* Scenario 1: Mass amount of packaging fees and no parameters given

    "Given
    DATA: lt_ent_pckg_fee      TYPE /vpcoe/t_pckf_entity_data,
          ls_ent_pckg_fee_data TYPE /vpcoe/s_pckf_ent_pckg_fee,
          lv_pckg_fee_cnt      TYPE i,
          lv_index             TYPE i.

    DATA lo_ent_processor TYPE REF TO tcl_pckf_proc_pckg_fee.

    lo_ent_processor ?= /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_processor(
       EXPORTING
         iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
         iv_upload_mode = 'F'
    ).

    lv_pckg_fee_cnt = 500.
    lv_index = 1.
    DO lv_pckg_fee_cnt TIMES.

      ls_ent_pckg_fee_data = VALUE #( product = |'P00'{ lv_index }| reportconfiguration = 'REPCFG_1' ).
      DATA(lr_entity) = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_pckg_fee_data ).
      APPEND lr_entity TO lt_ent_pckg_fee.
    ENDDO.

    DATA(lo_cache) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_cache( ).

    cl_abap_testdouble=>configure_call( double = lo_cache )->set_parameter( name = 'iv_entity_type' value = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee )->returning( value = VALUE /vpcoe/t_pckf_cache_reference( (
      entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
      ref_prot_uuid = '0815'
      rec_cnt = lv_pckg_fee_cnt
    ) ) ).
    lo_cache->get_references( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).

    cl_abap_testdouble=>configure_call( double = lo_cache )->returning( value = lt_ent_pckg_fee ).
    lo_cache->get_entities( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee iv_ref_prot_uuid = '0815' ).

    "When
    mo_cut->post_entity_changes(
      EXPORTING
        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
        "is_parameters = VALUE /vpcoe/s_pckf_posting_input( package_size = 1 )
        iv_test        = abap_false
    ).

    "Then
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act  =  lo_ent_processor->get_transfered_called( )
        exp  =  5
    ).

    cl_abap_testdouble=>verify_expectations( double = lo_cache ).

  ENDMETHOD.

ENDCLASS.
