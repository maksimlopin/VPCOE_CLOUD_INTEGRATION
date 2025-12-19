*"* use this source file for your ABAP unit test classes

CLASS tcl_susrdp_uph_wrap_bom_hdr DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      mo_cut              TYPE REF TO /vpcoe/cl_uph_wrap_bom_hdr,
      ms_header_material  TYPE cstmat,
      mt_wrap_items       TYPE /vpcoe/uph_wrap_bom_item,
      ms_material_general TYPE bapimatdoa.


    METHODS: setup,
      teardown,
      test_getters                 FOR TESTING,
      test_setters                 FOR TESTING,
      get_internal_data            FOR TESTING.


ENDCLASS.


CLASS tcl_susrdp_uph_wrap_bom_hdr IMPLEMENTATION.

  METHOD setup.

    ms_header_material = VALUE #( stlnr = '347897'
                                  stlal = '1'
                                  stlan = '1'
                                  "bom_versn = '1'
                                  aennr = '234'
                                  matnr = '3567'
                                  maktx = 'RDP_BM_PUMP'
                                  werks = '0001'
                                  datuv = '20220101'
                                  datub = '20221231'
                                  bmeng = 8
                                  bmein = 'PC'
                                  loekz = ''
                                  stlst = '1'
                                  stktx = 'Alternative Text').

    ms_material_general = VALUE #( matl_type = 'HALB' base_uom = 'PC' matl_group = 'SEMIS' ).

    mt_wrap_items = VALUE #(
                              ( NEW #( is_bom_item_data = VALUE #( idnrk = 'Comp1' ) ) )
                              ( NEW #( is_bom_item_data = VALUE #( idnrk = 'Comp2' ) ) )
                           ).

    "When
    mo_cut = NEW #( is_header_material = ms_header_material
                    ir_items = REF #( mt_wrap_items ) is_material_general = ms_material_general ).

  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.


  METHOD test_getters.

    DATA: ls_wrap_header_material  TYPE /vpcoe/cstmat,
          ls_wrap_material_general TYPE bapimatdoa.

    "Given

    "Then
    ls_wrap_header_material-stlnr     = mo_cut->get_bom_number(  ).
    ls_wrap_header_material-stlal     = mo_cut->get_bom_alternative_number(  ).
    ls_wrap_header_material-stlan     = mo_cut->get_bom_usage(  ).
    ls_wrap_header_material-bom_versn = mo_cut->get_bom_version(  ).
    ls_wrap_header_material-aennr     = mo_cut->get_change_number(  ).
    ls_wrap_header_material-matnr     = mo_cut->get_material(  ).
    ls_wrap_header_material-maktx     = mo_cut->get_material_description(  ).
    ls_wrap_header_material-werks     = mo_cut->get_plant(  ).
    ls_wrap_header_material-datuv     = mo_cut->get_valid_from(  ).
    ls_wrap_header_material-datub     = mo_cut->get_valid_to(  ).
    ls_wrap_header_material-bmeng     = mo_cut->get_base_amount(  ).
    ls_wrap_header_material-bmein     = mo_cut->get_base_uom(  ).
    ls_wrap_header_material-loekz     = mo_cut->is_marked_deleted(  ).
    ls_wrap_header_material-stlst     = mo_cut->get_bom_status(  ).
    ls_wrap_header_material-stktx     = mo_cut->get_bom_alternative_text(  ).

    ls_wrap_material_general-base_uom = mo_cut->get_material_base_uom(  ).
    ls_wrap_material_general-matl_type = mo_cut->get_material_type(  ).
    ls_wrap_material_general-matl_group = mo_cut->get_material_group(  ).

    "When
    cl_abap_unit_assert=>assert_equals( act = ls_wrap_header_material exp = ms_header_material ).
    cl_abap_unit_assert=>assert_equals( act = ls_wrap_material_general exp = ms_material_general ).

    DATA(lt_act_items) = mo_cut->get_items(  ).
    cl_abap_unit_assert=>assert_equals( act = lt_act_items exp = mt_wrap_items ).

    cl_abap_unit_assert=>assert_equals( act = mo_cut->get_item_count( ) exp = 2 ).

  ENDMETHOD.

  METHOD get_internal_data.

    DATA: ls_header_material TYPE cstmat.

    mo_cut->get_internal_data( IMPORTING es_header_material = ls_header_material es_material_general = DATA(ls_material_general) ).

    cl_abap_unit_assert=>assert_equals(
    act   = ls_header_material
    exp  =  ms_header_material
    ).

    cl_abap_unit_assert=>assert_equals(
    act   = ls_material_general
    exp  =  ms_material_general
    ).

  ENDMETHOD.


  METHOD test_setters.

    "Given
    DATA(lt_wrap_items) = VALUE /vpcoe/uph_wrap_bom_item(
                              ( NEW #( is_bom_item_data = VALUE #( idnrk = 'Comp0' ) ) )
                           ).

    "When
    mo_cut->set_items( REF #( lt_wrap_items ) ).

    "Then
    DATA(lt_act_items) = mo_cut->get_items(  ).

    cl_abap_unit_assert=>assert_equals(
        act   = lines( lt_act_items )
        exp  =  1
    ).

  ENDMETHOD.

ENDCLASS.
