CLASS ltc_surdp_uph_wrap_bom_item DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut            TYPE REF TO /vpcoe/cl_uph_wrap_bom_item,
      ms_bom_item_data  TYPE stpox,
      mt_wrap_item_data TYPE /vpcoe/t_uph_wrap_bom_item.

    METHODS:

      setup,
      teardown,
      test_getters FOR TESTING,
      test_setter  FOR TESTING.

ENDCLASS.

CLASS ltc_surdp_uph_wrap_bom_item IMPLEMENTATION.

  METHOD setup.

    "Given
    ms_bom_item_data = VALUE #( idnrk = 'RDP_BM_MOTOR_S'
                                potx1 = 'MOTORS SIZE S'
                                matkl = ''
                                mtart = 'CARTON'
                                werks = '0001'
                                xtlnr = '347897'
                                xtlal = '1'
                                xtlan = '1'
                                xmein = 'PC'
                                xmeng = 1
                                datuv = '20220101'
                                datub = '20221231'
                                aennr = '234'
                                menge = 6
                                meins = 'PC'
                                posnr = '0001'
                                postp = '1'
                                mmein = 'PC' ).

    mt_wrap_item_data = VALUE #(
                           ( NEW #( is_bom_item_data = VALUE #( idnrk = 'Comp1' ) ) )
                           ( NEW #( is_bom_item_data = VALUE #( idnrk = 'Comp2' ) ) )
                        ).

    "When
    mo_cut = NEW #( is_bom_item_data = ms_bom_item_data ir_items = REF #( mt_wrap_item_data ) ).




  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD test_getters.

    DATA ls_bom_item_data TYPE stpox.

    "Given

    "When
    ls_bom_item_data-idnrk = mo_cut->get_material(  ).
    ls_bom_item_data-potx1 = mo_cut->get_material_description(  ).
    ls_bom_item_data-matkl = mo_cut->get_material_group(  ).
    ls_bom_item_data-mtart = mo_cut->get_material_type(  ).
    ls_bom_item_data-werks = mo_cut->get_plant(  ).
    ls_bom_item_data-xtlnr = mo_cut->get_bom_number(  ).
    ls_bom_item_data-xtlal = mo_cut->get_bom_alternative_number(  ).
    ls_bom_item_data-xtlan = mo_cut->get_bom_usage(  ).
    ls_bom_item_data-xmein = mo_cut->get_bom_base_uom(  ).
    ls_bom_item_data-xmeng = mo_cut->get_bom_base_amount(  ).
    ls_bom_item_data-datuv = mo_cut->get_item_valid_from(  ).
    ls_bom_item_data-datub = mo_cut->get_item_valid_to(  ).
    ls_bom_item_data-aennr = mo_cut->get_item_change_number(  ).
    ls_bom_item_data-menge = mo_cut->get_item_amount(  ).
    ls_bom_item_data-meins = mo_cut->get_item_uom(  ).
    ls_bom_item_data-posnr = mo_cut->get_item_number(  ).
    ls_bom_item_data-postp = mo_cut->get_item_category(  ).
    ls_bom_item_data-mmein = mo_cut->get_material_base_uom(  ).

    "Then
    cl_abap_unit_assert=>assert_equals( act = ls_bom_item_data exp = ms_bom_item_data ).
    cl_abap_unit_assert=>assert_equals( act = mo_cut->get_internal_data(  ) exp = ms_bom_item_data ).

    DATA(lt_act_items_data) = mo_cut->get_items(  ).
    cl_abap_unit_assert=>assert_equals( act = lt_act_items_data exp = mt_wrap_item_data ).

    cl_abap_unit_assert=>assert_equals( act = mo_cut->get_item_count( ) exp = 2 ).

  ENDMETHOD.

  METHOD test_setter.

    "Given
    DATA(lt_wrap_items_data) = VALUE /vpcoe/t_uph_wrap_bom_item(
                              ( NEW #( is_bom_item_data = VALUE #( idnrk = 'Comp0' ) ) )
                           ).

    "When
    mo_cut->set_items( REF #( lt_wrap_items_data ) ).

    "Then
    DATA(lt_act_items_data) = mo_cut->get_items(  ).

    cl_abap_unit_assert=>assert_equals(
        act   = lines( lt_act_items_data )
        exp  =  1
    ).

  ENDMETHOD.


ENDCLASS.
