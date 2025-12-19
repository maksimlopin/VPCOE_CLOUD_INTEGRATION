
CLASS /vpcoe/tcl_uph_ent_pckg_elem DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_uph_ent_pckg_element.  "class under test

    METHODS: setup.
    METHODS: teardown.
    METHODS: is_marked_deleted FOR TESTING.
    METHODS: to_console FOR TESTING.
    METHODS: get_data FOR TESTING.
    METHODS: get_fractions FOR TESTING.
    METHODS: get_products FOR TESTING.

ENDCLASS.       "tcl_Surdp_Uph_Ent_Pckg_Elem


CLASS /vpcoe/tcl_uph_ent_pckg_elem IMPLEMENTATION.


  METHOD teardown.

    FREE f_cut.

  ENDMETHOD.


  METHOD setup.

    DATA: lv_deleted   TYPE abap_bool,
          ls_data      TYPE /vpcoe/s_uph_ent_pack_elem,
          ls_fractions TYPE /vpcoe/s_uph_ent_pack_frac,
          ls_products  TYPE /vpcoe/s_uph_ent_pack_prod,
          lt_fractions TYPE /vpcoe/t_uph_entity_data,
          lt_products  TYPE /vpcoe/t_uph_entity_data.

    NEW /vpcoe/cl_uph_ent_pckg_element( is_data = ls_data
                                       it_fractions = lt_fractions
                                       it_products  = lt_products
                                       iv_deleted   = lv_deleted ).

    lv_deleted = 'X'.

    "Packaging Element Attributes
    ls_data-displayid = 'TEST ClASS DATA ID'.
    ls_data-source = 'S4 HANA'.
    ls_data-validfrom = '09222021'.
    ls_data-description = 'Package Bottle'.
    ls_data-packagingtype = 'BOX'.

    "Packaging Elements Fraction
    ls_fractions-basicmaterialfractionid = 'BOX'.
    ls_fractions-weight = '10'.
    ls_fractions-weightunitofmeasure = 'KG'.
    DATA(lo_packelem_frac) = NEW /vpcoe/cl_uph_ent_pckg_frctn( is_data = ls_fractions iv_deleted = lv_deleted ).
    INSERT lo_packelem_frac INTO TABLE lt_fractions.

    "Packaging Element Product
    CLEAR: ls_products.
    ls_products-productid = 'PRODUCT ID'.
    DATA(lo_ent_packelem_prod) = NEW /vpcoe/cl_uph_ent_pckg_product( is_data = ls_products iv_deleted = lv_deleted ).
    INSERT lo_ent_packelem_prod INTO TABLE lt_products.

    NEW /vpcoe/cl_uph_ent_pckg_element( is_data = ls_data
                                       it_fractions = lt_fractions
                                       it_products  = lt_products
                                       iv_deleted   = lv_deleted ).

    f_cut =  NEW /vpcoe/cl_uph_ent_pckg_element( is_data = ls_data
                                           it_fractions = lt_fractions[]
                                           it_products  = lt_products[]
                                           iv_deleted   = lv_deleted ).

  ENDMETHOD.

  METHOD is_marked_deleted.

    DATA lv_mark_deleted TYPE abap_bool.

    lv_mark_deleted = f_cut->/vpcoe/if_uph_entity_data~is_marked_deleted(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_mark_deleted
      exp   = 'X' ).

  ENDMETHOD.


  METHOD to_console.

    f_cut->/vpcoe/if_uph_entity_data~to_console(  ).

  ENDMETHOD.


  METHOD get_data.

    DATA: ls_exp_data TYPE /vpcoe/s_uph_ent_pack_elem,
          ls_act_data TYPE /vpcoe/s_uph_ent_pack_elem.

    ls_act_data = f_cut->get_data(  ).

    "Packaging Element Attributes
    ls_exp_data-displayid = 'TEST ClASS DATA ID'.
    ls_exp_data-source = 'S4 HANA'.
    ls_exp_data-validfrom = '09222021'.
    ls_exp_data-description = 'Package Bottle'.
    ls_exp_data-packagingtype = 'BOX'.

    cl_abap_unit_assert=>assert_equals(
      act   = ls_act_data
      exp   = ls_exp_data  ).

  ENDMETHOD.


  METHOD get_fractions.

    DATA: lt_exp_fractions TYPE /vpcoe/t_uph_entity_data,
          ls_fractions     TYPE /vpcoe/s_uph_ent_pack_frac,
          lt_act_fractions TYPE /vpcoe/t_uph_entity_data,
          lv_deleted       TYPE abap_bool.

    lt_act_fractions = f_cut->get_fractions(  ).

    lv_deleted = 'X'.

    "Packaging Elements Fraction
    ls_fractions-basicmaterialfractionid = 'BOX'.
    ls_fractions-weight = '10'.
    ls_fractions-weightunitofmeasure = 'KG'.
    DATA(lo_packelem_frac) = NEW /vpcoe/cl_uph_ent_pckg_frctn( is_data = ls_fractions iv_deleted = lv_deleted ).
    INSERT lo_packelem_frac INTO TABLE lt_exp_fractions.

    cl_abap_unit_assert=>assert_equals(
      act   = lines( lt_act_fractions )
      exp   = lines( lt_exp_fractions ) ).

  ENDMETHOD.


  METHOD get_products.

    DATA: lt_exp_products TYPE /vpcoe/t_uph_entity_data,
          ls_products     TYPE /vpcoe/s_uph_ent_pack_prod,
          lt_act_products TYPE /vpcoe/t_uph_entity_data,
          lv_deleted      TYPE abap_bool.

    lt_act_products = f_cut->get_products(  ).

    lv_deleted = 'X'.

    "Packaging Element Product
    CLEAR: ls_products.
    ls_products-productid = 'PRODUCT ID'.
    DATA(lo_ent_packelem_prod) = NEW /vpcoe/cl_uph_ent_pckg_product( is_data = ls_products iv_deleted = lv_deleted ).
    INSERT lo_ent_packelem_prod INTO TABLE lt_exp_products.

    cl_abap_unit_assert=>assert_equals(
      act   = lines( lt_act_products )
      exp   = lines( lt_exp_products ) ).

  ENDMETHOD.


ENDCLASS.
