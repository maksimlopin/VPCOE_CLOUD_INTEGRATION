*"* use this source file for your ABAP unit test classes

CLASS /vpcoe/tcl_Uph_Ent_Pckg_CmpHdr DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.

  PRIVATE SECTION.
    DATA:
      f_Cut TYPE REF TO /vpcoe/cl_Uph_Ent_Pckg_Cmp_Hdr.  "class under test
    METHODS: setup.
    METHODS: teardown.
    METHODS: is_Marked_Deleted FOR TESTING.
    METHODS: to_Console FOR TESTING.
    METHODS: get_Data FOR TESTING.
    METHODS: get_Items FOR TESTING.
    METHODS: get_Products FOR TESTING.
ENDCLASS.       "tcl_Surdp_Uph_Ent_Pckg_Cmp_Hdr


CLASS /vpcoe/tcl_Uph_Ent_Pckg_CmpHdr IMPLEMENTATION.




  METHOD setup.
    DATA lv_deleted TYPE abap_bool.
    DATA is_cmp_hdr_data TYPE /vpcoe/s_uph_ent_pack_cmp_hdr.
    DATA ls_cmp_hdr_data TYPE /vpcoe/s_uph_ent_pack_cmp_hdr.
    DATA it_cmp_item_data TYPE /vpcoe/t_uph_entity_data.
    DATA ls_cmp_item_data TYPE /vpcoe/t_uph_entity_data.
    DATA lt_products TYPE /vpcoe/t_uph_entity_data.
    DATA ls_products TYPE /vpcoe/t_uph_entity_data.

    ls_cmp_hdr_data = VALUE #( source = 'system' displayid = 'Test class data ID' description = 'Packaging' basequantity = '10'
                                                  baseunitofmeasureid = 'ghi' ).


    f_Cut  = NEW #( is_cmp_hdr_data = ls_cmp_hdr_data
                   it_cmp_item_data = ls_cmp_item_data
                   it_products  = lt_products
                   iv_deleted = lv_deleted ).

    lv_deleted = ' '.

    ls_cmp_hdr_data-displayid = 'TEST ClASS DATA ID'.
    ls_cmp_hdr_data-source = 'system'.

    ls_cmp_hdr_data-description = 'Packaging'.
    ls_cmp_hdr_data-basequantity = '10'.
    ls_cmp_hdr_data-baseunitofmeasureid = 'ghi'.


    f_cut = NEW #( is_cmp_hdr_data = ls_cmp_hdr_data
                                       it_cmp_item_data = ls_cmp_item_data
                                       it_products  = lt_products
                                       iv_deleted = lv_deleted ).

    f_cut =  NEW #( is_cmp_hdr_data = ls_cmp_hdr_data
                                                    it_cmp_item_data = ls_cmp_item_data
                                                    it_products  = lt_products
                                                   iv_deleted = lv_deleted ).



  ENDMETHOD.

  METHOD teardown.



  ENDMETHOD.


  METHOD is_Marked_Deleted.

    DATA lv_Mark_Deleted TYPE abap_Bool.

    lv_Mark_Deleted = f_Cut->/vpcoe/if_Uph_Entity_Data~is_Marked_Deleted(  ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act   = lv_Mark_Deleted
      exp   = ''
    ).



  ENDMETHOD.


  METHOD to_Console.

    f_Cut->/vpcoe/if_Uph_Entity_Data~to_Console(  ).


  ENDMETHOD.


  METHOD get_Data.

    DATA rs_cmp_hdr_data TYPE /vpcoe/s_uph_ent_pack_cmp_hdr.
    rs_cmp_hdr_data = f_Cut->get_Data(  ).


    cl_Abap_Unit_Assert=>assert_equals(
      act   = rs_cmp_hdr_data-source
      exp   = 'system'
    ).



  ENDMETHOD.


  METHOD get_Items.


    DATA: lt_exp_cmp_item_data TYPE /vpcoe/t_uph_entity_data,
          ls_cmp_item_data     TYPE /vpcoe/s_uph_ent_pack_cmp_hdr,
          lt_act_cmp_item_data TYPE /vpcoe/t_uph_entity_data,
          lv_deleted           TYPE abap_bool.

    lt_act_cmp_item_data = f_cut->get_items(  ).

    lv_deleted = ''.
    ls_cmp_item_data-displayid = 'class id'.
    ls_cmp_item_data-source = 'phone'.
    ls_cmp_item_data-description = 'package'.
    ls_cmp_item_data-basequantity = '12'.
    ls_cmp_item_data-baseunitofmeasureid = 'xyz'.
*

    cl_abap_unit_assert=>assert_equals(
      act   = lines( lt_act_cmp_item_data )
      exp   = lines( lt_exp_cmp_item_data ) ).



  ENDMETHOD.


  METHOD get_Products.

    DATA: lt_exp_products TYPE /vpcoe/t_uph_entity_data,
          ls_products     TYPE /vpcoe/s_uph_ent_pack_prod,
          lt_act_products TYPE /vpcoe/t_uph_entity_data,
          lv_deleted      TYPE abap_bool.

    lt_act_products = f_cut->get_products(  ).

    lv_deleted = ' '.


    CLEAR: ls_products.
    ls_products-productid = 'PRODUCT ID'.
*
    cl_abap_unit_assert=>assert_equals(
      act   = lines( lt_act_products )
      exp   = lines( lt_exp_products ) ).

*



  ENDMETHOD.




ENDCLASS.
