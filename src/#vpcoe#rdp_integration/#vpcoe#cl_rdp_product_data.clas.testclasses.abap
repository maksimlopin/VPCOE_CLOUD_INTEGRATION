
CLASS /vpcoe/cl_product_ut DEFINITION FOR TESTING FINAL
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>/vpcoe/cl_Product_Ut
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>/VPCOE/CL_rdp_PRODUCT_DATA
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_rdp_product_data.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: get_product FOR TESTING.
    METHODS: get_product_delta FOR TESTING.
ENDCLASS.       "/vpcoe/cl_Product_Ut


CLASS /vpcoe/cl_product_ut IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.

    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                              iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
                                              iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product ).

    f_cut = NEW /vpcoe/cl_rdp_product_data( iv_api_type     = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                            iv_package_size = lo_cust->get_package_size( )
                                            iv_source       = /vpcoe/cl_common_helper=>get_source_id( ) ).


  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD get_product.

    DATA: ls_sel_opt     TYPE /vpcoe/s_selopt_product,
          lt_product_act TYPE /vpcoe/cl_rdp_product_data=>gty_t_product_combined,
          lt_product_exp TYPE /vpcoe/cl_rdp_product_data=>gty_t_product_combined.

    ls_sel_opt-matnr = VALUE #( ( sign = 'I' option = 'EQ' low = '000000000000000021'  ) ).


    lt_product_exp = VALUE #( ( id                            = '000000000000000021'
                                type                          = 'FERT'
                                is_marked_for_deletion        = 'false'
                                base_unit_of_measure          = 'EA'
                                division                      = ''
                                product_group                 = 'YBSVM1'
*                                gross_weight                  = '1.000'
*                                weight_unit                   = 'KG'
*                                net_weight                    = '1.000'
*                                volume_unit                   = ''
*                                material_volume               = ''
*                                internationalarticlenumbercat = ''

                                texts                         = VALUE #( ( language = 'EN' name =  'AST CKD1' ) )
                                product_unit_of_measures      = VALUE #( ( measurement_unit = 'EA'
                                                                      numerator = 1
                                                                      denominator = 1
*                                                                      material_volume = '0.000'
*                                                                      volume_unit = ''
*                                                                      gross_weight = '1.000'
*                                                                      weight_unit = 'KG'
*                                                                      global_trade_item_number = ''
*                                                                      unit_specific_product_length = '0.000'
*                                                                      unit_specific_product_width = '0.000'
*                                                                      unit_specific_product_height = '0.000'
                                                                      product_measurement_unit = ''
                                                                      base_unit ='EA' ) )
*                                                                      alternative_unit = '' ) )
                                product_sales                 = VALUE #( ( sales_organization = 'STBL' distribution_channel = 10 ) )
*                                product_plants                = VALUE #( ( id = 'STBL' ) )
                                ) ).


    f_cut->get_product(
      EXPORTING
        is_sel_opt        = ls_sel_opt
      IMPORTING
        et_product        = lt_product_act
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_product_act
      exp   = lt_product_exp
      msg   = 'Product assertion failed'
    ).

  ENDMETHOD.


  METHOD get_product_delta.

    DATA: ls_sel_opt     TYPE /vpcoe/s_selopt_product,
          lt_product_act TYPE /vpcoe/cl_rdp_product_data=>gty_t_product,
          lt_product_exp TYPE /vpcoe/cl_rdp_product_data=>gty_t_product,
          lt_product     TYPE /vpcoe/cl_rdp_product_data=>gty_t_product_combined.

    lt_product_exp = VALUE #( ( id                            = 'RDP-BM-PE-HANDLE'
                                type                          = 'VERP'
                                is_marked_for_deletion        = 'f'
                                base_unit_of_measure          = 'EA' )
*                                weight_unit                   = 'G'
*                                gross_weight                  = '100'
*                                net_weight                    = '90' )
                              ( id                            = 'RDP-BM-PE-HONEYCOM'
                                type                          = 'VERP'
                                is_marked_for_deletion        = 'f'
                                base_unit_of_measure          = 'ST' )
                              ( id                            = 'RDP-BM-PE-PARTITIO'
                                type                          = 'VERP'
                                is_marked_for_deletion        = 'f'
                                base_unit_of_measure          = 'EA'
*                                gross_weight                  = '100'
*                                weight_unit                   = 'G'
*                                net_weight                    = '90' )
                                ) ).


    f_cut->get_product_delta(
      EXPORTING
        is_sel_opt        = ls_sel_opt
        iv_chng_pointer_id = '/VPCOE/PRODUCT'
      IMPORTING
        et_product        = lt_product
    ).

    lt_product_act = CORRESPONDING #( lt_product ).

    MOVE-CORRESPONDING lt_product TO lt_product_act.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_product_act
      exp   = lt_product_exp
      msg   = 'Product delta assertion failed'
    ).
  ENDMETHOD.

ENDCLASS.
