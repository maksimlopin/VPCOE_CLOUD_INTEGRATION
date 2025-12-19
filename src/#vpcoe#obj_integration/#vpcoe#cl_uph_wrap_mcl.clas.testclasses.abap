CLASS ltcl_wrap_mcl_version_base DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PROTECTED SECTION.
    METHODS given_mcl_wrapper IMPORTING iv_objek        TYPE cuobn
                                        iv_matnr        TYPE matnr
                                        it_mcl_versions TYPE /vpcoe/uph_wrap_mcl_version.

    METHODS given_version_for IMPORTING iv_objek         TYPE cuobn
                                        iv_matnr         TYPE matnr
                                        iv_datuv         TYPE datuv    OPTIONAL
                                        iv_datub         TYPE datub    OPTIONAL
                                        iv_class         TYPE klasse_d OPTIONAL
                                        iv_lvorm         TYPE lvorm    OPTIONAL
                              RETURNING VALUE(rv_result) TYPE REF TO /vpcoe/cl_uph_wrap_mcl_version.

    METHODS then_assert_versions IMPORTING it_expected_versions TYPE /vpcoe/uph_wrap_mcl_version
                                           it_actual_versions   TYPE /vpcoe/uph_wrap_mcl_version.

    DATA m_cut TYPE REF TO /vpcoe/cl_uph_wrap_mcl.
ENDCLASS.



CLASS ltcl_wrap_mcl_version_base IMPLEMENTATION.
  METHOD given_mcl_wrapper.
    m_cut = NEW #( iv_objek = iv_objek iv_matnr = iv_matnr it_mcl_versions = it_mcl_versions ).
  ENDMETHOD.


  METHOD given_version_for.
    DATA ls_mcl_header         TYPE /vpcoe/if_uph_entity_mcl_proc=>gty_mat_class_parameter.
    DATA lt_mcl_character_data TYPE tt_bapi1003_alloc_values_char.
    DATA lt_mcl_number_data    TYPE tt_bapi1003_alloc_values_num.
    DATA lt_mcl_currency_data  TYPE tt_bapi1003_alloc_values_curr.

    ls_mcl_header-objek = iv_objek.
    ls_mcl_header-datuv = iv_datuv.
*    ls_mcl_header-datub = iv_datub.
    ls_mcl_header-class = iv_class.
    ls_mcl_header-lvorm = iv_lvorm.

    lt_mcl_character_data = VALUE #( ( charact = 'attribute1' value_char = 'val_att_1_' && iv_objek )
                                     ( charact = 'attribute2' value_char = 'val_att_2_' && iv_objek ) ).
    lt_mcl_number_data = VALUE #( ( charact = 'number1' value_from = '42.0' )
                                  ( charact = 'number2' value_from = '53.0' )
                                  ( charact = 'number3' value_from = '64.0' ) ).
    lt_mcl_currency_data = VALUE #( ( charact = 'currency1' value_from = '21.0' )
                                    ( charact = 'currency2' value_from = '23.0' ) ).

    rv_result = NEW #( is_mcl_header         = ls_mcl_header
                       it_mcl_character_data = lt_mcl_character_data
                       it_mcl_number_data    = lt_mcl_number_data
                       it_mcl_currency_data  = lt_mcl_currency_data ).
  ENDMETHOD.


  METHOD then_assert_versions.
    cl_abap_unit_assert=>assert_equals( exp = it_expected_versions  act = it_actual_versions ).
  ENDMETHOD.
ENDCLASS.



CLASS ltcl_get_versions_by_class DEFINITION FINAL FOR TESTING INHERITING FROM ltcl_wrap_mcl_version_base
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS empty_versions                FOR TESTING RAISING cx_static_check.
    METHODS version_by_class_only         FOR TESTING RAISING cx_static_check.
    METHODS version_by_class_and_key_date FOR TESTING RAISING cx_static_check.

ENDCLASS.



CLASS ltcl_get_versions_by_class IMPLEMENTATION.
  METHOD empty_versions.
    given_mcl_wrapper( iv_objek        = ''
                       iv_matnr        = ''
                       it_mcl_versions = VALUE #( ) ).

    " when
    DATA(lt_actual_versions) = m_cut->get_versions_by_class( iv_class = 'MY_CLASS_1' ).

    then_assert_versions( it_expected_versions = VALUE #( )
                          it_actual_versions   = lt_actual_versions ).
  ENDMETHOD.


  METHOD version_by_class_only.
    DATA(lv_version1) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1').
    DATA(lv_version2) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_2').
    DATA(lv_version3) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1').

    given_mcl_wrapper( iv_objek        = 'PROD_1'
                       iv_matnr        = 'PROD_1'
                       it_mcl_versions = VALUE #( ( lv_version1 )
                                                  ( lv_version2 )
                                                  ( lv_version3 ) ) ).

    " when
    DATA(lt_actual_versions) = m_cut->get_versions_by_class( iv_class = 'MY_CLASS_1' ).

    then_assert_versions( it_expected_versions = VALUE #( ( lv_version1 ) ( lv_version3 ) )
                          it_actual_versions   = lt_actual_versions ).
  ENDMETHOD.


  METHOD version_by_class_and_key_date.
    DATA(lv_version1) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1' iv_datuv = '20221001' iv_datub = '20221015' ).
    DATA(lv_version2) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1' iv_datuv = '20221016' iv_datub = '20221022' ).
    DATA(lv_version3) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_2' iv_datuv = '20221016' iv_datub = '20221022' ).
    DATA(lv_version4) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1' iv_datuv = '20221022' iv_datub = '20221031' ).
    DATA(lv_version5) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1' iv_datuv = '20221015' iv_datub = '20221019' ).
    DATA(lv_version6) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1' iv_datuv = '20221019' iv_datub = '20221023' ).
    DATA(lv_version7) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_2' iv_datuv = '20221019' iv_datub = '20221019' ).

    given_mcl_wrapper( iv_objek        = 'PROD_1'
                       iv_matnr        = 'PROD_1'
                       it_mcl_versions = VALUE #( ( lv_version1 )
                                                  ( lv_version2 )
                                                  ( lv_version3 )
                                                  ( lv_version4 )
                                                  ( lv_version5 )
                                                  ( lv_version6 )
                                                  ( lv_version7 ) ) ).

    " when
    DATA(lt_actual_versions) = m_cut->get_versions_by_class( iv_class = 'MY_CLASS_1' iv_valid_from = '20221019' ).

    then_assert_versions( it_expected_versions = VALUE #( ( lv_version2 ) ( lv_version5 ) ( lv_version6 ) )
                          it_actual_versions   = lt_actual_versions ).
  ENDMETHOD.
ENDCLASS.



CLASS ltcl_add_version DEFINITION FINAL FOR TESTING INHERITING FROM ltcl_wrap_mcl_version_base
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS empty_initial_versions FOR TESTING RAISING cx_static_check.
    METHODS with_initial_versions  FOR TESTING RAISING cx_static_check.
ENDCLASS.



CLASS ltcl_add_version IMPLEMENTATION.
  METHOD empty_initial_versions.
    given_mcl_wrapper( iv_objek        = 'PROD_1'
                       iv_matnr        = 'PROD_1'
                       it_mcl_versions = VALUE #( ) ).

    DATA(lv_added_version1) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1' iv_datuv = '20221001' iv_datub = '20221015' ).

    " when
    m_cut->add_version( io_mcl_version = lv_added_version1 ).

    then_assert_versions( it_expected_versions = VALUE #( ( lv_added_version1 ) )
                          it_actual_versions   = m_cut->get_versions( ) ).
  ENDMETHOD.


  METHOD with_initial_versions.
    DATA(lv_initial_version1) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1' iv_datuv = '20221001' iv_datub = '20221015' ).
    DATA(lv_initial_version2) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1' iv_datuv = '20221016' iv_datub = '20221022' ).

    given_mcl_wrapper( iv_objek        = 'PROD_1'
                       iv_matnr        = 'PROD_1'
                       it_mcl_versions = VALUE #( ( lv_initial_version1 ) ( lv_initial_version2 ) ) ).

    DATA(lv_added_version1) = given_version_for( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' iv_class = 'MY_CLASS_1' iv_datuv = '20221001' iv_datub = '20221015' ).

    " when
    m_cut->add_version( io_mcl_version = lv_added_version1 ).

    then_assert_versions(
        it_expected_versions = VALUE #( ( lv_initial_version1 ) ( lv_initial_version2 ) ( lv_added_version1 ) )
        it_actual_versions   = m_cut->get_versions( ) ).
  ENDMETHOD.
ENDCLASS.



CLASS ltcl_get_product_id DEFINITION FINAL FOR TESTING INHERITING FROM ltcl_wrap_mcl_version_base
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS product_id_existing FOR TESTING RAISING cx_static_check.
    METHODS product_id_empty FOR TESTING RAISING cx_static_check.
ENDCLASS.



CLASS ltcl_get_product_id IMPLEMENTATION.
  METHOD product_id_existing.
    given_mcl_wrapper( iv_objek        = 'PROD_1'
                       iv_matnr        = 'PROD_1'
                       it_mcl_versions = VALUE #( ) ).

    " when
    DATA(lv_objek) = m_cut->get_product_id( ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = 'PROD_1' act = lv_objek ).
  ENDMETHOD.

  METHOD product_id_empty.
    given_mcl_wrapper( iv_objek        = ''
                       iv_matnr        = ''
                       it_mcl_versions = VALUE #( ) ).

    " when
    DATA(lv_objek) = m_cut->get_product_id( ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = '' act = lv_objek ).
  ENDMETHOD.
ENDCLASS.



CLASS th_wrap_mcl_mocked_bapi_call DEFINITION FOR TESTING
  INHERITING FROM /vpcoe/cl_uph_wrap_mcl.

  PUBLIC SECTION. "#EC NUM_PUBLIC_ATTR
    DATA m_expected_mat_nr TYPE matnr.
    DATA m_returned_mat_desc TYPE c LENGTH 40.

  PROTECTED SECTION.
    METHODS call_bapi_material_get_detail REDEFINITION.

ENDCLASS.

CLASS th_wrap_mcl_mocked_bapi_call IMPLEMENTATION.

  METHOD call_bapi_material_get_detail.
    CLEAR: es_mat_general.

    cl_abap_unit_assert=>assert_equals( msg = 'Expected call with different matnr' exp = m_expected_mat_nr act = iv_matnr ).

    es_mat_general-matl_desc = m_returned_mat_desc.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_get_product_description DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS product_description_existing FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_get_product_description IMPLEMENTATION.
  METHOD product_description_existing.

    " Given: product id and cut with mocked bapi call
    DATA(lo_cut_with_mock) = NEW th_wrap_mcl_mocked_bapi_call( iv_objek = 'PROD_1' iv_matnr = 'PROD_1' it_mcl_versions = VALUE #( ) ).
    lo_cut_with_mock->m_expected_mat_nr = 'PROD_1'.
    lo_cut_with_mock->m_returned_mat_desc = 'Description for PROD_1'.

    " when
    DATA(lv_actual_description) = lo_cut_with_mock->get_product_description( ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = 'Description for PROD_1' act = lv_actual_description ).
  ENDMETHOD.

ENDCLASS.
