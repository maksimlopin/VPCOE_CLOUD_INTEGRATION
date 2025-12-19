**"* use this source file for your ABAP unit test classes
*
*CLASS ltc_wrap_mcl_version_base DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PROTECTED SECTION.
*    METHODS given_empty_initialization.
*    METHODS initialize_cut.
*
*    DATA m_cut                 TYPE REF TO cl_surdp_uph_wrap_mcl_version.  " class under test
*
*    DATA mv_mcl_header         TYPE if_surdp_uph_entity_mcl_proc=>gty_mat_class_parameter.
*    DATA mt_mcl_character_data TYPE tt_bapi1003_alloc_values_char.
*    DATA mt_mcl_number_data    TYPE tt_bapi1003_alloc_values_num.
*    DATA mt_mcl_currency_data  TYPE tt_bapi1003_alloc_values_curr.
*
*ENDCLASS.
*
*
*
*CLASS ltc_wrap_mcl_version_base IMPLEMENTATION.
*  METHOD given_empty_initialization.
*    CLEAR mv_mcl_header.
*    CLEAR mt_mcl_character_data.
*    CLEAR mt_mcl_number_data.
*    CLEAR mt_mcl_currency_data.
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD initialize_cut.
*    m_cut = NEW cl_surdp_uph_wrap_mcl_version( is_mcl_header         = mv_mcl_header
*                                               it_mcl_character_data = mt_mcl_character_data
*                                               it_mcl_number_data    = mt_mcl_number_data
*                                               it_mcl_currency_data  = mt_mcl_currency_data ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_boolean_val DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*    METHODS valid_boolan_values         FOR TESTING.
*    METHODS no_boolan_value             FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_boolean_val IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    mt_mcl_character_data = VALUE #( ( charact = 'valid_initial' value_char = ''           value_neutral = '')
*                                     ( charact = 'valid_Yes'     value_char = 'Yes'        value_neutral = 'Y')
*                                     ( charact = 'valid_No'      value_char = 'No'         value_neutral = 'N')
*                                     ( charact = 'valid_X'       value_char = 'X'          value_neutral = 'X')
*                                     ( charact = 'no_boolean'    value_char = 'some_value' value_neutral = 'some_value') ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = abap_undefined act = m_cut->get_boolean_val( iv_name_char = 'abc' ) ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_false( m_cut->get_boolean_val( iv_name_char = 'valid_No' ) ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = abap_undefined act = m_cut->get_boolean_val( iv_name_char = 'invalid_name' ) ).
*  ENDMETHOD.
*
*
*  METHOD valid_boolan_values.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_false( m_cut->get_boolean_val( iv_name_char = 'valid_initial' ) ).
*    cl_abap_unit_assert=>assert_false( m_cut->get_boolean_val( iv_name_char = 'valid_No' ) ).
*
*    cl_abap_unit_assert=>assert_true( m_cut->get_boolean_val( iv_name_char = 'valid_Yes' ) ).
*    cl_abap_unit_assert=>assert_true( m_cut->get_boolean_val( iv_name_char = 'valid_X' ) ).
*  ENDMETHOD.
*
*
*  METHOD no_boolan_value.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_false( m_cut->get_boolean_val( iv_name_char = 'no_boolean' ) ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_character_val DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_character_val IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    mt_mcl_character_data = VALUE #( ( charact = 'val1' value_char = 'char_value1')
*                                     ( charact = 'val2' value_char = 'char_value2')
*                                     ( charact = 'val3' value_char = 'char_value3') ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = '' act = m_cut->get_character_val( iv_name_char = 'abc' ) ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 'char_value1' act = m_cut->get_character_val( iv_name_char = 'val1' ) ).
*    cl_abap_unit_assert=>assert_equals( exp = 'char_value2' act = m_cut->get_character_val( iv_name_char = 'val2' ) ).
*    cl_abap_unit_assert=>assert_equals( exp = 'char_value3' act = m_cut->get_character_val( iv_name_char = 'val3' ) ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = '' act = m_cut->get_character_val( iv_name_char = 'invalid' ) ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_number_val DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_number_val IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    DATA value1 TYPE atflv VALUE '1.0'.
*    DATA value2 TYPE atflv VALUE '2.0'.
*    DATA value3 TYPE atflv VALUE '3.0'.
*
*    mt_mcl_number_data = VALUE #( ( charact = 'val1' value_from = value1 )
*                                  ( charact = 'val2' value_from = value2 )
*                                  ( charact = 'val3' value_from = value3 ) ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_number_val( iv_name_char = 'abc' ) ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '1.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_number_val( iv_name_char = 'val1' ) ).
*
*    expected_value = '2.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_number_val( iv_name_char = 'val2' ) ).
*
*    expected_value = '3.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_number_val( iv_name_char = 'val3' ) ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_number_val( iv_name_char = 'invalid' ) ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_quantity_val DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_quantity_val IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    DATA value1 TYPE atflv VALUE '1.0'.
*    DATA value2 TYPE atflv VALUE '2.0'.
*    DATA value3 TYPE atflv VALUE '3.0'.
*
*    mt_mcl_number_data = VALUE #( ( charact = 'val1' value_from = value1 )
*                                  ( charact = 'val2' value_from = value2 )
*                                  ( charact = 'val3' value_from = value3 ) ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_quantity_val( iv_name_char = 'abc' ) ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '1.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_quantity_val( iv_name_char = 'val1' ) ).
*
*    expected_value = '2.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_quantity_val( iv_name_char = 'val2' ) ).
*
*    expected_value = '3.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_quantity_val( iv_name_char = 'val3' ) ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = m_cut->get_quantity_val( iv_name_char = 'invalid' ) ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_quantity_unit DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_quantity_unit IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    mt_mcl_number_data = VALUE #( ( charact = 'uom1' unit_from = 'ABC' )
*                                  ( charact = 'uom2' unit_from = 'DEF' )
*                                  ( charact = 'uom3' unit_from = 'GH' ) ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = '' act = m_cut->get_quantity_unit( iv_name_char = 'abc' ) ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 'ABC' act = m_cut->get_quantity_unit( iv_name_char = 'uom1' ) ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 'DEF' act = m_cut->get_quantity_unit( iv_name_char = 'uom2' ) ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 'GH' act = m_cut->get_quantity_unit( iv_name_char = 'uom3' ) ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = '' act = m_cut->get_quantity_unit( iv_name_char = 'invalid' ) ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_internal_data DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_internal_data IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    DATA lv_objek TYPE cuobn VALUE 'prod_id'.
*    DATA lv_datuv TYPE datuv VALUE '20221011'.
*
*    mv_mcl_header = VALUE #( objek = lv_objek datuv = lv_datuv ).
*
*    mt_mcl_character_data = VALUE #( ( charact = 'att1' value_char = 'char_value1' )
*                                     ( charact = 'att2' value_char = 'char_value2' )
*                                     ( charact = 'att3' value_char = 'char_value3' ) ).
*
*    mt_mcl_number_data = VALUE #( value_to = '2.0'
*                                  ( charact = 'att1' value_from = '1.0' unit_from = 'KG'  unit_to = 'KG' )
*                                  ( charact = 'att2' value_from = '5.0' unit_from = 'GR'  unit_to = 'GR' ) ).
*
*    mt_mcl_currency_data = VALUE #( ( charact = 'att1' value_from = '8' value_to = '15' ) ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    m_cut->get_internal_data( IMPORTING es_mcl_header         = DATA(lv_header)
*                                        et_mcl_character_data = DATA(lt_character_data)
*                                        et_mcl_number_data    = DATA(lt_number_data)
*                                        et_mcl_currency_data  = DATA(lt_currency_data) ).
*
*    cl_abap_unit_assert=>assert_equals( exp = mv_mcl_header         act = lv_header ).
*    cl_abap_unit_assert=>assert_equals( exp = mt_mcl_character_data act = lt_character_data ).
*    cl_abap_unit_assert=>assert_equals( exp = mt_mcl_number_data    act = lt_number_data ).
*    cl_abap_unit_assert=>assert_equals( exp = mt_mcl_currency_data  act = lt_currency_data ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    m_cut->get_internal_data( IMPORTING es_mcl_header         = DATA(lv_header)
*                                        et_mcl_character_data = DATA(lt_character_data)
*                                        et_mcl_number_data    = DATA(lt_number_data)
*                                        et_mcl_currency_data  = DATA(lt_currency_data) ).
*
*    cl_abap_unit_assert=>assert_equals( exp = mv_mcl_header         act = lv_header ).
*    cl_abap_unit_assert=>assert_equals( exp = mt_mcl_character_data act = lt_character_data ).
*    cl_abap_unit_assert=>assert_equals( exp = mt_mcl_number_data    act = lt_number_data ).
*    cl_abap_unit_assert=>assert_equals( exp = mt_mcl_currency_data  act = lt_currency_data ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_product_id DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization FOR TESTING.
*    METHODS valid_prod_id        FOR TESTING.
*
*    METHODS given_header_data.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_product_id IMPLEMENTATION.
*  METHOD given_header_data.
*    DATA lv_objek TYPE cuobn VALUE 'prod_id'.
*    DATA lv_datuv TYPE datuv VALUE '20221011'.
*
*    mv_mcl_header = VALUE #( objek = lv_objek datuv = lv_datuv ).
*
*    mt_mcl_character_data = VALUE #( ).
*    mt_mcl_number_data = VALUE #( ).
*    mt_mcl_currency_data = VALUE #( ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization.
*    given_empty_initialization( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = ''  act = m_cut->get_product_id( ) ).
*  ENDMETHOD.
*
*
*  METHOD valid_prod_id.
*    given_header_data( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 'prod_id'  act = m_cut->get_product_id( ) ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_number_range DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_number_range IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    DATA lv_objek TYPE cuobn VALUE 'key'.
*    DATA lv_datuv TYPE datuv VALUE '20221011'.
*
*    mv_mcl_header = VALUE #( objek = lv_objek datuv = lv_datuv ).
*
*    mt_mcl_number_data = VALUE #( ( charact = 'att1' value_from = '1.0' value_to = '2.0' )
*                                  ( charact = 'att2' value_from = '5.0' value_to = '9.0' ) ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    m_cut->get_number_range( EXPORTING iv_name_char      = 'abc'
*                             IMPORTING ev_num_value_from = DATA(lv_number_value_from)
*                                       ev_num_value_to   = DATA(lv_number_value_to) ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_number_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_number_value_to ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    DATA lv_expected_num_val    TYPE /vpcoe/ehfnd_ehs_atflv.
*    DATA lv_expected_num_val_to TYPE /vpcoe/ehfnd_ehs_atflb.
*
*    m_cut->get_number_range( EXPORTING iv_name_char      = 'att1'
*                             IMPORTING ev_num_value_from = DATA(lv_number_value_from)
*                                       ev_num_value_to   = DATA(lv_number_value_to) ).
*
*    lv_expected_num_val = '1.0'.
*    lv_expected_num_val_to = '2.0'.
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val       act = lv_number_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val_to    act = lv_number_value_to ).
*
*    m_cut->get_number_range( EXPORTING iv_name_char      = 'att2'
*                             IMPORTING ev_num_value_from = lv_number_value_from
*                                       ev_num_value_to   = lv_number_value_to ).
*
*    lv_expected_num_val = '5.0'.
*    lv_expected_num_val_to = '9.0'.
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val       act = lv_number_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val_to    act = lv_number_value_to ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    m_cut->get_number_range( EXPORTING iv_name_char      = 'invalid'
*                             IMPORTING ev_num_value_from = DATA(lv_number_value_from)
*                                       ev_num_value_to   = DATA(lv_number_value_to) ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_number_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_number_value_to ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_quantity DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_quantity IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    DATA lv_objek TYPE cuobn VALUE 'key'.
*    DATA lv_datuv TYPE datuv VALUE '20221011'.
*
*    mv_mcl_header = VALUE #( objek = lv_objek datuv = lv_datuv ).
*
*    mt_mcl_number_data = VALUE #( ( charact = 'att1' value_from = '1.0' unit_from = 'KG' )
*                                  ( charact = 'att2' value_from = '5.0' unit_from = 'GR' ) ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    m_cut->get_quantity( EXPORTING iv_name_char = 'abc'
*                         IMPORTING ev_value     = DATA(lv_value_from)
*                                   ev_unit      = DATA(lv_unit_from) ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_unit_from ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    DATA lv_expected_num_val   TYPE /vpcoe/ehfnd_ehs_atflv.
*    DATA lv_expected_meas_unit TYPE /vpcoe/ehfnd_ehs_msehi.
*
*    m_cut->get_quantity( EXPORTING iv_name_char = 'att1'
*                         IMPORTING ev_value     = DATA(lv_value_from)
*                                   ev_unit      = DATA(lv_unit_from) ).
*
*    lv_expected_num_val = '1.0'.
*    lv_expected_meas_unit = 'KG'.
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val       act = lv_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_meas_unit     act = lv_unit_from ).
*
*    m_cut->get_quantity( EXPORTING iv_name_char = 'att2'
*                         IMPORTING ev_value     = lv_value_from
*                                   ev_unit      = lv_unit_from ).
*
*    lv_expected_num_val = '5.0'.
*    lv_expected_meas_unit = 'GR'.
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val       act = lv_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_meas_unit     act = lv_unit_from ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    m_cut->get_quantity( EXPORTING iv_name_char = 'invalid'
*                         IMPORTING ev_value     = DATA(lv_value_from)
*                                   ev_unit      = DATA(lv_unit_from) ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_unit_from ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_quantity_range DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_quantity_range IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    DATA lv_objek TYPE cuobn VALUE 'key'.
*    DATA lv_datuv TYPE datuv VALUE '20221011'.
*
*    mv_mcl_header = VALUE #( objek = lv_objek datuv = lv_datuv ).
*
*    mt_mcl_number_data = VALUE #(
*        ( charact = 'att1' value_from = '1.0' unit_from = 'KG'  value_to = '2.0' unit_to = 'KG' )
*        ( charact = 'att2' value_from = '5.0' unit_from = 'GR'  value_to = '9.0' unit_to = 'GR' ) ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    m_cut->get_quantity_range( EXPORTING iv_name_char           = 'abc'
*                               IMPORTING ev_quantity_value_from = DATA(lv_quantity_value_from)
*                                         ev_quantity_unit_from  = DATA(lv_quantity_unit_from)
*                                         ev_quantity_value_to   = DATA(lv_quantity_value_to)
*                                         ev_quantity_unit_to    = DATA(lv_quantity_unit_to) ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_quantity_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_quantity_unit_from ).
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_quantity_value_to ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_quantity_unit_to ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    DATA lv_expected_num_val      TYPE /vpcoe/ehfnd_ehs_atflv.
*    DATA lv_expected_meas_unit    TYPE /vpcoe/ehfnd_ehs_msehi.
*    DATA lv_expected_num_val_to   TYPE /vpcoe/ehfnd_ehs_atflb.
*    DATA lv_expected_meas_unit_to TYPE /vpcoe/ehfnd_ehs_msehi.
*
*    m_cut->get_quantity_range( EXPORTING iv_name_char           = 'att1'
*                               IMPORTING ev_quantity_value_from = DATA(lv_quantity_value_from)
*                                         ev_quantity_unit_from  = DATA(lv_quantity_unit_from)
*                                         ev_quantity_value_to   = DATA(lv_quantity_value_to)
*                                         ev_quantity_unit_to    = DATA(lv_quantity_unit_to) ).
*
*    lv_expected_num_val = '1.0'.
*    lv_expected_meas_unit = 'KG'.
*    lv_expected_num_val_to = '2.0'.
*    lv_expected_meas_unit_to = 'KG'.
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val       act = lv_quantity_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_meas_unit     act = lv_quantity_unit_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val_to    act = lv_quantity_value_to ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_meas_unit_to  act = lv_quantity_unit_to ).
*
*    m_cut->get_quantity_range( EXPORTING iv_name_char           = 'att2'
*                               IMPORTING ev_quantity_value_from = lv_quantity_value_from
*                                         ev_quantity_unit_from  = lv_quantity_unit_from
*                                         ev_quantity_value_to   = lv_quantity_value_to
*                                         ev_quantity_unit_to    = lv_quantity_unit_to ).
*
*    lv_expected_num_val = '5.0'.
*    lv_expected_meas_unit = 'GR'.
*    lv_expected_num_val_to = '9.0'.
*    lv_expected_meas_unit_to = 'GR'.
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val       act = lv_quantity_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_meas_unit     act = lv_quantity_unit_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_num_val_to    act = lv_quantity_value_to ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_meas_unit_to  act = lv_quantity_unit_to ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    m_cut->get_quantity_range( EXPORTING iv_name_char           = 'invalid'
*                               IMPORTING ev_quantity_value_from = DATA(lv_quantity_value_from)
*                                         ev_quantity_unit_from  = DATA(lv_quantity_unit_from)
*                                         ev_quantity_value_to   = DATA(lv_quantity_value_to)
*                                         ev_quantity_unit_to    = DATA(lv_quantity_unit_to) ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_quantity_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_quantity_unit_from ).
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_quantity_value_to ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_quantity_unit_to ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_currency DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_currency IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    mt_mcl_currency_data = VALUE #(
*        ( charact = 'att1' value_from = '1.0' currency_from = 'EUR'  value_to = '2.0' currency_to = 'EUR' )
*        ( charact = 'att2' value_from = '5.0' currency_from = 'USD'  value_to = '9.0' currency_to = 'USD' ) ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    m_cut->get_currency( EXPORTING iv_name_char           = 'att1'
*                         IMPORTING ev_currency_value_from = DATA(lv_value_from)
*                                   ev_currency_from       = DATA(lv_currency_from)
*                                   ev_currency_value_to   = DATA(lv_value_to)
*                                   ev_currency_to         = DATA(lv_currency_to) ).
*
*    DATA expected_value TYPE /vpcoe/ehfnd_ehs_atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_currency_from ).
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_value_to ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_currency_to ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    DATA lv_expected_value_from    TYPE atflv.
*    DATA lv_expected_currency_from TYPE waers.
*    DATA lv_expected_value_to      TYPE atflb.
*    DATA lv_expected_currency_to   TYPE waers.
*
*    m_cut->get_currency( EXPORTING iv_name_char           = 'att1'
*                         IMPORTING ev_currency_value_from = DATA(lv_value_from)
*                                   ev_currency_from       = DATA(lv_currency_from)
*                                   ev_currency_value_to   = DATA(lv_value_to)
*                                   ev_currency_to         = DATA(lv_currency_to) ).
*
*    lv_expected_value_from    = '1.0'.
*    lv_expected_currency_from = 'EUR'.
*    lv_expected_value_to      = '2.0'.
*    lv_expected_currency_to   = 'EUR'.
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_value_from       act = lv_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_currency_from    act = lv_currency_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_value_to         act = lv_value_to ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_currency_to      act = lv_currency_to ).
*
*    m_cut->get_currency( EXPORTING iv_name_char           = 'att2'
*                         IMPORTING ev_currency_value_from = lv_value_from
*                                   ev_currency_from       = lv_currency_from
*                                   ev_currency_value_to   = lv_value_to
*                                   ev_currency_to         = lv_currency_to ).
*
*    lv_expected_value_from    = '5.0'.
*    lv_expected_currency_from = 'USD'.
*    lv_expected_value_to      = '9.0'.
*    lv_expected_currency_to   = 'USD'.
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_value_from       act = lv_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_currency_from    act = lv_currency_from ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_value_to         act = lv_value_to ).
*    cl_abap_unit_assert=>assert_equals( exp = lv_expected_currency_to      act = lv_currency_to ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    m_cut->get_currency( EXPORTING iv_name_char           = 'invalid'
*                         IMPORTING ev_currency_value_from = DATA(lv_value_from)
*                                   ev_currency_from       = DATA(lv_currency_from)
*                                   ev_currency_value_to   = DATA(lv_value_to)
*                                   ev_currency_to         = DATA(lv_currency_to) ).
*
*    DATA expected_value TYPE atflv VALUE '0.0'.
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_value_from ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_currency_from ).
*    cl_abap_unit_assert=>assert_equals( exp = expected_value act = lv_value_to ).
*    cl_abap_unit_assert=>assert_equals( exp = ''             act = lv_currency_to ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_get_neutral_val DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS empty_initialization_params FOR TESTING.
*    METHODS valid_characteristic_name   FOR TESTING.
*    METHODS invalid_characteristic_name FOR TESTING.
*
*    METHODS given_various_characteristics.
*
*ENDCLASS.
*
*
*
*CLASS ltc_get_neutral_val IMPLEMENTATION.
*  METHOD given_various_characteristics.
*    mt_mcl_character_data = VALUE #(
*        ( charact = 'val1' value_neutral_long = 'char_value1_12345678901234567890123456789012345678901234567890')
*        ( charact = 'val2' value_neutral_long = 'char_value2_12345678901234567890123456789012345678901234567890')
*        ( charact = 'val3' value_neutral_long = 'char_value3_12345678901234567890123456789012345678901234567890') ).
*
*    initialize_cut( ).
*  ENDMETHOD.
*
*
*  METHOD empty_initialization_params.
*    given_empty_initialization( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = '' act = m_cut->get_neutral_val( iv_name_char = 'abc' ) ).
*  ENDMETHOD.
*
*
*  METHOD valid_characteristic_name.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 'char_value1_12345678901234567890123456789012345678901234567890'
*                                        act = m_cut->get_neutral_val( iv_name_char = 'val1' ) ).
*    cl_abap_unit_assert=>assert_equals( exp = 'char_value2_12345678901234567890123456789012345678901234567890'
*                                        act = m_cut->get_neutral_val( iv_name_char = 'val2' ) ).
*    cl_abap_unit_assert=>assert_equals( exp = 'char_value3_12345678901234567890123456789012345678901234567890'
*                                        act = m_cut->get_neutral_val( iv_name_char = 'val3' ) ).
*  ENDMETHOD.
*
*
*  METHOD invalid_characteristic_name.
*    given_various_characteristics( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = '' act = m_cut->get_neutral_val( iv_name_char = 'invalid' ) ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_header_attributes DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*  INHERITING FROM ltc_wrap_mcl_version_base.
*
*  PRIVATE SECTION.
*    METHODS get_valid_from          FOR TESTING.
*    METHODS is_marked_deleted_true  FOR TESTING.
*    METHODS is_marked_deleted_false FOR TESTING.
*
*ENDCLASS.
*
*
*
*CLASS ltc_header_attributes IMPLEMENTATION.
*  METHOD get_valid_from.
*    mv_mcl_header = VALUE #( objek = 'Prod1' datuv = '20220101' datub = '20230101' ).
*    initialize_cut( ).
*    cl_abap_unit_assert=>assert_equals( exp = '20220101' act = m_cut->get_valid_from( ) ).
*  ENDMETHOD.
*
*
*  METHOD is_marked_deleted_true.
*    mv_mcl_header = VALUE #( objek = 'Prod1' lvorm = 'X' ).
*    initialize_cut( ).
*    cl_abap_unit_assert=>assert_true( m_cut->is_marked_deleted( ) ).
*  ENDMETHOD.
*
*
*  METHOD is_marked_deleted_false.
*    mv_mcl_header = VALUE #( objek = 'Prod1' lvorm = ' ' ).
*    initialize_cut( ).
*    cl_abap_unit_assert=>assert_false( m_cut->is_marked_deleted( ) ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS th_wrap_mcl_version_mock_bapi DEFINITION FOR TESTING
*  INHERITING FROM cl_surdp_uph_wrap_mcl_version.
*
*  PUBLIC SECTION.                                  "#EC NUM_PUBLIC_ATTR
*    DATA m_expected_atnam TYPE atnam.
*    DATA m_returned_uom   TYPE meins.
*  PROTECTED SECTION.
*    METHODS call_bapi_charact_getdetail REDEFINITION.
*ENDCLASS.
*
*CLASS th_wrap_mcl_version_mock_bapi IMPLEMENTATION.
*
*  METHOD call_bapi_charact_getdetail.
*    cl_abap_unit_assert=>assert_equals( msg = 'Expected call with different characteristic name' exp = m_expected_atnam act = iv_characteristic_name ).
*
*    rv_details-unit_of_measurement = m_returned_uom.
*  ENDMETHOD.
*ENDCLASS.
*
*CLASS ltcl_get_uom_of_characteristic DEFINITION FINAL FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    METHODS uom_existing FOR TESTING RAISING cx_static_check.
*ENDCLASS.
*
*CLASS ltcl_get_uom_of_characteristic IMPLEMENTATION.
*  METHOD uom_existing.
*
*    DATA ls_mcl_header  TYPE if_surdp_uph_entity_mcl_proc=>gty_mat_class_parameter.
*    DATA lt_mcl_character_data  TYPE tt_bapi1003_alloc_values_char.
*    DATA lt_mcl_number_data  TYPE tt_bapi1003_alloc_values_num.
*    DATA lt_mcl_currency_data  TYPE tt_bapi1003_alloc_values_curr.
*
*    " Given: cut with mocked bapi call
*    DATA(lo_cut_with_mock) = NEW th_wrap_mcl_version_mock_bapi( is_mcl_header         = ls_mcl_header
*                                                                it_mcl_character_data = lt_mcl_character_data
*                                                                it_mcl_number_data    = lt_mcl_number_data
*                                                                it_mcl_currency_data  = lt_mcl_currency_data ).
*    lo_cut_with_mock->m_expected_atnam = 'MY_CHARACTERISTIC'.
*    lo_cut_with_mock->m_returned_uom = 'KG'.
*
*    " when
*    DATA(lv_actual_uom) = lo_cut_with_mock->get_uom_of_characteristic( 'MY_CHARACTERISTIC' ).
*
*    " then
*    cl_abap_unit_assert=>assert_equals( exp = 'KG' act = lv_actual_uom ).
*  ENDMETHOD.
*
*ENDCLASS.
