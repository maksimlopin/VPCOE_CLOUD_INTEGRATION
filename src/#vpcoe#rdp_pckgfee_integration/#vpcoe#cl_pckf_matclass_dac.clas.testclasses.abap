CLASS /vpcoe/tcl_pckf_matclass_dac DEFINITION DEFERRED.
CLASS /vpcoe/tcl_pckf_matclass_dac DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_pckf_matclass_dac.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.

    METHODS:
      convert_to_bapi_values FOR TESTING,
      convert_from_bapi_values FOR TESTING.

ENDCLASS.

CLASS /vpcoe/cl_pckf_matclass_dac DEFINITION LOCAL FRIENDS /vpcoe/tcl_pckf_matclass_dac.

CLASS /vpcoe/tcl_pckf_matclass_dac IMPLEMENTATION.

  METHOD class_setup.
  ENDMETHOD.

  METHOD class_teardown.
  ENDMETHOD.


  METHOD setup.
    f_cut = NEW #( ).
  ENDMETHOD.


  METHOD teardown.
  ENDMETHOD.

  METHOD convert_to_bapi_values.
    "given
    DATA lt_characteristic_char TYPE /vpcoe/if_pckf_matclass_dac=>gtyt_characteristic_value.
    DATA lt_characteristic_numb TYPE /vpcoe/if_pckf_matclass_dac=>gtyt_characteristic_value.
    DATA lt_characteristic_curr TYPE /vpcoe/if_pckf_matclass_dac=>gtyt_characteristic_value.
    lt_characteristic_char = VALUE #( ( characteristic = 'test'
                                        value = 'abc'
                                        data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-char ) ).
    lt_characteristic_numb = VALUE #( ( characteristic = 'test'
                                        value = 10
                                        unit_num = 'KG'
                                        data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-num ) ).
    lt_characteristic_curr = VALUE #( ( characteristic = 'test'
                                        value = 20
                                        value_to = '30'
                                        data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-curr ) ).

    "when
    f_cut->convert_to_bapi_values( EXPORTING it_characteristics = lt_characteristic_char
                                 IMPORTING et_char_values_char = DATA(lt_char_values_char) ).
    f_cut->convert_to_bapi_values( EXPORTING it_characteristics = lt_characteristic_numb
                                 IMPORTING et_char_values_num = DATA(lt_char_values_num) ).
    f_cut->convert_to_bapi_values( EXPORTING it_characteristics = lt_characteristic_curr
                                 IMPORTING et_char_values_curr = DATA(lt_char_values_curr) ).
    "then
    cl_abap_unit_assert=>assert_equals( act = lt_char_values_char[ 1 ]-charact
                                        exp = 'test').
    cl_abap_unit_assert=>assert_equals( act = lt_char_values_char[ 1 ]-value_char
                                        exp = 'abc').
    cl_abap_unit_assert=>assert_equals( act = lt_char_values_num[ 1 ]-charact
                                        exp = 'test').
    cl_abap_unit_assert=>assert_equals( act = lt_char_values_num[ 1 ]-value_from
                                        exp = 10 ).
    cl_abap_unit_assert=>assert_equals( act = lt_char_values_num[ 1 ]-unit_from
                                        exp = 'KG').
    cl_abap_unit_assert=>assert_equals( act = lt_char_values_curr[ 1 ]-charact
                                        exp = 'test').
    cl_abap_unit_assert=>assert_equals( act = lt_char_values_curr[ 1 ]-value_from
                                        exp = 20 ).
    cl_abap_unit_assert=>assert_equals( act = lt_char_values_curr[ 1 ]-value_to
                                        exp = 30 ).
  ENDMETHOD.

  METHOD convert_from_bapi_values.


    DATA: lt_char_values_num  TYPE tt_bapi1003_alloc_values_num,
          lt_char_values_char TYPE tt_bapi1003_alloc_values_char,
          lt_char_values_curr TYPE tt_bapi1003_alloc_values_curr,
          lt_characteristics  TYPE /vpcoe/if_pckf_matclass_dac=>gtyt_characteristic_value.

    "given
    lt_char_values_num = VALUE #(
      ( charact = 'N1' value_from = '10.5' unit_from = 'KG' )
      ( charact = 'N2' value_from = '5' value_to = '10' )
    ).

    lt_char_values_char = VALUE #(
      ( charact = 'C1' value_char = 'Yes' value_neutral = 'Y' )
      ( charact = 'C2' value_char = 'Lorem ipsum' )
    ).

    lt_char_values_curr = VALUE #(
      ( charact = 'W1' value_from = '8.5' )
      ( charact = 'W2' value_from = '8.5' value_to = '10' )
    ).

    "then
    f_cut->convert_from_bapi_values(
      EXPORTING
        it_char_values_num  = lt_char_values_num
        it_char_values_char = lt_char_values_char
        it_char_values_curr = lt_char_values_curr
      IMPORTING
        et_characteristics  = lt_characteristics
    ).

    "then
    DATA lt_exp_characteristics LIKE lt_characteristics.
    lt_exp_characteristics = VALUE #(
      ( characteristic = 'N1' value = '        1.0500000000000000E+01' value_num = '10.5' unit_num = 'KG' data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-num )
      ( characteristic = 'N2' value = '        5.0000000000000000E+00' value_num = '5' value_to = '10' data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-num )
      ( characteristic = 'C1' value = 'Yes' value_char = 'Yes' value_char_neutral = 'Y' data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-char )
      ( characteristic = 'C2' value = 'Lorem ipsum' value_char = 'Lorem ipsum' data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-char )
      ( characteristic = 'W1' value = '        8.5000000000000000E+00' value_curr = '8.5' data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-curr )
      ( characteristic = 'W2' value = '        8.5000000000000000E+00' value_curr = '8.5' value_to = '10' data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-curr )
    ).

    cl_abap_unit_assert=>assert_equals( act = lt_characteristics
                                        exp = lt_exp_characteristics ).

  ENDMETHOD.

ENDCLASS.
