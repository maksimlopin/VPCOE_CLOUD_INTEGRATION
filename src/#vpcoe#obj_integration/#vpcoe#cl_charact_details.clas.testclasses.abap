CLASS th_charac_details_mock_bapi DEFINITION FOR TESTING
  INHERITING FROM /vpcoe/cl_charact_details.

  PUBLIC SECTION.                                  "#EC NUM_PUBLIC_ATTR
    DATA mv_expected_atnam       TYPE atnam.
    DATA mv_returned_uom         TYPE meins.
    DATA mb_characteristic_found TYPE abap_bool.

    METHODS get_details_of_characteristic REDEFINITION.
ENDCLASS.

CLASS th_charac_details_mock_bapi IMPLEMENTATION.
  METHOD get_details_of_characteristic.

    cl_abap_unit_assert=>assert_equals( msg = 'Expected call with different characteristic name' exp = mv_expected_atnam act = iv_characteristic_name ).

    IF mb_characteristic_found = abap_true.
      rv_result-unit_of_measurement = mv_returned_uom.
    ENDIF.

  ENDMETHOD.
ENDCLASS.


CLASS ltcl_get_uom_of_characteristic DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS setup.
    METHODS given_characteristic_with_uom IMPORTING iv_expected_atnam       TYPE atnam
                                                    iv_returned_uom         TYPE meins OPTIONAL
                                                    ib_characteristic_found TYPE abap_bool DEFAULT abap_true.

    METHODS uom_existing                FOR TESTING RAISING cx_static_check.
    METHODS characteristic_not_existing FOR TESTING RAISING cx_static_check.

    DATA mo_cut TYPE REF TO th_charac_details_mock_bapi.
ENDCLASS.



CLASS ltcl_get_uom_of_characteristic IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW th_charac_details_mock_bapi( ).
  ENDMETHOD.

  METHOD uom_existing.

    given_characteristic_with_uom( iv_expected_atnam = 'MY_CHARACTERISTIC'
                                   iv_returned_uom = 'KG' ).

    DATA(lv_actual_uom) = mo_cut->get_uom_of_characteristic( 'MY_CHARACTERISTIC' ).

    cl_abap_unit_assert=>assert_equals( exp = 'KG' act = lv_actual_uom ).

  ENDMETHOD.


  METHOD characteristic_not_existing.

    given_characteristic_with_uom( iv_expected_atnam = 'NO_CHARACTERISTIC'
                                   ib_characteristic_found = abap_false ).

    DATA(lv_actual_uom) = mo_cut->get_uom_of_characteristic( 'NO_CHARACTERISTIC' ).

    cl_abap_unit_assert=>assert_initial( act = lv_actual_uom ).

  ENDMETHOD.


  METHOD given_characteristic_with_uom.

    mo_cut->mv_expected_atnam = iv_expected_atnam.
    mo_cut->mv_returned_uom   = iv_returned_uom.
    mo_cut->mb_characteristic_found   = ib_characteristic_found.

  ENDMETHOD.

ENDCLASS.
