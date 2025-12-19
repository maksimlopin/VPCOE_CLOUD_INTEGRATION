CLASS tcl_evaluate_response DEFINITION DEFERRED.

CLASS th_transfer_mapper_with_mock DEFINITION FOR TESTING INHERITING FROM /vpcoe/cl_uph_transfer_mapper
  DURATION SHORT
  RISK LEVEL HARMLESS
  FRIENDS tcl_evaluate_response.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_element_id_by_index REDEFINITION.

  PRIVATE SECTION.
    DATA mv_element_id_to_be_returned TYPE char40.

ENDCLASS.



CLASS th_transfer_mapper_with_mock IMPLEMENTATION.
  METHOD get_element_id_by_index.
    rv_result = mv_element_id_to_be_returned.
  ENDMETHOD.
ENDCLASS.


CLASS tcl_evaluate_response DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA f_cut TYPE REF TO th_transfer_mapper_with_mock.

    METHODS setup.
    METHODS empty_response_no_messages      FOR TESTING.
    METHODS simple_response_no_messages  FOR TESTING.
    METHODS error_forbidden  FOR TESTING.
    METHODS validation_error FOR TESTING.
ENDCLASS.



CLASS tcl_evaluate_response IMPLEMENTATION.
  METHOD setup.
    f_cut =  NEW th_transfer_mapper_with_mock( ).
  ENDMETHOD.


  METHOD empty_response_no_messages.
    DATA lv_response TYPE string.
    DATA lt_messages TYPE /vpcoe/t_uph_msg.

    f_cut->/vpcoe/if_uph_transfer_mapper~evaluate_response( EXPORTING iv_response = lv_response
                                                           IMPORTING et_messages = lt_messages ).

    cl_abap_unit_assert=>assert_initial( act = lt_messages ).
  ENDMETHOD.


  METHOD simple_response_no_messages.
    DATA lv_response TYPE string.
    DATA lt_messages TYPE /vpcoe/t_uph_msg.

    lv_response = 'Response'.
    f_cut->/vpcoe/if_uph_transfer_mapper~evaluate_response( EXPORTING iv_response = lv_response
                                                           IMPORTING et_messages = lt_messages ).

    cl_abap_unit_assert=>assert_initial( act = lt_messages ).
  ENDMETHOD.


  METHOD error_forbidden.
    DATA lv_response TYPE string.

    lv_response =   | \{ | &&
                    | "error":  | &&
                    | \{ | &&
                    | "code": 403,| &&
                    | "message": "FORBIDEN",| &&
                    | "details":[ | &&
                    | \{ | &&
                    | "code": "constraint_validation",| &&
                    | "message": "numeric value out of bounds (<16 digits>.<6 digits> expected)",| &&
                    | "target": "elements[0].baseQuantity"| &&
                    | \} | &&
                    | ] | &&
                    | \} | &&
                    | \} |.

    f_cut->/vpcoe/if_uph_transfer_mapper~evaluate_response( EXPORTING iv_response = lv_response
                                                           IMPORTING et_messages = DATA(lt_messages) ).

    cl_abap_unit_assert=>assert_not_initial( act = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages ) exp = 1 ).
    DATA(lv_message) = lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgty exp = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgid exp = '/VPCOE/PLM' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgno exp = '024' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgv1 exp = '403 FORBIDEN' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lv_message-details ) exp = 1 ).

    DATA(lv_message_detail) = lv_message-details[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgty exp = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgid exp = '/VPCOE/PLM' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgno exp = '025' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgv1 exp = 'constraint_validation numeric value out of bounds ' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgv2 exp = ' (<16 digits>.<6 digits> expected) elements[0].bas' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgv3 exp = 'eQuantity                                         ' ).
  ENDMETHOD.


  METHOD validation_error.
    DATA lv_response TYPE string.

    lv_response =   | \{ | &&
                    | "error":  | &&
                    | \{ | &&
                    | "code": 400,| &&
                    | "message": "The provided data is invalid: Log ID: f5d6175e-6317-469e-7278-785f10df3c7a, Time stamp: 2022-12-05T07:52:04Z.",| &&
                    | "details":[ | &&
                    | \{ | &&
                    | "code": "constraint_validation",| &&
                    | "message": "must not be empty",| &&
                    | "target": "elements[3].fractions[0].basicMaterialFraction"| &&
                    | \} | &&
                    | ] | &&
                    | \} | &&
                    | \} |.

    f_cut->mv_element_id_to_be_returned = 'SOME_ID'.

    f_cut->/vpcoe/if_uph_transfer_mapper~evaluate_response( EXPORTING iv_response = lv_response
                                                           IMPORTING et_messages = DATA(lt_messages) ).

    cl_abap_unit_assert=>assert_not_initial( act = lt_messages ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_messages ) exp = 1 ).
    DATA(lv_message) = lt_messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgty exp = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgid exp = '/VPCOE/PLM' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgno exp = '024' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgv1 exp = '400 The provided data is invalid: Log ID: f5d6175e' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgv2 exp = '-6317-469e-7278-785f10df3c7a, Time stamp: 2022-12-' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message-msgv3 exp = '05T07:52:04Z.                                     ' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lv_message-details ) exp = 1 ).

    DATA(lv_message_detail) = lv_message-details[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgty exp = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgid exp = '/VPCOE/PLM' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgno exp = '025' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgv1 exp = 'constraint_validation must not be empty SOME_ID el' ).
    cl_abap_unit_assert=>assert_equals( act = lv_message_detail-msgv2 exp = 'ements[3].fractions[0].basicMaterialFraction      ' ).
  ENDMETHOD.
ENDCLASS.
