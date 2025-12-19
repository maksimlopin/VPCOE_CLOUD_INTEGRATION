CLASS /vpcoe/cl_rdp_aunit_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: gc_json TYPE string VALUE 'json'.

    CLASS-METHODS: get_json_differences
      IMPORTING
        it_json_act             TYPE /vpcoe/cl_rdp_http=>gty_t_json
        it_json_exp             TYPE /vpcoe/cl_rdp_http=>gty_t_json
      RETURNING
        VALUE(rv_json_compared) TYPE string.

    CLASS-METHODS: compare_json
      IMPORTING
        iv_json_act             TYPE string
        iv_json_exp             TYPE string
      RETURNING
        VALUE(rv_json_compared) TYPE string.

    CLASS-METHODS: get_error_message
      IMPORTING
        iv_test_type           TYPE string
        iv_msg                 TYPE string
      RETURNING
        VALUE(rv_full_message) TYPE string.

    METHODS: form_json " not used
      IMPORTING
        it_text     TYPE table
        it_exp      TYPE table
      EXPORTING
        et_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.


    METHODS: get_json_fieldname " not used
      IMPORTING
        iv_db_name               TYPE string
      RETURNING
        VALUE(rv_json_fieldname) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /VPCOE/CL_RDP_AUNIT_HELPER IMPLEMENTATION.


  METHOD compare_json.

* 1. Parse JSON into the itab
    DATA(lo_json_parser) = NEW /ui5/cl_json_parser( ).
    DATA: lv_subitem_type TYPE string.

    lo_json_parser->parse( iv_json_exp ).
    DATA(lt_json_exp) = lo_json_parser->m_entries.

    lo_json_parser->parse( iv_json_act ).
    DATA(lt_json_act) = lo_json_parser->m_entries.

    " check if subitem type is TEXTS or PERIOD
    READ TABLE lt_json_exp WITH KEY parent = |/elements/1/texts| TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_subitem_type = 'texts'.
    ELSE.
      READ TABLE lt_json_exp WITH KEY parent = |/elements/1/period| TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_subitem_type = 'period'.
      ENDIF.
    ENDIF.

* 2. Parse JSON tables and check if number of ELEMENTS and subitems (TEXTS or PERIOD) records is equal
    DATA: lv_exp_elements_number TYPE int4.
    DATA: lv_act_elements_number TYPE int4.

* 2.1 Compare number of ELEMENTS in ACT and EXP tables
    LOOP AT lt_json_exp ASSIGNING FIELD-SYMBOL(<fs_json_exp_for_elements>)
        WHERE parent = |/elements|
        AND type = 2 AND subtype = 0.
      lv_exp_elements_number = <fs_json_exp_for_elements>-name.
    ENDLOOP.

    LOOP AT lt_json_act ASSIGNING FIELD-SYMBOL(<fs_json_act_for_elements>)
        WHERE parent = |/elements|
        AND type = 2 AND subtype = 0.
      lv_act_elements_number = <fs_json_act_for_elements>-name.
    ENDLOOP.

    IF lv_exp_elements_number NE lv_act_elements_number.
      rv_json_compared = |{ rv_json_compared }Different number of [elements] entries expected:|
                        && | act[{ lv_act_elements_number }], exp[{ lv_exp_elements_number }];|
                        && |{ cl_abap_char_utilities=>newline }|.
      RETURN.
    ENDIF.

* 2.2 Compare number of subitems in ACT and EXP tables

    TYPES: BEGIN OF ty_subitems_number,
             element_id      TYPE int4,
             subitems_number TYPE int4,
           END OF ty_subitems_number.
    DATA: lt_exp_subitems_number TYPE TABLE OF ty_subitems_number.
    DATA: lt_act_subitems_number TYPE TABLE OF ty_subitems_number.

    DATA: lv_exp_subitems_number TYPE int4.
    DATA: lv_act_subitems_number TYPE int4.
    DATA: lv_elements_counter TYPE int4.

    lv_elements_counter = 0.
    WHILE lv_elements_counter <= lv_exp_elements_number.

      lv_elements_counter = lv_elements_counter + 1.
      CLEAR: lv_exp_subitems_number, lv_act_subitems_number.

      " Calculate EXP number of subitems in current ELEMENT
      lv_exp_subitems_number = 0.
      LOOP AT lt_json_exp ASSIGNING FIELD-SYMBOL(<fs_json_exp_for_subitems>)
          WHERE parent = |/elements/{ lv_elements_counter }/{ lv_subitem_type }|.
        lv_exp_subitems_number = lv_exp_subitems_number + 1.
      ENDLOOP.
      APPEND VALUE ty_subitems_number( element_id = lv_elements_counter subitems_number = lv_exp_subitems_number ) TO lt_exp_subitems_number.
      " Calculate ACT number of TEXTS in current ELEMENT
      LOOP AT lt_json_act ASSIGNING FIELD-SYMBOL(<fs_json_act_for_subitems>)
          WHERE parent = |/elements/{ lv_elements_counter }/{ lv_subitem_type }|.
        lv_act_subitems_number = lv_act_subitems_number + 1.
      ENDLOOP.
      APPEND VALUE ty_subitems_number( element_id = lv_elements_counter subitems_number = lv_act_subitems_number ) TO lt_act_subitems_number.

      " Check if numbers of act and exp TEXTS are equal
      IF lv_exp_subitems_number NE lv_act_subitems_number.
        rv_json_compared = |{ rv_json_compared }Different number of [{ lv_subitem_type }] entries |
                          && |at position [/elements/{ lv_elements_counter }] expected: |
                          && |act[{ lv_act_subitems_number }], exp[{ lv_exp_subitems_number }];|
                          && |{ cl_abap_char_utilities=>newline }|.
      ENDIF.

    ENDWHILE.

    IF rv_json_compared IS NOT INITIAL.
      RETURN.
    ENDIF.

* 3. Check if number of act and exp fields inside ELEMENTS and TEXTS(or PERIOD) are equal
* 3.1 Check for ELEMENTS
    DATA: lv_elements_fields_counter TYPE int4.
    DATA: lv_subitems_fields_counter TYPE int4.
    DATA: lv_exp_elements_fields_number TYPE int4.
    DATA: lv_act_elements_fields_number TYPE int4.
    DATA: lv_exp_subitems_fields_number TYPE int4.
    DATA: lv_act_subitems_fields_number TYPE int4.

    lv_elements_fields_counter = 0.
    WHILE lv_elements_fields_counter <= lv_exp_elements_number.

      lv_elements_fields_counter = lv_elements_fields_counter + 1.
      CLEAR: lv_exp_elements_fields_number, lv_act_elements_fields_number.

      " Calculate EXP number of fields in current ELEMENT
      LOOP AT lt_json_exp ASSIGNING FIELD-SYMBOL(<fs_json_exp_for_elmnts_flds>)
          WHERE parent = |/elements/{ lv_elements_fields_counter }|.
        lv_exp_elements_fields_number = lv_exp_elements_fields_number + 1.
      ENDLOOP.
      " Calculate ACT number of fields in current ELEMENT
      LOOP AT lt_json_act ASSIGNING FIELD-SYMBOL(<fs_json_act_for_elmnts_flds>)
          WHERE parent = |/elements/{ lv_elements_fields_counter }|.
        lv_act_elements_fields_number = lv_act_elements_fields_number + 1.
      ENDLOOP.

      " Check if numbers of fields in current element are equal
      IF lv_exp_elements_fields_number NE lv_act_elements_fields_number.
        rv_json_compared = |{ rv_json_compared }Different number of fields in [elements] entity |
                          && |at position [/elements/{ lv_elements_fields_counter }] expected:|
                          && | act[{ lv_act_elements_fields_number }], exp[{ lv_exp_elements_fields_number }];|
                          && |{ cl_abap_char_utilities=>newline }|.
      ENDIF.

      " 3.2 Check for subitems
      WHILE lv_subitems_fields_counter <= lt_exp_subitems_number[ element_id = lv_elements_fields_counter ]-subitems_number.
        " Calculate EXP number of subitems in current ELEMENT
        lv_subitems_fields_counter = lv_subitems_fields_counter + 1.
        CLEAR: lv_exp_subitems_fields_number, lv_act_subitems_fields_number.

        LOOP AT lt_json_exp ASSIGNING FIELD-SYMBOL(<fs_json_exp_for_subitms_flds>)
            WHERE parent = |/elements/{ lv_elements_fields_counter }/{ lv_subitem_type }/{ lv_subitems_fields_counter }|.
          lv_exp_subitems_fields_number = lv_exp_subitems_fields_number + 1.
        ENDLOOP.
        LOOP AT lt_json_act ASSIGNING FIELD-SYMBOL(<fs_json_act_for_subitms_flds>)
            WHERE parent = |/elements/{ lv_elements_fields_counter }/{ lv_subitem_type }/{ lv_subitems_fields_counter }|.
          lv_act_subitems_fields_number = lv_act_subitems_fields_number + 1.
        ENDLOOP.

        " Check if numbers of fields in current subitem are equal
        IF lv_exp_subitems_fields_number NE lv_act_subitems_fields_number.
          rv_json_compared = |{ rv_json_compared }Different number of fields in [{ lv_subitem_type }] entity |
                            && |at position [/elements/{ lv_elements_fields_counter }/{ lv_subitem_type }/{ lv_subitems_fields_counter }] expected: |
                            && |act[{ lv_act_subitems_fields_number }], exp[{ lv_exp_subitems_fields_number }];|
                            && |{ cl_abap_char_utilities=>newline }|.
        ENDIF.

      ENDWHILE.

    ENDWHILE.

    IF rv_json_compared IS NOT INITIAL.
      RETURN.
    ENDIF.

* 4. If number of ELEMENTS and subitems records are equal, compare JSONs by index
    LOOP AT lt_json_exp ASSIGNING FIELD-SYMBOL(<fs_json_exp>). " loop at expected table

      READ TABLE lt_json_act INDEX sy-tabix
        INTO DATA(ls_current_act).

      IF sy-subrc = 0 AND ( ls_current_act-name = <fs_json_exp>-name AND ls_current_act-parent = <fs_json_exp>-parent ).
        " ok, field exists in actual table
        " check if values are equal
        IF ls_current_act-value = <fs_json_exp>-value.
          " ok, values are equal
        ELSE. "error, values are different
          rv_json_compared = |{ rv_json_compared }Value of the field [{ <fs_json_exp>-name }] |
                              && |does not match at position [{ <fs_json_exp>-parent }]: |
                              && |act[{ ls_current_act-value }], exp:[{ <fs_json_exp>-value }];|
                              && |{ cl_abap_char_utilities=>newline }|.
        ENDIF.

      ELSEIF sy-subrc = 0 AND ( ls_current_act-name NE <fs_json_exp>-name AND ls_current_act-parent = <fs_json_exp>-parent ).
        " error, field name is incorrect on this position
        rv_json_compared = |{ rv_json_compared }Field name does not match at position [{ <fs_json_exp>-parent }]: |
                              && |act:[{ ls_current_act-name }], exp:[{ <fs_json_exp>-name }];|
                              && |{ cl_abap_char_utilities=>newline }|.

      ELSE.
        " error, field does not exist in actual table
        rv_json_compared = |{ rv_json_compared }Field [{ <fs_json_exp>-name }] with value [{ <fs_json_exp>-value }] |
                              && |at position [{ <fs_json_exp>-parent }] |
                              && |missing in actual table;|
                              && |{ cl_abap_char_utilities=>newline }|.
      ENDIF.

    ENDLOOP.

* 5. if no error message was identified while parsing, check lines by symbol
    IF rv_json_compared IS INITIAL.

      DATA: lv_index TYPE int4.
      DATA: lv_error_index TYPE int4.
      DATA: lv_strlen TYPE int4.
      lv_index = 0.

      lv_strlen = strlen( iv_json_exp ).
      WHILE lv_index <= lv_strlen.
        IF iv_json_exp+lv_index(1) NE iv_json_act+lv_index(1).
          lv_error_index = lv_index.
          EXIT.
        ENDIF.
        lv_index = lv_index + 1.
      ENDWHILE.

      IF lv_error_index NE 0.
        rv_json_compared = |JSON mismatch at character №[{ lv_error_index }] out of [{ lv_strlen }] expected characters.|
                              && | act[{ iv_json_act+lv_index(1) }], exp[{ iv_json_exp+lv_index(1) }];|.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD form_json.
*
*    DATA: lv_json_string TYPE string.
*    DATA: lv_json_string_country_exp TYPE string.
*    DATA: lv_json_string_texts TYPE string.
*    DATA: lv_json_fieldname TYPE string.
*    DATA: lv_index TYPE i.
*    DATA: lv_value_is_number TYPE bool.
*    DATA: lv_number_value TYPE i.
*    DATA: lv_string_value TYPE string.
*    DATA: lv_field_type TYPE abap_typekind.
*    DATA: lv_type_name TYPE string.
*
*    " parse lt_country_exp
*    LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( it_exp[ 1 ] ) )->get_components( )
*               ASSIGNING FIELD-SYMBOL(<fs_country_exp>).
*
*      ASSIGN COMPONENT <fs_country_exp>-name OF STRUCTURE it_exp[ 1 ] TO FIELD-SYMBOL(<lv_value_exp>).
*
*      " check if value is char or number
*      lv_field_type = <fs_country_exp>-type->get_data_type_kind( <lv_value_exp> ).
*      lv_type_name = <fs_country_exp>-type->get_relative_name(  ).
*
***********************************************************************
*      IF ( lv_field_type EQ <fs_country_exp>-type->typekind_char ). " IF CHARACTER
*
*        IF lv_type_name EQ 'BOOLEAN'.  " IF BOOLEAN
*          IF <lv_value_exp> = 'X'.
*            lv_string_value = |"true"|.
*          ELSE.
*            lv_string_value = |"false"|.
*          ENDIF.
*        ELSE. " if string
*          IF <lv_value_exp> IS INITIAL.
*            lv_string_value = | null|.
*          ELSE.
*            lv_string_value = |"{ <lv_value_exp> }"|.
*          ENDIF.
*        ENDIF.
*
*        " OUTPUT for CHARACTER
*        IF lv_json_string_country_exp IS INITIAL. " first field
*          lv_json_string_country_exp = |{ lv_json_string_country_exp }"{ get_json_fieldname( <fs_country_exp>-name ) }":{ lv_string_value }|.
*        ELSEIF lv_json_string_country_exp IS NOT INITIAL. " another field
*          lv_json_string_country_exp = |{ lv_json_string_country_exp },"{ get_json_fieldname( <fs_country_exp>-name ) }":{ lv_string_value }|.
*        ENDIF.
*
*      ELSEIF ( lv_field_type EQ <fs_country_exp>-type->typekind_table ). " IF TABLE
*
*        " OUTPUT for TABLE values ('TEXTS' table field)
*        IF <lv_value_exp> IS NOT INITIAL. " another field
*          lv_json_string_country_exp = |{ lv_json_string_country_exp },"{ get_json_fieldname( <fs_country_exp>-name ) }":[|.
*        ELSEIF <lv_value_exp> IS INITIAL. " first field
*          " do nothing
*        ENDIF.
*
*      ELSE. " IF NUMBER (excluding char, boolean and table)
*
*        IF <lv_value_exp> IS INITIAL.
*          lv_number_value = 0.
*        ELSE.
*          lv_number_value = <lv_value_exp>.
*        ENDIF.
*
*        " OUTPUT for NUMERIC values
*        IF lv_json_string_country_exp IS INITIAL. " first field
*          lv_json_string_country_exp = |{ lv_json_string_country_exp }"{ get_json_fieldname( <fs_country_exp>-name ) }":{ lv_number_value }|.
*        ELSEIF lv_json_string_country_exp IS NOT INITIAL. " another field
*          lv_json_string_country_exp = |{ lv_json_string_country_exp },"{ get_json_fieldname( <fs_country_exp>-name ) }":{ lv_number_value }|.
*
*        ENDIF.
*
*      ENDIF.
*
***********************************************************************
*
**      IF <fs_country_exp>-name NE 'TEXTS' " NOT Text (number or tab) " boolean not handled!
**        AND ( lv_field_type EQ <fs_country_exp>-type->typekind_char
**              OR lv_field_type EQ <fs_country_exp>-type->typekind_table ).
**        lv_value_is_number = abap_false.
**
**        " is boolean
**        IF lv_type_name EQ 'BOOLEAN'.
**          IF <lv_value_exp> = 'X'.
**            lv_string_value = 'true'.
**          ELSE.
**            lv_string_value = 'false'.
**          ENDIF.
**        ELSE.
**          lv_string_value = <lv_value_exp>.
**        ENDIF.
**
**      ELSE. " is not text
**        lv_value_is_number = abap_true.
**        IF <lv_value_exp> IS INITIAL.
**          lv_number_value = 0.
**        ELSE.
**          lv_number_value = <lv_value_exp>.
**        ENDIF.
**      ENDIF.
**
**      " for text values
**      IF <fs_country_exp>-name NE 'TEXTS' AND lv_json_string_country_exp IS INITIAL AND lv_value_is_number = abap_false. " first field
**        lv_json_string_country_exp = |{ lv_json_string_country_exp }"{ get_json_fieldname( <fs_country_exp>-name ) }":"{ lv_string_value }"|.
**      ELSEIF <fs_country_exp>-name NE 'TEXTS' AND lv_json_string_country_exp IS NOT INITIAL AND lv_value_is_number = abap_false. " first field
**        lv_json_string_country_exp = |{ lv_json_string_country_exp },"{ get_json_fieldname( <fs_country_exp>-name ) }":"{ lv_string_value }"|.
**
**        " for numeric values
**      ELSEIF <fs_country_exp>-name NE 'TEXTS' AND lv_json_string_country_exp IS INITIAL AND lv_value_is_number = abap_true. " first field
**        lv_json_string_country_exp = |{ lv_json_string_country_exp }"{ get_json_fieldname( <fs_country_exp>-name ) }":{ lv_number_value }|.
**      ELSEIF <fs_country_exp>-name NE 'TEXTS' AND lv_json_string_country_exp IS NOT INITIAL AND lv_value_is_number = abap_true. " first field
**        lv_json_string_country_exp = |{ lv_json_string_country_exp },"{ get_json_fieldname( <fs_country_exp>-name ) }":{ lv_number_value }|.
**
**        " for 'TEXTS' table field
**      ELSEIF <fs_country_exp>-name EQ 'TEXTS' AND <lv_value_exp> IS NOT INITIAL.
**        lv_json_string_country_exp = |{ lv_json_string_country_exp },"{ get_json_fieldname( <fs_country_exp>-name ) }":[|.
**      ELSEIF <fs_country_exp>-name EQ 'TEXTS' AND <lv_value_exp> IS INITIAL.
**        " do nothing
**        ENDIF.
*
*    ENDLOOP.
*
*    " parse lt_text
*    LOOP AT it_text ASSIGNING FIELD-SYMBOL(<fs_text_str>).
*
*      CLEAR: lv_index.
*
*      IF lv_json_string_texts IS INITIAL.
*        lv_json_string_texts = lv_json_string_texts && '{'. " start new record
*      ELSEIF lv_json_string_texts IS NOT INITIAL.
*        lv_json_string_texts = lv_json_string_texts && ',{'. " start new record
*      ENDIF.
*
*      LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <fs_text_str> ) )->get_components( )
*                 ASSIGNING FIELD-SYMBOL(<fs_text>).
*
*        ASSIGN COMPONENT <fs_text>-name OF STRUCTURE <fs_text_str> TO FIELD-SYMBOL(<lv_value_text>).
*
**        CONDENSE lv_json_string_texts NO-GAPS.
*        lv_index = strlen( lv_json_string_texts ).
*        lv_index = lv_index - 1.
*        DATA(lv_last_char) = substring( val = lv_json_string_texts len = 1 off = lv_index ).
*
*        IF lv_last_char EQ '{'. " first field
*          lv_json_string_texts = |{ lv_json_string_texts }"{ get_json_fieldname( <fs_text>-name ) }":"{ <lv_value_text> }"|.
*
*        ELSEIF lv_last_char NE '{'. " other fields
*          lv_json_string_texts = |{ lv_json_string_texts },"{ get_json_fieldname( <fs_text>-name ) }":"{ <lv_value_text> }"|.
*        ENDIF.
*
*      ENDLOOP.
*
*      lv_json_string_texts = lv_json_string_texts && '}'. " end current record
*
*    ENDLOOP.
*
*    CONCATENATE
*        '{"source":"ECC","elements":[{'
*        lv_json_string_country_exp
*        lv_json_string_texts
*        ']}]}'
*    INTO lv_json_string.
*
*    INSERT VALUE #( elements = lv_json_string
*                     count = 1 ) INTO TABLE et_json_exp.
*
  ENDMETHOD.


  METHOD get_error_message.

    CASE iv_test_type.
      WHEN gc_json.
        rv_full_message = |JSON creation assertion failed. Hover over this message to see error description:{ cl_abap_char_utilities=>newline }{ iv_msg }|.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD get_json_differences.

    DATA(lo_json_parser) = NEW /ui5/cl_json_parser( ).

    DATA(lv_json_exp_size) = lines( it_json_exp ).
    DATA(lv_json_act_size) = lines( it_json_act ).

* 1. Check if number of JSON lines in act and exp tables is same
    IF lv_json_exp_size NE lv_json_act_size.
      rv_json_compared = |{ rv_json_compared }Different number of JSON records expected:|
                          && | act[{ lv_json_act_size }], exp[{ lv_json_exp_size }];|
                          && |{ cl_abap_char_utilities=>newline }|.
      RETURN.
    ENDIF.

* 2. Loop at act and exp JSONs and find differences
    LOOP AT it_json_exp ASSIGNING FIELD-SYMBOL(<fs_json_exp>).

      IF it_json_act[ sy-tabix ]-elements NE <fs_json_exp>-elements.
        rv_json_compared = rv_json_compared
                           && |JSON №{ sy-tabix }:{ cl_abap_char_utilities=>newline }|
                           && compare_json(
                                EXPORTING
                                  iv_json_act      = it_json_act[ sy-tabix ]-elements
                                  iv_json_exp      = <fs_json_exp>-elements
                              ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_json_fieldname.

*    CASE iv_db_name.
*
*        " for EXP values
*      WHEN 'ISO_CODE'.
*        rv_json_fieldname = 'isoCode'.
*      WHEN 'THREE_DIGIT_ISO_CODE'.
*        rv_json_fieldname = 'threeDigitIsoCode'.
*      WHEN 'THREE_LETTER_ISO_CODE'.
*        rv_json_fieldname = 'threeLetterIsoCode'.
*      WHEN 'ISEUROPEANUNIONMEMBER'.
*        rv_json_fieldname = 'isEuropeanUnionMember'.
*
*      WHEN 'SI_UNIT_CNVRSN_RATE_NUMR'.
*        rv_json_fieldname = 'siUnitCnvrsnRateNumerator'.
*      WHEN 'SI_UNIT_CNVRSN_RATE_DENOMR'.
*        rv_json_fieldname = 'siUnitCnvrsnRateDenominator'.
*      WHEN 'SI_UNIT_CNVRSN_RATE_EXPONENT'.
*        rv_json_fieldname = 'siUnitCnvrsnRateExponent'.
*      WHEN 'SI_UNIT_CNVRSN_ADDITIVE_VALUE'.
*        rv_json_fieldname = 'siUnitCnvrsnAdditiveValue'.
*
*      WHEN 'SI_UNIT'.
*        rv_json_fieldname = 'siUnit'.
*
*        " for TEXTs
*      WHEN 'TECHNICAL_NAME'.
*        rv_json_fieldname = 'technicalName'.
*      WHEN 'COMMERCIAL_NAME'.
*        rv_json_fieldname = 'commercialName'.
*      WHEN 'LONG_NAME'.
*        rv_json_fieldname = 'longName'.
*
*      WHEN OTHERS.
*        rv_json_fieldname = iv_db_name.
*        TRANSLATE rv_json_fieldname TO LOWER CASE.
*    ENDCASE.

  ENDMETHOD.
ENDCLASS.
