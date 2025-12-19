*"* use this source file for your ABAP unit test classes

CLASS /vpcoe/tcl_uph_tm_pckg_elem_v1 DEFINITION FOR TESTING ##CLASS_FINAL
  DURATION SHORT
  RISK LEVEL HARMLESS
.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_uph_tm_pckg_elem_v1.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: evaluate_response_test FOR TESTING.
    METHODS: get_entity_url_suffix_test FOR TESTING.
    METHODS: prepare_payload_test FOR TESTING.
ENDCLASS.       "tcl_Surdp_Uph_Tm_Pckg_Elem_V1


CLASS /VPCOE/tcl_uph_tm_pckg_elem_v1 IMPLEMENTATION.

  METHOD class_setup.

  ENDMETHOD.


  METHOD class_teardown.

  ENDMETHOD.


  METHOD setup.
    f_cut =  NEW /vpcoe/cl_uph_tm_pckg_elem_v1( ) .
  ENDMETHOD.


  METHOD teardown.

  ENDMETHOD.


  METHOD evaluate_response_test.

    DATA lv_response TYPE string.
    DATA lt_messages TYPE /vpcoe/t_uph_msg.


    f_cut->/VPCOE/if_uph_transfer_mapper~evaluate_response(
      EXPORTING
        iv_response = lv_response
     IMPORTING
       et_messages = lt_messages
    ).

    lv_response = 'Response'.

    f_cut->/vpcoe/if_uph_transfer_mapper~evaluate_response(
         EXPORTING
           iv_response = lv_response
        IMPORTING
          et_messages = lt_messages
       ).

    lv_response =  | \{ | &&
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

| \} | .

    f_cut->/vpcoe/if_uph_transfer_mapper~evaluate_response(
         EXPORTING
           iv_response = lv_response
        IMPORTING
          et_messages = lt_messages
       ).
    CLEAR lt_messages.
  ENDMETHOD.


  METHOD get_entity_url_suffix_test.

    DATA lv_entity_url_suffix TYPE string.

    lv_entity_url_suffix = f_cut->/vpcoe/if_uph_transfer_mapper~get_entity_url_suffix(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_entity_url_suffix
      exp   = '/PackagingElements'
    ).
  ENDMETHOD.


  METHOD prepare_payload_test.

    DATA lt_entity_data TYPE /vpcoe/t_uph_entity_data.
    DATA ls_parameters TYPE /vpcoe/s_pckg_elem_input.
    DATA: lv_payload                TYPE string,
          ls_packelem_data          TYPE /vpcoe/s_uph_ent_pack_elem,
          ls_packelem_frac_data     TYPE /vpcoe/s_uph_ent_pack_frac,
          ls_packelem_prod_data     TYPE /vpcoe/s_uph_ent_pack_prod,
          lt_pack_fract_entity_data TYPE /vpcoe/t_uph_entity_data,
          lt_pack_prod_entity_data  TYPE /vpcoe/t_uph_entity_data.


    lv_payload = f_cut->/VPCOE/if_uph_transfer_mapper~prepare_payload(
                                                               it_entity_data = lt_entity_data
                                                               is_parameters = ls_parameters       ).

    "Given
    ls_parameters-spec_radio_btn = abap_true.
    ls_parameters-subid = VALUE #( ( option = 'EQ' low = 'SPEC01' high = '' sign = 'I' ) ).
    ls_parameters-subcat = VALUE #( ( option = 'EQ' low = 'PURE_SUB' high = '' sign = 'I' ) ).
    ls_parameters-valfromdate = sy-datum.
    ls_parameters-sub_chgdon = VALUE #( ( option = 'GT' low = '20210101' high = '' sign = 'I' ) ).

    " it_entity_data

    ls_packelem_data-displayid = 'S4HANA'.
    ls_packelem_data-source = 'S4HANA'.
    ls_packelem_data-validfrom = '10.10.2009'.
    ls_packelem_data-description = 'Britannia biscuit' .
    ls_packelem_data-unitofmeasure = 'EA'.
    ls_packelem_data-packagingtype = 'EA'.
    ls_packelem_data-iscompound = 'X'.
    ls_packelem_data-usage_type = 'single'.
    ls_packelem_data-flexibility = ''.
    ls_packelem_data-isreusable = 'X"'.
    ls_packelem_data-reusecount = '10'.
    ls_packelem_data-renewablepercent = 20.
    ls_packelem_data-recyclablepercent = 40.
    ls_packelem_data-dimensionheight = 10.
    ls_packelem_data-dimensionunitofmeasure = 10.
    ls_packelem_data-volume = 100.
    ls_packelem_data-volumeunitofmeasure = 'EA'.

    "Packaging Element to Products [0..n]

    ls_packelem_prod_data-productid = 'Marigold'.
    DATA(lo_ent_packelem_prod) = NEW /vpcoe/cl_uph_ent_pckg_product( is_data = ls_packelem_prod_data iv_deleted = '' ).
    INSERT lo_ent_packelem_prod INTO TABLE lt_pack_prod_entity_data.


    "Packaging fractions [0..n]
    CLEAR lt_pack_fract_entity_data.

    ls_packelem_frac_data-basicmaterialfractionid = 362847.
    ls_packelem_frac_data-weight = 10.
    ls_packelem_frac_data-weightunitofmeasure = 'EA'.
    ls_packelem_frac_data-transparency = 'X'.
    ls_packelem_frac_data-color = 'RED'.
    ls_packelem_frac_data-recycledcontentpercent = 60.
    ls_packelem_frac_data-laminationtype = 'THick'.
    ls_packelem_frac_data-iscoextrudedlamination = 'abc'.
    ls_packelem_frac_data-thickness = 10.
    ls_packelem_frac_data-thicknessunitofmeasure = 'EA'.
    ls_packelem_frac_data-density = 20.
    ls_packelem_frac_data-densityunitofmeasure = 'EA'.
    ls_packelem_frac_data-isreinforced = 'YES'.

    DATA(lo_packelem_frac) = NEW /vpcoe/cl_uph_ent_pckg_frctn( is_data = ls_packelem_frac_data iv_deleted = '' ).
    INSERT lo_packelem_frac INTO TABLE lt_pack_fract_entity_data.

    DATA(lo_packelem_data) = NEW /vpcoe/cl_uph_ent_pckg_element( is_data = ls_packelem_data it_fractions = lt_pack_fract_entity_data it_products = lt_pack_prod_entity_data iv_deleted = '' ).
    INSERT lo_packelem_data INTO TABLE lt_entity_data.

    lv_payload = f_cut->/VPCOE/if_uph_transfer_mapper~prepare_payload(
                                                            it_entity_data = lt_entity_data
                                                            is_parameters = ls_parameters   ).



    " it_entity_data
    CLEAR : lt_pack_prod_entity_data,lt_entity_data,lt_pack_fract_entity_data.
    ls_packelem_data-displayid = 'S4HANA'.
    ls_packelem_data-source = 'S4HANA'.
    ls_packelem_data-validfrom = '10.10.2009'.
    ls_packelem_data-description = 'Britannia biscuit' .
    ls_packelem_data-unitofmeasure = 'EA'.
    ls_packelem_data-packagingtype = 'EA'.
    ls_packelem_data-iscompound = 'X'.
    ls_packelem_data-usage_type = 'single'.
    ls_packelem_data-flexibility = ''.
    ls_packelem_data-isreusable = 'X"'.
    ls_packelem_data-reusecount = '10'.
    ls_packelem_data-renewablepercent = 20.
    ls_packelem_data-recyclablepercent = 40.
    ls_packelem_data-dimensionheight = 10.
    ls_packelem_data-dimensionunitofmeasure = 10.
    ls_packelem_data-volume = 100.
    ls_packelem_data-volumeunitofmeasure = 'EA'.

    "Packaging Element to Products [0..n]

    lo_packelem_data = NEW /VPCOE/cl_uph_ent_pckg_element( is_data = ls_packelem_data it_fractions = lt_pack_fract_entity_data it_products = lt_pack_prod_entity_data iv_deleted = '' ).
    INSERT lo_packelem_data INTO TABLE lt_entity_data.

    lv_payload = f_cut->/VPCOE/if_uph_transfer_mapper~prepare_payload(
                                                            it_entity_data = lt_entity_data
                                                            is_parameters = ls_parameters   ).


*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = lv_Payload
*      exp   = lv_Payload
*    ).
  ENDMETHOD.




ENDCLASS.
