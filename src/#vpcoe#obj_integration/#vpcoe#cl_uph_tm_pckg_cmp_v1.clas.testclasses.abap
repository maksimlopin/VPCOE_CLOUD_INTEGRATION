*"* use this source file for your ABAP unit test classes

CLASS /vpcoe/tcl_uph_tm_pckg_cmp_v1 DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_uph_tm_pckg_cmp_v1.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: evaluate_response FOR TESTING.
    METHODS: get_entity_url_suffix FOR TESTING.
    METHODS: prepare_payload FOR TESTING.
ENDCLASS.       "tcl_Surdp_Uph_Tm_Pckg_Cmp_V1


CLASS /vpcoe/tcl_uph_tm_pckg_cmp_v1 IMPLEMENTATION.

  METHOD class_setup.
  ENDMETHOD.

  METHOD class_teardown.
  ENDMETHOD.


  METHOD setup.
    f_cut = NEW #( ).
  ENDMETHOD.


  METHOD teardown.
  ENDMETHOD.


  METHOD evaluate_response.
    DATA lv_response TYPE string.
    DATA lt_messages TYPE /vpcoe/t_uph_msg.

    f_cut->/vpcoe/if_uph_transfer_mapper~evaluate_response(
      EXPORTING
        iv_response = lv_response
     IMPORTING
       et_messages = lt_messages
    ).

lv_response = | \{ | &&
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
  ENDMETHOD.


  METHOD get_entity_url_suffix.

    DATA lv_entity_url_suffix TYPE string.

    lv_entity_url_suffix = f_cut->/vpcoe/if_uph_transfer_mapper~get_entity_url_suffix(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_entity_url_suffix
      exp   = '/PackagingCompositions'
    ).
  ENDMETHOD.


  METHOD prepare_payload.

    DATA lt_entity_data TYPE /vpcoe/t_uph_entity_data.
    DATA ls_parameters TYPE /vpcoe/s_pckg_elem_input.
    DATA: lv_payload               TYPE string,
          ls_packcomp_hdr_data     TYPE /vpcoe/s_uph_ent_pack_cmp_hdr,
          ls_packcomp_item_data    TYPE /vpcoe/s_uph_ent_pack_cmp_item,
          ls_packcomp_prod_data    TYPE /vpcoe/s_uph_ent_pack_prod,
          lt_pack_item_entity_data TYPE /vpcoe/t_uph_entity_data,
          lt_pack_prod_entity_data TYPE /vpcoe/t_uph_entity_data.

          lv_payload = f_cut->/vpcoe/if_uph_transfer_mapper~prepare_payload(
        it_entity_data = lt_entity_data
        is_parameters = ls_parameters ).

    "input parameters
    ls_parameters-spec_radio_btn = abap_true.
    ls_parameters-subid = VALUE #( ( option = 'EQ' low = 'SPEC01' high = '' sign = 'I' ) ).
    ls_parameters-subcat = VALUE #( ( option = 'EQ' low = 'PURE_SUB' high = '' sign = 'I' ) ).
    ls_parameters-valfromdate = sy-datum.
    ls_parameters-sub_chgdon = VALUE #( ( option = 'GT' low = '20210101' high = '' sign = 'I' ) ).

    " it_entity_data

    ls_packcomp_hdr_data-source = 'S4-Hana'.
    ls_packcomp_hdr_data-displayid = 'Britania'.
    ls_packcomp_hdr_data-description = 'Cokkies'.
    ls_packcomp_hdr_data-basequantity = 100.
    ls_packcomp_hdr_data-baseunitofmeasureid = 'EA'.


    "Packaging Element to Products [0..n]

    ls_packcomp_prod_data-productid =  'Marigold'.
    DATA(lo_ent_packelem_prod) = NEW /vpcoe/cl_uph_ent_pckg_product( is_data = ls_packcomp_prod_data iv_deleted = '' ).
    INSERT lo_ent_packelem_prod INTO TABLE lt_pack_prod_entity_data.


    "Packaging composition item data[0..n]
    CLEAR ls_packcomp_item_data.

    ls_packcomp_item_data-packagingelementdisplayid = 'Britannia'.
*    ls_packcomp_item_data-packagingelementvalidfrom = '2020.10.10'.
    ls_packcomp_item_data-levelcode = 'AB'.
    ls_packcomp_item_data-quantity = 100.
    ls_packcomp_item_data-quantityunitofmeasureid = 'EA'.
    ls_packcomp_item_data-eprgroup = ' '.
    ls_packcomp_item_data-wwfgroup = ' '.

    DATA(lo_packelem_frac) = NEW /vpcoe/cl_uph_ent_pckg_cmp_itm( is_data = ls_packcomp_item_data iv_deleted = '' ).
    INSERT lo_packelem_frac INTO TABLE lt_pack_item_entity_data.

    DATA(lo_packcomp_data) = NEW /vpcoe/cl_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data = ls_packcomp_hdr_data it_cmp_item_data = lt_pack_item_entity_data it_products = lt_pack_prod_entity_data iv_deleted = '' ).
    INSERT lo_packcomp_data INTO TABLE lt_entity_data.

    lv_payload = f_cut->/vpcoe/if_uph_transfer_mapper~prepare_payload(
        it_entity_data = lt_entity_data
        is_parameters = ls_parameters ).

    cl_abap_unit_assert=>assert_not_initial(
      act   = lv_payload
    ).
  ENDMETHOD.

ENDCLASS.
