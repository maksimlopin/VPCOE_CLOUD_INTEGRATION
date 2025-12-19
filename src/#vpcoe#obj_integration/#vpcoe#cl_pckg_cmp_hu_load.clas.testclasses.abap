*CLASS ltc_derive_input_from_params DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA mo_cut TYPE REF TO cl_surdp_uph_pckg_cmp_hu_load.
*
*    METHODS setup.
*
*    METHODS test_all_params_given FOR TESTING.
*    METHODS test_no_params_given  FOR TESTING.
*
*ENDCLASS.
*
*CLASS ltc_derive_input_from_params IMPLEMENTATION.
*  METHOD setup.
*    mo_cut = NEW #(  ).
*  ENDMETHOD.
*
*  METHOD test_all_params_given.
*    DATA ls_act_input                TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*    DATA ls_exp_input                TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*    DATA lv_source_id                TYPE surdp_uph_source_id.
*    DATA lv_rfc_destination          TYPE surdp_uph_rfcdest.
*    DATA lv_document_type_range      TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_document_type_range.
*    DATA lv_sales_org_range          TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_sales_org_range.
*    DATA lv_country_range            TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_country_range.
*    DATA lv_division_range           TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_division_range.
*    DATA lv_category_range           TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_category_range.
*    DATA lv_distribution_range       TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_distribution_range.
*    DATA lv_sddoc_range              TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_sddoc_range.
*    DATA lv_act_goods_mvt_date_range TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_act_goods_mvt_date_range.
*    DATA lv_plant_country_range      TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_plant_country_range.
*    DATA lv_material_range           TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_material_range.
*    DATA lv_material_type_range      TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_material_type_range.
*    DATA lv_material_group_range     TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_material_group_range.
*    DATA lv_plant_range              TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_plant_range.
*    DATA lv_ship_to_party_range      TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_ship_to_party_range.
*
*    " given all params
*    lv_source_id = 'S4H'.
*    lv_rfc_destination = 'RDP_REPL_API'.
*    lv_document_type_range = VALUE #( ( sign = 'I' high = '' low = 'LF' option = 'EQ' ) ).
*    lv_sales_org_range = VALUE #( ( sign = 'I' high = '' low = '0001' option = 'EQ' ) ).
*    lv_country_range  = VALUE #( ( sign = 'I' high = '' low = 'DE' option = 'EQ' ) ).
*    lv_division_range = VALUE #( ( sign = 'I' high = '' low = '01' option = 'EQ' ) ).
*    lv_category_range = VALUE #( ( sign = 'I' high = '' low = 'ABCD' option = 'EQ' ) ).
*    lv_distribution_range = VALUE #( ( sign = 'I' high = '' low = '01' option = 'EQ' ) ).
*    lv_sddoc_range = VALUE #( ( sign = 'I' high = '' low = '0123456789' option = 'EQ' ) ).
*    lv_act_goods_mvt_date_range = VALUE #( ( sign = 'I' high = '20201231' low = '20200930' option = 'BT' ) ).
*    lv_plant_country_range = VALUE #( ( sign = 'I' high = '' low = 'DE' option = 'EQ' ) ).
*    lv_material_range = VALUE #( ( sign = 'I' high = '' low = 'TEST_MAT' option = 'EQ' ) ).
*    lv_material_type_range = VALUE #( ( sign = 'I' high = '' low = 'VERP' option = 'EQ' ) ).
*    lv_material_group_range = VALUE #( ( sign = 'I' high = '' low = '123456789' option = 'EQ' ) ).
*    lv_plant_range = VALUE #( ( sign = 'I' high = '' low = '1010' option = 'EQ' ) ).
*    lv_ship_to_party_range = VALUE #( ( sign = 'I' high = '' low = '1234567899' option = 'EQ' ) ).
*
*    " when
*    ls_act_input = mo_cut->derive_input_from_parameters( iv_source_id                = lv_source_id
*                                                         iv_rfc_destination          = lv_rfc_destination
*                                                         iv_document_type_range      = lv_document_type_range
*                                                         iv_sales_org_range          = lv_sales_org_range
*                                                         iv_country_range            = lv_country_range
*                                                         iv_division_range           = lv_division_range
*                                                         iv_category_range           = lv_category_range
*                                                         iv_distribution_range       = lv_distribution_range
*                                                         iv_sddoc_range              = lv_sddoc_range
*                                                         iv_act_goods_mvt_date_range = lv_act_goods_mvt_date_range
*                                                         iv_plant_country_rage       = lv_plant_country_range
*                                                         iv_material_range           = lv_material_range
*                                                         iv_material_type_range      = lv_material_type_range
*                                                         iv_material_group_range     = lv_material_group_range
*                                                         iv_plant_range              = lv_plant_range
*                                                         iv_ship_to_party_range      = lv_ship_to_party_range ).
*
*    " then
*    ls_exp_input = VALUE #( source_id          = lv_source_id
*                            rfc_des            = lv_rfc_destination
*                            document_type      = lv_document_type_range
*                            sales_org          = lv_sales_org_range
*                            country            = lv_country_range
*                            division           = lv_division_range
*                            category           = lv_category_range
*                            distribution       = lv_distribution_range
*                            sddoc              = lv_sddoc_range
*                            act_goods_mvt_date = lv_act_goods_mvt_date_range
*                            plant_country      = lv_plant_country_range
*                            material           = lv_material_range
*                            material_group     = lv_material_group_range
*                            material_type      = lv_material_type_range
*                            plant              = lv_plant_range
*                            ship_to_party      = lv_ship_to_party_range ).
*
*   cl_abap_unit_assert=>assert_equals(  exp = ls_exp_input act = ls_act_input ).
*  ENDMETHOD.
*
*  METHOD test_no_params_given.
*    DATA ls_act_input                TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*    DATA ls_exp_input                TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*    DATA lv_source_id                TYPE surdp_uph_source_id.
*    DATA lv_rfc_destination          TYPE surdp_uph_rfcdest.
*    DATA lv_document_type_range      TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_document_type_range.
*    DATA lv_sales_org_range          TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_sales_org_range.
*    DATA lv_country_range            TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_country_range.
*    DATA lv_division_range           TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_division_range.
*    DATA lv_category_range           TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_category_range.
*    DATA lv_distribution_range       TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_distribution_range.
*    DATA lv_sddoc_range              TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_sddoc_range.
*    DATA lv_act_goods_mvt_date_range TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_act_goods_mvt_date_range.
*    DATA lv_plant_country_range      TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_plant_country_range.
*    DATA lv_material_range           TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_material_range.
*    DATA lv_material_type_range      TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_material_type_range.
*    DATA lv_material_group_range     TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_material_group_range.
*    DATA lv_plant_range              TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_plant_range.
*    DATA lv_ship_to_party_range      TYPE if_surdp_uph_pckg_cmp_hu_load=>ty_ship_to_party_range.
*
*    " given empty input
*
*    " when
*    ls_act_input = mo_cut->derive_input_from_parameters( iv_source_id                = lv_source_id
*                                                         iv_rfc_destination          = lv_rfc_destination
*                                                         iv_document_type_range      = lv_document_type_range
*                                                         iv_sales_org_range          = lv_sales_org_range
*                                                         iv_country_range            = lv_country_range
*                                                         iv_division_range           = lv_division_range
*                                                         iv_category_range           = lv_category_range
*                                                         iv_distribution_range       = lv_distribution_range
*                                                         iv_sddoc_range              = lv_sddoc_range
*                                                         iv_act_goods_mvt_date_range = lv_act_goods_mvt_date_range
*                                                         iv_plant_country_rage       = lv_plant_country_range
*                                                         iv_material_range           = lv_material_range
*                                                         iv_material_type_range      = lv_material_type_range
*                                                         iv_material_group_range     = lv_material_group_range
*                                                         iv_plant_range              = lv_plant_range
*                                                         iv_ship_to_party_range      = lv_ship_to_party_range ).
*
*    " then
*    ls_exp_input = VALUE #( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = ls_exp_input
*                                        act = ls_act_input ).
*  ENDMETHOD.
*
*ENDCLASS.
*
*
*
*CLASS ltc_is_authorized DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA mo_cut              TYPE REF TO cl_surdp_uph_pckg_cmp_hu_load.
*
*    METHODS setup.
*    METHODS teardown.
*
*    METHODS test_with_authorization    FOR TESTING.
*    METHODS test_without_authorization FOR TESTING
*              RAISING
*                cx_abap_auth_check_exception.
*
*ENDCLASS.
*
*CLASS ltc_is_authorized IMPLEMENTATION.
*
*  METHOD setup.
*    mo_cut = NEW #( ).
*  ENDMETHOD.
*
*  METHOD teardown.
*    CLEAR: mo_cut.
*  ENDMETHOD.
*
*  METHOD test_without_authorization.
*    " Given no authorization
*    cl_aunit_authority_check=>get_controller( )->restrict_authorizations_to( cl_aunit_authority_check=>create_auth_object_set( ) ).
*
*    DATA(lv_authorization_granted) = mo_cut->is_authorized( ).
*
*    cl_abap_unit_assert=>assert_false( lv_authorization_granted ).
*  ENDMETHOD.
*
*  METHOD test_with_authorization.
*     " Given authorizations are set to current user's role (expected to be sufficient)
*    cl_aunit_authority_check=>get_controller( )->reset( ).
*
*    DATA(lv_authorization_granted) = mo_cut->is_authorized( ).
*
*    cl_abap_unit_assert=>assert_true( lv_authorization_granted ).
*  ENDMETHOD.
*
*ENDCLASS.
