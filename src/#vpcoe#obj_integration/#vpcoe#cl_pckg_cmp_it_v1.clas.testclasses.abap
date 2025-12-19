*CLASS ltc_get_entity_url_suffix DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut TYPE REF TO cl_surdp_uph_tm_pckg_cmp_it_v1.
*
*    METHODS setup.
*
*    METHODS get_entity_url_suffix FOR TESTING.
*
*ENDCLASS.
*
*CLASS ltc_get_entity_url_suffix IMPLEMENTATION.
*  METHOD get_entity_url_suffix.
*    DATA lv_entity_url_suffix TYPE string.
*
*    lv_entity_url_suffix = f_cut->if_surdp_uph_transfer_mapper~get_entity_url_suffix( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = '/PackagingCompositionItems'
*                                        act = lv_entity_url_suffix ).
*  ENDMETHOD.
*
*  METHOD setup.
*    f_cut = NEW #(  ).
*  ENDMETHOD.
*
*ENDCLASS.
*
*
*CLASS ltc_prepare_payload DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut TYPE REF TO cl_surdp_uph_tm_pckg_cmp_it_v1.
*
*    METHODS setup.
*
*    METHODS given_packcomp_hdr_data IMPORTING iv_displayid                TYPE string
*                                    RETURNING VALUE(rs_packcomp_hdr_data) TYPE surdps_uph_ent_pack_cmp_hdr.
*
*    METHODS given_packcomp_item_data IMPORTING iv_packagingelementdisplayid TYPE string
*                                     RETURNING VALUE(rs_packcomp_item_data) TYPE surdps_uph_ent_pack_cmp_item.
*
*    METHODS no_entity_data_empty_payload FOR TESTING.
*    METHODS one_composition_one_item     FOR TESTING.
*    METHODS multiple_compositions        FOR TESTING.
*    METHODS one_composition_no_items     FOR TESTING.
*
*ENDCLASS.
*
*CLASS ltc_prepare_payload IMPLEMENTATION.
*  METHOD setup.
*    f_cut = NEW #(  ).
*  ENDMETHOD.
*
*   METHOD given_packcomp_hdr_data.
*    rs_packcomp_hdr_data-displayid           = iv_displayid.
*    rs_packcomp_hdr_data-description         = 'Cookies'.
*    rs_packcomp_hdr_data-basequantity        = 100.
*    rs_packcomp_hdr_data-baseunitofmeasureid = 'EA'.
*    rs_packcomp_hdr_data-consumersalesunit   = 'ST'.
*  ENDMETHOD.
*
*
*  METHOD given_packcomp_item_data.
*    rs_packcomp_item_data-packagingelementdisplayid = iv_packagingelementdisplayid.
*    rs_packcomp_item_data-packagingelementversion   = '1'.
*    rs_packcomp_item_data-levelcode                 = 'AB'.
*    rs_packcomp_item_data-quantity                  = 100.
*    rs_packcomp_item_data-quantityunitofmeasureid   = 'EA'.
*    rs_packcomp_item_data-eprgroup                  = ''.
*    rs_packcomp_item_data-wwfgroup                  = ''.
*    rs_packcomp_item_data-usage                     = 'commercial'.
*    rs_packcomp_item_data-count                     = 100.
*  ENDMETHOD.
*
*  METHOD multiple_compositions.
*    DATA lt_entity_data        TYPE surdpt_uph_entity_data.
*    DATA ls_parameters         TYPE surdps_pckg_elem_input.
*
*    DATA lv_act_payload        TYPE string.
*    DATA lv_exp_payload        TYPE string.
*    DATA ls_packcomp_hdr_data  TYPE surdps_uph_ent_pack_cmp_hdr.
*    DATA ls_packcomp_item_data TYPE surdps_uph_ent_pack_cmp_item.
*    DATA ls_packcomp_prod_data TYPE surdps_uph_ent_pack_prod.
*    DATA lt_packcomp_item_data TYPE surdpt_uph_entity_data.
*    DATA lt_packcomp_prod_data TYPE surdpt_uph_entity_data.
*
*    ls_parameters-source_id = 'S4H'.
*
*    " given packaging composition 1 header data
*    ls_packcomp_hdr_data = given_packcomp_hdr_data( iv_displayid = 'ZHU_TEST_PRODUCT-20240101' ).
*
*    ls_packcomp_prod_data-productid = 'ZHU_TEST_PRODUCT'.
*    DATA(lo_ent_packelem_prod) = NEW cl_surdp_uph_ent_pckg_product( is_data    = ls_packcomp_prod_data
*                                                                    iv_deleted = '' ).
*    INSERT lo_ent_packelem_prod INTO TABLE lt_packcomp_prod_data.
*
*    " given packaging composition item data
*    ls_packcomp_item_data = given_packcomp_item_data( iv_packagingelementdisplayid = 'HU_TEST_MAT_1' ).
*
*    DATA(lo_pckg_cmp_item) = NEW cl_surdp_uph_ent_pckg_cmp_item( is_data    = ls_packcomp_item_data
*                                                                 iv_deleted = '' ).
*    INSERT lo_pckg_cmp_item INTO TABLE lt_packcomp_item_data.
*
*    ls_packcomp_item_data = given_packcomp_item_data( iv_packagingelementdisplayid = 'HU_TEST_MAT_2' ).
*    lo_pckg_cmp_item = NEW cl_surdp_uph_ent_pckg_cmp_item( is_data    = ls_packcomp_item_data
*                                                           iv_deleted = '' ).
*    INSERT lo_pckg_cmp_item INTO TABLE lt_packcomp_item_data.
*
*    DATA(lo_packcomp_data) = NEW cl_surdp_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_packcomp_hdr_data
*                                                                it_cmp_item_data = lt_packcomp_item_data
*                                                                it_products      = lt_packcomp_prod_data
*                                                                iv_deleted       = '' ).
*    INSERT lo_packcomp_data INTO TABLE lt_entity_data.
*
*    " given packaging composition 2 header data
*    ls_packcomp_hdr_data = given_packcomp_hdr_data( iv_displayid = 'ZHU_TEST_PRODUCT_1-20240101' ).
*
*    ls_packcomp_prod_data-productid = 'ZHU_TEST_PRODUCT_1'.
*    lo_ent_packelem_prod = NEW cl_surdp_uph_ent_pckg_product( is_data    = ls_packcomp_prod_data
*                                                                    iv_deleted = '' ).
*    INSERT lo_ent_packelem_prod INTO TABLE lt_packcomp_prod_data.
*
*    " given packaging composition item data
*    ls_packcomp_item_data = given_packcomp_item_data( iv_packagingelementdisplayid = 'HU_TEST_MAT_1' ).
*
*    lo_pckg_cmp_item = NEW cl_surdp_uph_ent_pckg_cmp_item( is_data    = ls_packcomp_item_data
*                                                                 iv_deleted = '' ).
*    INSERT lo_pckg_cmp_item INTO TABLE lt_packcomp_item_data.
*
*    ls_packcomp_item_data = given_packcomp_item_data( iv_packagingelementdisplayid = 'HU_TEST_MAT_2' ).
*    lo_pckg_cmp_item = NEW cl_surdp_uph_ent_pckg_cmp_item( is_data    = ls_packcomp_item_data
*                                                           iv_deleted = '' ).
*    INSERT lo_pckg_cmp_item INTO TABLE lt_packcomp_item_data.
*
*    lo_packcomp_data = NEW cl_surdp_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_packcomp_hdr_data
*                                                                it_cmp_item_data = lt_packcomp_item_data
*                                                                it_products      = lt_packcomp_prod_data
*                                                                iv_deleted       = '' ).
*    INSERT lo_packcomp_data INTO TABLE lt_entity_data.
*
*
*    " when
*    lv_act_payload = f_cut->if_surdp_uph_transfer_mapper~prepare_payload( it_entity_data = lt_entity_data
*                                                                          is_parameters  = ls_parameters ).
*
*    " then
*    lv_exp_payload = '{"source":"S4H","elements":[{"id":"ZHU_TEST_PRODUCT-20240101",' &&
*                     '"items":[{"packagingElementId":"HU_TEST_MAT_1","packagingElementVersion":"1",' &&
*                     '"level":"AB","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"commercial",' &&
*                     '"count":100},{"packagingElementId":"HU_TEST_MAT_2","packagingElementVersion":"1","level":"AB",' &&
*                     '"quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"commercial","count":100}]},' &&
*                     '{"id":"ZHU_TEST_PRODUCT_1-20240101","items":[{"packagingElementId":"HU_TEST_MAT_1",' &&
*                     '"packagingElementVersion":"1","level":"AB","quantity":100.000000,"quantityUnitOfMeasure":"EA",' &&
*                     '"usage":"commercial","count":100},{"packagingElementId":"HU_TEST_MAT_2","packagingElementVersion":"1",' &&
*                     '"level":"AB","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"commercial","count":100},{"packagingElementId"' &&
*                     ':"HU_TEST_MAT_1","packagingElementVersion":"1","level":"AB","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"commercial",' &&
*                     '"count":100},{"packagingElementId":"HU_TEST_MAT_2","packagingElementVersion":"1","level":"AB","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"commercial","count":100}]}]}'.
*
*   cl_abap_unit_assert=>assert_equals( exp = lv_exp_payload act = lv_act_payload ).
*  ENDMETHOD.
*
*  METHOD no_entity_data_empty_payload.
*    DATA ls_parameters  TYPE surdps_pckg_elem_input.
*    DATA lv_act_payload TYPE string.
*    DATA lv_exp_payload TYPE string.
*
*    " given empty input and empty entity data
*    lv_act_payload = f_cut->if_surdp_uph_transfer_mapper~prepare_payload( it_entity_data = VALUE #( )
*                                                                          is_parameters  = ls_parameters ).
*
*    lv_exp_payload = '{}'.
*
*    cl_abap_unit_assert=>assert_equals( exp = lv_exp_payload
*                                        act = lv_act_payload ).
*  ENDMETHOD.
*
*  METHOD one_composition_one_item.
*    DATA lt_entity_data        TYPE surdpt_uph_entity_data.
*    DATA ls_parameters         TYPE surdps_pckg_elem_input.
*
*    DATA lv_act_payload        TYPE string.
*    DATA lv_exp_payload        TYPE string.
*    DATA ls_packcomp_hdr_data  TYPE surdps_uph_ent_pack_cmp_hdr.
*    DATA ls_packcomp_item_data TYPE surdps_uph_ent_pack_cmp_item.
*    DATA ls_packcomp_prod_data TYPE surdps_uph_ent_pack_prod.
*    DATA lt_packcomp_item_data TYPE surdpt_uph_entity_data.
*    DATA lt_packcomp_prod_data TYPE surdpt_uph_entity_data.
*
*    ls_parameters-source_id = 'S4H'.
*
*    " given packaging composition header data
*    ls_packcomp_hdr_data = given_packcomp_hdr_data( iv_displayid = 'ZHU_TEST_PRODUCT-20240101' ).
*
*    ls_packcomp_prod_data-productid = 'ZHU_TEST_PRODUCT'.
*    DATA(lo_ent_packelem_prod) = NEW cl_surdp_uph_ent_pckg_product( is_data    = ls_packcomp_prod_data
*                                                                    iv_deleted = '' ).
*    INSERT lo_ent_packelem_prod INTO TABLE lt_packcomp_prod_data.
*
*    " given packaging composition item data
*    ls_packcomp_item_data = given_packcomp_item_data( iv_packagingelementdisplayid = 'HU_TEST_MAT_1' ).
*
*    DATA(lo_pckg_cmp_item) = NEW cl_surdp_uph_ent_pckg_cmp_item( is_data    = ls_packcomp_item_data
*                                                                 iv_deleted = '' ).
*    INSERT lo_pckg_cmp_item INTO TABLE lt_packcomp_item_data.
*
*    DATA(lo_packcomp_data) = NEW cl_surdp_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_packcomp_hdr_data
*                                                                it_cmp_item_data = lt_packcomp_item_data
*                                                                it_products      = lt_packcomp_prod_data
*                                                                iv_deleted       = '' ).
*    INSERT lo_packcomp_data INTO TABLE lt_entity_data.
*
*    " when
*    lv_act_payload = f_cut->if_surdp_uph_transfer_mapper~prepare_payload( it_entity_data = lt_entity_data
*                                                                          is_parameters  = ls_parameters ).
*    " TODO: check packcomp to product assignment in payload
*    " then
*    lv_exp_payload = |\{"source":"S4H","elements":[\{"id":"ZHU_TEST_PRODUCT-20240101",| &&
*                     |"items":[\{"packagingElementId":"HU_TEST_MAT_1","packagingElementVersion":"1","level":"AB","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"commercial","count":100\}]\}]\}|.
*    cl_abap_unit_assert=>assert_equals( exp = lv_exp_payload
*                                        act = lv_act_payload ).
*  ENDMETHOD.
*
*  METHOD one_composition_no_items.
*     DATA lt_entity_data        TYPE surdpt_uph_entity_data.
*    DATA ls_parameters         TYPE surdps_pckg_elem_input.
*
*    DATA lv_act_payload        TYPE string.
*    DATA lv_exp_payload        TYPE string.
*    DATA ls_packcomp_hdr_data  TYPE surdps_uph_ent_pack_cmp_hdr.
*    DATA ls_packcomp_item_data TYPE surdps_uph_ent_pack_cmp_item.
*    DATA ls_packcomp_prod_data TYPE surdps_uph_ent_pack_prod.
*    DATA lt_packcomp_item_data TYPE surdpt_uph_entity_data.
*    DATA lt_packcomp_prod_data TYPE surdpt_uph_entity_data.
*
*    " given packaging composition header data
*    ls_packcomp_hdr_data = given_packcomp_hdr_data( iv_displayid = 'RCP_001' ).
*
*    ls_packcomp_prod_data-productid =  'Marigold'.
*    DATA(lo_ent_packelem_prod) = NEW cl_surdp_uph_ent_pckg_product( is_data = ls_packcomp_prod_data iv_deleted = '' ).
*    INSERT lo_ent_packelem_prod INTO TABLE lt_packcomp_prod_data.
*
*    DATA(lo_packcomp_data) = NEW cl_surdp_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_packcomp_hdr_data
*                                                                it_cmp_item_data = lt_packcomp_item_data
*                                                                it_products      = lt_packcomp_prod_data
*                                                                iv_deleted       = '' ).
*    INSERT lo_packcomp_data INTO TABLE lt_entity_data.
*
*    " when
*    lv_act_payload = f_cut->if_surdp_uph_transfer_mapper~prepare_payload( it_entity_data = lt_entity_data is_parameters = ls_parameters ).
*
*    " then
*    lv_exp_payload = '{"elements":[{"id":"RCP_001"}]}'.
*    cl_abap_unit_assert=>assert_equals( act = lv_act_payload exp = lv_exp_payload ).
*
*  ENDMETHOD.
*
*ENDCLASS.
