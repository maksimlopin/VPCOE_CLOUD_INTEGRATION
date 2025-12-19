*CLASS ltc_get_entity_url_suffix DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut TYPE REF TO cl_surdp_uph_tm_pckg_cmp_it_v2.
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
*CLASS ltc_get_element_id_by_index DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut TYPE REF TO cl_surdp_uph_tm_pckg_cmp_it_v2.
*
*    METHODS setup.
*
*    METHODS get_element_id_by_index FOR TESTING.
*
*ENDCLASS.
*
*CLASS cl_surdp_uph_tm_pckg_cmp_it_v2 DEFINITION LOCAL FRIENDS ltc_get_element_id_by_index.
*
*CLASS ltc_get_element_id_by_index IMPLEMENTATION.
*  METHOD get_element_id_by_index.
*    DATA lv_element_id1 TYPE string.
*    DATA lv_element_id2 TYPE string.
*    DATA lv_element_id3 TYPE string.
*
*    lv_element_id1 = f_cut->get_element_id_by_index( iv_index = 1 ).
*    lv_element_id2 = f_cut->get_element_id_by_index( iv_index = 2 ).
*    lv_element_id3 = f_cut->get_element_id_by_index( iv_index = 3 ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 'item1'
*                                        act = lv_element_id1 ).
*    cl_abap_unit_assert=>assert_equals( exp = 'item2'
*                                        act = lv_element_id2 ).
*    cl_abap_unit_assert=>assert_initial( lv_element_id3 ).
*  ENDMETHOD.
*
*  METHOD setup.
*    f_cut = NEW #(  ).
*
*    f_cut->ms_entity_hu_data_api = VALUE #( elements = VALUE #( ( id = 'item1' )
*                                                                ( id = 'item2' ) ) ).
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS ltc_prepare_payload DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut TYPE REF TO cl_surdp_uph_tm_pckg_cmp_it_v2.
*
*    METHODS setup.
*
*    METHODS given_packcomp_hdr_data IMPORTING iv_displayid                TYPE string
*                                    RETURNING VALUE(rs_packcomp_hdr_data) TYPE surdps_uph_ent_pack_cmp_hdr.
*
*    METHODS given_packcomp_item_data IMPORTING iv_packagingelementdisplayid TYPE string
*                                     RETURNING VALUE(rs_packcomp_item_data) TYPE surdps_uph_ent_pack_cmp_item.
*    METHODS given_packcomp_subitem_data IMPORTING iv_packagingelementdisplayid TYPE string
*                                     RETURNING VALUE(rs_packcomp_subitem_data) TYPE surdps_uph_ent_pack_cmp_subitm.
*
*    METHODS no_entity_data_empty_payload FOR TESTING.
*    METHODS one_composition_with_subitem     FOR TESTING.
*    METHODS one_composition_no_subitem     FOR TESTING.
*
*ENDCLASS.
*
*CLASS cl_surdp_uph_tm_pckg_cmp_it_v2 DEFINITION LOCAL FRIENDS ltc_prepare_payload.
*
*CLASS ltc_prepare_payload IMPLEMENTATION.
*  METHOD setup.
*    f_cut = NEW #(  ).
*  ENDMETHOD.
*
*  METHOD given_packcomp_hdr_data.
*    rs_packcomp_hdr_data-displayid           = iv_displayid.
*  ENDMETHOD.
*
*  METHOD given_packcomp_item_data.
*    rs_packcomp_item_data-packagingelementdisplayid = iv_packagingelementdisplayid.
*    rs_packcomp_item_data-packagingelementversion   = '1'.
*    rs_packcomp_item_data-levelcode                 = 'AB'.
*    rs_packcomp_item_data-quantity                  = 100.
*    rs_packcomp_item_data-quantityunitofmeasureid   = 'EA'.
*    rs_packcomp_item_data-usage                     = 'commercial'.
*    rs_packcomp_item_data-coverage                  = '26'.
*    rs_packcomp_item_data-count                     = 100.
*  ENDMETHOD.
*
*  METHOD given_packcomp_subitem_data.
*    rs_packcomp_subitem_data-packagingelementdisplayid = iv_packagingelementdisplayid.
*    rs_packcomp_subitem_data-packagingelementversion   = '1'.
*    rs_packcomp_subitem_data-quantity                  = 15.
*    rs_packcomp_subitem_data-quantityunitofmeasureid   = 'KG'.
*    rs_packcomp_subitem_data-usage                     = 'commercial'.
*    rs_packcomp_subitem_data-count                     = 15.
*    rs_packcomp_subitem_data-separability              = 'C'.
*  ENDMETHOD.
*
*    METHOD no_entity_data_empty_payload.
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
*  METHOD one_composition_with_subitem.
*    "given
*    DATA lt_entity_data        TYPE surdpt_uph_entity_data.
*    DATA ls_parameters         TYPE surdps_pckg_elem_input.
*
*    DATA lv_act_payload        TYPE string.
*    DATA lv_exp_payload        TYPE string.
*    DATA lt_packcomp_item_data TYPE surdpt_uph_entity_data.
*
*    ls_parameters-source_id = 'S4H'.
*    DATA(ls_packcomp_item_data) = given_packcomp_item_data( iv_packagingelementdisplayid = 'HU_TEST_ITEM_1' ).
*    DATA(lo_pckg_cmp_item) = NEW cl_surdp_uph_ent_pckg_cmp_item( is_data    = ls_packcomp_item_data
*                                                                 iv_deleted = '' ).
*    APPEND lo_pckg_cmp_item TO lt_packcomp_item_data.
*
*    DATA(ls_packcomp_hdr_data) = given_packcomp_hdr_data( iv_displayid = 'PC_1' ).
*    DATA(ls_packcomp_subitem_data) = given_packcomp_subitem_data( iv_packagingelementdisplayid = 'SUB01' ).
*    lo_pckg_cmp_item->set_subitems( it_cmp_subitem_data = VALUE #( ( NEW cl_surdp_uph_ent_pckg_cmp_sub( ls_packcomp_subitem_data ) ) ) ).
*
*    DATA(lo_packcomp_data) = NEW cl_surdp_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_packcomp_hdr_data
*                                                                it_cmp_item_data = lt_packcomp_item_data
*                                                                it_products      = VALUE #( )
*                                                                iv_deleted       = '' ).
*    APPEND lo_packcomp_data TO lt_entity_data.
*
*    lv_exp_payload = |\{"source":"S4H","elements":[\{"id":"PC_1",| &&
*                     |"items":[\{"packagingElementId":"HU_TEST_ITEM_1","packagingElementVersion":"1","level":"AB","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"commercial","coverage":26.00,"count":100,| &&
*                     |"subItems":[\{"packagingElementId":"SUB01","packagingElementVersion":"1","quantity":15.000000,"quantityUnitOfMeasure":"KG","usage":"commercial","count":15,"separability":"C"\}]\}]\}]\}|.
*    "when
*    lv_act_payload = f_cut->if_surdp_uph_transfer_mapper~prepare_payload( it_entity_data = lt_entity_data
*                                                                          is_parameters  = ls_parameters ).
*    "then
*    cl_abap_unit_assert=>assert_equals( exp = lv_exp_payload
*                                        act = lv_act_payload ).
*  ENDMETHOD.
*
*  METHOD one_composition_no_subitem.
*    "given
*    DATA lt_entity_data        TYPE surdpt_uph_entity_data.
*    DATA ls_parameters         TYPE surdps_pckg_elem_input.
*
*    DATA lv_act_payload        TYPE string.
*    DATA lv_exp_payload        TYPE string.
*    DATA lt_packcomp_item_data TYPE surdpt_uph_entity_data.
*
*    ls_parameters-source_id = 'S4H'.
*    DATA(ls_packcomp_item_data) = given_packcomp_item_data( iv_packagingelementdisplayid = 'HU_TEST_ITEM_1' ).
*    DATA(lo_pckg_cmp_item) = NEW cl_surdp_uph_ent_pckg_cmp_item( is_data    = ls_packcomp_item_data
*                                                                 iv_deleted = '' ).
*    APPEND lo_pckg_cmp_item TO lt_packcomp_item_data.
*
*    DATA(ls_packcomp_hdr_data) = given_packcomp_hdr_data( iv_displayid = 'PC_1' ).
*    DATA(lo_packcomp_data) = NEW cl_surdp_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_packcomp_hdr_data
*                                                                it_cmp_item_data = lt_packcomp_item_data
*                                                                it_products      = VALUE #( )
*                                                                iv_deleted       = '' ).
*    APPEND lo_packcomp_data TO lt_entity_data.
*
*    lv_exp_payload = |\{"source":"S4H","elements":[\{"id":"PC_1",| &&
*                     |"items":[\{"packagingElementId":"HU_TEST_ITEM_1","packagingElementVersion":"1","level":"AB","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"commercial","coverage":26.00,"count":100\}]\}]\}|.
*    "when
*    lv_act_payload = f_cut->if_surdp_uph_transfer_mapper~prepare_payload( it_entity_data = lt_entity_data
*                                                                          is_parameters  = ls_parameters ).
*    "then
*    cl_abap_unit_assert=>assert_equals( exp = lv_exp_payload
*                                        act = lv_act_payload ).
*  ENDMETHOD.
*
*ENDCLASS.
