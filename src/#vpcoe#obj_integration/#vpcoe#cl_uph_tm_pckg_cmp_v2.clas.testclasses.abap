*CLASS ltc_get_entity_url_suffix DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut TYPE REF TO /vpcoe/cl_uph_tm_pckg_cmp_v2.
*
*    METHODS setup.
*
*    METHODS get_entity_url_suffix FOR TESTING.
*
*ENDCLASS.
*
*CLASS ltc_get_entity_url_suffix IMPLEMENTATION.
*  METHOD setup.
*    f_cut = NEW #( ).
*  ENDMETHOD.
*
*
*  METHOD get_entity_url_suffix.
*    DATA lv_entity_url_suffix TYPE string.
*
*    lv_entity_url_suffix = f_cut->/vpcoe/if_uph_transfer_mapper~get_entity_url_suffix( ).
*
*    cl_abap_unit_assert=>assert_equals( act = lv_entity_url_suffix
*                                        exp = '/PackagingCompositions' ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS ltc_prepare_payload DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut TYPE REF TO /vpcoe/cl_uph_tm_pckg_cmp_v2.
*
*    METHODS setup.
*
*    METHODS given_packcomp_hdr_data IMPORTING iv_displayid                TYPE string
*                                    RETURNING VALUE(rs_packcomp_hdr_data) TYPE /vpcoe/s_uph_ent_pack_cmp_hdr.
*
*    METHODS given_packcomp_subitem_data IMPORTING iv_packagingelementdisplayid    TYPE string
*                                        RETURNING VALUE(rs_packcomp_subitem_data) TYPE /vpcoe/s_uph_ent_pack_cmp_subitm.
*
*    METHODS given_packcomp_item_data IMPORTING iv_packagingelementdisplayid TYPE string
*                                     RETURNING VALUE(rs_packcomp_item_data) TYPE /vpcoe/s_uph_ent_pack_cmp_item.
*
*    METHODS no_entity_data_empty_payload  FOR TESTING.
*    METHODS multiple_packcomp_with_items  FOR TESTING.
*    METHODS packcomp_with_no_product      FOR TESTING.
*    METHODS packcomp_with_no_items        FOR TESTING.
*
*ENDCLASS.
*
*
*
*CLASS ltc_prepare_payload IMPLEMENTATION.
*  METHOD setup.
*    f_cut = NEW #( ).
*  ENDMETHOD.
*
*
*  METHOD given_packcomp_hdr_data.
*    rs_packcomp_hdr_data-displayid           = iv_displayid.
*    rs_packcomp_hdr_data-description         = 'Cookies'.
*    rs_packcomp_hdr_data-basequantity        = 100.
*    rs_packcomp_hdr_data-baseunitofmeasureid = 'EA'.
*    rs_packcomp_hdr_data-consumersalesunit   = 'PC'.
*  ENDMETHOD.
*
*  METHOD given_packcomp_subitem_data.
*    rs_packcomp_subitem_data-packagingelementdisplayid = iv_packagingelementdisplayid.
*    rs_packcomp_subitem_data-packagingelementversion   = '1'.
*    rs_packcomp_subitem_data-quantity                  = 20.
*    rs_packcomp_subitem_data-quantityunitofmeasureid   = 'PC'.
*    rs_packcomp_subitem_data-separability              = 'C-CONSUMER'.
*    rs_packcomp_subitem_data-usage                     = 'H'.
*    rs_packcomp_subitem_data-count                     = 1.
*  ENDMETHOD.
*
*  METHOD given_packcomp_item_data.
*    rs_packcomp_item_data-packagingelementdisplayid = iv_packagingelementdisplayid.
*    rs_packcomp_item_data-packagingelementversion   = '1'.
*    rs_packcomp_item_data-levelcode                 = '11'.
*    rs_packcomp_item_data-quantity                  = 100.
*    rs_packcomp_item_data-quantityunitofmeasureid   = 'EA'.
*    rs_packcomp_item_data-coverage                  = '35.5'.
*    rs_packcomp_item_data-usage                     = 'C'.
*    rs_packcomp_item_data-count                     = 100.
*  ENDMETHOD.
*
*
*  METHOD no_entity_data_empty_payload.
*    DATA lt_entity_data TYPE /vpcoe/t_uph_entity_data.
*    DATA ls_parameters  TYPE /vpcoe/s_pckg_elem_input.
*    DATA: lv_payload     TYPE string,
*          lv_exp_payload TYPE string.
*
*    " given empty input and empty entity data
*    lv_payload = f_cut->/vpcoe/if_uph_transfer_mapper~prepare_payload( it_entity_data = lt_entity_data is_parameters = ls_parameters ).
*
*    lv_exp_payload = '{}'.
*
*    cl_abap_unit_assert=>assert_equals( act = lv_payload exp = lv_exp_payload ).
*  ENDMETHOD.
*
*
*  METHOD multiple_packcomp_with_items.
*    DATA lt_entity_data TYPE /vpcoe/t_uph_entity_data.
*    DATA ls_parameters  TYPE /vpcoe/s_pckg_elem_input.
*
*    DATA: lv_payload            TYPE string,
*          lv_exp_payload        TYPE string,
*          ls_packcomp_hdr_data  TYPE /vpcoe/s_uph_ent_pack_cmp_hdr,
*          ls_packcomp_item_data TYPE /vpcoe/s_uph_ent_pack_cmp_item,
*          ls_packcomp_prod_data TYPE /vpcoe/s_uph_ent_pack_prod,
*          lt_packcomp_item_data TYPE /vpcoe/t_uph_entity_data,
*          lt_packcomp_prod_data TYPE /vpcoe/t_uph_entity_data.
*
*    ls_parameters-source_id = 'S4H'.
*
*    " given packaging composition header data
*    ls_packcomp_hdr_data = given_packcomp_hdr_data( iv_displayid = 'RCP_001' ).
*
*    ls_packcomp_prod_data-productid =  'M-PACK'.
*    ls_packcomp_prod_data-business_process_direction =  'ALL'.
*    ls_packcomp_prod_data-valid_from =  '20250101'.
*    ls_packcomp_prod_data-valid_to =  '99991231'.
*
*    DATA(lo_ent_packelem_prod) = NEW /vpcoe/cl_uph_ent_pckg_product( is_data = ls_packcomp_prod_data iv_deleted = '' ).
*    INSERT lo_ent_packelem_prod INTO TABLE lt_packcomp_prod_data.
*
*    " given packaging composition item data
*    DATA(ls_packcomp_sub_item_data1) = given_packcomp_subitem_data( iv_packagingelementdisplayid = 'SUB-001' ).
*    DATA(ls_packcomp_sub_item_data2) = given_packcomp_subitem_data( iv_packagingelementdisplayid = 'SUB-002' ).
*    ls_packcomp_sub_item_data2-separability = 'T-TECH'.
*
*    ls_packcomp_item_data = given_packcomp_item_data( iv_packagingelementdisplayid = 'RCP_001-01' ).
*
*    DATA(lo_pckg_cmp_item) = NEW /vpcoe/cl_uph_ent_pckg_cmp_item( is_data = ls_packcomp_item_data iv_deleted = '' ).
*
*    lo_pckg_cmp_item->set_subitems( it_cmp_subitem_data = VALUE #( ( NEW /vpcoe/cl_uph_ent_pckg_cmp_sub( ls_packcomp_sub_item_data1 ) )
*                                                                   ( NEW /vpcoe/cl_uph_ent_pckg_cmp_sub( ls_packcomp_sub_item_data2 ) ) ) ).
*
*    INSERT lo_pckg_cmp_item INTO TABLE lt_packcomp_item_data.
*
*    CLEAR ls_packcomp_item_data.
*
*    " given packaging composition item data
*    ls_packcomp_item_data = given_packcomp_item_data( iv_packagingelementdisplayid = 'RCP_001-02' ).
*    ls_packcomp_item_data-levelcode = '10'.
*    ls_packcomp_item_data-count     = 150.
*
*    lo_pckg_cmp_item = NEW /vpcoe/cl_uph_ent_pckg_cmp_item( is_data = ls_packcomp_item_data iv_deleted = '' ).
*    INSERT lo_pckg_cmp_item INTO TABLE lt_packcomp_item_data.
*
*    DATA(lo_packcomp_data) = NEW /vpcoe/cl_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_packcomp_hdr_data
*                                                                it_cmp_item_data = lt_packcomp_item_data
*                                                                it_products      = lt_packcomp_prod_data
*                                                                iv_deleted       = '' ).
*    INSERT lo_packcomp_data INTO TABLE lt_entity_data.
*
*    " when
*    lv_payload = f_cut->/vpcoe/if_uph_transfer_mapper~prepare_payload( it_entity_data = lt_entity_data
*                                                                      is_parameters  = ls_parameters ).
*
*    " then
*    lv_exp_payload = '{"source":"S4H","elements":[{"id":"RCP_001","description":"Cookies","baseQuantity":100.000000,"baseUnitOfMeasure":"EA","consumerSalesUnit":"PC","products":[{"product":"M-PACK","validFrom":"2025-01-01","validTo":"9999-12-31","bu' &&
*'sinessProcessDirection":"ALL"}],"items":[{"packagingElementId":"RCP_001-01","packagingElementVersion":"1","level":"11","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"C","coverage":35.50,"count":100,"subItems":[{"packagingElementId"' &&
*':"SUB-001","packagingElementVersion":"1","quantity":20.000000,"quantityUnitOfMeasure":"PC","usage":"H","count":1,"separability":"C-CONSUMER"},{"packagingElementId":"SUB-002","packagingElementVersion":"1","quantity":20.000000,"quantityUnitOfMeasure"' &&
*':"PC","usage":"H","count":1,"separability":"T-TECH"}]},{"packagingElementId":"RCP_001-02","packagingElementVersion":"1","level":"10","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"C","coverage":35.50,"count":150}]}]}'.
*
*    cl_abap_unit_assert=>assert_equals( act = lv_payload exp = lv_exp_payload ).
*  ENDMETHOD.
*
*
*  METHOD packcomp_with_no_product.
*    DATA lt_entity_data TYPE /vpcoe/t_uph_entity_data.
*    DATA ls_parameters  TYPE /vpcoe/s_pckg_elem_input.
*
*    DATA: lv_payload            TYPE string,
*          lv_exp_payload        TYPE string,
*          ls_packcomp_hdr_data  TYPE /vpcoe/s_uph_ent_pack_cmp_hdr,
*          ls_packcomp_item_data TYPE /vpcoe/s_uph_ent_pack_cmp_item,
*          lt_packcomp_item_data TYPE /vpcoe/t_uph_entity_data,
*          lt_packcomp_prod_data TYPE /vpcoe/t_uph_entity_data.
*
*    ls_parameters-source_id = 'S4H'.
*
*    " given packaging composition header data
*    ls_packcomp_hdr_data = given_packcomp_hdr_data( iv_displayid = 'RCP_001' ).
*
*    " given packaging composition item data
*    ls_packcomp_item_data = given_packcomp_item_data( iv_packagingelementdisplayid = 'RCP_001-01' ).
*
*    DATA(lo_pckg_cmp_item) = NEW /vpcoe/cl_uph_ent_pckg_cmp_item( is_data = ls_packcomp_item_data iv_deleted = '' ).
*    INSERT lo_pckg_cmp_item INTO TABLE lt_packcomp_item_data.
*
*    DATA(lo_packcomp_data) = NEW /vpcoe/cl_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_packcomp_hdr_data
*                                                                it_cmp_item_data = lt_packcomp_item_data
*                                                                it_products      = lt_packcomp_prod_data
*                                                                iv_deleted       = '' ).
*    INSERT lo_packcomp_data INTO TABLE lt_entity_data.
*
*    " when
*    lv_payload = f_cut->/vpcoe/if_uph_transfer_mapper~prepare_payload( it_entity_data = lt_entity_data is_parameters = ls_parameters ).
*
*    " then
*    lv_exp_payload = '{"source":"S4H","elements":[{"id":"RCP_001","description":"Cookies","baseQuantity":100.000000,"baseUnitOfMeasure":"EA","consumerSalesUnit":"PC","items":[{"packagingElementId":"RCP_001-01","packagingElementVersion":"1","level":"' &&
*'11","quantity":100.000000,"quantityUnitOfMeasure":"EA","usage":"C","coverage":35.50,"count":100}]}]}'.
*
*    cl_abap_unit_assert=>assert_equals( act = lv_payload exp = lv_exp_payload ).
*  ENDMETHOD.
*
*
*  METHOD packcomp_with_no_items.
*    DATA lt_entity_data TYPE /vpcoe/t_uph_entity_data.
*    DATA ls_parameters  TYPE /vpcoe/s_pckg_elem_input.
*
*    DATA: lv_payload            TYPE string,
*          lv_exp_payload        TYPE string,
*          ls_packcomp_hdr_data  TYPE /vpcoe/s_uph_ent_pack_cmp_hdr,
*          ls_packcomp_prod_data TYPE /vpcoe/s_uph_ent_pack_prod,
*          lt_packcomp_item_data TYPE /vpcoe/t_uph_entity_data,
*          lt_packcomp_prod_data TYPE /vpcoe/t_uph_entity_data.
*
*    ls_parameters-source_id = 'S4H'.
*
*    " given packaging composition header data
*    ls_packcomp_hdr_data = given_packcomp_hdr_data( iv_displayid = 'RCP_001' ).
*
*    ls_packcomp_prod_data-productid =  'Marigold'.
*    DATA(lo_ent_packelem_prod) = NEW /vpcoe/cl_uph_ent_pckg_product( is_data = ls_packcomp_prod_data iv_deleted = '' ).
*    INSERT lo_ent_packelem_prod INTO TABLE lt_packcomp_prod_data.
*
*    DATA(lo_packcomp_data) = NEW /vpcoe/cl_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_packcomp_hdr_data
*                                                                it_cmp_item_data = lt_packcomp_item_data
*                                                                it_products      = lt_packcomp_prod_data
*                                                                iv_deleted       = '' ).
*    INSERT lo_packcomp_data INTO TABLE lt_entity_data.
*
*    " when
*    lv_payload = f_cut->/vpcoe/if_uph_transfer_mapper~prepare_payload( it_entity_data = lt_entity_data is_parameters = ls_parameters ).
*
*    " then
*    lv_exp_payload = '{"source":"S4H","elements":[{"id":"RCP_001","description":"Cookies","baseQuantity":100.000000,"baseUnitOfMeasure":"EA","consumerSalesUnit":"PC","products":[{"product":"Marigold"}]}]}'.
*    cl_abap_unit_assert=>assert_equals( act = lv_payload exp = lv_exp_payload ).
*  ENDMETHOD.
*
*ENDCLASS.
