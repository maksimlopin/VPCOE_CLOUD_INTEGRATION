**"* use this source file for your ABAP unit test classes
*CLASS ltc_hu_example DEFINITION FOR TESTING RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    TYPES ltty_inob_data TYPE STANDARD TABLE OF inob.
*    TYPES ltty_tcla_data TYPE STANDARD TABLE OF tcla.
*
*    CLASS-DATA mo_osql_env TYPE REF TO if_osql_test_environment.
*
*    CLASS-METHODS class_setup.
*    CLASS-METHODS class_teardown.
*
*    METHODS setup.
*
*    METHODS given_classification_data IMPORTING iv_class_id   TYPE clint
*                                                iv_class_name TYPE klasse_d.
*
*    METHODS given_classification_assgmt IMPORTING iv_matnr         TYPE cuobn
*                                                  iv_class_id      TYPE clint
*                                                  iv_valid_from    TYPE datuv OPTIONAL
*                                                  iv_assgmt_number TYPE adzhl
*                                                  iv_atinn         TYPE atinn OPTIONAL.
*
*    METHODS given_inob_data
*      IMPORTING it_inob_data TYPE ltty_inob_data.
*
*    METHODS given_tcla_data
*      IMPORTING it_tcla_data TYPE ltty_tcla_data.
*
*    METHODS map_hu_data                FOR TESTING.
*    METHODS map_hu_data_multiple       FOR TESTING.
*    METHODS map_hu_data_multobj_active FOR TESTING.
*    METHODS map_hu_data_empty          FOR TESTING.
*
*    DATA mo_cut TYPE REF TO cl_surdp_uph_pckcomp_hu_exmpl.
*ENDCLASS.
*
*CLASS ltc_hu_example IMPLEMENTATION.
*
*
*
*  METHOD class_setup.
*    mo_osql_env = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'klah' )
*                                                                                 ( 'kssk' )
*                                                                                 ( 'tcla' )
*                                                                                 ( 'inob' )
*                                                                                 ( 'ksml' )
*                                                                                 ( 'ausp' ) ) ).
*  ENDMETHOD.
*
*  METHOD class_teardown.
*    mo_osql_env->destroy( ).
*  ENDMETHOD.
*
*  METHOD setup.
*    mo_osql_env->clear_doubles(  ).
*
*    th_surdp_uph_factory_injector=>inject_factory_double( io_double = NEW td_surdp_uph_factory( ) ).
*
*    mo_cut = NEW cl_surdp_uph_pckcomp_hu_exmpl( ).
*
*    given_classification_data( iv_class_id = '1' iv_class_name = 'ZMCL_PACKELEM_ATTR' ).
*  ENDMETHOD.
*
*  METHOD given_classification_assgmt.
*    DATA lt_class_data                  TYPE TABLE OF kssk.
*    DATA lt_characteristics_assgmt_data TYPE TABLE OF ausp.
*
*    lt_class_data = VALUE #( ( clint = iv_class_id
*                               klart = '001'
*                               objek = iv_matnr
*                               adzhl = iv_assgmt_number
*                               datuv = COND #( WHEN iv_valid_from IS SUPPLIED THEN iv_valid_from ELSE '00010101' )
*                               datub = '99991231'
*                               mafid = 'O' ) ).
*
*    lt_characteristics_assgmt_data = VALUE #(
*        ( klart = '001'
*          objek = iv_matnr
*          adzhl = iv_assgmt_number
*          datuv = COND #( WHEN iv_valid_from IS SUPPLIED THEN iv_valid_from ELSE '00010101' )
*          datub = '99991231'
*          atinn = iv_atinn ) ).
*
*    mo_osql_env->insert_test_data( i_data = lt_class_data  ).
*    mo_osql_env->insert_test_data( i_data = lt_characteristics_assgmt_data ).
*  ENDMETHOD.
*
*  METHOD given_classification_data.
*    DATA lt_class_data           TYPE TABLE OF klah.
*    DATA lt_characteristics_data TYPE TABLE OF ksml.
*
*    lt_class_data = VALUE #( statu = '1'
*                             vdatu = '20220101'
*                             vondt = '00010101'
*                             bisdt = '99991231'
*                             klart = '001'
*                             ( clint = iv_class_id
*                               class = iv_class_name )
*                             ( clint = '2' class = 'TEST_CL_01'  ) ).
*
*    lt_characteristics_data = VALUE #( adzhl = 0001
*                                       klart = '001'
*                                       posnr = 001
*                                       lkenz = ''
*                                       ( clint = iv_class_id imerk = 0000000001 )
*                                       ( clint = '2' imerk = 0000000002 ) ).
*
*    mo_osql_env->insert_test_data( i_data = lt_class_data  ).
*    mo_osql_env->insert_test_data( i_data = lt_characteristics_data ).
*  ENDMETHOD.
*
*  METHOD given_inob_data.
*    mo_osql_env->get_double( 'inob' )->insert( cl_osql_test_data=>create( it_inob_data ) ).
*  ENDMETHOD.
*
*  METHOD given_tcla_data.
*    mo_osql_env->get_double( 'tcla' )->insert( cl_osql_test_data=>create( it_tcla_data ) ).
*  ENDMETHOD.
*
*  METHOD map_hu_data.
*    DATA lt_handling_units     TYPE if_surdp_uph_entity_hu_proc=>gty_t_hu_for_product.
*    DATA lt_packcomp_data      TYPE if_surdp_uph_entity_hu_proc=>gty_t_api_rp_packcomp_for_prod.
*    DATA lo_actual_entity_data TYPE REF TO cl_surdp_uph_ent_pckg_cmp_hdr.
*    DATA lt_act_pcmp_items     TYPE surdpt_uph_entity_data.
*
*    given_classification_assgmt( iv_class_id      = '1'
*                                 iv_matnr         = 'TEST_MAT_HU'
*                                 iv_assgmt_number = 0001
*                                 iv_atinn         = 0000000001
*                                 iv_valid_from    = '00010101'  ).
*
*    lt_packcomp_data = VALUE #( ( selection_criteria-product_id                 = 'ZHU_TEST_PRODUCT'
*                                  selection_criteria-business_process_direction = 'ALL'
*                                  selection_criteria-supplier_id                = ''
*                                  packaging_compositions                        = VALUE #(
*                                      ( supplier_id                = ''
*                                        business_process_direction = 'ALL'
*                                        id                         = 'ZHU_TEST_PRODUCT-20240101'
*                                        valid_from                 = '20240101'
*                                        valid_to                   = '99991231' ) ) ) ).
*
*    lt_handling_units = VALUE #(
*        ( product = 'ZHU_TEST_PRODUCT' handling_unit = 'TEST_MAT_HU' amount = '0.5' base_unit = 'EA' prd_amount = '0'  ) ).
*
*    " when
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_hu_proc~map_hu_data(
*                                      it_packcomp_data  = lt_packcomp_data
*                                      it_handling_units = lt_handling_units ).
*
*    " then
*    lo_actual_entity_data = th_surdp_uph_packaging_comp=>then_assert_single_pcmp_header(
*                                it_entity_data = lt_actual_entity_data ).
*    th_surdp_uph_packaging_comp=>then_assert_header_data( io_actual_entiy_data = lo_actual_entity_data
*                                                          iv_display_id        = 'ZHU_TEST_PRODUCT-20240101' ).
*
*    lt_act_pcmp_items = lo_actual_entity_data->get_items( ).
*    cl_abap_unit_assert=>assert_equals( exp = 1
*                                        act = lines( lt_act_pcmp_items ) ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 1
*                                                        iv_packelem_displayid = 'TEST_MAT_HU'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.5'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_actual_entity_data
*                                                           iv_product_id        = 'ZHU_TEST_PRODUCT'
*                                                           iv_valid_from        = '20240101'
*                                                           iv_valid_to          = '99991231' ).
*  ENDMETHOD.
*
*  METHOD map_hu_data_empty.
*
*    "when
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_hu_proc~map_hu_data( it_handling_units = VALUE #(  ) it_packcomp_data = VALUE #(  ) ).
*
*    "then
*    cl_abap_unit_assert=>assert_initial( lt_actual_entity_data ).
*
*  ENDMETHOD.
*
*  METHOD map_hu_data_multiple.
*    DATA lt_handling_units     TYPE if_surdp_uph_entity_hu_proc=>gty_t_hu_for_product.
*    DATA lt_packcomp_data      TYPE if_surdp_uph_entity_hu_proc=>gty_t_api_rp_packcomp_for_prod.
*    DATA lo_actual_entity_data TYPE REF TO cl_surdp_uph_ent_pckg_cmp_hdr.
*    DATA lt_act_pcmp_items     TYPE surdpt_uph_entity_data.
*
*    " given: setup 4 handling unit materials and 2 products
*    given_classification_assgmt( iv_class_id      = '1'
*                                 iv_matnr         = 'TEST_MAT_HU_1'
*                                 iv_assgmt_number = 0001
*                                 iv_atinn         = 0000000001
*                                 iv_valid_from    = '00010101'  ).
*
*    given_classification_assgmt( iv_class_id      = '1'
*                                 iv_matnr         = 'TEST_MAT_HU_2'
*                                 iv_assgmt_number = 0001
*                                 iv_atinn         = 0000000001
*                                 iv_valid_from    = '00010101'  ).
*
*    given_classification_assgmt( iv_class_id      = '1'
*                                 iv_matnr         = 'TEST_MAT_HU_3'
*                                 iv_assgmt_number = 0001
*                                 iv_atinn         = 0000000001
*                                 iv_valid_from    = '00010101'  ).
*
*    given_classification_assgmt( iv_class_id      = '1'
*                                 iv_matnr         = 'TEST_MAT_HU_4'
*                                 iv_assgmt_number = 0001
*                                 iv_atinn         = 0000000001
*                                 iv_valid_from    = '00010101'  ).
*
*    lt_packcomp_data = VALUE #( selection_criteria-business_process_direction = 'ALL'
*                                selection_criteria-supplier_id                = ''
*                                ( selection_criteria-product_id = 'ZHU_TEST_PRODUCT_1'
*                                  packaging_compositions        = VALUE #(
*                                      ( supplier_id                = ''
*                                        business_process_direction = 'ALL'
*                                        id                         = 'ZHU_TEST_PRODUCT_1-20240101'
*                                        valid_from                 = '20240101'
*                                        valid_to                   = '99991231' ) ) )
*                                ( selection_criteria-product_id = 'ZHU_TEST_PRODUCT_2'
*                                  packaging_compositions        = VALUE #(
*                                      ( supplier_id                = ''
*                                        business_process_direction = 'ALL'
*                                        id                         = 'ZHU_TEST_PRODUCT_2-20240101'
*                                        valid_from                 = '20240101'
*                                        valid_to                   = '99991231' ) ) )         ).
*
*    lt_handling_units = VALUE #( base_unit        = 'EA'
*                                 prd_amount = '0'
*                                 ( product = 'ZHU_TEST_PRODUCT_1' handling_unit = 'TEST_MAT_HU_1' amount = '0.5'  )
*                                 ( product = 'ZHU_TEST_PRODUCT_1' handling_unit = 'TEST_MAT_HU_2' amount = '0.6'  )
*                                 ( product = 'ZHU_TEST_PRODUCT_1' handling_unit = 'TEST_MAT_HU_3' amount = '0.7'  )
*                                 ( product = 'ZHU_TEST_PRODUCT_1' handling_unit = 'TEST_MAT_HU_4' amount = '0.8'  )
*                                 ( product = 'ZHU_TEST_PRODUCT_2' handling_unit = 'TEST_MAT_HU_1' amount = '0.5'  )
*                                 ( product = 'ZHU_TEST_PRODUCT_2' handling_unit = 'TEST_MAT_HU_2' amount = '0.6'  )
*                                 ( product = 'ZHU_TEST_PRODUCT_2' handling_unit = 'TEST_MAT_HU_3' amount = '0.7'  )
*                                 ( product = 'ZHU_TEST_PRODUCT_2' handling_unit = 'TEST_MAT_HU_4' amount = '0.8'  ) ).
*
*    " when
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_hu_proc~map_hu_data(
*                                      it_handling_units = lt_handling_units
*                                      it_packcomp_data  = lt_packcomp_data ).
*
*    " then
*    cl_abap_unit_assert=>assert_equals( exp = 2
*                                        act = lines( lt_actual_entity_data ) ).
*
*    lo_actual_entity_data ?= lt_actual_entity_data[ 1 ].
*    th_surdp_uph_packaging_comp=>then_assert_header_data( io_actual_entiy_data = lo_actual_entity_data
*                                                          iv_display_id        = 'ZHU_TEST_PRODUCT_1-20240101' ).
*
*    lt_act_pcmp_items = lo_actual_entity_data->get_items( ).
*    cl_abap_unit_assert=>assert_equals( exp = 4
*                                        act = lines( lt_act_pcmp_items ) ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 1
*                                                        iv_packelem_displayid = 'TEST_MAT_HU_1'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.5'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 2
*                                                        iv_packelem_displayid = 'TEST_MAT_HU_2'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.6'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 3
*                                                        iv_packelem_displayid = 'TEST_MAT_HU_3'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.7'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 4
*                                                        iv_packelem_displayid = 'TEST_MAT_HU_4'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.8'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_actual_entity_data
*                                                           iv_product_id        = 'ZHU_TEST_PRODUCT_1'
*                                                           iv_valid_from        = '20240101'
*                                                           iv_valid_to          = '99991231' ).
*
*        lo_actual_entity_data ?= lt_actual_entity_data[ 2 ].
*    th_surdp_uph_packaging_comp=>then_assert_header_data( io_actual_entiy_data = lo_actual_entity_data
*                                                          iv_display_id        = 'ZHU_TEST_PRODUCT_2-20240101' ).
*
*    lt_act_pcmp_items = lo_actual_entity_data->get_items( ).
*    cl_abap_unit_assert=>assert_equals( exp = 4
*                                        act = lines( lt_act_pcmp_items ) ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 1
*                                                        iv_packelem_displayid = 'TEST_MAT_HU_1'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.5'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 2
*                                                        iv_packelem_displayid = 'TEST_MAT_HU_2'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.6'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 3
*                                                        iv_packelem_displayid = 'TEST_MAT_HU_3'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.7'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 4
*                                                        iv_packelem_displayid = 'TEST_MAT_HU_4'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.8'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_actual_entity_data
*                                                           iv_product_id        = 'ZHU_TEST_PRODUCT_2'
*                                                           iv_valid_from        = '20240101'
*                                                           iv_valid_to          = '99991231' ).
*
*  ENDMETHOD.
*
*  METHOD map_hu_data_multobj_active.
*    DATA lt_handling_units     TYPE if_surdp_uph_entity_hu_proc=>gty_t_hu_for_product.
*    DATA lt_packcomp_data      TYPE if_surdp_uph_entity_hu_proc=>gty_t_api_rp_packcomp_for_prod.
*    DATA lo_actual_entity_data TYPE REF TO cl_surdp_uph_ent_pckg_cmp_hdr.
*    DATA lt_act_pcmp_items     TYPE surdpt_uph_entity_data.
*
*    given_tcla_data( VALUE #( ( multobj = abap_true klart = '001' ) ) ).
*
*    given_inob_data( VALUE #( ( cuobj = '000000000016233647' objek = 'ZHU_TEST_PRODUCT' klart = '001' obtab = 'MARA' ) ) ).
*
*    given_classification_assgmt( iv_class_id      = '1'
*                                 iv_matnr         = '000000000016233647'
*                                 iv_assgmt_number = 0001
*                                 iv_atinn         = 0000000001
*                                 iv_valid_from    = '00010101'  ).
*
*    lt_packcomp_data = VALUE #( ( selection_criteria-product_id                = 'ZHU_TEST_PRODUCT'
*                                  selection_criteria-business_process_direction = 'ALL'
*                                  selection_criteria-supplier_id                = ''
*                                  packaging_compositions                        = VALUE #(
*                                      ( supplier_id                = ''
*                                        business_process_direction = 'ALL'
*                                        id                         = 'ZHU_TEST_PRODUCT-20240101'
*                                        valid_from                 = '20240101'
*                                        valid_to                   = '99991231' ) ) ) ).
*
*    lt_handling_units = VALUE #(
*        ( product = 'ZHU_TEST_PRODUCT' handling_unit = 'TEST_MAT_HU' amount = '0.5' base_unit = 'EA' prd_amount = '0'  ) ).
*
*    " when
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_hu_proc~map_hu_data(
*                                      it_packcomp_data  = lt_packcomp_data
*                                      it_handling_units = lt_handling_units ).
*
*    " then
*    lo_actual_entity_data = th_surdp_uph_packaging_comp=>then_assert_single_pcmp_header(
*                                it_entity_data = lt_actual_entity_data ).
*    th_surdp_uph_packaging_comp=>then_assert_header_data( io_actual_entiy_data = lo_actual_entity_data
*                                                          iv_display_id        = 'ZHU_TEST_PRODUCT-20240101' ).
*
*    lt_act_pcmp_items = lo_actual_entity_data->get_items( ).
*    cl_abap_unit_assert=>assert_equals( exp = 1
*                                        act = lines( lt_act_pcmp_items ) ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = 1
*                                                        iv_packelem_displayid = 'TEST_MAT_HU'
*                                                        iv_packelem_version   = '0001-01-01'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.5'
*                                                        iv_quantity_uom       = 'EA'
*                                                        iv_count              = '1'
*                                                        iv_usage              = 'H'
*                                                        iv_eprgroup           = ''
*                                                        iv_wwfgroup           = '' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_actual_entity_data
*                                                           iv_product_id        = 'ZHU_TEST_PRODUCT'
*                                                           iv_valid_from        = '20240101'
*                                                           iv_valid_to          = '99991231' ).
*  ENDMETHOD.
*
*ENDCLASS.
