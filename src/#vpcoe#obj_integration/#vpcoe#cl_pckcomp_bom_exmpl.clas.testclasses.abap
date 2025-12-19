*CLASS ltc_pack_comp_bom_example DEFINITION FOR TESTING RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    CLASS-DATA: mo_osql_env TYPE REF TO if_osql_test_environment.
*
*    CLASS-METHODS: class_setup.
*    CLASS-METHODS: class_teardown.
*
*    METHODS setup.
*
*    METHODS given_bom_header IMPORTING iv_bom_matnr           TYPE matnr
*                                       iv_bom_stlnr           TYPE stnum
*                                       iv_basequantity        TYPE p
*                                       iv_baseunitofmeasureid TYPE meins
*                                       iv_consumersalesunit   TYPE meins
*                                       ir_bom_items           TYPE REF TO surdpt_uph_wrap_bom_item
*                             RETURNING VALUE(rv_result)       TYPE REF TO cl_surdp_uph_wrap_bom_hdr.
*
*    METHODS given_bom_item IMPORTING iv_bom_item_matnr    TYPE matnr
*                                     iv_bom_item_mtart    TYPE mtart
*                                     iv_bom_item_ojtxp    TYPE ojtxp
*                                     iv_bom_item_menge    TYPE kmpmg
*                                     iv_bom_item_category TYPE psitp
*                           RETURNING VALUE(rv_result)     TYPE REF TO cl_surdp_uph_wrap_bom_item.
*
*    METHODS given_bom_item_with_sub_items IMPORTING iv_bom_item_matnr    TYPE matnr
*                                                    iv_bom_item_mtart    TYPE mtart
*                                                    iv_bom_item_ojtxp    TYPE ojtxp
*                                                    iv_bom_item_menge    TYPE kmpmg
*                                                    iv_bom_item_category TYPE psitp
*                                                    ir_sub_items         TYPE REF TO surdpt_uph_wrap_bom_item
*                                          RETURNING VALUE(rv_result)     TYPE REF TO cl_surdp_uph_wrap_bom_item.
*
*    METHODS given_classification_data IMPORTING
*                                        iv_class_id   TYPE clint
*                                        iv_class_name TYPE klasse_d.
*
*    METHODS given_classification_assgmt IMPORTING
*                                          iv_bom_item_matnr TYPE cuobn
*                                          iv_class_id       TYPE clint.
*
*    METHODS map_bom_data                 FOR TESTING.
*    METHODS map_bom_data_multiple        FOR TESTING.
*    METHODS map_bom_data_multiple_levels FOR TESTING.
*    METHODS map_bom_data_empty_input     FOR TESTING.
*    METHODS map_bom_data_empty_item      FOR TESTING.
*
*    DATA mo_cut TYPE REF TO cl_surdp_uph_pckcomp_bom_exmpl.
*ENDCLASS.
*
*CLASS ltc_pack_comp_bom_example IMPLEMENTATION.
*
*  METHOD class_setup.
*
*    mo_osql_env = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'klah' )
*                                                                                 ( 'kssk' ) ) ).
*
*  ENDMETHOD.
*
*  METHOD class_teardown.
*
*    mo_osql_env->destroy(  ).
*
*  ENDMETHOD.
*
*  METHOD setup.
*
*    mo_osql_env->clear_doubles(  ).
*
*    th_surdp_uph_factory_injector=>inject_factory_double( io_double = NEW td_surdp_uph_factory( ) ).
*
*    mo_cut = NEW cl_surdp_uph_pckcomp_bom_exmpl( ).
*
*    given_classification_data( iv_class_id = '1' iv_class_name = 'ZMCL_PACKELEM_ATTR' ).
*
*  ENDMETHOD.
*
*  METHOD map_bom_data.
*    DATA lt_bom_data  TYPE surdpt_uph_wrap_bom_hdr.
*    DATA lr_bom_items TYPE REF TO surdpt_uph_wrap_bom_item.
*    DATA lr_sub_items TYPE REF TO surdpt_uph_wrap_bom_item.
*
*    lr_bom_items = NEW #( ).
*    lr_sub_items = NEW #( ).
*
*    " Given
*    th_surdp_uph_packaging_comp=>given_input_params( io_cut = mo_cut iv_source_id = 'SOURCE_ID' ).
*    DATA(lo_bom_header) = given_bom_header( iv_bom_matnr           = 'RDP-BOM-1'
*                                            iv_bom_stlnr           = '347897'
*                                            iv_basequantity        = 1
*                                            iv_baseunitofmeasureid = 'PC'
*                                            iv_consumersalesunit   = 'ST'
*                                            ir_bom_items           = lr_bom_items ).
*    APPEND lo_bom_header TO lt_bom_data.
*
*    DATA(lo_bom_item1) = given_bom_item( iv_bom_item_matnr = 'RDP-PE-WRAP' iv_bom_item_ojtxp = 'Wrap' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*    given_classification_assgmt( iv_class_id = '1' iv_bom_item_matnr = lo_bom_item1->get_material(  ) ).
*    DATA(lo_bom_item2) = given_bom_item( iv_bom_item_matnr = 'RDP-BM-MOTOR' iv_bom_item_ojtxp = 'Motor 50W' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*
*    DATA(lo_bom_sub_item1) = given_bom_item( iv_bom_item_matnr = 'RDP-BM-BATTERY' iv_bom_item_ojtxp = 'Battery' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*    DATA(lo_bom_sub_item2) = given_bom_item( iv_bom_item_matnr = 'RDP-PE-LABEL' iv_bom_item_ojtxp = 'Label' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 2 iv_bom_item_category = 'L' ).
*    given_classification_assgmt( iv_class_id = '1' iv_bom_item_matnr = lo_bom_sub_item2->get_material(  ) ).
*    APPEND lo_bom_sub_item1 TO lr_sub_items->*.
*    APPEND lo_bom_sub_item2 TO lr_sub_items->*.
*    DATA(lo_bom_item3) = given_bom_item_with_sub_items( iv_bom_item_matnr = 'RDP-PC-001' iv_bom_item_ojtxp = 'PC-001' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 2 ir_sub_items = lr_sub_items iv_bom_item_category = 'L' ).
*
*    APPEND lo_bom_item1 TO lr_bom_items->*.
*    APPEND lo_bom_item2 TO lr_bom_items->*.
*    APPEND lo_bom_item3 TO lr_bom_items->*.
*
*    " When
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_bom_proc~map_bom_data( lt_bom_data ).
*
*    " Then
*    DATA(lo_actual_entiy_data) = th_surdp_uph_packaging_comp=>then_assert_single_pcmp_header( lt_actual_entity_data ).
*    th_surdp_uph_packaging_comp=>then_assert_header_data( io_actual_entiy_data   = lo_actual_entiy_data
*                                                          iv_source_id           = 'SOURCE_ID'
*                                                          iv_display_id          = 'RDP-BOM-1-0001-1-1'
*                                                          iv_description         = 'RDP_BM_PUMP'
*                                                          iv_basequantity        = 1
*                                                          iv_baseunitofmeasureid = 'PC'
*                                                          iv_consumersalesunit   = 'ST' ).
*
*    DATA(lt_act_pcmp_items) = lo_actual_entiy_data->get_items(  ).
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_act_pcmp_items ) exp = 2 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entiy_data
*                                                        iv_item_no            = 1
*                                                        iv_packelem_displayid = 'RDP-PE-WRAP'
*                                                        iv_levelcode          = '10'
*                                                        iv_quantity           = 1
*                                                        iv_quantity_uom       = 'KG'
*                                                        iv_count              = 0 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entiy_data
*                                                        iv_item_no            = 2
*                                                        iv_packelem_displayid = 'RDP-PE-LABEL'
*                                                        iv_levelcode          = '10'
*                                                        iv_quantity           = 4
*                                                        iv_quantity_uom       = 'KG'
*                                                        iv_count              = 0 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_actual_entiy_data
*                                                           iv_product_id        = 'RDP-BOM-1'
*                                                           iv_valid_from        = '00000000'
*                                                           iv_valid_to          = '99991231' ).
*
*  ENDMETHOD.
*
*
*
*  METHOD map_bom_data_multiple.
*
*    DATA lt_bom_data  TYPE surdpt_uph_wrap_bom_hdr.
*    DATA lr_bom_items TYPE REF TO surdpt_uph_wrap_bom_item.
*    DATA lr_sub_items TYPE REF TO surdpt_uph_wrap_bom_item.
*
*    lr_bom_items = NEW #( ).
*    lr_sub_items = NEW #( ).
*
*    " Given 2 main BOMs with several sub-BOMs
*    th_surdp_uph_packaging_comp=>given_input_params( io_cut = mo_cut iv_source_id = 'SOURCE_ID' ).
*
*    DATA(lo_bom_header) = given_bom_header( iv_bom_matnr           = 'RDP-BOM-1'
*                                            iv_bom_stlnr           = '347897'
*                                            iv_basequantity        = 1
*                                            iv_baseunitofmeasureid = 'PC'
*                                            iv_consumersalesunit   = 'ST'
*                                            ir_bom_items           = lr_bom_items ).
*    APPEND lo_bom_header TO lt_bom_data.
*
*    DATA(lo_bom_item1) = given_bom_item( iv_bom_item_matnr = 'RDP-PE-WRAP' iv_bom_item_ojtxp = 'Wrap' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*    given_classification_assgmt( iv_class_id = '1' iv_bom_item_matnr = lo_bom_item1->get_material(  ) ).
*    DATA(lo_bom_item2) = given_bom_item( iv_bom_item_matnr = 'RDP-BM-MOTOR' iv_bom_item_ojtxp = 'Motor 50W' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*
*    DATA(lo_bom_sub_item1) = given_bom_item( iv_bom_item_matnr = 'RDP-BM-BATTERY' iv_bom_item_ojtxp = 'Battery' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*    DATA(lo_bom_sub_item2) = given_bom_item( iv_bom_item_matnr = 'RDP-PE-LABEL' iv_bom_item_ojtxp = 'Label' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 2 iv_bom_item_category = 'L' ).
*    given_classification_assgmt( iv_class_id = '1' iv_bom_item_matnr = lo_bom_sub_item2->get_material(  ) ).
*    APPEND lo_bom_sub_item1 TO lr_sub_items->*.
*    APPEND lo_bom_sub_item2 TO lr_sub_items->*.
*    DATA(lo_bom_item3) = given_bom_item_with_sub_items( iv_bom_item_matnr = 'RDP-PC-001' iv_bom_item_ojtxp = 'PC-001' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 2 ir_sub_items = lr_sub_items iv_bom_item_category = 'L' ).
*
*    DATA(lo_bom_item4) = given_bom_item( iv_bom_item_matnr = 'DOC-001' iv_bom_item_ojtxp = 'Some document' iv_bom_item_mtart = 'DOC' iv_bom_item_menge = 1 iv_bom_item_category = 'D' ).
*
*    APPEND lo_bom_item1 TO lr_bom_items->*.
*    APPEND lo_bom_item2 TO lr_bom_items->*.
*    APPEND lo_bom_item3 TO lr_bom_items->*.
*    APPEND lo_bom_item4 TO lr_bom_items->*.
*
*    CLEAR: lo_bom_header, lo_bom_item1, lo_bom_item2, lo_bom_item3, lo_bom_sub_item1, lo_bom_sub_item2, lr_bom_items, lr_sub_items.
*
*    lr_bom_items = NEW #( ).
*    lr_sub_items = NEW #( ).
*
*    lo_bom_header = given_bom_header( iv_bom_matnr           = 'RDP-BOM-2'
*                                      iv_bom_stlnr           = '444555'
*                                      iv_basequantity        = 1
*                                      iv_baseunitofmeasureid = 'PC'
*                                      iv_consumersalesunit   = 'ST'
*                                      ir_bom_items           = lr_bom_items ).
*
*    APPEND lo_bom_header TO lt_bom_data.
*
*    lo_bom_item1 = given_bom_item( iv_bom_item_matnr = 'RDP-WDSCREW' iv_bom_item_ojtxp = 'Woodscrew' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*    lo_bom_item2 = given_bom_item( iv_bom_item_matnr = 'SOME_ITEM'   iv_bom_item_ojtxp = 'Item with no relevant material class' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*
*
*    lo_bom_sub_item1 = given_bom_item( iv_bom_item_matnr = 'RDP-BM-MOTOR' iv_bom_item_ojtxp = 'MOTOR 50W' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*    lo_bom_sub_item2 = given_bom_item( iv_bom_item_matnr = 'RDP-PE-LABEL' iv_bom_item_ojtxp = 'Label' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 2 iv_bom_item_category = 'L' ).
*
*
*    APPEND lo_bom_sub_item1 TO lr_sub_items->*.
*    APPEND lo_bom_sub_item2 TO lr_sub_items->*.
*
*    lo_bom_item3 = given_bom_item_with_sub_items( iv_bom_item_matnr = 'RDP-PC-002' iv_bom_item_ojtxp = 'PC-002' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 3 ir_sub_items = lr_sub_items iv_bom_item_category = 'L' ).
*
*    APPEND lo_bom_item1 TO lr_bom_items->*.
*    APPEND lo_bom_item2 TO lr_bom_items->*.
*    APPEND lo_bom_item3 TO lr_bom_items->*.
*
*    " When
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_bom_proc~map_bom_data( lt_bom_data ).
*
*    " Then
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_actual_entity_data ) exp = 2 ).
*
*    DATA lo_first_pcmp_hdr_data TYPE REF TO cl_surdp_uph_ent_pckg_cmp_hdr.
*    lo_first_pcmp_hdr_data ?= lt_actual_entity_data[ 1 ].
*
*    DATA(lt_act_pcmp_items) = lo_first_pcmp_hdr_data->get_items(  ).
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_act_pcmp_items ) exp = 2 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_header_data( io_actual_entiy_data   = lo_first_pcmp_hdr_data
*                                                          iv_source_id           = 'SOURCE_ID'
*                                                          iv_display_id          = 'RDP-BOM-1-0001-1-1'
*                                                          iv_description         = 'RDP_BM_PUMP'
*                                                          iv_basequantity        = 1
*                                                          iv_baseunitofmeasureid = 'PC'
*                                                          iv_consumersalesunit   = 'ST' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_first_pcmp_hdr_data
*                                                        iv_item_no            = 1
*                                                        iv_packelem_displayid = 'RDP-PE-WRAP'
*                                                        iv_levelcode          = '10'
*                                                        iv_quantity           = 1
*                                                        iv_quantity_uom       = 'KG'
*                                                        iv_count              = 0 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_first_pcmp_hdr_data
*                                                        iv_item_no            = 2
*                                                        iv_packelem_displayid = 'RDP-PE-LABEL'
*                                                        iv_levelcode          = '10'
*                                                        iv_quantity           = 4
*                                                        iv_quantity_uom       = 'KG'
*                                                        iv_count              = 0 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_first_pcmp_hdr_data
*                                                           iv_product_id        = 'RDP-BOM-1'
*                                                           iv_valid_from        = '00000000'
*                                                           iv_valid_to          = '99991231' ).
*
*    DATA lo_second_pcmp_hdr_data TYPE REF TO cl_surdp_uph_ent_pckg_cmp_hdr.
*    lo_second_pcmp_hdr_data ?= lt_actual_entity_data[ 2 ].
*
*    lt_act_pcmp_items = lo_second_pcmp_hdr_data->get_items(  ).
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_act_pcmp_items ) exp = 1 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_header_data( io_actual_entiy_data   = lo_second_pcmp_hdr_data
*                                                          iv_source_id           = 'SOURCE_ID'
*                                                          iv_display_id          = 'RDP-BOM-2-0001-1-1'
*                                                          iv_description         = 'RDP_BM_PUMP'
*                                                          iv_basequantity        = 1
*                                                          iv_baseunitofmeasureid = 'PC'
*                                                          iv_consumersalesunit   = 'ST' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_second_pcmp_hdr_data
*                                                        iv_item_no            = 1
*                                                        iv_packelem_displayid = 'RDP-PE-LABEL'
*                                                        iv_levelcode          = '10'
*                                                        iv_quantity           = 6
*                                                        iv_quantity_uom       = 'KG'
*                                                        iv_count              = 0 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_second_pcmp_hdr_data
*                                                           iv_product_id        = 'RDP-BOM-2'
*                                                           iv_valid_from        = '00000000'
*                                                           iv_valid_to          = '99991231' ).
*
*  ENDMETHOD.
*
*
*
*
*  METHOD map_bom_data_multiple_levels.
*    DATA lt_bom_data  TYPE surdpt_uph_wrap_bom_hdr.
*    DATA lr_bom_items TYPE REF TO surdpt_uph_wrap_bom_item.
*    DATA lr_sub_items TYPE REF TO surdpt_uph_wrap_bom_item.
*
*    lr_bom_items = NEW #( ).
*    lr_sub_items = NEW #( ).
*
*    " Given
*    th_surdp_uph_packaging_comp=>given_input_params( io_cut = mo_cut iv_source_id = 'SOURCE_ID' ).
*    DATA(lo_bom_header) = given_bom_header( iv_bom_matnr           = 'RDP-BOM-1'
*                                            iv_bom_stlnr           = '347897'
*                                            iv_basequantity        = 1
*                                            iv_baseunitofmeasureid = 'PC'
*                                            iv_consumersalesunit   = 'ST'
*                                            ir_bom_items           = lr_bom_items ).
*    APPEND lo_bom_header TO lt_bom_data.
*
*    DATA(lo_bom_item1) = given_bom_item( iv_bom_item_matnr = 'RDP-PE-WRAP' iv_bom_item_ojtxp = 'Wrap' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*    given_classification_assgmt( iv_class_id = '1' iv_bom_item_matnr = lo_bom_item1->get_material(  ) ).
*    DATA(lo_bom_item2) = given_bom_item( iv_bom_item_matnr = 'RDP-BM-MOTOR' iv_bom_item_ojtxp = 'Motor 50W' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 1 iv_bom_item_category = 'L' ).
*
*    DATA(lo_sub_item_1) = given_bom_item( iv_bom_item_matnr = 'RDP-PE-WDSCREW' iv_bom_item_ojtxp = 'Woodscrew' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 3  iv_bom_item_category = 'L' ).
*    given_classification_assgmt( iv_class_id = '1' iv_bom_item_matnr = lo_sub_item_1->get_material(  ) ).
*
*    APPEND lo_sub_item_1 TO lr_sub_items->*.
*
*    DATA(lo_sub_item_2) = given_bom_item_with_sub_items( iv_bom_item_matnr = 'RDP-PE-TEST' iv_bom_item_ojtxp = 'Item with sub items' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 2 iv_bom_item_category = 'L' ir_sub_items = lr_sub_items ).
*
*    lr_sub_items = NEW #(  ).
*    APPEND lo_sub_item_2 TO lr_sub_items->*.
*
*    DATA(lo_bom_sub_item3) = given_bom_item_with_sub_items( iv_bom_item_matnr = 'RDP-BM-BATTERY' iv_bom_item_ojtxp = 'Battery' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 2 iv_bom_item_category = 'L' ir_sub_items = lr_sub_items ).
*
*    DATA(lo_bom_sub_item4) = given_bom_item( iv_bom_item_matnr = 'RDP-PE-LABEL' iv_bom_item_ojtxp = 'Label' iv_bom_item_mtart = 'VERP' iv_bom_item_menge = 2 iv_bom_item_category = 'L' ).
*    given_classification_assgmt( iv_class_id = '1' iv_bom_item_matnr = lo_bom_sub_item4->get_material(  ) ).
*
*    lr_sub_items = NEW #(  ).
*    APPEND lo_bom_sub_item3 TO lr_sub_items->*.
*    APPEND lo_bom_sub_item4 TO lr_sub_items->*.
*
*    DATA(lo_bom_item3) = given_bom_item_with_sub_items( iv_bom_item_matnr = 'RDP-PC-001' iv_bom_item_ojtxp = 'PC-001' iv_bom_item_mtart = 'HALB' iv_bom_item_menge = 2 ir_sub_items = lr_sub_items iv_bom_item_category = 'L' ).
*
*    APPEND lo_bom_item1 TO lr_bom_items->*.
*    APPEND lo_bom_item2 TO lr_bom_items->*.
*    APPEND lo_bom_item3 TO lr_bom_items->*.
*
*    " When
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_bom_proc~map_bom_data( lt_bom_data ).
*
*    " Then
*    DATA(lo_actual_entiy_data) = th_surdp_uph_packaging_comp=>then_assert_single_pcmp_header( lt_actual_entity_data ).
*    th_surdp_uph_packaging_comp=>then_assert_header_data( io_actual_entiy_data   = lo_actual_entiy_data
*                                                          iv_source_id           = 'SOURCE_ID'
*                                                          iv_display_id          = 'RDP-BOM-1-0001-1-1'
*                                                          iv_description         = 'RDP_BM_PUMP'
*                                                          iv_basequantity        = 1
*                                                          iv_baseunitofmeasureid = 'PC'
*                                                          iv_consumersalesunit   = 'ST' ).
*
*    DATA(lt_act_pcmp_items) = lo_actual_entiy_data->get_items(  ).
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_act_pcmp_items ) exp = 3 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entiy_data
*                                                        iv_item_no            = 1
*                                                        iv_packelem_displayid = 'RDP-PE-WRAP'
*                                                        iv_levelcode          = '10'
*                                                        iv_quantity           = 1
*                                                        iv_quantity_uom       = 'KG'
*                                                        iv_count              = 0 ).
*
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entiy_data
*                                                        iv_item_no            = 2
*                                                        iv_packelem_displayid = 'RDP-PE-WDSCREW'
*                                                        iv_levelcode          = '10'
*                                                        iv_quantity           = 24
*                                                        iv_quantity_uom       = 'KG'
*                                                        iv_count              = 0 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entiy_data
*                                                        iv_item_no            = 3
*                                                        iv_packelem_displayid = 'RDP-PE-LABEL'
*                                                        iv_levelcode          = '10'
*                                                        iv_quantity           = 4
*                                                        iv_quantity_uom       = 'KG'
*                                                        iv_count              = 0 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_actual_entiy_data
*                                                           iv_product_id        = 'RDP-BOM-1'
*                                                           iv_valid_from        = '00000000'
*                                                           iv_valid_to          = '99991231' ).
*  ENDMETHOD.
*
*
*
*
*
*  METHOD map_bom_data_empty_input.
*    " Given
*    DATA lt_bom_data  TYPE surdpt_uph_wrap_bom_hdr.
*
*    " When
*    DATA(lo_actual_entity_data) = mo_cut->if_surdp_uph_entity_bom_proc~map_bom_data( lt_bom_data ).
*
*    " Then
*    cl_abap_unit_assert=>assert_initial( lo_actual_entity_data ).
*  ENDMETHOD.
*
*  METHOD map_bom_data_empty_item.
*
*    "Given
*    DATA lt_bom_data  TYPE surdpt_uph_wrap_bom_hdr.
*    DATA lr_bom_items TYPE REF TO surdpt_uph_wrap_bom_item.
*
*    DATA(lo_bom_header) = given_bom_header( iv_bom_matnr           = 'RDP-BOM-1'
*                                            iv_bom_stlnr           = '347897'
*                                            iv_basequantity        = 1
*                                            iv_baseunitofmeasureid = 'PC'
*                                            iv_consumersalesunit   = 'ST'
*                                            ir_bom_items           = lr_bom_items ).
*
*    APPEND lo_bom_header TO lt_bom_data.
*
*    " When
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_bom_proc~map_bom_data( lt_bom_data ).
*
*    " Then
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_actual_entity_data ) exp = 0 ).
*
*  ENDMETHOD.
*
*
*  METHOD given_bom_header.
*    DATA ls_header_material  TYPE cstmat.
*    DATA ls_material_general TYPE bapimatdoa.
*
*    ls_header_material = VALUE #( stlnr     = iv_bom_stlnr
*                                  stlal     = '1'
*                                  stlan     = '1'
*                                  bom_versn = '1'
*                                  aennr     = '234'
*                                  matnr     = iv_bom_matnr
*                                  maktx     = 'RDP_BM_PUMP'
*                                  werks     = '0001'
*                                  datuv     = '00000000'
*                                  datub     = '00000000'
*                                  bmeng     = iv_basequantity
*                                  bmein     = iv_baseunitofmeasureid
*                                  loekz     = ''
*                                  stlst     = '1'
*                                  stktx     = 'Alternative Text').
*
*    ls_material_general = VALUE #( matl_type = 'HALB' base_uom = iv_consumersalesunit matl_group = 'SEMIS' ).
*
*    " When
*    rv_result = NEW #( is_header_material  = ls_header_material
*                       ir_items            = ir_bom_items
*                       is_material_general = ls_material_general ).
*  ENDMETHOD.
*
*
*  METHOD given_bom_item.
*    DATA ls_bom_item_data TYPE stpox.
*
*    ls_bom_item_data = VALUE #( idnrk = iv_bom_item_matnr
*                                ojtxp = iv_bom_item_ojtxp
*                                matkl = ''
*                                mtart = iv_bom_item_mtart
*                                werks = '0001'
*                                xtlnr = '347897'
*                                xtlal = '1'
*                                xtlan = '1'
*                                xmein = 'PC'
*                                xmeng = 1
*                                datuv = '20220101'
*                                datub = '20221231'
*                                aennr = '234'
*                                menge = iv_bom_item_menge
*                                meins = 'KG'
*                                posnr = '0001'
*                                postp = iv_bom_item_category
*                                mmein = 'PC' ).
*
*    rv_result = NEW #( is_bom_item_data = ls_bom_item_data ).
*  ENDMETHOD.
*
*  METHOD given_bom_item_with_sub_items.
*    DATA ls_bom_item_data TYPE stpox.
*
*    ls_bom_item_data = VALUE #( idnrk = iv_bom_item_matnr
*                                ojtxp = iv_bom_item_ojtxp
*                                matkl = ''
*                                mtart = iv_bom_item_matnr
*                                werks = '0001'
*                                xtlnr = '347897'
*                                xtlal = '1'
*                                xtlan = '1'
*                                xmein = 'PC'
*                                xmeng = 1
*                                datuv = '20220101'
*                                datub = '20221231'
*                                aennr = '234'
*                                menge = iv_bom_item_menge
*                                meins = 'KG'
*                                posnr = '0001'
*                                postp = 'L'
*                                mmein = 'PC' ).
*
*    rv_result = NEW #( is_bom_item_data = ls_bom_item_data ir_items = ir_sub_items ).
*  ENDMETHOD.
*
*
*  METHOD given_classification_data.
*
*    DATA lt_class_data TYPE TABLE OF klah.
*
*    lt_class_data = VALUE #( (
*                                 clint = iv_class_id
*                                 class = iv_class_name
*                                 statu = '1'
*                                 vdatu = '20220101'
*                                 vondt = '20221212'
*                                 bisdt = '99991231'
*    ) ).
*
*    mo_osql_env->insert_test_data( i_data = lt_class_data  ).
*
*  ENDMETHOD.
*
*  METHOD given_classification_assgmt.
*
*    DATA lt_class_data TYPE TABLE OF kssk.
*
*    lt_class_data = VALUE #( (
*                                 clint = iv_class_id
*                                 objek = iv_bom_item_matnr
*                                 datuv = '00000101'
*                                 datub = '99991231'
*    ) ).
*
*    mo_osql_env->insert_test_data( i_data = lt_class_data  ).
*
*  ENDMETHOD.
*
*
*
*ENDCLASS.
*
*
*CLASS ltc_surdp_bom_exmpl_alternatv1 DEFINITION FOR TESTING RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    METHODS setup.
*
*    METHODS given_bom_header IMPORTING iv_bom_matnr           TYPE matnr
*                                       iv_basequantity        TYPE p
*                                       iv_baseunitofmeasureid TYPE meins
*                                       iv_consumersalesunit   TYPE meins
*                                       ir_bom_items           TYPE REF TO surdpt_uph_wrap_bom_item
*                             RETURNING VALUE(rv_result)       TYPE REF TO cl_surdp_uph_wrap_bom_hdr.
*
*    METHODS given_bom_item IMPORTING iv_bom_item_matnr TYPE matnr
*                           RETURNING VALUE(rv_result)  TYPE REF TO cl_surdp_uph_wrap_bom_item.
*
*    METHODS map_bom_data FOR TESTING.
*    METHODS map_bom_data_empty_input FOR TESTING.
*    DATA mo_cut TYPE REF TO lcl_surdp_bom_exmpl_alternatv1.
*ENDCLASS.
*
*
*
*CLASS ltc_surdp_bom_exmpl_alternatv1 IMPLEMENTATION.
*  METHOD setup.
*    th_surdp_uph_factory_injector=>inject_factory_double( io_double = NEW td_surdp_uph_factory( ) ).
*
*    mo_cut = NEW lcl_surdp_bom_exmpl_alternatv1( ).
*  ENDMETHOD.
*
*
*  METHOD map_bom_data.
*    DATA lt_bom_data  TYPE surdpt_uph_wrap_bom_hdr.
*    DATA lr_bom_items TYPE REF TO surdpt_uph_wrap_bom_item.
*
*    lr_bom_items = NEW #( ).
*
*    " Given
*    th_surdp_uph_packaging_comp=>given_input_params( io_cut = mo_cut iv_source_id = 'SOURCE_ID' ).
*    DATA(lo_bom_header) = given_bom_header( iv_bom_matnr           = 'MyBOM1'
*                                            iv_basequantity        = 8
*                                            iv_baseunitofmeasureid = 'PC'
*                                            iv_consumersalesunit   = 'ST'
*                                            ir_bom_items           = lr_bom_items ).
*    APPEND lo_bom_header TO lt_bom_data.
*
*    DATA(lo_bom_item1) = given_bom_item( 'RDP-PC-MOTOR' ).
*    DATA(lo_bom_item2) = given_bom_item( 'RDP-MOTOR-WRAP' ).
*    APPEND lo_bom_item1 TO lr_bom_items->*.
*    APPEND lo_bom_item2 TO lr_bom_items->*.
*
*    " When
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_bom_proc~map_bom_data( lt_bom_data ).
*
*    " Then
*    DATA(lo_actual_entiy_data) = th_surdp_uph_packaging_comp=>then_assert_single_pcmp_header( lt_actual_entity_data ).
*    th_surdp_uph_packaging_comp=>then_assert_header_data( io_actual_entiy_data   = lo_actual_entiy_data
*                                                          iv_source_id           = 'SOURCE_ID'
*                                                          iv_display_id          = 'MyBOM1-0001-1-1'
*                                                          iv_description         = 'RDP_BM_PUMP'
*                                                          iv_basequantity        = 8
*                                                          iv_baseunitofmeasureid = 'PC'
*                                                          iv_consumersalesunit   = 'ST' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entiy_data
*                                                        iv_item_no            = 1
*                                                        iv_packelem_displayid = 'RDP-MOTOR-WRAP'
*                                                        iv_quantity           = 6
*                                                        iv_quantity_uom       = 'KG'
*                                                        iv_count              = 0 ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_actual_entiy_data
*                                                           iv_product_id        = 'MyBOM1'
*                                                           iv_valid_from        = '00000000'
*                                                           iv_valid_to          = '99991231' ).
*  ENDMETHOD.
*
*
*  METHOD map_bom_data_empty_input.
*    " Given
*    DATA lt_bom_data  TYPE surdpt_uph_wrap_bom_hdr.
*
*    " When
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_bom_proc~map_bom_data( lt_bom_data ).
*
*    " Then
*    cl_abap_unit_assert=>assert_initial( lt_actual_entity_data ).
*  ENDMETHOD.
*
*
*  METHOD given_bom_header.
*    DATA ls_header_material  TYPE cstmat.
*    DATA ls_material_general TYPE bapimatdoa.
*
*    ls_header_material = VALUE #( stlnr     = '347897'
*                                  stlal     = '1'
*                                  stlan     = '1'
*                                  bom_versn = '1'
*                                  aennr     = '234'
*                                  matnr     = iv_bom_matnr
*                                  maktx     = 'RDP_BM_PUMP'
*                                  werks     = '0001'
*                                  datuv     = '00000000'
*                                  datub     = '00000000'
*                                  bmeng     = iv_basequantity
*                                  bmein     = iv_baseunitofmeasureid
*                                  loekz     = ''
*                                  stlst     = '1'
*                                  stktx     = 'Alternative Text').
*
*    ls_material_general = VALUE #( matl_type = 'HALB' base_uom = iv_consumersalesunit matl_group = 'SEMIS' ).
*
*    " When
*    rv_result = NEW #( is_header_material  = ls_header_material
*                       ir_items            = ir_bom_items
*                       is_material_general = ls_material_general ).
*  ENDMETHOD.
*
*
*  METHOD given_bom_item.
*    DATA ls_bom_item_data TYPE stpox.
*
*    ls_bom_item_data = VALUE #( idnrk = iv_bom_item_matnr
*                                ojtxp = 'MOTORS SIZE S'
*                                matkl = ''
*                                mtart = 'VERP'
*                                werks = '0001'
*                                xtlnr = '347897'
*                                xtlal = '1'
*                                xtlan = '1'
*                                xmein = 'PC'
*                                xmeng = 1
*                                datuv = '20220101'
*                                datub = '20221231'
*                                aennr = '234'
*                                menge = 6
*                                meins = 'KG'
*                                posnr = '0001'
*                                postp = 'L'
*                                mmein = 'PC' ).
*
*    rv_result = NEW #( is_bom_item_data = ls_bom_item_data ).
*  ENDMETHOD.
*ENDCLASS.
