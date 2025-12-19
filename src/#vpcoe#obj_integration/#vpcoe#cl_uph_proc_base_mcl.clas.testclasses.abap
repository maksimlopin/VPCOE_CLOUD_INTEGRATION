**"* use this source file for your ABAP unit test classes
*CLASS th_surdp_uph_proc_base_mcl_imp DEFINITION FOR TESTING
*  INHERITING FROM cl_surdp_uph_proc_base_mcl.
*  PUBLIC SECTION.
*    METHODS constructor.
*    METHODS call_determine_param
**      RETURNING VALUE(rt_determined_params) TYPE if_surdp_uph_entity_mcl_proc~gty_t_mat_class_param
*      .
*    METHODS get_ms_parameters
*      RETURNING VALUE(rt_parameters) TYPE surdps_pckg_matclas_input.
*    METHODS get_mt_mat_clas_params
*      RETURNING VALUE(rt_mat_clas_params) TYPE if_surdp_uph_entity_mcl_proc~gty_t_mat_class_param.
*    METHODS is_processer_initialized
*      RETURNING VALUE(rv_initialized) TYPE abap_bool.
*    METHODS insert_call_bapi_return_values
*      IMPORTING is_bapi1003_num  TYPE bapi1003_alloc_values_num
*                is_bapi1003_char TYPE bapi1003_alloc_values_char
*                is_bapi1003_curr TYPE bapi1003_alloc_values_curr.
*    METHODS set_relevant_class
*      IMPORTING it_rel_class TYPE if_surdp_uph_entity_mcl_proc=>gty_t_mat_class.
*    METHODS clear_call_bapi_return_table.
*    METHODS if_surdp_uph_entity_mcl_proc~get_relevant_mat_clas REDEFINITION.
*    METHODS if_surdp_uph_entity_mcl_proc~map_mat_clas_data REDEFINITION.
*
*  PROTECTED SECTION.
*
*    DATA: mt_bapi1003_num  TYPE TABLE OF bapi1003_alloc_values_num,
*          mt_bapi1003_char TYPE TABLE OF bapi1003_alloc_values_char,
*          mt_bapi1003_curr TYPE TABLE OF bapi1003_alloc_values_curr,
*          mt_rel_class     TYPE if_surdp_uph_entity_mcl_proc=>gty_t_mat_class.
*
*    METHODS call_mat_clas_bapi REDEFINITION.
*
*ENDCLASS.
*
*CLASS th_surdp_uph_proc_base_mcl_imp IMPLEMENTATION.
*
*  METHOD constructor.
*
*    super->constructor(  ).
*
*  ENDMETHOD.
*
*  METHOD call_determine_param.
*    determine_mat_clas_params( ).
*  ENDMETHOD.
*
*  METHOD get_ms_parameters.
*    rt_parameters = ms_parameters.
*  ENDMETHOD.
*
*  METHOD is_processer_initialized.
*    rv_initialized = mv_initialized.
*  ENDMETHOD.
*
*  METHOD get_mt_mat_clas_params.
*    rt_mat_clas_params = mt_mat_clas_params.
*  ENDMETHOD.
*
*  METHOD call_mat_clas_bapi.
*    et_values_char = mt_bapi1003_char.
*    et_values_num  = mt_bapi1003_num.
*    et_values_curr = mt_bapi1003_curr.
*  ENDMETHOD.
*
*  METHOD insert_call_bapi_return_values.
*    APPEND is_bapi1003_num TO mt_bapi1003_num.
*    APPEND is_bapi1003_char TO mt_bapi1003_char.
*    APPEND is_bapi1003_curr TO mt_bapi1003_curr.
*  ENDMETHOD.
*
*  METHOD clear_call_bapi_return_table.
*    CLEAR mt_bapi1003_char.
*    CLEAR mt_bapi1003_curr.
*    CLEAR mt_bapi1003_num.
*  ENDMETHOD.
*
*  METHOD set_relevant_class.
*    mt_rel_class = it_rel_class.
*  ENDMETHOD.
*
*  METHOD if_surdp_uph_entity_mcl_proc~get_relevant_mat_clas.
*    rt_relevant_mat_clas = mt_rel_class.
*  ENDMETHOD.
*
*  METHOD if_surdp_uph_entity_mcl_proc~map_mat_clas_data.
*    rt_entity_data = VALUE #( ).
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS tcl_surdp_uph_proc_base_mcl DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    TYPES:
*      ltty_mara_data    TYPE STANDARD TABLE OF mara,
*      ltty_ksml_data    TYPE STANDARD TABLE OF ksml,
*      ltty_ausp_data    TYPE STANDARD TABLE OF ausp,
*      ltty_klah_data    TYPE STANDARD TABLE OF klah,
*      ltty_clf_hdr_data TYPE STANDARD TABLE OF clf_hdr,
*      ltty_prot_data    TYPE STANDARD TABLE OF surdpd_uph_prot,
*      ltty_kssk_data    TYPE STANDARD TABLE OF kssk.
*
*    CLASS-DATA
*      go_osql_environment TYPE REF TO if_osql_test_environment.
*
*    DATA:
*      mo_cut                  TYPE REF TO th_surdp_uph_proc_base_mcl_imp,
*      mo_logger_td            TYPE REF TO if_surdp_uph_logger,
*      mo_surdp_uph_factory_td TYPE REF TO if_surdp_uph_factory.
*
*    CLASS-METHODS:
*      class_setup,
*      class_teardown.
*
*    METHODS setup.
*    METHODS teardown.
*
*    METHODS given_mara_data
*      IMPORTING it_mara_data TYPE ltty_mara_data.
*
*    METHODS given_ksml_data
*      IMPORTING it_kssk_data TYPE ltty_ksml_data.
*
*    METHODS given_ausp_data
*      IMPORTING it_ausp_data TYPE ltty_ausp_data.
*
*    METHODS given_klah_data
*      IMPORTING it_klah_data TYPE ltty_klah_data.
*
*    METHODS given_clf_hdr_data
*      IMPORTING it_clf_hdr_data TYPE ltty_clf_hdr_data.
*
*    METHODS given_prot_data
*      IMPORTING it_prot_data TYPE ltty_prot_data.
*
*    METHODS given_kssk_data
*      IMPORTING it_kssk_data TYPE ltty_kssk_data.
*
*    METHODS determine_correct_parameter FOR TESTING.
*    METHODS determine_param_wo_valfr FOR TESTING.
*    METHODS determine_param_wo_valto FOR TESTING.
*    METHODS determine_param_wo_valfr_valto FOR TESTING.
*    METHODS determine_param_late_valfr FOR TESTING.
*    METHODS init_processor_fullload FOR TESTING.
*    METHODS init_processor_deltaload FOR TESTING.
*    METHODS prepare_processing_full FOR TESTING.
*    METHODS prepare_processing_validity FOR TESTING.
*    METHODS prepare_wo_determined_params FOR TESTING.
*    METHODS prepare_processing_delta FOR TESTING.
*    METHODS prepare_processing_2deltas FOR TESTING.
*    METHODS prepare__delta_wo_clf_hdr FOR TESTING.
*    METHODS prepare_prcssing_delta_no_prot FOR TESTING.
*    METHODS prepare_processing_no_rel_clas FOR TESTING.
*    METHODS process_package FOR TESTING.
*    METHODS process_package_wo_prepare FOR TESTING.
*    METHODS process_package_w_wrong_clas FOR TESTING.
*
*ENDCLASS.
*
*CLASS tcl_surdp_uph_proc_base_mcl IMPLEMENTATION.
*
*  METHOD class_setup.
*    go_osql_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'MARA' )
*                                                                                         ( 'AUSP' )
*                                                                                         ( 'KLAH' )
*                                                                                         ( 'KSML' )
*                                                                                         ( 'KSSK' )
*                                                                                         ( 'CLF_HDR' )
*                                                                                         ( 'SURDPD_UPH_PROT' ) ) ).
*  ENDMETHOD.
*
*  METHOD class_teardown.
*    go_osql_environment->destroy( ).
*  ENDMETHOD.
*
*  METHOD setup.
*    go_osql_environment->clear_doubles(  ).
*
*    th_surdp_uph_factory_injector=>inject_factory_double( io_double = NEW td_surdp_uph_factory(  ) ).
*
*    mo_cut = NEW th_surdp_uph_proc_base_mcl_imp( ).
*
*    mo_cut->set_relevant_class( it_rel_class = VALUE #( ( class_number = 'pack_frac1' )
*                                                        ( class_number = 'pack_attr1' ) ) ).
*
*  ENDMETHOD.
*
*  METHOD teardown.
*    go_osql_environment->clear_doubles( ).
*    mo_cut->clear_call_bapi_return_table( ).
*  ENDMETHOD.
*
*  METHOD given_mara_data.
*    go_osql_environment->get_double( 'mara' )->insert( cl_osql_test_data=>create( it_mara_data ) ).
*  ENDMETHOD.
*
*  METHOD given_ksml_data.
*    go_osql_environment->get_double( 'ksml' )->insert( cl_osql_test_data=>create( it_kssk_data ) ).
*  ENDMETHOD.
*
*  METHOD given_ausp_data.
*    go_osql_environment->get_double( 'ausp' )->insert( cl_osql_test_data=>create( it_ausp_data ) ).
*  ENDMETHOD.
*
*  METHOD given_klah_data.
*    go_osql_environment->get_double( 'klah' )->insert( cl_osql_test_data=>create( it_klah_data ) ).
*  ENDMETHOD.
*
*  METHOD given_clf_hdr_data.
*    go_osql_environment->get_double( 'clf_hdr' )->insert( cl_osql_test_data=>create( it_clf_hdr_data ) ).
*  ENDMETHOD.
*
*  METHOD given_prot_data.
*    go_osql_environment->get_double( 'SURDPD_UPH_PROT' )->insert( cl_osql_test_data=>create( it_prot_data ) ).
*  ENDMETHOD.
*
*  METHOD given_kssk_data.
*    go_osql_environment->get_double( 'kssk' )->insert( cl_osql_test_data=>create( it_kssk_data ) ).
*  ENDMETHOD.
*
*  METHOD determine_correct_parameter.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                             mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                             mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                             changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) )
*                             valfromdate = '20200101'
*                             valtodate = '20221231' ).
*
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    "when
*    mo_cut->call_determine_param( ).
*    "then
*    DATA(lv_determined_reslt) = mo_cut->get_mt_mat_clas_params(  ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-objek
*                                        exp = 'mat1' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datuv
*                                        exp = '20200101' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datub
*                                        exp = '99991231' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-class
*                                        exp = 'pack_frac1' ).
*  ENDMETHOD.
*
*  METHOD determine_param_wo_valfr.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                             mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                             mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                             changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) )
*                             valtodate = '20221231' ).
*
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    "when
*    mo_cut->call_determine_param( ).
*    "then
*    DATA(lv_determined_reslt) = mo_cut->get_mt_mat_clas_params(  ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-objek
*                                        exp = 'mat1' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datuv
*                                        exp = '20200101' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datub
*                                        exp = '99991231' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-class
*                                        exp = 'pack_frac1' ).
*  ENDMETHOD.
*
*  METHOD determine_param_wo_valto.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                             mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                             mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                             changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) )
*                             valfromdate = '20200101' ).
*
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    "when
*    mo_cut->call_determine_param( ).
*    "then
*    DATA(lv_determined_reslt) = mo_cut->get_mt_mat_clas_params(  ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-objek
*                                        exp = 'mat1' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datuv
*                                        exp = '20200101' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datub
*                                        exp = '99991231' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-class
*                                        exp = 'pack_frac1' ).
*  ENDMETHOD.
*
*  METHOD determine_param_wo_valfr_valto.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                             mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                             mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                             changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) ) ).
*
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    "when
*    mo_cut->call_determine_param( ).
*    "then
*    DATA(lv_determined_reslt) = mo_cut->get_mt_mat_clas_params(  ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-objek
*                                        exp = 'mat1' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datuv
*                                        exp = '20200101' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datub
*                                        exp = '99991231' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-class
*                                        exp = 'pack_frac1' ).
*  ENDMETHOD.
*
*  METHOD determine_param_late_valfr.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                             mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                             mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                             changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) )
*                             valfromdate = sy-datum + 1 ).
*
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    "when
*    mo_cut->call_determine_param( ).
*    "then
*    DATA(lv_determined_reslt) = mo_cut->get_mt_mat_clas_params(  ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-objek
*                                        exp = 'mat1' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datuv
*                                        exp = '20200101' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-datub
*                                        exp = '99991231' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_determined_reslt[ 1 ]-class
*                                        exp = 'pack_frac1' ).
*  ENDMETHOD.
*
*  METHOD init_processor_fullload.
*    "given
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                                     mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                                     mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                                     changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) )
*                                     valfromdate = '20200101'
*                                     valtodate = '20221231' ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    "then
*    cl_abap_unit_assert=>assert_equals( act = mo_cut->get_ms_parameters( )
*                                        exp = ls_input_data ).
*    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_processer_initialized( )
*                                        exp = abap_true ).
*  ENDMETHOD.
*
*  METHOD init_processor_deltaload.
*    "given
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                             mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                             mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                             changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) )
*                             valfromdate = '20200101'
*                             valtodate = '20221231' ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta
*                                                     is_parameters = ls_input_data ).
*    "then
*    cl_abap_unit_assert=>assert_initial( act = mo_cut->get_ms_parameters( ) ).
*    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_processer_initialized( )
*                                        exp = abap_true ).
*  ENDMETHOD.
*
*  METHOD prepare_processing_full.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                             mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                             mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                             changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) )
*                             valfromdate = '20200101'
*                             valtodate = '20221231' ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    DATA(lv_record_cnt) = mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lv_record_cnt
*                                        exp = 1 ).
*
*  ENDMETHOD.
*
*  METHOD prepare_wo_determined_params.
*    "given
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                             mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                             mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                             changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) )
*                             valfromdate = '20200101'
*                             valtodate = '20221231' ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    DATA(lv_record_cnt) = mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lv_record_cnt
*                                        exp = 0 ).
*  ENDMETHOD.
*
*  METHOD prepare_processing_validity.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1_period_span_over' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_mara_data( VALUE #( ( matnr = 'mat2_period_within'    mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_mara_data( VALUE #( ( matnr = 'mat3_periods_overlap'  mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_mara_data( VALUE #( ( matnr = 'mat4_periods_outside'  mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*
*    given_ausp_data( VALUE #( ( objek = 'mat1_period_span_over' lkenz = '' datuv = '20220101' datub = '20220131' atzhl = '1' atinn = '0000000001' klart = '001' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat2_period_within'    lkenz = '' datuv = '20220110' datub = '20220120' atzhl = '1' atinn = '0000000001' klart = '001' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat3_periods_overlap'  lkenz = '' datuv = '20211201' datub = '20220115' atzhl = '1' atinn = '0000000001' klart = '001' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat3_periods_overlap'  lkenz = '' datuv = '20220116' datub = '20220331' atzhl = '2' atinn = '0000000001' klart = '001' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat4_periods_outside'  lkenz = '' datuv = '20211201' datub = '20211231' atzhl = '1' atinn = '0000000001' klart = '001' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat4_periods_outside'  lkenz = '' datuv = '20220201' datub = '20220331' atzhl = '2' atinn = '0000000001' klart = '001' ) ) ).
*
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*
*    given_kssk_data( VALUE #( ( objek = 'mat1_period_span_over' mafid = 'O' klart = '001' adzhl = '1' aennr = 'CN1' clint = '1000000000' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat2_period_within'    mafid = 'O' klart = '001' adzhl = '1' aennr = 'CN2' clint = '1000000000' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat3_periods_overlap'  mafid = 'O' klart = '001' adzhl = '1' aennr = 'CN3' clint = '1000000000' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat3_periods_overlap'  mafid = 'O' klart = '001' adzhl = '2' aennr = 'CN4' clint = '1000000000' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat4_periods_outside'  mafid = 'O' klart = '001' adzhl = '1' aennr = 'CN5' clint = '1000000000' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat4_periods_outside'  mafid = 'O' klart = '001' adzhl = '2' aennr = 'CN6' clint = '1000000000' ) ) ).
*
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( )
*                             mat_type = VALUE #( )
*                             mat_group = VALUE #( )
*                             valfromdate = '20220105'
*                             valtodate   = '20220125' ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    DATA(lv_record_cnt) = mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
**    cl_abap_unit_assert=>assert_equals( act = lv_record_cnt exp = 4 ).
*
*    DATA lv_expected_mat_clas_params TYPE if_surdp_uph_entity_mcl_proc=>gty_t_mat_class_param  .
*    lv_expected_mat_clas_params = VALUE #( ( objek = 'mat1_period_span_over' datuv = '20220101' datub = '20220131' class = 'pack_frac1' lvorm = '' )
*                                           ( objek = 'mat2_period_within'    datuv = '20220110' datub = '20220120' class = 'pack_frac1' lvorm = '' )
*                                           ( objek = 'mat3_periods_overlap'  datuv = '20211201' datub = '20220115' class = 'pack_frac1' lvorm = '' )
*                                           ( objek = 'mat3_periods_overlap'  datuv = '20220116' datub = '20220331' class = 'pack_frac1' lvorm = '' )
*                                         ).
*
*    DATA(lv_actual_mat_clas_params) = mo_cut->get_mt_mat_clas_params(  ).
*
*    SORT lv_actual_mat_clas_params ASCENDING by objek datuv.
*    cl_abap_unit_assert=>assert_equals( act = lv_actual_mat_clas_params exp = lv_expected_mat_clas_params ).
*
*
*  ENDMETHOD.
*
*  METHOD prepare_processing_delta.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' lvorm = '' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    given_clf_hdr_data( VALUE #( ( objek = 'mat1' obtab = 'MARA' mafid = 'O' klart = '001' tstmp_c = '2022121580000.0000000' ) ) ).
*    given_prot_data( VALUE #( ( uuid = CONV #( '0894EF4586611EEC9EBA5FAEA97A8530' )
*                                upload_entity = 'PE_MCL'
*                                upload_mode = 'F'
*                                start_timestamp = '2022120080000.0000000'
*                                end_timestamp = '20221220080100.0000000'
*                                failed = ''
*                                selection =
*'{"material":[{"sign":"I","option":"EQ","low":"mat1"}],"mat_type":[{"sign":"I","option":"EQ","low":"mty1"}],"mat_group":[{"sign":"I","option":"EQ","low":"mgrp1"}],"changedate":[{"sign":"I","option":"EQ","low":"2022-05-01"}],"valfromdate":"2022-01-01' &&
*'","valtodate":"2022-12-31"}' ) ) ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).
*    DATA(lv_record_cnt) = mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lv_record_cnt
*                                        exp = 1 ).
*  ENDMETHOD.
*
*  METHOD prepare_processing_2deltas.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' lvorm = '' )
*                              ( matnr = 'mat2' mtart = 'mty2' matkl = 'mgrp2' laeda = '20220501' lvorm = '' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' )
*                              ( objek = 'mat2' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' )
*                              ( objek = 'mat2' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    given_clf_hdr_data( VALUE #( ( objek = 'mat1' obtab = 'MARA' mafid = 'O' klart = '001' tstmp_c = '20221015080000.0000000' )
*                                 ( objek = 'mat2' obtab = 'MARA' mafid = 'O' klart = '001' tstmp_c = '20201001080000.0000000' ) ) ).
*    given_prot_data( VALUE #( ( uuid = CONV #( '0894EF4586611EEC9EBA5FAEA97A8530' )
*                                upload_entity = 'PE_MCL'
*                                upload_mode = 'F'
*                                start_timestamp = '20221012080000.0000000'
*                                end_timestamp = '20221012080100.0000000'
*                                failed = ''
*                                selection =
*'{"material":[{"sign":"I","option":"CP","low":"mat*"}],"mat_type":[{"sign":"I","option":"CP","low":"mty*"}],"mat_group":[{"sign":"I","option":"CP","low":"mgrp*"}],"changedate":[{"sign":"I","option":"EQ","low":"2022-05-01"}],"valfromdate":"2022-01-01' &&
*'","valtodate":"2022-12-31"}' ) ) ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).
*    DATA(lv_record_cnt) = mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lv_record_cnt
*                                        exp = 1 ).
*  ENDMETHOD.
*
*  METHOD prepare__delta_wo_clf_hdr.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' lvorm = '' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    "given_clf_hdr_data( VALUE #( ( objek = 'mat1' obtab = 'MARA' mafid = 'O' klart = '001' tstmp_c = '2022121580000.0000000' ) ) ).
*    given_prot_data( VALUE #( ( uuid = CONV #( '0894EF4586611EEC9EBA5FAEA97A8530' )
*                                upload_entity = 'PE_MCL'
*                                upload_mode = 'F'
*                                start_timestamp = '2022120080000.0000000'
*                                end_timestamp = '20221220080100.0000000'
*                                failed = ''
*                                selection =
*'{"material":[{"sign":"I","option":"EQ","low":"mat1"}],"mat_type":[{"sign":"I","option":"EQ","low":"mty1"}],"mat_group":[{"sign":"I","option":"EQ","low":"mgrp1"}],"changedate":[{"sign":"I","option":"EQ","low":"2022-05-01"}],"valfromdate":"2022-01-01' &&
*'","valtodate":"2022-12-31"}' ) ) ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_once( ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).
*    DATA(lv_record_cnt) = mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lv_record_cnt
*                                        exp = 0 ).
*  ENDMETHOD.
*
*  METHOD prepare_prcssing_delta_no_prot.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' lvorm = '' )
*                              ( matnr = 'mat2' mtart = 'mty2' matkl = 'mgrp2' laeda = '20220501' lvorm = '' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' )
*                              ( objek = 'mat2' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' )
*                              ( objek = 'mat2' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    given_clf_hdr_data( VALUE #( ( objek = 'mat1' obtab = 'MARA' mafid = 'O' klart = '001' tstmp_c = '2022121580000.0000000' )
*                                 ( objek = 'mat2' obtab = 'MARA' mafid = 'O' klart = '001' tstmp_c = '2020120080000.0000000' ) ) ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_times( 1 ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).
*    DATA(lv_record_cnt) = mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lv_record_cnt
*                                        exp = 0 ).
*  ENDMETHOD.
*
*  METHOD prepare_processing_no_rel_clas.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    DATA ls_input_data TYPE surdps_pckg_matclas_input.
*    ls_input_data = VALUE #( material = VALUE #( ( sign = 'I' option = 'EQ' low = 'mat1' ) )
*                             mat_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'mty1' ) )
*                             mat_group = VALUE #( ( sign = 'I' option = 'EQ' low = 'mgrp1' ) )
*                             changedate = VALUE #( ( sign = 'I' option = 'EQ' low = '20220501' ) )
*                             valfromdate = '20200101'
*                             valtodate = '20221231' ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_times( 2 ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    mo_cut->set_relevant_class( it_rel_class = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                                     is_parameters = ls_input_data ).
*    DATA(lv_record_cnt) = mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lv_record_cnt
*                                        exp = 0 ).
*
*  ENDMETHOD.
*
*  METHOD process_package.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' lvorm = '' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_clf_hdr_data( VALUE #( ( objek = 'mat1' obtab = 'MARA' mafid = 'O' klart = '001' tstmp_c = '2022121580000.0000000' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    given_prot_data( VALUE #( ( uuid = CONV #( '0894EF4586611EEC9EBA5FAEA97A8530' )
*                                upload_entity = 'PE_MCL'
*                                upload_mode = 'F'
*                                start_timestamp = '2022120080000.0000000'
*                                end_timestamp = '20221220080100.0000000'
*                                failed = ''
*                                selection =
*'{"material":[{"sign":"I","option":"EQ","low":"mat1"}],"mat_type":[{"sign":"I","option":"EQ","low":"mty1"}],"mat_group":[{"sign":"I","option":"EQ","low":"mgrp1"}],"changedate":[{"sign":"I","option":"EQ","low":"2022-05-01"}],"valfromdate":"2022-01-01' &&
*'","valtodate":"2022-12-31"}' ) ) ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_times( 2 ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).
*    DATA(lt_entity_data) = mo_cut->if_surdp_uph_entity_proc~process_package( ).
*
*
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_entity_data )
*                                        exp = 0 ).
*    cl_abap_unit_assert=>assert_equals( act = lines( mo_cut->get_mt_mat_clas_params( ) )
*                                        exp = 1 ).
*  ENDMETHOD.
*
*  METHOD process_package_wo_prepare.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' lvorm = '' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac1' ) ) ).
*    given_clf_hdr_data( VALUE #( ( objek = 'mat1' obtab = 'MARA' mafid = 'O' klart = '001' tstmp_c = '2022121580000.0000000' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    given_prot_data( VALUE #( ( uuid = CONV #( '0894EF4586611EEC9EBA5FAEA97A8530' )
*                                upload_entity = 'PE_MCL'
*                                upload_mode = 'F'
*                                start_timestamp = '2022120080000.0000000'
*                                end_timestamp = '20221220080100.0000000'
*                                failed = ''
*                                selection =
*'{"material":[{"sign":"I","option":"EQ","low":"mat1"}],"mat_type":[{"sign":"I","option":"EQ","low":"mty1"}],"mat_group":[{"sign":"I","option":"EQ","low":"mgrp1"}],"changedate":[{"sign":"I","option":"EQ","low":"2022-05-01"}],"valfromdate":"2022-01-01' &&
*'","valtodate":"2022-12-31"}' ) ) ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_times( 2 ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).
*    mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    DATA(lt_entity_data) = mo_cut->if_surdp_uph_entity_proc~process_package( ).
*
*
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_entity_data )
*                                        exp = 0 ).
*    cl_abap_unit_assert=>assert_equals( act = lines( mo_cut->get_mt_mat_clas_params( ) )
*                                        exp = 1 ).
*  ENDMETHOD.
*
*  METHOD process_package_w_wrong_clas.
*    "given
*    given_mara_data( VALUE #( ( matnr = 'mat1' mtart = 'mty1' matkl = 'mgrp1' laeda = '20220501' lvorm = '' ) ) ).
*    given_ausp_data( VALUE #( ( objek = 'mat1' lkenz = '' datuv = '20200101' datub = '99991231' atinn = '0000000001' klart = '001' ) ) ).
*    given_ksml_data( VALUE #( ( clint = '1000000000' imerk = '0000000001' ) ) ).
*    given_klah_data( VALUE #( ( clint = '1000000000' class = 'pack_frac_wrong' ) ) ).
*    given_kssk_data( VALUE #( ( objek = 'mat1' mafid = 'O' klart = '001' clint = '1000000000' ) ) ).
*    given_clf_hdr_data( VALUE #( ( objek = 'mat1' obtab = 'MARA' mafid = 'O' klart = '001' tstmp_c = '2022121580000.0000000' ) ) ).
*    given_prot_data( VALUE #( ( uuid = CONV #( '0894EF4586611EEC9EBA5FAEA97A8530' )
*                                upload_entity = 'PE_MCL'
*                                upload_mode = 'F'
*                                start_timestamp = '2022120080000.0000000'
*                                end_timestamp = '20221220080100.0000000'
*                                failed = ''
*                                selection =
*'{"material":[{"sign":"I","option":"EQ","low":"mat1"}],"mat_type":[{"sign":"I","option":"EQ","low":"mty1"}],"mat_group":[{"sign":"I","option":"EQ","low":"mgrp1"}],"changedate":[{"sign":"I","option":"EQ","low":"2022-05-01"}],"valfromdate":"2022-01-01' &&
*'","valtodate":"2022-12-31"}' ) ) ).
*    cl_abap_testdouble=>configure_call( cl_surdp_uph_factory=>get_instance( )->get_logger( ) )->ignore_all_parameters( )->and_expect( )->is_called_times( 2 ).
*    cl_surdp_uph_factory=>get_instance( )->get_logger( )->add_messages( it_messages = VALUE #( ) ).
*    "when
*    mo_cut->if_surdp_uph_entity_proc~init_processor( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                                                     iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).
*    DATA(lt_entity_data) = mo_cut->if_surdp_uph_entity_proc~process_package( ).
*    "then
*    cl_abap_testdouble=>verify_expectations( cl_surdp_uph_factory=>get_instance( )->get_logger( ) ).
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_entity_data )
*                                        exp = 0 ).
*    cl_abap_unit_assert=>assert_equals( act = lines( mo_cut->get_mt_mat_clas_params( ) )
*                                        exp = 0 ).
*  ENDMETHOD.
*
*ENDCLASS.
*
*
*CLASS ltc_map_base DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PROTECTED SECTION.
*    METHODS given_version_for IMPORTING iv_objek         TYPE cuobn
*                                        iv_datuv         TYPE datuv OPTIONAL
*                              RETURNING VALUE(rv_result) TYPE REF TO cl_surdp_uph_wrap_mcl_version.
*
*    DATA m_cut TYPE REF TO lcl_surdp_uph_wrap_mcl_map.
*
*  PRIVATE SECTION.
*    METHODS setup.
*
*ENDCLASS.
*
*
*CLASS ltc_map_base IMPLEMENTATION.
*  METHOD setup.
*    m_cut = NEW #( ).
*  ENDMETHOD.
*
*
*  METHOD given_version_for.
*    DATA ls_mcl_header         TYPE if_surdp_uph_entity_mcl_proc=>gty_mat_class_parameter.
*    DATA lt_mcl_character_data TYPE tt_bapi1003_alloc_values_char.
*    DATA lt_mcl_number_data    TYPE tt_bapi1003_alloc_values_num.
*    DATA lt_mcl_currency_data  TYPE tt_bapi1003_alloc_values_curr.
*
*    ls_mcl_header-objek = iv_objek.
*    ls_mcl_header-datuv = iv_datuv.
*
*    lt_mcl_character_data = VALUE #( ( charact = 'attribute1' value_char = 'val_att_1_' && iv_objek )
*                                     ( charact = 'attribute2' value_char = 'val_att_2_' && iv_objek ) ).
*    lt_mcl_number_data = VALUE #( ( charact = 'number1' value_from = '42.0' )
*                                  ( charact = 'number2' value_from = '53.0' )
*                                  ( charact = 'number3' value_from = '64.0' ) ).
*    lt_mcl_currency_data = VALUE #( ( charact = 'currency1' value_from = '21.0' )
*                                    ( charact = 'currency2' value_from = '23.0' ) ).
*
*    rv_result = NEW #( is_mcl_header         = ls_mcl_header
*                       it_mcl_character_data = lt_mcl_character_data
*                       it_mcl_number_data    = lt_mcl_number_data
*                       it_mcl_currency_data  = lt_mcl_currency_data ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS ltc_map_add_version DEFINITION DEFERRED.
*CLASS cl_surdp_uph_proc_base_mcl DEFINITION LOCAL FRIENDS ltc_map_add_version.
*
*CLASS ltc_map_add_version DEFINITION FINAL FOR TESTING INHERITING FROM ltc_map_base
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    METHODS add_one_version                FOR TESTING RAISING cx_static_check.
*    METHODS add_versions_multiple_prods    FOR TESTING RAISING cx_static_check.
*    METHODS add_versions_same_product      FOR TESTING RAISING cx_static_check.
*    METHODS add_multiple_versions_to_prods FOR TESTING RAISING cx_static_check.
*
*    METHODS when_adding_a_version          IMPORTING iv_version TYPE REF TO cl_surdp_uph_wrap_mcl_version.
*
*    METHODS then_expect_mat_class_table.
*
*    DATA mv_expected_internal_map TYPE HASHED TABLE OF lcl_surdp_uph_wrap_mcl_map=>ty_map_key_value_pair WITH UNIQUE KEY objek.
*ENDCLASS.
*
*
*
*CLASS ltc_map_add_version IMPLEMENTATION.
*  METHOD add_one_version.
*    DATA(lv_version) = given_version_for( iv_objek = 'PROD_1' ).
*
*    when_adding_a_version( iv_version = lv_version ).
*
*    mv_expected_internal_map = VALUE #( ( objek = 'PROD_1' mcl_versions = VALUE #( ( lv_version ) ) ) ).
*
*    then_expect_mat_class_table( ).
*  ENDMETHOD.
*
*
*  METHOD add_versions_multiple_prods.
*    DATA(lv_version1) = given_version_for( iv_objek = 'PROD_1' ).
*    DATA(lv_version2) = given_version_for( iv_objek = 'PROD_2' ).
*    DATA(lv_version3) = given_version_for( iv_objek = 'PROD_3' ).
*
*    when_adding_a_version( iv_version = lv_version1 ).
*    when_adding_a_version( iv_version = lv_version2 ).
*    when_adding_a_version( iv_version = lv_version3 ).
*
*    mv_expected_internal_map = VALUE #( ( objek = 'PROD_1' mcl_versions = VALUE #( ( lv_version1 ) ) )
*                                        ( objek = 'PROD_2' mcl_versions = VALUE #( ( lv_version2 ) ) )
*                                        ( objek = 'PROD_3' mcl_versions = VALUE #( ( lv_version3 ) ) ) ).
*
*    then_expect_mat_class_table( ).
*  ENDMETHOD.
*
*
*  METHOD add_versions_same_product.
*    DATA(lv_version1) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20220101' ).
*    DATA(lv_version2) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20210101' ).
*    DATA(lv_version3) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20200101' ).
*
*    when_adding_a_version( iv_version = lv_version1 ).
*    when_adding_a_version( iv_version = lv_version2 ).
*    when_adding_a_version( iv_version = lv_version3 ).
*
*    mv_expected_internal_map = VALUE #( ( objek = 'PROD_1' mcl_versions = VALUE #( ( lv_version1 ) ( lv_version2 ) ( lv_version3 ) ) ) ).
*
*    then_expect_mat_class_table( ).
*  ENDMETHOD.
*
*
*  METHOD add_multiple_versions_to_prods.
*    DATA(lv_version1) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20220101').
*    DATA(lv_version2) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20210101').
*    DATA(lv_version3) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20200101').
*
*    DATA(lv_version4) = given_version_for( iv_objek = 'PROD_2' iv_datuv = '20220202').
*    DATA(lv_version5) = given_version_for( iv_objek = 'PROD_2' iv_datuv = '20210202').
*
*    DATA(lv_version6) = given_version_for( iv_objek = 'PROD_3' iv_datuv = '20220303').
*    DATA(lv_version7) = given_version_for( iv_objek = 'PROD_3' iv_datuv = '20210303').
*
*    when_adding_a_version( iv_version = lv_version7 ).
*    when_adding_a_version( iv_version = lv_version1 ).
*    when_adding_a_version( iv_version = lv_version3 ).
*    when_adding_a_version( iv_version = lv_version5 ).
*    when_adding_a_version( iv_version = lv_version4 ).
*    when_adding_a_version( iv_version = lv_version2 ).
*    when_adding_a_version( iv_version = lv_version6 ).
*
*    mv_expected_internal_map = VALUE #(
*        ( objek = 'PROD_3' mcl_versions = VALUE #( ( lv_version7 ) ( lv_version6 ) ) )
*        ( objek = 'PROD_1' mcl_versions = VALUE #( ( lv_version1 ) ( lv_version3 ) ( lv_version2 )  ) )
*        ( objek = 'PROD_2' mcl_versions = VALUE #( ( lv_version5 ) ( lv_version4 ) ) ) ).
*
*    then_expect_mat_class_table( ).
*  ENDMETHOD.
*
*
*  METHOD when_adding_a_version.
*    m_cut->add_version( iv_mcl_version = iv_version ).
*  ENDMETHOD.
*
*
*  METHOD then_expect_mat_class_table.
*    cl_abap_unit_assert=>assert_equals( exp = mv_expected_internal_map act = m_cut->get_internal_map(  ) ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS ltc_map_get_mat_class_wrapper DEFINITION DEFERRED.
*CLASS cl_surdp_uph_proc_base_mcl DEFINITION LOCAL FRIENDS ltc_map_get_mat_class_wrapper.
*
*CLASS ltc_map_get_mat_class_wrapper DEFINITION FINAL FOR TESTING INHERITING FROM ltc_map_base
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    METHODS multiple_versions_for_1_prod  FOR TESTING.
*    METHODS multiple_prods_with_1_version FOR TESTING.
*    METHODS multiple_prods_and_versions   FOR TESTING.
*
*    METHODS given_internal_map.
*    METHODS when_retrieving_wrappers.
*    METHODS then_expect_wrappers.
*
*    DATA mv_given_internal_map TYPE HASHED TABLE OF lcl_surdp_uph_wrap_mcl_map=>ty_map_key_value_pair WITH UNIQUE KEY objek.
*    DATA mv_actual_wrappers TYPE surdpt_uph_wrap_mcl.
*    DATA mv_expected_wrappers TYPE surdpt_uph_wrap_mcl.
*ENDCLASS.
*
*
*
*CLASS ltc_map_get_mat_class_wrapper IMPLEMENTATION.
*  METHOD multiple_versions_for_1_prod.
*    DATA(lv_version1) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20220101').
*    DATA(lv_version2) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20210101').
*    DATA(lv_version3) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20200101').
*
*    mv_given_internal_map = VALUE #( ( objek = 'PROD_1' mcl_versions = VALUE #( ( lv_version1 ) ( lv_version3 ) ( lv_version2 )  ) ) ).
*    given_internal_map( ).
*
*    when_retrieving_wrappers( ).
*
*    DATA(lv_exp_wrapper_prod1) = NEW cl_surdp_uph_wrap_mcl( iv_objek = 'PROD_1' it_mcl_versions = VALUE #( ( lv_version1 ) ( lv_version3 ) ( lv_version2 ) ) ).
*
*    mv_expected_wrappers = VALUE #( ( lv_exp_wrapper_prod1 ) ).
*
*    then_expect_wrappers( ).
*  ENDMETHOD.
*
*
*  METHOD multiple_prods_with_1_version.
*    DATA(lv_version1) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20220101').
*    DATA(lv_version2) = given_version_for( iv_objek = 'PROD_2' iv_datuv = '20220202').
*    DATA(lv_version3) = given_version_for( iv_objek = 'PROD_3' iv_datuv = '20220303').
*
*    mv_given_internal_map = VALUE #( ( objek = 'PROD_3' mcl_versions = VALUE #( ( lv_version3 ) ) )
*                                     ( objek = 'PROD_1' mcl_versions = VALUE #( ( lv_version1 ) ) )
*                                     ( objek = 'PROD_2' mcl_versions = VALUE #( ( lv_version2 ) ) ) ).
*
*    given_internal_map( ).
*
*    when_retrieving_wrappers( ).
*
*    DATA(lv_exp_wrapper_prod3) = NEW cl_surdp_uph_wrap_mcl( iv_objek = 'PROD_3' it_mcl_versions = VALUE #( ( lv_version3 ) ) ).
*    DATA(lv_exp_wrapper_prod1) = NEW cl_surdp_uph_wrap_mcl( iv_objek = 'PROD_1' it_mcl_versions = VALUE #( ( lv_version1 ) ) ).
*    DATA(lv_exp_wrapper_prod2) = NEW cl_surdp_uph_wrap_mcl( iv_objek = 'PROD_2' it_mcl_versions = VALUE #( ( lv_version2 ) ) ).
*
*    mv_expected_wrappers = VALUE #( ( lv_exp_wrapper_prod3 ) ( lv_exp_wrapper_prod1 ) ( lv_exp_wrapper_prod2 ) ).
*
*    then_expect_wrappers( ).
*  ENDMETHOD.
*
*
*  METHOD multiple_prods_and_versions.
*    DATA(lv_version1) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20220101').
*    DATA(lv_version2) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20210101').
*    DATA(lv_version3) = given_version_for( iv_objek = 'PROD_1' iv_datuv = '20200101').
*
*    DATA(lv_version4) = given_version_for( iv_objek = 'PROD_2' iv_datuv = '20220202').
*    DATA(lv_version5) = given_version_for( iv_objek = 'PROD_2' iv_datuv = '20210202').
*
*    DATA(lv_version6) = given_version_for( iv_objek = 'PROD_3' iv_datuv = '20220303').
*    DATA(lv_version7) = given_version_for( iv_objek = 'PROD_3' iv_datuv = '20210303').
*
*    mv_given_internal_map = VALUE #(
*        ( objek = 'PROD_3' mcl_versions = VALUE #( ( lv_version7 ) ( lv_version6 ) ) )
*        ( objek = 'PROD_1' mcl_versions = VALUE #( ( lv_version1 ) ( lv_version3 ) ( lv_version2 )  ) )
*        ( objek = 'PROD_2' mcl_versions = VALUE #( ( lv_version5 ) ( lv_version4 ) ) ) ).
*
*    given_internal_map( ).
*
*    when_retrieving_wrappers( ).
*
*    DATA(lv_exp_wrapper_prod3) = NEW cl_surdp_uph_wrap_mcl( iv_objek = 'PROD_3' it_mcl_versions = VALUE #( ( lv_version7 ) ( lv_version6 ) ) ).
*    DATA(lv_exp_wrapper_prod1) = NEW cl_surdp_uph_wrap_mcl( iv_objek = 'PROD_1' it_mcl_versions = VALUE #( ( lv_version1 ) ( lv_version3 ) ( lv_version2 ) ) ).
*    DATA(lv_exp_wrapper_prod2) = NEW cl_surdp_uph_wrap_mcl( iv_objek = 'PROD_2' it_mcl_versions = VALUE #( ( lv_version5 ) ( lv_version4 ) ) ).
*
*    mv_expected_wrappers = VALUE #( ( lv_exp_wrapper_prod3 ) ( lv_exp_wrapper_prod1 ) ( lv_exp_wrapper_prod2 ) ).
*
*    then_expect_wrappers( ).
*  ENDMETHOD.
*
*
*  METHOD given_internal_map.
*    m_cut =  NEW #( mv_given_internal_map ).
*  ENDMETHOD.
*
*
*  METHOD when_retrieving_wrappers.
*    mv_actual_wrappers = m_cut->get_mat_class_wrapper( ).
*  ENDMETHOD.
*
*
*  METHOD then_expect_wrappers.
*    DATA lb_expected_found TYPE boolean VALUE abap_false.
*
*    LOOP AT mv_expected_wrappers INTO DATA(lv_expected_wrapper).
*
*      LOOP AT mv_actual_wrappers INTO DATA(lv_actual_wrapper).
*        IF lv_actual_wrapper->get_product_id( ) = lv_expected_wrapper->get_product_id( ).
*          lb_expected_found = abap_true.
*          cl_abap_unit_assert=>assert_equals( exp = lv_expected_wrapper->get_versions( ) act = lv_actual_wrapper->get_versions( ) ).
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*
*      IF lb_expected_found = abap_false.
*        cl_abap_unit_assert=>fail( 'Excpected Wrapper not found in actuals for ' && lv_actual_wrapper->get_product_id( ) && '.' ).
*      ELSE.
*        lb_expected_found = abap_false.
*      ENDIF.
*    ENDLOOP.
*  ENDMETHOD.
*ENDCLASS.
