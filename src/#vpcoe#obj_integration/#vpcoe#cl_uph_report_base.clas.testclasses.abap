*CLASS tcl_mclload_derive_upload_mode DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut TYPE REF TO cl_surdp_uph_report_base.  " class under test
*
*    METHODS setup.
*
*    METHODS test_initial_load    FOR TESTING.
*    METHODS test_delta_load      FOR TESTING.
*    METHODS test_both_given_load FOR TESTING.
*    METHODS test_non_given       FOR TESTING.
*
*ENDCLASS.
*
*
*
*CLASS tcl_mclload_derive_upload_mode IMPLEMENTATION.
*  METHOD setup.
*    f_cut = NEW cl_surdp_uph_report_base( ).
*  ENDMETHOD.
*
*
*  METHOD test_initial_load.
*    " Given / "When
*    DATA(upload_mode) = f_cut->if_surdp_uph_report~derive_upload_mode( ic_initial_load = 'X'
*                                                   ic_delta_load   = '' ).
*    " Then
*    cl_abap_unit_assert=>assert_equals( msg = 'Upload mode is not initial load / full mode'
*                                        exp = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                        act = upload_mode ).
*  ENDMETHOD.
*
*
*  METHOD test_delta_load.
*    " Given / "When
*    DATA(upload_mode) = f_cut->if_surdp_uph_report~derive_upload_mode( ic_initial_load = ''
*                                                   ic_delta_load   = 'X' ).
*    " Then
*    cl_abap_unit_assert=>assert_equals( msg = 'Upload mode is not delta load'
*                                        exp = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta
*                                        act = upload_mode ).
*  ENDMETHOD.
*
*
*  METHOD test_both_given_load.
*    " Given / "When
*    DATA(upload_mode) = f_cut->if_surdp_uph_report~derive_upload_mode( ic_initial_load = 'X'
*                                                   ic_delta_load   = 'X' ).
*    " Then
*    cl_abap_unit_assert=>assert_equals( msg = 'Upload mode is not initial load / full mode'
*                                        exp = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                        act = upload_mode ).
*  ENDMETHOD.
*
*
*  METHOD test_non_given.
*    " Given / "When
*    DATA(upload_mode) = f_cut->if_surdp_uph_report~derive_upload_mode( ic_initial_load = ''
*                                                   ic_delta_load   = '' ).
*    " Then
*    cl_abap_unit_assert=>assert_equals( msg = 'Upload mode is not initial load / full mode'
*                                        exp = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                        act = upload_mode ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS tcl_mclload_execute_upload DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut     TYPE REF TO cl_surdp_uph_report_base.  " class under test
*    DATA mr_double TYPE REF TO if_surdp_uph_exec_load_wrap.
*
*    METHODS: setup,
*             teardown.
*
*    METHODS test_execute_upload FOR TESTING.
*
*ENDCLASS.
*
*
*
*CLASS tcl_mclload_execute_upload IMPLEMENTATION.
*  METHOD setup.
*    mr_double ?= cl_abap_testdouble=>create( 'if_surdp_uph_exec_load_wrap' ).
*    f_cut = NEW cl_surdp_uph_report_base( io_exec_load_wrapper = mr_double ).
*  ENDMETHOD.
*
*
*  METHOD teardown.
*    CLEAR: f_cut, mr_double.
*  ENDMETHOD.
*
*
*  METHOD test_execute_upload.
*    " Given
*    DATA(lv_upload_mode) = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.
*    DATA(lo_input)       = 'any'.
*    DATA(lv_test)        = 'X'.
*
*    " Given
*    cl_abap_testdouble=>configure_call( mr_double )->and_expect( )->is_called_once( ).
*    mr_double->execute_upload( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                               iv_upload_mode   = lv_upload_mode
*                               is_parameters    = lo_input
*                               iv_test          = lv_test ).
*
*    " When
*    f_cut->if_surdp_uph_report~execute_upload( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
*                           iv_upload_mode = lv_upload_mode
*                           is_parameters  = lo_input
*                           iv_test        = lv_test ).
*
*    cl_abap_testdouble=>verify_expectations( mr_double ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS tcl_mclload_rfc_destination DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    DATA f_cut     TYPE REF TO cl_surdp_uph_report_base.  " class under test
*    DATA mr_double TYPE REF TO if_surdp_uph_factory.
*
*    METHODS: setup,
*             teardown.
*
*    METHODS test_given_rfc_destination     FOR TESTING.
*    METHODS test_default_rfc_destination   FOR TESTING.
*
*ENDCLASS.
*
*
*
*CLASS tcl_mclload_rfc_destination IMPLEMENTATION.
*  METHOD setup.
*    mr_double ?= cl_abap_testdouble=>create( 'if_surdp_uph_factory' ).
*    f_cut = NEW cl_surdp_uph_report_base( io_surdp_uph_factory = mr_double ).
*  ENDMETHOD.
*
*
*  METHOD teardown.
*    CLEAR f_cut.
*  ENDMETHOD.
*
*  METHOD test_given_rfc_destination.
*    DATA given_rfc_destination TYPE rfcdest VALUE 'AUNIT_DESTINATION'.
*
*    " Given
*    cl_abap_testdouble=>configure_call( mr_double  )->set_parameter( name = 'ev_endpoint_dest' value = given_rfc_destination ).
*    mr_double->get_target_destination( ).
*
*    " When
*    DATA(lv_rfc_dest) = f_cut->if_surdp_uph_report~derive_rfc_target_destination( ).
*
*    " Then
*    cl_abap_unit_assert=>assert_equals( exp = given_rfc_destination act = lv_rfc_dest ).
*  ENDMETHOD.
*
*
*  METHOD test_default_rfc_destination.
*    DATA initial_rfc_destination TYPE rfcdest VALUE ''.
*
*    " Given
*    cl_abap_testdouble=>configure_call( mr_double  )->set_parameter( name = 'ev_endpoint_dest' value = initial_rfc_destination ).
*    mr_double->get_target_destination( ).
*
*    " When
*    DATA(lv_rfc_dest) = f_cut->if_surdp_uph_report~derive_rfc_target_destination( ).
*
*    " Then
*    cl_abap_unit_assert=>assert_equals( exp = if_surdp_uph_report=>gc_default_rfc_destination act = lv_rfc_dest ).
*  ENDMETHOD.
*ENDCLASS.
