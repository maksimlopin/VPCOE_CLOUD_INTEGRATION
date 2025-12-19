*CLASS tcl_bomload_derive_upload_mode DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    DATA f_cut TYPE REF TO cl_surdp_uph_pckg_cmp_bom_load.  " class under test
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
*CLASS tcl_bomload_derive_upload_mode IMPLEMENTATION.
*  METHOD setup.
*    f_cut = NEW cl_surdp_uph_pckg_cmp_bom_load( ).
*  ENDMETHOD.
*
*
*  METHOD test_initial_load.
*    " Given / "When
*    DATA(upload_mode) = f_cut->derive_upload_mode( ic_initial_load = 'X'
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
*    DATA(upload_mode) = f_cut->derive_upload_mode( ic_initial_load = ''
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
*    DATA(upload_mode) = f_cut->derive_upload_mode( ic_initial_load = 'X'
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
*    DATA(upload_mode) = f_cut->derive_upload_mode( ic_initial_load = ''
*                                                   ic_delta_load   = '' ).
*    " Then
*    cl_abap_unit_assert=>assert_equals( msg = 'Upload mode is not initial load / full mode'
*                                        exp = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                        act = upload_mode ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS tcl_bomload_derive_input DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    DATA f_cut TYPE REF TO cl_surdp_uph_pckg_cmp_bom_load.  " class under test
*
*    METHODS setup.
*
*    METHODS test_all_params_given FOR TESTING.
*    METHODS test_no_params_given  FOR TESTING.
*
*ENDCLASS.
*
*CLASS tcl_bomload_derive_input IMPLEMENTATION.
*  METHOD setup.
*    f_cut = NEW cl_surdp_uph_pckg_cmp_bom_load( ).
*  ENDMETHOD.
*
*
*  METHOD test_all_params_given.
*    DATA:
*      lv_source_id             TYPE surdp_uph_source_id,
*      lv_rfc_destination       TYPE surdp_uph_rfcdest,
*      lv_material_range        TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_material_range,
*      lv_material_type_range   TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_material_type_range,
*      lv_plant_range           TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_plant_range,
*      lv_bom_usage_range       TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_bom_usage_range,
*      lv_alternative_bom_range TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_alternative_bom_range,
*      lv_bom_status            TYPE stlst,
*      lv_bom_valid_from_date   TYPE sy-datum,
*      lv_bom_change_date_range TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_change_date_range,
*      lv_max_explosion_level   TYPE cs_maxst.
*
*    " Given
*    lv_source_id             = 'UnitTestSourceId#42'.
*    lv_rfc_destination       = 'UnitTestRfcDestination#99'.
*    lv_material_range        = VALUE if_surdp_uph_pckg_cmp_bom_load=>ty_material_range(
*                                         ( sign = 'I' option = 'BT' low = 'AA' high = 'BB') ).
*    lv_material_type_range   = VALUE if_surdp_uph_pckg_cmp_bom_load=>ty_material_type_range(
*                                         ( sign = 'I' option = 'BT' low = 'CC' high = 'DD') ).
*    lv_plant_range           = VALUE if_surdp_uph_pckg_cmp_bom_load=>ty_plant_range(
*                                         ( sign = 'I' option = 'BT' low = 'EE' high = 'FF') ).
*    lv_bom_usage_range       = VALUE if_surdp_uph_pckg_cmp_bom_load=>ty_bom_usage_range(
*                                         ( sign = 'I' option = 'BT' low = 'GG' high = 'HH') ).
*    lv_alternative_bom_range = VALUE if_surdp_uph_pckg_cmp_bom_load=>ty_material_range(
*                                         ( sign = 'I' option = 'BT' low = 'II' high = 'JJ') ).
*    lv_bom_status            = '01'.
*    lv_bom_valid_from_date   = '2022-04-11'.
*    lv_bom_change_date_range = VALUE if_surdp_uph_pckg_cmp_bom_load=>ty_change_date_range(
*                                         ( sign = 'I' option = 'BT' low = '2022-04-11' high = '2022-04-22') ).
*    lv_max_explosion_level   = '33'.
*
*    " When
*    DATA(lo_input)    = f_cut->derive_input_from_parameters( iv_source_id             = lv_source_id
*                                                             iv_rfc_destination       = lv_rfc_destination
*                                                             iv_material_range        = lv_material_range
*                                                             iv_material_type_range   = lv_material_type_range
*                                                             iv_plant_range           = lv_plant_range
*                                                             iv_bom_usage_range       = lv_bom_usage_range
*                                                             iv_alternative_bom_range = lv_alternative_bom_range
*                                                             iv_bom_status            = lv_bom_status
*                                                             iv_bom_validity_date     = lv_bom_valid_from_date
*                                                             iv_bom_change_date_range = lv_bom_change_date_range
*                                                             iv_max_explosion_level   = lv_max_explosion_level ).
*    " Then
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-source_id   act = lv_source_id              ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-rfc_des     act = lv_rfc_destination        ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-material    act = lv_material_range         ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-mat_type    act = lv_material_type_range    ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-plant       act = lv_plant_range            ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-bom_usage   act = lv_bom_usage_range        ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-bom_alter   act = lv_alternative_bom_range  ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-bom_status  act = lv_bom_status             ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-valfromdate act = lv_bom_valid_from_date    ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-bom_chgdon  act = lv_bom_change_date_range  ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-max_exp_lvl act = lv_max_explosion_level    ).
*  ENDMETHOD.
*
*  METHOD test_no_params_given.
*    DATA:
*      lv_source_id             TYPE surdp_uph_source_id,
*      lv_rfc_destination       TYPE surdp_uph_rfcdest,
*      lv_material_range        TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_material_range,
*      lv_material_type_range   TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_material_type_range,
*      lv_plant_range           TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_plant_range,
*      lv_bom_usage_range       TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_bom_usage_range,
*      lv_alternative_bom_range TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_alternative_bom_range,
*      lv_bom_status            TYPE stlst,
*      lv_bom_valid_from_date   TYPE sy-datum,
*      lv_bom_change_date_range TYPE if_surdp_uph_pckg_cmp_bom_load=>ty_change_date_range,
*      lv_max_explosion_level   TYPE cs_maxst.
*
*    " When
*    DATA(lo_input)    = f_cut->derive_input_from_parameters( iv_source_id             = lv_source_id
*                                                             iv_rfc_destination       = lv_rfc_destination
*                                                             iv_material_range        = lv_material_range
*                                                             iv_material_type_range   = lv_material_type_range
*                                                             iv_plant_range           = lv_plant_range
*                                                             iv_bom_usage_range       = lv_bom_usage_range
*                                                             iv_alternative_bom_range = lv_alternative_bom_range
*                                                             iv_bom_status            = lv_bom_status
*                                                             iv_bom_validity_date     = lv_bom_valid_from_date
*                                                             iv_bom_change_date_range = lv_bom_change_date_range
*                                                             iv_max_explosion_level   = lv_max_explosion_level ).
*    " Then
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-source_id   act = lv_source_id              ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-rfc_des     act = lv_rfc_destination        ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-material    act = lv_material_range         ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-mat_type    act = lv_material_type_range    ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-plant       act = lv_plant_range            ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-bom_usage   act = lv_bom_usage_range        ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-bom_alter   act = lv_alternative_bom_range  ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-bom_status  act = lv_bom_status             ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-valfromdate act = lv_bom_valid_from_date    ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-bom_chgdon  act = lv_bom_change_date_range  ).
*    cl_abap_unit_assert=>assert_equals( exp = lo_input-max_exp_lvl act = lv_max_explosion_level    ).
*
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS tcl_bomload_execute_upload DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    DATA f_cut     TYPE REF TO cl_surdp_uph_pckg_cmp_bom_load.  " class under test
*    DATA mr_double TYPE REF TO if_surdp_uph_exec_load_wrap.
*
*    METHODS:
*      setup,
*      teardown.
*
*    METHODS test_execute_upload FOR TESTING.
*
*ENDCLASS.
*
*CLASS tcl_bomload_execute_upload IMPLEMENTATION.
*  METHOD setup.
*    mr_double ?= cl_abap_testdouble=>create( 'if_surdp_uph_exec_load_wrap' ).
*    f_cut = NEW cl_surdp_uph_pckg_cmp_bom_load( io_exec_load_wrapper = mr_double ).
*  ENDMETHOD.
*
*  METHOD teardown.
*    CLEAR: f_cut, mr_double.
*  ENDMETHOD.
*
*  METHOD test_execute_upload.
*    " Given
*    DATA(lv_upload_mode) = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.
*    DATA(lo_input)       = 'any'.
*    DATA(lv_test)        = 'X'.
*
*    " Given
*    cl_abap_testdouble=>configure_call( mr_double )->and_expect( )->is_called_once( ).
*    mr_double->execute_upload( iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
*                               iv_upload_mode   = lv_upload_mode
*                               is_parameters    = lo_input
*                               iv_test          = lv_test ).
*
*    " When
*    f_cut->execute_upload( iv_upload_mode = lv_upload_mode
*                           is_parameters  = lo_input
*                           iv_test        = lv_test ).
*
*    cl_abap_testdouble=>verify_expectations( mr_double ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*CLASS tcl_bomload_check_auth DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    DATA f_cut     TYPE REF TO cl_surdp_uph_pckg_cmp_bom_load.  " class under test
*    DATA mr_double TYPE REF TO if_surdp_uph_exec_load_wrap.
*
*    METHODS:
*      setup,
*      teardown.
*
*    METHODS test_with_authorization    FOR TESTING.
*    METHODS test_without_authorization FOR TESTING RAISING cx_abap_auth_check_exception.
*
*ENDCLASS.
*
*CLASS tcl_bomload_check_auth IMPLEMENTATION.
*  METHOD setup.
*    f_cut = NEW cl_surdp_uph_pckg_cmp_bom_load( io_exec_load_wrapper = mr_double ).
*  ENDMETHOD.
*
*
*  METHOD teardown.
*    CLEAR: f_cut.
*  ENDMETHOD.
*
*
*  METHOD test_without_authorization.
*    " Given no authorization
*    cl_aunit_authority_check=>get_controller( )->restrict_authorizations_to( cl_aunit_authority_check=>create_auth_object_set( ) ).
*
*    " When
*    DATA(lv_authorization_granted) = f_cut->is_authorized( ).
*
*    " Then
*    cl_abap_unit_assert=>assert_false( lv_authorization_granted ).
*
*  ENDMETHOD.
*
*
*  METHOD test_with_authorization.
*    " Given authorizations are set to current user's role (expected to be sufficient)
*    cl_aunit_authority_check=>get_controller( )->reset( ).
*
*    " When
*    DATA(lv_authorization_granted) = f_cut->is_authorized( ).
*
*    " Then
*    cl_abap_unit_assert=>assert_true( lv_authorization_granted ).
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*CLASS tcl_bomload_rfc_destination DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    DATA f_cut     TYPE REF TO cl_surdp_uph_pckg_cmp_bom_load.  " class under test
*    DATA mr_double TYPE REF TO if_surdp_uph_factory.
*
*    METHODS:
*      setup,
*      teardown.
*
*    METHODS test_given_rfc_destination   FOR TESTING.
*    METHODS test_default_rfc_destination FOR TESTING.
*
*ENDCLASS.
*
*
*
*CLASS tcl_bomload_rfc_destination IMPLEMENTATION.
*  METHOD setup.
*    mr_double ?= cl_abap_testdouble=>create( 'if_surdp_uph_factory' ).
*    f_cut = NEW cl_surdp_uph_pckg_cmp_bom_load( io_surdp_uph_factory = mr_double ).
*  ENDMETHOD.
*
*
*  METHOD teardown.
*    CLEAR: f_cut.
*  ENDMETHOD.
*
*
*  METHOD test_given_rfc_destination.
*    DATA given_rfc_destination TYPE rfcdest VALUE 'AUNIT_DESTINATION'.
*    " Given
*    cl_abap_testdouble=>configure_call( mr_double  )->set_parameter( name = 'ev_endpoint_dest' value = given_rfc_destination ).
*    mr_double->get_target_destination( ).
*
*    " When
*    DATA(lv_rfc_dest) = f_cut->derive_rfc_target_destination( ).
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
*    DATA(lv_rfc_dest) = f_cut->derive_rfc_target_destination( ).
*
*    " Then
*    cl_abap_unit_assert=>assert_equals( exp = 'RDP_REPL_API' act = lv_rfc_dest ).
*  ENDMETHOD.
*ENDCLASS.
