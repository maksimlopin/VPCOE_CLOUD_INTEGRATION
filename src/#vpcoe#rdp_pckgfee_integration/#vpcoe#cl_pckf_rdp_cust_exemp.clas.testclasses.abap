*CLASS /vpcoe/ltcl_pckf_proc_pckg_fee DEFINITION DEFERRED.
*CLASS /vpcoe/cl_pckf_proc_pckg_fee DEFINITION LOCAL FRIENDS /vpcoe/ltcl_pckf_proc_pckg_fee.
*
*CLASS /vpcoe/ltcl_pckf_proc_pckg_fee DEFINITION FOR TESTING RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    DATA:
*      mo_cut TYPE REF TO /vpcoe/cl_pckf_proc_pckg_fee.  "class under test
*
*    METHODS:
*      setup,
*      prepare_query_parameter FOR TESTING,
*      prepare_retrieve_delta FOR TESTING,
*      prepare_retrieve_full FOR TESTING,
*      deserialize_selection_params FOR TESTING.
*
*ENDCLASS.
*
*CLASS /vpcoe/ltcl_pckf_proc_pckg_fee IMPLEMENTATION.
*
*  METHOD setup.
*
*    DATA(lo_factory_double) = NEW /vpcoe/td_pckf_factory(  ).
*    /vpcoe/th_pckf_factory_inject=>inject_factory_double( io_double = lo_factory_double ).
*
*    lo_factory_double->set_entity_processor( io_double = mo_cut ).
*    lo_factory_double->set_entity_mapper( io_double = NEW /vpcoe/cl_pckf_tm_pckg_fee_v1( ) ).
*
*    mo_cut = NEW /vpcoe/cl_pckf_proc_pckg_fee( ).
*
*  ENDMETHOD.
*
*  METHOD prepare_query_parameter.
*
*    DATA: ls_parameters TYPE /vpcoe/s_pckf_retrieval_input,
*          lv_payload    TYPE string.
*
*    "Case 1:
*
*    "Given
*    ls_parameters-report_config_id = VALUE #( ( sign = 'I' option = 'EQ' low = 'TEST_RDP_E2E_GB_ECC' ) ).
*    ls_parameters-context = VALUE #( ( sign = 'I' option = 'EQ' low = 'RDP_DEFAULT' ) ( sign = 'I' option = 'EQ' low = 'SYSGRP2' ) ).
*    ls_parameters-bus_proc_dir = 'OUTBOUND'.
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_upload_mode   = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters = ls_parameters ).
*
*    "When
*    mo_cut->prepare_query_parameter(
*      IMPORTING
*        ev_uri_suffix   = DATA(lv_uri_suffix)
*        ev_query_string = DATA(lv_query_string)
*    ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  =  lv_uri_suffix
*        exp                  =  'PackagingFees'
*    ).
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  =  lv_query_string
*        exp                  =  'ReportConfiguration=TEST_RDP_E2E_GB_ECC&Context=RDP_DEFAULT%2cSYSGRP2&BusinessProcessDirection=OUTBOUND&$expand=PackagingFeeExtension,Suppliers'
*    ).
*
*    "Case 2:
*
*    "Given
*    CLEAR ls_parameters.
*    ls_parameters-test_mode = abap_true.
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_upload_mode   = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters = ls_parameters ).
*
*    "When
*    mo_cut->prepare_query_parameter(
*      IMPORTING
*        ev_uri_suffix   = lv_uri_suffix
*        ev_query_string = lv_query_string
*    ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  =  lv_query_string
*        exp                  =  '$expand=PackagingFeeExtension,Suppliers'
*    ).
*
*  ENDMETHOD.
*
*  METHOD prepare_retrieve_delta.
** In case of a delta load, test prepare is retrieving last inital/delta load entry
*
*    DATA: lv_delta_link   TYPE string,
*          lv_rfc_des      TYPE string,
*          lv_package_size TYPE i.
*
*    "Given
*    DATA lt_protocol TYPE STANDARD TABLE OF /vpcoe/pckf_prot.
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*      EXPORTING
*        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta
*    ).
*
*    lt_protocol = VALUE #( (
*        uuid = '0050568B293A1EED9DB012C5E52471B5'
*        entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        start_timestamp = '20221206171105.0000000'
*        end_timestamp = '20221206171107.0000000'
*        failed = ''
*        selection = '{"rfc_des":"VPCOE_RDP_PCKF","delta_link":"bGFzdE1vZGlmaWVkQXQ9MjAyMi0xMi0wNiAxMDoyNDowNS43MDc2OTg=","package_size":500}'
*     ) ).
*
*    DATA(lo_protocol_access) = /vpcoe/cl_pckf_factory=>get_instance( )->get_protocol_access( ).
*    cl_abap_testdouble=>configure_call( double = lo_protocol_access )->returning( value = lt_protocol ).
*    lo_protocol_access->read_from_protocol(
*      EXPORTING
*        iv_entity_type  = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_only_success = abap_true
*    ).
*
*    "When
*    DATA(lv_record_cnt_exp) = mo_cut->/vpcoe/if_pckf_entity_proc~prepare_retrieve( ).
*
*    "Then
*    DATA(lr_parameters) = mo_cut->/vpcoe/if_pckf_entity_proc~get_parameters( ).
*    cl_abap_unit_assert=>assert_bound(
*      EXPORTING
*        act              =  lr_parameters
*    ).
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~get_parameter_value(
*      EXPORTING
*        iv_name  = /vpcoe/if_pckf_entity_proc=>gc_parameters-delta_link
*      IMPORTING
*        ev_value = lv_delta_link
*    ).
*    mo_cut->/vpcoe/if_pckf_entity_proc~get_parameter_value(
*      EXPORTING
*        iv_name  = /vpcoe/if_pckf_entity_proc=>gc_parameters-rfc_des
*      IMPORTING
*        ev_value = lv_rfc_des
*    ).
*    mo_cut->/vpcoe/if_pckf_entity_proc~get_parameter_value(
*      EXPORTING
*        iv_name  = /vpcoe/if_pckf_entity_proc=>gc_parameters-package_size
*      IMPORTING
*        ev_value = lv_package_size
*    ).
*
*    cl_abap_unit_assert=>assert_equals( act = lv_delta_link exp = 'bGFzdE1vZGlmaWVkQXQ9MjAyMi0xMi0wNiAxMDoyNDowNS43MDc2OTg=' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_rfc_des exp = 'VPCOE_RDP_PCKF' ).
*    cl_abap_unit_assert=>assert_equals( act = lv_package_size exp = 500 ).
*
*  ENDMETHOD.
*
*  METHOD prepare_retrieve_full.
** In case of a full load, check prepare is doing nothing
*
*    DATA: lv_delta_link TYPE string.
*
*    "Given
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*      EXPORTING
*        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*    ).
*    "When
*    DATA(lv_record_cnt_exp) = mo_cut->/vpcoe/if_pckf_entity_proc~prepare_retrieve( ).
*
*    "Then
*    mo_cut->/vpcoe/if_pckf_entity_proc~get_parameter_value(
*      EXPORTING
*        iv_name  = /vpcoe/if_pckf_entity_proc=>gc_parameters-delta_link
*      IMPORTING
*        ev_value = lv_delta_link
*    ).
*
*    cl_abap_unit_assert=>assert_initial( act = lv_delta_link ).
*
*  ENDMETHOD.
*
*  METHOD deserialize_selection_params.
** Test deserialization of selection parameters
*
*    "Given
*    DATA lv_json_str TYPE string.
*    DATA ls_selection_params_act TYPE /vpcoe/s_pckf_retrieval_input.
*    DATA ls_selection_params_exp TYPE /vpcoe/s_pckf_retrieval_input.
*
*    lv_json_str = |\{| &&
*                  |"report_categ_id":[\{"sign":"I","option":"EQ","low":"GB_TAX_PACK_2022"\}],| &&
*                  |"report_config_id":[\{"sign":"I","option":"EQ","low":"TEST_RDP_E2E_GB_ECC"\}],| &&
*                  |"country":"DE",| &&
*                  |"region":"BW",| &&
*                  |"ship_parrole":[\{"sign":"I","option":"EQ","low":"RETAILER"\}],| &&
*                  |"rfc_des":"VPCOE_RDP_PCKF",| &&
*                  |"nextlink":"",| &&
*                  |"deltalink":"",| &&
*                  |"package_size":"5",| &&
*                  |"test_mode":"X"| &&
*                  |\}|.
*
*    ls_selection_params_exp-report_categ_id = VALUE #( ( sign = 'I' option = 'EQ' low = 'GB_TAX_PACK_2022' ) ).
*    ls_selection_params_exp-report_config_id = VALUE #( ( sign = 'I' option = 'EQ' low = 'TEST_RDP_E2E_GB_ECC' ) ).
*    ls_selection_params_exp-country = 'DE'.
*    ls_selection_params_exp-region = 'BW'.
*    ls_selection_params_exp-ship_parrole = VALUE #( ( sign = 'I' option = 'EQ' low = 'RETAILER' ) ).
*    ls_selection_params_exp-rfc_des = 'VPCOE_RDP_PCKF'.
*    ls_selection_params_exp-next_link = ''.
*    ls_selection_params_exp-delta_link = ''.
*    ls_selection_params_exp-package_size = '5'.
*    ls_selection_params_exp-test_mode = abap_true.
*
*    "When
*    mo_cut->/vpcoe/if_pckf_entity_proc~deserialize_selection_params( EXPORTING iv_json_str = lv_json_str
*                                                                     IMPORTING es_selection_params = ls_selection_params_act ).
*
*    "Then
*    cl_abap_unit_assert=>assert_not_initial( ls_selection_params_act ).
*    cl_abap_unit_assert=>assert_equals( act = ls_selection_params_act exp = ls_selection_params_exp ).
*  ENDMETHOD.
*
*ENDCLASS.
