*CLASS /vpcoe/ltcl_pckf_proc_ip DEFINITION FOR TESTING INHERITING FROM /vpcoe/cl_pckf_proc_base.
*
*  PUBLIC SECTION.
*
*    METHODS: constructor
*      IMPORTING
*        !io_rdp_http TYPE REF TO /vpcoe/cl_pckgfee_http OPTIONAL.
*
*ENDCLASS.
*
*CLASS /vpcoe/ltcl_pckf_proc_ip IMPLEMENTATION.
*
*  METHOD constructor.
*    super->constructor(  ).
*
*    "inject http communication util
*    IF io_rdp_http IS BOUND.
*      mo_rdp_http = io_rdp_http.
*    ENDIF.
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS /vpcoe/ltcl_pckf_proc_base DEFINITION DEFERRED.
*CLASS /vpcoe/cl_pckf_proc_base DEFINITION LOCAL FRIENDS /vpcoe/ltcl_pckf_proc_base.
*
*CLASS /vpcoe/ltcl_pckf_proc_base DEFINITION FOR TESTING RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    DATA: mt_entity_data_pckg_fee TYPE /vpcoe/t_pckf_entity_data,
*
*          mv_entity_type          TYPE /vpcoe/pckf_entity,
*          mv_upload_mode          TYPE /vpcoe/upload_mode,
*          mo_comm_util            TYPE REF TO /vpcoe/td_http_communic, "/vpcoe/td_http_communication,
*          mo_cut                  TYPE REF TO /vpcoe/ltcl_pckf_proc_ip.  "class under test
*
*    METHODS:
*      setup,
*      init_testdata,
*      test_init_processor FOR TESTING,
*      get_entity   FOR TESTING,
*      get_mode FOR TESTING,
*      prepare_retrieve FOR TESTING,
*      deserialize_selection_params FOR TESTING,
*      prepare_query_parameter_init FOR TESTING,
*      prepare_query_parameter_next FOR TESTING,
*      prepare_query_parameter_delta FOR TESTING,
*      retrieve_package FOR TESTING.
*
*ENDCLASS.
*
*CLASS /vpcoe/ltcl_pckf_proc_base IMPLEMENTATION.
*
*  METHOD setup.
*
*    DATA(lo_factory_double) = NEW /vpcoe/td_pckf_factory(  ).
*    /vpcoe/th_pckf_factory_inject=>inject_factory_double( io_double = lo_factory_double ).
*
*    "Inject communication util double
*    IF mo_comm_util IS NOT BOUND.
*      TRY.
*          mo_comm_util = NEW /vpcoe/td_http_communic( )."/vpcoe/td_http_communication( ).
*        CATCH cx_oa2c INTO DATA(lx_ecx).
*      ENDTRY.
*    ENDIF.
*
*    mo_cut = NEW /vpcoe/ltcl_pckf_proc_ip( io_rdp_http = mo_comm_util ).
*
*    lo_factory_double->set_entity_processor( io_double = mo_cut ).
*    lo_factory_double->set_entity_mapper( io_double = NEW /vpcoe/cl_pckf_tm_pckg_fee_v1( ) ).
*    init_testdata( ).
*
*  ENDMETHOD.
*
*  METHOD init_testdata.
*
*    APPEND INITIAL LINE TO mt_entity_data_pckg_fee REFERENCE INTO DATA(lo_entity_data_pckg_fee).
*    DATA(ls_ent_data_pckf_fee_11) = VALUE /vpcoe/s_pckf_ent_pckg_fee(
*         product = 'TEST_MAT_VS4'
*         validfrom = '20220101'
*         validto = '20221231'
*         reportconfiguration = 'REP_CFG1'
*         reportcategory = 'REP_CAT1'
*         reportcategorycountry = 'DE'
*         reportcategoryregion = 'BW'
*         applicableorganizationdata = VALUE #( ( companycode = '1020' salesorganization = 'DE01'
*                                division = '10'
*                                distributionchannel = '10'   ) )
*         referencequantity = '1.0'
*         baseunitofmeasure = 'PC'
*         totalfee = '11.5'
*         currency = 'EUR' ).
**APPEND 'RETAILER' TO ls_ent_data_pckf_fee_11-shiptopartyrole.
*    GET TIME STAMP FIELD ls_ent_data_pckf_fee_11-lastchangeddate.
*
*    lo_entity_data_pckg_fee->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_data_pckf_fee_11  ).
*
*    APPEND INITIAL LINE TO mt_entity_data_pckg_fee REFERENCE INTO lo_entity_data_pckg_fee.
*    DATA(ls_ent_data_pckf_fee_18) = VALUE /vpcoe/s_pckf_ent_pckg_fee(
*         product = 'TEST_MAT_UNKOWN'
*         validfrom = '20220101'
*         validto = '20221231'
*         reportconfiguration = 'REP_CFG1'
*         reportcategory = 'REP_CAT1'
*         reportcategorycountry = 'DE'
*         reportcategoryregion = 'BW'
*         applicableorganizationdata = VALUE #( ( companycode = '1020' salesorganization = 'DE01'
*                                division = '10'
*                                distributionchannel = '10'   ) )
*         referencequantity = '1.0'
*         baseunitofmeasure = 'PC'
*         totalfee = '18.5'
*         currency = 'EUR' ).
*    GET TIME STAMP FIELD ls_ent_data_pckf_fee_18-lastchangeddate.
*
*    lo_entity_data_pckg_fee->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_data_pckf_fee_18  ).
*
*    APPEND INITIAL LINE TO mt_entity_data_pckg_fee REFERENCE INTO lo_entity_data_pckg_fee.
*    DATA(ls_ent_data_pckf_fee_31) = VALUE /vpcoe/s_pckf_ent_pckg_fee(
*         product = 'TEST_MAT_VS4'
*         validfrom = '20220101'
*         validto = '20221231'
*         reportconfiguration = 'REP_CFG2'
*         reportcategory = 'REP_CAT1'
*         reportcategorycountry = 'DE'
*         reportcategoryregion = 'BW'
*         applicableorganizationdata = VALUE #( ( companycode = '1020' salesorganization = 'DE01'
*                                division = '10'
*                                distributionchannel = '10'   ) )
*         referencequantity = '1.0'
*         baseunitofmeasure = 'PC'
*         totalfee = '5.5'
*         currency = 'EUR' ).
*    GET TIME STAMP FIELD ls_ent_data_pckf_fee_31-lastchangeddate.
*
*    lo_entity_data_pckg_fee->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_data_pckf_fee_31  ).
*
*  ENDMETHOD.
*
*  METHOD test_init_processor.
*
*    "Given
*    CHECK mo_cut IS BOUND.
*
*    "When
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_upload_mode   = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta
*        is_parameters = VALUE /vpcoe/s_pckf_retrieval_input(  ) ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals( act = mo_cut->/vpcoe/if_pckf_entity_proc~get_entity(  ) exp = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).
*    cl_abap_unit_assert=>assert_equals( act = mo_cut->/vpcoe/if_pckf_entity_proc~get_mode(  ) exp = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta ).
*
*  ENDMETHOD.
*
*  METHOD get_entity.
*
*    "Given
*    CHECK mo_cut IS BOUND.
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*       iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*       iv_upload_mode   = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta
*   ).
*    "When
*    DATA(lv_upload_entity) = mo_cut->/vpcoe/if_pckf_entity_proc~get_entity( ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*       act   = lv_upload_entity
*       exp   = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*     ).
*
*  ENDMETHOD.
*
*
*  METHOD get_mode.
*
*    "Given
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*       iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*       iv_upload_mode   = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta
*    ).
*
*    "When
*    DATA(lv_upload_mode) = mo_cut->/vpcoe/if_pckf_entity_proc~get_mode( ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_upload_mode
*      exp   =  /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta
*    ).
*  ENDMETHOD.
*
*  METHOD prepare_retrieve.
*    DATA(lv_record_cnt) = mo_cut->/vpcoe/if_pckf_entity_proc~prepare_retrieve( ).
*  ENDMETHOD.
*
*  METHOD deserialize_selection_params.
*    DATA:lv_json_str TYPE string.
*    mo_cut->/vpcoe/if_pckf_entity_proc~deserialize_selection_params( lv_json_str ).
*  ENDMETHOD.
*
*  METHOD retrieve_package.
*
*    DATA: ls_parameters TYPE /vpcoe/s_pckf_retrieval_input,
*          lv_payload    TYPE string.
*
*    "Given
*    ls_parameters-package_size = 10.
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_upload_mode   = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters = ls_parameters ).
*
*    DATA(lr_entity) = mt_entity_data_pckg_fee[ 1 ].
*
*    lv_payload = lr_entity->entity_to_json( ).
*
*    mo_comm_util->set_result(
*      EXPORTING
*        iv_status  = /vpcoe/if_plm_constants=>gc_ok_response
*        iv_payload = lv_payload
*    ).
*
*    "When
*    DATA(lt_entity_data) = mo_cut->/vpcoe/if_pckf_entity_proc~retrieve_package(
*                     iv_act_package  = 1
*                     iv_package_size = ls_parameters-package_size
*                 ).
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  =  lines( lt_entity_data )
*        exp                  =  1
*    ).
*
*    DATA(lv_last_uri_suffix) = mo_comm_util->get_last_uri_suffix( ).
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  =  lv_last_uri_suffix
*        exp                  =  'PackagingFees'
*    ).
*
*  ENDMETHOD.
*
*  METHOD prepare_query_parameter_init.
*
*    DATA: ls_parameters TYPE /vpcoe/s_pckf_retrieval_input,
*          lv_payload    TYPE string.
*
*    "Given
*    ls_parameters-report_categ_id = VALUE #( ( sign = 'I' option = 'EQ' low = 'GB_TAX_PACK_2022' ) ).
*    ls_parameters-report_config_id = VALUE #( ( sign = 'I' option = 'EQ' low = 'TEST_RDP_E2E_GB_ECC' ) ).
*    ls_parameters-country = 'GB'.
*    ls_parameters-delta_link = ''.
*    ls_parameters-next_link = ''.
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_upload_mode   = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters = ls_parameters ).
*
*    "When
*    CAST /vpcoe/cl_pckf_proc_base( mo_cut )->prepare_query_parameter(
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
*        exp                  =  'ReportConfiguration=TEST_RDP_E2E_GB_ECC&ReportCategory=GB_TAX_PACK_2022&ReportCategoryCountry=GB'
*    ).
*
*  ENDMETHOD.
*
*  METHOD prepare_query_parameter_next.
*    "Test next link
*    DATA: ls_parameters TYPE /vpcoe/s_pckf_retrieval_input,
*          lv_payload    TYPE string.
*
*    "Given
*    ls_parameters-report_categ_id = VALUE #( ( sign = 'I' option = 'EQ' low = 'GB_TAX_PACK_2022' ) ).
*    ls_parameters-report_config_id = VALUE #( ( sign = 'I' option = 'EQ' low = 'TEST_RDP_E2E_GB_ECC' ) ).
*    ls_parameters-country = 'GB'.
*    ls_parameters-delta_link = ''.
*    ls_parameters-next_link = ''.
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_upload_mode   = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters = ls_parameters ).
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~set_parameter_value(
*      EXPORTING
*        iv_name  = /vpcoe/if_pckf_entity_proc=>gc_parameters-next_link
*        iv_value = '/PackagingFees?nextLinkToken=bGFzdE1vZGlmaWVkQXQ9MjAyMi0xMS0yMyAxNToxNjo1Ny43MzgwOTUmbGFzdFBLPWExYzA5YThhLTYwZDctNGRlMC1hOTM3LWZiNDg3MGIzNDg3MCZzaGlwVG9QYXJ0eVJvbGU9W05PX1JPTEVd'
*    ).
*
*    CAST /vpcoe/cl_pckf_proc_base( mo_cut )->prepare_query_parameter(
*      IMPORTING
*        ev_uri_suffix   = DATA(lv_uri_suffix)
*        ev_query_string = DATA(lv_query_string)
*    ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  =  lv_query_string
*        exp                  =  'nextLinkToken=bGFzdE1vZGlmaWVkQXQ9MjAyMi0xMS0yMyAxNToxNjo1Ny43MzgwOTUmbGFzdFBLPWExYzA5YThhLTYwZDctNGRlMC1hOTM3LWZiNDg3MGIzNDg3MCZzaGlwVG9QYXJ0eVJvbGU9W05PX1JPTEVd'
*    ).
*
*  ENDMETHOD.
*
*  METHOD prepare_query_parameter_delta.
*    "Test delta link
*    DATA: ls_parameters TYPE /vpcoe/s_pckf_retrieval_input,
*          lv_payload    TYPE string.
*
*    "Given
*    ls_parameters-report_categ_id = VALUE #( ( sign = 'I' option = 'EQ' low = 'GB_TAX_PACK_2022' ) ).
*    ls_parameters-report_config_id = VALUE #( ( sign = 'I' option = 'EQ' low = 'TEST_RDP_E2E_GB_ECC' ) ).
*    ls_parameters-country = 'GB'.
*    ls_parameters-delta_link = ''.
*    ls_parameters-next_link = ''.
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~init_processor(
*        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*        iv_upload_mode   = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta
*        is_parameters = ls_parameters ).
*
*    mo_cut->/vpcoe/if_pckf_entity_proc~set_parameter_value(
*      EXPORTING
*        iv_name  = /vpcoe/if_pckf_entity_proc=>gc_parameters-delta_link
*        iv_value = '/PackagingFees?deltaToken=bGFzdE1vZGlmaWVkQXQ9MjAyMi0xMS0yMyAxNToxNjo1Ny43MzgwOTU='
*    ).
*
*    CAST /vpcoe/cl_pckf_proc_base( mo_cut )->prepare_query_parameter(
*      IMPORTING
*        ev_uri_suffix   = DATA(lv_uri_suffix)
*        ev_query_string = DATA(lv_query_string)
*    ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  =  lv_query_string
*        exp                  =  'deltaToken=bGFzdE1vZGlmaWVkQXQ9MjAyMi0xMS0yMyAxNToxNjo1Ny43MzgwOTU='
*    ).
*
*  ENDMETHOD.
*
*ENDCLASS.
