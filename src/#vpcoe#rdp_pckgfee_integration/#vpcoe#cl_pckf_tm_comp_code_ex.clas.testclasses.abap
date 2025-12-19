*"* use this source file for your ABAP unit test classes
*
*CLASS /vpcoe/tcl_pckf_tm_pckg_fee_v1 DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*.
*  PRIVATE SECTION.
*    DATA:
*      f_cut TYPE REF TO /vpcoe/cl_pckf_tm_pckg_fee_v1.  "class under test
*
*    CLASS-METHODS: class_setup.
*    CLASS-METHODS: class_teardown.
*    METHODS: setup.
*    METHODS: teardown.
*    METHODS: evaluate_response FOR TESTING.
*    METHODS: get_entity_url_suffix FOR TESTING.
*    METHODS: symetric_parse_and_prepare FOR TESTING.
*    METHODS: parse_payload   FOR TESTING.
*    METHODS: parse_payload_with_context FOR TESTING.
*    METHODS: remove_prefix_context FOR TESTING.
*
*ENDCLASS.
*
*CLASS /vpcoe/cl_pckf_tm_pckg_fee_v1 DEFINITION LOCAL FRIENDS /vpcoe/tcl_pckf_tm_pckg_fee_v1.
*
*CLASS /vpcoe/tcl_pckf_tm_pckg_fee_v1 IMPLEMENTATION.
*
*  METHOD class_setup.
*  ENDMETHOD.
*
*  METHOD class_teardown.
*  ENDMETHOD.
*
*
*  METHOD setup.
*    f_cut = NEW #( ).
*  ENDMETHOD.
*
*
*  METHOD teardown.
*  ENDMETHOD.
*
*
*  METHOD evaluate_response.
*    DATA lv_response TYPE string.
*    DATA lt_messages TYPE /vpcoe/t_uph_msg.
*
*    f_cut->/vpcoe/if_pckf_transfer_mapper~evaluate_response(
*      EXPORTING
*        iv_response = lv_response
*     IMPORTING
*       et_messages = lt_messages
*    ).
*
*    lv_response = | \{ | &&
*    | "error":  | &&
*      | \{ | &&
*    | "code": 403,| &&
*    | "message": "FORBIDEN",| &&
*    | "details":[ | &&
*    | \{ | &&
*    | "code": "constraint_validation",| &&
*    | "message": "numeric value out of bounds (<16 digits>.<6 digits> expected)",| &&
*    | "target": "elements[0].baseQuantity"| &&
*    | \} | &&
*              | ] | &&
*    | \} | &&
*    | \} | .
*
*    f_cut->/vpcoe/if_pckf_transfer_mapper~evaluate_response(
*         EXPORTING
*           iv_response = lv_response
*        IMPORTING
*          et_messages = lt_messages
*       ).
*  ENDMETHOD.
*
*
*  METHOD get_entity_url_suffix.
*
*    DATA lv_entity_url_suffix TYPE string.
*
*    lv_entity_url_suffix = f_cut->/vpcoe/if_pckf_transfer_mapper~get_entity_url_suffix(  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_entity_url_suffix
*      exp   = 'PackagingFees'
*    ).
*  ENDMETHOD.
*
*  METHOD parse_payload.
*
*    Given
*    DATA lt_entity_data  TYPE /vpcoe/t_pckf_entity_data.
*    DATA lv_payload      TYPE string.
*
*    lv_payload = |\{"value":[\{"product":"FERT_RDP_GF_FG_MCA","validFrom":"2021-01-01","validTo":"9999-12-31",| &
*        |"reportConfiguration":"NO_UK_TEST_TETR","reportCategoryCountry":"UK","referenceQuantity":1000.00000000000000,| &
*        |"baseUnitOfMeasure":"PC","totalFee":55.00000000000000,"currency":"EUR","lastChangedAt":20220722063531.7480000,| &
*        |"applicableOrganizationData":[\{"companyCode":"R002"\}],"shipToPartyRole":["RETAILER"],"reportCategory":"GB_TAX_PACK_2022",| &
*        |"packagingFeeExtension":\{"productionOrderCheck":"false","inHouseProductionChargeableFee":10,"inHouseProductionStatisticalFee":12,| &
*        |"taxForPackagingAsMaterial":15,"exemptions":[\{"code":"75_a1"\}],"plasticWeightInKg":14,"nonRecycledPlasticInKg":10\}, | &
*        |"suppliers":[\{"supplier":"customer","isExcludedFromReportConfiguration":"true"\}],"isFallbackFee":"false"\}],"nextLink":"",| &
*        |"deltaLink":"/PackagingFees?deltaToken=bGFzdE1vZGlmaWVkQXQ9MjAyMi0wOC0xOCAwOTo0NDozNC43NTM2ODgwMDAmcGs9ZmUxMDRkNGQtODQyMi00YzRiLTk3M2EtZDNkZDYwNjNiZWRh"\}|.
*
*    When
*    f_cut->/vpcoe/if_pckf_transfer_mapper~parse_payload(
*      EXPORTING
*        iv_payload = lv_payload
*      IMPORTING
*        et_entity_data = lt_entity_data
*        ev_next_link = DATA(lv_next_link)
*        ev_delta_link = DATA(lv_delta_link) ).
*
*    Then
*    cl_abap_unit_assert=>assert_initial( act = lv_next_link ).
*    cl_abap_unit_assert=>assert_not_initial( act = lt_entity_data ).
*    cl_abap_unit_assert=>assert_not_initial( act = lv_delta_link ).
*    cl_abap_unit_assert=>assert_equals( act  = lv_delta_link
*                                         exp = '/PackagingFees?deltaToken=bGFzdE1vZGlmaWVkQXQ9MjAyMi0wOC0xOCAwOTo0NDozNC43NTM2ODgwMDAmcGs9ZmUxMDRkNGQtODQyMi00YzRiLTk3M2EtZDNkZDYwNjNiZWRh' ).
*
*  ENDMETHOD.
*
*  METHOD parse_payload_with_context.
*
*    Given
*    DATA lt_entity_data  TYPE /vpcoe/t_pckf_entity_data.
*    DATA lv_payload      TYPE string.
*
*    lv_payload = |\{"value": [| &
*    |\{| &
*      |"Context": "SYSGRP1",| &
*      |"ProductId": "SYSGRP1:IT_ES_CLR_PRO_001",| &
*      |"Product": "SYSGRP1:IT_ES_CLR_PRO_001",| &
*      |"ValidFrom": "20230101",| &
*      |"ValidTo": "99991231",| &
*      |"ReportConfigurationId": "SK_SG1_S1_QM7_002",| &
*      |"ReportConfiguration": "SK_SG1_S1_QM7_002",| &
*      |"ReportCategoryId": "ES_TAX_PACK_2023",| &
*      |"ReportCategory": "ES_TAX_PACK_2023",| &
*      |"ReportCategoryCountry": "SYSGRP1:ES",| &
*      |"ReportCategoryRegion": null,| &
*      |"PackagingComposition": "SYSGRP1:IT_ES_PC_CLR_001_1",| &
*      |"BusinessProcessDirection": "INBOUND",| &
*      |"ApplicableOrganizationData": [| &
*      |  \{| &
*      |    "CompanyCode": "SYSGRP1:ES01",| &
*      |    "SalesOrganization": "SYSGRP1:ES01",| &
*      |    "DistributionChannel": "SYSGRP1:01",| &
*      |    "Division": "SYSGRP1:01"| &
*      |  \}| &
*      |],| &
*      |"PackagingFeeExtension": \{| &
*      |  "ProductionOrderCheck": true,| &
*      |  "InhouseProductionChargeableFee": 1.5,| &
*      |  "InhouseProductionStatisticalFee": 99.0,| &
*      |  "TaxForPackagingAsMaterial": 2.5,| &
*      |  "Exemptions": [| &
*      |    \{| &
*      |      "Code": "73c"| &
*      |    \}| &
*      |  ],| &
*      |  "PlasticWeightInKg": 370.0,| &
*      |  "NonRecycledPlasticInKg": 370.0| &
*      |\},| &
*      |"Suppliers": [],| &
*      |"IsFallbackFee": true,| &
*      |"ReferenceQuantity": 1000,| &
*      |"BaseUnitOfMeasure": "SYSGRP1:PC",| &
*      |"BaseUnitOfMeasureISOCode": "PCE",| &
*      |"ShipToPartyRole": [| &
*      |  "PROF_CUSTM",| &
*      |  "RETAILER",| &
*      |  "HOSPITAL",| &
*      |  "END_CONSUM"| &
*      |],| &
*      |"TotalFee": 99.0,| &
*      |"Currency": "SYSGRP1:EUR",| &
*      |"WeightInKg": 370.0,| &
*      |"NonRecycledPlasticInKG": 370.0,| &
*      |"ExemptionCode": [| &
*      |  "73c"| &
*      |],| &
*      |"LastChangedAt": "2023-07-28T07:05:58.243+00:00"| &
*    |\}  ],| &
*  |"NextLink": null,| &
*  |"DeltaLink": "/PackagingFees?deltaToken=bGFzdE1vZGlmaWVkQXQ9MjAyMy0wNy0yOCAwOToxOTozOC4zMTYzNTMmQ29udGV4dD1TWVNHUlAxLFNZU0dSUDI%3D&%24expand=PackagingFeeExtension%2CSuppliers"| &
*|\}|.
*
*    When
*    f_cut->/vpcoe/if_pckf_transfer_mapper~parse_payload(
*      EXPORTING
*        iv_payload = lv_payload
*      IMPORTING
*        et_entity_data = lt_entity_data
*        ev_next_link = DATA(lv_next_link)
*        ev_delta_link = DATA(lv_delta_link) ).
*
*    Then
*    DATA(ls_payload_data) = VALUE /vpcoe/s_pckf_ent_pckg_fee(
*      product  = 'IT_ES_CLR_PRO_001'
*      validfrom = '20230101'
*      validto = '99991231'
*      reportconfiguration = 'SK_SG1_S1_QM7_002'
*      reportcategorycountry = 'ES'
*      reportcategoryregion  =  ''
*      packagingcomposition = 'IT_ES_PC_CLR_001_1'
*      referencequantity = '1000'
*      baseunitofmeasure = 'PC'
*      totalfee  = '99.00'
*      currency = 'EUR'
*      lastchangeddate =  '20230728070558.2430000'
*      applicableorganizationdata = VALUE #( ( companycode = 'ES01' salesorganization = 'ES01' division = '01' distributionchannel = '01'  ) )
*      shiptopartyrole = VALUE #(
*                                  ( |PROF_CUSTM| )
*                                  ( |RETAILER| )
*                                  ( |HOSPITAL| )
*                                  ( |END_CONSUM| )
*                                )
*      reportcategory  = 'ES_TAX_PACK_2023'
*      suppliers        = VALUE #( )
*      isfallbackfee    = 'X'
*      context         = 'SYSGRP1'
*      business_process_direction = 'INBOUND'
*      packagingfeeextension   = VALUE #(
*      productionordercheck           = 'X'
*      inhouseproductionchargeablefee = '1.5'
*      inhouseprodnstatisticalfee     = '99.0'
*      taxforpackagingasmaterial      = '2.5'
*      exemptions = VALUE #( ( code = '73c' ) )
*      plasticweightinkg               = '370.0'
*      nonrecycledplasticinkg         = '370.0' )
*      ).
*
*    cl_abap_unit_assert=>assert_initial( act = lv_next_link ).
*    cl_abap_unit_assert=>assert_not_initial( act = lt_entity_data ).
*    cl_abap_unit_assert=>assert_equals( act = CAST /vpcoe/cl_pckf_ent_pckg_fee( lt_entity_data[ 1 ] )->get_data( ) exp = ls_payload_data ).
*    cl_abap_unit_assert=>assert_not_initial( act = lv_delta_link ).
*    cl_abap_unit_assert=>assert_equals( act  = lv_delta_link
*                                         exp = '/PackagingFees?deltaToken=bGFzdE1vZGlmaWVkQXQ9MjAyMy0wNy0yOCAwOToxOTozOC4zMTYzNTMmQ29udGV4dD1TWVNHUlAxLFNZU0dSUDI%3D&%24expand=PackagingFeeExtension%2CSuppliers' ).
*
*  ENDMETHOD.
*
*  METHOD symetric_parse_and_prepare.
*
*    Given
*    DATA lv_payload_exp      TYPE string.
*    DATA lt_entity_data      TYPE /vpcoe/t_pckf_entity_data.
*    DATA ls_parameters       TYPE /vpcoe/s_pckf_ent_pckg_fee.
*    DATA lv_payload_act      TYPE string.
*
*    ls_parameters-context = 'SYSGRP1'.
*    ls_parameters-business_process_direction = 'INBOUND'.
*    ls_parameters-product             = 'FERT_RDP_GF_FG_MCA'.
*    ls_parameters-validfrom             = '20210101'.
*    ls_parameters-validto               = '99991231'.
*    ls_parameters-reportconfiguration = 'NO_UK_TEST_TETR'.
*    ls_parameters-reportcategory      = 'GB_TAX_PACK_2022'.
*    ls_parameters-reportcategorycountry = 'UK'.
*    ls_parameters-reportcategoryregion  = ' '.
*    ls_parameters-packagingcomposition = 'PC_GF_FG_CMI_V03'.
*    ls_parameters-applicableorganizationdata = VALUE #( ( companycode = 'R002' salesorganization = 'RDE1' division = 'S1' distributionchannel = '01'  ) ).
*    ls_parameters-referencequantity     = '1000'.
*    ls_parameters-baseunitofmeasure     = 'PC'.
*    APPEND 'RETAILER' TO ls_parameters-shiptopartyrole.
*    ls_parameters-totalfee              = '55.00000000000000'.
*    ls_parameters-currency              = 'EUR'.
*    ls_parameters-packagingfeeextension-productionordercheck  = abap_true.
*    ls_parameters-packagingfeeextension-inhouseproductionchargeablefee = 10.
*    ls_parameters-packagingfeeextension-inhouseprodnstatisticalfee     = 12.
*    ls_parameters-packagingfeeextension-taxforpackagingasmaterial      = 15.
*    ls_parameters-packagingfeeextension-exemptions                     = VALUE #( ( code = '75_a1' ) ).
*    ls_parameters-packagingfeeextension-plasticweightinkg              = 14.
*    ls_parameters-packagingfeeextension-nonrecycledplasticinkg         = 10.
*    ls_parameters-suppliers                      = VALUE #( ( supplier = 'VENDOR_1' is_excluded_from_report_config = abap_true ) ).
*    ls_parameters-isfallbackfee                                        = abap_true.
*    ls_parameters-lastchangeddate       = '20220722063531.7480000'.
*
*
*    lv_payload_exp = |\{"value":[\{"context":"SYSGRP1","product":"FERT_RDP_GF_FG_MCA","validFrom":"2021-01-01","validTo":"9999-12-31","reportConfiguration":"NO_UK_TEST_TETR","reportCategoryCountry":"UK",| &
*    |"referenceQuantity":1000.00000000000000,"baseUnitOfMeasure":"PC","totalFee":55.00000000000000,"currency":"EUR","lastChangedAt":"2022-07-22T06:35:31.7480000Z","applicableOrganizationData":[\{"companyCode":"R002","salesOrganization":"RDE1",| &
*    |"division":"S1","distributionChannel":"01"\}],"shipToPartyRole":["RETAILER"],"reportCategory":"GB_TAX_PACK_2022","packagingComposition":"PC_GF_FG_CMI_V03","businessProcessDirection":"INBOUND",| &
*    |"packagingFeeExtension":\{"productionOrderCheck":true,"InhouseProductionChargeableFee":10.00000000000000,"InhouseProductionStatisticalFee":12.00000000000000,"taxForPackagingAsMaterial":15.00000000000000,"plasticWeightInKg":14.00000000000000,| &
*    |"nonRecycledPlasticInKg":10.00000000000000,"exemptions":[\{"code":"75_a1"\}]\},"suppliers":[\{"supplier":"VENDOR_1","isExcludedFromReportConfig":true\}],"isFallbackFee":true\}]\}|.
*
*    DATA(lo_pack_fee) = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_parameters iv_deleted = '' ).
*    INSERT lo_pack_fee INTO TABLE lt_entity_data.
*
*    When
*    lv_payload_act = f_cut->/vpcoe/if_pckf_transfer_mapper~prepare_payload(
*       it_entity_data = lt_entity_data
*       is_parameters  = ls_parameters ).
*
*    CLEAR lt_entity_data.
*
*    f_cut->/vpcoe/if_pckf_transfer_mapper~parse_payload(
*      EXPORTING
*        iv_payload = lv_payload_act
*      IMPORTING
*        et_entity_data = lt_entity_data
*        ev_next_link = DATA(lv_next_link)
*        ev_delta_link = DATA(lv_delta_link) ).
*
*    Then
*    cl_abap_unit_assert=>assert_not_initial( act = lv_payload_exp ).
*    cl_abap_unit_assert=>assert_equals( act = lv_payload_act
*                                        exp = lv_payload_exp ).
*
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_entity_data ) exp = 1 ).
*    cl_abap_unit_assert=>assert_equals( act = CAST /vpcoe/cl_pckf_ent_pckg_fee( lt_entity_data[ 1 ] )->get_data( ) exp = ls_parameters ).
*
*  ENDMETHOD.
*
*  METHOD remove_prefix_context.
*    given
*    when
*    DATA(lv_result_with_ctx) = f_cut->remove_prefix_context( iv_attribute = 'SG1:prod_1'
*                                                            iv_prefix    = 'SG1' ).
*
*    DATA(lv_result_no_ctx) = f_cut->remove_prefix_context( iv_attribute = 'PROD2'
*                                                        iv_prefix    = '' ).
*
*    DATA(lv_result_no_attrib) = f_cut->remove_prefix_context( iv_attribute = ''
*                                                        iv_prefix    = 'SG1' ).
*
*    then
*    cl_abap_unit_assert=>assert_equals( act = lv_result_with_ctx
*                                        exp = 'prod_1' ).
*
*    cl_abap_unit_assert=>assert_equals( act = lv_result_no_ctx
*                                        exp = 'PROD2' ).
*
*    cl_abap_unit_assert=>assert_equals( act = lv_result_no_attrib
*                                        exp = '' ).
*  ENDMETHOD.
*
*ENDCLASS.
