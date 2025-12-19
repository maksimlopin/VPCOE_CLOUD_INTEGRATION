CLASS /vpcoe/cl_delivery_data_ut DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      f_cut   TYPE REF TO /vpcoe/cl_rdp_delivery_data,
      lo_cust TYPE REF TO /vpcoe/cl_rdp_helper.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: get_delivery FOR TESTING.
    METHODS: build_json FOR TESTING.

ENDCLASS.

CLASS /vpcoe/cl_delivery_data_ut IMPLEMENTATION.

  METHOD class_setup.

  ENDMETHOD.

  METHOD class_teardown.

  ENDMETHOD.

  METHOD setup.

    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery ).

    f_cut = NEW /vpcoe/cl_rdp_delivery_data( iv_api_type     = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                         iv_package_size = lo_cust->get_package_size( )
                                         iv_source       = lo_cust->get_source_id( )
                                         iv_mode         = /vpcoe/cl_common_helper=>sc_mode-screen ).

  ENDMETHOD.

  METHOD teardown.

  ENDMETHOD.

  METHOD get_delivery.

    DATA: lt_delivery_act TYPE /vpcoe/cl_rdp_delivery_data=>gty_t_delivery_combined,
          lt_delivery_exp TYPE /vpcoe/cl_rdp_delivery_data=>gty_t_delivery_combined,
          ls_sel_opt      TYPE /vpcoe/s_selopt_delivery,
          lv_failed_exp   TYPE abap_bool,
          lv_no_data_exp  TYPE abap_bool.

    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-delivery ).

    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'CA' ) INTO TABLE ls_sel_opt-country.
    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'LR' ) INTO TABLE ls_sel_opt-document_type.

    lt_delivery_exp = VALUE #( ( id = '0084000001'
                                 type = 'LR'
                                 sales_organization = 'STBL'
                                 ship_to_party = 'TEST1_VS'
                                 ship_to_region = 'ON'
                                 ship_to_country = 'CA'
                                 actual_goods_movement_date = '00000000'
                                 incoterms = 'UN'
                                 overall_goods_movement_status = 'A'
                                 sold_to_party = 'TEST1_VS'
                                 items = VALUE #( ( id = '000010'
                                                    batch = ' '
                                                    category = 'REN'
                                                    base_quantity = '5.000'
                                                    base_unit = 'ST'
                                                    distribution_channel = '10'
                                                    division = '10'
                                                    plant = 'STBL'
                                                    product = 'TEST_MM_VS'
                                                    sales_quantity = '5.000'
                                                    sales_unit = 'ST'
                                                    ship_from_country = 'US'
                                                    reference_s_d_document = '0060000002'
                                                    reference_s_d_document_item = '10'
                                                    reference_s_d_documentcategory = 'H' ) ) )
                               ( id = '0084000002'
                                 type = 'LR'
                                 sales_organization = 'STBL'
                                 ship_to_party = 'TEST1_VS'
                                 ship_to_region = 'ON'
                                 ship_to_country = 'CA'
                                 actual_goods_movement_date = '20220619'
                                 incoterms = 'UN'
                                 overall_goods_movement_status = 'C'
                                 sold_to_party = 'TEST1_VS'
                                 items = VALUE #( ( id = '000010'
                                                    batch = ' '
                                                    category = 'REN'
                                                    base_quantity = '5.000'
                                                    base_unit = 'ST'
                                                    distribution_channel = '10'
                                                    division = '10'
                                                    plant = 'STBL'
                                                    product = 'TEST_MM_VS'
                                                    sales_quantity = '5.000'
                                                    sales_unit = 'ST'
                                                    ship_from_country = 'BR'
                                                    reference_s_d_document = '0060000003'
                                                    reference_s_d_document_item = '10'
                                                    reference_s_d_documentcategory = 'H' ) ) )
                               ( id = '0084000004'
                                 type = 'LR'
                                 sales_organization = 'STBL'
                                 ship_to_party = 'TEST1_VS'
                                 ship_to_region = 'ON'
                                 ship_to_country = 'CA'
                                 actual_goods_movement_date = '20220620'
                                 incoterms = 'UN'
                                 overall_goods_movement_status = 'C'
                                 sold_to_party = 'TEST1_VS'
                                 items = VALUE #( ( id = '000010'
                                                    batch = ' '
                                                    category = 'REN'
                                                    base_quantity = '2.000'
                                                    base_unit = 'ST'
                                                    distribution_channel = '10'
                                                    division = '10'
                                                    plant = 'STBL'
                                                    product = 'TEST_MM_VS'
                                                    sales_quantity = '2.000'
                                                    sales_unit = 'ST'
                                                    ship_from_country = 'US'
                                                    reference_s_d_document = '0060000006'
                                                    reference_s_d_document_item = '10'
                                                    reference_s_d_documentcategory = 'H' ) ) ) ).

    lv_failed_exp = abap_false.
    lv_no_data_exp = abap_false.

    f_cut->get_delivery(
      EXPORTING
        io_cust    = lo_cust
        is_sel_opt = ls_sel_opt
        iv_source  = /vpcoe/cl_common_helper=>get_source_id( )
        io_log     = lo_log
      IMPORTING
        et_delivery = lt_delivery_act
        ev_failed   = DATA(lv_failed_act)
        ev_no_data  = DATA(lv_no_data_act)
      CHANGING
        ct_total_count         = lo_log->mt_sum
        ct_total_count_billdoc = lo_log->mt_sum  ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_delivery_act
      exp   = lt_delivery_exp
      msg   = |Delivery assertion failed| ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_failed_act
      exp   = lv_failed_exp
      msg   = |'Failed' assertion failed| ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_no_data_act
      exp   = lv_no_data_exp
      msg   = |'No data' assertion failed| ).
  ENDMETHOD.

  METHOD build_json.

    DATA: lt_delivery     TYPE /vpcoe/cl_rdp_delivery_data=>gty_t_delivery_combined,
          lt_delivery_hdr TYPE /vpcoe/t_delivery_hdr,
          ls_json_exp     TYPE /vpcoe/cl_rdp_http=>gty_s_json,
          ls_sel_opt      TYPE /vpcoe/s_selopt_delivery.

    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-delivery ).

    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'CA' ) INTO TABLE ls_sel_opt-country.
    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'LR' ) INTO TABLE ls_sel_opt-document_type.

    f_cut->get_delivery(
      EXPORTING
        io_cust    = lo_cust
        is_sel_opt = ls_sel_opt
        iv_source  = /vpcoe/cl_common_helper=>get_source_id( )
        io_log     = lo_log
      IMPORTING
        et_delivery = lt_delivery
        ev_failed   = DATA(lv_failed)
        ev_no_data  = DATA(lv_no_data)
      CHANGING
        ct_total_count         = lo_log->mt_sum
        ct_total_count_billdoc = lo_log->mt_sum  ).

    lt_delivery_hdr = CORRESPONDING #( lt_delivery ).

    f_cut = NEW /vpcoe/cl_rdp_delivery_data( iv_api_type     = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                             iv_package_size = lo_cust->get_package_size( )
                                             iv_source       = lo_cust->get_source_id( )
                                             iv_mode         = /vpcoe/cl_common_helper=>sc_mode-send ).
    f_cut->get_items(
      EXPORTING
        it_delivery = lt_delivery_hdr
        is_sel_opt  = ls_sel_opt
      IMPORTING
        et_items = DATA(lt_items) ).

    f_cut->build_json(
    EXPORTING
      it_delivery_hdr = lt_delivery_hdr
      it_delivery_itm = lt_items
    IMPORTING
      es_json         = DATA(ls_json_act) ).

    ls_json_exp-count = 3.
    ls_json_exp-elements = `{"source":"ECC","elements":[{"id":"0084000001","type":"LR","salesOrganization":"STBL",` &&
                           `"shipToParty":"TEST1_VS","shipToRegion":"ON","shipToCountry":"CA","actualGoodsMovementDate": null,` &&
                           `"incoterms":"UN","overallGoodsMovementStatus":"A","soldToParty":"TEST1_VS","items":[{"id":10,"category":"REN",` &&
                           `"product":"TEST_MM_VS","batch": null,"baseQuantity":5.000,"baseUnit":"ST","salesQuantity":5.000,"salesUnit":"ST",` &&
                           `"plant":"STBL","distributionChannel":"10","division":"10","shipFromCountry":"US","referenceSDDocument":"0060000002",` &&
                           `"referenceSDDocumentItem":10,"referenceSDDocumentcategory":"H"}]},{"id":"0084000002","type":"LR","salesOrganization":"STBL",` &&
                           `"shipToParty":"TEST1_VS","shipToRegion":"ON","shipToCountry":"CA","actualGoodsMovementDate":"2022-06-19","incoterms":"UN",` &&
                           `"overallGoodsMovementStatus":"C","soldToParty":"TEST1_VS","items":[{"id":10,"category":"REN","product":"TEST_MM_VS","batch": null,` &&
                           `"baseQuantity":5.000,"baseUnit":"ST","salesQuantity":5.000,"salesUnit":"ST","plant":"STBL","distributionChannel":"10","division":"10",` &&
                           `"shipFromCountry":"BR","referenceSDDocument":"0060000003","referenceSDDocumentItem":10,"referenceSDDocumentcategory":"H"}]},` &&
                           `{"id":"0084000004","type":"LR","salesOrganization":"STBL","shipToParty":"TEST1_VS","shipToRegion":"ON","shipToCountry":"CA",` &&
                           `"actualGoodsMovementDate":"2022-06-20","incoterms":"UN","overallGoodsMovementStatus":"C","soldToParty":"TEST1_VS",` &&
                           `"items":[{"id":10,"category":"REN","product":"TEST_MM_VS","batch": null,"baseQuantity":2.000,"baseUnit":"ST","salesQuantity":2.000,` &&
                           `"salesUnit":"ST","plant":"STBL","distributionChannel":"10","division":"10","shipFromCountry":"US","referenceSDDocument":"0060000006",` &&
                           `"referenceSDDocumentItem":10,"referenceSDDocumentcategory":"H"}]}]}`.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_json_act
      exp   = ls_json_exp ).

  ENDMETHOD.

ENDCLASS.
