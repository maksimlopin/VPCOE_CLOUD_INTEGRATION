CLASS /vpcoe/cl_rdp_config_obj_ut DEFINITION DEFERRED.
CLASS /vpcoe/cl_rdp_config_obj DEFINITION LOCAL FRIENDS /vpcoe/cl_rdp_config_obj_ut.

CLASS /vpcoe/cl_rdp_config_obj_ut DEFINITION FOR TESTING FINAL
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      f_cut     TYPE REF TO /vpcoe/cl_rdp_config_obj,
      mv_source TYPE string.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: get_country FOR TESTING.
    METHODS: get_currency FOR TESTING.
    METHODS: get_dlvr_doc_item_cat FOR TESTING.
    METHODS: get_dlvr_doc_type FOR TESTING.
    METHODS: get_movement_type FOR TESTING.
    METHODS: get_product_group FOR TESTING.
    METHODS: get_product_hierarchy FOR TESTING.
    METHODS: get_product_type FOR TESTING.
    METHODS: get_region FOR TESTING.
    METHODS: get_uom FOR TESTING.
    METHODS: get_uom_dimension FOR TESTING.
    METHODS: get_uom_iso_code FOR TESTING.
    METHODS: send FOR TESTING.

ENDCLASS.       "/VPCOE/CL_RDP_CONFIG_OBJ_Ut


CLASS /vpcoe/cl_rdp_config_obj_ut IMPLEMENTATION.

  METHOD class_setup.

  ENDMETHOD.


  METHOD class_teardown.

  ENDMETHOD.


  METHOD setup.
    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                              iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                              iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-region ).

    f_cut = NEW /vpcoe/cl_rdp_config_obj( io_rdp_helper = lo_cust
                                          iv_mode       = /vpcoe/cl_rdp_helper=>sc_mode-send
                                          io_log        = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-configuration ) ).
    mv_source = /vpcoe/cl_rdp_helper=>get_source_id( ).
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.

  METHOD get_country.
    DATA lt_country_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_country.
    DATA lt_country TYPE /vpcoe/cl_rdp_config_obj=>gty_t_country.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text     TYPE gty_t_country_t.

    lt_text = VALUE #( ( name = 'Allemagne'
                         language = 'FR' )
                       ( name = 'Deutschland'
                         language = 'DE' )
                       ( name = 'Germany'
                         language = 'EN' )
                       ( name = 'Duitsland'
                         language = 'NL' )
                       ( name = 'Alemania'
                         language = 'ES' )
                       ( name = 'ドイツ'
                         language = 'JA' ) ).

    lt_country_exp = VALUE #( ( id = 'DE'
                                iso_code = 'DE'
                                three_digit_iso_code = '276'
                                three_letter_iso_code = 'DEU'
                                iseuropeanunionmember = 'X'
                                texts = lt_text ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                                && `"elements":[{"id":"DE","isoCode":"DE","threeDigitIsoCode":"276","threeLetterIsoCode":"DEU",`
                                && `"isEuropeanUnionMember":true,"texts":[{"name":"Allemagne","language":"FR"},{"name":"Deutschland","language":"DE"},`
                                && `{"name":"Germany","language":"EN"},{"name":"Duitsland","language":"NL"},{"name":"Alemania","language":"ES"},`
                                && `{"name":"ドイツ","language":"JA"}]}]}`
                      count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_country(
     IMPORTING
       et_country = lt_country
       et_json = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"DE","isoCode":"DE",`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_country WHERE id NE 'DE'.                 "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_country
      exp   = lt_country_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_currency.
    DATA lt_currency_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_currency.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text     TYPE gty_t_currency_t.

    lt_text = VALUE #( ( name = 'Andorianische Peseta (alt--> EUR)'
                         language = 'DE' )
                       ( name = 'Andorran Peseta --> (Old --> EUR)'
                         language = 'EN' )
                       ( name = 'アンドラペセタ  --> (旧 ---> EUR)'
                         language = 'JA' )
                       ( name = 'Andorrese peseta (oud--> EUR)'
                         language = 'NL' )
                       ( name = 'Peseta andorrana (obsoleta--> EUR)'
                         language = 'ES' ) ).

    lt_currency_exp = VALUE #( ( id = 'ADP'
                                 iso_code = 'ADP'
                                 is_primary_i_s_o_currency = 'false'
                                 texts = lt_text ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                            && `"elements":[{"id":"ADP","isoCode":"ADP","isPrimaryISOCurrency":"false",`
                            && `"texts":[{"name":"Andorianische Peseta (alt--> EUR)","language":"DE"},`
                            && `{"name":"Andorran Peseta --> (Old --> EUR)","language":"EN"},{"name":"アンドラペセタ  --> (旧 ---> EUR)","language":"JA"},`
                            && `{"name":"Andorrese peseta (oud--> EUR)","language":"NL"},{"name":"Peseta andorrana (obsoleta--> EUR)","language":"ES"}]}]}`
                  count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_currency(
     IMPORTING
       et_currency = DATA(lt_currency)
       et_json = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"ADP","isoCode":"ADP",`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_currency WHERE id NE 'ADP'.               "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_currency
      exp   = lt_currency_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_dlvr_doc_item_cat.
    DATA lt_dlvr_doc_item_cat_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_dd_item_category.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text     TYPE gty_t_dd_item_category_t.

    lt_text = VALUE #( ( name = 'eBay Payment Process'
                         language = 'EN' )
                       ( name = 'eBay-Kaufabwicklung'
                         language = 'DE' )
                       ( name = 'eBay 支払処理'
                         language = 'JA' )
                       ( name = 'eBay-koopafhandeling'
                         language = 'NL' )
                       ( name = 'Gestión compras eBay'
                         language = 'ES' ) ).

    lt_dlvr_doc_item_cat_exp = VALUE #( ( id = 'AEBA'
                                          texts = lt_text ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                            && `"elements":[{"id":"AEBA",`
                            && `"texts":[{"name":"eBay Payment Process","language":"EN"},`
                            && `{"name":"eBay-Kaufabwicklung","language":"DE"},{"name":"eBay 支払処理","language":"JA"},`
                            && `{"name":"eBay-koopafhandeling","language":"NL"},{"name":"Gestión compras eBay","language":"ES"}]}]}`
                    count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_dlvr_doc_item_cat(
      IMPORTING
       et_dlvr_doc_item_cat = DATA(lt_dlvr_doc_item_cat)
       et_json = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"AEBA",`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_dlvr_doc_item_cat WHERE id NE 'AEBA'.     "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).

    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act   = lt_dlvr_doc_item_cat
      exp   = lt_dlvr_doc_item_cat_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_dlvr_doc_type.
    DATA lt_dlvr_doc_type_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_dd_type.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text     TYPE gty_t_dd_type.

    lt_text = VALUE #( ( name = 'Barverkauf'
                         language = 'DE' )
                       ( name = 'Cash Sales'
                         language = 'EN' )
                       ( name = '現金販売'
                         language = 'JA' )
                       ( name = 'Contante verkoop'
                         language = 'NL' )
                       ( name = 'Venta al contado'
                         language = 'ES' ) ).

    lt_dlvr_doc_type_exp = VALUE #( ( id = 'BV'
                                      texts = lt_text ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                            && `"elements":[{"id":"BV",`
                            && `"texts":[{"name":"Barverkauf","language":"DE"},`
                            && `{"name":"Cash Sales","language":"EN"},{"name":"現金販売","language":"JA"},`
                            && `{"name":"Contante verkoop","language":"NL"},{"name":"Venta al contado","language":"ES"}]}]}`
                    count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_dlvr_doc_type(
      IMPORTING
       et_dlvr_doc_type = DATA(lt_dlvr_doc_type)
       et_json          = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `"source":"ECC","elements":[{"id":"BV",`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_dlvr_doc_type WHERE id NE 'BV'.           "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_dlvr_doc_type
      exp   = lt_dlvr_doc_type_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_movement_type.
    DATA lt_movement_type_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_movement_type.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text     TYPE gty_t_movement_type.
    lt_text = VALUE #( ( name = 'WE Wareneingang'
                         language = 'DE' )
                       ( name = 'GR goods receipt'
                         language = 'EN' )
                       ( name = '購買発注入庫'
                         language = 'JA' )
                       ( name = 'GO goederenontvangst'
                         language = 'NL' )
                       ( name = 'EM Entr.mercancías'
                         language = 'ES' ) ).

    lt_movement_type_exp = VALUE #( ( id = '101'
                                      texts = lt_text ) ).
    INSERT VALUE #( elements = `{"source":"ECC",`
                            && `"elements":[{"id":"101",`
                            && `"texts":[{"name":"WE Wareneingang","language":"DE"},`
                            && `{"name":"GR goods receipt","language":"EN"},{"name":"購買発注入庫","language":"JA"},`
                            && `{"name":"GO goederenontvangst","language":"NL"},{"name":"EM Entr.mercancías","language":"ES"}]}]}`
                    count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_movement_type(
     IMPORTING
       et_movement_type = DATA(lt_movement_type)
       et_json          = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"101",`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_movement_type WHERE id NE '101'.          "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_movement_type
      exp   = lt_movement_type_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_product_group.
    DATA lt_product_group_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_product_group.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text     TYPE gty_t_product_group.

    lt_text = VALUE #( ( name = 'Warengruppe 1'
                         description = ' '
                         language = 'DE' )
                       ( name = 'Material group 1'
                         description = ' '
                         language = 'EN' )
                       ( name = '品目グループ 1'
                         description = ' '
                         language = 'JA' )
                       ( name = 'Goederengroep 1'
                         description = ' '
                         language = 'NL' )
                       ( name = 'Grupo de artículos 1'
                         description = ' '
                         language = 'ES' ) ).

    lt_product_group_exp = VALUE #( ( id = '01'
                                      texts = lt_text ) ).
    INSERT VALUE #( elements = `{"source":"ECC",`
                            && `"elements":[{"id":"01",`
                            && `"texts":[{"name":"Warengruppe 1","description": null,"language":"DE"},`
                            && `{"name":"Material group 1","description": null,"language":"EN"},{"name":"品目グループ 1","description": null,"language":"JA"},`
                            && `{"name":"Goederengroep 1","description": null,"language":"NL"},{"name":"Grupo de artículos 1","description": null,"language":"ES"}]}]}`
                    count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_product_group(
     IMPORTING
       et_product_group = DATA(lt_product_group)
       et_json          = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"01"`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_product_group WHERE id NE '01'.           "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_product_group
      exp   = lt_product_group_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_product_hierarchy.
    DATA lt_product_hierarchy_exp TYPE /vpcoe/tt_product_hierarchy.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text     TYPE /vpcoe/tt_product_hierarchy_t.

    lt_text = VALUE #( ( name = 'Werkzeuge'
                         language = 'DE' )
                       ( name = 'Best Practices Prod. Hier. Top Level'
                         language = 'EN' )
                       ( name = 'ツール'
                         language = 'JA' )
                       ( name = 'Gereedschap'
                         language = 'NL' )
                       ( name = 'Herramientas'
                         language = 'ES' ) ).

    lt_product_hierarchy_exp = VALUE #( ( id = '00001'
                                          level = '1'
                                          texts = lt_text ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                            && `"elements":[{"id":"00001","level":1,`
                            && `"texts":[{"name":"Werkzeuge","language":"DE"},`
                            && `{"name":"Best Practices Prod. Hier. Top Level","language":"EN"},{"name":"ツール","language":"JA"},`
                            && `{"name":"Gereedschap","language":"NL"},{"name":"Herramientas","language":"ES"}]}]}`
                    count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_product_hierarchy(
      IMPORTING
       et_product_hierarchy = DATA(lt_product_hierarchy)
       et_json              = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"00001","level":1,`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_product_hierarchy WHERE id NE '00001'.

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_product_hierarchy
      exp   = lt_product_hierarchy_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_product_type.
    DATA lt_product_type_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_product_type.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text TYPE gty_t_product_type.

    lt_text = VALUE #( ( name = 'Abfall'
                         language = 'DE' )
                       ( name = 'Waste'
                         language = 'EN' )
                       ( name = '廃棄物'
                         language = 'JA' )
                       ( name = 'Afval'
                         language = 'NL' )
                       ( name = 'Residuos'
                         language = 'ES' ) ).

    lt_product_type_exp = VALUE #( ( id = 'ABF'
                                     texts = lt_text ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                            && `"elements":[{"id":"ABF",`
                            && `"texts":[{"name":"Abfall","language":"DE"},`
                            && `{"name":"Waste","language":"EN"},{"name":"廃棄物","language":"JA"},`
                            && `{"name":"Afval","language":"NL"},{"name":"Residuos","language":"ES"}]}]}`
                    count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_product_type(
     IMPORTING
       et_product_type = DATA(lt_product_type)
       et_json         = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"ABF",`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_product_type WHERE id NE 'ABF'.           "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_product_type
      exp   = lt_product_type_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_region.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_region_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_region.
    DATA lt_region_act TYPE /vpcoe/cl_rdp_config_obj=>gty_t_region.
    DATA: lt_text TYPE gty_t_region_t.

    lt_text = VALUE #( ( name = 'Aberdeenshire'
                         language = 'EN' )
                       ( name = 'Aberdeenshire'
                         language = 'DE' )
                       ( name = 'Aberdeenshire'
                         language = 'JA' )
                       ( name = 'Aberdeenshire'
                         language = 'NL' )
                       ( name = 'Aberdeenshire'
                         language = 'ES' ) ).
    lt_region_exp = VALUE #( ( id = 'AB'
                               country = 'GB'
                               texts = lt_text ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                            && `"elements":[{"id":"AB","country":"GB",`
                            && `"texts":[{"name":"Aberdeenshire","language":"EN"},`
                            && `{"name":"Aberdeenshire","language":"DE"},{"name":"Aberdeenshire","language":"JA"},`
                            && `{"name":"Aberdeenshire","language":"NL"},{"name":"Aberdeenshire","language":"ES"}]}]}`
                    count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_region(
     IMPORTING
       et_json          = DATA(lt_json)
       et_region        = DATA(lt_region) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"AB","country":"GB",`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    READ TABLE lt_region INTO DATA(ls_region) WITH KEY id      = 'AB'
                                                       country = 'GB'.
    APPEND ls_region TO lt_region_act.

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_region_act
      exp   = lt_region_exp ).

  ENDMETHOD.
*
  METHOD get_uom.
    DATA lt_uom_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_uom.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text TYPE gty_t_uom_t.

    lt_text = VALUE #( ( name = '%'
                         technical_name = '%'
                         commercial_name = '%'
                                             language = 'EN'
                                             long_name = 'Percentage' )
                                             ( name = '%'
                         technical_name = '%'
                         commercial_name = '%'
                                               language = 'DE'
                                               long_name = 'Prozent' )
                                               ( name = '%'
                         technical_name = '%'
                         commercial_name = '%'
                                               language = 'JA'
                                               long_name = '百分率' )
                                               ( name = '%'
                         technical_name = '%'
                         commercial_name = '%'
                                               language = 'NL'
                                               long_name = 'Procent' )
                                               ( name = '%'
                         technical_name = '%'
                         commercial_name = '%'
                                                language = 'ES'
                                                long_name = 'Porcentaje' ) ).
    lt_uom_exp = VALUE #( ( id = '%'
                            iso_code = 'P1'
                            dimension = 'PROPOR'
                            si_unit_cnvrsn_rate_numr = '1'
                            si_unit_cnvrsn_rate_denomr = '100'
                            si_unit_cnvrsn_rate_exponent = '0'
                            si_unit_cnvrsn_additive_value = '0.000000'
                            texts = lt_text ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                               && `"elements":[{"id":"%","isoCode":"P1","dimension":"PROPOR","siUnitCnvrsnRateNumerator":1,`
                               && `"siUnitCnvrsnRateDenominator":100,"siUnitCnvrsnRateExponent":0,"siUnitCnvrsnAdditiveValue":0,`
                               && `"texts":[{"name":"%","technicalName":"%","commercialName":"%","language":"EN","longName":"Percentage"},`
                               && `{"name":"%","technicalName":"%","commercialName":"%","language":"DE","longName":"Prozent"},`
                               && `{"name":"%","technicalName":"%","commercialName":"%","language":"JA","longName":"百分率"},`
                               && `{"name":"%","technicalName":"%","commercialName":"%","language":"NL","longName":"Procent"},`
                               && `{"name":"%","technicalName":"%","commercialName":"%","language":"ES","longName":"Porcentaje"}]}]}`
                     count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_uom(
     IMPORTING
       et_uom           = DATA(lt_uom)
       et_json          = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"%","isoCode":"P1"`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_uom WHERE id NE '%'.                      "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_uom
      exp   = lt_uom_exp
       ).

  ENDMETHOD.

  METHOD get_uom_dimension.
    DATA lt_uom_d_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_uom_dimension.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text TYPE /vpcoe/tt_uom_dimension_t.

    lt_text = VALUE #( ( name = '(dimensionslos)'
                         language = 'DE' )
                       ( name = '(no dimensions)'
                         language = 'EN' )
                       ( name = '無次元'
                         language = 'JA' )
                       ( name = '(dimensieloos)'
                         language = 'NL' )
                       ( name = '(sin dimensiones)'
                         language = 'ES' ) ).

    lt_uom_d_exp = VALUE #( ( id = 'AAAADL'
                            si_unit = ' '
                            texts = lt_text ) ).

    INSERT VALUE #( elements = `{`
                            && `"source":"ECC",`
                            && `"elements":[`
                            && `{`
                            && `"id":"AAAADL",`
                            && `"siUnit": null,`
                            && `"texts":[`
                            && `{`
                            && `"name":"(dimensionslos)",`
                            && `"language":"DE"`
                            && `},`
                            && `{`
                            && `"name":"(no dimensions)",`
                            && `"language":"EN"`
                            && `},`
                            && `{`
                            && `"name":"無次元",`
                            && `"language":"JA"`
                            && `},`
                            && `{`
                            && `"name":"(dimensieloos)",`
                            && `"language":"NL"`
                            && `},`
                            && `{`
                            && `"name":"(sin dimensiones)",`
                            && `"language":"ES"`
                            && `}`
                            && `]`
                            && `}`
                            && `]`
                            && `}`
                          count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_uom_dimension(
     IMPORTING
       et_uom_d         = DATA(lt_uom_d)
       et_json          = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"AAAADL",`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_uom_d WHERE id NE 'AAAADL'.               "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_uom_d
      exp   = lt_uom_d_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.
*
  METHOD get_uom_iso_code.
    DATA lt_iso_uom_exp TYPE /vpcoe/cl_rdp_config_obj=>gty_t_uom_iso_code.
    DATA lt_json_exp TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA lt_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA: lt_text TYPE gty_t_uom_iso_code_t.

    lt_text = VALUE #( ( name = 'Gramm/Kubikzentimeter'
                         language = 'DE' )
                       ( name = 'グラム/立方センチメートル'
                         language = 'JA' )
                       ( name = 'Gram/Cubic centimeter'
                         language = 'EN' )
                       ( name = 'Gram/kubieke centimeter'
                         language = 'NL' )
                       ( name = 'Gramos/Centímetro cúbico'
                         language = 'ES' ) ).

    lt_iso_uom_exp = VALUE #( ( id = '23'
                                 texts = lt_text ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                            && `"elements":[{"id":"23",`
                            && `"texts":[{"name":"Gramm/Kubikzentimeter","language":"DE"},`
                            && `{"name":"グラム/立方センチメートル","language":"JA"},{"name":"Gram/Cubic centimeter","language":"EN"},`
                            && `{"name":"Gram/kubieke centimeter","language":"NL"},{"name":"Gramos/Centímetro cúbico","language":"ES"}]}]}`
                    count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_uom_iso_code(
     IMPORTING
       et_iso_uom       = DATA(lt_iso_uom)
       et_json          = DATA(lt_json) ).

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"23"`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    DELETE lt_iso_uom WHERE id NE '23'.                 "#EC CI_SORTSEQ

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_iso_uom
      exp   = lt_iso_uom_exp ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD send.
    DATA lt_json TYPE /vpcoe/cl_rdp_http=>gty_t_json.
    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                      iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
                                                      iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-uom_dimension  ).
    DATA lv_status_exp TYPE i.
    DATA lv_reason_exp TYPE string.

    INSERT VALUE #( elements = `{ "source":"ECC",`
                               && `"elements": [`
                               &&    `{`
                               &&        `"id": "AAAADL",`
                               &&        `"siUnit": null,`
                               &&        `"lengthExponent": 0,`
                               &&        `"massExponent": 0,`
                               &&        `"timeExponent": 0,`
                               &&        `"electricCurrentExponent": 0,`
                               &&        `"temperaturExponent": 0,`
                               &&        `"molquantityExponent": 0,`
                               &&        `"luminosityExponent": 0,`
                               &&        `"hasUnitswithTemperatureSpec": false,`
                               &&         `"hasUnitswithPressureSpec": false,`
                               &&         `"texts": [`
                               &&         `    {`
                               &&         `        "name": "(dimensionslos)",`
                               &&         `        "language": "DE"`
                               &&         `    },`
                               &&         `    {`
                               &&         `        "name": "(no dimensions)",`
                               &&         `        "language": "EN"`
                               &&         `    },`
                               &&         `    {`
                               &&       `          "name": "無次元",`
                               &&       `          "language": "JA"`
                               &&       `      },`
                               &&       `      {`
                               &&       `          "name": "(dimensieloos)",`
                               &&       `          "language": "NL"`
                               &&       `      },`
                               &&       `      {`
                               &&       `          "name": "(sin dimensiones)",`
                               &&        `         "language": "ES"`
                               &&        `     }`
                               &&        ` ]`
                               &&    ` }`
                               &&  `]}`
                        count = 1 ) INTO TABLE lt_json.

    lv_status_exp = '204'.
    lv_reason_exp = 'No Content'.

    TRY.
        sy-batch = abap_true. "To avoid Message Displaying for test purpose
        f_cut->send(
          EXPORTING
            iv_srv_id = lo_cust->get_srv_id( )
            it_json = lt_json
            io_cust = lo_cust
          IMPORTING
            ev_status = DATA(lv_status)
            ev_reason = DATA(lv_reason) ).

      CATCH cx_uuid_error cx_oa2c.
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = 'Exception in Send()' ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act   = lv_status
      exp   = lv_status_exp
      msg   = 'Status assertion failed' ).
    cl_abap_unit_assert=>assert_equals(
      act   = lv_reason
      exp   = lv_reason_exp
      msg   = 'Reason assertion failed' ).
  ENDMETHOD.

ENDCLASS.
