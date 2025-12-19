*"* use this source file for your ABAP unit test classes
CLASS /vpcoe/cl_customer_ut DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      f_cut   TYPE REF TO /vpcoe/cl_rdp_customer_data,
      lo_cust TYPE REF TO /vpcoe/cl_rdp_helper.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: get_customer_all FOR TESTING.
    METHODS: get_customer_delta FOR TESTING.
    METHODS: get_address FOR TESTING.

ENDCLASS.

CLASS /vpcoe/cl_customer_ut IMPLEMENTATION.

  METHOD class_setup.

  ENDMETHOD.

  METHOD class_teardown.

  ENDMETHOD.

  METHOD setup.

    DATA iv_mode TYPE /vpcoe/de_mode.

    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                      iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
                                                      iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer ).

    f_cut = NEW /vpcoe/cl_rdp_customer_data( iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                         iv_package_size = lo_cust->get_package_size( )
                                         iv_source       = lo_cust->get_source_id( )
                                         iv_mode         = iv_mode ).

  ENDMETHOD.

  METHOD teardown.

  ENDMETHOD.

  METHOD get_customer_all.

    DATA: lt_customer_act     TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
          lt_json_act         TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_customer_del_act TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
          lt_json_del_act     TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_customer_exp     TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
          lt_json_exp         TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_customer_del_exp TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
          lt_json_del_exp     TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          ls_sel_opt          TYPE /vpcoe/s_selopt_customer.

    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'US' ) INTO TABLE ls_sel_opt-country.
    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'AR' ) INTO TABLE ls_sel_opt-region.

    lt_customer_exp = VALUE #( ( id                            = '0000400021'
                                 name                          = 'test create cust new'
                                 region                        = 'AR'
                                 country                       = 'US' )
                               ( id                            = '0000400024'
                                 name                          = 'test create cust new #4'
                                 region                        = 'AR'
                                 country                       = 'US' )
                               ( id                            = '2002213607'
                                 name                          = 'WALMART INC US'
                                 region                        = 'AR'
                                 country                       = 'US' )
                                 ( id                          = '2002252237'
                                 name                          = 'DATEK INC.'
                                 region                        = 'AR'
                                 country                       = 'US' ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                                && `"elements":[{"id":"0000400021","name":"test create cust new","region":"AR","country":"US",`
                                && `"isMarkedForDeletion":null,"taxNumbers":[],"category": null,"postalCode": null,"phone": null,`
                                && `"fax": null,"email": null,"street": null,"houseNumber": null,"city": null,`
                                && `"contactPersonFirstName": null,"contactPersonLastName": null},`
                                && `{"id":"0000400024","name":"test create cust new #4","region":"AR","country":"US",`
                                && `"isMarkedForDeletion":null,"taxNumbers":[],"category": null,"postalCode": null,`
                                && `"phone": null,"fax": null,"email": null,"street": null,"houseNumber": null,"city": null,`
                                && `"contactPersonFirstName": null,"contactPersonLastName": null},{"id":"2002213607",`
                                && `"name":"WALMART INC US","region":"AR","country":"US","isMarkedForDeletion":null,"taxNumbers":[],`
                                && `"category": null,"postalCode": null,"phone": null,"fax": null,"email": null,"street": null,`
                                && `"houseNumber": null,"city": null,"contactPersonFirstName": null,"contactPersonLastName": null},`
                                && `{"id":"2002252237","name":"DATEK INC.","region":"AR","country":"US","isMarkedForDeletion":null,`
                                && `"taxNumbers":[],"category": null,"postalCode": null,"phone": null,"fax": null,"email": null,`
                                && `"street": null,"houseNumber": null,"city": null,"contactPersonFirstName": null,"contactPersonLastName": null}]}`
              count = 4 ) INTO TABLE lt_json_exp.

    f_cut->get_customer_all(
      EXPORTING
        is_sel_opt = ls_sel_opt
        io_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-customer )
    IMPORTING
      et_customer     = lt_customer_act
      et_json         = lt_json_act
      et_customer_del = lt_customer_del_act
      et_json_del     = lt_json_del_act ).


    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.
    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg_del) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_del_act
        it_json_exp = lt_json_del_exp
        ).
    ENDIF.


    cl_abap_unit_assert=>assert_equals(
      act   = lt_customer_del_act
      exp   = lt_customer_del_exp
      msg   = 'Customer delta assertion error' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_del_act
      exp   = lt_json_del_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg_del )
       ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_customer_act
      exp   = lt_customer_exp
      msg   = 'Customer assertion error' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_customer_delta.

    DATA: lt_customer_del_act TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
          lt_json_del_act     TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_customer_exp     TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
          lt_json_exp         TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_customer_act     TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
          lt_json_act         TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_customer_del_exp TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
          lt_json_del_exp     TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lv_chng_pointer_id  TYPE edi_mestyp VALUE '/VPCOE/CUSTOMER'.

    INSERT VALUE #( elements = `{"source":"ECC",`
                                 && `"elements":[{"id":"TEST1_VS","name":"Test Customer VS XD3 (KNA1)","region":"ON",`
                                 && `"country":"CA","isMarkedForDeletion":false,"taxNumbers":[],"category":"2",`
                                 && `"postalCode":"A1B 1C1","phone": null,"fax": null,"email": null,"street": null,`
                                 && `"houseNumber": null,"city":"Washington DC","contactPersonFirstName": null,"contactPersonLastName": null}]}`
                                 count = 1 ) INTO TABLE lt_json_del_exp.

    lt_customer_del_exp = VALUE #( ( id                    = 'TEST1_VS'
                                    name                   = 'Test Customer VS XD3 (KNA1)'
                                    region                 = 'ON'
                                    country                = 'CA'
                                    is_marked_for_deletion = '-'
                                    category               = '2'
                                    postal_code            = 'A1B 1C1'
                                    city                   = 'Washington DC' ) ).

    f_cut->get_customer_delta( EXPORTING
      iv_chng_pointer_id = lv_chng_pointer_id
      io_log             = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-customer )
      IMPORTING
        et_customer_del = lt_customer_del_act
        et_json_del     = lt_json_del_act
        et_customer     = lt_customer_act
        et_json         = lt_json_act ).

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.
    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg_del) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_del_act
        it_json_exp = lt_json_del_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_customer_del_act
      exp   = lt_customer_del_exp
      msg   = 'Customer Delta assertion error' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_del_act
      exp   = lt_json_del_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_customer_act
      exp   = lt_customer_exp
      msg   = 'Customer assertion error' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.

  METHOD get_address.
        DATA: lt_customer_act TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
              lt_customer_exp TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer.

    lt_customer_exp = VALUE #( ( id           = '0000123456'
                                 name         = 'Subcontractor Test 777'
                                 region       = '05'
                                 country      = 'DE'
                                 category     = '2'
                                 postal_code  = '11111'
                                 phone        = '999 326 5303'
                                 street       = 'GILES RD'
                                 house_number = '200'
                                 city         = 'Blacksburg' )
                               ( id           = '0000100099'
                                 name         = 'Domestic Customer'
                                 region       = 'MI'
                                 country      = 'US'
                                 category     = '2'
                                 postal_code  = '12345'
                                 street       = 'Main Avenue'
                                 house_number = '100'
                                 city         = 'Michigan' ) ).

    lt_customer_act = VALUE #( ( id           = '0000123456'
                                 name         = 'Subcontractor Test 777'
                                 region       = '05'
                                 country      = 'DE' )
                               ( id           = '0000100099'
                                 name         = 'Domestic Customer'
                                 region       = 'MI'
                                 country      = 'US' ) ).

    f_cut->get_address( CHANGING ct_customer = lt_customer_act ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_customer_act
      exp   = lt_customer_exp
      msg   = 'Customer assertion error' ).

  ENDMETHOD.
ENDCLASS.
