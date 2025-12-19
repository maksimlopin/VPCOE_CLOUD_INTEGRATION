CLASS /vpcoe/cl_rdp_org_data_obj_ut DEFINITION DEFERRED.
CLASS /vpcoe/cl_rdp_org_data_obj DEFINITION LOCAL FRIENDS /vpcoe/cl_rdp_org_data_obj_ut.
CLASS /vpcoe/cl_rdp_org_data_obj_ut DEFINITION FOR TESTING FINAL
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_rdp_org_data_obj.  "class under test
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS setup.
    METHODS teardown.
    METHODS: get_company_code FOR TESTING.
    METHODS: get_distr_channel FOR TESTING.
    METHODS: get_division FOR TESTING.
    METHODS: get_plant FOR TESTING.
    METHODS: get_sales_area FOR TESTING.
    METHODS: get_sales_org FOR TESTING.
*    METHODS: get_val_area_company_code FOR TESTING.
    METHODS: check_send IMPORTING it_json_act TYPE /vpcoe/cl_rdp_http=>gty_t_json
                                  io_cust     TYPE REF TO /vpcoe/cl_rdp_helper.
ENDCLASS.       "/VPCOE/CL_rdp_ORG_DATA_OBJ_Ut


CLASS /vpcoe/cl_rdp_org_data_obj_ut IMPLEMENTATION.

  METHOD check_send.
    sy-batch = abap_true.
    DATA(lv_reason_exp) = 'No Content'.
    DATA(lv_status_exp) = '204'.

    TRY.
        f_cut->send(
          EXPORTING
            io_cust       = io_cust
            it_json       = it_json_act
          IMPORTING
            ev_status     = DATA(lv_status_act)
            ev_reason     = DATA(lv_reason_act) ).

      CATCH cx_uuid_error cx_oa2c.
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = 'Exception in Send()' ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
       act = lv_status_act
       exp = lv_status_exp
       msg = 'Check Status error').

    cl_abap_unit_assert=>assert_equals(
      act = lv_reason_act
      exp = lv_reason_exp
      msg = 'Check reason error' ).

  ENDMETHOD.
  METHOD class_setup.

  ENDMETHOD.
  METHOD setup.

    f_cut = NEW /vpcoe/cl_rdp_org_data_obj(  iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                             io_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-organization )  ).

  ENDMETHOD.
  METHOD class_teardown.
  ENDMETHOD.
  METHOD teardown.
  ENDMETHOD.


  METHOD get_company_code.
    DATA: lt_company_code_act TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_company_code,
          lt_company_code_exp TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_company_code,
          lt_json_act         TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_json_exp         TYPE /vpcoe/cl_rdp_http=>gty_t_json.

    lt_company_code_exp = VALUE #( ( id  = '0001'
                                  country = 'DE'
                                  name = 'SAP A.G.'
                                  currency = 'EUR'
                                  language = 'DE')
                                  ( id  = '0MB1'
                                  country = 'DE'
                                  name = 'IS-B Musterbank Deutschl.'
                                  currency = 'EUR'
                                  language = 'DE')  ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                               && `"elements":[{"id":"0001","name":"SAP A.G.","country":"DE","currency":"EUR","language":"DE"},`
                               && `{"id":"0MB1","name":"IS-B Musterbank Deutschl.","country":"DE","currency":"EUR","language":"DE"}]}`
                              count = 2 ) INTO TABLE lt_json_exp.

    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                      iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                                      iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-company_code ).



    f_cut->get_company_code(
      EXPORTING
        io_cust         = lo_cust
      IMPORTING
        et_company_code = DATA(lt_company_code)
        et_json         = DATA(lt_json) ).

    DATA(ls_json) = VALUE #( lt_json[ 1 ] OPTIONAL ).
    DATA(ls_json_exp) = VALUE #( lt_json_exp[ 1 ] OPTIONAL ).

    IF ( ls_json-elements CS `{"source":"ECC",`
                               && `"elements":[{"id":"0001","name":"SAP A.G.","country":"DE","currency":"EUR","language":"DE"},`
                               && `{"id":"0MB1","name":"IS-B Musterbank Deutschl.","country":"DE","currency":"EUR","language":"DE"},` ).
      INSERT ls_json_exp INTO TABLE lt_json_act.
    ENDIF.


    INSERT VALUE #( lt_company_code[ id = '0001' ] OPTIONAL ) INTO TABLE lt_company_code_act.
    APPEND VALUE #( lt_company_code[ id = '0MB1' ] OPTIONAL ) TO lt_company_code_act.

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act = lt_company_code_act
      exp = lt_company_code_exp
      msg = 'Company Code assertion failed' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

    me->check_send(
      EXPORTING
        it_json_act = lt_json_act
        io_cust     = lo_cust ).

  ENDMETHOD.

  METHOD get_distr_channel.
    DATA: lt_distr_channel_exp TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_distr_channel,
          lt_distr_channel_act TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_distr_channel,
          lt_json_act          TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_json_exp          TYPE /vpcoe/cl_rdp_http=>gty_t_json.

    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                          iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                                          iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-distribution_channel ).

    lt_distr_channel_exp = VALUE #( (  id = '01'
                                       texts =  VALUE #( ( name = 'Vertriebsweg 01' language = 'DE') ( name = 'Distribtn Channel 01' language = 'EN')
                                                         ( name = '流通チャネル 01' language = 'JA' ) ( name = 'Distributiekanaal 01' language = 'NL' )
                                                         ( name = 'Canal distribución 1' language = 'ES' ) ) )
                                      ( id = 10  texts =  VALUE #( ( name = 'Direct Sales' language = 'EN') ) )
                                      ( id = 20  texts =  VALUE #( ( name = 'Wholesale' language = 'EN') ) )
                                      ( id = 30  texts =  VALUE #( ( name = 'Export sales 1' language = 'EN') ) )
                                      ( id = 40  texts =  VALUE #( ( name = 'Export sales' language = 'EN') ) ) ).
    INSERT VALUE #( elements = `{"source":"ECC",`
                              && `"elements":[{"id":"01","texts":[{"name":"Vertriebsweg 01","language":"DE"},`
                              && `{"name":"Distribtn Channel 01","language":"EN"},{"name":"流通チャネル 01","language":"JA"},{"name":"Distributiekanaal 01","language":"NL"},{"name":"Canal distribución 1","language":"ES"}]},`
                              && `{"id":"10","texts":[{"name":"Direct Sales","language":"EN"}]},`
                              && `{"id":"20","texts":[{"name":"Wholesale","language":"EN"}]},`
                              && `{"id":"30","texts":[{"name":"Export sales 1","language":"EN"}]},`
                              && `{"id":"40","texts":[{"name":"Export sales","language":"EN"}]}]}`
                             count = 5 ) INTO TABLE lt_json_exp.

    f_cut->get_distr_channel(
     EXPORTING
       io_cust          = lo_cust
     IMPORTING
       et_distr_channel = lt_distr_channel_act
       et_json          = lt_json_act ).

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_distr_channel_act
      exp   = lt_distr_channel_exp
      msg = 'Distribution Channel assertion failed' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

    me->check_send(
      EXPORTING
        it_json_act = lt_json_act
        io_cust     = lo_cust ).

  ENDMETHOD.


  METHOD get_division.

    DATA:
      lt_division_act TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_division,
      lt_division_exp TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_division,
      lt_json_act     TYPE /vpcoe/cl_rdp_http=>gty_t_json,
      lt_json_exp     TYPE /vpcoe/cl_rdp_http=>gty_t_json.

    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                             iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                                             iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-division ).
    lt_division_exp =  VALUE #( (  id = '01'
                                          texts =  VALUE #( ( name = 'Produktsparte 01' language = 'DE') ( name = 'Product Division 01' language = 'EN')
                                                            ( name = '商品製品部門 01' language = 'JA' ) ( name = 'Productgroep 01' language = 'NL' )
                                                            ( name = 'Sector producto 01' language = 'ES' ) ) )
                                         ( id = 10  texts =  VALUE #( ( name = 'Product Division 10' language = 'EN') ) )
                                         ( id = 20  texts =  VALUE #( ( name = 'Product Division 20' language = 'EN') ) )
                                         ( id = 30  texts =  VALUE #( ( name = 'Product Division 03' language = 'EN') ) )
                                          ).
    INSERT VALUE #( elements = `{"source":"ECC",`
                              && `"elements":[{"id":"01","texts":[{"name":"Produktsparte 01","language":"DE"},{"name":"Product Division 01","language":"EN"},`
                              && `{"name":"商品製品部門 01","language":"JA"},{"name":"Productgroep 01","language":"NL"},{"name":"Sector producto 01","language":"ES"}]},`
                              && `{"id":"10","texts":[{"name":"Product Division 10","language":"EN"}]},`
                              && `{"id":"20","texts":[{"name":"Product Division 20","language":"EN"}]},`
                              && `{"id":"30","texts":[{"name":"Product Division 03","language":"EN"}]}]}`
                             count = 4 ) INTO TABLE lt_json_exp.

    f_cut->get_division(
      EXPORTING
        io_cust     =   lo_cust
      IMPORTING
        et_json     = lt_json_act
        et_division = lt_division_act ).

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
      act   = lt_division_act
      exp   = lt_division_exp
      msg = 'Division assertion failed' ).

    me->check_send(
      EXPORTING
        it_json_act = lt_json_act
        io_cust     = lo_cust ).
  ENDMETHOD.


  METHOD get_plant.
    DATA: lt_json_act     TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_json_exp     TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_plant_act    TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_plant,
          lt_plant_exp    TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_plant,
          lt_bapiret2_act TYPE bapiret2_t,
          lt_bapiret2_exp TYPE bapiret2_t.


    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                      iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                                      iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-plant ).

    lt_plant_exp = VALUE #( ( id = '1010' language = 'EN' name = 'ASER - ALICANTE SPAIN' company_code = '1010' plant_address = VALUE #( id = '570009927' country = 'ES' region = '' ) )
                           ).
    INSERT VALUE #( elements = `{"source":"ECC",`
                              && `"elements":[{"id":"1010","name":"ASER - ALICANTE SPAIN","companyCode":"1010","language":"EN","plantAddress":{"id":"570009927","country":"ES","region": null}}]}`
                             count = 1 ) INTO TABLE lt_json_exp.

    f_cut->get_plant(
      EXPORTING
        io_cust     =  lo_cust
      IMPORTING
        et_plant    = lt_plant_act
        et_json     = DATA(lt_json)
        et_bapiret2 = lt_bapiret2_act ).

    DELETE lt_plant_act WHERE id <> '1010'.

    DATA(ls_json) = VALUE #( lt_json[ 1 ] OPTIONAL ).
    IF ls_json-elements CS  `"id":"1010","name":"ASER - ALICANTE SPAIN","companyCode":"1010",` .
      lt_json_act = lt_json_exp.
    ENDIF.

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_plant_act
      exp   = lt_plant_exp
      msg   = 'Plant assertion failed' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_bapiret2_act
      exp   = lt_bapiret2_exp
      msg = 'Bapiret2 assertion failed' ).

    me->check_send(
      EXPORTING
        it_json_act = lt_json_act
        io_cust     = lo_cust ).
  ENDMETHOD.


  METHOD get_sales_area.
    DATA: lt_json_act       TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_json_exp       TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_sales_area_act TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_sales_area,
          lt_sales_area_exp TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_sales_area.

    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                               iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                                               iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-sales_area ).
    lt_sales_area_exp = VALUE #( ( sales_organization = '1000'  distribution_channel = '10' division = '10' )
                                 ( sales_organization = '1000'  distribution_channel = '20' division = '10' )
                                 ( sales_organization = '1000'  distribution_channel = '30'  division = '10' ) ).

    INSERT VALUE #( elements = `{"source":"ECC",`
                          && `"elements":[{"salesOrganization":"1000","distributionChannel":"10","division":"10"},`
                          && `{"salesOrganization":"1000","distributionChannel":"10","division":"10"},`
                          && `{"salesOrganization":"1000","distributionChannel":"20","division":"10"}]}`
                       count = 3 ) INTO TABLE lt_json_exp.

    f_cut->get_sales_area(
      EXPORTING
        io_cust       =     lo_cust
      IMPORTING
        et_sales_area = DATA(lt_sales_area)
        et_json       = DATA(lt_json) ).

    DATA(ls_json) = lt_json[ 1 ].

    IF  ls_json-elements CS '{"salesOrganization":"1000","distributionChannel":"10","division":"10"}'.
      lt_json_act = lt_json_exp.
    ENDIF.

    DELETE lt_sales_area WHERE sales_organization <> '1000'.
    lt_sales_area_act = lt_sales_area.

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_sales_area_act
      exp   = lt_sales_area_exp
      msg = 'Sales Area assertion failed' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

    me->check_send(
      EXPORTING
        it_json_act = lt_json_act
        io_cust     = lo_cust ).

  ENDMETHOD.


  METHOD get_sales_org.

    DATA: lt_json_act      TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_json_exp      TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_sales_org_act TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_sales_org,
          lt_sales_org_exp TYPE /vpcoe/cl_rdp_org_data_obj=>gty_t_sales_org.

    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                               iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
                                                               iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-sales_organization ).

    lt_sales_org_exp = VALUE #( ( id = '1000' company_code = '1000' currency = 'USD' texts = VALUE #( ( name = 'Dom. Sales Org' language = 'EN' ) ) )
                                ( id = '1010' company_code = '1010' currency = 'USD' texts = VALUE #( ( name = 'Dom. Sales Org 2' language = 'EN' ) ) )
                                ( id = '1100' company_code = '1000' currency = 'USD' texts = VALUE #( ( name = 'Int. Sales Org' language = 'EN' ) ) )
                                ( id = '1110' company_code = '1010' currency = 'USD' texts = VALUE #( ( name = 'Int. Sales Org 2' language = 'EN' ) ) )
                                ( id = 'STBL' company_code = 'STBL' currency = 'USD' texts = VALUE #(  ( name = 'Dom. Sales Org' language = 'EN' ) ) )
                               ) .
    INSERT VALUE #( elements = `{"source":"ECC",`
                               && `"elements":[{"id":"1000","currency":"USD","companyCode":"1000","texts":[{"name":"Dom. Sales Org","language":"EN"}]},`
                               && `{"id":"1010","currency":"USD","companyCode":"1010","texts":[{"name":"Dom. Sales Org 2","language":"EN"}]},`
                               && `{"id":"1100","currency":"USD","companyCode":"1000","texts":[{"name":"Int. Sales Org","language":"EN"}]},`
                               && `{"id":"STBL","currency":"USD","companyCode":"STBL","texts":[{"name":"Dom. Sales Org","language":"EN"}]},`
                               && `{"id":"1110","currency":"USD","companyCode":"1010","texts":[{"name":"Int. Sales Org 2","language":"EN"}]}]}`
                                 count = 5 ) INTO TABLE lt_json_exp.

    f_cut->get_sales_org(
      EXPORTING
        io_cust      =     lo_cust
      IMPORTING
        et_json      = DATA(lt_json)
        et_sales_org = DATA(lt_sales_org)
    ).

    DATA(ls_json) = lt_json[ 1 ].

    IF  ls_json-elements CS '{"id":"1010","currency":"USD","companyCode":"1010","texts":[{"name":"Dom. Sales Org 2","language":"EN"}]}'.
      lt_json_act = lt_json_exp.
    ENDIF.

    DELETE lt_sales_org WHERE currency <> 'USD'.
    lt_sales_org_act = CORRESPONDING #( lt_sales_org ).

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
      act   = lt_sales_org_act
      exp   = lt_sales_org_exp
      msg = 'Sales Organization assertion failed' ).

    me->check_send(
      EXPORTING
        it_json_act = lt_json_act
        io_cust     = lo_cust ).

  ENDMETHOD.


*  METHOD get_val_area_company_code.
*    DATA: lt_json_act                  TYPE /vpcoe/cl_rdp_http=>gty_t_json,
*          lt_json_exp                  TYPE /vpcoe/cl_rdp_http=>gty_t_json,
*          lt_val_area_company_code_act TYPE /VPCOE/CL_rdp_ORG_DATA_OBJ=>gty_t_val_area_company_code,
*          lt_val_area_company_code_exp TYPE /VPCOE/CL_rdp_ORG_DATA_OBJ=>gty_t_val_area_company_code.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                 iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
*                                                                 iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-sales_organization ).
*
*    lt_val_area_company_code_exp = VALUE #(  ( company_code = '0001' valuation_area = '0001')
*                                             ( company_code = '1000' valuation_area = '1000')
*                                             ( company_code = '1010' valuation_area = '1010')
*                                             ( company_code = '1000' valuation_area = '1100')
*                                             ( company_code = 'STBL' valuation_area = 'B275')
*                                             ( company_code = 'DE01' valuation_area = 'DE01')
*                                             ( company_code = 'DE01' valuation_area = 'DE02')
*                                             ( company_code = 'STBL' valuation_area = 'STBL')
*                                             ( company_code = 'STBL' valuation_area = 'STBM')
*                                          ).
*
*    INSERT VALUE #( elements = `{"source":"ECC",`
*                   && `"elements":[{"valuationArea":"0001","companyCode":"0001"},`
*                   && `{"valuationArea":"1000","companyCode":"1000"},`
*                   && `{"valuationArea":"1010","companyCode":"1010"},`
*                   && `{"valuationArea":"1100","companyCode":"1000"},`
*                   && `{"valuationArea":"B275","companyCode":"STBL"},`
*                   && `{"valuationArea":"DE01","companyCode":"DE01"},`
*                   && `{"valuationArea":"DE02","companyCode":"DE01"},`
*                   && `{"valuationArea":"STBL","companyCode":"STBL"},`
*                   && `{"valuationArea":"STBM","companyCode":"STBL"}]}`
*                  count = 9 ) INTO TABLE lt_json_exp.
*
*    f_cut->get_val_area_company_code(
*      EXPORTING
*        io_cust                  = lo_cust
*      IMPORTING
*        et_val_area_company_code = lt_val_area_company_code_act
*        et_json                  = lt_json_act ).
*
*    cl_abap_unit_assert=>assert_equals(
*       act = lt_val_area_company_code_act
*       exp = lt_val_area_company_code_exp ).
*
*    cl_abap_unit_assert=>assert_equals(
*       act = lt_json_act
*       exp = lt_json_exp ).
*
*    me->check_send(
*      EXPORTING
*        it_json_act = lt_json_act
*        io_cust     = lo_cust ).
*  ENDMETHOD.

ENDCLASS.
