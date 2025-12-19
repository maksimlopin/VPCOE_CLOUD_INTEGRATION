CLASS /vpcoe/cl_rdp_inventory_ut DEFINITION DEFERRED.
CLASS /vpcoe/cl_rdp_inventory DEFINITION LOCAL FRIENDS /vpcoe/cl_rdp_inventory_ut.

CLASS /vpcoe/cl_rdp_inventory_ut DEFINITION FOR TESTING FINAL
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_rdp_inventory.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: prepare_json FOR TESTING.
    METHODS: retrieve_stock_data FOR TESTING.
    METHODS: retrieve_stock_intransit FOR TESTING.
ENDCLASS.       "/vpcoe/cl_rdp_inventory_Ut


CLASS /vpcoe/cl_rdp_inventory_ut IMPLEMENTATION.

  METHOD class_setup.

  ENDMETHOD.


  METHOD class_teardown.

  ENDMETHOD.


  METHOD setup.
    f_cut = NEW #( iv_package_size = '1'
                   iv_source       = /vpcoe/cl_rdp_helper=>get_source_id( )
                   iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp ).
  ENDMETHOD.


  METHOD teardown.
  ENDMETHOD.

  METHOD prepare_json.
    DATA: lt_stock_result TYPE /vpcoe/cl_rdp_inventory=>gty_t_stock_data_rdp, "/vpcoe/cl_inventory_pfm=>gty_t_stock_data
          lt_json_act     TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_json_exp     TYPE /vpcoe/cl_rdp_http=>gty_t_json.

    INSERT VALUE #( plant                   = 'STBL'
                    product    	            = '000000000000000071'
                    stock_quantity_in_base_unit	= '100000.000'
                    base_unit_of_measure        = 'EA'
                    period-calendar_month    = '12'
                    period-calendar_year     = '2022' ) INTO TABLE lt_stock_result.

    f_cut->prepare_json(
      EXPORTING
        it_stock_result = lt_stock_result
      IMPORTING
        et_json         = lt_json_act ).

    INSERT VALUE #( elements = '{"source":"ECC","elements":[{"plant":"STBL","product":"000000000000000071","companyCode":"","stockQuantityInBaseUnit":100000.000,"baseUnitOfMeasure":"EA","period":{"calendarMonth":12,"calendarYear":2022}}]}'
                    count    = 1 ) INTO TABLE lt_json_exp.

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

  ENDMETHOD.


  METHOD retrieve_stock_data.
    DATA: ls_sel_opt          TYPE /vpcoe/s_selopt_inventory,
          lv_failed_act       TYPE abap_bool,
          lt_stock_result_exp TYPE /vpcoe/cl_rdp_inventory=>gty_t_stock_data_rdp,
          lo_log              TYPE REF TO /vpcoe/cl_rdp_log.

    lo_log = NEW #( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-inventory ).

    INSERT VALUE #( sign = 'I'
                    option = 'EQ'
                    low	   = '12' ) INTO TABLE ls_sel_opt-month.

    INSERT VALUE #( sign = 'I'
                    option = 'EQ'
                    low	   = '2022' ) INTO TABLE ls_sel_opt-year.

    INSERT VALUE #( sign = 'I'
                    option = 'EQ'
                    low	   = 'STBL' ) INTO TABLE ls_sel_opt-co_code.

    INSERT VALUE #( sign = 'I'
                    option = 'CP'
                    low	   = '*' ) INTO TABLE ls_sel_opt-plant.

    INSERT VALUE #( sign = 'I'
                    option = 'EQ'
                    low	   = '000000000000000071' ) INTO TABLE ls_sel_opt-prod.

    INSERT VALUE #( sign = 'I'
                    option = 'EQ'
                    low	   = 'E' ) INTO TABLE ls_sel_opt-sobkz_range.

    ls_sel_opt-sobkz = 'E'.
    ls_sel_opt-sbbst = 'X'.

    DATA(lt_stock_result_act) = f_cut->retrieve_stock_data(
                                      EXPORTING
                                        is_sel_opt = ls_sel_opt
                                        io_log     = lo_log
                                      IMPORTING
                                        ev_failed = lv_failed_act ).

    INSERT VALUE #( plant                   = 'STBL'
                    product    	            = '000000000000000071'
                    stock_quantity_in_base_unit	= '100000.000'
                    base_unit_of_measure        = 'EA'
                    period-calendar_month    = '12'
                    period-calendar_year     = '2022' ) INTO TABLE lt_stock_result_exp .

    cl_abap_unit_assert=>assert_equals(
      act = lv_failed_act
      exp = abap_false
      msg = |'Failed' assertion failed| ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_stock_result_act
      exp = lt_stock_result_exp
      msg = |Stock result assertion failed| ).
  ENDMETHOD.


  METHOD retrieve_stock_intransit.
    DATA: ls_sel_opt          TYPE /vpcoe/s_selopt_inventory,
          lv_failed_act       TYPE abap_bool,
          lo_log              TYPE REF TO /vpcoe/cl_rdp_log,
          lt_stock_result_act TYPE /vpcoe/cl_rdp_inventory=>gty_t_stock_data_rdp,
          lt_stock_result_exp TYPE /vpcoe/cl_rdp_inventory=>gty_t_stock_data_rdp.

    lo_log = NEW #( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-inventory ).

    INSERT VALUE #( sign = 'I'
                    option = 'EQ'
                    low	   = '12' ) INTO TABLE ls_sel_opt-month.

    INSERT VALUE #( sign = 'I'
                    option = 'EQ'
                    low	   = '2022' ) INTO TABLE ls_sel_opt-year.

    INSERT VALUE #( sign = 'I'
                    option = 'CP'
                    low	   = 'STBL' ) INTO TABLE ls_sel_opt-co_code.

    INSERT VALUE #( sign = 'I'
                    option = 'CP'
                    low	   = '*' ) INTO TABLE ls_sel_opt-plant.

    ls_sel_opt-trans = 'X'.

    lt_stock_result_act = f_cut->retrieve_stock_intransit(
                                    EXPORTING
                                      is_sel_opt = ls_sel_opt
                                      io_log     = lo_log
                                    IMPORTING
                                      ev_failed   = lv_failed_act ).

    INSERT VALUE #( plant                   = 'STBL'
                    product    	            = 'RAW01'
                    stock_quantity_in_base_unit	= '3.000'
                    base_unit_of_measure        = 'EA'
                    period-calendar_month    = '12'
                    period-calendar_year     = '2022' ) INTO TABLE lt_stock_result_exp.

    cl_abap_unit_assert=>assert_equals(
      act = lv_failed_act
      exp = abap_false
      msg = |'Failed' assertion failed| ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_stock_result_act
      exp = lt_stock_result_exp
      msg = |Stock result assertion failed| ).
  ENDMETHOD.

ENDCLASS.
