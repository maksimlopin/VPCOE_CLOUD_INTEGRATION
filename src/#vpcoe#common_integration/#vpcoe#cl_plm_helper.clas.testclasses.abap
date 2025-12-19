*CLASS /vpcoe/cl_rdp_custom_helperut DEFINITION DEFERRED.
*CLASS /vpcoe/cl_rdp_custom_helper DEFINITION LOCAL FRIENDS /vpcoe/cl_rdp_custom_helperut.
*
*CLASS /vpcoe/cl_rdp_custom_helperut DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*  PRIVATE SECTION.
*    DATA:
*      f_cut TYPE REF TO /vpcoe/cl_rdp_custom_helper.  "class under test
*
*    METHODS: add_sel_opt FOR TESTING.
*    METHODS: add_symsg_to_bapiret FOR TESTING.
*    METHODS: check_package_exist FOR TESTING.
*    METHODS: convert_deep_tab_to_flat FOR TESTING.
*    METHODS: convert_langu_code FOR TESTING.
*    METHODS: get_http_client FOR TESTING.
*    METHODS: get_month_year_last_run FOR TESTING.
*    CLASS-METHODS class_setup.
*    CLASS-METHODS class_teardown.
*    METHODS setup.
*    METHODS teardown.
*ENDCLASS.       "/VPCOE/CL_rdp_CUSTOM_HELPERut
*
*
*CLASS /vpcoe/cl_rdp_custom_helperut IMPLEMENTATION.
*  METHOD class_setup.
*  ENDMETHOD.
*  METHOD class_teardown.
*  ENDMETHOD.
*  METHOD setup.
*    f_cut = NEW /vpcoe/cl_rdp_custom_helper(  iv_api_type = /vpcoe/cl_rdp_custom_helper=>sc_api_type-rdp
*                                               iv_srv_grp = /vpcoe/cl_rdp_custom_helper=>sc_grp_id-material_doc
*                                               iv_srv_id  = /vpcoe/cl_rdp_custom_helper=>sc_service_id-material_doc ).
*  ENDMETHOD.
*  METHOD teardown.
*  ENDMETHOD.
*
*  METHOD add_sel_opt.
*
*    DATA: lt_cntry  TYPE RANGE OF t001w-land1,
*          ct_so_act TYPE RANGE OF t001w-land1,
*          ct_so_exp TYPE RANGE OF t001w-land1.
*
*    f_cut->add_sel_opt(
*      EXPORTING
*        it_so_gen = lt_cntry
*      CHANGING
*        ct_so     = ct_so_act
*    ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = ct_so_act
*      exp   = ct_so_exp
*    ).
*  ENDMETHOD.
*
*
*  METHOD add_symsg_to_bapiret.
*    DATA: lt_bapiret2_act TYPE bapiret2_t,
*          lt_bapiret2_exp TYPE bapiret2_t.
*
*    lt_bapiret2_exp = VALUE #( ( number = 000  log_msg_no = 000000 ) ).
*
*    f_cut->add_symsg_to_bapiret(
*      CHANGING
*        ct_bapiret2 = lt_bapiret2_act ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_bapiret2_act
*      exp   = lt_bapiret2_exp ).
*
*  ENDMETHOD.
*
*
*  METHOD check_package_exist.
*
**    cl_abap_unit_assert=>assert_equals(
**      act   = f_cut->check_package_exist( '/VPCOE/RDP_INTEGRATION' )
**      exp   = abap_true ).
*
*  ENDMETHOD.
*
*
*  METHOD convert_deep_tab_to_flat.
*    DATA: lt_company_code TYPE STANDARD TABLE OF /vpcoe/s_distr_channel WITH NON-UNIQUE KEY id.
*    FIELD-SYMBOLS: <fs_flat_data_act> TYPE STANDARD TABLE.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_custom_helper( iv_api_type = /vpcoe/cl_rdp_custom_helper=>sc_api_type-rdp
*                                                      iv_srv_grp  = /vpcoe/cl_rdp_custom_helper=>sc_grp_id-organization
*                                                      iv_srv_id   = /vpcoe/cl_rdp_custom_helper=>sc_service_id-division ).
*
*    DATA(lo_flat_data_exp) = NEW /vpcoe/s_distr_channel( ).
*
*    lt_company_code = VALUE #( ( id = '01' texts = VALUE #( ( language = 'EN' name = 'USA' ) ) ) ).
*
*    READ TABLE lt_company_code INTO lo_flat_data_exp->* INDEX 1.
*    ASSIGN lo_flat_data_exp->* TO FIELD-SYMBOL(<fs_flat_data_exp>).
*
*    DATA(lv_exp) = <fs_flat_data_exp>-id.
*    DATA(ro_flat_data_act) =  f_cut->convert_deep_tab_to_flat( it_deep_data = lt_company_code ).
*    ASSIGN ro_flat_data_act->* TO <fs_flat_data_act>.
*
*    READ TABLE <fs_flat_data_act> ASSIGNING FIELD-SYMBOL(<fs_flat_data>) INDEX 1.
*    ASSIGN COMPONENT 'ID' OF STRUCTURE <fs_flat_data> TO FIELD-SYMBOL(<fs>).
*    DATA(lv_act) = CONV /vpcoe/id_division( <fs> ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_act
*      exp   = lv_exp ).
*  ENDMETHOD.
*
*
*  METHOD convert_langu_code.
*
*    DATA(lv_langu_code) = CONV spras('EN').
*    DATA(lv_ext_langu_exp) = 'EN'.
*    DATA(lv_ext_langu_act) = f_cut->convert_langu_code( iv_int_langu =  lv_langu_code ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_ext_langu_act
*      exp   = lv_ext_langu_exp ).
*  ENDMETHOD.
*
*
*  METHOD get_http_client.
*    DATA: lt_bapiret2_exp TYPE bapiret2_t,
*          lt_bapiret2_act TYPE bapiret2_t.
*
*    f_cut->mv_api_type = /vpcoe/cl_rdp_custom_helper=>sc_api_type-rdp.
*    DATA(lo_http) = f_cut->get_http_client( IMPORTING et_bapiret2 = lt_bapiret2_act ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_bapiret2_act
*      exp   = lt_bapiret2_exp ).
*
*    cl_abap_unit_assert=>assert_bound(
*      EXPORTING
*        act              = lo_http ).
*
*  ENDMETHOD.
*
*
*  METHOD get_month_year_last_run.
*    f_cut->get_month_year_last_run(
*      EXPORTING
*        iv_last_run =   '20230808'
*      IMPORTING
*        ev_month    = DATA(lv_month_act)
*        ev_year     = DATA(lv_year_act) ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_month_act
*      exp   = '08' ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_year_act
*      exp   = '2023' ).
*  ENDMETHOD.
*
*ENDCLASS.
