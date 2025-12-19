*CLASS /vpcoe/cl_rdp_material_doc_ut DEFINITION DEFERRED.
*CLASS /vpcoe/cl_rdp_material_doc DEFINITION LOCAL FRIENDS /vpcoe/cl_rdp_material_doc_ut.
*
*CLASS /vpcoe/cl_rdp_material_doc_ut DEFINITION FOR TESTING FINAL
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*  PRIVATE SECTION.
*
*    TYPES: ty_matl_exp TYPE STANDARD TABLE OF /vpcoe/matl_exp.
*    TYPES:  BEGIN OF ty_matl_id,
*              id TYPE mblnr,
*            END OF ty_matl_id.
*
*    DATA: f_cut   TYPE REF TO /vpcoe/cl_rdp_material_doc.
*    DATA: tt_matl_id TYPE STANDARD TABLE OF ty_matl_id .
*
*    CLASS-METHODS: class_setup.
*    CLASS-METHODS: class_teardown.
*    METHODS: setup.
*    METHODS: teardown.
*    METHODS: get_mat_doc_add_gr_prod_t_mode FOR TESTING.
*    METHODS: get_mat_doc_add_gr_st_imp_t_m FOR TESTING.
*    METHODS: get_mat_doc_add_gr_st_t_m FOR TESTING.
*    METHODS: get_mat_doc_add_gi_st_exp_t_m FOR TESTING.
*    METHODS: get_mat_doc_add_gi_st_t_m FOR TESTING.
*    METHODS: get_mat_doc_add_gr_po_imp_t_m FOR TESTING.
*    METHODS: get_mat_doc_add_gr_po_t_m FOR TESTING.
*    METHODS: store_json_t_m FOR TESTING.
*
*    METHODS: mat_doc_send FOR TESTING.
*
*    METHODS: select_exp_matl_data
*      IMPORTING
*        it_matl_id  LIKE tt_matl_id
*      EXPORTING
*        et_matl_exp TYPE ty_matl_exp.
*
*    METHODS: assert_material
*      IMPORTING
*        it_matl_act       TYPE /vpcoe/t_material
*        it_matl_exp       TYPE ty_matl_exp
*      RETURNING
*        VALUE(rv_message) TYPE string.
*
*ENDCLASS.
*
*
*CLASS /vpcoe/cl_rdp_material_doc_ut IMPLEMENTATION.
*
*  METHOD class_setup.
*
*  ENDMETHOD.
*
*
*  METHOD class_teardown.
*
*  ENDMETHOD.
*
*
*  METHOD setup.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                            iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                            iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*
*    f_cut = NEW /vpcoe/cl_rdp_material_doc( iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                             iv_package_size = lo_cust->get_package_size( )
*                                             iv_source       = lo_cust->get_source_id( ) ).
*
*  ENDMETHOD.
*
*
*  METHOD teardown.
*
*
*
*  ENDMETHOD.
*
*  METHOD get_mat_doc_add_gr_prod_t_mode.
*
*    DATA: ls_sel_opt     TYPE /vpcoe/s_selopt_mat_doc,
*          lt_mat_doc_act TYPE /vpcoe/t_material,
*          lt_mat_doc_exp TYPE /vpcoe/t_material,
*          lv_total_count TYPE /vpcoe/tt_log_sum.
*
*    DATA: lt_billdoc_count_act  TYPE /vpcoe/tt_log_sum.
*    DATA: lt_suppinv_count_act  TYPE /vpcoe/tt_log_sum.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                                iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ).
*
*    ls_sel_opt = VALUE #( matdocid = VALUE /vpcoe/tt_r_matdocid( ( sign = 'I' option = 'EQ' low = '5000000062' high = '00000000')
*                                                                 ( sign = 'I' option = 'EQ' low = '5000000177' high = '00000000') ) ).
*
*    select_exp_matl_data(
*      EXPORTING
*        it_matl_id  = VALUE #( ( id = '5000000177' ) ( id = '5000000062' ) )
*      IMPORTING
*        et_matl_exp = lt_mat_doc_exp
*    ).
*
*    f_cut->get_material_document(
*      EXPORTING
*        io_cust     = lo_cust
*        iv_code     = f_cut->sc_variants-gr_prod
*        is_sel_opt  = ls_sel_opt
*        io_log      = lo_log
*       iv_mode      = /vpcoe/cl_rdp_helper=>sc_mode-screen
*     IMPORTING
*       et_material_document = lt_mat_doc_act
*     CHANGING
*       ct_total_count = lv_total_count
*       ct_total_count_billdoc = lt_billdoc_count_act
*       ct_total_count_suppinv = lt_suppinv_count_act ).
*
*    SORT lt_mat_doc_exp BY id.
*    SORT lt_mat_doc_act BY id.
*
*    DATA(lv_message) = assert_material( it_matl_act = lt_mat_doc_act it_matl_exp = lt_mat_doc_exp ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_mat_doc_act
*      exp   = lt_mat_doc_exp
*      ).
*
*  ENDMETHOD.
*
*  METHOD get_mat_doc_add_gr_st_imp_t_m.
*
*    DATA: ls_sel_opt     TYPE /vpcoe/s_selopt_mat_doc,
*          lt_mat_doc_act TYPE /vpcoe/t_material,
*          lt_mat_doc_exp TYPE /vpcoe/t_material,
*          lv_total_count TYPE /vpcoe/tt_log_sum.
*    DATA: lt_billdoc_count_act  TYPE /vpcoe/tt_log_sum.
*    DATA: lt_suppinv_count_act  TYPE /vpcoe/tt_log_sum.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                                iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ).
*
*    ls_sel_opt-matdocid = VALUE #( ( sign = 'I' option = 'EQ' low = '4900000242' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000121' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000126' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000160' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000123' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000152' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000062' high = '00000000')
*                                    ).
*
*    select_exp_matl_data(
*          EXPORTING
*            it_matl_id  = VALUE #( ( id = '4900000242' )
*                                   ( id = '5000000121' )
*                                   ( id = '5000000126' )
*                                   ( id = '5000000160' )
*                                   ( id = '5000000123' )
*                                   ( id = '5000000152' ) )
*          IMPORTING
*            et_matl_exp = lt_mat_doc_exp
*        ).
*
*    f_cut->get_material_document(
*      EXPORTING
*        io_cust     = lo_cust
*        iv_code     = f_cut->sc_variants-gr_st_imp
*        is_sel_opt  = ls_sel_opt
*        io_log      = lo_log
*        iv_mode     = /vpcoe/cl_rdp_helper=>sc_mode-screen
*     IMPORTING
*       et_material_document = lt_mat_doc_act
*     CHANGING
*       ct_total_count = lv_total_count
*       ct_total_count_billdoc = lt_billdoc_count_act
*       ct_total_count_suppinv = lt_suppinv_count_act ).
*
*    SORT lt_mat_doc_exp BY id.
*    SORT lt_mat_doc_act BY id.
*
*    DELETE lt_mat_doc_exp WHERE id = '4900000242' AND item = '0001'.
*
*    DATA(lv_message) = assert_material( it_matl_act = lt_mat_doc_act it_matl_exp = lt_mat_doc_exp ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_mat_doc_act
*      exp   = lt_mat_doc_exp
*      ).
*
*  ENDMETHOD.
*
*  METHOD get_mat_doc_add_gr_st_t_m.
*    DATA: ls_sel_opt     TYPE /vpcoe/s_selopt_mat_doc,
*          lt_mat_doc_act TYPE /vpcoe/t_material,
*          lt_mat_doc_exp TYPE /vpcoe/t_material,
*          lv_total_count TYPE /vpcoe/tt_log_sum.
*    DATA: lt_billdoc_count_act  TYPE /vpcoe/tt_log_sum.
*    DATA: lt_suppinv_count_act  TYPE /vpcoe/tt_log_sum.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                                iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ).
*
*    ls_sel_opt-matdocid = VALUE #( ( sign = 'I' option = 'EQ' low = '4900000185' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000068' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000264' high = '00000000')
*     ).
*
*    select_exp_matl_data(
*          EXPORTING
*            it_matl_id  = VALUE #( ( id = '4900000185' )
*                                   ( id = '5000000068' )
*                                   ( id = '4900000264' )  )
*          IMPORTING
*            et_matl_exp = lt_mat_doc_exp
*        ).
*
*    f_cut->get_material_document(
*      EXPORTING
*        io_cust     = lo_cust
*        iv_code     = f_cut->sc_variants-gr_st
*        is_sel_opt  = ls_sel_opt
*        io_log      = lo_log
*        iv_mode     = /vpcoe/cl_rdp_helper=>sc_mode-screen
*     IMPORTING
*       et_material_document = lt_mat_doc_act
*     CHANGING
*       ct_total_count = lv_total_count
*       ct_total_count_billdoc = lt_billdoc_count_act
*       ct_total_count_suppinv = lt_suppinv_count_act ).
*
*    SORT lt_mat_doc_exp BY id.
*    SORT lt_mat_doc_act BY id.
*
*    DATA(lv_message) = assert_material( it_matl_act = lt_mat_doc_act it_matl_exp = lt_mat_doc_exp ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_mat_doc_act
*      exp   = lt_mat_doc_exp
*      ).
*
*  ENDMETHOD.
*
*  METHOD get_mat_doc_add_gi_st_exp_t_m.
*
*    DATA: ls_sel_opt     TYPE /vpcoe/s_selopt_mat_doc,
*          lt_mat_doc_act TYPE /vpcoe/t_material,
*          lt_mat_doc_exp TYPE /vpcoe/t_material,
*          lv_total_count TYPE /vpcoe/tt_log_sum.
*    DATA: lt_billdoc_count_act  TYPE /vpcoe/tt_log_sum.
*    DATA: lt_suppinv_count_act  TYPE /vpcoe/tt_log_sum.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                                iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ).
*
*    ls_sel_opt-matdocid = VALUE #( ( sign = 'I' option = 'EQ' low = '4900000177' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000178' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000215' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000220' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000174' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000175' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000190' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000242' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000270' high = '00000000') ).
*
*    select_exp_matl_data(
*          EXPORTING
*            it_matl_id  = VALUE #( ( id = '4900000177' )
*                                   ( id = '4900000178' )
*                                   ( id = '4900000215' )
*                                   ( id = '4900000220' )
*                                   ( id = '4900000174' )
*                                   ( id = '4900000175' )
*                                   ( id = '4900000190' )
*                                   ( id = '4900000242' )
*                                   ( id = '4900000270' ) )
*          IMPORTING
*            et_matl_exp = lt_mat_doc_exp
*        ).
*
*    DELETE lt_mat_doc_exp WHERE item <> '0001'.
*
*    f_cut->get_material_document(
*      EXPORTING
*        io_cust     = lo_cust
*        iv_code     = f_cut->sc_variants-gi_st_exp
*        is_sel_opt  = ls_sel_opt
*        io_log      = lo_log
*        iv_mode = /vpcoe/cl_rdp_helper=>sc_mode-screen
*     IMPORTING
*       et_material_document = lt_mat_doc_act
*     CHANGING
*       ct_total_count = lv_total_count
*       ct_total_count_billdoc = lt_billdoc_count_act
*       ct_total_count_suppinv = lt_suppinv_count_act ).
*
*    SORT lt_mat_doc_exp BY id.
*    SORT lt_mat_doc_act BY id.
*
*    DATA(lv_message) = assert_material( it_matl_act = lt_mat_doc_act it_matl_exp = lt_mat_doc_exp ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_mat_doc_act
*      exp   = lt_mat_doc_exp
*      ).
*
*  ENDMETHOD.
*
*  METHOD get_mat_doc_add_gi_st_t_m.
*    DATA: ls_sel_opt     TYPE /vpcoe/s_selopt_mat_doc,
*          lt_mat_doc_act TYPE /vpcoe/t_material,
*          lt_mat_doc_exp TYPE /vpcoe/t_material,
*          lv_total_count TYPE /vpcoe/tt_log_sum.
*    DATA: lt_billdoc_count_act  TYPE /vpcoe/tt_log_sum.
*    DATA: lt_suppinv_count_act  TYPE /vpcoe/tt_log_sum.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                                iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ).
*
*    ls_sel_opt-matdocid = VALUE #( ( sign = 'I' option = 'EQ' low = '4900000104' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000105' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000241' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000261' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000192' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000134' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '4900000201' high = '00000000')
*                                    ).
*
*    ls_sel_opt = VALUE #( matdocid = VALUE #( ( sign = 'I' option = 'EQ' low = '4900000104' high = '00000000')
*                                              ( sign = 'I' option = 'EQ' low = '4900000105' high = '00000000')
*                                              ( sign = 'I' option = 'EQ' low = '4900000241' high = '00000000')
*                                              ( sign = 'I' option = 'EQ' low = '4900000261' high = '00000000')
*                                              ( sign = 'I' option = 'EQ' low = '4900000192' high = '00000000')
*                                              ( sign = 'I' option = 'EQ' low = '4900000134' high = '00000000')
*                                              ( sign = 'I' option = 'EQ' low = '4900000201' high = '00000000') )
*
*                                        ).
*
*    select_exp_matl_data(
*      EXPORTING
*        it_matl_id  = VALUE #( ( id = '4900000104' )
*                               ( id = '4900000105' )
*                               ( id = '4900000241' )
*                               ( id = '4900000261' )
*                               ( id = '4900000192' )
*                               ( id = '4900000134' )
*                               ( id = '4900000201' ) )
*      IMPORTING
*        et_matl_exp = lt_mat_doc_exp
*    ).
*
*    f_cut->get_material_document(
*      EXPORTING
*        io_cust     = lo_cust
*        iv_code     = f_cut->sc_variants-gi_st
*        is_sel_opt  = ls_sel_opt
*        io_log      = lo_log
*        iv_mode = /vpcoe/cl_rdp_helper=>sc_mode-screen
*     IMPORTING
*       et_material_document = lt_mat_doc_act
*     CHANGING
*       ct_total_count = lv_total_count
*       ct_total_count_billdoc = lt_billdoc_count_act
*       ct_total_count_suppinv = lt_suppinv_count_act ).
*
*    DELETE lt_mat_doc_act WHERE item = '0003'.
*
*    SORT lt_mat_doc_exp BY id.
*    SORT lt_mat_doc_act BY id.
*
*    DATA(lv_message) = assert_material( it_matl_act = lt_mat_doc_act it_matl_exp = lt_mat_doc_exp ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_mat_doc_act
*      exp   = lt_mat_doc_exp
*      ).
*
*  ENDMETHOD.
*
*  METHOD get_mat_doc_add_gr_po_imp_t_m.
*    DATA: ls_sel_opt     TYPE /vpcoe/s_selopt_mat_doc,
*          lt_mat_doc_act TYPE /vpcoe/t_material,
*          lt_mat_doc_exp TYPE /vpcoe/t_material,
*          lv_total_count TYPE /vpcoe/tt_log_sum.
*
*    DATA: lt_billdoc_count_act  TYPE /vpcoe/tt_log_sum.
*    DATA: lt_suppinv_count_act  TYPE /vpcoe/tt_log_sum.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                                iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ).
*
*    ls_sel_opt-matdocid = VALUE #( ( sign = 'I' option = 'EQ' low = '5000000082' high = '00000000') ).
*
*    select_exp_matl_data(
*      EXPORTING
*        it_matl_id  = VALUE #( ( id = '5000000044' )
*                               ( id = '5000000043' )
*                               ( id = '5000000093' )
*                               ( id = '5000000082' ) )
*      IMPORTING
*        et_matl_exp = lt_mat_doc_exp ).
*
*
*    f_cut->get_material_document(
*      EXPORTING
*        io_cust     = lo_cust
*        iv_code     = f_cut->sc_variants-gr_po_imp
*        is_sel_opt  = ls_sel_opt
*        io_log      = lo_log
*        iv_mode = /vpcoe/cl_rdp_helper=>sc_mode-screen
*     IMPORTING
*       et_material_document = lt_mat_doc_act
*     CHANGING
*       ct_total_count = lv_total_count
*       ct_total_count_billdoc = lt_billdoc_count_act
*       ct_total_count_suppinv = lt_suppinv_count_act ).
*
*    SORT lt_mat_doc_exp BY id.
*    SORT lt_mat_doc_act BY id.
*
*    DATA(lv_message) = assert_material( it_matl_act = lt_mat_doc_act it_matl_exp = lt_mat_doc_exp ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_mat_doc_act
*      exp   = lt_mat_doc_exp
*      ).
*
*  ENDMETHOD.
*
*  METHOD get_mat_doc_add_gr_po_t_m.
*    DATA: ls_sel_opt     TYPE /vpcoe/s_selopt_mat_doc,
*          lt_mat_doc_act TYPE /vpcoe/t_material,
*          lt_mat_doc_exp TYPE /vpcoe/t_material,
*          lv_total_count TYPE /vpcoe/tt_log_sum.
*
*    DATA: lt_billdoc_count_act  TYPE /vpcoe/tt_log_sum.
*    DATA: lt_suppinv_count_act  TYPE /vpcoe/tt_log_sum.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                                iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ).
*
*    ls_sel_opt-matdocid = VALUE #( ( sign = 'I' option = 'EQ' low = '5000000002' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000004' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000005' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000151' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000153' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000148' high = '00000000')
*                                   ( sign = 'I' option = 'EQ' low = '5000000149' high = '00000000') ).
*
*
*    select_exp_matl_data(
*      EXPORTING
*        it_matl_id  = VALUE #( ( id = '5000000002' )
*                               ( id = '5000000004' )
*                               ( id = '5000000005' )
*                               ( id = '5000000151' )
*                               ( id = '5000000153' )
*                               ( id = '5000000148' )
*                               ( id = '5000000149' ) )
*      IMPORTING
*        et_matl_exp = lt_mat_doc_exp
*).
*
*    f_cut->get_material_document(
*      EXPORTING
*        io_cust     = lo_cust
*        iv_code     = f_cut->sc_variants-gr_po
*        is_sel_opt  = ls_sel_opt
*        io_log      = lo_log
*       iv_mode = /vpcoe/cl_rdp_helper=>sc_mode-screen
*     IMPORTING
*       et_material_document = lt_mat_doc_act
*     CHANGING
*       ct_total_count = lv_total_count
*       ct_total_count_billdoc = lt_billdoc_count_act
*       ct_total_count_suppinv = lt_suppinv_count_act ).
*
*    SORT lt_mat_doc_exp BY id.
*    SORT lt_mat_doc_act BY id.
*
*    DATA(lv_message) = assert_material( it_matl_act = lt_mat_doc_act it_matl_exp = lt_mat_doc_exp ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = lt_mat_doc_act
*      exp   = lt_mat_doc_exp
*      ).
*
*  ENDMETHOD.
*
*
*  METHOD mat_doc_send.
*
*    DATA: ls_sel_opt   TYPE /vpcoe/s_selopt_mat_doc,
*          lt_total_exp TYPE /vpcoe/tt_log_sum,
*          lt_total_act TYPE /vpcoe/tt_log_sum.
*
*    DATA: lt_billdoc_count_act  TYPE /vpcoe/tt_log_sum.
*    DATA: lt_suppinv_count_act  TYPE /vpcoe/tt_log_sum.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                                iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ).
*
*    ls_sel_opt = VALUE #( country = VALUE /vpcoe/tt_r_land1( ( sign = 'I' option = 'EQ' low = 'US' ) ) ).
*
*    lt_total_exp = VALUE #( ( total = 20 total_failed = 0 sub_object = 'MATERIAL_DOCUMENTS' ) ).
*
*    f_cut->get_material_document(
*      EXPORTING
*        io_cust         = lo_cust
*        iv_code         = f_cut->sc_variants-gr_prod
*        is_sel_opt      = ls_sel_opt
*        io_log          = lo_log
*        iv_mode         = /vpcoe/cl_rdp_helper=>sc_mode-send
*        iv_send_mat_doc = abap_true
*        iv_send         = abap_true
*     CHANGING
*       ct_total_count = lt_total_act
*       ct_total_count_billdoc = lt_billdoc_count_act
*       ct_total_count_suppinv = lt_suppinv_count_act ).
*
*    cl_abap_unit_assert=>assert_equals(
*     act   = lt_total_act
*     exp   = lt_total_exp ).
*
*  ENDMETHOD.
*
*  METHOD  store_json_t_m .
*
*    DATA: ls_json_exp    TYPE /vpcoe/cl_rdp_http=>gty_s_json,
*          ls_json_act    TYPE /vpcoe/cl_rdp_http=>gty_s_json,
*          lt_mat_doc     TYPE /vpcoe/t_material,
*          lt_mat_doc_jsn TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data.
*
*    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
*                                                                iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
*                                                                iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc ).
*
*    select_exp_matl_data(
*  EXPORTING
*    it_matl_id  = VALUE #( ( id = '5000000002' )
*                           ( id = '5000000004' )
*                           ( id = '5000000005' )
*                           ( id = '5000000151' )
*                           ( id = '5000000153' )
*                           ( id = '5000000148' )
*                           ( id = '5000000149' ) )
*  IMPORTING
*    et_matl_exp = lt_mat_doc ).
*
*    ls_json_exp-count = 7.
*
*    ls_json_exp-elements = `{"source": "ECC",` &&
*                          ` "elements": [ {` &&
*                          `"id": "5000000002",` &&
*                          `"item": 1,` &&
*                          `"line": 1,` &&
*                          `"year": 2021,` &&
*                          `"postingDate": "2021-10-28",` &&
*                          `"creationDate": "2021-10-28",` &&
*                          `"creationTime": "08:56:18",` &&
*                          `"plant": "STBL",` &&
*                          `"issuingOrReceivingPlant": null,` &&
*                          `"stockMaterial": "RAW01",` &&
*                          `"inventoryStockType": "E",` &&
*                          `"companyCode": "STBL",` &&
*                          `"baseUnitOfMeasure": "EA",` &&
*                          `"quantityInBaseUnit": 1.000,` &&
*                          `"materialStockChangeQtyInBaseUnit": 1.000,` &&
*                          `"entryUnit": "EA",` &&
*                          `"quantityInEntryUnit": 1.000,` &&
*                          `"inventoryManagementReferenceDocument": null,` &&
*                          `"referenceDocument": null,` &&
*                          `"salesOrder": "0000000030",` &&
*                          `"purchaseOrder": "4500000002",` &&
*                          `"productionOrder": null,` &&
*                          `"delivery": null,` &&
*                           `"isReversalMovementType": false,` &&
*                           `"goodsMovementType": "101",` &&
*                           `"product": "RAW01",` &&
*                           `"issuingOrReceivingProduct": null,` &&
*                           `"batch": "0000000007",` &&
*                           `"manufactureDate": null,` &&
*                           `"issuingOrReceivingBatch": null,` &&
*                           `"supplier": "TEST",` &&
*                           `"customer": null,` &&
*                           `"goodsRecipientName": null,` &&
*                           `"referenceDocumentType": "B",` &&
*                           `"accountingDocumentType": "WE",` &&
*                           `"transactionType": "WE",` &&
*                           `"stockChangeCategory": "GR",` &&
*                           `"isCrossPlantTransfer": false,` &&
*                           `"logisticProcess": "GR_PO",` &&
*                           `"incoterms": null,` &&
*                           `"shipToCountry": "US",` &&
*                           `"shipToRegion": "CA",` &&
*                           `"shipFromCountry": "US",` &&
*                           `"goodsSupplier": null,` &&
*                           `"isCancelled": null,` &&
*                           `"purchaseOrderItem": 10,` &&
*                           `"deliveryDocumentItem": 0 },` &&
*                           `{ "id": "5000000004",` &&
*                           `"item": 1,` &&
*                           `"line": 1,` &&
*                           `"year": 2021,` &&
*                           `"postingDate": "2021-11-02",` &&
*                           `"creationDate": "2021-11-02",` &&
*                           `"creationTime": "11:00:04",` &&
*                           `"plant": "STBL",` &&
*                           `"issuingOrReceivingPlant": null,` &&
*                           `"stockMaterial": "RAW01",` &&
*                           `"inventoryStockType": null,` &&
*                           `"companyCode": "STBL",` &&
*                           `"baseUnitOfMeasure": "EA",` &&
*                           `"quantityInBaseUnit": 1.000,` &&
*                           `"materialStockChangeQtyInBaseUnit": 1.000,` &&
*                           `"entryUnit": "EA",` &&
*                           `"quantityInEntryUnit": 1.000,` &&
*                           `"inventoryManagementReferenceDocument": null,` &&
*                           `"referenceDocument": null,` &&
*                           `"salesOrder": null,` &&
*                           `"purchaseOrder": "4500000004",` &&
*                           `"productionOrder": null,` &&
*                           `"delivery": null,` &&
*                           `"isReversalMovementType": false,` &&
*                           `"goodsMovementType": "101",` &&
*                           `"product": "RAW01",` &&
*                           `"issuingOrReceivingProduct": null,` &&
*                           `"batch": "0000000014",` &&
*                           `"manufactureDate": null,` &&
*                           `"issuingOrReceivingBatch": null,` &&
*                           `"supplier": "TEST",` &&
*                           `"customer": null,` &&
*                           `"goodsRecipientName": null,` &&
*                           `"referenceDocumentType": "B",` &&
*                           `"accountingDocumentType": "WE",` &&
*                           `"transactionType": "WE",` &&
*                           `"stockChangeCategory": "GR",` &&
*                           `"isCrossPlantTransfer": false,` &&
*                           `"logisticProcess": "GR_PO",` &&
*                           `"incoterms": null,` &&
*                           `"shipToCountry": "US",` &&
*                           `"shipToRegion": "CA",` &&
*                           `"shipFromCountry": "US",` &&
*                           `"goodsSupplier": null,` &&
*                           `"isCancelled": true,` &&
*                           `"purchaseOrderItem": 10,` &&
*                           `"deliveryDocumentItem": 0 },` &&
*                          `{ "id": "5000000005",` &&
*                            `"item": 1,` &&
*                            `"line": 1,` &&
*                            `"year": 2021,` &&
*                            `"postingDate": "2021-11-02",` &&
*                            `"creationDate": "2021-11-02",` &&
*                            `"creationTime": "11:02:32",` &&
*                            `"plant": "STBL",` &&
*                            `"issuingOrReceivingPlant": null,` &&
*                            `"stockMaterial": "RAW01",` &&
*                            `"inventoryStockType": null,` &&
*                            `"companyCode": "STBL",` &&
*                            `"baseUnitOfMeasure": "EA",` &&
*                            `"quantityInBaseUnit": 1.000,` &&
*                            `"materialStockChangeQtyInBaseUnit": -1.000,` &&
*                            `"entryUnit": "EA",` &&
*                            `"quantityInEntryUnit": 1.000,` &&
*                            `"inventoryManagementReferenceDocument": null,` &&
*                            `"referenceDocument": null,` &&
*                            `"salesOrder": null,` &&
*                            `"purchaseOrder": "4500000004",` &&
*                            `"productionOrder": null,` &&
*                            `"delivery": null,` &&
*                            `"isReversalMovementType": true,` &&
*                            `"goodsMovementType": "102",` &&
*                            `"product": "RAW01",` &&
*                            `"issuingOrReceivingProduct": null,` &&
*                            `"batch": "0000000014",` &&
*                            `"manufactureDate": null,` &&
*                            `"issuingOrReceivingBatch": null,` &&
*                            `"supplier": "TEST",` &&
*                            `"customer": null,` &&
*                            `"goodsRecipientName": null,` &&
*                            `"referenceDocumentType": "B",` &&
*                            `"accountingDocumentType": "WE",` &&
*                            `"transactionType": "WE",` &&
*                            `"stockChangeCategory": "GR",` &&
*                            `"isCrossPlantTransfer": false,` &&
*                            `"logisticProcess": "GR_PO",` &&
*                            `"incoterms": null,` &&
*                            `"shipToCountry": "US",` &&
*                            `"shipToRegion": "CA",` &&
*                            `"shipFromCountry": "US",` &&
*                            `"goodsSupplier": null,` &&
*                            `"isCancelled": null,` &&
*                            `"purchaseOrderItem": 10,` &&
*                            `"deliveryDocumentItem": 0 ,` &&
*                          `{"id": "5000000148",` &&
*                            `"item": 1,` &&
*                            `"line": 1,` &&
*                            `"year": 2023,` &&
*                            `"postingDate": "2023-06-08",` &&
*                            `"creationDate": "2023-11-08",` &&
*                            `"creationTime": "16:06:03",` &&
*                            `"plant": "STBL",` &&
*                            `"issuingOrReceivingPlant": null,` &&
*                            `"stockMaterial": "RAW01",` &&
*                            `"inventoryStockType": null,` &&
*                            `"companyCode": "STBL",` &&
*                            `"baseUnitOfMeasure": "EA",` &&
*                            `"quantityInBaseUnit": 6.000,` &&
*                            `"materialStockChangeQtyInBaseUnit": -6.000,` &&
*                            `"entryUnit": "EA",` &&
*                            `"quantityInEntryUnit": 6.000,` &&
*                            `"inventoryManagementReferenceDocument": null,` &&
*                            `"referenceDocument": null,` &&
*                            `"salesOrder": null,` &&
*                            `"purchaseOrder": "4500000163",` &&
*                            `"productionOrder": null,` &&
*                            `"delivery": null,` &&
*                            `"isReversalMovementType": false,` &&
*                            `"goodsMovementType": "161",` &&
*                            `"product": "RAW01",` &&
*                            `"issuingOrReceivingProduct": null,` &&
*                            `"batch": "0000000136",` &&
*                            `"manufactureDate": null,` &&
*                            `"issuingOrReceivingBatch": null,` &&
*                            `"supplier": "TEST",` &&
*                            `"customer": null,` &&
*                            `"goodsRecipientName": null,` &&
*                            `"referenceDocumentType": "B",` &&
*                            `"accountingDocumentType": null,` &&
*                            `"transactionType": "WE",` &&
*                            `"stockChangeCategory": "GR",` &&
*                            `"isCrossPlantTransfer": false,` &&
*                            `"logisticProcess": "GR_PO",` &&
*                            `"incoterms": null,` &&
*                            `"shipToCountry": "US",` &&
*                            `"shipToRegion": "CA",` &&
*                            `"shipFromCountry": "US",` &&
*                            `"goodsSupplier": null,` &&
*                            `"isCancelled": true,` &&
*                            `"purchaseOrderItem": 10,` &&
*                            `"deliveryDocumentItem": 0 },` &&
*                          `{ "id": "5000000149",` &&
*                            `"item": 1,` &&
*                            `"line": 1,` &&
*                            `"year": 2023,` &&
*                            `"postingDate": "2023-06-08",` &&
*                            `"creationDate": "2023-11-08",` &&
*                            `"creationTime": "16:06:51",` &&
*                            `"plant": "STBL",` &&
*                            `"issuingOrReceivingPlant": null,` &&
*                            `"stockMaterial": "RAW01",` &&
*                            `"inventoryStockType": null,` &&
*                            `"companyCode": "STBL",` &&
*                            `"baseUnitOfMeasure": "EA",` &&
*                            `"quantityInBaseUnit": 6.000,` &&
*                            `"materialStockChangeQtyInBaseUnit": 6.000,` &&
*                            `"entryUnit": "EA",` &&
*                            `"quantityInEntryUnit": 6.000,` &&
*                            `"inventoryManagementReferenceDocument": null,` &&
*                            `"referenceDocument": null,` &&
*                            `"salesOrder": null,` &&
*                            `"purchaseOrder": "4500000163",` &&
*                            `"productionOrder": null,` &&
*                            `"delivery": null,` &&
*                            `"isReversalMovementType": true,` &&
*                            `"goodsMovementType": "162",` &&
*                            `"product": "RAW01",` &&
*                            `"issuingOrReceivingProduct": null,` &&
*                            `"batch": "0000000136",` &&
*                            `"manufactureDate": null,` &&
*                            `"issuingOrReceivingBatch": null,` &&
*                            `"supplier": "TEST",` &&
*                            `"customer": null,` &&
*                            `"goodsRecipientName": null,` &&
*                            `"referenceDocumentType": "B",` &&
*                            `"accountingDocumentType": null,` &&
*                            `"transactionType": "WE",` &&
*                            `"stockChangeCategory": "GR",` &&
*                            `"isCrossPlantTransfer": false,` &&
*                            `"logisticProcess": "GR_PO",` &&
*                            `"incoterms": null,` &&
*                            `"shipToCountry": "US",` &&
*                            `"shipToRegion": "CA",` &&
*                            `"shipFromCountry": "US",` &&
*                            `"goodsSupplier": null,` &&
*                            `"isCancelled": null,` &&
*                            `"purchaseOrderItem": 10,` &&
*                            `"deliveryDocumentItem": 0 },` &&
*                            `{"id": "5000000151",` &&
*                              `"item": 1,` &&
*                              `"line": 1,` &&
*                              `"year": 2023,` &&
*                              `"postingDate": "2023-06-08",` &&
*                              `"creationDate": "2023-11-08",` &&
*                              `"creationTime": "16:12:33",` &&
*                              `"plant": "STBL",` &&
*                              `"issuingOrReceivingPlant": null,` &&
*                              `"stockMaterial": "RAW01",` &&
*                              `"inventoryStockType": null,` &&
*                              `"companyCode": "STBL",` &&
*                              `"baseUnitOfMeasure": "EA",` &&
*                              `"quantityInBaseUnit": 4.000,` &&
*                              `"materialStockChangeQtyInBaseUnit": -4.000,` &&
*                              `"entryUnit": "EA",` &&
*                              `"quantityInEntryUnit": 4.000,` &&
*                              `"inventoryManagementReferenceDocument": null,` &&
*                              `"referenceDocument": null,` &&
*                              `"salesOrder": null,` &&
*                              `"purchaseOrder": "4500000164",` &&
*                              `"productionOrder": null,` &&
*                              `"delivery": null,` &&
*                              `"isReversalMovementType": false,` &&
*                              `"goodsMovementType": "122",` &&
*                              `"product": "RAW01",` &&
*                              `"issuingOrReceivingProduct": null,` &&
*                              `"batch": "0000000137",` &&
*                              `"manufactureDate": null,` &&
*                              `"issuingOrReceivingBatch": null,` &&
*                              `"supplier": "TEST",` &&
*                              `"customer": null,` &&
*                              `"goodsRecipientName": null,` &&
*                              `"referenceDocumentType": "B",` &&
*                              `"accountingDocumentType": null,` &&
*                              `"transactionType": "WE",` &&
*                              `"stockChangeCategory": "GR",` &&
*                              `"isCrossPlantTransfer": false,` &&
*                              `"logisticProcess": "GR_PO",` &&
*                              `"incoterms": null,` &&
*                              `"shipToCountry": "US",` &&
*                              `"shipToRegion": "CA",` &&
*                              `"shipFromCountry": "US",` &&
*                              `"goodsSupplier": null,` &&
*                              `"isCancelled": true,` &&
*                              `"purchaseOrderItem": 10,` &&
*                              `"deliveryDocumentItem": 0 },` &&
*                            `{"id": "5000000153",` &&
*                              `"item": 1,` &&
*                              `"line": 1,` &&
*                              `"year": 2023,` &&
*                              `"postingDate": "2023-06-08",` &&
*                              `"creationDate": "2023-11-13",` &&
*                              `"creationTime": "18:05:32",` &&
*                              `"plant": "STBL",` &&
*                              `"issuingOrReceivingPlant": null,` &&
*                              `"stockMaterial": "RAW01",` &&
*                              `"inventoryStockType": null,` &&
*                              `"companyCode": "STBL",` &&
*                              `"baseUnitOfMeasure": "EA",` &&
*                              `"quantityInBaseUnit": 4.000,` &&
*                              `"materialStockChangeQtyInBaseUnit": 4.000,` &&
*                              `"entryUnit": "EA",` &&
*                              `"quantityInEntryUnit": 4.000,` &&
*                              `"inventoryManagementReferenceDocument": null,` &&
*                              `"referenceDocument": null,` &&
*                              `"salesOrder": null,` &&
*                              `"purchaseOrder": "4500000164",` &&
*                              `"productionOrder": null,` &&
*                              `"delivery": null,` &&
*                              `"isReversalMovementType": true,` &&
*                              `"goodsMovementType": "123",` &&
*                              `"product": "RAW01",` &&
*                              `"issuingOrReceivingProduct": null,` &&
*                              `"batch": "0000000137",` &&
*                              `"manufactureDate": null,` &&
*                              `"issuingOrReceivingBatch": null,` &&
*                              `"supplier": "TEST",` &&
*                              `"customer": null,` &&
*                              `"goodsRecipientName": null,` &&
*                              `"referenceDocumentType": "B",` &&
*                              `"accountingDocumentType": null,` &&
*                              `"transactionType": "WE",` &&
*                              `"stockChangeCategory": "GR",` &&
*                              `"isCrossPlantTransfer": false,` &&
*                              `"logisticProcess": "GR_PO",` &&
*                              `"incoterms": null,` &&
*                              `"shipToCountry": "US",` &&
*                              `"shipToRegion": "CA",` &&
*                              `"shipFromCountry": "US",` &&
*                              `"goodsSupplier": null,` &&
*                              `"isCancelled": null,` &&
*                              `"purchaseOrderItem": 10,` &&
*                              `"deliveryDocumentItem": 0 }]}.`.
*
*    lt_mat_doc_jsn = CORRESPONDING #( lt_mat_doc ).
*    f_cut->store_json(
*      EXPORTING
*        io_cust              = lo_cust
*        it_material_document = lt_mat_doc_jsn
*      IMPORTING
*        es_json              = ls_json_act ) .
*
*    cl_abap_unit_assert=>assert_equals(
*     act   = ls_json_act
*     exp   = ls_json_exp ).
*
*  ENDMETHOD.
*
*  METHOD select_exp_matl_data.
*
*    SELECT * FROM /vpcoe/matl_exp
*    FOR ALL ENTRIES IN @it_matl_id
*    WHERE id = @it_matl_id-id
*    INTO CORRESPONDING FIELDS OF TABLE @et_matl_exp.
*
*  ENDMETHOD.
*
*  METHOD assert_material.
*
*    CHECK it_matl_act NE it_matl_exp.
*
*    DATA: it_details TYPE abap_compdescr_tab.
*    DATA: lv_act_descr TYPE REF TO cl_abap_structdescr.
*    DATA: ls_matl_act TYPE /vpcoe/matl_exp .
*    DATA: lv_line_index TYPE int4.
*    DATA: lv_fieldname TYPE abap_compname .
*
*    rv_message = |{ rv_message }Material document assertion error. Hover over this message to see error description:{ cl_abap_char_utilities=>newline }|.
*
** 1. Check if act and exp tables have same size
*    IF lines( it_matl_act ) NE lines( it_matl_exp ).
*      rv_message = |{ rv_message }Different number of table records expected: |
*                   && |act[{ lines( it_matl_act ) }], exp[{ lines( it_matl_exp ) }] { cl_abap_char_utilities=>newline }|.
*      RETURN.
*    ENDIF.
*
** 2. Compare act and exp tables by values
*    LOOP AT it_matl_exp ASSIGNING FIELD-SYMBOL(<fs_matl_exp>).
*
*      lv_line_index = lv_line_index + 1.
*      ls_matl_act = it_matl_act[ id = <fs_matl_exp>-id item = <fs_matl_exp>-item line = <fs_matl_exp>-line ].
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*      IF <fs_matl_exp> NE ls_matl_act.
*
*        LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <fs_matl_exp> ) )->get_components( )
*                       ASSIGNING FIELD-SYMBOL(<fs_material_exp>).
*
*          ASSIGN COMPONENT <fs_material_exp>-name OF STRUCTURE it_matl_exp[ lv_line_index ] TO FIELD-SYMBOL(<fs_value_exp>).
*
*          lv_act_descr ?= cl_abap_typedescr=>describe_by_data( ls_matl_act ).
*          it_details[] = lv_act_descr->components[].
*
*          IF <fs_material_exp>-name = 'MAT_DOC_YEAR'. " this name differs from act table since field name 'year' is reserved
*            " and can't be used as database table field name
*            lv_fieldname = 'YEAR'.
*          ELSE.
*            lv_fieldname = <fs_material_exp>-name.
*          ENDIF.
*
*          ASSIGN COMPONENT lv_fieldname
*              OF STRUCTURE it_matl_act[ lv_line_index ] TO FIELD-SYMBOL(<fs_value_act>).
*
*          IF <fs_value_exp> NE <fs_value_act>.
*            rv_message = |{ rv_message }Line[{ lv_line_index }], field name[{ <fs_material_exp>-name }], |
*            && |act[{ <fs_value_act> }], exp[{ <fs_value_exp> }] { cl_abap_char_utilities=>newline }|.
*          ENDIF.
*
*        ENDLOOP.
*      ENDIF.
*    ENDLOOP.
*
*  ENDMETHOD.
*
*ENDCLASS.
