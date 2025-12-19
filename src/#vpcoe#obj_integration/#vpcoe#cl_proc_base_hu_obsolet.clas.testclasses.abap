*CLASS ltc_surdp_uph_proc_base_hu_ip DEFINITION DEFERRED.
*CLASS cl_surdp_uph_proc_base_hu DEFINITION LOCAL FRIENDS ltc_surdp_uph_proc_base_hu_ip.
*CLASS ltc_surdp_uph_proc_base_hu_ip DEFINITION FOR TESTING
*  INHERITING FROM cl_surdp_uph_proc_base_hu.
*
*  PUBLIC SECTION.
*
*    METHODS:
*      constructor,
*
*      get_hu_data RETURNING VALUE(rt_hu) TYPE if_surdp_uph_entity_hu_proc=>gty_t_hu_for_product,
*
*      if_surdp_uph_entity_hu_proc~map_hu_data REDEFINITION,
*
*      proxy_determine_base_unit
*        IMPORTING iv_product       TYPE matnr
*        RETURNING VALUE(rv_result) TYPE meins,
*
*      proxy_convert_to_base_unit
*        IMPORTING iv_product       TYPE matnr
*                  iv_quantity      TYPE any
*                  iv_unit          TYPE meins
*                  iv_plant         TYPE werks_d OPTIONAL
*        RETURNING VALUE(rv_result) TYPE quan_15.
*
*
*ENDCLASS.
*
*CLASS ltc_surdp_uph_proc_base_hu_ip IMPLEMENTATION.
*  METHOD constructor.
*    super->constructor( ).
*  ENDMETHOD.
*
*  METHOD get_hu_data.
*    rt_hu = mt_calculated_hu.
*  ENDMETHOD.
*
*  METHOD if_surdp_uph_entity_hu_proc~map_hu_data.
*    " Test mapping: For each item create a simple composition/item
*    " call parent to ensure coverage
*    super->if_surdp_uph_entity_hu_proc~map_hu_data( it_packcomp_data  = VALUE #( )
*                                                    it_handling_units = VALUE #( ) ).
*
*    DATA lt_entity_comp       TYPE surdpt_uph_entity_data.
*    DATA lt_entity_comp_items TYPE surdpt_uph_entity_data.
*    DATA lt_entity_products   TYPE surdpt_uph_entity_data.
*
*    LOOP AT it_packcomp_data REFERENCE INTO DATA(lr_packcomp_data).
*
*      LOOP AT lr_packcomp_data->packaging_compositions REFERENCE INTO DATA(lr_packaging_composition).
*
*        CLEAR: lt_entity_comp_items,
*               lt_entity_products.
*
*        APPEND INITIAL LINE TO lt_entity_products REFERENCE INTO DATA(lr_entity_product).
*        lr_entity_product->* = NEW cl_surdp_uph_ent_pckg_product(
*            is_data    = VALUE #(
*                productid                  = lr_packcomp_data->selection_criteria-product_id
*                valid_from                 = lr_packaging_composition->valid_from
*                valid_to                   = lr_packaging_composition->valid_to
*                business_process_direction = lr_packcomp_data->selection_criteria-business_process_direction
*                supplier                   = lr_packcomp_data->selection_criteria-supplier_id )
*            iv_deleted = abap_false ).
*
*        LOOP AT it_handling_units REFERENCE INTO DATA(lr_handling_unit) WHERE product = lr_packcomp_data->selection_criteria-product_id.
*
*          APPEND INITIAL LINE TO lt_entity_comp_items REFERENCE INTO DATA(lr_comp_item).
*          lr_comp_item->* = NEW cl_surdp_uph_ent_pckg_cmp_item(
*                                    is_data    = VALUE #( packagingelementdisplayid = lr_handling_unit->handling_unit
*                                                          levelcode                 = '30'
*                                                          quantity                  = lr_handling_unit->amount
*                                                          quantityunitofmeasureid   = lr_handling_unit->base_unit
*                                                          count                     = '1'
*                                                          usage                     = 'H'
*                                                          packagingelementversion   = '1'
*                                                          wwfgroup                  = 'W1'
*                                                          eprgroup                  = 'E1' )
*                                    iv_deleted = abap_false ).
*        ENDLOOP.
*
*      ENDLOOP.
*
*      IF lt_entity_comp_items IS NOT INITIAL.
*        APPEND INITIAL LINE TO lt_entity_comp REFERENCE INTO DATA(lr_entity_comp).
*        lr_entity_comp->* = NEW cl_surdp_uph_ent_pckg_cmp_hdr(
*                                    is_cmp_hdr_data  = VALUE #( displayid = lr_packaging_composition->id )
*                                    it_cmp_item_data = lt_entity_comp_items
*                                    it_products      = lt_entity_products
*                                    iv_deleted       = abap_false ).
*      ENDIF.
*
*    ENDLOOP.
*    rt_entity_data = lt_entity_comp.
*  ENDMETHOD.
*
*  METHOD proxy_determine_base_unit.
*
*    rv_result = determine_base_unit(
*      EXPORTING
*        iv_product = iv_product
*    ).
*
*  ENDMETHOD.
*
*  METHOD proxy_convert_to_base_unit.
*
*    rv_result = convert_to_base_unit(
*      EXPORTING
*        iv_product  = iv_product
*        iv_quantity =  iv_quantity
*        iv_unit     = iv_unit
*        iv_plant    = iv_plant
*    ).
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*
*CLASS ltc_surdp_uph_proc_base_hu DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    CLASS-DATA mo_osql_env TYPE REF TO if_osql_test_environment.
*
*    DATA mo_cut            TYPE REF TO ltc_surdp_uph_proc_base_hu_ip.
*    DATA mo_factory_double TYPE REF TO if_surdp_uph_factory.
*
*    CLASS-METHODS class_setup.
*    CLASS-METHODS class_teardown.
*
*    METHODS setup.
*    METHODS teardown.
*
*    " test methods for init_processor
*    METHODS init_processor_w_params       FOR TESTING.
*    METHODS init_processor_w_empty_params FOR TESTING.
*
*    " test methods for prepare_process
*    METHODS prepare_process_params        FOR TESTING.
*    METHODS prepare_process_empty_params  FOR TESTING.
*    METHODS prepare_process_empty         FOR TESTING.
*
*    " test methods for process_package
*    METHODS process_package_prepared      FOR TESTING.
*    METHODS process_package_not_prepared  FOR TESTING.
*    METHODS process_package_empty_result  FOR TESTING.
*
*    " other test methods
*    METHODS convert_to_base_unit          FOR TESTING.
*    METHODS determine_base_unit           FOR TESTING.
*
*ENDCLASS.
*
*CLASS ltc_surdp_uph_proc_base_hu IMPLEMENTATION.
*  METHOD class_setup.
*    mo_osql_env = cl_osql_test_environment=>create(
*        i_dependency_list = VALUE #( ( 'LIKP' ) ( 'LIPS' ) ( 'KNA1' ) ( 'VBPA' ) ( 'ADRC' ) ( 'T001W' ) ( 'VEKP' ) ( 'VEPO' ) ( 'MARA' ) ) ).
*  ENDMETHOD.
*
*  METHOD class_teardown.
*    mo_osql_env->destroy( ).
*  ENDMETHOD.
*
*  METHOD setup.
*    DATA lt_mara TYPE STANDARD TABLE OF mara.
*
*    mo_factory_double = NEW td_surdp_uph_factory( ).
*    th_surdp_uph_factory_injector=>inject_factory_double( io_double = mo_factory_double ).
*    mo_cut = NEW #(  ).
*
*    " product data
*    lt_mara = VALUE #(
*                       ( matnr = 'RDP-HU-BOX' meins = 'ST' )
*                       ( matnr = 'RDP-HU-STR' meins = 'M' )
*                       ( matnr = 'RDP-HU-BUB' meins = 'M' )
*                       ( matnr = 'RDP-BM-PUMP-M-CPL' meins = 'ST' )
*                     ).
*    mo_osql_env->get_double( 'mara' )->insert( cl_osql_test_data=>create( lt_mara ) ).
*
*  ENDMETHOD.
*
*  METHOD teardown.
*    mo_osql_env->clear_doubles( ).
*  ENDMETHOD.
*
*  METHOD prepare_process_params.
*    DATA lt_likp        TYPE STANDARD TABLE OF likp.
*    DATA lt_lips        TYPE STANDARD TABLE OF lips.
*    DATA lt_kna1        TYPE STANDARD TABLE OF kna1.
*    DATA lt_vbpa        TYPE STANDARD TABLE OF vbpa.
*    DATA lt_adrc        TYPE STANDARD TABLE OF adrc.
*    DATA lt_t001w       TYPE STANDARD TABLE OF t001w.
*    DATA lt_vekp        TYPE STANDARD TABLE OF vekp.
*    DATA lt_vepo        TYPE STANDARD TABLE OF vepo.
*    DATA lt_hu_data     TYPE if_surdp_uph_entity_hu_proc=>gty_t_hu_for_product.
*    DATA ls_act_hu_data TYPE if_surdp_uph_entity_hu_proc=>gty_s_hu_for_product.
*    DATA ls_exp_hu_data TYPE if_surdp_uph_entity_hu_proc=>gty_s_hu_for_product.
*    DATA ls_parameters  TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*
*    " setup parameters
*    ls_parameters-act_goods_mvt_date = VALUE #( ( sign = 'I' option = 'BT' low = '20240821' high = '20241121' ) ).
*
*    " setup delivery header data
*    lt_likp = VALUE #( ( vbeln     = '0080003328'
*                         lfart     = 'LO'
*                         erdat     = '20241015'
*                         aedat     = '20241015'
*                         inco1     = 'CFR'
*                         kunag     = ''
*                         vkorg     = '0001'
*                         kunnr     = 'TST_CUST1'
*                         wadat_ist = '20240921'
*                         wbstk     = 'C'
*                         vbtyp     = 'J'
*                         spe_loekz = abap_false ) ).
*    mo_osql_env->get_double( 'likp' )->insert( cl_osql_test_data=>create( lt_likp ) ).
*
*    lt_kna1 = VALUE #( ( kunnr = 'TST_CUST1' ) ).
*    mo_osql_env->get_double( 'kna1' )->insert( cl_osql_test_data=>create( lt_kna1 ) ).
*
*    lt_vbpa = VALUE #( ( vbeln = '0080003328' adrnr = '0000006935' parvw = 'WE' ) ).
*    mo_osql_env->get_double( 'vbpa' )->insert( cl_osql_test_data=>create( lt_vbpa ) ).
*
*    lt_adrc = VALUE #( ( addrnumber = '0000006935' date_from = '00010101' nation = '' ) ).
*    mo_osql_env->get_double( 'adrc' )->insert( cl_osql_test_data=>create( lt_adrc ) ).
*
*    " setup delivery item data
*    lt_lips = VALUE #(
*        vbeln = '0080003328'
*        werks = '0001'
*        vtweg = '01'
*        spart = '01'
*        lgort = '0001'
*        ( mtart = 'VERP' matkl = '01' posnr = '900002' matnr = 'RDP-HU-BOX' lgmng = '2' meins = 'ST' lfimg = '2' pstyv = 'HUPM' vrkme = 'ST' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900003' matnr = 'RDP-HU-STR' lgmng = '6' meins = 'M' lfimg = '6' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900004' matnr = 'RDP-HU-BUB' lgmng = '4' meins = 'M' lfimg = '4' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'FERT' matkl = ''   posnr = '000010' matnr = 'RDP-BM-PUMP-M-CPL' lgmng = '30' meins = 'ST' lfimg = '30' pstyv = 'DLN' vrkme = 'ST' ) ).
*    mo_osql_env->get_double( 'lips' )->insert( cl_osql_test_data=>create( lt_lips ) ).
*
*    lt_t001w = VALUE #( ( werks = '0001' ) ).
*    mo_osql_env->get_double( 't001w' )->insert( cl_osql_test_data=>create( lt_t001w ) ).
*
*    " setup handling units header data
*    lt_vekp = VALUE #( ( vbeln_gen = '0080003328' vpobjkey = '0080003328' venum = '0000001410' vhilm = 'RDP-HU-BOX' meins = 'ST' ) ).
*    mo_osql_env->get_double( 'vekp' )->insert( cl_osql_test_data=>create( lt_vekp ) ).
*
*    " setup handling units item data
*    lt_vepo = VALUE #(
*        venum = '0000001410'
*        ( vepos = '000001' vemng = '15' pstyv = '' matnr = 'RDP-BM-PUMP-M-CPL' veanz = '0' vemeh = 'ST'  )
*        ( vepos = '000003' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-STR' veanz = '3' vemeh = 'M'  )
*        ( vepos = '000004' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-BUB' veanz = '2' vemeh = 'M'  ) ).
*    mo_osql_env->get_double( 'vepo' )->insert( cl_osql_test_data=>create( lt_vepo ) ).
*
*    " when
*    mo_cut->if_surdp_uph_entity_proc~init_processor(
*        is_parameters    = ls_parameters
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu
*        iv_upload_mode   = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full ).
*    mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*
*    " then
*    lt_hu_data = mo_cut->get_hu_data( ).
*    cl_abap_unit_assert=>assert_equals( exp = 3
*                                        act = lines( lt_hu_data ) ).
*
*    ls_act_hu_data = lt_hu_data[ handling_unit = 'RDP-HU-BOX' ].
*    ls_exp_hu_data-product       = 'RDP-BM-PUMP-M-CPL'.
*    ls_exp_hu_data-handling_unit = 'RDP-HU-BOX'.
*    ls_exp_hu_data-amount        = '0.066667'.
*    ls_exp_hu_data-base_unit     = 'ST'.
*    ls_exp_hu_data-prd_amount    = '15'.
*
*    cl_abap_unit_assert=>assert_equals( exp = ls_exp_hu_data
*                                        act = ls_act_hu_data ).
*
*    ls_act_hu_data = lt_hu_data[ handling_unit = 'RDP-HU-STR' ].
*    ls_exp_hu_data-product       = 'RDP-BM-PUMP-M-CPL'.
*    ls_exp_hu_data-handling_unit = 'RDP-HU-STR'.
*    ls_exp_hu_data-amount        = '0.200000'.
*    ls_exp_hu_data-base_unit     = 'M'.
*    ls_exp_hu_data-prd_amount    = '15'.
*
*    cl_abap_unit_assert=>assert_equals( exp = ls_exp_hu_data
*                                        act = ls_act_hu_data ).
*
*    ls_act_hu_data = lt_hu_data[ handling_unit = 'RDP-HU-BUB' ].
*    ls_exp_hu_data-product       = 'RDP-BM-PUMP-M-CPL'.
*    ls_exp_hu_data-handling_unit = 'RDP-HU-BUB'.
*    ls_exp_hu_data-amount        = '0.133333'.
*    ls_exp_hu_data-base_unit     = 'M'.
*    ls_exp_hu_data-prd_amount    = '15'.
*
*    cl_abap_unit_assert=>assert_equals( exp = ls_exp_hu_data
*                                        act = ls_act_hu_data ).
*  ENDMETHOD.
*
*  METHOD prepare_process_empty_params.
*    DATA lt_likp        TYPE STANDARD TABLE OF likp.
*    DATA lt_lips        TYPE STANDARD TABLE OF lips.
*    DATA lt_kna1        TYPE STANDARD TABLE OF kna1.
*    DATA lt_vbpa        TYPE STANDARD TABLE OF vbpa.
*    DATA lt_adrc        TYPE STANDARD TABLE OF adrc.
*    DATA lt_t001w       TYPE STANDARD TABLE OF t001w.
*    DATA lt_vekp        TYPE STANDARD TABLE OF vekp.
*    DATA lt_vepo        TYPE STANDARD TABLE OF vepo.
*    DATA lt_hu_data     TYPE if_surdp_uph_entity_hu_proc=>gty_t_hu_for_product.
*    DATA ls_act_hu_data TYPE if_surdp_uph_entity_hu_proc=>gty_s_hu_for_product.
*    DATA ls_exp_hu_data TYPE if_surdp_uph_entity_hu_proc=>gty_s_hu_for_product.
*    DATA ls_parameters  TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*
*    "setup params
*
*    " setup delivery header data
*    lt_likp = VALUE #( ( vbeln     = '0080003328'
*                         lfart     = 'LO'
*                         erdat     = '20241015'
*                         aedat     = '20241015'
*                         inco1     = 'CFR'
*                         kunag     = ''
*                         vkorg     = '0001'
*                         kunnr     = 'TST_CUST1'
*                         wadat_ist = '00000000'
*                         wbstk     = 'C'
*                         vbtyp     = 'J'
*                         spe_loekz = abap_false ) ).
*    mo_osql_env->get_double( 'likp' )->insert( cl_osql_test_data=>create( lt_likp ) ).
*
*    lt_kna1 = VALUE #( ( kunnr = 'TST_CUST1' ) ).
*    mo_osql_env->get_double( 'kna1' )->insert( cl_osql_test_data=>create( lt_kna1 ) ).
*
*    lt_vbpa = VALUE #( ( vbeln = '0080003328' adrnr = '0000006935' parvw = 'WE' ) ).
*    mo_osql_env->get_double( 'vbpa' )->insert( cl_osql_test_data=>create( lt_vbpa ) ).
*
*    lt_adrc = VALUE #( ( addrnumber = '0000006935' date_from = '00010101' nation = '' ) ).
*    mo_osql_env->get_double( 'adrc' )->insert( cl_osql_test_data=>create( lt_adrc ) ).
*
*    " setup delivery item data
*    lt_lips = VALUE #(
*        vbeln = '0080003328'
*        werks = '0001'
*        vtweg = '01'
*        spart = '01'
*        lgort = '0001'
*        ( mtart = 'VERP' matkl = '01' posnr = '900002' matnr = 'RDP-HU-BOX' lgmng = '2' meins = 'ST' lfimg = '2' pstyv = 'HUPM' vrkme = 'ST' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900003' matnr = 'RDP-HU-STR' lgmng = '6' meins = 'M' lfimg = '6' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900004' matnr = 'RDP-HU-BUB' lgmng = '4' meins = 'M' lfimg = '4' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'FERT' matkl = ''   posnr = '000010' matnr = 'RDP-BM-PUMP-M-CPL' lgmng = '30' meins = 'ST' lfimg = '30' pstyv = 'DLN' vrkme = 'ST' ) ).
*    mo_osql_env->get_double( 'lips' )->insert( cl_osql_test_data=>create( lt_lips ) ).
*
*    lt_t001w = VALUE #( ( werks = '0001' ) ).
*    mo_osql_env->get_double( 't001w' )->insert( cl_osql_test_data=>create( lt_t001w ) ).
*
*    " setup handling units header data
*    lt_vekp = VALUE #( ( vbeln_gen = '0080003328' vpobjkey = '0080003328' venum = '0000001410' vhilm = 'RDP-HU-BOX' meins = 'ST' ) ).
*    mo_osql_env->get_double( 'vekp' )->insert( cl_osql_test_data=>create( lt_vekp ) ).
*
*    " setup handling units item data
*    lt_vepo = VALUE #(
*        venum = '0000001410'
*        ( vepos = '000001' vemng = '15' pstyv = '' matnr = 'RDP-BM-PUMP-M-CPL' veanz = '0' vemeh = 'ST' )
*        ( vepos = '000003' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-STR' veanz = '3' vemeh = 'M'  )
*        ( vepos = '000004' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-BUB' veanz = '2' vemeh = 'M'  ) ).
*    mo_osql_env->get_double( 'vepo' )->insert( cl_osql_test_data=>create( lt_vepo ) ).
*
*    " when
*    mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*
*    " then
*    lt_hu_data = mo_cut->get_hu_data( ).
*    cl_abap_unit_assert=>assert_equals( exp = 3
*                                        act = lines( lt_hu_data ) ).
*
*    ls_act_hu_data = lt_hu_data[ handling_unit = 'RDP-HU-BOX' ].
*    ls_exp_hu_data-product       = 'RDP-BM-PUMP-M-CPL'.
*    ls_exp_hu_data-handling_unit = 'RDP-HU-BOX'.
*    ls_exp_hu_data-amount        = '0.066667'.
*    ls_exp_hu_data-base_unit     = 'ST'.
*    ls_exp_hu_data-prd_amount    = '15'.
*
*    cl_abap_unit_assert=>assert_equals( exp = ls_exp_hu_data
*                                        act = ls_act_hu_data ).
*
*    ls_act_hu_data = lt_hu_data[ handling_unit = 'RDP-HU-STR' ].
*    ls_exp_hu_data-product       = 'RDP-BM-PUMP-M-CPL'.
*    ls_exp_hu_data-handling_unit = 'RDP-HU-STR'.
*    ls_exp_hu_data-amount        = '0.200000'.
*    ls_exp_hu_data-base_unit     = 'M'.
*    ls_exp_hu_data-prd_amount    = '15'.
*
*    cl_abap_unit_assert=>assert_equals( exp = ls_exp_hu_data
*                                        act = ls_act_hu_data ).
*
*    ls_act_hu_data = lt_hu_data[ handling_unit = 'RDP-HU-BUB' ].
*    ls_exp_hu_data-product       = 'RDP-BM-PUMP-M-CPL'.
*    ls_exp_hu_data-handling_unit = 'RDP-HU-BUB'.
*    ls_exp_hu_data-amount        = '0.133333'.
*    ls_exp_hu_data-base_unit     = 'M'.
*    ls_exp_hu_data-prd_amount    = '15'.
*
*    cl_abap_unit_assert=>assert_equals( exp = ls_exp_hu_data
*                                        act = ls_act_hu_data ).
*  ENDMETHOD.
*
*  METHOD prepare_process_empty.
*    DATA lt_likp    TYPE STANDARD TABLE OF likp.
*    DATA lt_lips    TYPE STANDARD TABLE OF lips.
*    DATA lt_kna1    TYPE STANDARD TABLE OF kna1.
*    DATA lt_vbpa    TYPE STANDARD TABLE OF vbpa.
*    DATA lt_adrc    TYPE STANDARD TABLE OF adrc.
*    DATA lt_t001w   TYPE STANDARD TABLE OF t001w.
*    DATA lt_vekp    TYPE STANDARD TABLE OF vekp.
*    DATA lt_vepo    TYPE STANDARD TABLE OF vepo.
*    DATA lt_hu_data TYPE if_surdp_uph_entity_hu_proc=>gty_t_hu_for_product.
*
*    " setup empty data set
*    mo_osql_env->get_double( 'likp' )->insert( cl_osql_test_data=>create( lt_likp ) ).
*    mo_osql_env->get_double( 'lips' )->insert( cl_osql_test_data=>create( lt_lips ) ).
*    mo_osql_env->get_double( 'kna1' )->insert( cl_osql_test_data=>create( lt_kna1 ) ).
*    mo_osql_env->get_double( 'vbpa' )->insert( cl_osql_test_data=>create( lt_vbpa ) ).
*    mo_osql_env->get_double( 'adrc' )->insert( cl_osql_test_data=>create( lt_adrc ) ).
*    mo_osql_env->get_double( 't001w' )->insert( cl_osql_test_data=>create( lt_t001w ) ).
*    mo_osql_env->get_double( 'vekp' )->insert( cl_osql_test_data=>create( lt_vekp ) ).
*    mo_osql_env->get_double( 'vepo' )->insert( cl_osql_test_data=>create( lt_vepo ) ).
*
*    " when
*    mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*
*    " then
*    lt_hu_data = mo_cut->get_hu_data( ).
*
*    cl_abap_unit_assert=>assert_initial( lt_hu_data ).
*  ENDMETHOD.
*
*  METHOD init_processor_w_params.
*    DATA ls_parameters     TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*    DATA ls_act_parameters TYPE REF TO if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*    DATA ls_exp_parameters TYPE REF TO if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*
*    " given parameters
*    ls_parameters-material           = VALUE #( ( sign = 'I' option = 'EQ' high = 'RDP-BM-PC-CPLPACK' low = '' ) ).
*    ls_parameters-act_goods_mvt_date = VALUE #( ( sign = 'I' option = 'BT' low = '20240101' high = '20240401' ) ).
*
*    " when
*    mo_cut->if_surdp_uph_entity_proc~init_processor(
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu
*        iv_upload_mode   = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters    = ls_parameters ).
*    ls_act_parameters ?= mo_cut->if_surdp_uph_entity_proc~get_parameters( ).
*
*    " then
*    ls_exp_parameters = NEW #( material = ls_parameters-material act_goods_mvt_date = ls_parameters-act_goods_mvt_date ).
*
*    cl_abap_unit_assert=>assert_equals( exp = ls_exp_parameters->material
*                                        act = ls_act_parameters->material ).
*
*    cl_abap_unit_assert=>assert_equals( exp = ls_exp_parameters->act_goods_mvt_date
*                                        act = ls_act_parameters->act_goods_mvt_date ).
*  ENDMETHOD.
*
*  METHOD init_processor_w_empty_params.
*    DATA ls_parameters     TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*    DATA ls_act_parameters TYPE REF TO if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*
*    " given empty parameters
*
*    " when
*    mo_cut->if_surdp_uph_entity_proc~init_processor(
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu
*        iv_upload_mode   = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters    = ls_parameters ).
*    ls_act_parameters ?= mo_cut->if_surdp_uph_entity_proc~get_parameters( ).
*
*    " then
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->material  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->material_group  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->material_type  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->source_id  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->ship_to_party  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->sddoc  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->sales_org  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->rfc_des  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->plant_country  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->plant  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->document_type  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->division  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->distribution  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->category  ).
*    cl_abap_unit_assert=>assert_initial( act = ls_act_parameters->act_goods_mvt_date  ).
*
*  ENDMETHOD.
*
*  METHOD process_package_empty_result.
*    DATA lt_likp       TYPE STANDARD TABLE OF likp.
*    DATA lt_lips       TYPE STANDARD TABLE OF lips.
*    DATA lt_kna1       TYPE STANDARD TABLE OF kna1.
*    DATA lt_vbpa       TYPE STANDARD TABLE OF vbpa.
*    DATA lt_adrc       TYPE STANDARD TABLE OF adrc.
*    DATA lt_t001w      TYPE STANDARD TABLE OF t001w.
*    DATA lt_vekp       TYPE STANDARD TABLE OF vekp.
*    DATA lt_vepo       TYPE STANDARD TABLE OF vepo.
*    DATA ls_parameters TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*
*    " setup parameters
*    ls_parameters-act_goods_mvt_date = VALUE #( ( sign = 'I' option = 'BT' low = '20240821' high = '20241121' ) ).
*    ls_parameters-material           = VALUE #( ( sign = 'I' option = 'EQ' low = 'RDP-BM-PUMP-M-CPL' ) ).
*
*    " setup delivery header data
*    lt_likp = VALUE #( ( vbeln     = '0080003328'
*                         lfart     = 'LO'
*                         erdat     = '20241015'
*                         aedat     = '20241015'
*                         inco1     = 'CFR'
*                         kunag     = ''
*                         vkorg     = '0001'
*                         kunnr     = 'TST_CUST1'
*                         wadat_ist = '20240921'
*                         wbstk     = 'C'
*                         vbtyp     = 'J'
*                         spe_loekz = abap_false ) ).
*    mo_osql_env->get_double( 'likp' )->insert( cl_osql_test_data=>create( lt_likp ) ).
*
*    lt_kna1 = VALUE #( ( kunnr = 'TST_CUST1' ) ).
*    mo_osql_env->get_double( 'kna1' )->insert( cl_osql_test_data=>create( lt_kna1 ) ).
*
*    lt_vbpa = VALUE #( ( vbeln = '0080003328' adrnr = '0000006935' parvw = 'WE' ) ).
*    mo_osql_env->get_double( 'vbpa' )->insert( cl_osql_test_data=>create( lt_vbpa ) ).
*
*    lt_adrc = VALUE #( ( addrnumber = '0000006935' date_from = '00010101' nation = '' ) ).
*    mo_osql_env->get_double( 'adrc' )->insert( cl_osql_test_data=>create( lt_adrc ) ).
*
*    " setup delivery item data
*    lt_lips = VALUE #(
*        vbeln = '0080003328'
*        werks = '0001'
*        vtweg = '01'
*        spart = '01'
*        lgort = '0001'
*        ( mtart = 'VERP' matkl = '01' posnr = '900002' matnr = 'RDP-HU-BOX' lgmng = '2' meins = 'ST' lfimg = '2' pstyv = 'HUPM' vrkme = 'ST' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900003' matnr = 'RDP-HU-STR' lgmng = '6' meins = 'M' lfimg = '6' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900004' matnr = 'RDP-HU-BUB' lgmng = '4' meins = 'M' lfimg = '4' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'FERT' matkl = ''   posnr = '000010' matnr = 'RDP-BM-PUMP-M-CPL' lgmng = '30' meins = 'ST' lfimg = '30' pstyv = 'DLN' vrkme = 'ST' ) ).
*    mo_osql_env->get_double( 'lips' )->insert( cl_osql_test_data=>create( lt_lips ) ).
*
*    lt_t001w = VALUE #( ( werks = '0001' ) ).
*    mo_osql_env->get_double( 't001w' )->insert( cl_osql_test_data=>create( lt_t001w ) ).
*
*    " setup handling units header data
*    lt_vekp = VALUE #( ( vbeln_gen = '0080003328' vpobjkey = '0080003328' venum = '0000001410' vhilm = 'RDP-HU-BOX' meins = 'ST' ) ).
*    mo_osql_env->get_double( 'vekp' )->insert( cl_osql_test_data=>create( lt_vekp ) ).
*
*    " setup handling units item data
*    lt_vepo = VALUE #(
*        venum = '0000001410'
*        ( vepos = '000001' vemng = '15' pstyv = '' matnr = 'RDP-BM-PUMP-M-CPL' veanz = '0' vemeh = 'ST'  )
*        ( vepos = '000003' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-STR' veanz = '3' vemeh = 'M'  )
*        ( vepos = '000004' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-BUB' veanz = '2' vemeh = 'M'  ) ).
*    mo_osql_env->get_double( 'vepo' )->insert( cl_osql_test_data=>create( lt_vepo ) ).
*
*    " calls to mock of http util
*    DATA(lo_mock_http_util) = mo_factory_double->get_http_util( ).
*
*    DATA lo_http_client TYPE REF TO if_http_client.
*    lo_http_client ?= cl_abap_testdouble=>create( 'if_http_client' ).
*
*    cl_abap_testdouble=>configure_call( lo_mock_http_util )->returning( lo_http_client )->and_expect( )->is_called_once( ).
*    lo_mock_http_util->get_http_client( iv_rfc_des        = ls_parameters-rfc_des
*                                        iv_request_method = 'POST'
*                                        iv_uri_suffix     = '/GetPackagingCompositionsForProduct' ).
*
*    DATA lv_entity_data TYPE string
*         VALUE '{"source":"S4H","selectionPeriod":{"from":"2024-08-21","to":"2024-11-21"},"productAssignments":[{"product":"RDP-BM-PUMP-M-CPL","businessProcessDirection":"ALL","supplierId": null}]}'.
*    DATA lv_response    TYPE string.
*
*    lv_response = ''.
*
*    cl_abap_testdouble=>configure_call( lo_mock_http_util )->ignore_parameter( name = 'iv_entity_data' )->set_parameter(
*        name  = 'ev_response_txt'
*        value = lv_response )->set_parameter( name  = 'ev_code'
*                                              value = '200' )->and_expect( )->is_called_once( ).
*    lo_mock_http_util->post_data_to_api( iv_entity_data = lv_entity_data ).
*
*    " when
*    mo_cut->if_surdp_uph_entity_proc~init_processor(
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu
*        iv_upload_mode   = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters    = ls_parameters ).
*    mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    DATA(lt_entity_data) = mo_cut->if_surdp_uph_entity_proc~process_package( iv_act_package  = 1
*                                                                             iv_package_size = 50 ).
*
*    " then
*    cl_abap_testdouble=>verify_expectations( lo_mock_http_util ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 0
*                                        act = lines( lt_entity_data ) ).
*  ENDMETHOD.
*
*  METHOD process_package_not_prepared.
*    DATA lt_likp               TYPE STANDARD TABLE OF likp.
*    DATA lt_lips               TYPE STANDARD TABLE OF lips.
*    DATA lt_kna1               TYPE STANDARD TABLE OF kna1.
*    DATA lt_vbpa               TYPE STANDARD TABLE OF vbpa.
*    DATA lt_adrc               TYPE STANDARD TABLE OF adrc.
*    DATA lt_t001w              TYPE STANDARD TABLE OF t001w.
*    DATA lt_vekp               TYPE STANDARD TABLE OF vekp.
*    DATA lt_vepo               TYPE STANDARD TABLE OF vepo.
*    DATA ls_parameters         TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*    DATA lo_actual_entity_data TYPE REF TO cl_surdp_uph_ent_pckg_cmp_hdr.
*
*    " setup parameters
*    ls_parameters-act_goods_mvt_date = VALUE #( ( sign = 'I' option = 'BT' low = '20240821' high = '20241121' ) ).
*    ls_parameters-material           = VALUE #( ( sign = 'I' option = 'EQ' low = 'RDP-BM-PUMP-M-CPL' ) ).
*
*    " setup delivery header data
*    lt_likp = VALUE #( ( vbeln     = '0080003328'
*                         lfart     = 'LO'
*                         erdat     = '20241015'
*                         aedat     = '20241015'
*                         inco1     = 'CFR'
*                         kunag     = ''
*                         vkorg     = '0001'
*                         kunnr     = 'TST_CUST1'
*                         wadat_ist = '20240921'
*                         wbstk     = 'C'
*                         vbtyp     = 'J'
*                         spe_loekz = abap_false ) ).
*    mo_osql_env->get_double( 'likp' )->insert( cl_osql_test_data=>create( lt_likp ) ).
*
*    lt_kna1 = VALUE #( ( kunnr = 'TST_CUST1' ) ).
*    mo_osql_env->get_double( 'kna1' )->insert( cl_osql_test_data=>create( lt_kna1 ) ).
*
*    lt_vbpa = VALUE #( ( vbeln = '0080003328' adrnr = '0000006935' parvw = 'WE' ) ).
*    mo_osql_env->get_double( 'vbpa' )->insert( cl_osql_test_data=>create( lt_vbpa ) ).
*
*    lt_adrc = VALUE #( ( addrnumber = '0000006935' date_from = '00010101' nation = '' ) ).
*    mo_osql_env->get_double( 'adrc' )->insert( cl_osql_test_data=>create( lt_adrc ) ).
*
*    " setup delivery item data
*    lt_lips = VALUE #(
*        vbeln = '0080003328'
*        werks = '0001'
*        vtweg = '01'
*        spart = '01'
*        lgort = '0001'
*        ( mtart = 'VERP' matkl = '01' posnr = '900002' matnr = 'RDP-HU-BOX' lgmng = '2' meins = 'ST' lfimg = '2' pstyv = 'HUPM' vrkme = 'ST' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900003' matnr = 'RDP-HU-STR' lgmng = '6' meins = 'M' lfimg = '6' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900004' matnr = 'RDP-HU-BUB' lgmng = '4' meins = 'M' lfimg = '4' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'FERT' matkl = ''   posnr = '000010' matnr = 'RDP-BM-PUMP-M-CPL' lgmng = '30' meins = 'ST' lfimg = '30' pstyv = 'DLN' vrkme = 'ST' ) ).
*    mo_osql_env->get_double( 'lips' )->insert( cl_osql_test_data=>create( lt_lips ) ).
*
*    lt_t001w = VALUE #( ( werks = '0001' ) ).
*    mo_osql_env->get_double( 't001w' )->insert( cl_osql_test_data=>create( lt_t001w ) ).
*
*    " setup handling units header data
*    lt_vekp = VALUE #( ( vbeln_gen = '0080003328' vpobjkey = '0080003328' venum = '0000001410' vhilm = 'RDP-HU-BOX' meins = 'ST' ) ).
*    mo_osql_env->get_double( 'vekp' )->insert( cl_osql_test_data=>create( lt_vekp ) ).
*
*    " setup handling units item data
*    lt_vepo = VALUE #(
*        venum = '0000001410'
*        ( vepos = '000001' vemng = '15' pstyv = '' matnr = 'RDP-BM-PUMP-M-CPL' veanz = '0' vemeh = 'ST'  )
*        ( vepos = '000003' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-STR' veanz = '3' vemeh = 'M'  )
*        ( vepos = '000004' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-BUB' veanz = '2' vemeh = 'M'  ) ).
*    mo_osql_env->get_double( 'vepo' )->insert( cl_osql_test_data=>create( lt_vepo ) ).
*
*    " calls to mock of http util
*    DATA(lo_mock_http_util) = mo_factory_double->get_http_util( ).
*
*    DATA lo_http_client TYPE REF TO if_http_client.
*    lo_http_client ?= cl_abap_testdouble=>create( 'if_http_client' ).
*
*    cl_abap_testdouble=>configure_call( lo_mock_http_util )->returning( lo_http_client )->and_expect( )->is_called_once( ).
*    lo_mock_http_util->get_http_client( iv_rfc_des        = ls_parameters-rfc_des
*                                        iv_request_method = 'POST'
*                                        iv_uri_suffix     = '/GetPackagingCompositionsForProduct' ).
*
*    DATA lv_entity_data TYPE string
*         VALUE '{"source":"S4H","selectionPeriod":{"from":"2024-08-21","to":"2024-11-21"},"productAssignments":[{"productId":"RDP-BM-PUMP-M-CPL","businessProcessDirection":"ALL","supplierId": null}]}'.
*    DATA lv_response    TYPE string.
*
*    lv_response = |\{"value":[\{"selectionCriteria":\{"supplier":null,"productId":"RDP-BM-PUMP-M-CPL","businessProcessDirection":"ALL","supplierId":null\},"packagingCompositions":[\{"id":"RDP-BM-PUMP-M-CPL-0001-1-01-20220101"| &&
*                                                        |,"validFrom":"2022-01-01","validTo":"9999-12-31","businessProcessDirection":"ALL","supplierId":null\}]\}]\}|.
*
*    cl_abap_testdouble=>configure_call( lo_mock_http_util )->ignore_parameter( name = 'iv_entity_data' )->set_parameter(
*        name  = 'ev_response_txt'
*        value = lv_response )->set_parameter( name  = 'ev_code'
*                                              value = '200' )->and_expect( )->is_called_once( ).
*    lo_mock_http_util->post_data_to_api( iv_entity_data = lv_entity_data ).
*
*    " when
*    mo_cut->if_surdp_uph_entity_proc~init_processor(
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu
*        iv_upload_mode   = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters    = ls_parameters ).
*    mo_cut->if_surdp_uph_entity_proc~prepare_process( ).
*    DATA(lt_entity_data) = mo_cut->if_surdp_uph_entity_proc~process_package( iv_act_package  = 1
*                                                                             iv_package_size = 50 ).
*
*    " then
*    cl_abap_testdouble=>verify_expectations( lo_mock_http_util ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1
*                                        act = lines( lt_entity_data ) ).
*
*    lo_actual_entity_data ?= lt_entity_data[ 1 ].
*
*    th_surdp_uph_packaging_comp=>then_assert_header_data(
*        io_actual_entiy_data = lo_actual_entity_data
*        iv_display_id        = 'RDP-BM-PUMP-M-CPL-0001-1-01-20220101' ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 3
*                                        act = lines( lo_actual_entity_data->get_items( ) ) ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = th_surdp_uph_packaging_comp=>determ_item_entity_data_row( it_entity_data = lo_actual_entity_data->get_items( ) iv_packelem_displayid = 'RDP-HU-BOX' )
*                                                        iv_packelem_displayid = 'RDP-HU-BOX'
*                                                        iv_packelem_version   = '1'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.066667'
*                                                        iv_quantity_uom       = 'ST'
*                                                        iv_count              = 1
*                                                        iv_wwfgroup           = 'W1'
*                                                        iv_eprgroup           = 'E1'
*                                                        iv_usage              = 'H' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = th_surdp_uph_packaging_comp=>determ_item_entity_data_row( it_entity_data = lo_actual_entity_data->get_items( ) iv_packelem_displayid = 'RDP-HU-STR' )
*                                                        iv_packelem_displayid = 'RDP-HU-STR'
*                                                        iv_packelem_version   = '1'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.200000'
*                                                        iv_quantity_uom       = 'M'
*                                                        iv_count              = 1
*                                                        iv_wwfgroup           = 'W1'
*                                                        iv_eprgroup           = 'E1'
*                                                        iv_usage              = 'H' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = th_surdp_uph_packaging_comp=>determ_item_entity_data_row( it_entity_data = lo_actual_entity_data->get_items( ) iv_packelem_displayid = 'RDP-HU-BUB' )
*                                                        iv_packelem_displayid = 'RDP-HU-BUB'
*                                                        iv_packelem_version   = '1'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.133333'
*                                                        iv_quantity_uom       = 'M'
*                                                        iv_count              = 1
*                                                        iv_wwfgroup           = 'W1'
*                                                        iv_eprgroup           = 'E1'
*                                                        iv_usage              = 'H' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_actual_entity_data
*                                                           iv_product_id        = 'RDP-BM-PUMP-M-CPL'
*                                                           iv_valid_from        = '20220101'
*                                                           iv_valid_to          = '99991231' ).
*  ENDMETHOD.
*
*  METHOD process_package_prepared.
*    DATA lt_likp               TYPE STANDARD TABLE OF likp.
*    DATA lt_lips               TYPE STANDARD TABLE OF lips.
*    DATA lt_kna1               TYPE STANDARD TABLE OF kna1.
*    DATA lt_vbpa               TYPE STANDARD TABLE OF vbpa.
*    DATA lt_adrc               TYPE STANDARD TABLE OF adrc.
*    DATA lt_t001w              TYPE STANDARD TABLE OF t001w.
*    DATA lt_vekp               TYPE STANDARD TABLE OF vekp.
*    DATA lt_vepo               TYPE STANDARD TABLE OF vepo.
*    DATA ls_parameters         TYPE if_surdp_uph_pckg_cmp_hu_load=>gty_s_hu_input.
*    DATA lo_actual_entity_data TYPE REF TO cl_surdp_uph_ent_pckg_cmp_hdr.
*
*    " setup parameters
*    ls_parameters-act_goods_mvt_date = VALUE #( ( sign = 'I' option = 'BT' low = '20240821' high = '20241121' ) ).
*    ls_parameters-material           = VALUE #( ( sign = 'I' option = 'EQ' low = 'RDP-BM-PUMP-M-CPL' ) ).
*
*    " setup delivery header data
*    lt_likp = VALUE #( ( vbeln     = '0080003328'
*                         lfart     = 'LO'
*                         erdat     = '20241015'
*                         aedat     = '20241015'
*                         inco1     = 'CFR'
*                         kunag     = ''
*                         vkorg     = '0001'
*                         kunnr     = 'TST_CUST1'
*                         wadat_ist = '20240921'
*                         wbstk     = 'C'
*                         vbtyp     = 'J'
*                         spe_loekz = abap_false ) ).
*    mo_osql_env->get_double( 'likp' )->insert( cl_osql_test_data=>create( lt_likp ) ).
*
*    lt_kna1 = VALUE #( ( kunnr = 'TST_CUST1' ) ).
*    mo_osql_env->get_double( 'kna1' )->insert( cl_osql_test_data=>create( lt_kna1 ) ).
*
*    lt_vbpa = VALUE #( ( vbeln = '0080003328' adrnr = '0000006935' parvw = 'WE' ) ).
*    mo_osql_env->get_double( 'vbpa' )->insert( cl_osql_test_data=>create( lt_vbpa ) ).
*
*    lt_adrc = VALUE #( ( addrnumber = '0000006935' date_from = '00010101' nation = '' ) ).
*    mo_osql_env->get_double( 'adrc' )->insert( cl_osql_test_data=>create( lt_adrc ) ).
*
*    " setup delivery item data
*    lt_lips = VALUE #(
*        vbeln = '0080003328'
*        werks = '0001'
*        vtweg = '01'
*        spart = '01'
*        lgort = '0001'
*        ( mtart = 'VERP' matkl = '01' posnr = '900002' matnr = 'RDP-HU-BOX' lgmng = '2' meins = 'ST' lfimg = '2' pstyv = 'HUPM' vrkme = 'ST' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900003' matnr = 'RDP-HU-STR' lgmng = '6' meins = 'M' lfimg = '6' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'VERP' matkl = '01' posnr = '900004' matnr = 'RDP-HU-BUB' lgmng = '4' meins = 'M' lfimg = '4' pstyv = 'HUPM' vrkme = 'M' )
*        ( mtart = 'FERT' matkl = ''   posnr = '000010' matnr = 'RDP-BM-PUMP-M-CPL' lgmng = '30' meins = 'ST' lfimg = '30' pstyv = 'DLN' vrkme = 'ST' ) ).
*    mo_osql_env->get_double( 'lips' )->insert( cl_osql_test_data=>create( lt_lips ) ).
*
*    lt_t001w = VALUE #( ( werks = '0001' ) ).
*    mo_osql_env->get_double( 't001w' )->insert( cl_osql_test_data=>create( lt_t001w ) ).
*
*    " setup handling units header data
*    lt_vekp = VALUE #( ( vbeln_gen = '0080003328' vpobjkey = '0080003328' venum = '0000001410' vhilm = 'RDP-HU-BOX' meins = 'ST' ) ).
*    mo_osql_env->get_double( 'vekp' )->insert( cl_osql_test_data=>create( lt_vekp ) ).
*
*    " setup handling units item data
*    lt_vepo = VALUE #(
*        venum = '0000001410'
*        ( vepos = '000001' vemng = '15' pstyv = '' matnr = 'RDP-BM-PUMP-M-CPL' veanz = '0' vemeh = 'ST'  )
*        ( vepos = '000003' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-STR' veanz = '3' vemeh = 'M'  )
*        ( vepos = '000004' vemng = '0' pstyv = 'HUPM' matnr = 'RDP-HU-BUB' veanz = '2' vemeh = 'M'  ) ).
*    mo_osql_env->get_double( 'vepo' )->insert( cl_osql_test_data=>create( lt_vepo ) ).
*
*    " calls to mock of http util
*    DATA(lo_mock_http_util) = mo_factory_double->get_http_util( ).
*
*    DATA lo_http_client TYPE REF TO if_http_client.
*    lo_http_client ?= cl_abap_testdouble=>create( 'if_http_client' ).
*
*    cl_abap_testdouble=>configure_call( lo_mock_http_util )->returning( lo_http_client )->and_expect( )->is_called_once( ).
*    lo_mock_http_util->get_http_client( iv_rfc_des        = ls_parameters-rfc_des
*                                        iv_request_method = 'POST'
*                                        iv_uri_suffix     = '/GetPackagingCompositionsForProduct' ).
*
*    DATA lv_entity_data TYPE string
*         VALUE '{"source":"S4H","selectionPeriod":{"from":"2024-08-21","to":"2024-11-21"},"productAssignments":[{"productId":"RDP-BM-PUMP-M-CPL","businessProcessDirection":"ALL","supplierId": null}]}'.
*    DATA lv_response    TYPE string.
*
*    lv_response = |\{"value":[\{"selectionCriteria":\{"supplier":null,"productId":"RDP-BM-PUMP-M-CPL","businessProcessDirection":"ALL","supplierId":null\},"packagingCompositions":[\{"id":"RDP-BM-PUMP-M-CPL-0001-1-01-20220101"| &&
*                                                        |,"validFrom":"2022-01-01","validTo":"9999-12-31","businessProcessDirection":"ALL","supplierId":null\}]\}]\}|.
*
*    cl_abap_testdouble=>configure_call( lo_mock_http_util )->ignore_parameter( name = 'iv_entity_data' )->set_parameter(
*        name  = 'ev_response_txt'
*        value = lv_response )->set_parameter( name  = 'ev_code'
*                                              value = '200' )->and_expect( )->is_called_once( ).
*    lo_mock_http_util->post_data_to_api( iv_entity_data = lv_entity_data ).
*
*    " when
*    mo_cut->if_surdp_uph_entity_proc~init_processor(
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu
*        iv_upload_mode   = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters    = ls_parameters ).
*    DATA(lt_entity_data) = mo_cut->if_surdp_uph_entity_proc~process_package( iv_act_package  = 1
*                                                                             iv_package_size = 50 ).
*
*    " then
*    cl_abap_testdouble=>verify_expectations( lo_mock_http_util ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1
*                                        act = lines( lt_entity_data ) ).
*
*    lo_actual_entity_data ?= lt_entity_data[ 1 ].
*
*    th_surdp_uph_packaging_comp=>then_assert_header_data(
*        io_actual_entiy_data = lo_actual_entity_data
*        iv_display_id        = 'RDP-BM-PUMP-M-CPL-0001-1-01-20220101' ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 3
*                                        act = lines( lo_actual_entity_data->get_items( ) ) ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = th_surdp_uph_packaging_comp=>determ_item_entity_data_row( it_entity_data = lo_actual_entity_data->get_items( ) iv_packelem_displayid = 'RDP-HU-BOX' )
*                                                        iv_packelem_displayid = 'RDP-HU-BOX'
*                                                        iv_packelem_version   = '1'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.066667'
*                                                        iv_quantity_uom       = 'ST'
*                                                        iv_count              = 1
*                                                        iv_wwfgroup           = 'W1'
*                                                        iv_eprgroup           = 'E1'
*                                                        iv_usage              = 'H' ).
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = th_surdp_uph_packaging_comp=>determ_item_entity_data_row( it_entity_data = lo_actual_entity_data->get_items( ) iv_packelem_displayid = 'RDP-HU-STR' )
*                                                        iv_packelem_displayid = 'RDP-HU-STR'
*                                                        iv_packelem_version   = '1'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.200000'
*                                                        iv_quantity_uom       = 'M'
*                                                        iv_count              = 1
*                                                        iv_wwfgroup           = 'W1'
*                                                        iv_eprgroup           = 'E1'
*                                                        iv_usage              = 'H' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_item_data( io_actual_entiy_data  = lo_actual_entity_data
*                                                        iv_item_no            = th_surdp_uph_packaging_comp=>determ_item_entity_data_row( it_entity_data = lo_actual_entity_data->get_items( ) iv_packelem_displayid = 'RDP-HU-BUB' )
*                                                        iv_packelem_displayid = 'RDP-HU-BUB'
*                                                        iv_packelem_version   = '1'
*                                                        iv_levelcode          = '30'
*                                                        iv_quantity           = '0.133333'
*                                                        iv_quantity_uom       = 'M'
*                                                        iv_count              = 1
*                                                        iv_wwfgroup           = 'W1'
*                                                        iv_eprgroup           = 'E1'
*                                                        iv_usage              = 'H' ).
*
*    th_surdp_uph_packaging_comp=>then_assert_product_data( io_actual_entiy_data = lo_actual_entity_data
*                                                           iv_product_id        = 'RDP-BM-PUMP-M-CPL'
*                                                           iv_valid_from        = '20220101'
*                                                           iv_valid_to          = '99991231' ).
*  ENDMETHOD.
*
*
*  METHOD convert_to_base_unit.
*
*    "given
*    DATA lt_mara TYPE STANDARD TABLE OF mara.
*
*    " setup delivery header data
*    lt_mara = VALUE #( ( matnr = 'RDP_1' meins = 'G' ) ).
*    mo_osql_env->get_double( 'mara' )->insert( cl_osql_test_data=>create( lt_mara ) ).
*
*    "when
*    mo_cut->proxy_convert_to_base_unit(
*      EXPORTING
*        iv_product  = 'RDP_1'
*        iv_quantity = 105
*        iv_unit     = 'KG'
*      RECEIVING
*        rv_result   = DATA(lv_result)
*    ).
*
*    "then
*    cl_abap_unit_assert=>assert_equals( exp = 105000 act = lv_result ).
*
*  ENDMETHOD.
*
*  METHOD determine_base_unit.
*
*    "given
*    DATA lt_mara TYPE STANDARD TABLE OF mara.
*
*    " setup delivery header data
*    lt_mara = VALUE #( ( matnr = 'RDP_1' meins = 'ST' ) ).
*    mo_osql_env->get_double( 'mara' )->insert( cl_osql_test_data=>create( lt_mara ) ).
*
*    "when
*    mo_cut->proxy_determine_base_unit(
*      EXPORTING
*        iv_product  = 'RDP_1'
*      RECEIVING
*        rv_result   = DATA(lv_result)
*    ).
*
*    "then
*    cl_abap_unit_assert=>assert_equals( exp = 'ST' act = lv_result ).
*
*  ENDMETHOD.
*
*ENDCLASS.
