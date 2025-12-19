*CLASS tcl_pckf_proc_pckg_fee DEFINITION FOR TESTING
*  INHERITING FROM /vpcoe/cl_pckf_proc_pckg_fee.
*
*  PUBLIC SECTION.
*
*    DATA:
*      mt_entity_data     TYPE /vpcoe/t_pckf_entity_data,
*      mv_retrieve_called TYPE i.
*
*    METHODS:
*      constructor
*        IMPORTING
*          !io_rdp_http TYPE REF TO /vpcoe/cl_pckgfee_http OPTIONAL,
*
*      /vpcoe/if_pckf_entity_proc~retrieve_package REDEFINITION,
*
*      set_entity_data
*        IMPORTING
*          !it_entity_data TYPE /vpcoe/t_pckf_entity_data,
*
*      get_retrieve_called
*        RETURNING VALUE(rv_result) TYPE i.
*
*ENDCLASS.
*
*CLASS tcl_pckf_proc_pckg_fee IMPLEMENTATION.
*
*  METHOD constructor.
*    super->constructor( ).
*
*    "inject http communication util
*    IF io_rdp_http IS BOUND.
*      mo_rdp_http = io_rdp_http.
*    ENDIF.
*
*  ENDMETHOD.
*
*  METHOD set_entity_data.
*    mt_entity_data = it_entity_data.
*  ENDMETHOD.
*
*
*  METHOD /vpcoe/if_pckf_entity_proc~retrieve_package.
** Test retrieve
*    ADD 1 TO mv_retrieve_called.
*
*    DATA(lv_total_cnt) = lines( mt_entity_data ).
*
*    DATA(lv_start_idx) = ( iv_act_package - 1 ) * iv_package_size + 1.
*
*    IF iv_act_package * iv_package_size < lv_total_cnt.
*
*      TRY.
*          DATA(lv_next_link) = cl_system_uuid=>create_uuid_c32_static( ).
*        CATCH cx_uuid_error INTO DATA(lr_exc) .
*      ENDTRY.
*
*      /vpcoe/if_pckf_entity_proc~set_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-next_link iv_value = lv_next_link ).
*    ELSE.
*      /vpcoe/if_pckf_entity_proc~set_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-next_link iv_value = '' ).
*    ENDIF.
*
*    DATA(lv_idx) = lv_start_idx.
*    DATA(lv_cnt) = 1.
*    WHILE lv_idx <= lv_total_cnt AND lv_cnt <= iv_package_size.
*
*      APPEND INITIAL LINE TO rt_entity_data REFERENCE INTO DATA(lr_entity_data).
*      lr_entity_data->* = mt_entity_data[ lv_idx ].
*
*      ADD 1 TO lv_idx.
*      ADD 1 TO lv_cnt.
*
*    ENDWHILE.
*
*  ENDMETHOD.
*
*  METHOD get_retrieve_called.
*    rv_result = mv_retrieve_called.
*  ENDMETHOD.
*
*ENDCLASS.
*
*
*CLASS tcl_pckf_exec_retrieval DEFINITION DEFERRED.
*CLASS /vpcoe/cl_pckf_exec_retrieval DEFINITION LOCAL FRIENDS tcl_pckf_exec_retrieval.
*
*CLASS tcl_pckf_exec_retrieval DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    DATA:
*      mo_comm_util TYPE REF TO /vpcoe/td_http_communic,
*      mo_cut       TYPE REF TO /vpcoe/cl_pckf_exec_retrieval.  "class under test
*
*    METHODS: setup.
*    METHODS: teardown.
*
*    METHODS:
*      exec_retrieve_sc0 FOR TESTING,
*      exec_retrieve_sc1 FOR TESTING.
*
*ENDCLASS.
*
*CLASS tcl_pckf_exec_retrieval IMPLEMENTATION.
*
*  METHOD setup.
*
*    DATA(lo_factory_double) = NEW /vpcoe/td_pckf_factory(  ).
*
*    "Inject communication util double
*    IF mo_comm_util IS NOT BOUND.
*      TRY.
*          mo_comm_util = NEW /vpcoe/td_http_communic( ).
*        CATCH cx_oa2c INTO DATA(lx_ecx).
*      ENDTRY.
*    ENDIF.
*
*    DATA(lo_ent_proc_double) = NEW tcl_pckf_proc_pckg_fee( io_rdp_http = mo_comm_util ).
*    lo_factory_double->set_entity_processor( io_double = lo_ent_proc_double ).
*
*    /vpcoe/th_pckf_factory_inject=>inject_factory_double( io_double = lo_factory_double ).
*
*    mo_cut = NEW /vpcoe/cl_pckf_exec_retrieval( ).
*
*  ENDMETHOD.
*
*  METHOD teardown.
*    ROLLBACK WORK.
*  ENDMETHOD.
*
*
*
*  METHOD exec_retrieve_sc0.
** Scenario 0: Small amount of packaging fees and default package size given
*
*    "Given
*    DATA: lt_ent_pckg_fee      TYPE /vpcoe/t_pckf_entity_data,
*          ls_ent_pckg_fee_data TYPE /vpcoe/s_pckf_ent_pckg_fee.
*
*    DATA lo_ent_processor TYPE REF TO tcl_pckf_proc_pckg_fee.
*
*    lo_ent_processor ?= /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_processor(
*       EXPORTING
*         iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*         iv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*    ).
*
*    ls_ent_pckg_fee_data = VALUE #( product = 'P001' reportconfiguration = 'REPCFG_1' ).
*    DATA(lr_entity) = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_pckg_fee_data ).
*    APPEND lr_entity TO lt_ent_pckg_fee.
*
*    ls_ent_pckg_fee_data = VALUE #( product = 'P002' reportconfiguration = 'REPCFG_1' ).
*    lr_entity = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_pckg_fee_data ).
*    APPEND lr_entity TO lt_ent_pckg_fee.
*
*    ls_ent_pckg_fee_data = VALUE #( product = 'P003' reportconfiguration = 'REPCFG_2' ).
*    lr_entity = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_pckg_fee_data ).
*    APPEND lr_entity TO lt_ent_pckg_fee.
*
*    lo_ent_processor->set_entity_data( lt_ent_pckg_fee ).
*
*    DATA(lo_cache) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_cache( ).
*
*    cl_abap_testdouble=>configure_call( double = lo_cache )->ignore_parameter( name = 'iv_ref_prot_uuid' )->and_expect( )->is_called_times( 1 ).
*    lo_cache->set_entities(
*      EXPORTING
*        it_entities      = lt_ent_pckg_fee
*        iv_ref_prot_uuid = '0815'
*    ).
*
*    "When
*    mo_cut->/vpcoe/if_pckf_exec_retrieval~execute_retrieval(
*      EXPORTING
*        iv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters  = VALUE /vpcoe/s_pckf_retrieval_input( rfc_des = 'RFC_01' )
*        iv_test        = abap_false
*    ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act  =  lo_ent_processor->get_retrieve_called( )
*        exp  =  1
*    ).
*
*    DATA lv_rfc_des TYPE rfcdest.
*    lo_ent_processor->/vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-rfc_des IMPORTING ev_value = lv_rfc_des ).
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act  =  lv_rfc_des
*        exp  =  'RFC_01'
*    ).
*
*    cl_abap_testdouble=>verify_expectations( double = lo_cache ).
*
*  ENDMETHOD.
*
*  METHOD exec_retrieve_sc1.
** Scenario 1: Mass amount of packaging fees and parameters given
*
*    "Given
*    DATA: lt_ent_pckg_fee      TYPE /vpcoe/t_pckf_entity_data,
*          ls_ent_pckg_fee_data TYPE /vpcoe/s_pckf_ent_pckg_fee,
*          lv_pckg_fee_cnt      TYPE i,
*          lv_index             TYPE i.
*
*    DATA lo_ent_processor TYPE REF TO tcl_pckf_proc_pckg_fee.
*
*    lo_ent_processor ?= /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_processor(
*       EXPORTING
*         iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*         iv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*    ).
*
*    lv_pckg_fee_cnt = 480.
*    lv_index = 1.
*    DO lv_pckg_fee_cnt TIMES.
*
*      ls_ent_pckg_fee_data = VALUE #( product = |'P00'{ lv_index }| reportconfiguration = 'REPCFG_1' ).
*      DATA(lr_entity) = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_ent_pckg_fee_data ).
*      APPEND lr_entity TO lt_ent_pckg_fee.
*    ENDDO.
*
*    lo_ent_processor->set_entity_data( lt_ent_pckg_fee ).
*
*    DATA(lo_cache) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_cache( ).
*
*    DATA(lt_ent_pckg_fee_1) = VALUE /vpcoe/t_pckf_entity_data( ).
*    lv_index = 1.
*    DO 50 TIMES.
*      APPEND INITIAL LINE TO lt_ent_pckg_fee_1 REFERENCE INTO DATA(lr_ent_pckg_fee_1).
*      lr_ent_pckg_fee_1->* = lt_ent_pckg_fee[ lv_index ].
*      ADD 1 TO lv_index.
*    ENDDO.
*
*    "expect first entity package
*    cl_abap_testdouble=>configure_call( double = lo_cache )->ignore_parameter( name = 'iv_ref_prot_uuid' )->and_expect( )->is_called_times( 1 ).
*    lo_cache->set_entities(
*      EXPORTING
*        it_entities      = lt_ent_pckg_fee_1
*        iv_ref_prot_uuid = '0815'
*    ).
*
*    cl_abap_testdouble=>configure_call( double = lo_cache )->ignore_all_parameters( )->and_expect( )->is_called_times( 9 ).
*    lo_cache->set_entities(
*      EXPORTING
*        it_entities      = lt_ent_pckg_fee
*        iv_ref_prot_uuid = '0815'
*    ).
*
*    "When
*    mo_cut->/vpcoe/if_pckf_exec_retrieval~execute_retrieval(
*      EXPORTING
*        iv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters  = VALUE /vpcoe/s_pckf_retrieval_input( package_size = 50 )
*        iv_test        = abap_false
*    ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act  =  lo_ent_processor->get_retrieve_called( )
*        exp  =  10
*    ).
*
*    cl_abap_testdouble=>verify_expectations( double = lo_cache ).
*
*  ENDMETHOD.
*
*ENDCLASS.
