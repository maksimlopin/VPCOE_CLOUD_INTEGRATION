CLASS /vpcoe/tcl_uph_exec_load_wrap DEFINITION DEFERRED.
CLASS /vpcoe/cl_uph_exec_load_wrap DEFINITION LOCAL FRIENDS /vpcoe/tcl_uph_exec_load_wrap.

CLASS /vpcoe/tcl_uph_exec_load_wrap DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_uph_exec_load_wrap,  "class under test
      mo_upl_proc_pckg_plm TYPE REF TO /vpcoe/if_uph_entity_proc .
    METHODS: setup.
    METHODS: teardown.
    METHODS: load_wrap_td FOR TESTING.
    METHODS: get_f4_help FOR TESTING.

ENDCLASS.       "tcl_Susrdp_Uph_Pckg_Load_Wrap


CLASS /vpcoe/tcl_uph_exec_load_wrap IMPLEMENTATION.

  METHOD setup.

    f_cut = NEW #( ).

    mo_upl_proc_pckg_plm ?= cl_abap_testdouble=>create( object_name = '/VPCOE/IF_UPH_ENTITY_PROC' ).

    data(lo_factory_double) = NEW /vpcoe/td_uph_factory(  ).
    lo_factory_double->set_entity_processor( mo_upl_proc_pckg_plm ).
    /vpcoe/th_uph_factory_injector=>inject_factory_double( io_double = lo_factory_double ).

  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD load_wrap_td.

    DATA : ls_input TYPE /vpcoe/s_pckg_elem_input.

       CLEAR : ls_input-spec_radio_btn.
    ls_input-objid = 'RDP_UPLOAD'.
*    ls_input-valtodate = sy-datum.

******************* TODOPM ***********************************
    "configure the call to prepare -| return 2 |
* data : mo_osql_env TYPE REF TO if_osql_test_environment.
* mo_osql_env = cl_osql_test_environment=>create( i_dependency_list = VALUE #( (  'ESTRH' ) ) ).
*
** mo_osql_env->destroy(  ).
    TYPES:
      BEGIN OF lty_sub_key,
        recn    TYPE eserecn,
        subid   TYPE esesubid,
        subcat  TYPE esesubcat,
        upddats TYPE eseupddats,
      END OF lty_sub_key .
    TYPES:
      ltty_sub_key TYPE SORTED TABLE OF lty_sub_key WITH UNIQUE KEY recn.
      data lv_count type i VALUE 2.

*    DATA :  mo_sub_proxy_mock TYPE REF TO cl_ehfnd_ehssub_proxy.
*    mo_sub_proxy_mock ?= cl_abap_testdouble=>create( object_name = 'CL_EHFND_EHSSUB_PROXY' ).
*    DATA(lt_hitlist_result) = VALUE ehfndt_ehs_hitpos( ( recn = '01' subcat = 'REAL_SUB' ) ( recn = '02' subcat = 'REAL_SUB' ) ).
*    cl_abap_testdouble=>configure_call( mo_sub_proxy_mock )->set_parameter( name = 'et_substances' value = lt_hitlist_result )->and_expect( )->is_called_once(  ).

    data(lt_sub_keys) = VALUE ltty_sub_key( ( recn = '21' subcat = 'REAL_SUB' ) ( recn = '22' subcat = 'REAL_SUB' ) ).
*    cl_abap_testdouble=>configure_call( mo_upl_proc_pckg_plm )->set_parameter( name = 'mt_sub_keys' value = lt_sub_keys )->and_expect( )->is_called_once(  ).
    cl_abap_testdouble=>configure_call( mo_upl_proc_pckg_plm )->returning( value = lv_count )->and_expect( )->is_called_once( ) . " times( 3 ). "called_once(  ).
    mo_upl_proc_pckg_plm->prepare_process(  ).

**********************************************************************
    cl_abap_unit_assert=>assert_bound( act = f_cut ).

    "TODO: rework this test: the execute upload method should return all collected messages
    "which then are checked here for the different cases

    f_cut->execute_upload(
        iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm
        iv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
        is_parameters = ls_input
        iv_test = abap_true ).

  ENDMETHOD.

  METHOD get_f4_help.
    DATA : lt_hit_objid TYPE STANDARD TABLE OF tcghit-objid.

    DATA(lv_retirn_code) = /vpcoe/cl_uph_exec_load_wrap=>get_f4_help(
    iv_retfield = 'OBJID' iv_dynprofield = 'P_HITOBJ' it_value_tab = lt_hit_objid ) ##NEEDED.

  ENDMETHOD.

ENDCLASS.
