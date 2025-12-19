*&---------------------------------------------------------------------*
*& Report R_SURDP_UPH_PCKG_CMP_BOM_LOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/r_uph_pckg_cmp_bom_load.

INCLUDE /vpcoe/version.

CONSTANTS:
  gc_screen_grp1_m1         TYPE char3      VALUE 'M1',
  gc_screen_grp1_m2         TYPE char3      VALUE 'M2',
  gc_screen_grp1_m3         TYPE char3      VALUE 'M3',
  gc_ucomm_onli             TYPE syst_ucomm VALUE 'ONLI',
  gc_ucomm_display_validity TYPE syst_ucomm VALUE 'DISPLAY_VALIDITY'.

TABLES: sscrfields.

DATA: lv_material         TYPE matnr,
      lv_plant            TYPE werks_d,
      lv_material_type    TYPE mtart,
      lv_bom_usage        TYPE stlan,
      lv_alt_bom          TYPE stlal,
      lv_bom_chgdon       TYPE trgr_date,
      lv_bom_val_from     TYPE sy-datum,
      lv_bom_status       TYPE stlst,
      lv_max_exp_lvl      TYPE cs_maxst,
      ls_surdp_s_input    TYPE /vpcoe/s_pckg_bom_input,
      lv_upload_mode      TYPE /vpcoe/upload_mode,
      lv_display_validity TYPE abap_bool VALUE abap_false,
      lo_logger           TYPE REF TO /vpcoe/if_uph_logger.

DATA lo_bom_load TYPE REF TO /vpcoe/cl_uph_pcg_cmp_bom_load.

INCLUDE /vpcoe/uph_pckg_cmp_bom_load.

INITIALIZATION.
  lo_bom_load = NEW /vpcoe/cl_uph_pcg_cmp_bom_load( ).

  DATA(lo_cust) = NEW /vpcoe/cl_plm_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-plm
                                            iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-plm
                                            iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-bom ).

  lo_logger = /vpcoe/cl_uph_factory=>get_instance( )->get_logger( iv_test_mode   = p_test
                                                                  iv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full ).

  IF NOT lo_bom_load->/vpcoe/if_uph_pcg_cmp_bom_load~is_authorized(  ). "#EC PREFER_IS_NOT
    MESSAGE e009(/vpcoe/plm) WITH /vpcoe/if_uph_pcg_cmp_bom_load=>gc_tcode.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "get RFC destination
  IF p_source IS INITIAL.
    p_source = /vpcoe/cl_rdp_helper=>get_source_id( ).
  ENDIF.

  p_varn = 'BoM Var.' && sy-datum && '\' && sy-uzeit.

  INCLUDE /vpcoe/version_set.

START-OF-SELECTION.

  PERFORM f_main_processing.


FORM f_main_processing.

  IF p_test = abap_false.

    DATA(lv_coincidence) = lo_cust->compare_pass_prefixes( iv_rfc_des = p_rfcdes io_logger = lo_logger ).
    IF lv_coincidence <> abap_true.
      RETURN.
    ENDIF.

  ENDIF.
                                                           "#EC CI_FORM
  CLEAR : ls_surdp_s_input, lv_upload_mode.

  lv_upload_mode = lo_bom_load->derive_upload_mode( ic_initial_load = p_iload
                                                    ic_delta_load   = p_dload ).

  ls_surdp_s_input = lo_bom_load->/vpcoe/if_uph_pcg_cmp_bom_load~derive_input_from_parameters( iv_source_id             = p_source
                                                                                               iv_rfc_destination       = p_rfcdes
                                                                                               iv_material_range        = s_mat[]
                                                                                               iv_material_type_range   = s_mattyp[]
                                                                                               iv_plant_range           = s_plant[]
                                                                                               iv_bom_usage_range       = s_usage[]
                                                                                               iv_alternative_bom_range = s_altbom[]
                                                                                               iv_bom_status            = p_bomst
                                                                                               iv_bom_validity_date     = p_valdat
                                                                                               iv_bom_change_date_range = s_chdate[]
                                                                                               iv_max_explosion_level   = p_explvl
                                                                                               iv_package_size          = lo_cust->get_package_size( )
                                                                                               iv_path_prefix           = lo_cust->get_service_url( )
                                                                                               iv_api_type              = lo_cust->get_api_type( )
                                                                                               iv_load_id               = p_varn ).

  lo_bom_load->execute_upload( iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
                               iv_upload_mode   = lv_upload_mode
                               is_parameters    = ls_surdp_s_input
                               iv_test          = p_test ).
ENDFORM.


" Local test class
CLASS lcl_test DEFINITION FOR TESTING INHERITING FROM cl_aunit_assert
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_double_exec_load_wrap TYPE REF TO /vpcoe/if_uph_exec_load_wrap.

    METHODS setup.
    METHODS teardown.

    METHODS test_main_processing FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    mo_double_exec_load_wrap ?= cl_abap_testdouble=>create( '/vpcoe/if_uph_exec_load_wrap' ).
    lo_bom_load = NEW /vpcoe/cl_uph_pcg_cmp_bom_load( io_exec_load_wrapper = mo_double_exec_load_wrap ).
  ENDMETHOD.


  METHOD teardown.
    CLEAR: mo_double_exec_load_wrap.
  ENDMETHOD.

  METHOD test_main_processing.

    " Given: setup parameters
    p_iload = 'X'.
    p_test  = 'X'.

    p_bomst   = '01'.
    p_explvl  = '5'.

    cl_abap_testdouble=>configure_call( mo_double_exec_load_wrap )->ignore_parameter( name = 'is_parameters' )->and_expect( )->is_called_once( ).
    mo_double_exec_load_wrap->execute_upload( iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
                                              iv_upload_mode   = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
                                              is_parameters    = ls_surdp_s_input
                                              iv_test          = 'X' ).

    " When
    PERFORM f_main_processing.

    " Then
    assert_equals( act = sy-subrc exp = 0 ).

    assert_equals( exp = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full act = lv_upload_mode  ).
    assert_not_initial( act = ls_surdp_s_input ).

    assert_equals( exp = '01'         act = ls_surdp_s_input-bom_status ).
    assert_equals( exp = '5'          act = ls_surdp_s_input-max_exp_lvl ).

    cl_abap_testdouble=>verify_expectations( mo_double_exec_load_wrap ).
  ENDMETHOD.

ENDCLASS.
