*&---------------------------------------------------------------------*
*& Report  /VPCOE/R_UPH_PCKG_MCL_LOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/r_uph_pckg_mcl_load.

INCLUDE /vpcoe/version.

CONSTANTS:
  gc_screen_grp1_m1         TYPE char3      VALUE 'M1',
  gc_screen_grp1_m2         TYPE char3      VALUE 'M2',
  gc_screen_grp1_m3         TYPE char3      VALUE 'M3',

  gc_ucomm_onli             TYPE syst_ucomm VALUE 'ONLI',
  gc_ucomm_display_validity TYPE syst_ucomm VALUE 'DISPLAY_VALIDITY'.

TABLES sscrfields.

DATA: lv_material         TYPE matnr,
      lv_material_type    TYPE mtart,
      lv_material_group   TYPE matkl,
      lv_change_date      TYPE mara-laeda,
      lv_validity         TYPE sy-datum,
      lv_display_validity TYPE abap_bool VALUE abap_false,
      ls_surdp_s_input    TYPE /vpcoe/s_pckg_matclas_input,
      lv_upload_mode      TYPE /vpcoe/upload_mode,
      lo_mcl_load         TYPE REF TO /vpcoe/cl_uph_pckg_mcl_load,
      lo_logger           TYPE REF TO /vpcoe/if_uph_logger.

INCLUDE /vpcoe/uph_pckg_mcl_slscrn.

INITIALIZATION.
  lo_mcl_load = NEW /vpcoe/cl_uph_pckg_mcl_load( ).

  DATA(lo_cust) = NEW /vpcoe/cl_plm_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-plm
                                            iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-plm
                                            iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-mcl ).

  lo_logger = /vpcoe/cl_uph_factory=>get_instance( )->get_logger( iv_test_mode   = p_test
                                                                  iv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full ).

  "Authorization check
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD'  FIELD /vpcoe/if_uph_pckg_mcl_load=>gc_tcode.

  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH /vpcoe/if_uph_pckg_mcl_load=>gc_tcode.
    LEAVE LIST-PROCESSING.
  ENDIF.

*  " get RFC destination
  p_rfcdes = /vpcoe/if_plm_constants=>gc_rfc_plm_destinations-pkg_elements.
  IF p_source IS INITIAL.
    p_source = /vpcoe/cl_plm_helper=>get_source_id( ).
  ENDIF.

  INCLUDE /vpcoe/version_set.

*
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

  lv_upload_mode = lo_mcl_load->derive_upload_mode( ic_initial_load = p_iload
                                                    ic_delta_load   = p_dload ).

  ls_surdp_s_input = lo_mcl_load->/vpcoe/if_uph_pckg_mcl_load~derive_input_from_parameters( iv_source_id            = p_source
                                                                                            iv_rfc_destination      = p_rfcdes
                                                                                            iv_api_type             = lo_cust->get_api_type( )
                                                                                            iv_package_size         = lo_cust->get_package_size( )
                                                                                            iv_path_prefix          = lo_cust->get_service_url( )
                                                                                            iv_material_range       = s_mat[]
                                                                                            iv_material_type_range  = s_mattyp[]
                                                                                            iv_material_group_range = s_matgrp[]
                                                                                            iv_change_date_range    = s_chdate[]
                                                                                            iv_validity_date_range  = s_valdat[] ).

  lo_mcl_load->execute_upload( iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl
                               iv_upload_mode   = lv_upload_mode
                               is_parameters    = ls_surdp_s_input
                               iv_test          = p_test ).
ENDFORM.
