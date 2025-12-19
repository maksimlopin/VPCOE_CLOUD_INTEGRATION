*&---------------------------------------------------------------------*
*& Report R_PCKF_RETRIEVAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/r_pckf_retrieval.

INCLUDE /vpcoe/version.

CONSTANTS:
  gc_screen_grp1_m1 TYPE char3      VALUE 'M1',
  gc_screen_grp1_m2 TYPE char3      VALUE 'M2',
  gc_screen_grp1_m3 TYPE char3      VALUE 'M3',
  gc_screen_grp1_m4 TYPE char3      VALUE 'M4',

  gc_ucomm_onli     TYPE syst_ucomm VALUE 'ONLI',
  gc_tcode          TYPE syst_tcode VALUE '/VPCOE/PCKF_RETRIEVE'.

TABLES : sscrfields.

DATA :
  ls_retrieval_input   TYPE /vpcoe/s_pckf_retrieval_input,
  lv_upload_mode       TYPE /vpcoe/upload_mode,
  lv_report_categ      TYPE /vpcoe/pckf_report_categ_id,
  lv_report_config     TYPE /vpcoe/pckf_report_config_id,
  lv_shiptoparty       TYPE string,
  lv_context           TYPE char255,
  lv_busi_proc_directn TYPE /vpcoe/pckf_proc_drctn,
  lv_version           TYPE i.

DATA lo_exec_retrieval TYPE REF TO /vpcoe/cl_pckf_exec_retrieval.

INCLUDE /vpcoe/pckf_sel_scr_retrieve.

INITIALIZATION.

  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD'  FIELD gc_tcode.
  IF sy-subrc <> 0.
    MESSAGE e010(/vpcoe/pckf) WITH gc_tcode.
    LEAVE LIST-PROCESSING.
  ENDIF.
  " set the initial load id to save the variant
  p_varn = 'PCKF VAR. ' && sy-datum && '\' && sy-uzeit.
  lo_exec_retrieval = NEW /vpcoe/cl_pckf_exec_retrieval( ).

  "get RFC destination
  p_rfcdes = lo_exec_retrieval->/vpcoe/if_pckf_exec_retrieval~derive_rfc_target_destination(  ).
  p_pckgsz = /vpcoe/if_pckf_entity_proc=>gc_defaults-retrieve_package_size.

  "restrict select option for context
  DATA: ls_restrictions TYPE sscr_restrict.
  APPEND VALUE #( name = 'EQ' options-eq = abap_true ) TO ls_restrictions-opt_list_tab.
  APPEND VALUE #( kind = 'S' name = 'S_CONTX' sg_main = 'I' sg_addy = '' op_main = 'EQ' op_addy = '' ) TO ls_restrictions-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = ls_restrictions.

  INCLUDE /vpcoe/version_set.

START-OF-SELECTION.
  PERFORM f_main_processing.


FORM f_main_processing.                                    "#EC CI_FORM

  CLEAR : ls_retrieval_input, lv_upload_mode.

  IF p_iload IS NOT INITIAL.
    lv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full.
  ELSEIF p_dload IS NOT INITIAL.
    lv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
    IF p_dvar IS INITIAL.
      MESSAGE e033(/vpcoe/pckf).
    ENDIF.
  ELSE.
    lv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full.
  ENDIF.

  ls_retrieval_input-rfc_des = p_rfcdes.
  ls_retrieval_input-test_mode = p_test.
  ls_retrieval_input-report_config_id = s_rptcnf[].
  ls_retrieval_input-report_categ_id = s_rptcat[].
  ls_retrieval_input-cust_depend = COND #( WHEN r_cust = abap_true THEN abap_true
                                           WHEN r_scnr = abap_true THEN abap_false ).
*  ls_retrieval_input-ship_parrole = s_shpptr[].
  ls_retrieval_input-country = p_cntry.
*  ls_retrieval_input-region = p_rgn.
  ls_retrieval_input-package_size = p_pckgsz.
  IF lv_version = 1.
    ls_retrieval_input-context = s_contx[].
  ENDIF.
  ls_retrieval_input-bus_proc_dir = s_dirctn-low.

* pass the initial load Id for initial load, incase of delta load initial load id should be selected
  IF p_iload IS NOT INITIAL OR p_dvar IS NOT INITIAL.
    TRANSLATE p_varn TO UPPER CASE.
    ls_retrieval_input-initial_load_id = p_varn.
  ENDIF.




  lo_exec_retrieval->/vpcoe/if_pckf_exec_retrieval~execute_retrieval( iv_upload_mode = lv_upload_mode
                                                                      is_parameters  = ls_retrieval_input
                                                                      iv_test        = p_test ).
ENDFORM.
