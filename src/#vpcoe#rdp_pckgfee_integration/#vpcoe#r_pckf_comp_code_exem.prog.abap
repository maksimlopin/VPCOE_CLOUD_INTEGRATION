*&---------------------------------------------------------------------*
*& Report  /VPCOE/R_PCKF_CUSTOMER_ROLE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/r_pckf_comp_code_exem.
INCLUDE /vpcoe/version.

CONSTANTS: gc_tcode TYPE syst_tcode VALUE '/VPCOE/PCKF_CC_EXMP'.

DATA: gt_customer_role   TYPE STANDARD TABLE OF vrm_value WITH DEFAULT KEY,

      ls_comp_code_input TYPE /vpcoe/s_pckf_comp_code_input,
      lv_upload_mode     TYPE /vpcoe/upload_mode,
      lv_rdp_comp_code   TYPE string,

      lo_exec_comp_code  TYPE REF TO /vpcoe/cl_pckf_company_code.

INCLUDE /vpcoe/pckf_sel_scr_comp_code.

INITIALIZATION.

  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD'  FIELD gc_tcode.
  IF sy-subrc <> 0.
    MESSAGE e010(/vpcoe/pckf) WITH gc_tcode.
    LEAVE LIST-PROCESSING.
  ENDIF.

  lo_exec_comp_code = NEW /vpcoe/cl_pckf_company_code( ).
  INCLUDE /vpcoe/version_set.

START-OF-SELECTION.

  IF p_iload IS NOT INITIAL.
    lv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full.
  ELSEIF p_dload IS NOT INITIAL.
    lv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
  ELSE.
    lv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full.
  ENDIF.

  DATA(lo_company_code) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                                    iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-company_code
                                                    iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-company_code_exe ).

  ls_comp_code_input = VALUE #( rfc_des         = lo_company_code->get_generic_rfc_name( )
                                test_mode       = p_test
                                company_code    = p_code
                                report_category = p_ctgr
                                package_size    = lo_company_code->get_package_size( )
                                comp_code_url   = lo_company_code->get_service_url( ) ).

  lo_exec_comp_code->/vpcoe/if_pckf_exec_retrieval~execute_retrieval( iv_upload_mode = lv_upload_mode
                                                                      is_parameters  = ls_comp_code_input
                                                                      iv_test        = p_test ).
