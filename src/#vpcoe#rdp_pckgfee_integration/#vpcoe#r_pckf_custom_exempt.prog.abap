*&---------------------------------------------------------------------*
*& Report  /VPCOE/R_PCKF_CUSTOM_EXEMPT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/r_pckf_custom_exempt.

INCLUDE /vpcoe/version.

CONSTANTS: gc_tcode TYPE syst_tcode VALUE '/VPCOE/PCKF_CUS_EXMP'.

DATA: gt_customer_role    TYPE STANDARD TABLE OF vrm_value WITH DEFAULT KEY,

      ls_cust_exemp_input TYPE /vpcoe/s_pckf_cstm_exemp_input,
      lv_upload_mode      TYPE /vpcoe/upload_mode,
      lv_rdp_cust_exemp   TYPE string,

      lo_exec_cust_exemp  TYPE REF TO /vpcoe/cl_pckf_customer_exemp.

INCLUDE /vpcoe/pckf_sel_scr_cstm_exmp.

INITIALIZATION.

  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD'  FIELD gc_tcode.
  IF sy-subrc <> 0.
    MESSAGE e010(/vpcoe/pckf) WITH gc_tcode.
    LEAVE LIST-PROCESSING.
  ENDIF.

  lo_exec_cust_exemp = NEW /vpcoe/cl_pckf_customer_exemp( ).
  INCLUDE /vpcoe/version_set.

START-OF-SELECTION.

  IF p_iload IS NOT INITIAL.
    lv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full.
  ELSEIF p_dload IS NOT INITIAL.
    lv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
  ELSE.
    lv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full.
  ENDIF.

  DATA(lo_customer_exemp) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                                    iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-customer
                                                    iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-customer_exemptions ).

  ls_cust_exemp_input = VALUE #( rfc_des        = lo_customer_exemp->get_generic_rfc_name( )
                                test_mode       = p_test
                                customer        = p_cstm
                                report_category = p_ctgr
                                package_size    = lo_customer_exemp->get_package_size( )
                                cust_exemp_url  = lo_customer_exemp->get_service_url( ) ).
*
  lo_exec_cust_exemp->/vpcoe/if_pckf_exec_retrieval~execute_retrieval( iv_upload_mode = lv_upload_mode
                                                                       is_parameters  = ls_cust_exemp_input
                                                                       iv_test        = p_test ).
