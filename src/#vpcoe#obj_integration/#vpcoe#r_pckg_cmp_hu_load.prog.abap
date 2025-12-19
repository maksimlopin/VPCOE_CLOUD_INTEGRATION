*&---------------------------------------------------------------------*
*& Report  /VPCOE/R_PCKG_CMP_HU_LOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/r_pckg_cmp_hu_load.

CONSTANTS gc_ucomm_display_enddate TYPE syst_ucomm VALUE 'DISPLAY_VALIDITY'. " to be discussed
CONSTANTS gc_ucomm_onli            TYPE syst_ucomm VALUE 'ONLI'.
CONSTANTS gc_screen_grp1_m1        TYPE char3      VALUE 'M1'.

TABLES: sscrfields, lips, likp, adrc, t001w.
DATA ls_surdp_s_input   TYPE /vpcoe/if_pckg_cmp_hu_load=>gty_s_hu_input.
DATA lv_target_dest     TYPE rfcdest.
DATA lo_hu_load         TYPE REF TO /vpcoe/cl_pckg_cmp_hu_load.
DATA lv_display_enddate TYPE abap_bool.
DATA lv_message         TYPE string.
DATA lo_logger          TYPE REF TO /vpcoe/if_uph_logger.

INCLUDE /vpcoe/pckg_hu_sel_screen.

INITIALIZATION.

  DATA(lo_cust) = NEW /vpcoe/cl_plm_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-plm
                                            iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-plm
                                            iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-handling_units ).

  lo_logger = /vpcoe/cl_uph_factory=>get_instance( )->get_logger( iv_test_mode   = p_test
                                                                  iv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full ).

  lo_hu_load = NEW #( ).

  IF NOT lo_hu_load->/vpcoe/if_pckg_cmp_hu_load~is_authorized( ). "#EC PREFER_IS_NOT
    MESSAGE e009(/vpcoe/plm) WITH /vpcoe/if_pckg_cmp_hu_load=>gc_tcode.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_source IS INITIAL.
    p_source = 'ECC_EMEA'.
  ENDIF.

  p_rfcdes = /vpcoe/if_plm_constants=>gc_rfc_plm_destinations-pkg_compositions.

  " set start date to today - 3 months
  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING
      months  = -3
      olddate = sy-datum
    IMPORTING
      newdate = p_start.

START-OF-SELECTION.
  PERFORM f_main_processing.

FORM f_main_processing.

  IF p_test = abap_false.
*                          TEMPORARY
*******    DATA(lv_coincidence) = lo_cust->compare_pass_prefixes( iv_rfc_des = p_rfcdes io_logger = lo_logger ).
*******    IF lv_coincidence <> abap_true.
*******      RETURN.
*******    ENDIF.

  ENDIF.
                                                           "#EC CI_FORM
  CLEAR: ls_surdp_s_input.

  ls_surdp_s_input = lo_hu_load->/vpcoe/if_pckg_cmp_hu_load~derive_input_from_parameters(
                   iv_source_id                = p_source
                   iv_rfc_destination          = p_rfcdes
                   iv_act_goods_mvt_date_range = VALUE #( ( sign   = 'I'
                                                            option = 'BT'
                                                            high   = p_end
                                                            low    = p_start )  )
                   iv_category_range           = s_categ[]
                   iv_country_range            = s_cntry[]
                   iv_distribution_range       = s_distr[]
                   iv_division_range           = s_divis[]
                   iv_document_type_range      = s_type[]
                   iv_material_range           = s_matnr[]
                   iv_material_group_range     = s_group[]
                   iv_material_type_range      = s_typem[]
                   iv_plant_country_rage       = s_pcntr[]
                   iv_plant_range              = s_plant[]
                   iv_sales_org_range          = s_sales[]
                   iv_sddoc_range              = s_vbeln[]
                   iv_ship_to_party_range      = s_kunnr[] ).

  " start the upload
  lo_hu_load->/vpcoe/if_uph_report~execute_upload( iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu
                                                   iv_upload_mode   = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
                                                   is_parameters    = ls_surdp_s_input
                                                   iv_test          = p_test ).
ENDFORM.
