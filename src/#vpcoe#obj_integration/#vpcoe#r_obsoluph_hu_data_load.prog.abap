*&---------------------------------------------------------------------*
*& Report  /vpcoe/r_uph_pckg_data_load
*&
*&---------------------------------------------------------------------*
*& Upload of data to RDP Push API
*&---------------------------------------------------------------------*

REPORT /vpcoe/r_obsoluph_hu_data_load MESSAGE-ID /vpcoe/plm.

*OBSOLETED!!!


*
*INCLUDE /vpcoe/version.
*
*TABLES: sscrfields, lips, likp, adrc, t001w.
*DATA:
*  lv_spec_type   TYPE tcg31-subcat,
*  lv_spec_id     TYPE estrh-subid,
*  ls_s_input     TYPE /vpcoe/s_pckg_elem_input,
*  ls_sel_opt     TYPE /vpcoe/s_selopt_hu,
*  lv_target_dest TYPE rfcdest,
*  lv_date        TYPE wadat_ist,
*  lo_plm_report  TYPE REF TO /vpcoe/cl_hu_plm_load.
*
*INCLUDE /vpcoe/uph_hu_sel_screen.
*
*INITIALIZATION.
*  CLEAR: lv_spec_type, lv_spec_id, ls_s_input.
*
*  "Authorization check
*  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
*                                                      ELSE '/VPCOE/UPH_PCI_HU' ).
*  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
*  IF sy-subrc <> 0.
*    MESSAGE e009(/vpcoe/plm) WITH lv_tcode.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
*  lo_plm_report = NEW #( ).
*
*  INCLUDE /vpcoe/version_set.
*
*START-OF-SELECTION.
*
*  IF p_rfcdes IS INITIAL.
*    MESSAGE i032(/vpcoe/plm) DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
*
*  IF p_wadat IS INITIAL OR p_month IS INITIAL.
*    MESSAGE i045(/vpcoe/plm).
*    RETURN.
*  ELSEIF p_month > 6 OR p_month < 1.
*    MESSAGE i046(/vpcoe/plm).
*    RETURN.
*  ELSE.
*    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*      EXPORTING
*        date      = p_wadat
*        days      = 0
*        months    = p_month
*        signum    = '-'
*        years     = 0
*      IMPORTING
*        calc_date = lv_date.
*
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*  ENDIF.
*
*  DATA(lo_cust) = NEW /vpcoe/cl_plm_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-plm
*                                            iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-plm
*                                            iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-handling_units ).
*
*  ls_sel_opt = VALUE #( document_type = so_type[]
*                        sales_org     = so_sales[]
*                        country       = so_cntry[]
*                        division      = so_divis[]
*                        category      = so_categ[]
*                        distribution  = so_distr[]
*                        vbeln         = so_vbeln[]
*                        plnt_country = so_pcntr[]
*                        mat_number   = so_matnr[]
*                        mat_type     = so_typem[]
*                        mat_group    = so_group[]
*                        plant        = so_plant[]
*                        ship_to_party = so_kunnr[]
*                        act_goods_mvt_date = VALUE #( ( sign   = 'I'
*                                                        option = 'BT'
*                                                        high   = p_wadat
*                                                        low    = lv_date )  )
*                        source_id = p_source
*                        rfc_des = p_rfcdes
*                        package_size = lo_cust->get_package_size( )
*                        path_prefix  = lo_cust->get_service_url( )
*                        api_type     = lo_cust->get_api_type( ) ).
*
*  "start the upload
*  lo_plm_report->execute_upload( iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu
*                                 iv_upload_mode   = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*                                 is_parameters    = ls_sel_opt
*                                 iv_test          = p_test ).
