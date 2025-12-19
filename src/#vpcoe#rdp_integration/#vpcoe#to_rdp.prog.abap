*&---------------------------------------------------------------------*
*& Report  /VPCOE/TO_RDP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/to_rdp.
CONSTANTS: gc_bom_pack       TYPE char40 VALUE 'BOM_PACK',
*           gc_plm_pack       TYPE char40 VALUE 'PLM_PACK',
*           gc_bom_rcp        TYPE char40 VALUE 'BOM_RCP',
*           gc_pckg_plm       TYPE char40 VALUE 'PCKG_PLM',
           gc_pckg_matcl     TYPE char40 VALUE 'PCKG_MATCL',
           gc_post_pfee      TYPE char40 VALUE 'POST_PFEE',
           gc_retrieve_pfee  TYPE char40 VALUE 'RETRIEVE_PFEE',
           gc_ent_cstm_role  TYPE char40 VALUE 'CSTM_ROLE',
           gc_ent_cstm_exemp TYPE char40 VALUE 'CSTM_EXEMP',
           gc_ent_comp_code  TYPE char40 VALUE 'COMP_CODE'.

DATA: gt_rdp_srv_id TYPE STANDARD TABLE OF vrm_value WITH DEFAULT KEY,
      gt_plm_srv_id TYPE STANDARD TABLE OF vrm_value WITH DEFAULT KEY,
      gt_pfe_srv_id TYPE STANDARD TABLE OF vrm_value WITH DEFAULT KEY.

INCLUDE /vpcoe/version.
*-----------------Selection Screens-----------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK srvtype WITH FRAME TITLE text-006.
PARAMETERS: r_rdp RADIOBUTTON GROUP srv USER-COMMAND srv_id DEFAULT 'X',
            r_plm RADIOBUTTON GROUP srv,
            r_pfe RADIOBUTTON GROUP srv.
SELECTION-SCREEN END OF BLOCK srvtype.

SELECTION-SCREEN BEGIN OF BLOCK rdp_srvid WITH FRAME TITLE text-003.
PARAMETERS: p_rdpsrv TYPE char40 AS LISTBOX VISIBLE LENGTH 40 OBLIGATORY USER-COMMAND lb_cmd MODIF ID rdp.
SELECTION-SCREEN END OF BLOCK rdp_srvid.

SELECTION-SCREEN BEGIN OF BLOCK plm_rep WITH FRAME TITLE text-004.
PARAMETERS: p_plmsrv TYPE char40 AS LISTBOX VISIBLE LENGTH 40 OBLIGATORY USER-COMMAND lb_cmd MODIF ID plm.
SELECTION-SCREEN END OF BLOCK plm_rep.

SELECTION-SCREEN BEGIN OF BLOCK pckg_fee WITH FRAME TITLE text-005.
PARAMETERS: p_pfesrv TYPE char40 AS LISTBOX VISIBLE LENGTH 40 OBLIGATORY USER-COMMAND lb_cmd MODIF ID pfe.
SELECTION-SCREEN END OF BLOCK pckg_fee.

*-----------------Screens Processing----------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'RDP'.
        screen-active = COND #( WHEN r_rdp = 'X' THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      WHEN 'PLM'.
        screen-active = COND #( WHEN r_plm = 'X' THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      WHEN 'PFE'.
        screen-active = COND #( WHEN r_pfe = 'X' THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      WHEN 'TRL'.
        screen-intensified = '1'.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
  INCLUDE /vpcoe/version_set.

*-----------------Initialization--------------------------------------*
INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.
  gt_rdp_srv_id = VALUE #( ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-batch         text = 'Batch'(015) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-organization  text = 'Organization Data'(022) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration text = 'Configuration Data'(016) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-incoterms     text = 'Incoterms'(032) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-customer      text = 'Customer'(017) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-supplier      text = 'Supplier'(024) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-product       text = 'Product'(023) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-delivery      text = 'Delivery'(018) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc  text = 'Material Documents'(021) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-inventory     text = 'Inventory'(019) ) ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_RDPSRV'
      values          = gt_rdp_srv_id
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc = 0.
    p_rdpsrv = /vpcoe/cl_rdp_helper=>sc_grp_id-batch.
  ENDIF.

  gt_plm_srv_id = VALUE #( ( key = gc_bom_pack   text = 'Extract Packaging Composition from BOM'(025) )
*                           ( key = gc_plm_pack   text = 'Extract Packaging Composition from PLM'(026) )
*                           ( key = gc_bom_rcp    text = 'Extract Packaging Composition from Recipe Development'(027) )
*                           ( key = gc_pckg_plm   text = 'Extract Packaging Elements from PLM'(028) )
                           ( key = gc_pckg_matcl text = 'Extract Packaging Element from Mat.Classification'(029) )
                           ( key = /vpcoe/cl_rdp_helper=>sc_grp_id-hu  text = 'Extract Packaging Composition Items from Handling Units'(033) ) ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_PLMSRV'
      values          = gt_plm_srv_id
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc = 0.
    p_plmsrv = gc_bom_pack.
  ENDIF.

  gt_pfe_srv_id = VALUE #( ( key = gc_retrieve_pfee  text = 'Retrieve Packaging Fees'(031) )
                           ( key = gc_post_pfee      text = 'Post Packaging Fees'(030) )
                           ( key = gc_ent_cstm_role  text = 'Retrieve Customer Role'(035) )
                           ( key = gc_ent_cstm_exemp text = 'Retrieve Customer Exemptions'(036) )
                           ( key = gc_ent_comp_code  text = 'Retrieve Company Code Exemptions'(034) ) ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_PFESRV'
      values          = gt_pfe_srv_id
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc = 0.
    p_pfesrv = gc_retrieve_pfee.
  ENDIF.

*-----------------Report Processing-----------------------------------*
START-OF-SELECTION.

  IF r_rdp = abap_true.
    DATA(lv_id) = gt_rdp_srv_id[ key = p_rdpsrv ].
    CASE lv_id-key.
      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-batch.
        SUBMIT /vpcoe/batch_to_rdp AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-configuration.
        SUBMIT /vpcoe/configuration_to_rdp AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-customer.
        SUBMIT /vpcoe/customer_to_rdp AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-delivery.
        SUBMIT /vpcoe/delivery_to_rdp AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-incoterms.
        SUBMIT /vpcoe/incoterms_to_rdp AND RETURN VIA SELECTION-SCREEN.
*
      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-inventory.
        SUBMIT /vpcoe/inventory_to_rdp AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc.
        SUBMIT /vpcoe/mat_doc_to_rdp AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-organization.
        SUBMIT /vpcoe/org_data_to_rdp AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-product.
        SUBMIT /vpcoe/product_to_rdp AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-supplier.
        SUBMIT /vpcoe/supplier_to_rdp AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-hu.
        SUBMIT /vpcoe/r_pckg_cmp_hu_load AND RETURN VIA SELECTION-SCREEN.
    ENDCASE.

  ELSEIF r_plm = abap_true.
    lv_id = gt_plm_srv_id[ key = p_plmsrv ].
    CASE lv_id-key.
      WHEN gc_bom_pack.
        SUBMIT /vpcoe/r_uph_pckg_cmp_bom_load AND RETURN VIA SELECTION-SCREEN.

*      WHEN gc_plm_pack.
*        SUBMIT /vpcoe/r_uph_pckg_cmp_plm_load AND RETURN VIA SELECTION-SCREEN.

*      WHEN gc_bom_rcp.
*        SUBMIT /vpcoe/r_uph_pckg_cmp_rcp_load AND RETURN VIA SELECTION-SCREEN.

*      WHEN gc_pckg_plm.
*        SUBMIT /vpcoe/r_uph_pckg_data_load AND RETURN VIA SELECTION-SCREEN.

      WHEN gc_pckg_matcl.
        SUBMIT /vpcoe/r_uph_pckg_mcl_load AND RETURN VIA SELECTION-SCREEN.

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-hu.
        SUBMIT /vpcoe/r_pckg_cmp_hu_load AND RETURN VIA SELECTION-SCREEN.

    ENDCASE.

  ELSEIF r_pfe = abap_true.
    lv_id = gt_pfe_srv_id[ key = p_pfesrv ].
    CASE lv_id-key.
      WHEN gc_post_pfee.
        SUBMIT /vpcoe/r_pckf_posting  AND RETURN VIA SELECTION-SCREEN.

      WHEN gc_retrieve_pfee.
        SUBMIT /vpcoe/r_pckf_retrieval AND RETURN VIA SELECTION-SCREEN.

      WHEN gc_ent_cstm_role.
        SUBMIT /vpcoe/r_pckf_customer_role AND RETURN VIA SELECTION-SCREEN.

      WHEN gc_ent_comp_code.
        SUBMIT /vpcoe/r_pckf_comp_code_exem AND RETURN VIA SELECTION-SCREEN.

      WHEN gc_ent_cstm_exemp.
        SUBMIT /vpcoe/r_pckf_custom_exempt AND RETURN VIA SELECTION-SCREEN.

    ENDCASE.

  ENDIF.
