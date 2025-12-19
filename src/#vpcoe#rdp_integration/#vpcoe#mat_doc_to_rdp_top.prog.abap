*&---------------------------------------------------------------------*
*&  Include           /VPCOE/MAT_DOC_TO_RDP_TOP
*&---------------------------------------------------------------------*
TABLES: t001w, mseg, sscrfields, vbrk, vbrp, rbkp, rseg, mara, mvke.

TYPES: BEGIN OF gs_variants,
         code    TYPE /vpcoe/de_mat_doc_variants,
         variant TYPE boolean,
       END OF gs_variants.

DATA: go_cust                  TYPE REF TO /vpcoe/cl_rdp_helper,
      gt_material_document     TYPE STANDARD TABLE OF /vpcoe/cl_rdp_material_doc=>gty_s_material,
      gt_supinvi               TYPE /vpcoe/t_supinvi_data,
      gt_billdi                TYPE /vpcoe/t_billdi_data,
      gt_supinvi_all           TYPE /vpcoe/t_supinvi_data,
      gt_billdi_all            TYPE /vpcoe/t_billdi_data,
      gt_variants              TYPE STANDARD TABLE OF gs_variants,
      lv_ucomm                 TYPE sy-ucomm,
      lv_answer                TYPE char3,
      lt_r_sel_name            TYPE /vpcoe/tt_r_name,
      lt_selid_vrm             TYPE vrm_values,
      lt_sel_name              TYPE /vpcoe/cl_rdp_helper=>gty_t_sel_names,
      gt_sel_params            TYPE /vpcoe/cl_rdp_helper=>gty_t_sel_opt,
      gt_bapiret2              TYPE bapiret2_t,
      gt_selid_vrm             TYPE vrm_values,
      gt_material_document_all TYPE STANDARD TABLE OF /vpcoe/cl_rdp_material_doc=>gty_s_material,
      gs_sel_opt               TYPE /vpcoe/s_selopt_mat_doc,
      gv_scr_fld_state         TYPE screen-input,
      gv_global_no_data        TYPE flag,
      gv_default               TYPE string VALUE /vpcoe/cl_rdp_helper=>sc_service_id-material_doc.

FIELD-SYMBOLS: <gv_value> TYPE any,
               <gt_value> TYPE STANDARD TABLE.
