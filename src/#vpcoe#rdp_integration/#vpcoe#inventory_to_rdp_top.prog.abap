*&---------------------------------------------------------------------*
*&  Include           /VPCOE/INVENTORY_TO_RDP_TOP
*&---------------------------------------------------------------------*
TABLES:mseg, sscrfields.

DATA: go_cust             TYPE REF TO /vpcoe/cl_rdp_helper,
      lv_ucomm            TYPE sy-ucomm,
      lv_answer           TYPE char3,
      lt_r_sel_name       TYPE /vpcoe/tt_r_name,
      lt_selid_vrm        TYPE vrm_values,
      lt_sel_name         TYPE /vpcoe/cl_rdp_helper=>gty_t_sel_names,
      gv_month            TYPE numc2,
      gv_scr_fld_state    TYPE screen-input,
      gt_selid_vrm        TYPE vrm_values,
      gt_sel_params       TYPE /vpcoe/cl_rdp_helper=>gty_t_sel_opt,
      gv_sum              TYPE i,
      gt_bapiret2         TYPE bapiret2_t,
      gv_period_no_closed TYPE abap_bool,
      gv_default          TYPE string VALUE /vpcoe/cl_rdp_helper=>sc_service_id-inventory.

FIELD-SYMBOLS: <gv_value> TYPE any,
               <gt_value> TYPE STANDARD TABLE.
