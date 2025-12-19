*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_CUSTOMER_TO_RDP_TOP
*&---------------------------------------------------------------------*

TABLES: kna1, knb1, cdhdr, sscrfields, knvv.

DATA: gt_bapiret2     TYPE bapiret2_t,
      gt_bapiret2_ext TYPE bapiret2_t,
      gt_customer     TYPE STANDARD TABLE OF /vpcoe/cl_rdp_customer_data=>gty_s_customer.

DATA:
  lv_ucomm           TYPE sy-ucomm,
  gv_runid           TYPE /vpcoe/de_run_id,
  gv_api_type        TYPE /vpcoe/de_api_type,
  gv_customer_role   TYPE /vpcoe/de_customer_role,
  gs_sel_opt         TYPE /vpcoe/s_selopt_customer,
  gs_sel_opt_ext     TYPE /vpcoe/cl_rdp_customer_data=>gty_s_sel_opt_ext,
  go_payload_handler TYPE REF TO /vpcoe/cl_rdp_payload_handler,
  gv_default         TYPE string VALUE /vpcoe/cl_rdp_helper=>sc_service_id-customer.
