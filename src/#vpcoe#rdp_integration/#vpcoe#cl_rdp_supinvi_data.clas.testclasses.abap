CLASS /vpcoe/cl_rdp_supinvi_data_ut DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    DATA: f_cut   TYPE REF TO /vpcoe/cl_rdp_supinvi_data,
          lo_cust TYPE REF TO /vpcoe/cl_rdp_helper.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: process_supplier_inv_items FOR TESTING.
    METHODS: build_json FOR TESTING.

ENDCLASS.


CLASS /vpcoe/cl_rdp_supinvi_data_ut IMPLEMENTATION.

  DEFINE add_supinvi_expected.
    APPEND VALUE #( supplier_invoice      = &1
                    supplier_invoice_item = &2
                    fiscal_year           = &3
                    document_date         = &4
                    supplier_invoice_id   = &5
                    purchase_order        = &6
                    purchase_order_item   = &7 ) TO lt_supinvi_exp.
  END-OF-DEFINITION.

  METHOD class_setup.

  ENDMETHOD.


  METHOD class_teardown.

  ENDMETHOD.


  METHOD setup.

    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-supinvi
                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-supinvi ).

    f_cut = NEW /vpcoe/cl_rdp_supinvi_data( iv_mode = /vpcoe/cl_common_helper=>sc_mode-document ).

  ENDMETHOD.


  METHOD teardown.

  ENDMETHOD.

  METHOD process_supplier_inv_items.

    DATA: lt_supinvi_act     TYPE /vpcoe/t_supinvi_data,
          lt_supinvi_exp     TYPE /vpcoe/t_supinvi_data,
          ls_sel_opt         TYPE /vpcoe/s_selopt_supinvi,
          lt_billdi          TYPE STANDARD TABLE OF /vpcoe/billdi,
          lv_count           TYPE i,
          lt_session_id      TYPE /vpcoe/cl_rdp_payload_handler=>gty_r_sesion_id,
          lt_total_count_act TYPE /vpcoe/tt_log_sum,
          lt_total_count_exp TYPE /vpcoe/tt_log_sum.

    lt_billdi = VALUE #( ( session_id          = /vpcoe/cl_common_helper=>generate_session_id( )
                           mat_doc             = '5000000070'
                           added_on            = '20250502'
                           purchase_order_item = '00010'
                           purchase_order      = '4500000069'
                           code                = 'GR_PO'
                           ship_to_country     = 'DE'
                           ship_from_country   = 'DE' )
                         ( session_id          = /vpcoe/cl_common_helper=>generate_session_id( )
                           mat_doc             = '5000000001'
                           added_on            = '20250502'
                           purchase_order_item = '00010'
                           purchase_order      = '4500000001'
                           code                = 'GR_PO'
                           ship_to_country     = 'US'
                           ship_from_country   = 'US' )
                         ( session_id          = /vpcoe/cl_common_helper=>generate_session_id( )
                           mat_doc             = '5000000002'
                           added_on            = '20250502'
                           purchase_order_item = '00010'
                           purchase_order      = '4500000002'
                           code                = 'GR_PO'
                           ship_to_country     = 'US'
                           ship_from_country   = 'US' )
                           ).

    MODIFY /vpcoe/billdi FROM TABLE lt_billdi.
    CALL FUNCTION 'DB_COMMIT'.

    LOOP AT lt_billdi ASSIGNING FIELD-SYMBOL(<ls_billdi>).
      INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_billdi>-session_id ) INTO TABLE ls_sel_opt-supinvi_session_id.
    ENDLOOP.

    INSERT VALUE #( sign = 'I' option = 'EQ' low = '5105600111' ) INTO TABLE ls_sel_opt-supplier_invoice.
    INSERT VALUE #( sign = 'I' option = 'EQ' high = '2021' ) INTO TABLE ls_sel_opt-fiscal_year.

    lt_total_count_exp = VALUE #( ( total = 1 total_failed = 0 sub_object = 'SUPPINVI' ) ).

    add_supinvi_expected `5105600111` `000001` `2021` `20211105` `4500000069` `4500000069` `00010`.

    f_cut->process_supplier_inv_items(
      EXPORTING
        is_sel_opt      = ls_sel_opt
        io_log          = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-supinvi )
*        iv_send         = abap_true
        iv_code         = /vpcoe/cl_rdp_material_doc=>sc_variants-gr_po
     IMPORTING
       et_supinvi       = lt_supinvi_act
     CHANGING
       ct_total_count_suppinv = lt_total_count_act ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_supinvi_act
      exp   = lt_supinvi_exp ).

*    cl_abap_unit_assert=>assert_equals(
*     act   = lt_total_count_act
*     exp   = lt_total_count_exp ).

  ENDMETHOD.


  METHOD build_json.

    DATA: lt_json_exp    TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_json_act    TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_supinvi_exp TYPE /vpcoe/t_supinvi_data.

    add_supinvi_expected `5105600111` `000001` `2021` `20211105` `4500000069` `4500000069` `00010`.

    f_cut->build_json(
      EXPORTING
        it_supinvi = lt_supinvi_exp
      IMPORTING
        et_json = lt_json_act ).

    lt_json_exp = VALUE #( ( elements = `{"source":"ECC","elements":` &&
                                        `[{"supplierInvoice":"5105600111","supplierInvoiceItem":1,"fiscalYear":2021,` &&
                                        `"documentDate":"2021-11-05","supplierInvoiceIDByInvgParty":"4500000069",` &&
                                        `"purchaseOrder":"4500000069","purchaseOrderItem":10}]}`
                             count = 1 ) ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json_act
      exp   = lt_json_exp ).

  ENDMETHOD.

ENDCLASS.
