*&---------------------------------------------------------------------*
*& Report  /vpcoe/upd_matl_exp_data
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

* This report allows to update database table /vpcoe/matl_exp in following ways:
* 1. update values manually:
*    please fill lt_matl_exp table in the code, run the program and select correcponding radio button
* 2. transport values from another system (default - BP7) via RFC:
*    please run the program, select corresponding radio button, click Enter and provide RFC destination

REPORT /vpcoe/upd_matl_exp_data.

INCLUDE /vpcoe/version.

SELECTION-SCREEN COMMENT /1(50) text-001.

PARAMETERS : p_upd RADIOBUTTON GROUP rad,
             p_tr  RADIOBUTTON GROUP rad.

PARAMETERS : p_dest LENGTH 20 DEFAULT 'BP7CLNT217RFC' MODIF ID id1.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_tr = abap_false AND screen-group1 = 'ID1'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  INCLUDE /vpcoe/version_set.

START-OF-SELECTION.

*************************************************************************** 1. UPDATE DATA MANUALLY
  DATA: lt_matl_exp TYPE STANDARD TABLE OF /vpcoe/matl_exp.

  IF p_upd = abap_true.

    lt_matl_exp = VALUE #( ). " Insert your values here. See record template below:
    " Note: replace 'year' field name with 'mat_doc_year'

*                              ( id                             = '5000000070'
*                                item                           = 1
*                                line                           = 1
*                                mat_doc_year                   = 2021 " instead of 'year'
*                                posting_date                   = '20211104'
*                                creation_date                  = '20221004'
*                                creation_time                  = '235239'
*                                plant                          = 'DE01'
*                                plant_country                  = 'DE'
*                                issuing_or_receiving_plant     = ''
*                                issuing_or_receiving_plnt_cntr = ''
*                                stock_material                 = 'RAW01'
*                                inventory_stock_type           = ''
*                                company_code                   = 'DE01'
*                                base_unit_of_measure           = 'EA'
*                                quantity_in_base_unit          = '1.000'
*                                materialstockchangeqtybaseunit = '1.000'
*                                entry_unit                     = 'EA'
*                                quantity_in_entry_unit         = '1.000'
*                                invtry_mgmt_reference_document = '5000000070'
*                                reference_document             = ''
*                                sales_order                    = ''
*                                purchase_order                 = '4500000069'
*                                production_order               = ''
*                                delivery                       = ''
*                                is_reversal_movement_type      = '-'
*                                goods_movement_type            = '101'
*                                product                        = 'RAW01'
*                                issg_or_rcvg_product_id        = ''
*                                batch                          = '0000000090'
*                                manufacture_date               = '00000000'
*                                issg_or_rcvg_batch_id          = ''
*                                supplier                       = 'PLANT DE01'
*                                supplier_country               = 'DE'
*                                customer                       = ''
*                                customer_country               = ''
*                                goods_recipient_name           = ''
*                                reference_document_type        = 'B'
*                                accounting_document_type       = ''
*                                transaction_type               = 'WE'
*                                stock_change_category          = 'GR'
*                                is_cross_plant_transfer        = 'F'
*                                logistic_process               = 'GR_PO'
*                                incoterms                      = ''
*                                lgort                          = 'DE01'
*                                ship_to_country                = 'DE'
*                                ship_to_region                 = '14'
*                                ship_from_country              = 'DE'
*                                vendor_acc_number              = 'PLANT DE01'
*                                plant_region                   = '01'
*                                category                       = '0' )

    MODIFY /vpcoe/matl_exp FROM TABLE lt_matl_exp.

    IF sy-subrc = 0.
      WRITE / | Table /vpcoe/matl_exp updated successfully |.
    ELSE.
      WRITE / | Modify of /vpcoe/matl_exp failed |.
    ENDIF.

  ENDIF.

*************************************************************************** 2. TRANSPORT DATA BY RFC
  IF p_tr = abap_true.

    CALL FUNCTION '/VPCOE/RDP_READ_MATL_EXP_DATA'
      DESTINATION p_dest "'BP7CLNT217RFC'
      TABLES
        tt_matl_exp           = lt_matl_exp
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc = 0.
      MODIFY /vpcoe/matl_exp FROM TABLE lt_matl_exp.
      IF sy-subrc = 0.
        WRITE / |Data was transported successfully. { lines( lt_matl_exp ) } records updated|.
      ELSE.
        WRITE / |Data transportation failed. System code: { sy-subrc }|.
      ENDIF.
    ELSE.
      DATA: lv_error_msg TYPE string.
      CASE sy-subrc.
        WHEN 1.
          lv_error_msg = 'communication_failure'.
        WHEN 2.
          lv_error_msg = 'system_failure'.
        WHEN 3.
          lv_error_msg = 'others'.
      ENDCASE.
      WRITE / |RFC destination is invalid. Error reason: { lv_error_msg }|.
    ENDIF.

  ENDIF.
