class /VPCOE/CL_RDP_SUPINVI_DATA definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_code_list_category,
             category    TYPE c LENGTH 1,
             explanation TYPE c LENGTH 30,
           END OF gty_s_code_list_category .

  methods CONSTRUCTOR
    importing
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND .
  methods PROCESS_SUPPLIER_INV_ITEMS
    importing
      !IV_CODE type /VPCOE/DE_MAT_DOC_VARIANTS optional
      !IV_FILE_PATH type STRING optional
      !IV_SAVE_BACKGROUND type ABAP_BOOL optional
      !IS_SEL_OPT type /VPCOE/S_SELOPT_SUPINVI optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IV_SEND type ABAP_BOOL optional
    exporting
      !ET_SUPINVI type /VPCOE/T_SUPINVI_DATA
    changing
      !CT_TOTAL_COUNT_SUPPINV type /VPCOE/TT_LOG_SUM optional .
  class-methods DOWNLOAD_EXCEL
    importing
      !IT_DATA type /VPCOE/T_SUPINVI_DATA
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
    changing
      !CO_XLS_HANDLER type ref to /VPCOE/CL_XLS_HANDLER .
  methods DELETE_EXPIRED .
  methods BUILD_JSON
    importing
      !IT_SUPINVI type /VPCOE/T_SUPINVI_DATA
    exporting
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
protected section.

  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SOURCE type STRING .
  data MV_MODE type /VPCOE/DE_MODE .
  data MO_CUST type ref to /VPCOE/CL_RDP_HELPER .
  data MO_LOG type ref to /VPCOE/CL_RDP_LOG .
  data MV_SESSION_ITEM type INT4 .
  data MV_SESSION_ID type RAW16 .
private section.

  methods SEND_JSON .
  class-methods GET_EXCEL_HEADER
    exporting
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE .
ENDCLASS.



CLASS /VPCOE/CL_RDP_SUPINVI_DATA IMPLEMENTATION.


METHOD build_json.
  DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

  DATA: lt_name_mappings TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings.

  INSERT VALUE #( abap = 'SUPPLIER_INVOICE_ID' json = 'supplierInvoiceIDByInvgParty' ) INTO TABLE lt_name_mappings.

  APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
  <ls_json>-count = lines( it_supinvi ).
  <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data = VALUE /vpcoe/str_supinvi_json( source   = me->mv_source
                                                                                                                elements = it_supinvi )
                                                                       iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                       it_name_mappings = lt_name_mappings ).

  REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.

  me->mv_session_item = me->mv_session_item + 1.

  GET BADI lo_badi.
  DATA(lv_srv_grp) = me->mo_cust->get_srv_grp( ).
  DATA(lv_srvid) = me->mo_cust->get_srv_id( ).
  CALL BADI lo_badi->adjust_json
    EXPORTING
      iv_srv_grp  = lv_srv_grp
      iv_srv_id   = lv_srvid
      iv_api_type = me->mv_api_type
      iv_level    = /vpcoe/cl_common_helper=>sc_level-mtrl_suppinv
    CHANGING
      ct_json     = et_json.

  CALL FUNCTION '/VPCOE/STORE_JSON_BCKGRND'
    EXPORTING
      iv_api_type     = me->mv_api_type
      iv_srv_grp      = me->mo_cust->get_srv_grp( )
      iv_srv_id       = me->mo_cust->get_srv_id( )
      iv_session_id   = me->mv_session_id
      iv_session_item = me->mv_session_item
      is_json         = <ls_json>.

ENDMETHOD.


  METHOD CONSTRUCTOR.

    me->mv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp.

    me->mo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = me->mv_api_type
                                            iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-supinvi
                                            iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-supinvi ).

    me->mv_package_size = me->mo_cust->get_package_size( ).
    me->mv_source       = /vpcoe/cl_rdp_helper=>get_source_id( ).
    me->mv_mode         = iv_mode.

  ENDMETHOD.


  METHOD delete_expired.
    DATA lv_expire_date TYPE sy-datum.

    lv_expire_date = sy-datum - 10.

*   Delete obsoleted entries or fully processed
    DELETE FROM /vpcoe/billdi
      WHERE added_on < lv_expire_date.

    IF sy-subrc = 0.
      MESSAGE 'Obsoleted entries were deleted' TYPE 'I' DISPLAY LIKE 'S'.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.


  METHOD download_excel.
    DATA: r_table   TYPE REF TO data,
          lt_header TYPE /vpcoe/cl_xls_handler=>gty_t_title.

    FIELD-SYMBOLS: <lt_data> TYPE any.

    CREATE DATA r_table LIKE it_data.
    ASSIGN r_table->* TO <lt_data>.
    <lt_data> = it_data.

    get_excel_header(
      IMPORTING
        et_header     = lt_header ).

    ASSIGN r_table->* TO <lt_data>.

    co_xls_handler->execute( iv_name = CONV #( 'SupplierInvoiceItem' ) ).

    co_xls_handler->fill_data( it_item_tab = <lt_data>
                            it_title    = lt_header ).

  ENDMETHOD.


  METHOD get_excel_header.
    CLEAR: et_header.

    et_header = VALUE #( ( description      = 'Supplier Invoice'
                               internal_name    = 'supplierInvoice'
                               data_type        = 'CHAR(10)'
                               mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                               s4hana_attribute = 'RBKP-BELNR'
                               vpcoe_attribute  = 'SUPPLIER_INVOICE'
                               is_key           = abap_true )
                             ( description      = 'Supplier Invoice Item'
                               internal_name    = 'supplierInvoiceItem'
                               data_type        = 'CHAR(6)'
                               mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                               s4hana_attribute = 'RSEG-BUZEI'
                               vpcoe_attribute  = 'SUPPLIER_INVOICE_ITEM'
                               is_key           = abap_true )
                             ( description      = 'Fiscal Year'
                               internal_name    = 'fiscalYear'
                               data_type        = 'CHAR(4)'
                               mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                               s4hana_attribute = 'RBKP-GJAHR'
                               vpcoe_attribute  = 'FISCAL_YEAR'
                               is_key           = abap_true )
                             ( description      = 'Document Date'
                               internal_name    = 'documentDate'
                               data_type        = 'Date'
                               mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                               s4hana_attribute = 'RBKP-BLDAT'
                               vpcoe_attribute  = 'DOCUMENT_DATE'
                               is_key           = abap_false )
                             ( description      = 'Supplier Invoicing ID'
                               internal_name    = 'supplierInvoiceIDByInvcgParty'
                               data_type        = 'CHAR(16)'
                               mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                               s4hana_attribute = 'RBKP-XBLNR'
                               vpcoe_attribute  = 'SUPPLIER_INVOICE_ID'
                               is_key           = abap_false )
                             ( description      = 'Purchase Order'
                               internal_name    = 'purchaseOrder'
                               data_type        = 'CHAR(10)'
                               mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                               s4hana_attribute = 'RSEG-EBELN'
                               vpcoe_attribute  = 'PURCHASE_ORDER'
                               is_key           = abap_false )
                            (  description      = 'Purchase Order Item'
                               internal_name    = 'purchaseOrderItem'
                               data_type        = 'CHAR(5)'
                               mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                               s4hana_attribute = 'RSEG-EBELP'
                               vpcoe_attribute  = 'PURCHASE_ORDER_ITEM'
                               is_key           = abap_false ) ).

  ENDMETHOD.


METHOD process_supplier_inv_items.
  DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

  DATA: lt_supinvi           TYPE /vpcoe/t_supinvi_data,
        lt_supinvi_pack      TYPE /vpcoe/t_supinvi_data,
        lt_mat_doc           TYPE SORTED TABLE OF /vpcoe/billdi WITH NON-UNIQUE KEY mat_doc,
        lt_material_document TYPE SORTED TABLE OF /vpcoe/billdi WITH NON-UNIQUE KEY mat_doc,
        lv_total             TYPE /vpcoe/str_log_sum-total,
        lv_total_failed      TYPE /vpcoe/str_log_sum-total_failed,
        lv_skip              TYPE xfeld,
        lv_expire_date       TYPE sy-datum,
        lv_pack_size         TYPE i,
        lv_act_package       TYPE i,
        lv_num_packages      TYPE i,
        lv_record_cnt        TYPE i,
        lv_top               TYPE i.

  GET BADI lo_badi.

  DATA(lv_srvgrp) = me->mo_cust->get_srv_grp( ).
  DATA(lv_srvid) =  me->mo_cust->get_srv_id( ).


  CALL BADI lo_badi->skip_selection
    EXPORTING
      iv_srv_grp  = lv_srvgrp
      iv_srv_id   = lv_srvid
      iv_api_type = me->mv_api_type
      iv_level    = /vpcoe/cl_common_helper=>sc_level-mtrl_suppinv
      is_sel_opt  = is_sel_opt
      iv_mode     = me->mv_mode
      io_log      = io_log
    CHANGING
      cv_skip     = lv_skip.

  IF lv_skip = abap_false.

    DATA(lv_db_connection) = /vpcoe/cl_common_helper=>get_db_connection(
      iv_srv_grp  = lv_srvgrp
      iv_srv_id   = lv_srvid
      iv_api_type = me->mv_api_type
      iv_level    = /vpcoe/cl_common_helper=>sc_level-mtrl_suppinv
      is_sel_opt  = is_sel_opt ).

* Get MatDocs from the MatDoc report run
    IF is_sel_opt-supinvi_session_id IS NOT INITIAL.
      SELECT *
        INTO TABLE lt_material_document
         FROM /vpcoe/billdi
           WHERE /vpcoe/billdi~session_id IN is_sel_opt-supinvi_session_id
            AND ( ( /vpcoe/billdi~code = /vpcoe/cl_rdp_material_doc=>sc_variants-gr_po AND /vpcoe/billdi~ship_from_country IN is_sel_opt-supinvi_country AND ship_to_country IN is_sel_opt-supinvi_country )
             OR ( /vpcoe/billdi~code = /vpcoe/cl_rdp_material_doc=>sc_variants-gr_po_imp AND /vpcoe/billdi~ship_from_country <> /vpcoe/billdi~ship_to_country AND ship_to_country IN is_sel_opt-supinvi_country ) ) ."ship_from_country NOT IN
*is_sel_opt-supinvi_country
      IF sy-subrc <> 0.
        MESSAGE i080(/vpcoe/common) WITH iv_code INTO io_log->sv_msg_text.
        io_log->add_sy_msg( ).
        RETURN.
      ENDIF.
    ENDIF.

    ASSIGN ct_total_count_suppinv[ sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-supinvi ] TO FIELD-SYMBOL(<ls_sum>).
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO  ct_total_count_suppinv ASSIGNING <ls_sum>.
      <ls_sum>-sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-supinvi.
    ENDIF.

    IF lt_material_document IS INITIAL.
      RETURN.
    ENDIF.

    me->mv_session_id = me->mo_cust->generate_session_id( ).

    lv_pack_size = mv_package_size * 10.

    lv_record_cnt = lines( lt_material_document ).

    IF lv_record_cnt MOD lv_pack_size = 0.
      lv_num_packages = lv_record_cnt DIV lv_pack_size.
    ELSE.
      lv_num_packages = ( lv_record_cnt DIV lv_pack_size ) + 1.
    ENDIF.
    IF lv_num_packages = 0.
      lv_num_packages = 1.
    ENDIF.

    lv_act_package = 1.

    WHILE lv_act_package <= lv_num_packages.
      lv_top = ( lv_act_package - 1 ) * lv_pack_size + 1.

      LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>) FROM lv_top.
        INSERT <ls_material_document> INTO TABLE lt_mat_doc.
        IF lines( lt_mat_doc ) = lv_pack_size.
          EXIT.
        ENDIF.
      ENDLOOP.

      SELECT rbkp~belnr AS supplier_invoice,
             rseg~buzei AS supplier_invoice_item,
             rbkp~gjahr AS fiscal_year,
             rbkp~bldat AS document_date,
             rbkp~xblnr AS supplier_invoice_id,
             rseg~ebeln AS purchase_order,
             rseg~ebelp AS purchase_order_item
*             rbkp~stblg AS reversal_document,
*             rbkp~stjah AS reversal_documenr_year
         FROM rbkp INNER JOIN rseg ON rbkp~belnr = rseg~belnr
                                   AND rbkp~gjahr = rseg~gjahr
        INTO CORRESPONDING FIELDS OF TABLE @lt_supinvi
         CONNECTION (lv_db_connection)
              PACKAGE SIZE @me->mv_package_size
         FOR ALL ENTRIES IN @lt_mat_doc
        WHERE rbkp~belnr IN  @is_sel_opt-supplier_invoice
          AND rseg~buzei IN  @is_sel_opt-supplier_invoice_item
          AND rbkp~gjahr IN  @is_sel_opt-fiscal_year
          AND rseg~ebeln = @lt_mat_doc-purchase_order
          AND rbkp~rbstat = '5' OR rbkp~rbstat = '0'
          AND rbkp~bukrs IN @is_sel_opt-comp_code
          AND rseg~werks IN @is_sel_opt-plant
          AND rseg~ebelp = @lt_mat_doc-purchase_order_item.

        CALL BADI lo_badi->adjust_data_retrieval
          EXPORTING
            iv_srv_grp  = lv_srvgrp
            iv_srv_id   = lv_srvid
            iv_api_type = me->mv_api_type
            iv_level    = /vpcoe/cl_common_helper=>sc_level-mtrl_suppinv
            is_sel_opt  = is_sel_opt
            iv_mode     = me->mv_mode
            io_log      = io_log
          CHANGING
            ct_data     = lt_supinvi.

        IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND iv_send = abap_true.
          me->build_json( lt_supinvi ).
        ELSEIF lines( et_supinvi ) < /vpcoe/cl_rdp_helper=>gc_display_amount.
          et_supinvi = CORRESPONDING #( BASE ( et_supinvi ) lt_supinvi ).
        ELSE.
          lv_act_package = lv_num_packages.
          EXIT.
        ENDIF.

      ENDSELECT.

      lv_act_package = lv_act_package + 1.
      CLEAR lt_mat_doc.
    ENDWHILE.

    IF et_supinvi IS INITIAL AND me->mv_mode <> /vpcoe/cl_rdp_helper=>sc_mode-send.
      MESSAGE i080(/vpcoe/common) WITH iv_code INTO io_log->sv_msg_text.
      io_log->add_sy_msg( ).
      RETURN.
    ENDIF.

  ELSE.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = lv_srvgrp
        iv_srv_id   = lv_srvid
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-mtrl_suppinv
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        ct_data     = lt_supinvi.

    LOOP AT lt_supinvi ASSIGNING FIELD-SYMBOL(<ls_supinvi>).
      INSERT <ls_supinvi> INTO TABLE lt_supinvi_pack.
      IF lines( lt_supinvi_pack ) = me->mv_package_size.
        IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND iv_send = abap_true.
          me->build_json( lt_supinvi ).
        ELSE.
          et_supinvi = lt_supinvi.
          RETURN.
        ENDIF.

        CLEAR lt_supinvi_pack.
      ENDIF.
    ENDLOOP.

    IF lt_supinvi_pack IS NOT INITIAL.
      IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND iv_send = abap_true.
        me->build_json( lt_supinvi ).
      ELSE.
        et_supinvi = lt_supinvi.
        RETURN.
      ENDIF.

    ENDIF.
  ENDIF.

  IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND iv_send = abap_true.
*   Mark Entries as Processed
    LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_mat_doc>).
      IF <ls_mat_doc>-session_id IS NOT INITIAL.
        <ls_mat_doc>-supinvi = abap_true.
      ENDIF.
    ENDLOOP.

    MODIFY /vpcoe/billdi FROM TABLE lt_mat_doc.
    CALL FUNCTION 'DB_COMMIT'.

    lv_expire_date = sy-datum - 10.

*   Delete obsoleted entries or fully processed
    DELETE FROM /vpcoe/billdi
      WHERE ( billdi = abap_true AND supinvi = abap_true )
         OR added_on < lv_expire_date.

*  Send Supplier Invoices Items
    NEW /vpcoe/cl_rdp_payload_handler( )->send_payload_multi(
      EXPORTING
        it_r_session_ids = VALUE #( ( sign = 'I' option = 'EQ' low = me->mv_session_id ) )
        iv_api_type      = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
        iv_srv_id        = me->mo_cust->get_srv_id( )
        io_log           = io_log
        io_cust          = me->mo_cust
      CHANGING
        cv_total         = <ls_sum>-total
        cv_total_failed  = <ls_sum>-total_failed ).

  ENDIF.

ENDMETHOD.


method SEND_JSON.

*  DATA(lv_url) = me->mo_cust->get_service_url( IMPORTING et_bapiret2 = gt_bapiret2 ).
*  lo_log->add_bapiret( EXPORTING it_bapiret2_t = gt_bapiret2 ).
*
*  IF lo_log->check( ).
*    IF sy-batch = space.
*      lo_log->display_message( ).
*    ENDIF.
*    lo_log->save( ).
*    RETURN.
*  ENDIF.

  endmethod.
ENDCLASS.
