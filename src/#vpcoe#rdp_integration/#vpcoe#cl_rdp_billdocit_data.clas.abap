class /VPCOE/CL_RDP_BILLDOCIT_DATA definition
  public
  create public .

public section.

  types:
    gty_doc TYPE SORTED TABLE OF /vpcoe/billdi WITH NON-UNIQUE KEY mat_doc .
  types:
    BEGIN OF gty_s_code_list_category,
        category    TYPE c LENGTH 1,
        explanation TYPE c LENGTH 30,
      END OF gty_s_code_list_category .

  methods CONSTRUCTOR
    importing
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND .
  methods PROCESS_BILLING_DOC_ITEMS
    importing
      !IV_SERVICE_ID type /VPCOE/DE_SERVICE_ID optional
      !IV_SAVE_BACKGROUND type ABAP_BOOL optional
      !IV_FILE_PATH type STRING optional
      !IV_CODE type /VPCOE/DE_MAT_DOC_VARIANTS optional
      !IS_SEL_OPT type /VPCOE/S_SELOPT_BILLDI optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IV_SEND type ABAP_BOOL optional
    exporting
      !ET_BILLDI type /VPCOE/T_BILLDI_DATA
    changing
      !CT_TOTAL_COUNT_BILLDOC type /VPCOE/TT_LOG_SUM optional .
  class-methods DOWNLOAD_EXCEL
    importing
      !IT_BILL_DOC type /VPCOE/T_BILLDI_DATA
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
    changing
      !CO_XLS_HANDLER type ref to /VPCOE/CL_XLS_HANDLER .
  methods DELETE_EXPIRED .
  methods BUILD_AND_STORE_JSON
    importing
      !IT_BILLDI type /VPCOE/T_BILLDI_DATA
      !IV_LEVEL type /VPCOE/LEVEL
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
  methods BUILD_JSON
    importing
      !IT_BILLDI type /VPCOE/T_BILLDI_DATA
      !IV_LEVEL type /VPCOE/LEVEL
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
    exporting
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods CLOSE_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
protected section.

  data MV_SRVID type /VPCOE/DE_SERVICE_ID .
  data MV_SRVGRP type /VPCOE/DE_SERVICE_GROUP .
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
  class-methods GET_CODE_LIST
    exporting
      !ET_CODE_LIST type /VPCOE/CL_XLS_HANDLER=>GTY_T_CODELIST .
  class-methods GET_EXCEL_HEADER
    importing
      !IT_BILL_DOC type /VPCOE/T_BILLDI_DATA
      !IV_SHEET_NAME type CHAR40
    exporting
      !EV_HEADER type STRING
      !EV_CODE_LIST type STRING
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_DATA type DATA .
  class-methods GET_EXCEL_SHEET
    exporting
      !ET_SHEETS type /VPCOE/TT_SHEETS_STR .
ENDCLASS.



CLASS /VPCOE/CL_RDP_BILLDOCIT_DATA IMPLEMENTATION.


METHOD build_and_store_json.

  me->build_json(
    EXPORTING
      it_billdi = it_billdi
      iv_level  = iv_level
      io_log    = io_log
    IMPORTING
      et_json   = DATA(lt_json) ).

  LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>).
    CALL FUNCTION '/VPCOE/STORE_JSON_BCKGRND'
      EXPORTING
        iv_api_type     = me->mv_api_type
        iv_srv_grp      = me->mv_srvgrp
        iv_srv_id       = me->mv_srvid
        iv_session_id   = me->mv_session_id
        iv_session_item = me->mv_session_item
        is_json         = <ls_json>.
  ENDLOOP.

ENDMETHOD.


METHOD build_json.
  DATA: lo_badi          TYPE REF TO /vpcoe/adjust_data_retrieval,
        lt_name_mappings TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings.

  CLEAR et_json.

  INSERT VALUE #( abap = 'REFERENCESDDOC' json = 'referenceSDDocument' ) INTO TABLE lt_name_mappings.
  INSERT VALUE #( abap = 'REFERENCESDDOCITEM' json = 'referenceSDDocumentItem' ) INTO TABLE lt_name_mappings.
  INSERT VALUE #( abap = 'REFERENCESDDOCCAT' json = 'referenceSDDocumentCategory' ) INTO TABLE lt_name_mappings.

  APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
  <ls_json>-count = lines( it_billdi ).
  <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                                EXPORTING is_data        = VALUE /vpcoe/str_billdi_json( source   = me->mv_source
                                                                                                         elements = it_billdi )
                                                          iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                          it_name_mappings = lt_name_mappings ).

  REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.

  me->mv_session_item = me->mv_session_item + 1.

  GET BADI lo_badi.
  CALL BADI lo_badi->adjust_json
    EXPORTING
      iv_srv_grp  = me->mv_srvgrp
      iv_srv_id   = me->mv_srvid
      iv_api_type = me->mv_api_type
      iv_level    = iv_level
    CHANGING
      ct_json     = et_json.

ENDMETHOD.


METHOD close_replication.

* Should be redefined

  RETURN.

ENDMETHOD.


  METHOD constructor.

    me->mv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp.

    me->mo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = me->mv_api_type
                                            iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-billdocit
                                            iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-billdocit ).

    me->mv_package_size = me->mo_cust->get_package_size( ).
    me->mv_srvgrp       = me->mo_cust->get_srv_grp( ).
    me->mv_srvid       =  me->mo_cust->get_srv_id( ).
    me->mv_source       = /vpcoe/cl_rdp_helper=>get_source_id( ).
    me->mv_mode         = iv_mode.

  ENDMETHOD.


  METHOD delete_expired.
    DATA lv_expire_date TYPE sy-datum.

    lv_expire_date = sy-datum - 10.

* Delete obsoleted entries or fully processed
    DELETE FROM /vpcoe/billdi
      WHERE added_on < lv_expire_date.

    IF sy-subrc = 0.
      MESSAGE 'Obsoleted entries were deleted' TYPE 'I' DISPLAY LIKE 'S'.
      COMMIT WORK.
    ELSE.
      MESSAGE 'Nothing to delete' TYPE 'I' DISPLAY LIKE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD download_excel.

    DATA: r_table   TYPE REF TO data,
          lt_header TYPE /vpcoe/cl_xls_handler=>gty_t_title.

    FIELD-SYMBOLS: <lt_data> TYPE any.

   get_excel_sheet(
      IMPORTING
        et_sheets = DATA(lt_sheets) ).

*    DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path = iv_file_path
*                                                   iv_save_background = iv_save_background  ).

    LOOP AT lt_sheets ASSIGNING FIELD-SYMBOL(<ls_sheets>).

      get_excel_header(
        EXPORTING
          iv_sheet_name = <ls_sheets>
          it_bill_doc = it_bill_doc
        IMPORTING
          et_header     = lt_header
          et_data       = r_table ).

      ASSIGN r_table->* TO <lt_data>.

      co_xls_handler->execute( iv_name = CONV #( <ls_sheets> ) ).

      CASE <ls_sheets>.
        WHEN 'Code list Document Category'.
          co_xls_handler->fill_data( it_item_tab = <lt_data>
                                  it_title    = lt_header
                                  iv_code_list = 'SD Document Category'
                                  iv_style     = /vpcoe/cl_xls_handler=>sc_styles-code_list_style ).
        WHEN 'BillingDocumentItem'.
          co_xls_handler->fill_data( it_item_tab = <lt_data>
                                  it_title    = lt_header ).
      ENDCASE.
    ENDLOOP.

*    co_xls_handler->save_xls_file( io_log = io_log ).
  ENDMETHOD.


  METHOD get_code_list.
    CLEAR: et_code_list.

    SELECT domvalue_l AS id, ddtext AS description
      FROM dd07t
      WHERE domname  = 'VBTYP'
        AND ddlanguage = 'E'
    INTO TABLE @et_code_list.

  ENDMETHOD.


  METHOD get_excel_header.
    CLEAR: ev_code_list, ev_header, et_header, et_data.

    TYPES:
      BEGIN OF ty_s_billdoc_with_furmula,
        formula TYPE string.
            INCLUDE TYPE /vpcoe/s_billdi_data.
    TYPES:
       END OF ty_s_billdoc_with_furmula.

    DATA: lt_code_list TYPE /vpcoe/cl_xls_handler=>gty_t_codelist,
          lt_bill_doc  TYPE SORTED TABLE OF ty_s_billdoc_with_furmula WITH NON-UNIQUE KEY formula.

    FIELD-SYMBOLS <lt_data> TYPE any.
    CASE iv_sheet_name.
      WHEN 'Code list Document Category'.
        et_header = VALUE #( ( description = 'Code'        vpcoe_attribute = 'ID' )
                             ( description = 'Description' vpcoe_attribute = 'DESCRIPTION' ) ).

        CREATE DATA et_data LIKE lt_code_list.
        ASSIGN et_data->* TO <lt_data>.
        get_code_list( IMPORTING et_code_list = lt_code_list ).
        <lt_data> = lt_code_list.

      WHEN 'BillingDocumentItem'.
        et_header = VALUE #( ( description      = 'Billing Document'
                                internal_name    = 'billingDocument'
                                data_type        = 'CHAR(10)'
                                mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                                s4hana_attribute = 'VBRP-VBELN'
                                vpcoe_attribute  = 'BILLING_DOCUMENT'
                                is_key           = abap_true )
                              ( description      = 'Billing Document Item'
                                internal_name    = 'billingDocumentItem'
                                data_type        = 'CHAR(6)'
                                mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                                s4hana_attribute = 'VBRP-POSNR'
                                vpcoe_attribute  = 'BILLING_DOCUMENT_ITEM'
                                is_key           = abap_true )
                              ( description      = 'Billing Document Date'
                                internal_name    = 'billingDocumentDate'
                                data_type        = 'Date'
                                mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                                s4hana_attribute = 'VBRP-FKDAT_ANA'
                                vpcoe_attribute  = 'BILLING_DOCUMENT_DATE'
                                is_key           = abap_false )
                              ( description      = 'Reference SD Document'
                                internal_name    = 'referenceSDDocument'
                                data_type        = 'CHAR(10)'
                                mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                                s4hana_attribute = 'VBRP-VGBEL'
                                vpcoe_attribute  = 'REFERENCESDDOC'
                                is_key           = abap_false )
                              ( description      = 'Reference SD Document Item'
                                internal_name    = 'referenceSDDocumentItem'
                                data_type        = 'CHAR(6)'
                                mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                                s4hana_attribute = 'VBRP-VGPOS'
                                vpcoe_attribute  = 'REFERENCESDDOCITEM'
                                is_key           = abap_false )
                              ( description      = 'Reference SD Document Category'
                                internal_name    = 'referenceSDDocumentCategory'
                                data_type        = 'CHAR(4)'
                                mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                                s4hana_attribute = 'VBRP-VGTYP'
                                vpcoe_attribute  = 'REFERENCESDDOCCAT'
                                is_key           = abap_false )
                              ( description      = ''
                                internal_name    = ''
                                data_type        = ''
                                mandatory        = ''
                                s4hana_attribute = ''
                                vpcoe_attribute  = 'FORMULA') ).

        CREATE DATA et_data LIKE lt_bill_doc.
        ASSIGN et_data->* TO <lt_data>.

        lt_bill_doc = VALUE #( FOR <ls_bill_doc> IN it_bill_doc
                               ( billing_document = <ls_bill_doc>-billing_document
                                 billing_document_date = <ls_bill_doc>-billing_document_date
                                 billing_document_item = <ls_bill_doc>-billing_document_item
                                 referencesddoc = <ls_bill_doc>-referencesddoc
                                 referencesddoccat = <ls_bill_doc>-referencesddoccat
                                 referencesddocitem = <ls_bill_doc>-referencesddocitem
                                 formula = `=VLOOKUP(RC[-1],'Code list Document Category'!R1C1:R100C2,2,FALSE)` ) ).

        <lt_data> = lt_bill_doc.
    ENDCASE.
  ENDMETHOD.


  METHOD get_excel_sheet.
    CLEAR: et_sheets.

    et_sheets = VALUE #( ( 'BillingDocumentItem' ) ( 'Code list Document Category' ) ).

  ENDMETHOD.


METHOD process_billing_doc_items.
  DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

  DATA: lt_billdi_pack       TYPE /vpcoe/t_billdi_data,
        lv_expire_date       TYPE sy-datum,
        lt_bill_docs         TYPE /vpcoe/t_billdi_data,
        lt_mat_doc           TYPE SORTED TABLE OF /vpcoe/billdi WITH NON-UNIQUE KEY mat_doc,
        lt_material_document TYPE SORTED TABLE OF /vpcoe/billdi WITH NON-UNIQUE KEY mat_doc,
        lv_total             TYPE /vpcoe/str_log_sum-total,
        lv_total_failed      TYPE /vpcoe/str_log_sum-total_failed,
        lv_skip              TYPE xfeld,
        lv_cond_bill_doc     TYPE string,
        lv_cond_doc          TYPE string,
        lv_no_data           TYPE abap_bool VALUE abap_true,
        lv_pack_size         TYPE i,
        lv_act_package       TYPE i,
        lv_num_packages      TYPE i,
        lv_record_cnt        TYPE i,
        lv_top               TYPE i,
        lv_level             TYPE /vpcoe/level.

  GET BADI lo_badi.

  lv_level = SWITCH #( iv_service_id WHEN /vpcoe/cl_common_helper=>sc_service_id-delivery
                                       THEN /vpcoe/cl_common_helper=>sc_level-dlvr_billdoc
                                     WHEN /vpcoe/cl_common_helper=>sc_service_id-material_doc
                                       THEN /vpcoe/cl_common_helper=>sc_level-mtrl_billdoc ).

  DATA(lv_db_connection) = /vpcoe/cl_common_helper=>get_db_connection( iv_srv_grp  = me->mv_srvgrp
                                                                       iv_srv_id   = me->mv_srvid
                                                                       iv_api_type = me->mv_api_type
                                                                       is_sel_opt  = is_sel_opt
                                                                       iv_level    = lv_level ).

  CALL BADI lo_badi->skip_selection
    EXPORTING
      iv_srv_grp  = me->mv_srvgrp
      iv_srv_id   = me->mv_srvid
      iv_api_type = me->mv_api_type
      is_sel_opt  = is_sel_opt
      iv_level    = lv_level
      iv_mode     = me->mv_mode
      io_log      = io_log
    CHANGING
      cv_skip     = lv_skip.

  IF lv_skip = abap_false.
* Get MatDocs from the MatDoc report run
*    IF is_sel_opt-billdoc_session_id IS NOT INITIAL.
    CASE iv_service_id.
      WHEN /vpcoe/cl_common_helper=>sc_service_id-delivery.
        lv_cond_bill_doc = `( vbrp~vgbel = @lt_mat_doc-mat_doc AND vbrp~vgpos = @lt_mat_doc-delivery_document_item`
                        && ` AND ( vbrp~vgtyp = 'J' OR vbrp~vgtyp = 'T' ) ) OR ( vbrp~vgbel = @lt_mat_doc-delivery_document `
                        && `AND vbrp~vgpos = @lt_mat_doc-reference_sd_document_item AND ( vbrp~vgtyp = 'C' OR vbrp~vgtyp = 'G' `
                        && `OR vbrp~vgtyp = 'H' OR vbrp~vgtyp = 'K' OR vbrp~vgtyp = 'L' ) )`.

        lv_cond_doc = `session_id IN is_sel_opt-billdoc_session_id AND ship_to_country NOT IN is_sel_opt-billdi_country AND ship_from_country IN is_sel_opt-billdi_country`.
        " in case of changes the condition above there is need to update the select in HANDLE_BILLDI method

      WHEN /vpcoe/cl_common_helper=>sc_service_id-material_doc.
        lv_cond_bill_doc = `vbrp~vgpos = @lt_mat_doc-delivery_document_item AND vbrp~vgbel = @lt_mat_doc-delivery_document`
        && ` AND ( vbrp~vgtyp = 'J' OR vbrp~vgtyp = 'T' )` .

        lv_cond_doc = `session_id IN is_sel_opt-billdoc_session_id `
                      && `AND ( ( code = /vpcoe/cl_rdp_material_doc=>sc_variants-gr_st_imp AND ship_from_country NOT IN is_sel_opt-billdi_country `
                                    && `AND ship_to_country IN is_sel_opt-billdi_country ) `
                           && `OR ( code = /vpcoe/cl_rdp_material_doc=>sc_variants-gi_st_exp AND ship_from_country IN is_sel_opt-billdi_country`
                                    && ` AND ship_to_country NOT IN is_sel_opt-billdi_country ) )`.
    ENDCASE.

    SELECT *
      INTO TABLE lt_material_document
       FROM /vpcoe/billdi
         WHERE (lv_cond_doc).

    IF sy-subrc <> 0.
      IF iv_service_id = /vpcoe/cl_common_helper=>sc_service_id-delivery.
        MESSAGE i079(/vpcoe/common) WITH /vpcoe/cl_common_helper=>sc_service_id-delivery INTO io_log->sv_msg_text.
      ELSE.
        MESSAGE i079(/vpcoe/common) WITH iv_code INTO io_log->sv_msg_text.
      ENDIF.
      io_log->add_sy_msg( ).
      RETURN.
    ENDIF.
*    ENDIF.
*    IF lt_material_document IS INITIAL.
*      RETURN.
*    ENDIF.

    ASSIGN ct_total_count_billdoc[ sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-billdocit ] TO FIELD-SYMBOL(<ls_sum>).
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO ct_total_count_billdoc ASSIGNING <ls_sum>.
      <ls_sum>-sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-billdocit.
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

      SELECT vbrk~vbeln AS billing_document,
             vbrp~posnr AS billing_document_item,
             vbrk~fkdat AS billing_document_date,
             vbrp~vgbel AS referencesddoc,
             vbrp~vgpos AS referencesddocitem,
             vbrp~vgtyp AS referencesddoccat
        FROM vbrk INNER JOIN vbrp ON vbrk~vbeln = vbrp~vbeln
        INTO CORRESPONDING FIELDS OF TABLE @lt_bill_docs
          CONNECTION (lv_db_connection)
              PACKAGE SIZE @me->mv_package_size
         FOR ALL ENTRIES IN @lt_mat_doc
        WHERE vbrk~vbeln IN @is_sel_opt-billdoc
          AND vbrk~fkdat IN @is_sel_opt-bdocdate
          AND vbrp~vgtyp IN @is_sel_opt-bsddoccat
          AND (lv_cond_bill_doc).

        CALL BADI lo_badi->adjust_data_retrieval
          EXPORTING
            iv_srv_grp  = me->mv_srvgrp
            iv_srv_id   = me->mv_srvid
            iv_api_type = me->mv_api_type
            is_sel_opt  = is_sel_opt
            iv_level    = lv_level
            iv_mode     = me->mv_mode
            io_log      = io_log
          CHANGING
            ct_data     = lt_bill_docs.

        lv_no_data = abap_false.

        IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND iv_send = abap_true.
          me->build_and_store_json( EXPORTING it_billdi = lt_bill_docs
                                              iv_level  = lv_level
                                              io_log    = io_log ).
        ELSEIF lines( et_billdi ) < /vpcoe/cl_rdp_helper=>gc_display_amount.
          INSERT LINES OF lt_bill_docs INTO TABLE et_billdi.
        ELSE.
          lv_act_package = lv_num_packages.
          EXIT.
        ENDIF.

      ENDSELECT.

      lv_act_package = lv_act_package + 1.
      CLEAR lt_mat_doc.
    ENDWHILE.

    IF lv_no_data = abap_true.
      IF iv_service_id = /vpcoe/cl_common_helper=>sc_service_id-delivery.
        MESSAGE i079(/vpcoe/common) WITH /vpcoe/cl_common_helper=>sc_service_id-delivery INTO io_log->sv_msg_text.
      ELSE.
        MESSAGE i079(/vpcoe/common) WITH iv_code INTO io_log->sv_msg_text.
      ENDIF.
      io_log->add_sy_msg( ).
      RETURN.
    ENDIF.

  ELSE.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = me->mv_srvgrp
        iv_srv_id   = me->mv_srvid
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_level    = lv_level
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        ct_data     = lt_bill_docs.

    LOOP AT lt_bill_docs ASSIGNING FIELD-SYMBOL(<ls_billdi>).
      INSERT <ls_billdi> INTO TABLE lt_billdi_pack.
      IF lines( lt_billdi_pack ) = me->mv_package_size.
        IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND iv_send = abap_true.
          me->build_and_store_json( EXPORTING it_billdi = lt_bill_docs
                                              iv_level  = lv_level
                                              io_log    = io_log ).
        ELSE.
          et_billdi = lt_bill_docs.
          RETURN.
        ENDIF.

        CLEAR lt_billdi_pack.
      ENDIF.
    ENDLOOP.

    IF lt_billdi_pack IS NOT INITIAL.
      IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND iv_send = abap_true.
        me->build_and_store_json( EXPORTING it_billdi = lt_bill_docs
                                            iv_level  = lv_level
                                            io_log    = io_log ).
      ELSE.
        et_billdi = lt_bill_docs.
        RETURN.
      ENDIF.

    ENDIF.
  ENDIF.

  IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND iv_send = abap_true.

*   Mark Entries as Processed
    LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_mat_doc>).
      IF <ls_mat_doc>-session_id IS NOT INITIAL.
        <ls_mat_doc>-billdi = abap_true.
      ENDIF.
    ENDLOOP.

    MODIFY /vpcoe/billdi FROM TABLE lt_material_document.
    CALL FUNCTION 'DB_COMMIT'.

    lv_expire_date = sy-datum - 10.

*   Delete obsoleted entries or fully processed
    DELETE FROM /vpcoe/billdi
      WHERE ( billdi = abap_true AND supinvi = abap_true )
         OR added_on < lv_expire_date.

*  Send BillDocItems
    NEW /vpcoe/cl_rdp_payload_handler( )->send_payload_multi(
      EXPORTING
        it_r_session_ids = VALUE #( ( sign = 'I' option = 'EQ' low = me->mv_session_id ) )
        iv_api_type      = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
        iv_srv_id        = me->mv_srvid
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
