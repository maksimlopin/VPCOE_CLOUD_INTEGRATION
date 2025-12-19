class /VPCOE/CL_INTEGRATION_HELPER definition
  public
  final
  create public .

public section.

  interfaces /VPCOE/IF_INTEGRATION_HELPER .

  constants GC_COMPLIANCE_INFO_STRUCTURE type STRUKNAME value '/VPCOE/S_CMPL_CHK_APP_PARAMS' ##NO_TEXT.

  class-methods FILL_FROM_MULTIRANGE
    importing
      !IT_MULTIRANGE type /VPCOE/T_MULTIRANGE
      !IV_FIELDNAME type FIELDNAME
    changing
      !CT_SINGLERANGE type INDEX TABLE .
  class-methods FILL_LOGISTICS_DOC_NUMBER
    importing
      !IV_LOGISTIC_DOC_CATEGORY type /VPCOE/LOG_DOC_CATEGORY
      !IV_LOGISTIC_DOC_NO type /VPCOE/LOG_DOC_NUMBER
    exporting
      !EV_SALES_ORDER type VBELN_VA
      !EV_SALES_ORDER_WOC type VBELN_VA
      !EV_SALES_QUOTATION type VBELN_VA
      !EV_SALES_CONTRACT type VBELN_VA
      !EV_SALES_INQUIRY type VBELN_VA
      !EV_OUTBOUND_DELIVERY type VBELN
      !EV_SALES_SCHEDULING_AGREEMENT type VBELN_VA
      !EV_FREIGHT_UNIT type /VPCOE/TOR_ID
      !EV_FREIGHT_ORDER type /VPCOE/TOR_ID
      !EV_FREIGHT_BOOKING type /VPCOE/TOR_ID
      !EV_PURCHASE_ORDER type EBELN
      !EV_PURCHASE_CONTRACT type EBELN
      !EV_PURCHASING_INFO_RECORD type INFNR
      !EV_PUR_SCHEDULING_AGREEMENT type EBELN .
  class-methods GET_REMOTE_COMPONENT_VERSION
    importing
      !IV_RFC_DESTINATION type RFC_DEST
      !IV_COMPONENT_NAME type /VPCOE/COMPONENT_NAME
    returning
      value(RV_COMPONENT_VERSION) type MGV_COMPONENT_VERSION
    raising
      /VPCOE/CX_HRRCF_RFC_COMMUNICTN .
  class-methods GET_REMOTE_SYSTEM_ID
    importing
      !IV_RFC_DESTINATION type RFC_DEST
    returning
      value(RV_SYSID) type SYSYSID
    raising
      /VPCOE/CX_HRRCF_RFC_COMMUNICTN .
  class-methods IS_ERROR_SEVERE
    importing
      !IT_ERROR_TAB type BAPIRETTAB
    returning
      value(RV_IS_SEVERE) type BOOLEAN .
  class-methods IS_REMOTE_SWITCH_ON
    importing
      !IV_RFC_DESTINATION type RFC_DEST
      !IV_SWITCH_NAME type /VPCOE/COMPONENT_NAME
    returning
      value(RV_ON) type ABAP_BOOL
    raising
      /VPCOE/CX_HRRCF_RFC_COMMUNICTN .
  class-methods SHOW_COMPLIANCE_INFO
    importing
      !IS_PARAMS type /VPCOE/S_CMPL_CHK_APP_PARAMS
      !IV_COMPLIANCE_INFO_USAGE_ROLE type IHTTPVAL default 'SALES' .
  methods CONSTRUCTOR .
*"* public components of class CL_EHFND_INTEGRATION_HELPER
*"* do not include other source files here!!!
  methods GET_REMOTE_TABLE_CONTENT
    importing
      !IV_TABLENAME type TABNAME
      !IV_STRUCT_TABLENAME type TABNAME optional
      !IV_RFC_DESTINATION type RFC_DEST optional
      !IT_SELOPT type /VPCOE/T_SEL_OPTIONS optional
      !IV_ROWCOUNT type I optional
      !IV_ROWSKIPS type I optional
    changing
      !CT_FIELDS type /VPCOE/T_DB_FIELDS optional
      !CT_CONTENT type STANDARD TABLE .
  class-methods IS_S4H_CLOUD
    returning
      value(RV_IS_S4H_CLOUD) type ABAP_BOOL .
  class-methods GET_MSG_FROM_SYS_FIELDS
    returning
      value(RS_MSG) type SYMSG .
  PROTECTED SECTION.
*"* protected components of class CL_EHFND_INTEGRATION_HELPER
*"* do not include other source files here!!!
private section.

  data MO_RFC_READ_TABLE type ref to LIF_RFC_READ_TABLE .
  class-data:
    BEGIN OF gv_s4h,
        _valid          TYPE abap_bool,
        public_cloud_on TYPE abap_bool,
        on_premise_on   TYPE abap_bool,
      END OF gv_s4h .
  constants CV_ID type INDX-SRTFD value 'VALIDATE_GV_S4H:1' ##NO_TEXT.
  constants CS4H_ON_PREMISE type SFW_BFUNCTION value 'SIMPLIFY_ON_PREMISE' ##NO_TEXT.

  class-methods GET_LOG_DOC_NUMBER_CHAR_10
    importing
      !IV_LOG_DOC_NUMBER type ANY optional
    returning
      value(RESULT) type CHAR10 .
  class-methods VALIDATE_GV_S4H .
  class-methods IS_CLOUD
    returning
      value(RV_IS_CLOUD) type ABAP_BOOL .
*"* private components of class CL_EHFND_INTEGRATION_HELPER
*"* do not include other source files here!!!
ENDCLASS.



CLASS /VPCOE/CL_INTEGRATION_HELPER IMPLEMENTATION.


  METHOD /vpcoe/if_integration_helper~get_remote_table_content.

*    TRY.
        me->get_remote_table_content(
          EXPORTING
            iv_tablename        = iv_tablename
            iv_struct_tablename = iv_struct_tablename
            iv_rfc_destination  = iv_rfc_destination
            it_selopt           = it_selopt
            iv_rowcount         = iv_rowcount
            iv_rowskips         = iv_rowskips
          CHANGING
            ct_fields           = ct_fields
            ct_content          = ct_content ).

*      CATCH /vpcoe/cx_hrrcf_rfc_communictn INTO DATA(lx_cx_rfc_error).
*        RAISE EXCEPTION TYPE /vpcoe/cx_hrrcf_rfc_communictn
*          EXPORTING
*            previous = lx_cx_rfc_error.
*      CATCH /vpcoe/cx_inbound_processing INTO DATA(lx_cs_inb_proc).
*        RAISE EXCEPTION TYPE cx_ehswx_inbound_processing
*          EXPORTING
*            previous = lx_cs_inb_proc.
*      CATCH cx_esh_au_not_authorized INTO DATA(lx_cx_no_auth).
*        RAISE EXCEPTION TYPE cx_esh_au_not_authorized
*          EXPORTING
*            previous = lx_cx_no_auth.
*    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
    CREATE OBJECT me->mo_rfc_read_table TYPE lcl_rfc_read_table.
  ENDMETHOD.


  METHOD fill_from_multirange.

    DATA: ls_multirange LIKE LINE OF it_multirange.
    DATA: lr_singlerange TYPE REF TO data.

    FIELD-SYMBOLS: <lt_singlerange> TYPE INDEX TABLE.
    FIELD-SYMBOLS: <lv_singlerange> TYPE any.

    ASSIGN ct_singlerange TO <lt_singlerange>.

    " create dynamically the correct table line type
    CREATE DATA lr_singlerange LIKE LINE OF <lt_singlerange>.
    ASSIGN lr_singlerange->* TO <lv_singlerange>.

    LOOP AT it_multirange INTO ls_multirange WHERE fname = iv_fieldname.
      MOVE-CORRESPONDING ls_multirange TO <lv_singlerange>.
      APPEND <lv_singlerange> TO ct_singlerange.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_logistics_doc_number.

    " Enter the document number to the correct receiver based on the given logistics document type

    CLEAR: ev_outbound_delivery, ev_sales_order, ev_sales_quotation, ev_purchase_contract.

    CASE iv_logistic_doc_category.

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-outbound_delivery.
        ev_outbound_delivery = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-sales_order.
        ev_sales_order = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-sales_order_wo_charge .
        ev_sales_order_woc = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-sales_quotation.
        ev_sales_quotation = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-sales_contract.
        ev_sales_contract = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-sales_inquiry.
        ev_sales_inquiry = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-scheduling_agreement.
        ev_sales_scheduling_agreement = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-freight_order.
        ev_freight_order = iv_logistic_doc_no.

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-freight_unit.
        ev_freight_unit = iv_logistic_doc_no.

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-freight_booking.
        ev_freight_booking = iv_logistic_doc_no.

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-standard_purchase_order OR
           /vpcoe/cl_common_helper=>sc_log_doc_cat-purchase_order_w_transp_rlvnce OR
           /vpcoe/cl_common_helper=>sc_log_doc_cat-stock_transport_order OR
           /vpcoe/cl_common_helper=>sc_log_doc_cat-ico_sales_purchase_order OR
           /vpcoe/cl_common_helper=>sc_log_doc_cat-ico_stock_transport_order.
        ev_purchase_order = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-value_contract OR
           /vpcoe/cl_common_helper=>sc_log_doc_cat-quantity_contract OR
           /vpcoe/cl_common_helper=>sc_log_doc_cat-purchase_contract .
        ev_purchase_contract = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-purchase_scheduling_agreement.
        ev_pur_scheduling_agreement = get_log_doc_number_char_10( iv_logistic_doc_no ).

      WHEN /vpcoe/cl_common_helper=>sc_log_doc_cat-purchase_info_record.
        ev_purchasing_info_record = get_log_doc_number_char_10( iv_logistic_doc_no ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_log_doc_number_char_10.
    DATA: lv_number_without_zeros TYPE /vpcoe/log_doc_number.
    WRITE iv_log_doc_number TO lv_number_without_zeros.
    " Now add leading zeros again matching the field - e.g. '12345678' becomes '00000000000012345678'
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_number_without_zeros
      IMPORTING
        output = result.

  ENDMETHOD.


  METHOD get_msg_from_sys_fields.
    CLEAR rs_msg.

    rs_msg-msgid = sy-msgid.
    rs_msg-msgno = sy-msgno.
    rs_msg-msgty = sy-msgty.
    rs_msg-msgv1 = sy-msgv1.
    rs_msg-msgv2 = sy-msgv2.
    rs_msg-msgv3 = sy-msgv3.
    rs_msg-msgv4 = sy-msgv4.
  ENDMETHOD.


  METHOD get_remote_component_version.
    DATA msg TYPE c LENGTH 255.
*** Begin of correction 2695835 - 19.09.2018
    "change from DELIVERY_GET_COMPONENT_STATE to prevent unwanted authority-check
    CALL FUNCTION 'DELIVERY_GET_COMPONENT_RELEASE'
      DESTINATION iv_rfc_destination
      EXPORTING
        iv_compname           = iv_component_name
      IMPORTING
        ev_compvers           = rv_component_version
      EXCEPTIONS
        comp_not_found        = 1
        communication_failure = 2 MESSAGE msg
        OTHERS                = 3.

    IF sy-subrc <> 0.
      " Check fallback for S4CORE
      IF ( iv_component_name = 'SAP_APPL' OR iv_component_name = 'EA-APPL' ).
        " check if remote system is S4CORE (in this case the components for EA-APPL and SAP_APPL are integrated)
        CALL FUNCTION 'DELIVERY_GET_COMPONENT_RELEASE'
          DESTINATION iv_rfc_destination
          EXPORTING
            iv_compname    = 'S4CORE'
          IMPORTING
            ev_compvers    = rv_component_version
          EXCEPTIONS
            comp_not_found = 1.
        IF sy-subrc <> 0.
          rv_component_version = '000'.
        ELSE.
          "Return a very high value for component version for S4CORE
          rv_component_version = '740'.
        ENDIF.
      ELSE.
        " If the component is not found, return a low version so that checking for a certain
        " version or higher will fail. This might be an error though also?
        rv_component_version = '000'.
*       The other exceptions won't be raise explicit, because a short dump
*       is the best way here to raise this exceptions.
      ENDIF.
    ENDIF.
*** End of correction 2695835 - 19.09.2018
  ENDMETHOD.


  METHOD get_remote_system_id.

    DATA: lv_rfc_message TYPE /vpcoe/rfc_message.

    CALL FUNCTION 'SCT2_GET_T000_ENTRY_RFC'
      DESTINATION iv_rfc_destination
      IMPORTING
        ev_sysid              = rv_sysid
      EXCEPTIONS
        system_failure        = 1 MESSAGE lv_rfc_message
        communication_failure = 2 MESSAGE lv_rfc_message.


* error handling
    IF sy-subrc <> 0 .
      CASE sy-subrc.
        WHEN 1.
          RAISE EXCEPTION TYPE /vpcoe/cx_hrrcf_rfc_communictn
            EXPORTING
              textid = /vpcoe/cx_hrrcf_rfc_communictn=>cx_hrrcf_rfc_system_failure.
*              mv_rfc_destination = iv_rfc_destination
*              mv_function        = 'GET_REMOTE_SYSTEM_ID'
*              mv_message_text    = lv_rfc_message.
        WHEN 2.
          RAISE EXCEPTION TYPE /vpcoe/cx_hrrcf_rfc_communictn
            EXPORTING
              textid = /vpcoe/cx_hrrcf_rfc_communictn=>/vpcoe/cx_hrrcf_rfc_communictn.
*             mv_rfc_destination = iv_rfc_destination
*             mv_function        = 'GET_REMOTE_SYSTEM_ID'
*             mv_message_text    = lv_rfc_message.
      ENDCASE.
    ENDIF.                             " sy-subrc <> 0


  ENDMETHOD.


  METHOD get_remote_table_content.
* ----------------------------------------------------------------------
* Local data
* ----------------------------------------------------------------------
    TYPES: BEGIN OF lty_fieldmap,
             index     TYPE i,
             fieldname TYPE string,
           END OF lty_fieldmap.

    DATA: lt_content         TYPE TABLE OF tab512.
    DATA: lv_fieldname       TYPE string.
    DATA: lt_contentlist     TYPE TABLE OF string.
    DATA: lv_contentline     LIKE LINE OF lt_contentlist.
    DATA: ls_fieldmap        TYPE lty_fieldmap.
    DATA: lt_fieldmap        TYPE TABLE OF lty_fieldmap.
    DATA: lo_sdescr          TYPE REF TO cl_abap_structdescr.
    DATA: lo_tdescr          TYPE REF TO cl_abap_tabledescr.
    DATA: lr_thandle         TYPE REF TO data.
    DATA: lr_shandle         TYPE REF TO data.
    DATA: lv_rfc_message     TYPE /vpcoe/rfc_message.
    DATA: lv_tablename       TYPE char40.
    DATA: lt_result_tab      TYPE match_result_tab.
    DATA: lv_subrc           TYPE sy-subrc.
    DATA: lv_last_symbol     TYPE char1.
    DATA: lv_num_position    TYPE i.
    DATA: lv_act_length      TYPE i.
    DATA: lv_result_lines    TYPE i.

* field symbols
    FIELD-SYMBOLS:
      <lt_table>      TYPE STANDARD TABLE,
      <ls_field>      LIKE LINE OF ct_fields,
      <lv_strucfield> TYPE any,
      <lv_structure>  TYPE any,
      <ls_content>    LIKE LINE OF lt_content,
      <ls_fieldmap>   LIKE LINE OF lt_fieldmap.

* ----------------------------------------------------------------------
* Function body
* ----------------------------------------------------------------------
* describe structure
    IF ( iv_struct_tablename IS NOT INITIAL ).
      lo_sdescr ?= cl_abap_structdescr=>describe_by_name( iv_struct_tablename ).
    ELSE.
      lo_sdescr ?= cl_abap_structdescr=>describe_by_name( iv_tablename ).
    ENDIF.

    lo_tdescr  =  cl_abap_tabledescr=>create( lo_sdescr ).

* create data refence followed by table creation
    CREATE DATA lr_thandle TYPE HANDLE lo_tdescr.
    ASSIGN lr_thandle->* TO <lt_table>.
    CREATE DATA lr_shandle TYPE HANDLE lo_sdescr.
    ASSIGN lr_shandle->* TO <lv_structure>.

* read table content
* with rfc destination

    me->mo_rfc_read_table->call_fuba_rfc_read_table(
      EXPORTING
        iv_tablename        = iv_tablename
        iv_struct_tablename = iv_struct_tablename
        iv_rfc_destination  = iv_rfc_destination
        it_selopt           = it_selopt
        iv_rowcount         = iv_rowcount
        iv_rowskips         = iv_rowskips
      IMPORTING
        ev_subrc            = lv_subrc
        ev_rfc_message      = lv_rfc_message
      CHANGING
        ct_fields           = ct_fields
        ct_content          = lt_content ).

* error handling
    IF ( lv_subrc <> 0 ).
      MOVE iv_tablename TO lv_tablename.
      CASE lv_subrc.
        WHEN 5.
          RETURN. " RAISE EXCEPTION TYPE cx_esh_au_not_authorized .

        WHEN 6.
          RETURN. " RAISE EXCEPTION TYPE /vpcoe/cx_inbound_processing.

        WHEN 7.
          RETURN. " RAISE EXCEPTION TYPE /vpcoe/cx_hrrcf_rfc_communictn
*          EXPORTING
*            textid = /vpcoe/cx_hrrcf_rfc_communictn=>cx_hrrcf_rfc_system_failure.

        WHEN 8.
          RETURN. " RAISE EXCEPTION TYPE /vpcoe/cx_hrrcf_rfc_communictn
*          EXPORTING
*            textid = /vpcoe/cx_hrrcf_rfc_communictn=>/vpcoe/cx_hrrcf_rfc_communictn.

        WHEN OTHERS.
*       no message just return nothing
      ENDCASE.
    ENDIF.

* build and fill output
    LOOP AT ct_fields ASSIGNING <ls_field>.
      CONCATENATE '<LV_STRUCTURE>' '-' <ls_field>-fieldname INTO lv_fieldname.
      CLEAR ls_fieldmap.
      ls_fieldmap-fieldname = lv_fieldname.
      ls_fieldmap-index = sy-tabix.
      APPEND ls_fieldmap TO lt_fieldmap.
    ENDLOOP.

    DATA(lv_lines) = lines( ct_fields ).

    READ TABLE ct_fields REFERENCE INTO DATA(lr_s_fields) INDEX lv_lines.
    IF sy-subrc = 0.
      lv_num_position = lr_s_fields->offset + lr_s_fields->length.
    ENDIF.

    LOOP AT lt_content ASSIGNING <ls_content>.
      CLEAR <lv_structure>.
      FIND ALL OCCURRENCES OF '|' IN <ls_content>-wa RESULTS lt_result_tab.
      SPLIT <ls_content>-wa AT '|' INTO TABLE lt_contentlist.

      CLEAR:
        lv_act_length,
        lv_last_symbol,
        lv_result_lines.

      lv_act_length = strlen( <ls_content>-wa ).

      IF lv_act_length < lv_num_position. " exceptional case: the last field is empty and delimiter wasn't added to the end yet
        lv_result_lines = lines( lt_result_tab ) + 1.
      ELSE.

        lv_act_length = lv_act_length - 1.
        lv_last_symbol = <ls_content>-wa+lv_act_length(1).

        IF lv_last_symbol = '|'.
          lv_result_lines = lines( lt_result_tab ).
        ELSE.
          lv_result_lines = lines( lt_result_tab ) + 1.
        ENDIF.

      ENDIF.

      IF lv_result_lines = lines( lt_fieldmap ).
        LOOP AT lt_fieldmap ASSIGNING <ls_fieldmap>.
          ASSIGN (<ls_fieldmap>-fieldname) TO <lv_strucfield>.
          IF  ( sy-subrc <> 0 ).
            CONTINUE.
          ENDIF.
          READ TABLE lt_contentlist INTO lv_contentline INDEX <ls_fieldmap>-index.
          <lv_strucfield> = lv_contentline.

        ENDLOOP.
        APPEND <lv_structure> TO <lt_table>.
      ENDIF.
    ENDLOOP.

* fill changing table
* generic tables must be changing parameters
* otherwise the structure is not clear
    ct_content = <lt_table>.

  ENDMETHOD.


  METHOD is_cloud.
    validate_gv_s4h( ).

    rv_is_cloud = gv_s4h-public_cloud_on.
  ENDMETHOD.


  METHOD is_error_severe.
    rv_is_severe = abap_false.
    LOOP AT it_error_tab TRANSPORTING NO FIELDS
      WHERE type = 'A' OR type = 'E'.

      rv_is_severe = abap_true.
      EXIT. "LOOP
    ENDLOOP.
  ENDMETHOD.


  METHOD is_remote_switch_on.


    DATA: lv_rfc_message  TYPE /vpcoe/rfc_message,
          lv_switch_state TYPE sfw_switchpos.

    CALL FUNCTION 'SFW_GET_SWITCH_STATE'
      DESTINATION iv_rfc_destination
      EXPORTING
        switch_name           = iv_switch_name
      IMPORTING
        switch_state          = lv_switch_state
      EXCEPTIONS
        system_failure        = 1 MESSAGE lv_rfc_message
        communication_failure = 2 MESSAGE lv_rfc_message.


* error handling
    IF ( sy-subrc <> 0 ).
      CASE sy-subrc.
        WHEN 1.
          RAISE EXCEPTION TYPE /vpcoe/cx_hrrcf_rfc_communictn
            EXPORTING
              textid = /vpcoe/cx_hrrcf_rfc_communictn=>cx_hrrcf_rfc_system_failure.
*              mv_rfc_destination = iv_rfc_destination
*              mv_function        = 'IS_REMOTE_SWITCH_ON'
*              mv_message_text    = lv_rfc_message.
        WHEN 2.
          RAISE EXCEPTION TYPE /vpcoe/cx_hrrcf_rfc_communictn
            EXPORTING
              textid = /vpcoe/cx_hrrcf_rfc_communictn=>/vpcoe/cx_hrrcf_rfc_communictn.
*              mv_rfc_destination = iv_rfc_destination
*              mv_function        = 'IS_REMOTE_SWITCH_ON'
*              mv_message_text    = lv_rfc_message.
      ENDCASE.
    ENDIF.

* the fixed value is "T" for "On"
    rv_on = boolc( lv_switch_state = 'T' ).

  ENDMETHOD.


  METHOD is_s4h_cloud.
    DATA lv_is_s4h_cloud TYPE abap_bool.

    DATA(lv_is_fake_cloud) = cl_user_settings_mm=>get_parameter( im_parameter_id = 'FAKE_CLOUD' ). " just for testing purpose

    lv_is_s4h_cloud = /vpcoe/cl_integration_helper=>is_cloud( ).

    IF lv_is_fake_cloud = abap_true OR lv_is_s4h_cloud = abap_true.
      rv_is_s4h_cloud = abap_true.
    ELSE.
      rv_is_s4h_cloud = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD show_compliance_info.

    " Purpose: Open Fiori app "Product Compliance Information"

    DATA:
      lv_document_number TYPE /vpcoe/log_doc_number,
      lt_parameters      TYPE tihttpnvp.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = is_params-document_number
      IMPORTING
        output = lv_document_number.

    lt_parameters = VALUE #(
      ( name  = 'Material'                  value = is_params-material )
      ( name  = 'HighlightedCountry'        value = is_params-country_to )
      ( name  = 'LogisticDocumentCategory'  value = is_params-document_category )
      ( name  = 'LogisticDocumentNumber'    value = lv_document_number )
      ( name  = 'LogisticDocumentItem'      value = is_params-document_item_number )
      ( name  = 'UsageRole'                 value = iv_compliance_info_usage_role )
    ).

    cl_lsapi_manager=>navigate_to_intent(
      EXPORTING
        object            = 'ChemicalComplianceInfo'
        action            = 'displayProducts'
        navigation_mode   = if_lsapi=>gc_s_navigation_mode-new_external_window
        parameters        = lt_parameters
        add_source_system = abap_false
      EXCEPTIONS
        error             = 1
        not_supported     = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      " Navigation is not supported, most properly is the method called from a backend system. IBN based
      " navigation only works in frontend systems or via NWBC.
    ENDIF.

  ENDMETHOD.


  METHOD validate_gv_s4h.
    DATA lv_on TYPE flag.
    DATA lv_wa TYPE indx. " for analysis: user and timestamp when shared memory was last set - cos_shared_mem


    IF gv_s4h-_valid = abap_true.
      RETURN.
    ENDIF.

    " try shared memory
    IMPORT
        s4h_public_cloud = gv_s4h-public_cloud_on
        s4h_on_premise   = gv_s4h-on_premise_on
      FROM SHARED MEMORY indx(aa) TO lv_wa ID cv_id.
    IF sy-subrc = 0.
      gv_s4h-_valid = abap_true.
      RETURN.
    ENDIF.

    " get info about cloud installation from cloud-only packages
    SELECT SINGLE @abap_true FROM tdevc WHERE devclass = 'SR_APS' INTO @gv_s4h-public_cloud_on.
    IF sy-subrc <> 0.
      gv_s4h-public_cloud_on = abap_false.
    ENDIF.

    " get info about on-premise installation from Business Function activation
    CALL FUNCTION 'SFW_IS_BFUNC_SWITCHED_ON'
      EXPORTING
        bfunc_name     = cs4h_on_premise
      IMPORTING
        is_switched_on = lv_on.
    IF lv_on = 'X'.
      gv_s4h-on_premise_on = abap_true.
    ELSE.
      gv_s4h-on_premise_on = abap_false.
    ENDIF.

    " store to shared memory
    lv_wa-usera = sy-uname.
*    GET TIME STAMP FIELD lv_wa-timestamp.
*    EXPORT
*        s4h_public_cloud = gv_s4h-public_cloud_on
*        s4h_on_premise   = gv_s4h-on_premise_on
*      TO SHARED MEMORY cos_shared_mem(aa) FROM lv_wa ID cv_id. ----?

    gv_s4h-_valid = abap_true.
  ENDMETHOD.
ENDCLASS.
