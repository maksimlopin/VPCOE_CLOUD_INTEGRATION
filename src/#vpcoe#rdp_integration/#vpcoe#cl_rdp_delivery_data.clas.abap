class /VPCOE/CL_RDP_DELIVERY_DATA definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_sel_opt,
        deliv     TYPE RANGE OF lfart,
        orgnz     TYPE RANGE OF vkorg,
        ship_t    TYPE RANGE OF kunnr,
        country   TYPE RANGE OF land1,
        region    TYPE RANGE OF regio,
        chng_date TYPE RANGE OF aedat,
        crt_date  TYPE RANGE OF erdat,
        category  TYPE RANGE OF pstyv,
        distrib   TYPE RANGE OF vtweg,
        division  TYPE RANGE OF spart,
      END OF gty_s_sel_opt .
  types:
    gty_t_cpi TYPE STANDARD TABLE OF bdicpident .
  types:
    gty_r_lfart     TYPE RANGE OF lfart .                                     "Delivery Type
  types:
    gty_r_vkorg     TYPE RANGE OF vkorg .                                     "Sales Organization
  types:
    gty_r_kunnr     TYPE RANGE OF kunnr .                                     "Ship-to party
  types:
    gty_r_land1     TYPE RANGE OF land1 .                                     "Country
  types:
    gty_r_regio     TYPE RANGE OF regio .                                     "Region
  types:
    gty_r_chng_date TYPE RANGE OF aedat .                                     "Change date
  types:
    gty_r_crt_date  TYPE RANGE OF erdat .                                     "Create date
  types:
    gty_r_category  TYPE RANGE OF pstyv .
  types:
    gty_r_distribution  TYPE RANGE OF vtweg .
  types:
    gty_t_twlad TYPE SORTED TABLE OF twlad WITH NON-UNIQUE KEY werks lgort .
  types:
    BEGIN OF gty_s_adrc,
        addrnumber TYPE adrc-addrnumber,
        country    TYPE adrc-country,
      END OF gty_s_adrc .
  types:
    gty_t_adrc TYPE SORTED TABLE OF gty_s_adrc WITH NON-UNIQUE KEY addrnumber .
  types:
    gty_r_division  TYPE RANGE OF spart .
  types GTY_S_DELIVERY_ITEMS_JSON type /VPCOE/STR_DLVR_ITEMS_JSON .
  types GTY_S_DELIVERY_ITEMS type /VPCOE/STR_DELIVERY_ITEMS .
  types GTY_S_CUSTOMER type /VPCOE/STR_CUSTOMER .
  types:
    gty_t_customer TYPE SORTED TABLE OF gty_s_customer WITH NON-UNIQUE KEY id .
  types:
    gty_t_delivery_items TYPE SORTED TABLE OF gty_s_delivery_items .
  types:
    gty_t_delivery_items_json TYPE SORTED TABLE OF gty_s_delivery_items_json WITH NON-UNIQUE KEY id .
  types GTY_S_DELIVERY_DEL type /VPCOE/STR_DELIVERY_DEL .
  types:
    gty_t_delivery_del TYPE SORTED TABLE OF gty_s_delivery_del WITH NON-UNIQUE KEY id .
  types GTY_S_DELIVERY_COMBINED type /VPCOE/STR_DELIVERY .
  types:
    gty_t_delivery_combined TYPE SORTED TABLE OF gty_s_delivery_combined WITH NON-UNIQUE KEY id .
  types GTY_S_DELIVERY_JSON type /VPCOE/STR_DELIVERY_JSON .
  types:
    gty_t_delivery_json TYPE STANDARD TABLE OF gty_s_delivery_json .
  types GTY_S_DELIVERY_DEL_JSON type /VPCOE/STR_DELIVERY_DEL_JSON .
  types:
    gty_t_delivery_del_json TYPE STANDARD TABLE OF gty_s_delivery_del_json .
  types:
    BEGIN OF gty_s_where_cond,
        lines TYPE char72,
      END OF gty_s_where_cond .
  types:
    gty_t_where_cond TYPE STANDARD TABLE OF gty_s_where_cond .
  types:
    BEGIN OF gty_s_delivery_tables,
        delivery TYPE /vpcoe/t_delivery_hdr,
        items    TYPE /vpcoe/t_delivery_items,
      END OF gty_s_delivery_tables .

  methods BUILD_JSON
    importing
      !IT_DELIVERY_HDR type /VPCOE/T_DELIVERY_HDR
      !IT_DELIVERY_ITM type /VPCOE/T_DELIVERY_ITEMS
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    exporting
      !ES_JSON type /VPCOE/CL_RDP_HTTP=>GTY_S_JSON
      !ET_DELIVERY type /VPCOE/CL_RDP_DELIVERY_DATA=>GTY_T_DELIVERY_COMBINED .
  methods CHANGE_STATUS
    importing
      !IV_TEST_RUN type XFELD .
  methods CONSTRUCTOR
    importing
      !IV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE
      !IV_SOURCE type STRING
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional .
  methods DECREASE_WP_COUNT
    importing
      !P_TASK type CLIKE .
  methods GET_DATA_FOR_XLS
    importing
      !IO_LOG_BIL_DOC type ref to /VPCOE/CL_RDP_LOG optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IS_SEL_OPT type /VPCOE/S_SELOPT_DELIVERY
    exporting
      !ET_DELIVERY type /VPCOE/T_DELIVERY_XLS_FRM
      !ET_DELIVERY_ITEM type /VPCOE/T_DELIVERY_ITEMS_XLS
      !ET_BILLDI type /VPCOE/T_BILLDI_DATA .
  methods GET_DELIVERY
    importing
      !IV_SOURCE type STRING optional
      !IS_SEL_OPT type /VPCOE/S_SELOPT_DELIVERY
      !IT_DIVISION type GTY_R_DIVISION optional
      !IT_CATEGORY type GTY_R_CATEGORY optional
      !IT_DISTRIBUTION type GTY_R_DISTRIBUTION optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IO_LOG_BILLDOC type ref to /VPCOE/CL_RDP_LOG optional
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER optional
    exporting
      !EV_FAILED type ABAP_BOOL
      !ET_BILLDI type /VPCOE/T_BILLDI_DATA
      !EV_NO_DATA type ABAP_BOOL
      !ET_DELIVERY type /VPCOE/CL_RDP_DELIVERY_DATA=>GTY_T_DELIVERY_COMBINED
    changing
      !CT_TOTAL_COUNT type /VPCOE/TT_LOG_SUM optional
      !CT_TOTAL_COUNT_BILLDOC type /VPCOE/TT_LOG_SUM optional .
  methods GET_EXCEL_HEADER
    importing
      !IV_SHEET_NAME type CHAR40 optional
    exporting
      !EV_HEADER type STRING
      !EV_HEADER_LIST type STRING
      !EV_ITEM type STRING
      !EV_CODE_LIST type STRING
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_HEADER_ITM type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_CODE_LIST type /VPCOE/CL_XLS_HANDLER=>GTY_T_CODELIST
      !ET_HEADER_LIST type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_CODE_LIST_DELIVERYITEM type /VPCOE/CL_XLS_HANDLER=>GTY_T_CODELIST
      !EV_CODE_LIST_DELIVERYITEM type STRING
      !EV_HEADER_LIST_DELIVERYITEM type STRING .
  methods GET_ITEMS
    importing
      !IT_DELIVERY type /VPCOE/T_DELIVERY_HDR
      !IS_SEL_OPT type /VPCOE/S_SELOPT_DELIVERY
    exporting
      !ET_ITEMS type /VPCOE/T_DELIVERY_ITEMS .
  methods SEND_CHANGED_CUSTOMER
    importing
      !IT_CUSTOMER type /VPCOE/CL_RDP_CUSTOMER_DATA=>GTY_T_CUSTOMER .
  methods SET_CHANGED_CUSTOMER
    importing
      !IT_CUSTOMER type /VPCOE/CL_RDP_CUSTOMER_DATA=>GTY_T_CUSTOMER .
  methods CLOSE_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
  PROTECTED SECTION.
private section.

  data MO_LOG type ref to /VPCOE/CL_RDP_LOG .
  data MS_DELIVERY_TABLES type GTY_S_DELIVERY_TABLES .
  data MT_CPI type /VPCOE/CL_RDP_DELIVERY_DATA=>GTY_T_CPI .
  data MT_DB_JSON type /VPCOE/CL_RDP_PAYLOAD_HANDLER=>GTY_T_DB_JSON .
  data MT_SHIP_FROM_ADRC type GTY_T_ADRC .
  data MT_TWLAD type GTY_T_TWLAD .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_CHNG_POINTER_ID type EDI_MESTYP .
  data MV_MODE type /VPCOE/DE_MODE value 1 ##NO_TEXT.
  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SOURCE type STRING .
  data MV_TASKS_RUNNING type INT4 .
  data MT_T001W type /VPCOE/T_T001W .
  data MT_CHANGED_CUSTOMER type /VPCOE/CL_RDP_CUSTOMER_DATA=>GTY_T_CUSTOMER .

  methods ADD_DYNAMIC_COLUMNS
    exporting
      !EV_COLUMNS type STRING .
  methods HANDLE_BILLDI
    importing
      !IT_DLV_HDR type /VPCOE/T_DELIVERY_HDR
      !IT_DLV_ITEMS type /VPCOE/T_DELIVERY_ITEMS
      !IS_SEL_OPT type /VPCOE/S_SELOPT_DELIVERY
    returning
      value(RT_SESION_ID) type /VPCOE/CL_RDP_PAYLOAD_HANDLER=>GTY_R_SESION_ID .
  methods SET_SHIP_FROM_COUNTRY
    changing
      !CT_DELIVERY_ITEM type /VPCOE/T_DELIVERY_ITEMS .
ENDCLASS.



CLASS /VPCOE/CL_RDP_DELIVERY_DATA IMPLEMENTATION.


  METHOD add_dynamic_columns.
    DATA: lt_table_name TYPE ddfields.

    ev_columns =    `likp~vbeln     AS id,`
                  && `likp~lfart     AS type,`
                  && `likp~vkorg     AS sales_organization,`
                  && `likp~kunnr     AS ship_to_party,`
                  && `likp~erdat     AS create_date,`
                  && `likp~aedat     AS change_date,`
                  && `likp~wadat_ist AS actual_goods_movement_date,`
                  && `likp~inco1     AS incoterms,`
                  && `CASE WHEN likp~kunag = ' ' THEN likp~kunnr `
                  &&                            `ELSE likp~kunag END AS sold_to_party, `
                  && ` adrc~country AS ship_to_country, `
                  && `adrc~region  AS ship_to_region,`.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'LIKP'
        langu     = sy-langu
      TABLES
        dfies_tab = lt_table_name.

    IF sy-saprl >= '754' AND line_exists( lt_table_name[ fieldname = 'WBSTK' ] ).
      ev_columns = ev_columns && `LIKP~WBSTK AS overall_goods_movement_status`.
    ELSE.
      ev_columns = ev_columns && `VBUK~WBSTK AS overall_goods_movement_status`.
    ENDIF.

  ENDMETHOD.


  METHOD build_json.
    DATA: lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval,
          lt_json TYPE /vpcoe/cl_rdp_http=>gty_t_json.

    CLEAR: es_json,
           et_delivery.

    GET BADI lo_badi.

    LOOP AT it_delivery_hdr ASSIGNING FIELD-SYMBOL(<ls_delivery>).
      IF NOT line_exists( it_delivery_itm[ vbeln = <ls_delivery>-id ] ).
        CONTINUE.
      ENDIF.
      INSERT VALUE #( id                            = <ls_delivery>-id
                      type                          = <ls_delivery>-type
                      sales_organization            = <ls_delivery>-sales_organization
                      ship_to_party                 = <ls_delivery>-ship_to_party
                      ship_to_country               = <ls_delivery>-ship_to_country
                      ship_to_region                = <ls_delivery>-ship_to_region
                      actual_goods_movement_date    = <ls_delivery>-actual_goods_movement_date
                      incoterms                     = <ls_delivery>-incoterms
                      overall_goods_movement_status = <ls_delivery>-overall_goods_movement_status
                      sold_to_party                 = <ls_delivery>-sold_to_party
                      items                         = VALUE #( FOR <ls_items> IN it_delivery_itm
                                                                  WHERE ( vbeln = <ls_delivery>-id )
                                                                   ( CORRESPONDING #( <ls_items> ) ) ) )
               INTO TABLE et_delivery.
    ENDLOOP.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
        iv_level    = /vpcoe/cl_common_helper=>sc_level-build_json
        iv_api_type = me->mv_api_type
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_delivery.

    IF et_delivery IS INITIAL.
      RETURN.
    ENDIF.

    IF mv_mode = /vpcoe/cl_common_helper=>sc_mode-send.
      APPEND INITIAL LINE TO lt_json ASSIGNING FIELD-SYMBOL(<ls_json>).

      <ls_json>-elements = /vpcoe/cl_common_helper=>serialize_json( EXPORTING is_data        = VALUE /vpcoe/cl_rdp_delivery_data=>gty_s_delivery_json( source   = me->mv_source
                                                                                                                                                       elements = et_delivery )
                                                                              iv_pretty_name = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case ).
      <ls_json>-count = lines( et_delivery ).

      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null'.
      REPLACE ALL OCCURRENCES OF ':"0000-00-00"' IN <ls_json>-elements WITH ': null'.

      CALL BADI lo_badi->adjust_json
        EXPORTING
          iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
          iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
          iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
          io_log      = me->mo_log
        CHANGING
          ct_json     = lt_json.

      es_json = <ls_json>.
    ENDIF.

  ENDMETHOD.


  METHOD change_status.
    IF iv_test_run = abap_false.
      CALL FUNCTION 'CHANGE_POINTERS_STATUS_WRITE'
        EXPORTING
          message_type           = mv_chng_pointer_id
        TABLES
          change_pointers_idents = mt_cpi.
    ENDIF.
  ENDMETHOD.


  METHOD close_replication.
   "Shoud be redefined in SUMA Handler
    RETURN.
  ENDMETHOD.


  METHOD constructor.
    me->mv_api_type     = iv_api_type.
    me->mv_package_size = iv_package_size.
    me->mv_source       = iv_source.
    me->mv_mode         = iv_mode.
    me->mo_log          = io_log.

    SELECT *
      INTO TABLE me->mt_twlad
         FROM twlad.

    IF sy-subrc = 0.
      SELECT DISTINCT addrnumber, country
        INTO TABLE @me->mt_ship_from_adrc
          FROM adrc
            FOR ALL ENTRIES IN @me->mt_twlad
              WHERE addrnumber = @me->mt_twlad-adrnr.
      IF sy-subrc <> 0.
        CLEAR me->mt_twlad.
      ENDIF.
    ENDIF.

    SELECT werks, land1, regio
      INTO CORRESPONDING FIELDS OF TABLE @me->mt_t001w
        FROM t001w.
    IF sy-subrc <> 0.
      CLEAR me->mt_t001w.
    ENDIF.

  ENDMETHOD.


  METHOD decrease_wp_count.

    DATA: lt_messages TYPE bapiret2_t,
          lv_failed   TYPE xfeld,
          lv_status   TYPE i,
          lv_reason   TYPE string.

    RECEIVE RESULTS FROM FUNCTION '/VPCOE/SEND_JSON_BCKGRND'
      IMPORTING
        ev_failed             = lv_failed
        ev_reason             = lv_reason
        ev_status             = lv_status
      TABLES
        et_messages           = lt_messages
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        OTHERS                = 3.

    DATA(lv_sysubrc) = sy-subrc.

    READ TABLE me->mt_db_json ASSIGNING FIELD-SYMBOL(<ls_db_json>) WITH KEY task_id = p_task.
    IF sy-subrc = 0.

      <ls_db_json>-failed          = lv_failed.
      <ls_db_json>-response_status = lv_status.
      <ls_db_json>-response_message = lv_reason.

      IF lv_sysubrc = 0.
        <ls_db_json>-processing_ended = abap_true.
        me->mo_log->add_bapiret( EXPORTING it_bapiret2_t = lt_messages ).
      ELSE.
        <ls_db_json>-do_wait  = abap_true.
        <ls_db_json>-attempts = <ls_db_json>-attempts + 1.
      ENDIF.

      <ls_db_json>-processing_started = abap_false.

      me->mv_tasks_running = me->mv_tasks_running - 1.
    ENDIF.

  ENDMETHOD.


  METHOD get_data_for_xls.
    CLEAR: et_delivery,
           et_delivery_item,
           et_billdi.

    me->get_delivery(
      EXPORTING
        io_log         = io_log
        is_sel_opt     = is_sel_opt
        io_log_billdoc = io_log_bil_doc
      IMPORTING
        et_billdi = et_billdi  ).

    LOOP AT ms_delivery_tables-delivery ASSIGNING FIELD-SYMBOL(<ls_delivery>).
      APPEND INITIAL LINE TO et_delivery ASSIGNING FIELD-SYMBOL(<ls_delivery_xls>).
      <ls_delivery_xls> = CORRESPONDING #( <ls_delivery> ).
      <ls_delivery_xls>-formula = `=VLOOKUP(RC[-1],'Code list Delivery'!R1C1:R6C2,2,FALSE)`.
    ENDLOOP.

    me->get_items(
          EXPORTING
            it_delivery = ms_delivery_tables-delivery
            is_sel_opt  = is_sel_opt
          IMPORTING
            et_items    = DATA(lt_items) ).

    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_delivery_items>).
      APPEND INITIAL LINE TO et_delivery_item ASSIGNING FIELD-SYMBOL(<ls_delivery_item_xls>).
      <ls_delivery_item_xls> = CORRESPONDING #( <ls_delivery_items> ).
      <ls_delivery_item_xls>-formula = `=VLOOKUP(RC[-1],'Code list DeliveryItem'!R1C1:R100C2,2,FALSE)`.
    ENDLOOP.

  ENDMETHOD.


METHOD get_delivery.
  TYPES: BEGIN OF lty_s_kna1,
           kunnr TYPE kna1-kunnr,
           land1 TYPE kna1-land1,
           regio TYPE kna1-regio,
         END OF lty_s_kna1.

  DATA: lo_badi                TYPE REF TO /vpcoe/adjust_data_retrieval,
        lt_kna1                TYPE SORTED TABLE OF lty_s_kna1 WITH NON-UNIQUE KEY kunnr,
        lv_skip                TYPE abap_bool,
        lv_session_id          TYPE raw16,
        lv_session_item        TYPE int4,
        lt_delivery_hdr	       TYPE /vpcoe/t_delivery_hdr,
        lt_delivery_pack       TYPE /vpcoe/t_delivery_hdr,
        lt_items               TYPE /vpcoe/t_delivery_items,
        lt_delivery_items_pack TYPE /vpcoe/t_delivery_items,
        lt_delivery_combined   TYPE /vpcoe/cl_rdp_delivery_data=>gty_t_delivery_combined,
        lt_customer_to_be_send TYPE /vpcoe/cl_rdp_customer_data=>gty_t_customer,
        lv_ship_to_party       TYPE parvw VALUE 'WE',
        lv_where_for_dates     TYPE string,
        lv_step_counter        TYPE int4,
        lv_progress            TYPE int4,
        lt_session_id          TYPE /vpcoe/tt_r_raw16.

  CLEAR: ev_failed,
         et_billdi,
         et_delivery.

  ev_no_data = abap_true.

  me->mo_log = io_log.
  DATA(lv_max_count) = /vpcoe/cl_common_helper=>get_max_lines_count( ).

  GET BADI lo_badi.
  CALL BADI lo_badi->skip_selection
    EXPORTING
      iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
      iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
      iv_level    = /vpcoe/cl_common_helper=>sc_level-dlvr_bckgrnd
      iv_api_type = me->mv_api_type
      iv_mode     = me->mv_mode
      io_log      = me->mo_log
      is_sel_opt  = is_sel_opt
    CHANGING
      cv_skip     = lv_skip.

  lv_session_id = /vpcoe/cl_common_helper=>generate_session_id( ).

  IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-document.
    me->mv_package_size = '2147483647'. "max for INT4
  ENDIF.

  ASSIGN ct_total_count[ sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-delivery ] TO FIELD-SYMBOL(<ls_sum>).
  IF sy-subrc <> 0.
    APPEND INITIAL LINE TO ct_total_count ASSIGNING <ls_sum>.
    <ls_sum>-sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-delivery.
  ENDIF.

  IF lv_skip = abap_false.
    IF is_sel_opt-crt_date IS NOT INITIAL AND is_sel_opt-chng_date IS INITIAL.
      lv_where_for_dates  =  '( likp~erdat IN @is_sel_opt-crt_date )'.
    ELSEIF is_sel_opt-crt_date IS INITIAL AND is_sel_opt-chng_date IS NOT INITIAL.
      lv_where_for_dates =  '( likp~aedat IN @is_sel_opt-chng_date )'.
    ELSE.
      lv_where_for_dates =  '( likp~erdat IN @is_sel_opt-crt_date OR likp~aedat IN @is_sel_opt-chng_date )'.
    ENDIF.

    SELECT kunnr, land1, regio
      FROM kna1
      INTO TABLE @lt_kna1.
    IF sy-subrc <> 0.
      CLEAR lt_kna1.
    ENDIF.

    DATA(lv_db_connection) = /vpcoe/cl_common_helper=>get_db_connection(
                                    EXPORTING iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
                                              iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
                                              iv_level    = /vpcoe/cl_common_helper=>sc_level-dlvr_bckgrnd
                                              iv_api_type = me->mv_api_type
                                              is_sel_opt  = is_sel_opt ).

    " prepare dynamic columns selection for S4H and ECC
    " Case: if S4H system join will be on LIKP table
    " if ECC system join will be on VBUK table
    me->add_dynamic_columns(
      IMPORTING
        ev_columns = DATA(lv_columns) ).

    SELECT COUNT(*)
          FROM likp LEFT JOIN vbuk ON likp~vbeln = vbuk~vbeln
                    LEFT JOIN lips ON likp~vbeln = lips~vbeln
                    LEFT JOIN kna1 ON likp~kunnr = kna1~kunnr
                    LEFT JOIN kna1 AS kna_sh ON likp~kunag = kna1~kunnr
                    JOIN vbpa ON likp~vbeln = vbpa~vbeln
                    JOIN adrc ON vbpa~adrnr = adrc~addrnumber
             WHERE likp~lfart    IN @is_sel_opt-document_type
              AND likp~vkorg     IN @is_sel_opt-sales_org
              AND likp~kunnr     IN @is_sel_opt-ship_to_party
              AND likp~vbeln     IN @is_sel_opt-vbeln
              AND likp~wadat_ist IN @is_sel_opt-actual_goods_movement_date
              AND (lv_where_for_dates)
              AND (    ( likp~vbtyp = 'J' AND vbpa~parvw  = @lv_ship_to_party )
                    OR ( likp~vbtyp = 'T' AND vbpa~parvw  = @lv_ship_to_party )
                    OR ( likp~vbtyp <> 'J' AND likp~vbtyp <> 'T' ) )
              AND likp~spe_loekz = @abap_false
              AND lips~pstyv  IN @is_sel_opt-category
              AND lips~vtweg  IN @is_sel_opt-distribution
              AND lips~spart  IN @is_sel_opt-division
              AND lips~matnr  IN @is_sel_opt-matnr
              AND adrc~region IN @is_sel_opt-region
         INTO @DATA(lv_total_lines)
            CONNECTION (lv_db_connection).

    IF sy-subrc = 0.
      SELECT DISTINCT (lv_columns)
            FROM likp  LEFT JOIN vbuk ON likp~vbeln = vbuk~vbeln
                       LEFT JOIN lips ON likp~vbeln = lips~vbeln
                       LEFT JOIN kna1 ON likp~kunnr = kna1~kunnr
                       LEFT JOIN kna1 AS kna_sh ON likp~kunag = kna1~kunnr
                      JOIN vbpa ON likp~vbeln = vbpa~vbeln
                      JOIN adrc ON vbpa~adrnr = adrc~addrnumber

               WHERE likp~lfart    IN @is_sel_opt-document_type
                AND likp~vkorg     IN @is_sel_opt-sales_org
                AND likp~kunnr     IN @is_sel_opt-ship_to_party
                AND likp~vbeln     IN @is_sel_opt-vbeln
                AND likp~wadat_ist IN @is_sel_opt-actual_goods_movement_date
                AND (lv_where_for_dates)
                AND (    ( likp~vbtyp = 'J' AND vbpa~parvw  = @lv_ship_to_party )
                      OR ( likp~vbtyp = 'T' AND vbpa~parvw  = @lv_ship_to_party )
                      OR ( likp~vbtyp <> 'J' AND likp~vbtyp <> 'T' ) )
                AND likp~spe_loekz = @abap_false
                AND lips~pstyv  IN @is_sel_opt-category
                AND lips~vtweg  IN @is_sel_opt-distribution
                AND lips~spart  IN @is_sel_opt-division
                AND lips~matnr  IN @is_sel_opt-matnr
                AND adrc~region    IN @is_sel_opt-region
         INTO CORRESPONDING FIELDS OF TABLE @lt_delivery_hdr
           CONNECTION (lv_db_connection)
             PACKAGE SIZE @lv_max_count.

        " Get ShipToCountry and ShipToRegion based on the Priority
        LOOP AT lt_delivery_hdr ASSIGNING FIELD-SYMBOL(<ls_delivery_hdr>).
          IF <ls_delivery_hdr>-sold_to_party IS INITIAL.
            <ls_delivery_hdr>-sold_to_party = <ls_delivery_hdr>-ship_to_party.
          ENDIF.

          IF <ls_delivery_hdr>-ship_to_country IS INITIAL.
            <ls_delivery_hdr>-ship_to_country = VALUE #( lt_kna1[ kunnr = <ls_delivery_hdr>-ship_to_party ]-land1 OPTIONAL ).
            IF <ls_delivery_hdr>-ship_to_country IS INITIAL.
              <ls_delivery_hdr>-ship_to_country = VALUE #( lt_kna1[ kunnr = <ls_delivery_hdr>-sold_to_party ]-land1 OPTIONAL ).
            ENDIF.

          ENDIF.

          IF <ls_delivery_hdr>-ship_to_region IS INITIAL.
            <ls_delivery_hdr>-ship_to_region = VALUE #( lt_kna1[ kunnr = <ls_delivery_hdr>-ship_to_party ]-regio OPTIONAL ).
            IF <ls_delivery_hdr>-ship_to_region IS INITIAL.
              <ls_delivery_hdr>-ship_to_region = VALUE #( lt_kna1[ kunnr = <ls_delivery_hdr>-sold_to_party ]-regio OPTIONAL ).
            ENDIF.

          ENDIF.
        ENDLOOP.

        CALL BADI lo_badi->adjust_data_retrieval
          EXPORTING
            iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
            iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
            iv_level    = /vpcoe/cl_common_helper=>sc_level-dlvr_bckgrnd
            iv_api_type = me->mv_api_type
            is_sel_opt  = is_sel_opt
            iv_mode     = me->mv_mode
            io_log      = me->mo_log
          CHANGING
            ct_data     = lt_delivery_hdr.

        lv_step_counter = lv_step_counter + 1.
        lv_progress = ( lv_max_count * lv_step_counter * 100 ) DIV lv_total_lines.
        me->mo_log->add_msg_progress( EXPORTING iv_progress = lv_progress iv_level = CONV #( 'Delivery' ) ).

        lv_session_item = lv_session_item + 1.

        DELETE lt_delivery_hdr WHERE ship_to_country NOT IN is_sel_opt-country.

        IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-document.
          INSERT LINES OF lt_delivery_hdr INTO TABLE ms_delivery_tables-delivery.
        ENDIF.

        me->get_items(
          EXPORTING
            it_delivery = lt_delivery_hdr
            is_sel_opt  = is_sel_opt
          IMPORTING
            et_items    = lt_items ).

        IF lt_items IS INITIAL.
          CONTINUE.
        ENDIF.

        IF mv_mode = /vpcoe/cl_common_helper=>sc_mode-document.
          INSERT LINES OF lt_items INTO TABLE ms_delivery_tables-items.
        ENDIF.

        ev_no_data = abap_false.

        IF is_sel_opt-get_billdi = abap_true.
          lt_session_id =  me->handle_billdi( EXPORTING it_dlv_hdr   = lt_delivery_hdr
                                                        it_dlv_items = lt_items
                                                        is_sel_opt   = is_sel_opt ).
        ENDIF.

        LOOP AT lt_delivery_hdr ASSIGNING <ls_delivery_hdr>.
          "check if customer data should be send in advance
          READ TABLE me->mt_changed_customer ASSIGNING FIELD-SYMBOL(<ls_customer>) WITH TABLE KEY id = <ls_delivery_hdr>-ship_to_party.
          IF sy-subrc = 0.
            INSERT <ls_customer> INTO TABLE lt_customer_to_be_send.
            DELETE me->mt_changed_customer WHERE id = <ls_delivery_hdr>-ship_to_party.
          ENDIF.

          READ TABLE me->mt_changed_customer ASSIGNING <ls_customer> WITH TABLE KEY id = <ls_delivery_hdr>-sold_to_party.
          IF sy-subrc = 0.
            INSERT <ls_customer> INTO TABLE lt_customer_to_be_send.
            DELETE me->mt_changed_customer WHERE id = <ls_delivery_hdr>-sold_to_party.
          ENDIF.

          INSERT <ls_delivery_hdr> INTO TABLE lt_delivery_pack.

          IF lines( lt_delivery_pack ) = me->mv_package_size.
            me->build_json( EXPORTING
                             it_delivery_hdr = lt_delivery_pack
                             it_delivery_itm = lt_items
                             io_log = io_log
                            IMPORTING
                             es_json         = DATA(ls_json)
                             et_delivery     = lt_delivery_combined ).

            INSERT LINES OF lt_delivery_combined INTO TABLE et_delivery.

            IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-send AND ls_json-count > 0 AND is_sel_opt-send_delivery = abap_true.
              lv_session_item = lv_session_item + 1.
              CALL FUNCTION '/VPCOE/STORE_JSON_BCKGRND'
                EXPORTING
                  iv_api_type     = me->mv_api_type
                  iv_srv_grp      = /vpcoe/cl_common_helper=>sc_grp_id-delivery
                  iv_srv_id       = /vpcoe/cl_common_helper=>sc_service_id-delivery
                  iv_session_id   = lv_session_id
                  iv_session_item = lv_session_item
                  is_json         = ls_json.
            ENDIF.
            CLEAR lt_delivery_pack.
          ENDIF.
          IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-screen.
            IF lines( et_delivery ) >= /vpcoe/cl_common_helper=>gc_display_amount.
              DELETE et_delivery FROM /vpcoe/cl_common_helper=>gc_display_amount + 1. "For Display Mode only first past of the data
              RETURN.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDSELECT.

      IF lt_delivery_pack IS NOT INITIAL.
        me->build_json( EXPORTING
                           it_delivery_hdr = lt_delivery_pack
                           it_delivery_itm = lt_items
                        IMPORTING
                           es_json         = ls_json
                           et_delivery     = lt_delivery_combined ).

        INSERT LINES OF lt_delivery_combined INTO TABLE et_delivery.

        IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-send AND ls_json-count > 0 AND is_sel_opt-send_delivery = abap_true.
          lv_session_item = lv_session_item + 1.
          CALL FUNCTION '/VPCOE/STORE_JSON_BCKGRND'
            EXPORTING
              iv_api_type     = me->mv_api_type
              iv_srv_grp      = /vpcoe/cl_common_helper=>sc_grp_id-delivery
              iv_srv_id       = /vpcoe/cl_common_helper=>sc_service_id-delivery
              iv_session_id   = lv_session_id
              iv_session_item = lv_session_item
              is_json         = ls_json.
        ENDIF.

        CLEAR lt_delivery_pack.
      ENDIF.

    ELSE.
      CLEAR lv_total_lines.
    ENDIF.

    IF ev_no_data = abap_true.
      RETURN.
    ENDIF.

  ELSE.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
        iv_level    = /vpcoe/cl_common_helper=>sc_level-dlvr_bckgrnd
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = lt_delivery_hdr.

    IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-document.
      ms_delivery_tables-delivery = lt_delivery_hdr .
      RETURN.
    ENDIF.

    IF is_sel_opt-get_billdi = abap_true.
      lt_session_id = me->handle_billdi( EXPORTING it_dlv_hdr    = lt_delivery_hdr
                                                   it_dlv_items  = lt_items
                                                   is_sel_opt    = is_sel_opt ).
    ENDIF.

    IF lt_delivery_hdr IS NOT INITIAL.
      ev_no_data = abap_false.
      me->get_items(
         EXPORTING
           it_delivery = lt_delivery_hdr
           is_sel_opt  = is_sel_opt
         IMPORTING
           et_items    = lt_items ).

      IF lt_items IS NOT INITIAL.
        LOOP AT lt_delivery_hdr INTO DATA(lgr_delivery_hdr) GROUP BY ( sy-tabix - 1 ) DIV me->mv_package_size + 1.

          CLEAR: lt_delivery_pack, lt_delivery_items_pack.

          IF NOT line_exists( lt_items[ KEY vbeln_itm_cat COMPONENTS vbeln = lgr_delivery_hdr-id ] ).
            CONTINUE.
          ENDIF.

          LOOP AT GROUP lgr_delivery_hdr ASSIGNING FIELD-SYMBOL(<ls_delivery>) .
            READ TABLE me->mt_changed_customer ASSIGNING <ls_customer> WITH TABLE KEY id = <ls_delivery_hdr>-ship_to_party.
            IF sy-subrc = 0.
              INSERT <ls_customer> INTO TABLE lt_customer_to_be_send.
              DELETE me->mt_changed_customer WHERE id = <ls_delivery_hdr>-ship_to_party.
            ENDIF.

            READ TABLE me->mt_changed_customer ASSIGNING <ls_customer> WITH TABLE KEY id = <ls_delivery_hdr>-sold_to_party.
            IF sy-subrc = 0.
              INSERT <ls_customer> INTO TABLE lt_customer_to_be_send.
              DELETE me->mt_changed_customer WHERE id = <ls_delivery_hdr>-sold_to_party.
            ENDIF.

            INSERT <ls_delivery> INTO TABLE lt_delivery_pack.

            INSERT LINES OF VALUE /vpcoe/t_delivery_items( FOR <ls_items> IN lt_items USING KEY vbeln_itm_cat
                                                               WHERE ( vbeln = <ls_delivery>-id ) ( <ls_items> ) )
                INTO TABLE lt_delivery_items_pack.
          ENDLOOP.

          <ls_sum>-total = <ls_sum>-total + lines( lt_delivery_pack ).
          lv_session_item = lv_session_item + 1.

          me->build_json( EXPORTING
                            it_delivery_hdr = lt_delivery_pack
                            it_delivery_itm = lt_delivery_items_pack
                          IMPORTING
                            es_json         = ls_json
                            et_delivery     = lt_delivery_combined ).

          INSERT LINES OF lt_delivery_combined INTO TABLE et_delivery.

          IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-send AND ls_json-count > 0 AND is_sel_opt-send_delivery = abap_true.
            lv_session_item = lv_session_item + 1.
            CALL FUNCTION '/VPCOE/STORE_JSON_BCKGRND'
              EXPORTING
                iv_api_type     = me->mv_api_type
                iv_srv_grp      = /vpcoe/cl_common_helper=>sc_grp_id-delivery
                iv_srv_id       = /vpcoe/cl_common_helper=>sc_service_id-delivery
                iv_session_id   = lv_session_id
                iv_session_item = lv_session_item
                is_json         = ls_json.
          ENDIF.

          IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-screen.
            IF lines( et_delivery ) >= /vpcoe/cl_common_helper=>gc_display_amount.
              DELETE et_delivery FROM /vpcoe/cl_common_helper=>gc_display_amount + 1. "For Display Mode only first past of the data
              RETURN.
            ENDIF.
          ENDIF.

        ENDLOOP.

      ENDIF.
    ELSE.
      ev_no_data = abap_true.
      RETURN.

    ENDIF.
  ENDIF.

  IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-send AND is_sel_opt-send_delivery = abap_true.
    IF lt_customer_to_be_send IS NOT INITIAL.
      me->send_changed_customer( lt_customer_to_be_send ).
      MESSAGE i093(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      io_log->add_sy_msg( ).
    ENDIF.

    NEW /vpcoe/cl_rdp_payload_handler( )->send_payload_multi(
      EXPORTING
        it_r_session_ids = VALUE #( ( sign = 'I' option = 'EQ' low = lv_session_id ) )
        iv_api_type      = me->mv_api_type
        iv_srv_id        = /vpcoe/cl_common_helper=>sc_service_id-delivery
        io_log           = io_log
        io_cust          = io_cust
      CHANGING
        cv_total         = <ls_sum>-total
        cv_total_failed  = <ls_sum>-total_failed ).
  ENDIF.

* Send Billing Document Items
  IF is_sel_opt-get_billdi = abap_true.
    NEW /vpcoe/cl_rdp_billdocit_data( me->mv_mode )->process_billing_doc_items(
      EXPORTING
        io_log          = io_log_billdoc
        iv_service_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
        iv_send         = is_sel_opt-send_bil_doc
        is_sel_opt      = VALUE #( bdocdate       = is_sel_opt-bdocdate
                                   bsddoccat      = is_sel_opt-bsddoccat
                                   billdoc        = is_sel_opt-billdoc
                                   billdi_country = is_sel_opt-billdi_country
                                   billdoc_session_id = lt_session_id )

      IMPORTING
        et_billdi       = et_billdi
      CHANGING
        ct_total_count_billdoc = ct_total_count_billdoc ).

  ENDIF.

ENDMETHOD.


  METHOD get_excel_header.

    CLEAR: et_header,
           ev_code_list,
           ev_header,
           ev_item,
           et_header_list,
           et_code_list,
           ev_header_list.

    ev_header    = 'Delivery'.
    ev_item      = 'DeliveryItem'.
    ev_code_list = 'Code list Delivery'.
    ev_code_list_deliveryitem = 'Code list DeliveryItem'.
    ev_header_list_deliveryitem = 'SD Document Category'.
    ev_header_list = 'Overall Goods Movement Status'.
    et_header_list = VALUE #( ( description = 'Code'        vpcoe_attribute = 'ID' )
                              ( description = 'Description' vpcoe_attribute = 'DESCRIPTION' ) ).

    et_code_list = VALUE #( ( id = '' description = 'Not relevant' )
                            ( id = 'A' description = 'Not yet processed' )
                            ( id = 'B' description = 'Partially processed' )
                            ( id = 'C' description = 'Completely processed' ) ).

     Select domvalue_l as id, ddtext as description
      from dd07t
      WHERE domname  = 'VBTYP'
        AND ddlanguage = 'E'
    INTO TABLE @et_code_list_deliveryitem.

    et_header = VALUE #( ( description      = 'Delivery'
                             internal_name    = 'id'
                             data_type        = 'CHAR(10)'
                             mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                             s4hana_attribute = 'VBELN'
                             vpcoe_attribute  = 'ID'
                             is_key           = abap_true )
                           ( description      = 'Sales Organization'
                             internal_name    = 'salesOrganization'
                             data_type        = 'CHAR(4)'
                             mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                             s4hana_attribute = 'VKORG'
                             vpcoe_attribute  = 'SALES_ORGANIZATION'
                             is_key           = abap_false )
                           ( description      = 'Delivery Type'
                             internal_name    = 'deliveryDocumentType'
                             data_type        = 'CHAR(4)'
                             mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                             s4hana_attribute = 'LFART'
                             vpcoe_attribute  = 'TYPE'
                             is_key           = abap_false )
                           ( description      = 'Sold-To Party'
                             internal_name    = 'soldToParty'
                             data_type        = 'CHAR(10)'
                             mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                             s4hana_attribute = 'KUNNR'
                             vpcoe_attribute  = 'SOLD_TO_PARTY'
                             is_key           = abap_false )
                           ( description      = 'Ship-To Party'
                             internal_name    = 'shipToParty'
                             data_type        = 'CHAR(10)'
                             mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                             s4hana_attribute = 'KUNNR'
                             vpcoe_attribute  = 'SHIP_TO_PARTY'
                             is_key           = abap_false )
                           ( description      = 'Ship-To State/Province'
                             internal_name    = 'shipToRegion'
                             data_type        = 'CHAR(3)'
                             mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                             s4hana_attribute = 'REGION'
                             vpcoe_attribute  = 'SHIP_TO_REGION'
                             is_key           = abap_false )
                           ( description      = 'Ship-To Country/Region'
                             internal_name    = 'shipToCountry'
                             data_type        = 'CHAR(3)'
                             mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                             s4hana_attribute = 'COUNTRY'
                             vpcoe_attribute  = 'SHIP_TO_COUNTRY'
                             is_key           = abap_false )
                           ( description      = 'Actual Goods Movement Date'
                             internal_name    = 'actualGoodsMovementDate'
                             data_type        = 'Date'
                             mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                             s4hana_attribute = 'WADAT_IST'
                             vpcoe_attribute  = 'ACTUAL_GOODS_MOVEMENT_DATE'
                             is_key           = abap_false )
                             ( description    = 'Overall Goods Movement Status'
                               internal_name  = 'overallGoodsMovementStatus'
                               data_type      = 'CHAR(1)'
                               mandatory      = 'optional'
                               s4hana_attribute = 'WBSTK'
                               vpcoe_attribute  = 'OVERALL_GOODS_MOVEMENT_STATUS' )
                               ( description    = ''
                               internal_name  = ''
                               data_type      = ''
                               mandatory      = ''
                               s4hana_attribute = ''
                               vpcoe_attribute  = 'FORMULA' )
                              ( description     = 'Incoterms'
                                internal_name   = 'incoterms'
                                data_type       = 'CHAR(3)'
                                mandatory       = 'optional'
                                s4hana_attribute = 'LIKP.INCO1'
                                vpcoe_attribute = 'INCOTERMS' ) ).

    et_header_itm = VALUE #( ( description      = 'Delivery'
                   internal_name    = 'delivery'
                   data_type        = 'CHAR(10)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                   s4hana_attribute = 'VBELN'
                   vpcoe_attribute  = 'VBELN'
                   is_key           = abap_true )
                 ( description      = 'Delivery Item'
                   internal_name    = 'position'
                   data_type        = 'Num(6)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                   s4hana_attribute = 'POSNR'
                   vpcoe_attribute  = 'ID'
                   is_key           = abap_true )
                 ( description      = 'Delivery Item Category'
                   internal_name    = 'category'
                   data_type        = 'CHAR(4)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                   s4hana_attribute = 'PSTYV'
                   vpcoe_attribute  = 'CATEGORY' )
                 ( description      = 'Product Number'
                   internal_name    = 'product'
                   data_type        = 'CHAR(40)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                   s4hana_attribute = 'MATNR'
                   vpcoe_attribute  = 'PRODUCT')
                 ( description      = 'Plant'
                   internal_name    = 'plant'
                   data_type        = 'CHAR(4)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                   s4hana_attribute = 'WERKS'
                   vpcoe_attribute  = 'PLANT')
                 ( description      = 'Batch'
                   internal_name    = 'batch'
                   data_type        = 'CHAR(10)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                   s4hana_attribute = 'CHARG'
                   vpcoe_attribute  = 'BATCH')
                 ( description      = 'Actual Quantity Delivered (in sales units)'
                   internal_name    = 'salesQuantity'
                   data_type        = 'DEC(13,3)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                   s4hana_attribute = 'LFIMG'
                   vpcoe_attribute  = 'SALES_QUANTITY')
                 ( description      = 'Sales Unit'
                   internal_name    = 'salesUnit'
                   data_type        = 'CHAR(3)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                   s4hana_attribute = 'VRKME'
                   vpcoe_attribute  = 'SALES_UNIT')
                 ( description      = 'Base  Quantity'
                   internal_name    = 'baseQuantity'
                   data_type        = 'DEC(13,3)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                   s4hana_attribute = 'LGMNG'
                   vpcoe_attribute  = 'BASE_QUANTITY')
                 ( description      = 'Base Unit'
                   internal_name    = 'baseUnit'
                   data_type        = 'CHAR(3)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                   s4hana_attribute = 'MEINS'
                   vpcoe_attribute  = 'BASE_UNIT')
                   ( description      = 'Ship-From Country/Region'
                   internal_name    = 'shipFromCountry'
                   data_type        = 'CHAR(3)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                   s4hana_attribute = 'COUNTRY'
                   vpcoe_attribute  = 'SHIP_FROM_COUNTRY')
                 ( description      = 'Distribution Channel'
                   internal_name    = 'distributionChannel'
                   data_type        = 'CHAR(3)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                   s4hana_attribute = 'VTWEG'
                   vpcoe_attribute  = 'DISTRIBUTION_CHANNEL')
                 ( description      = 'Division'
                   internal_name    = 'division'
                   data_type        = 'CHAR(2)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                   s4hana_attribute = 'SPART'
                   vpcoe_attribute  = 'DIVISION')
                 ( description      = 'Reference SD Document'
                   internal_name    = 'referenceSDDocument'
                   data_type        = 'CHAR(10)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                   s4hana_attribute = 'VGBEL'
                   vpcoe_attribute  = 'REFERENCE_S_D_DOCUMENT')
                 ( description      = 'Reference SD Document Item'
                   internal_name    = 'referenceSDDocumentItem'
                   data_type        = 'CHAR(6)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                   s4hana_attribute = 'VGPOS'
                   vpcoe_attribute  = 'REFERENCE_S_D_DOCUMENT_ITEM')
                 ( description      = 'Reference SD Document Category'
                   internal_name    = 'referenceSDDocumentCategory'
                   data_type        = 'CHAR(4)'
                   mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                   s4hana_attribute = 'VGTYPL_V'
                   vpcoe_attribute  = 'REFERENCE_S_D_DOCUMENTCATEGORY')
                 ( description    = ''
                   internal_name  = ''
                   data_type      = ''
                   mandatory      = ''
                   s4hana_attribute = ''
                   vpcoe_attribute  = 'FORMULA' ) ).

  ENDMETHOD.


  METHOD get_items.
    DATA: lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval,
          lv_skip TYPE abap_bool.

    CLEAR et_items.

    GET BADI lo_badi.

    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
        iv_level    = /vpcoe/cl_common_helper=>sc_level-itm
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false AND it_delivery IS NOT INITIAL.

      DATA(lv_db_connection) = /vpcoe/cl_common_helper=>get_db_connection(
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
        iv_level    = /vpcoe/cl_common_helper=>sc_level-itm
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt ).

      SELECT lips~vbeln AS vbeln,
             lips~posnr AS id,
             lips~pstyv AS category,
             lips~matnr AS product,
             lips~lgmng AS base_quantity,
             lips~meins AS base_unit,
             lips~lfimg AS sales_quantity,
             lips~vrkme AS sales_unit,
             lips~charg AS batch,
             lips~werks AS plant,
             lips~vtweg AS distribution_channel,
             lips~spart AS division,
             lips~lgort AS lgort,
             lips~vgbel AS reference_s_d_document,
             lips~vgpos AS reference_s_d_document_item,
             lips~vgtyp AS reference_s_d_documentcategory
         INTO CORRESPONDING FIELDS OF TABLE @et_items
         CONNECTION (lv_db_connection)
           FROM lips LEFT JOIN t001w ON lips~werks = t001w~werks
             FOR ALL ENTRIES IN @it_delivery
                WHERE lips~vbeln = @it_delivery-id
                  AND lips~pstyv  IN @is_sel_opt-category
                  AND lips~vtweg  IN @is_sel_opt-distribution
                  AND lips~spart  IN @is_sel_opt-division
                  AND lips~matnr  IN @is_sel_opt-matnr
                  AND t001w~land1 IN @is_sel_opt-plnt_country.
      IF sy-subrc <> 0.
        CLEAR et_items.
      ENDIF.

      IF /vpcoe/cl_common_helper=>get_conf_param_value( /vpcoe/cl_common_helper=>sc_config_param-dlv_catch_weight ) IS NOT INITIAL.
        LOOP AT et_items ASSIGNING FIELD-SYMBOL(<ls_item>).
          IF <ls_item>-sales_quantity <> 0.
            <ls_item>-base_quantity = <ls_item>-sales_quantity.
            <ls_item>-base_unit     = <ls_item>-sales_unit.
          ENDIF.
        ENDLOOP.
      ENDIF.

      me->set_ship_from_country( CHANGING ct_delivery_item = et_items ).
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-delivery
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-delivery
        iv_level    = /vpcoe/cl_common_helper=>sc_level-itm
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_items.

  ENDMETHOD.


  METHOD handle_billdi.

    DATA: lt_dlv_hdr    TYPE SORTED TABLE OF /vpcoe/str_delivery_hdr WITH NON-UNIQUE KEY primary_key COMPONENTS id,
          lt_billdi     TYPE STANDARD TABLE OF /vpcoe/billdi,
          lv_session_id TYPE raw16.

    lt_dlv_hdr = it_dlv_hdr.

    LOOP AT it_dlv_items ASSIGNING FIELD-SYMBOL(<ls_dlvr_item>) .

      DATA(lv_ship_to_country) = VALUE #( lt_dlv_hdr[ id = <ls_dlvr_item>-vbeln ]-ship_to_country OPTIONAL ).

      IF lv_ship_to_country NOT IN is_sel_opt-billdi_country AND <ls_dlvr_item>-ship_from_country IN is_sel_opt-billdi_country.
        CLEAR lv_session_id.
        lv_session_id = /vpcoe/cl_common_helper=>generate_session_id( ).

        INSERT VALUE #( session_id                 = lv_session_id
                        mat_doc                    = <ls_dlvr_item>-vbeln "Delivery Number
                        delivery_document_item     = <ls_dlvr_item>-id "Delivery Position
                        delivery_document          = <ls_dlvr_item>-reference_s_d_document "Reference SD Document
                        reference_sd_document_item = <ls_dlvr_item>-reference_s_d_document_item "Reference SD Document Item
                        added_on                   = sy-datum
                        ship_to_country            = lv_ship_to_country
                        ship_from_country          = <ls_dlvr_item>-ship_from_country
                        )
                        INTO TABLE lt_billdi.

        INSERT VALUE #( sign = 'I' option = 'EQ' low = lv_session_id ) INTO TABLE rt_sesion_id.

      ENDIF.

    ENDLOOP.

    IF lt_billdi IS NOT INITIAL.
      MODIFY /vpcoe/billdi FROM TABLE lt_billdi.
      CALL FUNCTION 'DB_COMMIT'.
    ENDIF.

  ENDMETHOD.


METHOD send_changed_customer.

  IF it_customer IS INITIAL.
    RETURN.
  ENDIF.

  DATA(lo_cust_customer) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                     iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
                                                     iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer ).

  DATA(lo_customer_data) = NEW /vpcoe/cl_rdp_customer_data( iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                            iv_package_size = lo_cust_customer->get_package_size( )
                                                            iv_source       = /vpcoe/cl_rdp_helper=>get_source_id( ) ).

  lo_customer_data->build_json( EXPORTING it_customer = it_customer
                                IMPORTING et_json = DATA(lt_json_cust) ).

  MESSAGE i071(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
  me->mo_log->add_sy_msg( ).

  NEW /vpcoe/cl_rdp_payload_handler( )->send_payload(
    EXPORTING
      io_cust   = lo_cust_customer
      it_json   = lt_json_cust
      io_log    = me->mo_log
    IMPORTING
      ev_status = DATA(lv_status) ).

  IF lv_status <> /vpcoe/cl_rdp_http=>gc_s_http_status-ok AND me->mo_log->check( ).
    IF sy-batch = space.
      me->mo_log->display_message( iv_with_total = abap_false ).
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD set_changed_customer.

  me->mt_changed_customer = it_customer.

ENDMETHOD.


METHOD set_ship_from_country.

  DATA lv_adrnr TYPE  ad_addrnum.

  LOOP AT ct_delivery_item ASSIGNING FIELD-SYMBOL(<ls_delivery_item>).
    CLEAR lv_adrnr.

    IF <ls_delivery_item>-lgort IS NOT INITIAL.
      lv_adrnr = VALUE #( me->mt_twlad[ lgort = <ls_delivery_item>-lgort werks = <ls_delivery_item>-plant  ]-adrnr OPTIONAL ).
      IF lv_adrnr IS INITIAL.
        <ls_delivery_item>-ship_from_country = VALUE #( me->mt_t001w[ werks = <ls_delivery_item>-plant ]-land1 OPTIONAL ).
      ELSE.
        <ls_delivery_item>-ship_from_country = VALUE #( me->mt_ship_from_adrc[ addrnumber = lv_adrnr ]-country OPTIONAL ).
      ENDIF.
    ELSEIF <ls_delivery_item>-plant IS NOT INITIAL.
      <ls_delivery_item>-ship_from_country = VALUE #( me->mt_t001w[ werks = <ls_delivery_item>-plant ]-land1 OPTIONAL ).
    ENDIF.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
