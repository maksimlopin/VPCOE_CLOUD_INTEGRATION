class /VPCOE/CL_RDP_INVENTORY definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_stock_rdp,
        plant                       TYPE t001k-bwkey,
        product                     TYPE matnr,
        company_code                TYPE bukrs,
        stock_quantity_in_base_unit TYPE mseg-menge,
        base_unit_of_measure        TYPE mseg-meins,
      END OF gty_s_stock_rdp .
  types:
    gty_t_stock_rdp TYPE STANDARD TABLE OF gty_s_stock_rdp WITH DEFAULT KEY .
  types:
    BEGIN OF gty_s_stock_period,
        calendar_month TYPE numc2,
        calendar_year  TYPE mseg-mjahr,
      END OF gty_s_stock_period .
  types:
    BEGIN OF gty_s_stock_data_rdp.
            INCLUDE TYPE gty_s_stock_rdp.
    TYPES period   TYPE gty_s_stock_period.
    TYPES:
       END OF gty_s_stock_data_rdp .
  types:
    gty_t_stock_data_rdp TYPE STANDARD TABLE OF gty_s_stock_data_rdp WITH DEFAULT KEY .
  types:
    BEGIN OF gty_s_stock_data_out_rdp,
        source TYPE  text10.
    TYPES elements TYPE  gty_t_stock_data_rdp.
    TYPES: END OF gty_s_stock_data_out_rdp .
  types:
    gty_r_co_code TYPE RANGE OF t001-bukrs .

  methods DOWNLOAD_EXCEL
    importing
      !IV_FILE_PATH type STRING
      !IT_INVENTORY type GTY_T_STOCK_DATA_RDP
      !IV_SAVE_BACKGROUND type ABAP_BOOL
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
  methods CONSTRUCTOR
    importing
      !IV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE
      !IV_SOURCE type STRING
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND .
  methods GET_INVENTORY_STOCKS
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG
      !IV_TEST_RUN type FLAG
      !IS_SEL_OPT type /VPCOE/S_SELOPT_INVENTORY
      !IV_PATH type STRING
      !IO_CUST type ref to /VPCOE/CL_COMMON_HELPER
      !IV_SAVE_BACKGROUND type ABAP_BOOL
    exporting
      !EV_FAILED type ABAP_BOOL
      !EV_NO_DATA type ABAP_BOOL
    changing
      !CV_TOTAL_COUNT type I .
  methods GET_INVENTORY_RESULTS
    returning
      value(RT_STOCK_RESULT) type GTY_T_STOCK_RDP .
  class-methods GET_COMPANY_CODE
    importing
      !IT_R_PLANT type /VPCOE/TT_R_WERKS_D
      !IT_R_PRODUCT type /VPCOE/TT_R_MATNR
    returning
      value(RT_R_COMP_CODE) type GTY_R_CO_CODE .
PROTECTED SECTION.

  CLASS-DATA mt_inventory_results TYPE gty_t_stock_data_rdp .

  METHODS retrieve_stock_intransit
    IMPORTING
      !is_sel_opt            TYPE /vpcoe/s_selopt_inventory
      !io_log                TYPE REF TO /vpcoe/cl_rdp_log
    EXPORTING
      !ev_failed             TYPE xfeld
    RETURNING
      VALUE(rt_stock_result) TYPE gty_t_stock_data_rdp .
  METHODS store_temporary_data
    IMPORTING
      !it_inventory_stock TYPE gty_t_stock_data_rdp .
  METHODS clear_temporary_data .
  METHODS get_temporary_data
    EXPORTING
      !et_inventory_stock TYPE gty_t_stock_data_rdp .
  METHODS retrieve_stock_data
    IMPORTING
      !is_sel_opt            TYPE /vpcoe/s_selopt_inventory
      !io_log                TYPE REF TO /vpcoe/cl_rdp_log
    EXPORTING
      !ev_failed             TYPE xfeld
    RETURNING
      VALUE(rt_stock_result) TYPE gty_t_stock_data_rdp .
  METHODS get_additional_data_for_stocks
    CHANGING
      !ct_stock_result TYPE gty_t_stock_data_rdp .
  METHODS prepare_json
    IMPORTING
      !it_stock_result TYPE STANDARD TABLE
    EXPORTING
      !et_json         TYPE /vpcoe/cl_rdp_http=>gty_t_json .
  METHODS retrieve_stock
    IMPORTING
      !is_sel_opt            TYPE /vpcoe/s_selopt_inventory
      !io_log                TYPE REF TO /vpcoe/cl_rdp_log
    EXPORTING
      !ev_failed             TYPE xfeld
    RETURNING
      VALUE(rt_stock_result) TYPE gty_t_stock_data_rdp .
private section.

  data MV_ITEM_ID type INT4 .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SOURCE type STRING .
  data MV_MODE type /VPCOE/DE_MODE .
ENDCLASS.



CLASS /VPCOE/CL_RDP_INVENTORY IMPLEMENTATION.


  METHOD CLEAR_TEMPORARY_DATA.

    DELETE FROM /vpcoe/inventory WHERE uname = sy-uname.

  ENDMETHOD.


METHOD CONSTRUCTOR.

  me->mv_api_type     = iv_api_type.
  me->mv_package_size = iv_package_size.
  me->mv_source       = iv_source.
  me->mv_mode         = iv_mode.

ENDMETHOD.


  METHOD download_excel.

    DATA: lt_header TYPE /vpcoe/cl_xls_handler=>gty_t_title.

    "create excel header template
    lt_header = VALUE #(
                         ( description      = 'Product'
                           internal_name    = 'product'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.MATNR MARD.MATNR'
                           vpcoe_attribute  = 'PRODUCT'
                           is_key           = abap_true )
                         ( description      = 'Plant'
                           internal_name    = 'plant'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.WERKS MCHA.WERKS'
                           vpcoe_attribute  = 'PLANT'
                           is_key           = abap_true )
                         ( description      = 'Snapshot Month'
                           internal_name    = 'calendarMonth'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.MONTH_BUDAT MONTH_BUDAT'
                           vpcoe_attribute  = 'PERIOD-CALENDARMONTH'
                           is_key           = abap_true )
                         ( description      = 'Snapshot Year'
                           internal_name    = 'year'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.MJAHR MSEG.MJAHR'
                           vpcoe_attribute  = 'PERIOD-CALENDARYEAR'
                           is_key           = abap_true )
                         ( description      = 'Company Code'
                           internal_name    = 'companyCode'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.BUKRS MSEG.BUKRS'
                           vpcoe_attribute  = 'COMPANYCODE')
                         ( description      = 'Quantity In Base Unit'
                           internal_name    = 'quantityInBaseUnit'
                           data_type        = 'QUAN(13,3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.MENGE MSEG.MENGE'
                           vpcoe_attribute  = 'STOCKQUANTITYINBASEUNIT')
                         ( description      = 'Product Base Unit'
                           internal_name    = 'baseUnitOfMeasure'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.MEINS MSEG.MEINS'
                           vpcoe_attribute  = 'BASEUNITOFMEASURE') ).

    DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path = iv_file_path
                                                   iv_save_background = iv_save_background  ).

    lo_xls_file->execute( iv_name = CONV #( /vpcoe/cl_rdp_helper=>sc_service_id-inventory ) ).
    lo_xls_file->fill_data( it_item_tab = it_inventory
                            it_title    = lt_header ).

    lo_xls_file->save_xls_file( io_log = io_log ).

  ENDMETHOD.


  METHOD get_additional_data_for_stocks.
    DATA: lt_bukrs TYPE SORTED TABLE OF t001k WITH NON-UNIQUE KEY bwkey.

    SELECT bukrs, bwkey
      FROM t001k
        FOR ALL ENTRIES IN @ct_stock_result
          WHERE bwkey = @ct_stock_result-plant
        INTO CORRESPONDING FIELDS OF TABLE @lt_bukrs.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT ct_stock_result ASSIGNING FIELD-SYMBOL(<ls_stock>).
      <ls_stock>-company_code = VALUE #( lt_bukrs[ bwkey = <ls_stock>-plant ]-bukrs OPTIONAL ).
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_COMPANY_CODE.
    DATA: lt_co_code   TYPE STANDARD TABLE OF t001k-bukrs.

    SELECT DISTINCT t001k~bukrs
    INTO TABLE @lt_co_code
    FROM t001k
        LEFT JOIN marc ON (  t001k~bwkey = marc~werks )
          WHERE marc~werks IN @it_r_plant
          AND marc~matnr   IN @it_r_product.
    IF sy-subrc = 0.
      LOOP AT lt_co_code ASSIGNING FIELD-SYMBOL(<ls_co_code>).
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_co_code> ) INTO TABLE rt_r_comp_code.
      ENDLOOP.
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD GET_INVENTORY_RESULTS.

    rt_stock_result = me->mt_inventory_results.

  ENDMETHOD.


  METHOD get_inventory_stocks.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    DATA: lt_bapiret2  TYPE bapiret2_t,
          lt_json      TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lv_skip      TYPE abap_bool,
          ls_sel_opt   TYPE /vpcoe/s_selopt_inventory,
          lt_sobkz     TYPE STANDARD TABLE OF sobkz,
          lt_stock_tmp TYPE /vpcoe/cl_rdp_inventory=>gty_t_stock_data_rdp,
          lv_progress  TYPE int4.

    CLEAR: ev_failed.

    ev_no_data = abap_true.
    ls_sel_opt = is_sel_opt.

    IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send.
      io_cust->get_http_client( IMPORTING et_bapiret2 = lt_bapiret2 ).
      io_log->add_bapiret( EXPORTING it_bapiret2_t = lt_bapiret2 ).

      IF io_log->check( ).
        ev_failed = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-inventory
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-inventory
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-hdr
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        cv_skip     = lv_skip.

    me->clear_temporary_data( ).

    IF lv_skip = abap_false.

      "Storage Location
      IF is_sel_opt-lgbst = abap_true.
        ls_sel_opt = is_sel_opt.
        CLEAR: ls_sel_opt-sbbst,
               ls_sel_opt-sobkz_range,
               ls_sel_opt-trans.

        lt_stock_tmp = me->retrieve_stock_data(
                          EXPORTING
                            is_sel_opt = ls_sel_opt
                            io_log     = io_log
                          IMPORTING
                            ev_failed  = ev_failed ).
        IF ev_failed = abap_true.
          MESSAGE e075(/vpcoe/common) WITH 'Storage loc./batch stock' INTO io_log->sv_msg_text.
          io_log->add_sy_msg( ).
        ENDIF.

        io_log->add_msg_progress( EXPORTING iv_level = CONV #( 'Storage Location' )
                                            iv_add_message_to_log = abap_true
                                            iv_save_log = abap_false ).
        "Save temp data
        me->store_temporary_data( lt_stock_tmp ).
      ENDIF.

      "Special Stock
      IF is_sel_opt-sbbst = abap_true.
        ls_sel_opt = is_sel_opt.
        CLEAR: ls_sel_opt-lgbst,
               ls_sel_opt-trans.

        lt_sobkz = VALUE #( ( 'E' ) ( 'K' ) ( 'M' ) ( 'O' ) ( 'Q' ) ( 'T' ) ( 'V' ) ( 'W' ) ).

        LOOP AT lt_sobkz ASSIGNING FIELD-SYMBOL(<ls_sobkz>) WHERE table_line IN ls_sel_opt-sobkz_range.
          lv_progress = ( sy-tabix * 100 ) DIV 8.
          ls_sel_opt-sobkz = <ls_sobkz>.
          lt_stock_tmp = me->retrieve_stock_data(
                            EXPORTING
                              is_sel_opt = ls_sel_opt
                              io_log     = io_log
                            IMPORTING
                              ev_failed  = ev_failed ).

          io_log->add_msg_progress( EXPORTING iv_level    = CONV #( 'Special Stock' )
                                              iv_progress = lv_progress
                                              iv_add_message_to_log = abap_true
                                              iv_save_log = abap_false ).
          IF ev_failed = abap_true.
            MESSAGE w075(/vpcoe/common) WITH 'Special stock ( Indicator = ' && <ls_sobkz> && ')' INTO io_log->sv_msg_text.
            io_log->add_sy_msg( ).
            CONTINUE.
          ENDIF.
          "Save temp data
          me->store_temporary_data( lt_stock_tmp ).
        ENDLOOP.
      ENDIF.

      "In Transit
      IF is_sel_opt-trans = abap_true.
        ls_sel_opt = is_sel_opt.
        CLEAR: ls_sel_opt-lgbst,
               ls_sel_opt-sbbst,
               ls_sel_opt-sobkz_range.

        lt_stock_tmp = me->retrieve_stock_data(
                          EXPORTING
                            is_sel_opt = ls_sel_opt
                            io_log     = io_log
                          IMPORTING
                            ev_failed  = ev_failed ).

        io_log->add_msg_progress( EXPORTING iv_level = CONV #( 'In Transit' )
                                            iv_add_message_to_log = abap_true
                                            iv_save_log = abap_false ).
        IF ev_failed = abap_true.
          MESSAGE e075(/vpcoe/common) WITH 'In Transit' INTO io_log->sv_msg_text.
          io_log->add_sy_msg( ).
        ENDIF.
        "Save temp data
        me->store_temporary_data( lt_stock_tmp ).
      ENDIF.

      "collect data
      me->get_temporary_data(
        IMPORTING
          et_inventory_stock = me->mt_inventory_results ).

      IF me->mt_inventory_results IS INITIAL.
        RETURN.
      ENDIF.

      "get material group and company code
      me->get_additional_data_for_stocks(
        CHANGING
          ct_stock_result = me->mt_inventory_results  ).

      DELETE me->mt_inventory_results WHERE stock_quantity_in_base_unit < 0.
      IF sy-subrc = 0.
        MESSAGE i081(/vpcoe/common) INTO io_log->sv_msg_text.
        io_log->add_sy_msg( ).
      ENDIF.
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-inventory
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-inventory
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        ct_data     = me->mt_inventory_results.

    IF me->mt_inventory_results IS NOT INITIAL.
      ev_no_data = abap_false.
    ENDIF.

    CASE me->mv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
        IF sy-batch = abap_true AND iv_save_background = abap_false.
          MESSAGE e063(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          io_log->add_sy_msg( ).
          RETURN.
        ELSE.
          me->download_excel(
                EXPORTING
                  iv_file_path = iv_path
                  it_inventory = me->mt_inventory_results
                  iv_save_background = iv_save_background
                  io_log = io_log ).
        ENDIF.

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        me->prepare_json(
          EXPORTING
            it_stock_result = me->mt_inventory_results
          IMPORTING
            et_json         = lt_json ).

        CALL BADI lo_badi->adjust_json
          EXPORTING
            iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
            iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-inventory
            iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-inventory
            io_log      = io_log
          CHANGING
            ct_json     = lt_json.

        NEW /vpcoe/cl_rdp_payload_handler( )->send_payload(
          EXPORTING
            io_cust   = io_cust
            io_log    = io_log
            it_json   = lt_json
          IMPORTING
            ev_status = DATA(lv_status) ).

        IF lv_status <> /vpcoe/cl_rdp_http=>gc_s_http_status-ok.
          ev_failed = abap_true.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD get_temporary_data.

    CLEAR: et_inventory_stock.

    SELECT plant,
           product,
           productgroup,
           companycode,
           SUM( stockquantityinbaseunit ) AS stock_quantity_in_base_unit,
           baseunitofmeasure AS base_unit_of_measure,
           calendarmonth AS period-calendar_month,
           calendaryear AS period-calendar_year
      FROM /vpcoe/inventory
        INTO CORRESPONDING FIELDS OF TABLE @et_inventory_stock
          WHERE uname = @sy-uname
        GROUP BY plant, product, productgroup, companycode, baseunitofmeasure, calendarmonth, calendaryear.

    IF sy-subrc = 0.
      me->clear_temporary_data( ).
    ENDIF.

  ENDMETHOD.


  METHOD prepare_json.
    DATA: lt_inventory TYPE /vpcoe/cl_rdp_inventory=>gty_t_stock_data_rdp.

    CLEAR et_json.

    IF me->mv_package_size IS INITIAL.
      lt_inventory = it_stock_result.
    ELSE.
      LOOP AT it_stock_result ASSIGNING FIELD-SYMBOL(<ls_stock>).
        INSERT <ls_stock> INTO TABLE lt_inventory.

        IF lines( lt_inventory ) = me->mv_package_size.
          APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          <ls_json>-count = lines( lt_inventory ).
          <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data          = VALUE gty_s_stock_data_out_rdp(
                                                                                                     source   = mv_source
                                                                                                     elements = lt_inventory  )
                                                                               iv_pretty_name   = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

          REPLACE ALL OCCURRENCES OF ':"0000-00-00"' IN <ls_json>-elements WITH ': null' ##no_text.
          CLEAR: lt_inventory.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lt_inventory IS NOT INITIAL.
      APPEND INITIAL LINE TO et_json ASSIGNING <ls_json>.
      <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data          = VALUE gty_s_stock_data_out_rdp(
                                                                                                  source   = mv_source
                                                                                                  elements = lt_inventory  )
                                                                           iv_pretty_name   = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
      <ls_json>-count = lines( lt_inventory ).

      REPLACE ALL OCCURRENCES OF ':"0000-00-00"' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDIF.

  ENDMETHOD.


  METHOD retrieve_stock.

    DATA: lt_rspar_tab         TYPE TABLE OF rsparams,
          ls_rspar_line        TYPE rsparams,
          lo_data              TYPE REF TO data,
          lv_date              TYPE budat,
          lv_last_day_in_month TYPE budat.

    FIELD-SYMBOLS <lt_result> TYPE ANY TABLE.

    CLEAR ev_failed.

    IF is_sel_opt-prod IS NOT INITIAL.
      LOOP AT is_sel_opt-prod INTO DATA(ls_matnr).
        ls_rspar_line-selname = 'MATNR'.
        MOVE-CORRESPONDING ls_matnr TO ls_rspar_line.
        APPEND ls_rspar_line TO lt_rspar_tab.
      ENDLOOP.
    ELSE.
      ls_rspar_line-selname = 'MATNR'.
      ls_rspar_line-sign    = 'I'.
      ls_rspar_line-option  = 'CP'.
      ls_rspar_line-low = '*'.
      APPEND ls_rspar_line TO lt_rspar_tab.
    ENDIF.

    CLEAR ls_rspar_line.
    IF is_sel_opt-plant IS NOT INITIAL.
      LOOP AT is_sel_opt-plant INTO DATA(ls_werks).
        ls_rspar_line-selname = 'WERKS'.
        MOVE-CORRESPONDING ls_werks TO ls_rspar_line.
        APPEND ls_rspar_line TO lt_rspar_tab.
      ENDLOOP.
    ELSE.
      ls_rspar_line-selname = 'WERKS'.
      ls_rspar_line-sign    = 'I'.
      ls_rspar_line-option  = 'CP'.
      ls_rspar_line-low     = '*'.
      APPEND ls_rspar_line TO lt_rspar_tab.
    ENDIF.

    CLEAR ls_rspar_line.
    IF is_sel_opt-co_code IS NOT INITIAL.
      LOOP AT is_sel_opt-co_code INTO DATA(ls_company_code).
        ls_rspar_line-selname = 'BUKRS'.
        MOVE-CORRESPONDING ls_company_code TO ls_rspar_line.
        APPEND ls_rspar_line TO lt_rspar_tab.
      ENDLOOP.
    ELSE.
      ls_rspar_line-selname = 'BUKRS'.
      ls_rspar_line-sign    = 'I'.
      ls_rspar_line-option  = 'CP'.
      ls_rspar_line-low     = '*'.
      APPEND ls_rspar_line TO lt_rspar_tab.
    ENDIF.

    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'PA_SUMFL'.
    ls_rspar_line-sign    = 'I'.
    ls_rspar_line-option  = 'EQ'.
    ls_rspar_line-low     = 'X'.
    APPEND ls_rspar_line TO lt_rspar_tab.

    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'SOBKZ'.
    ls_rspar_line-sign    = 'I'.
    ls_rspar_line-option  = 'EQ'.
    ls_rspar_line-low     = is_sel_opt-sobkz.
    APPEND ls_rspar_line TO lt_rspar_tab.

    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'XSUM'.
    ls_rspar_line-sign    = 'I'.
    ls_rspar_line-option  = 'EQ'.
    ls_rspar_line-low = ''.
    APPEND ls_rspar_line TO lt_rspar_tab.

    ls_rspar_line-selname = 'XCHAR'.
    APPEND ls_rspar_line TO lt_rspar_tab.

    ls_rspar_line-selname = 'PA_SFLVA'.
    APPEND ls_rspar_line TO lt_rspar_tab.

    ls_rspar_line-selname = 'P_VARI'.
    APPEND ls_rspar_line TO lt_rspar_tab.

    ls_rspar_line-selname = 'XNOMCHB'.
    APPEND ls_rspar_line TO lt_rspar_tab.

    ls_rspar_line-selname = 'NOSTO'.
    APPEND ls_rspar_line TO lt_rspar_tab.

    IF is_sel_opt-from_arc = abap_true.
      ls_rspar_line-selname = 'ARCHIVE'.
      ls_rspar_line-low     = abap_true.
      APPEND ls_rspar_line TO lt_rspar_tab.

      ls_rspar_line-selname = 'PA_AISTR'.
      ls_rspar_line-low = is_sel_opt-arc_idx.
      APPEND ls_rspar_line TO lt_rspar_tab.
    ENDIF.

    READ TABLE is_sel_opt-month INTO DATA(ls_month) INDEX 1.
    IF sy-subrc = 0.
      DATA(lv_month) = ls_month-low.
    ENDIF.
    READ TABLE is_sel_opt-year INTO DATA(ls_year) INDEX 1.
    IF sy-subrc = 0.
      DATA(lv_year) = ls_year-low.
    ENDIF.

    CONCATENATE lv_year lv_month '01' INTO lv_date.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_date
      IMPORTING
        last_day_of_month = lv_last_day_in_month
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc = 0.
      CLEAR ls_rspar_line.
      ls_rspar_line-selname = 'DATUM'.
      ls_rspar_line-sign    = 'I'.
      ls_rspar_line-option  = 'EQ'.
      ls_rspar_line-low = lv_last_day_in_month.
      ls_rspar_line-high = lv_last_day_in_month.
      APPEND ls_rspar_line TO lt_rspar_tab.
    ENDIF.

    IF is_sel_opt-lgbst IS INITIAL AND is_sel_opt-sbbst IS INITIAL.
      DATA(lv_lgbst) = 'X'.
    ELSE.
      lv_lgbst = is_sel_opt-lgbst.
    ENDIF.
    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'LGBST'.
    ls_rspar_line-kind    = 'P'.
    ls_rspar_line-sign    = 'I'.
    ls_rspar_line-option  = 'EQ'.
    ls_rspar_line-low     = lv_lgbst.
    APPEND ls_rspar_line TO lt_rspar_tab.

    ls_rspar_line-selname = 'BWBST'.
    ls_rspar_line-low     = ''.
    APPEND ls_rspar_line TO lt_rspar_tab.

    ls_rspar_line-selname = 'SBBST'.
    ls_rspar_line-low     = is_sel_opt-sbbst.
    APPEND ls_rspar_line TO lt_rspar_tab.

    CALL FUNCTION '/VPCOE/CALL_RM07MLBD' DESTINATION 'NONE'
      IMPORTING
        ev_failed             = ev_failed
      TABLES
        it_rspar              = lt_rspar_tab
        et_stock              = rt_stock_result
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2.

    IF sy-subrc <> 0.
      io_log->add_sy_msg( ).
      MESSAGE e032(/vpcoe/common) INTO io_log->sv_msg_text.
      io_log->add_sy_msg( ).
      ev_failed = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD RETRIEVE_STOCK_DATA.

    IF is_sel_opt-trans = abap_true.
      rt_stock_result = me->retrieve_stock_intransit(
                                EXPORTING
                                  is_sel_opt = is_sel_opt
                                  io_log     = io_log
                                IMPORTING
                                  ev_failed  = ev_failed ).

    ELSE.
      rt_stock_result = me->retrieve_stock(
                                 EXPORTING
                                   is_sel_opt = is_sel_opt
                                   io_log     = io_log
                                 IMPORTING
                                   ev_failed  = ev_failed ).
    ENDIF.

  ENDMETHOD.


  METHOD retrieve_stock_intransit.
    DATA: lt_rspar_tab         TYPE TABLE OF rsparams,
          ls_rspar_line        TYPE rsparams,
          lv_date              TYPE budat,
          lv_last_day_in_month TYPE budat.

    CLEAR ev_failed.

    IF is_sel_opt-prod IS NOT INITIAL.
      LOOP AT is_sel_opt-prod INTO DATA(ls_matnr).
        ls_rspar_line-selname = 'MATNR'.
        MOVE-CORRESPONDING ls_matnr TO ls_rspar_line.
        APPEND ls_rspar_line TO lt_rspar_tab.
      ENDLOOP.
    ELSE.
      ls_rspar_line-selname = 'MATNR'.
      ls_rspar_line-sign    = 'I'.
      ls_rspar_line-option  = 'CP'.
      ls_rspar_line-low     = '*'.
      APPEND ls_rspar_line TO lt_rspar_tab.
    ENDIF.

    CLEAR ls_rspar_line.
    IF is_sel_opt-plant IS NOT INITIAL.
      LOOP AT is_sel_opt-plant INTO DATA(ls_werks).
        ls_rspar_line-selname = 'WERKS'.
        MOVE-CORRESPONDING ls_werks TO ls_rspar_line.
        APPEND ls_rspar_line TO lt_rspar_tab.
      ENDLOOP.
    ELSE.
      ls_rspar_line-selname = 'WERKS'.
      ls_rspar_line-sign    = 'I'.
      ls_rspar_line-option  = 'CP'.
      ls_rspar_line-low     = '*'.
      APPEND ls_rspar_line TO lt_rspar_tab.
    ENDIF.

    CLEAR ls_rspar_line.
    IF is_sel_opt-co_code IS NOT INITIAL.
      LOOP AT is_sel_opt-co_code INTO DATA(ls_company_code).
        ls_rspar_line-selname = 'BUKRS'.
        MOVE-CORRESPONDING ls_company_code TO ls_rspar_line.
        APPEND ls_rspar_line TO lt_rspar_tab.
      ENDLOOP.
    ELSE.
      ls_rspar_line-selname = 'BUKRS'.
      ls_rspar_line-sign    = 'I'.
      ls_rspar_line-option  = 'CP'.
      ls_rspar_line-low     = '*'.
      APPEND ls_rspar_line TO lt_rspar_tab.
    ENDIF.

    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'SOBKZ'.
    ls_rspar_line-sign    = 'I'.
    ls_rspar_line-option  = 'EQ'.
    ls_rspar_line-low     = is_sel_opt-sobkz.
    APPEND ls_rspar_line TO lt_rspar_tab.

    READ TABLE is_sel_opt-month INTO DATA(ls_month) INDEX 1.
    IF sy-subrc = 0.
      DATA(lv_month) = ls_month-low.
    ENDIF.
    READ TABLE is_sel_opt-year INTO DATA(ls_year) INDEX 1.
    IF sy-subrc = 0.
      DATA(lv_year) = ls_year-low.
    ENDIF.
*
    CONCATENATE lv_year lv_month '01' INTO lv_date.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_date
      IMPORTING
        last_day_of_month = lv_last_day_in_month
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc = 0.
      CLEAR ls_rspar_line.
      ls_rspar_line-selname = 'PA_BUDAT'.
      ls_rspar_line-sign    = 'I'.
      ls_rspar_line-option  = 'EQ'.
      ls_rspar_line-low = lv_last_day_in_month.
      APPEND ls_rspar_line TO lt_rspar_tab.
    ENDIF.

    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'XTRAM'.
    ls_rspar_line-low     = is_sel_opt-xtram.
    APPEND ls_rspar_line TO lt_rspar_tab.

    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'XNLCC'.
    ls_rspar_line-low     = is_sel_opt-xnlcc.
    APPEND ls_rspar_line TO lt_rspar_tab.

    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'XELIK'.
    ls_rspar_line-low     = is_sel_opt-xelik.
    APPEND ls_rspar_line TO lt_rspar_tab.

    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'XLOEK'.
    ls_rspar_line-low     = abap_false.
    APPEND ls_rspar_line TO lt_rspar_tab.
    ls_rspar_line-selname = 'XSPER'.
    APPEND ls_rspar_line TO lt_rspar_tab.

    CLEAR ls_rspar_line.
    ls_rspar_line-selname = 'PA_SUM'.
    ls_rspar_line-sign    = 'I'.
    ls_rspar_line-option  = 'EQ'.
    ls_rspar_line-low = ''.
    APPEND ls_rspar_line TO lt_rspar_tab.

    CALL FUNCTION '/VPCOE/CALL_RM07MTRB_DATE' DESTINATION 'NONE'
      IMPORTING
        ev_failed             = ev_failed
      TABLES
        it_rspar              = lt_rspar_tab
        et_stock              = rt_stock_result
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2.

    IF sy-subrc <> 0.
      io_log->add_sy_msg( ).
      MESSAGE e032(/vpcoe/common) INTO io_log->sv_msg_text.
      io_log->add_sy_msg( ).
      ev_failed = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD store_temporary_data.
    DATA: lt_inventory_db TYPE STANDARD TABLE OF /vpcoe/inventory.

    LOOP AT it_inventory_stock ASSIGNING FIELD-SYMBOL(<ls_stock>).
      me->mv_item_id = me->mv_item_id + 1.
      INSERT VALUE #( uname                   = sy-uname
                      item_id                 = me->mv_item_id
                      plant                   = <ls_stock>-plant
                      product                 = <ls_stock>-product
                      companycode             = <ls_stock>-company_code
                      stockquantityinbaseunit = <ls_stock>-stock_quantity_in_base_unit
                      baseunitofmeasure       = <ls_stock>-base_unit_of_measure
                      calendarmonth           = <ls_stock>-period-calendar_month
                      calendaryear            = <ls_stock>-period-calendar_year ) INTO TABLE lt_inventory_db.
    ENDLOOP.

    INSERT /vpcoe/inventory FROM TABLE lt_inventory_db.

  ENDMETHOD.
ENDCLASS.
