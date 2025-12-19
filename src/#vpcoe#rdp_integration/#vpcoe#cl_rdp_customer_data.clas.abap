class /VPCOE/CL_RDP_CUSTOMER_DATA definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_sel_opt,
        cust_num  TYPE RANGE OF kunnr,
        country   TYPE RANGE OF land1_gp,
        region    TYPE RANGE OF regio,
        crt_date  TYPE RANGE OF cddatum,
        chng_date TYPE RANGE OF cddatum,
        role      TYPE RANGE OF /vpcoe/de_customer_role,
        bukrs     TYPE /vpcoe/tt_r_bukrs,
      END OF gty_s_sel_opt .
  types:
    BEGIN OF gty_s_tax_number,
        cust_num      TYPE kunnr,
        tax_number_xl TYPE bptaxnumxl,
        country       TYPE land1.
    INCLUDE TYPE /vpcoe/str_tax_number AS tax.
    TYPES:
       END OF gty_s_tax_number .
  types:
    gty_t_tax_number TYPE SORTED TABLE OF gty_s_tax_number WITH NON-UNIQUE KEY cust_num .
  types:
    BEGIN OF gty_s_sel_opt_ext,
        delta_only TYPE abap_bool.
    INCLUDE TYPE gty_s_sel_opt AS option.
    TYPES:
    END OF gty_s_sel_opt_ext .
  types:
    gty_t_cpi TYPE STANDARD TABLE OF bdicpident WITH NON-UNIQUE KEY cpident .
  types:
    gty_r_kunnr    TYPE RANGE OF kunnr .
  types:
    gty_r_land1    TYPE RANGE OF land1_gp .
  types:
    gty_r_regio    TYPE RANGE OF regio .
  types:
    gty_r_date     TYPE RANGE OF cddatum .
  types GTY_S_CUSTOMER type /VPCOE/STR_CUSTOMER .
  types:
    gty_t_customer TYPE SORTED TABLE OF gty_s_customer WITH NON-UNIQUE KEY id .
  types:
    BEGIN OF gty_s_customer_xls,
             formula TYPE string.
            INCLUDE TYPE gty_s_customer.
    TYPES:
    END OF gty_s_customer_xls .
  types:
    gty_t_customer_xls TYPE SORTED TABLE OF gty_s_customer_xls WITH NON-UNIQUE KEY id .
  types GTY_S_CUSTOMER_JSON type /VPCOE/STR_CUSTOMER_JSON .
  types:
    BEGIN OF gty_s_customer_tables,
        customer           TYPE gty_t_customer_xls, "gty_t_customer,
        tax_number         TYPE gty_t_tax_number,
        customer_extension TYPE /vpcoe/t_customer_ext,
      END OF gty_s_customer_tables .

  methods BUILD_JSON
    importing
      !IT_CUSTOMER type GTY_T_CUSTOMER
      !IT_TAX_NUMBER type GTY_T_TAX_NUMBER optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    exporting
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods CHANGE_STATUS
    importing
      !IV_TEST_RUN type XFELD .
  methods CONSTRUCTOR
    importing
      !IV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE
      !IV_SOURCE type STRING
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND .
  methods DOWNLOAD_EXCEL
    importing
      !IV_SERVICE_ID type /VPCOE/DE_SERVICE_ID default /VPCOE/CL_COMMON_HELPER=>SC_SERVICE_ID-CUSTOMER
      !IV_FILE_PATH type STRING
      !IV_SAVE_BACKGROUND type ABAP_BOOL
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
  methods GET_ADDRESS
    changing
      !CT_CUSTOMER type GTY_T_CUSTOMER .
  methods GET_CUSTOMER_ALL
    importing
      !IV_DELTA type ABAP_BOOL optional
      !IS_SEL_OPT type /VPCOE/S_SELOPT_CUSTOMER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    exporting
      !ET_CUSTOMER_DEL type GTY_T_CUSTOMER
      !ET_CUSTOMER type GTY_T_CUSTOMER
      !ET_JSON_DEL type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_CUSTOMER_DELTA
    importing
      !IV_CHNG_POINTER_ID type EDI_MESTYP
      !IS_SEL_OPT type /VPCOE/S_SELOPT_CUSTOMER optional
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    exporting
      !ET_CUSTOMER_DEL type GTY_T_CUSTOMER
      !ET_CUSTOMER type GTY_T_CUSTOMER
      !ET_JSON_DEL type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !ET_BAPIRET2 type BAPIRET2_T .
  methods GET_CUSTOMER_EXT
    importing
      !IS_SEL_OPT type GTY_S_SEL_OPT_EXT
      !IT_CUSTOMER type GTY_T_CUSTOMER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    exporting
      !ET_CUSTOMER_EXT type /VPCOE/T_CUSTOMER_EXT
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods CLOSE_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
protected section.

  methods GET_EXCEL_HEADER
    importing
      !IV_SHEET_NAME type CHAR40
    exporting
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_DATA type DATA .
  methods GET_EXCEL_SHEET
    importing
      !IV_SERVICE_ID type /VPCOE/DE_SERVICE_ID
    exporting
      !ET_SHEETS type /VPCOE/TT_SHEETS_STR .
  methods GET_TAX_NUMBER
    importing
      !IT_CUSTOMER type GTY_T_CUSTOMER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    exporting
      !ET_TAX_NUMBER type GTY_T_TAX_NUMBER .
private section.

  types:
    BEGIN OF gty_s_code_list_category,
           category     TYPE c LENGTH 1,
           explanation TYPE c LENGTH 30,
         END OF gty_s_code_list_category .
  types:
    gty_t_code_list_category TYPE TABLE OF gty_s_code_list_category .

  data MT_CPI type /VPCOE/CL_RDP_CUSTOMER_DATA=>GTY_T_CPI .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_CHNG_POINTER_ID type EDI_MESTYP .
  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SOURCE type STRING .
  data MV_MODE type /VPCOE/DE_MODE .
  data MS_CUSTOMER_TABLES type GTY_S_CUSTOMER_TABLES .
  data MV_CVI_INDICATOR_ACTIVE type FLAG .
ENDCLASS.



CLASS /VPCOE/CL_RDP_CUSTOMER_DATA IMPLEMENTATION.


  METHOD build_json.
    DATA: lt_customer_pack TYPE gty_t_customer,
          lt_customer	     TYPE gty_t_customer.

    CLEAR et_json.

    IF me->mv_package_size IS INITIAL.
      lt_customer_pack = it_customer.
    ELSE.
      lt_customer = it_customer.
      LOOP AT lt_customer ASSIGNING FIELD-SYMBOL(<ls_cusotmer>).
        <ls_cusotmer>-tax_numbers = VALUE #( FOR <ls_tax_number> IN it_tax_number WHERE ( cust_num = <ls_cusotmer>-id )
                                            ( tax_number      =  COND #( WHEN <ls_tax_number>-tax_number IS NOT INITIAL
                                                                              THEN <ls_tax_number>-tax_number
                                                                               ELSE <ls_tax_number>-tax_number_xl )
                                              tax_number_type = <ls_tax_number>-tax_number_type ) ).

        INSERT <ls_cusotmer> INTO TABLE lt_customer_pack.

        IF lines( lt_customer_pack ) = me->mv_package_size.
          APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          <ls_json>-count = lines( lt_customer_pack ).
          <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                                        EXPORTING is_data        = VALUE gty_s_customer_json( source   = me->mv_source
                                                                                                              elements = lt_customer_pack
                                                                                                             )
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

          REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
          CLEAR lt_customer_pack.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF lt_customer_pack IS NOT INITIAL.
      APPEND INITIAL LINE TO et_json ASSIGNING <ls_json>.
      <ls_json>-count = lines( lt_customer_pack ).
      <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                                    EXPORTING is_data        = VALUE gty_s_customer_json( source   = me->mv_source
                                                                                                          elements = lt_customer_pack )
                                                              iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
      REPLACE ALL OCCURRENCES OF ':"f"' IN <ls_json>-elements WITH ': false' ##no_text.
      REPLACE ALL OCCURRENCES OF ':"t"' IN <ls_json>-elements WITH ': true' ##no_text.
    ENDIF.


  ENDMETHOD.


  METHOD change_status.
    IF iv_test_run = abap_false AND mv_chng_pointer_id IS NOT INITIAL.
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

    SELECT SINGLE link_source~active_indicator, link_target~active_indicator
         FROM mdsc_ctrl_opt_a AS link_source
         JOIN mdsc_ctrl_opt_a AS link_target ON link_target~sync_obj_target = link_source~sync_obj_source
         WHERE link_source~sync_obj_target = 'CUSTOMER' ##no_text
           AND link_target~sync_obj_source = 'CUSTOMER' ##no_text
         INTO ( @DATA(lv_active_indicator_source), @DATA(lv_active_indicator_target) ).

    IF lv_active_indicator_source = abap_true AND lv_active_indicator_target = abap_true.
      me->mv_cvi_indicator_active = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD download_excel.

    DATA: r_table   TYPE REF TO data,
          lt_header TYPE /vpcoe/cl_xls_handler=>gty_t_title.
    FIELD-SYMBOLS: <lt_data> TYPE any.

    get_excel_sheet(
      EXPORTING
        iv_service_id = iv_service_id
      IMPORTING
        et_sheets     = DATA(lt_sheets) ).

    DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path = iv_file_path
                                                   iv_save_background = iv_save_background  ).

    LOOP AT lt_sheets ASSIGNING FIELD-SYMBOL(<ls_sheets>).

      IF <ls_sheets> = 'Code list CustomerRole' OR <ls_sheets> = 'Code list TaxNumberCategory' OR <ls_sheets> = 'Additional code lists'.

        get_excel_header(
           EXPORTING
             iv_sheet_name = <ls_sheets>
           IMPORTING
             et_header     = lt_header
             et_data       = r_table ).

        ASSIGN r_table->* TO <lt_data>.

        lo_xls_file->execute( iv_name = CONV #( <ls_sheets> ) ).

        lo_xls_file->fill_data( it_item_tab   = <lt_data>
                                it_title      = lt_header
                                iv_tax_number = COND #( WHEN <ls_sheets> = 'Code list TaxNumberCategory'
                                                        THEN 'Tax Number Category'
                                                        WHEN <ls_sheets> = 'Additional code lists'
                                                        THEN 'Code List Business Partner Category'
                                                        ELSE 'Code list CustomerRole' )
                                iv_style      = /vpcoe/cl_xls_handler=>sc_styles-tax_hdr_style ).

      ELSE.

        get_excel_header(
           EXPORTING
             iv_sheet_name = <ls_sheets>
           IMPORTING
             et_header     = lt_header
             et_data       = r_table ).

        ASSIGN r_table->* TO <lt_data>.

        lo_xls_file->execute( iv_name = CONV #( <ls_sheets> ) ).

        CASE iv_service_id.
          WHEN /vpcoe/cl_rdp_helper=>sc_service_id-customer.
            lo_xls_file->create_range_table( iv_formula = `='Code list TaxNumberCategory'!R3C1:R662C2`
                                             iv_name = 'TaxNumberCategory' ).

            lo_xls_file->create_range_table( iv_formula = `='Additional code lists'!R3C1:R7C2`
                                             iv_name = 'Customer' ).

          WHEN /vpcoe/cl_rdp_helper=>sc_service_id-customer_ext.
            lo_xls_file->create_range_table( iv_formula = `='Code list CustomerRole'!R3C1:R7C3`
                                             iv_name = 'CustomerRole' ).
        ENDCASE.

        lo_xls_file->fill_data( it_item_tab = <lt_data>
                                it_title    = lt_header ).
      ENDIF.

    ENDLOOP.

    lo_xls_file->save_xls_file( io_log = io_log ).

  ENDMETHOD.


  METHOD get_address.

    IF ct_customer IS INITIAL.
      RETURN.
    ENDIF.

    IF me->mv_cvi_indicator_active = abap_true.

      GET TIME STAMP FIELD DATA(lv_timestamp).
      SELECT DISTINCT cvi_cust_link~customer           AS id,
                      but000~type                      AS category,
                      adrc~post_code1                  AS postal_code,
                      adrc~street                      AS street,
                      adrc~house_num1                  AS house_number,
                      adrc~city1                       AS city,
                      adrc~tel_number                  AS phone,
                      adr6~smtp_addr                   AS email,
                      adrc~fax_number                  AS fax,
                      contactpersbut000~name_first     AS contact_person_first_name,
                      contactpersbut000~name_last      AS contact_person_last_name
          FROM cvi_cust_link
          LEFT JOIN but000 ON cvi_cust_link~partner_guid = but000~partner_guid
          LEFT JOIN but020 ON but000~partner = but020~partner
          LEFT JOIN adrc   ON but020~addrnumber = adrc~addrnumber
          LEFT JOIN adr6   ON but020~addrnumber = adr6~addrnumber
          LEFT JOIN but021_fs ON but020~addrnumber = but021_fs~addrnumber
          LEFT JOIN but051 ON but000~partner = but051~partner1 AND but051~reltyp = 'BUR001' ##no_text
          LEFT JOIN but000 AS contactpersbut000 ON contactpersbut000~partner =  but051~partner2
          "LEFT JOIN adrc   AS contactpersadrc ON contactpersbut020~addrnumber = contactpersadrc~addrnumber
        FOR ALL ENTRIES IN @ct_customer
          WHERE cvi_cust_link~customer = @ct_customer-id
            AND but021_fs~adr_kind = 'XXDEFAULT' ##no_text "standart address
            AND but021_fs~valid_from < @lv_timestamp
            AND but021_fs~valid_to   > @lv_timestamp
        INTO TABLE @DATA(lt_address).

      DELETE ADJACENT DUPLICATES FROM lt_address COMPARING id.

    ELSE.
      SELECT DISTINCT kna1~kunnr     AS id,
                      adrc~title     AS category,
                      kna1~pstlz     AS postal_code,
                      kna1~stras     AS street,
                      kna1~ort01     AS city,
                      kna1~telf1     AS phone,
                      adr6~smtp_addr AS email,
                      kna1~telfx     AS fax,
                      knvk~namev     AS contact_person_first_name,
                      knvk~name1     AS contact_person_last_name
         FROM kna1
         LEFT JOIN adrc ON kna1~adrnr = adrc~addrnumber
         LEFT JOIN adr6 ON kna1~adrnr = adr6~addrnumber
         LEFT JOIN knvk ON knvk~kunnr = kna1~kunnr
         INTO CORRESPONDING FIELDS OF TABLE @lt_address
         FOR ALL ENTRIES IN @ct_customer
          WHERE kna1~kunnr = @ct_customer-id.

      DELETE ADJACENT DUPLICATES FROM lt_address COMPARING id.
    ENDIF.

    LOOP AT lt_address ASSIGNING FIELD-SYMBOL(<ls_address>).
      ASSIGN ct_customer[ id = <ls_address>-id ] TO FIELD-SYMBOL(<ls_customer>).
      IF sy-subrc = 0.
        <ls_customer> = CORRESPONDING #( BASE ( <ls_customer> )  <ls_address> EXCEPT id ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_customer_all.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.
    DATA: lv_skip    TYPE abap_bool.

    CLEAR: et_customer,
           et_json,
           et_customer_del,
           et_json_del.

* Get Only existing Customers
    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-customer
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT DISTINCT kna1~kunnr AS id,
                      kna1~name1 && ' ' && kna1~name2  AS name,
                      kna1~regio AS region,
                      t005~intca AS country
        INTO CORRESPONDING FIELDS OF TABLE @et_customer
          FROM kna1 LEFT JOIN cdhdr ON kna1~kunnr = cdhdr~objectid
                    LEFT JOIN knb1 ON kna1~kunnr = knb1~kunnr
                    LEFT JOIN knvv ON kna1~kunnr = knvv~kunnr
                    LEFT JOIN t005 ON kna1~land1 = t005~land1
            WHERE kna1~kunnr IN @is_sel_opt-customer_id
              AND kna1~land1 IN @is_sel_opt-country
              AND kna1~regio IN @is_sel_opt-region
              AND kna1~erdat IN @is_sel_opt-crt_date
              AND kna1~ktokd IN @is_sel_opt-ktokd
              AND kna1~loevm = @abap_false
              AND kna1~nodel = @abap_false
              AND knb1~bukrs IN @is_sel_opt-bukrs
              AND cdhdr~udate IN @is_sel_opt-chng_date
              AND knvv~vkorg IN @is_sel_opt-vkorg.

      IF sy-subrc <> 0.
        CLEAR et_customer.
      ENDIF.

      io_log->add_msg_progress( iv_level = CONV #( 'Customer' )
                                iv_save_log = abap_false
                                iv_add_message_to_log = abap_true ).

      me->get_address( CHANGING ct_customer = et_customer ).

    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-customer
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        ct_data     = et_customer.

    me->get_tax_number(
      EXPORTING
        it_customer   = et_customer
        io_log        = io_log
      IMPORTING
        et_tax_number = DATA(lt_tax_number) ).
    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      DATA: lv_formula TYPE string VALUE '=IFERROR(VLOOKUP(RC[-1],Customer,2,FALSE),"unknown")'.

      ms_customer_tables-customer = VALUE #( FOR <ls_customer> IN et_customer
                                           ( VALUE #( BASE CORRESPONDING #( <ls_customer> ) formula = lv_formula ) ) ).
      ms_customer_tables-tax_number = lt_tax_number.
      RETURN.
    ENDIF.

    IF et_customer IS NOT INITIAL AND et_json IS SUPPLIED.
      me->build_json( EXPORTING it_customer   = et_customer
                                it_tax_number = lt_tax_number
                                io_log = io_log
                      IMPORTING et_json       = et_json ).

      CALL BADI lo_badi->adjust_json
        EXPORTING
          iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
          iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer
          iv_level    = /vpcoe/cl_rdp_helper=>sc_level-customer
          iv_api_type = me->mv_api_type
          io_log      = io_log
        CHANGING
          ct_json     = et_json.
    ENDIF.

* Get Only Deleted Customers in Delta
    CLEAR lv_skip.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-customer_del
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT DISTINCT kna1~kunnr     AS id,
                      kna1~name1     AS name,
                      kna1~regio     AS region,
                      kna1~land1     AS country,
                      CASE WHEN kna1~loevm = 'X' THEN 'X'
                                                 ELSE '-' END AS is_marked_for_deletion
          INTO CORRESPONDING FIELDS OF TABLE @et_customer_del
            FROM kna1
                LEFT JOIN cdhdr ON kna1~kunnr = cdhdr~objectid
                LEFT JOIN knb1 ON kna1~kunnr = knb1~kunnr
                WHERE kna1~kunnr IN @is_sel_opt-customer_id
                AND kna1~land1 IN @is_sel_opt-country
                AND kna1~regio IN @is_sel_opt-region
                AND kna1~erdat IN @is_sel_opt-crt_date
                AND cdhdr~udate IN @is_sel_opt-chng_date
                AND knb1~bukrs IN @is_sel_opt-bukrs
                AND ( kna1~loevm = @abap_true OR kna1~nodel = @abap_true ).
      IF sy-subrc <> 0.
        CLEAR et_customer_del.
      ENDIF.
      me->get_address( CHANGING ct_customer = et_customer_del ).
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-customer_del
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        ct_data     = et_customer_del.

    me->get_tax_number(
      EXPORTING
        it_customer   = et_customer_del
        io_log        = io_log
      IMPORTING
        et_tax_number = DATA(lt_tax_number_del) ).

    IF et_customer_del IS NOT INITIAL AND et_json_del IS SUPPLIED.
      me->build_json( EXPORTING it_customer   = et_customer_del
                                it_tax_number = lt_tax_number_del
                                io_log = io_log
                      IMPORTING et_json       = et_json_del ).

      CALL BADI lo_badi->adjust_json
        EXPORTING
          iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
          iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer
          iv_level    = /vpcoe/cl_rdp_helper=>sc_level-customer_del
          iv_api_type = me->mv_api_type
          io_log      = io_log
        CHANGING
          ct_json     = et_json_del.
    ENDIF.

  ENDMETHOD.


  METHOD get_customer_delta.

    DATA: lt_r_customer_id TYPE gty_r_kunnr,
          ls_sel_opt       TYPE /vpcoe/s_selopt_customer.

    CLEAR: et_customer,
           et_json,
           et_bapiret2,
           et_customer_del,
           et_json_del.

    mv_chng_pointer_id = iv_chng_pointer_id.

    /vpcoe/cl_rdp_helper=>read_change_pointers(
       EXPORTING
         iv_chng_pointer_id = mv_chng_pointer_id
         it_r_change_date   = is_sel_opt-chng_date
         it_r_create_date   = is_sel_opt-crt_date
       IMPORTING
         et_r_objid         = lt_r_customer_id
         et_cpi             = mt_cpi ).

    IF lt_r_customer_id IS NOT INITIAL.
      ls_sel_opt = is_sel_opt.
      "For Delta Mode Create/Change date shouldn't be taken into consideration for 'regular' select
      CLEAR: ls_sel_opt-crt_date,
             ls_sel_opt-chng_date,
             ls_sel_opt-customer_id.

      LOOP AT lt_r_customer_id ASSIGNING FIELD-SYMBOL(<ls_r_customer_id>).
        IF <ls_r_customer_id>-low IN is_sel_opt-customer_id.
          INSERT <ls_r_customer_id> INTO TABLE ls_sel_opt-customer_id.
        ENDIF.
      ENDLOOP.

*      ls_sel_opt-customer_id = lt_r_customer_id.

      me->get_customer_all(
        EXPORTING
          iv_delta        = abap_true
          is_sel_opt      = ls_sel_opt
          io_log          = io_log
       IMPORTING
          et_customer     = et_customer
          et_json         = et_json
          et_customer_del = et_customer_del
          et_json_del     = et_json_del  ).
    ENDIF.

  ENDMETHOD.


  METHOD get_customer_ext.

    DATA: lo_badi              TYPE REF TO /vpcoe/adjust_data_retrieval,
          lt_customer_ext      TYPE /vpcoe/t_customer_ext,
          lt_customer_pack_ext TYPE /vpcoe/t_customer_ext.

    CLEAR: et_customer_ext,
           et_json.

    GET BADI lo_badi.

    CALL BADI lo_badi->get_ext_data
      EXPORTING
        iv_api_type = me->mv_api_type
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer_ext
        is_sel_opt  = is_sel_opt
        it_data     = it_customer
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        ct_ext_data = lt_customer_ext.

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_customer_tables-customer_extension = lt_customer_ext.
      RETURN.
    ENDIF.

    IF lt_customer_ext IS NOT INITIAL AND et_json IS SUPPLIED.
      LOOP AT lt_customer_ext ASSIGNING FIELD-SYMBOL(<ls_customer_ext>).
        INSERT <ls_customer_ext> INTO TABLE lt_customer_pack_ext.

        IF lines( lt_customer_pack_ext ) = mv_package_size AND mv_package_size IS NOT INITIAL.
          APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = VALUE /vpcoe/s_customer_ext_json( source   = mv_source
                                                                                                                                  elements = lt_customer_pack_ext )
                                                                               iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
          <ls_json>-count = lines( lt_customer_pack_ext ).
          INSERT LINES OF lt_customer_pack_ext INTO TABLE et_customer_ext.
          CLEAR lt_customer_pack_ext.
        ENDIF.

      ENDLOOP.
      IF lt_customer_pack_ext IS NOT INITIAL.
        INSERT LINES OF lt_customer_pack_ext INTO TABLE et_customer_ext.
        APPEND INITIAL LINE TO et_json ASSIGNING <ls_json>.
        <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = VALUE /vpcoe/s_customer_ext_json( source   = mv_source
                                                                                                                                elements = lt_customer_pack_ext )
                                                                             iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
        <ls_json>-count = lines( lt_customer_pack_ext ).
      ENDIF.

      CALL BADI lo_badi->adjust_json
        EXPORTING
          iv_api_type = me->mv_api_type
          iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
          iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer_ext
          io_log      = io_log
        CHANGING
          ct_json     = et_json.

    ENDIF.

  ENDMETHOD.


  METHOD get_excel_header.

    FIELD-SYMBOLS <lt_data> TYPE any.

    TYPES:
      BEGIN OF ty_s_tax_with_furmula,
        formula TYPE string.
            INCLUDE TYPE gty_s_tax_number.
    TYPES:
       END OF ty_s_tax_with_furmula.

    TYPES:
      BEGIN OF ty_s_ext_with_furmula,
        formula TYPE string.
    INCLUDE TYPE /vpcoe/s_customer_ext AS ext.
    TYPES:
       END OF ty_s_ext_with_furmula.

    DATA: lt_tax_number         TYPE SORTED TABLE OF ty_s_tax_with_furmula WITH NON-UNIQUE KEY formula,
          lt_cust_ext           TYPE SORTED TABLE OF ty_s_ext_with_furmula WITH NON-UNIQUE KEY formula,
          lt_code_list_category TYPE gty_t_code_list_category.

    CLEAR: et_header,
           et_data.

    SELECT tfktaxnumtype_t~taxtype, text
        FROM tfktaxnumtype_t
        INNER JOIN /vpcoe/taxnumtyp ON tfktaxnumtype_t~taxtype = /vpcoe/taxnumtyp~taxtype
        WHERE spras = 'E'
        INTO TABLE @DATA(lt_tax).

    CASE iv_sheet_name.
      WHEN 'CustomerTaxNumber'.
        CREATE DATA et_data LIKE lt_tax_number.
        ASSIGN et_data->* TO <lt_data>.
        LOOP AT ms_customer_tables-tax_number ASSIGNING FIELD-SYMBOL(<ls_tax_num>).
          IF NOT line_exists( lt_tax[ taxtype = <ls_tax_num>-tax_number_type ] ).
            CONTINUE.
          ENDIF.
          lt_tax_number = VALUE #( BASE lt_tax_number
                                          ( cust_num        = <ls_tax_num>-cust_num
                                            tax_number_xl   = COND #( WHEN <ls_tax_num>-tax_number IS NOT INITIAL
                                                                      THEN <ls_tax_num>-tax_number
                                                                  ELSE <ls_tax_num>-tax_number_xl )
                                            tax_number_type = <ls_tax_num>-tax_number_type
                                            formula         = '=VLOOKUP(RC[-1],TaxNumberCategory,2,FALSE)'  ) ).
        ENDLOOP.

        <lt_data> = lt_tax_number.
        et_header = VALUE #(
                         ( description      = 'Customer Number'
                           internal_name    = 'customerId'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'KUNNR'
                           vpcoe_attribute  = 'CUST_NUM'
                           is_key           = abap_true )
                         ( description      = 'Tax Number Category'
                           internal_name    = 'taxNumberType'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'TAXTYPE'
                           vpcoe_attribute  = 'TAX_NUMBER_TYPE'
                           is_key           = abap_true )
                         ( description      = ''
                           internal_name    = ''
                           data_type        = ''
                           s4hana_attribute = ''
                           vpcoe_attribute  = 'FORMULA'
                           mandatory        = '' )
                         ( description      = 'Tax Number'
                           internal_name    = 'taxNumber'
                           data_type        = 'CHAR(20)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'TAXNUM'
                           vpcoe_attribute  = 'TAX_NUMBER_XL' ) ).

      WHEN 'Customer'.
        CREATE DATA et_data LIKE ms_customer_tables-customer.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_customer_tables-customer.
        et_header = VALUE #(
                         ( description      = 'Customer Number'
                           internal_name    = 'id'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'KUNNR'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Customer Name'
                           internal_name    = 'name'
                           data_type        = 'CHAR(80)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MD_CUSTOMER_NAME'
                           vpcoe_attribute  = 'NAME' )
                         ( description      = 'State/Province'
                           internal_name    = 'region'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'REGIO'
                           vpcoe_attribute  = 'REGION')
                         ( description      = 'Country/Region'
                           internal_name    = 'country'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LAND1_GP'
                           vpcoe_attribute  = 'COUNTRY' )
                         ( description      = 'Street'
                           internal_name    = 'street'
                           data_type        = 'CHAR(60)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-STREET'
                           vpcoe_attribute  = 'STREET')
                         ( description      = 'House Number'
                           internal_name    = 'houseNumber'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-HOUSE_NUM1'
                           vpcoe_attribute  = 'HOUSE_NUMBER')
                         ( description      = 'Postal code'
                           internal_name    = 'postalCode'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-POST_CODE1'
                           vpcoe_attribute  = 'POSTAL_CODE')
                         ( description      = 'Postal code'
                           internal_name    = 'postalCode'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-POST_CODE1'
                           vpcoe_attribute  = 'POSTAL_CODE')
                         ( description      = 'City'
                           internal_name    = 'city'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-CITY1'
                           vpcoe_attribute  = 'CITY')
                         ( description      = 'Firstname of Contact Person'
                           internal_name    = 'contactPersonFirstName'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-Name1'
                           vpcoe_attribute  = 'CONTACT_PERSON_FIRST_NAME')
                         ( description      = 'Lastname of Contact Person'
                           internal_name    = 'contactPersonLastName'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-Name2'
                           vpcoe_attribute  = 'CONTACT_PERSON_LAST_NAME')
                         ( description      = 'Phone number'
                           internal_name    = 'phone'
                           data_type        = 'CHAR(30)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-TEL_NUMBER'
                           vpcoe_attribute  = 'PHONE')
                         ( description      = 'Fax number'
                           internal_name    = 'fax'
                           data_type        = 'CHAR(30)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-FAX_NUMBER'
                           vpcoe_attribute  = 'FAX')
                         ( description      = 'Email'
                           internal_name    = 'email'
                           data_type        = 'CHAR(241)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADRC-SMTP_ADDR'
                           vpcoe_attribute  = 'EMAIL')
                         ( description      = 'Category'
                           internal_name    = 'businessPartnerCategory'
                           data_type        = 'CHAR(1)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'BUT000-TYPE'
                           vpcoe_attribute  = 'CATEGORY')
                         ( description      = ''
                           internal_name    = ''
                           data_type        = ''
                           s4hana_attribute = ''
                           vpcoe_attribute  = 'FORMULA'
                           mandatory        = '' ) ).

      WHEN 'CustomerExtension'.
        CREATE DATA et_data LIKE lt_cust_ext.
        ASSIGN et_data->* TO <lt_data>.
        lt_cust_ext = VALUE #( FOR <ls_cust_ext> IN ms_customer_tables-customer_extension
                               ( ext = <ls_cust_ext>
                                 formula = '=VLOOKUP(RC[-1],CustomerRole,2,FALSE)' ) ).

        <lt_data> = lt_cust_ext.
        et_header = VALUE #(
                         ( description      = 'Customer Number'
                           internal_name    = 'id'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'KUNNR'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Tag'
                           internal_name    = 'tag'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'TAG' )
                         ( description      = 'Role'
                           internal_name    = 'role'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'ROLE')
                         ( description      = ''
                           internal_name    = ''
                           data_type        = ''
                           mandatory        = ''
                           s4hana_attribute = ''
                           vpcoe_attribute  = 'FORMULA' ) ).

      WHEN 'Code list TaxNumberCategory'.
        CREATE DATA et_data LIKE lt_tax.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = lt_tax.
        et_header = VALUE #(
                         ( description      = 'Category'
                           vpcoe_attribute  = 'TAXTYPE' )
                         ( description      = 'Explanation'
                           vpcoe_attribute  = 'TEXT' ) ).
      WHEN 'Additional code lists'.
        lt_code_list_category = VALUE #( ( category = ''  explanation = 'Unknown' )
                                         ( category = '1' explanation = 'Person' )
                                         ( category = '2' explanation = 'Organization' )
                                         ( category = '3' explanation = 'Group' ) ).

        CREATE DATA et_data LIKE lt_code_list_category.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = lt_code_list_category.

        et_header = VALUE #( ( description      = 'Category'
                               vpcoe_attribute  = 'CATEGORY' )
                             ( description      = 'Explanation'
                               vpcoe_attribute  = 'EXPLANATION' ) ).
      WHEN 'Code list CustomerRole'.
        SELECT domvalue_l AS role,
                    ddtext AS lable,
                CASE
           WHEN domvalue_l = 'END_CONSUM'
                     THEN 'End consumer (private person)'
           WHEN domvalue_l = 'NEXT_PACKP'
                     THEN 'Currently only needed for UK Report, to identify the producer of the next step in the production chain of packaging material '
           WHEN domvalue_l = 'RETAILER'
                     THEN 'Retailer'
           WHEN domvalue_l = 'PROF_CUSTM'
                     THEN 'Professional Customer (commercial person)'
           WHEN domvalue_l = 'HOSPITAL'
                     THEN 'Hospital' END AS explanation
           FROM dd07v
           WHERE domname = '/VPCOE/CUSTOMER_ROLE'
         INTO TABLE @DATA(lt_role).
        CREATE DATA et_data LIKE lt_role.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = lt_role.
        et_header = VALUE #(
                         ( description      = 'Role'
                           vpcoe_attribute  = 'ROLE' )
                         ( description      = 'Lable'
                           vpcoe_attribute  = 'LABLE' )
                         ( description      = 'Explanation'
                           vpcoe_attribute  = 'EXPLANATION' ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_excel_sheet.

    CLEAR et_sheets.

    CASE iv_service_id.
      WHEN /vpcoe/cl_rdp_helper=>sc_service_id-customer.
        et_sheets = VALUE #( ( 'Code list TaxNumberCategory' ) ( 'CustomerTaxNumber' ) ( 'Customer' ) ( 'Additional code lists' ) ).

      WHEN /vpcoe/cl_rdp_helper=>sc_service_id-customer_ext.
        et_sheets = VALUE #( ( 'Code list CustomerRole' ) ( 'CustomerExtension' ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_tax_number.
    DATA: lo_badi           TYPE REF TO /vpcoe/adjust_data_retrieval,
          lv_skip           TYPE abap_bool,
          lt_knas           TYPE /vpcoe/cl_rdp_customer_data=>gty_t_tax_number,
          lt_customer_range TYPE /vpcoe/tt_r_kunnr,
          ls_sel_opt        TYPE /vpcoe/s_selopt_customer,
          lt_tax_number     TYPE gty_t_tax_number.

    CLEAR et_tax_number.

    GET BADI lo_badi.

    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-tax_number
        iv_api_type = me->mv_api_type
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      IF it_customer IS NOT INITIAL.
        SELECT c~kunnr  AS cust_num,
               c~stcdt  AS tax_number_type,
               c~stceg  AS tax_number,
               c~land1  AS country
         FROM kna1 AS c
         INTO CORRESPONDING FIELDS OF TABLE @et_tax_number
         FOR ALL ENTRIES IN @it_customer
         WHERE c~kunnr = @it_customer-id AND c~stceg <> ' '.
        IF sy-subrc <> 0.
          CLEAR et_tax_number.
        ENDIF.

        SELECT cs~kunnr  AS cust_num,
               cs~stceg  AS tax_number,
               cs~land1  AS country
          FROM knas AS cs
          INTO CORRESPONDING FIELDS OF TABLE @lt_knas
          FOR ALL ENTRIES IN @it_customer
          WHERE cs~kunnr = @it_customer-id AND cs~stceg <> ' '.
        IF sy-subrc = 0.
          et_tax_number = CORRESPONDING #( BASE ( et_tax_number ) lt_knas ).
        ELSE.
          CLEAR lt_knas.
        ENDIF.
        LOOP AT et_tax_number ASSIGNING FIELD-SYMBOL(<ls_tax>).
          <ls_tax>-tax_number_type = <ls_tax>-country && '0'.
        ENDLOOP.
        IF me->mv_cvi_indicator_active = abap_true.
          SELECT c~kunnr      AS cust_num,
                 tax~taxtype  AS tax_number_type,
                 tax~taxnum   AS tax_number,
                 tax~taxnumxl AS tax_number_xl
             INTO CORRESPONDING FIELDS OF TABLE @et_tax_number
               FROM  dfkkbptaxnum AS tax
               JOIN but000 AS b ON tax~partner = b~partner
               JOIN cvi_cust_link AS cvi ON b~partner_guid = cvi~partner_guid
               JOIN kna1 AS c  ON c~kunnr = cvi~customer
            FOR ALL ENTRIES IN @it_customer
            WHERE c~kunnr = @it_customer-id.
          IF sy-subrc = 0.
            et_tax_number = CORRESPONDING #( BASE ( et_tax_number ) lt_tax_number ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    lt_customer_range = VALUE /vpcoe/tt_r_kunnr( FOR <ls_customer> IN it_customer ( sign = 'I'
                                                                                    option = 'EQ'
                                                                                    low = <ls_customer>-id ) ).
    ls_sel_opt-customer_id = lt_customer_range.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-customer
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-customer
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-tax_number
        iv_api_type = me->mv_api_type
        is_sel_opt  = ls_sel_opt
        iv_mode     = me->mv_mode
        io_log      = io_log
      CHANGING
        ct_data     = et_tax_number.

  ENDMETHOD.
ENDCLASS.
