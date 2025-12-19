class /VPCOE/CL_RDP_SUPPLIER_DATA definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_tax_num,
        id          TYPE char10,
        description TYPE string,
      END OF gty_s_tax_num .
  types:
    BEGIN OF gty_s_sel_opt,
        ven_id    TYPE RANGE OF lifnr,
        country   TYPE RANGE OF land1,
        region    TYPE RANGE OF regio,
        crt_date  TYPE RANGE OF cddatum,
        chng_date TYPE RANGE OF cddatum,
        ekorg     TYPE RANGE OF lfm1-ekorg,
        ktokk     TYPE RANGE OF lfa1-ktokk,
      END OF gty_s_sel_opt .
  types:
    BEGIN OF gty_s_tax_number,
        ven_id        TYPE lifnr,
        tax_number_xl TYPE  bptaxnumxl, ".
        country       TYPE land1.
    INCLUDE TYPE /vpcoe/str_tax_number AS tax.
    TYPES:
       END OF gty_s_tax_number .
  types:
    BEGIN OF gty_s_tax_num_xls,
        ven_id          TYPE lifnr,
        tax_number_xl   TYPE  bptaxnumxl,
        tax_number_type TYPE  bptaxtype,
      END OF gty_s_tax_num_xls .
  types:
    gty_t_tax_number TYPE SORTED TABLE OF gty_s_tax_number WITH NON-UNIQUE KEY ven_id .
  types:
    BEGIN OF gty_s_sel_opt_ext,
        delta_only TYPE abap_bool.
    INCLUDE TYPE gty_s_sel_opt AS option.
    TYPES:
    END OF gty_s_sel_opt_ext .
  types:
    gty_t_cpi TYPE STANDARD TABLE OF bdicpident .
  types:
    gty_r_lifnr TYPE RANGE OF lifnr .                            "Id
  types:
    gty_r_country TYPE RANGE OF land1 .                            "Country
  types:
    gty_r_region TYPE RANGE OF regio .                            "Region
  types:
    gty_r_date TYPE RANGE OF cddatum .                             "Creation(Change) date
  types GTY_S_SUPPLIER type /VPCOE/STR_SUPPLIER .
  types GTY_S_SUPPLIER_JSN type /VPCOE/STR_SUPPLIER_JSN .
  types:
    gty_t_supplier TYPE SORTED TABLE OF gty_s_supplier WITH UNIQUE KEY id .
  types:
    gty_t_supplier_jsn TYPE SORTED TABLE OF gty_s_supplier_jsn WITH UNIQUE KEY id .
  types GTY_S_SUPPLIER_JSON type /VPCOE/STR_SUPPLIER_JSON .
  types:
    BEGIN OF gty_s_supplier_tables,
        supplier   TYPE gty_t_supplier_jsn,
        tax_number TYPE gty_t_tax_number,
      END OF gty_s_supplier_tables .

  methods CHANGE_STATUS
    importing
      !IV_TEST_RUN type XFELD .
  methods CONSTRUCTOR
    importing
      !IV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_SOURCE type STRING
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional .
  methods DOWNLOAD_EXCEL
    importing
      !IV_FILE_PATH type STRING
      !IS_SUPPLIER_TABLES type GTY_S_SUPPLIER_TABLES
      !IV_SAVE_BACKGROUND type ABAP_BOOL
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
  methods GET_SUPPLIER
    importing
      !IV_DELTA type ABAP_BOOL optional
      !IS_SEL_OPT type /VPCOE/S_SELOPT_SUPPLIER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    exporting
      !ET_SUPPLIER type GTY_T_SUPPLIER_JSN
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !ES_SUPPLIER_TABLES type GTY_S_SUPPLIER_TABLES .
  methods GET_SUPPLIER_DELTA
    importing
      !IV_CHNG_POINTER_ID type EDI_MESTYP
      !IS_SEL_OPT type /VPCOE/S_SELOPT_SUPPLIER
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    exporting
      !ET_SUPPLIER type GTY_T_SUPPLIER_JSN
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !ES_SUPPLIER_TABLES type GTY_S_SUPPLIER_TABLES .
  methods GET_SUPPLIER_EXT
    importing
      !IS_SEL_OPT type GTY_S_SEL_OPT_EXT
      !IT_SUPPLIER type GTY_T_SUPPLIER
    exporting
      !ET_SUPPLIER_EXT type /VPCOE/T_SUPPLIER_EXT
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods BUILD_JSON
    importing
      !IT_SUPPLIER type GTY_T_SUPPLIER_JSN
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    exporting
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods CLOSE_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
  PROTECTED SECTION.
private section.

  data MT_CPI type /VPCOE/CL_RDP_SUPPLIER_DATA=>GTY_T_CPI .
  data MV_CHNG_POINTER_ID type EDI_MESTYP .
  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SOURCE type STRING .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_MODE type /VPCOE/DE_MODE value 1 ##NO_TEXT.
  data MO_LOG type ref to /VPCOE/CL_RDP_LOG .

  methods GET_TAX_NUMBER
    importing
      !IS_SEL_OPT type /VPCOE/S_SELOPT_SUPPLIER
    exporting
      !ET_TAX_NUMBER type GTY_T_TAX_NUMBER
    changing
      !CT_SUPPLIER type GTY_T_SUPPLIER .
  methods GET_EXCEL_HEADER
    importing
      !IV_SHEET_NAME type CHAR40
      !IS_SUPPLIER_TABLES type GTY_S_SUPPLIER_TABLES
    exporting
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_DATA type ref to DATA .
  methods GET_EXCEL_SHEET
    exporting
      !ET_SHEETS type /VPCOE/TT_SHEETS_STR .
ENDCLASS.



CLASS /VPCOE/CL_RDP_SUPPLIER_DATA IMPLEMENTATION.


  METHOD build_json.
    DATA: lt_supplier_pack TYPE gty_t_supplier_jsn,
          lt_supplier	     TYPE gty_t_supplier_jsn.

    CLEAR et_json.

    IF me->mv_package_size IS INITIAL.
      lt_supplier_pack = it_supplier.
    ELSE.
      lt_supplier = it_supplier.
      LOOP AT lt_supplier ASSIGNING FIELD-SYMBOL(<ls_supplier>).
        INSERT <ls_supplier> INTO TABLE lt_supplier_pack.

        IF lines( lt_supplier_pack ) = me->mv_package_size.
          APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          <ls_json>-count = lines( lt_supplier_pack ).
          <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                           EXPORTING is_data        = VALUE gty_s_supplier_json( source   = me->mv_source
                                                                                 elements = lt_supplier_pack )
                                     iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
          REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
          CLEAR lt_supplier_pack.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF lt_supplier_pack IS NOT INITIAL.
      APPEND INITIAL LINE TO et_json ASSIGNING <ls_json>.
      <ls_json>-count = lines( lt_supplier_pack ).
      <ls_json>-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                       EXPORTING is_data        = VALUE gty_s_supplier_json( source   = me->mv_source
                                                                             elements = lt_supplier_pack )
                                 iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
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
  ENDMETHOD.


  METHOD download_excel.

    DATA: lt_table      TYPE REF TO data,
          lt_tax_header TYPE /vpcoe/cl_xls_handler=>gty_t_title.
    FIELD-SYMBOLS: <lt_data> TYPE any.

    get_excel_sheet(
      IMPORTING
        et_sheets     = DATA(lt_sheets) ).

    DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path = iv_file_path
                                                   iv_save_background = iv_save_background  ).

    LOOP AT lt_sheets ASSIGNING FIELD-SYMBOL(<ls_sheets>).

      lo_xls_file->create_range_table( iv_formula = `='Code list TaxNumberCategory'!R3C1:R662C2`
                                       iv_name = 'TaxNumberCategory' ).

      IF <ls_sheets> = 'Code list TaxNumberCategory'.

        SELECT taxtype, text
          FROM tfktaxnumtype_t
          WHERE spras = 'E'
          INTO TABLE @DATA(lt_tax).

        lt_tax_header = VALUE #(
                         ( description      = 'Category'
                           vpcoe_attribute  = 'TAXTYPE' )
                         ( description      = 'Explanation'
                           vpcoe_attribute  = 'TEXT' ) ).

        lo_xls_file->execute( iv_name = CONV #( <ls_sheets> ) ).
        lo_xls_file->fill_data(  it_item_tab = lt_tax
                                 it_title    = lt_tax_header
                                 iv_tax_number = 'Code List for Tax Number Category' ).

      ELSE.
        get_excel_header(
          EXPORTING
            iv_sheet_name     =  <ls_sheets>
            is_supplier_tables =  is_supplier_tables
          IMPORTING
            et_header         = DATA(lt_header)
            et_data           = lt_table ).

        ASSIGN lt_table->* TO <lt_data>.

        lo_xls_file->execute( iv_name = CONV #( <ls_sheets> ) ).
        lo_xls_file->fill_data( it_item_tab = <lt_data>
                                it_title    = lt_header ).
      ENDIF.

    ENDLOOP.

    lo_xls_file->save_xls_file( io_log = io_log ).

  ENDMETHOD.


  METHOD get_excel_header.

    FIELD-SYMBOLS <lt_data> TYPE any.

    TYPES:
      BEGIN OF ty_s_tax_with_furmula,
        formula TYPE string.
            INCLUDE TYPE gty_s_tax_num_xls.
    TYPES:
       END OF ty_s_tax_with_furmula.

    DATA: lt_tax_number TYPE STANDARD TABLE OF ty_s_tax_with_furmula.

    CLEAR: et_header,
           et_data.

    CASE iv_sheet_name.
      WHEN 'Supplier'.
        CREATE DATA et_data LIKE is_supplier_tables-supplier.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = is_supplier_tables-supplier.
        et_header = VALUE #(
                         ( description      = 'Supplier Number'
                           internal_name    = 'id'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LIFNR'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Supplier Name'
                           internal_name    = 'name'
                           data_type        = 'CHAR(80)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'NAME1 + NAME2'
                           vpcoe_attribute  = 'NAME' )
                         ( description      = 'Country/Region'
                           internal_name    = 'country'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LAND1'
                           vpcoe_attribute  = 'COUNTRY' )
                         ( description      = 'State/Province'
                           internal_name    = 'region_id'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'REGIO'
                           vpcoe_attribute  = 'REGION') ).
      WHEN 'SupplierTaxNumber'.
        CREATE DATA et_data LIKE lt_tax_number.
        ASSIGN et_data->* TO <lt_data>.
        lt_tax_number = VALUE #( FOR <ls_tax_num> IN is_supplier_tables-tax_number
                                      ( ven_id          = <ls_tax_num>-ven_id
                                        tax_number_xl   = COND #( WHEN <ls_tax_num>-tax_number IS NOT INITIAL
                                                                  THEN <ls_tax_num>-tax_number
                                                                  ELSE <ls_tax_num>-tax_number_xl )
                                        tax_number_type = <ls_tax_num>-tax_number_type
                                        formula         = '=VLOOKUP(RC[-1],TaxNumberCategory,2,FALSE)' ) ).
        <lt_data> = lt_tax_number.
        et_header = VALUE #(
                       ( description      = 'Supplier Number'
                         internal_name    = 'supplierId'
                         data_type        = 'CHAR(10)'
                         mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                         s4hana_attribute = 'LIFNR'
                         vpcoe_attribute  = 'VEN_ID'
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
                         vpcoe_attribute  = 'TAX_NUMBER_XL'
                         is_key           = abap_true ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_excel_sheet.
    et_sheets = VALUE #( ( 'Code list TaxNumberCategory' ) ( 'SupplierTaxNumber' ) ( 'Supplier' ) ).
  ENDMETHOD.


  METHOD get_supplier.
    DATA: lo_badi         TYPE REF TO /vpcoe/adjust_data_retrieval,
          lt_supplier_all	TYPE gty_t_supplier,
          lv_skip         TYPE abap_bool,
          ls_supplier_jsn TYPE gty_s_supplier_jsn.

    CLEAR: et_supplier,
           et_json,
           es_supplier_tables.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-supplier
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-supplier
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-supplier
        iv_api_type = me->mv_api_type
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT DISTINCT lfa1~lifnr     AS id,
                      lfa1~name1 && @space && lfa1~name2 AS name,
                      lfa1~land1     AS country,
                      lfa1~regio     AS region,
                      CASE WHEN lfa1~loevm = @abap_true THEN 'X'
                                                       ELSE '-' END AS is_marked_for_deletion,
                     lfa1~nodel      AS nodel
          FROM lfa1 LEFT JOIN cdhdr ON lfa1~lifnr = cdhdr~objectid
                    LEFT JOIN lfm1 ON lfa1~lifnr = lfm1~lifnr
                    LEFT JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
             INTO CORRESPONDING FIELDS OF TABLE @lt_supplier_all
           WHERE lfa1~lifnr  IN @is_sel_opt-supplier_id
              AND lfa1~land1  IN @is_sel_opt-country
              AND lfa1~regio  IN @is_sel_opt-region
              AND lfa1~erdat  IN @is_sel_opt-crt_date
              AND lfa1~ktokk  IN @is_sel_opt-ktokk
              AND cdhdr~udate IN @is_sel_opt-chng_date
              AND lfm1~ekorg  IN @is_sel_opt-ekorg
              AND lfb1~bukrs  IN @is_sel_opt-bukrs.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = conv #( 'Supplier' ) iv_save_log = abap_false iv_add_message_to_log = abap_true ).
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-supplier
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-supplier
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-supplier
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = lt_supplier_all.

    me->get_tax_number(
      EXPORTING
        is_sel_opt    = is_sel_opt
      IMPORTING
        et_tax_number = DATA(lt_tax_number_all)
      CHANGING
        ct_supplier   = lt_supplier_all ).

    LOOP AT lt_supplier_all ASSIGNING FIELD-SYMBOL(<ls_supplier>).

      ls_supplier_jsn = VALUE #( id          = <ls_supplier>-id
                                 name        = <ls_supplier>-name
                                 country     = <ls_supplier>-country
                                 region      = <ls_supplier>-region
                                 tax_numbers = <ls_supplier>-tax_numbers
                                 is_marked_for_deletion = <ls_supplier>-is_marked_for_deletion ).

        INSERT ls_supplier_jsn INTO TABLE et_supplier.

      IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-screen.
        IF lines( et_supplier ) > /vpcoe/cl_rdp_helper=>gc_display_amount.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CASE mv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
        me->build_json(
          EXPORTING
            it_supplier   = et_supplier
            io_log        = io_log
          IMPORTING
            et_json       = et_json ).

        CALL BADI lo_badi->adjust_json
          EXPORTING
            iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-supplier
            iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-supplier
            iv_api_type = me->mv_api_type
            io_log      = me->mo_log
          CHANGING
            ct_json     = et_json.

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
        es_supplier_tables = VALUE #( supplier   = VALUE #( FOR <ls_suppliers> IN lt_supplier_all ( id                     = <ls_suppliers>-id
                                                                                                    name                   = <ls_suppliers>-name
                                                                                                    country                = <ls_suppliers>-country
                                                                                                    region                 = <ls_suppliers>-region
                                                                                                    is_marked_for_deletion = <ls_suppliers>-is_marked_for_deletion
                                                                                                    tax_numbers            = <ls_suppliers>-tax_numbers ) )
                                      tax_number = lt_tax_number_all ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_supplier_delta.
    DATA: lt_r_supplier_id TYPE gty_r_lifnr,
          ls_sel_opt       TYPE /vpcoe/s_selopt_supplier.

    CLEAR: et_supplier,
           et_json,
           es_supplier_tables.

    mv_chng_pointer_id = iv_chng_pointer_id.

    /vpcoe/cl_rdp_helper=>read_change_pointers(
       EXPORTING
         iv_chng_pointer_id = mv_chng_pointer_id
         it_r_change_date   = is_sel_opt-chng_date
         it_r_create_date   = is_sel_opt-crt_date
       IMPORTING
         et_r_objid         = lt_r_supplier_id
         et_cpi             = mt_cpi ).

    IF lt_r_supplier_id IS NOT INITIAL.
      ls_sel_opt = is_sel_opt.

      "For Delta Mode Create/Change date shouldn't be taken into consideration for 'regular' select
      CLEAR: ls_sel_opt-chng_date,
             ls_sel_opt-crt_date,
             ls_sel_opt-supplier_id.

      LOOP AT lt_r_supplier_id ASSIGNING FIELD-SYMBOL(<ls_r_supplier_id>).
        IF <ls_r_supplier_id>-low IN is_sel_opt-supplier_id.
          INSERT <ls_r_supplier_id> INTO TABLE ls_sel_opt-supplier_id.
        ENDIF.
      ENDLOOP.

*      ls_sel_opt-supplier_id = lt_r_supplier_id.

      me->get_supplier(
        EXPORTING
          iv_delta   = abap_true
          is_sel_opt = ls_sel_opt
        IMPORTING
          et_supplier        = et_supplier
          et_json            = et_json
          es_supplier_tables = es_supplier_tables ).
    ENDIF.

  ENDMETHOD.


  METHOD get_supplier_ext.

  ENDMETHOD.


  METHOD get_tax_number.

    DATA: lo_badi       TYPE REF TO /vpcoe/adjust_data_retrieval,
          lv_skip       TYPE abap_bool,
          lt_tax_number TYPE gty_t_tax_number.

    DATA: lt_lfas TYPE gty_t_tax_number.

    CLEAR et_tax_number.

    GET BADI lo_badi.

    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_api_type = me->mv_api_type
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-supplier
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-supplier
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-tax_number
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      IF ct_supplier IS NOT INITIAL.
        SELECT SINGLE active_indicator
          FROM mdsc_ctrl_opt_a
            INTO @DATA(lv_active_indicator)
               WHERE sync_obj_target = 'VENDOR'.

        SELECT v~lifnr  AS ven_id,
               v~stcdt  AS tax_number_type,
               v~stceg  AS tax_number,
               v~land1  AS country
          FROM lfa1 AS v
          INTO CORRESPONDING FIELDS OF TABLE @et_tax_number
          FOR ALL ENTRIES IN @ct_supplier
          WHERE v~lifnr = @ct_supplier-id AND v~stceg <> ' '.
        IF sy-subrc <> 0.
          CLEAR et_tax_number.
        ENDIF.

        SELECT vs~stceg AS tax_number,
               vs~lifnr AS ven_id,
               vs~land1  AS country
          FROM  lfas AS vs
          INTO CORRESPONDING FIELDS OF TABLE @lt_lfas
          FOR ALL ENTRIES IN @ct_supplier
          WHERE vs~lifnr = @ct_supplier-id AND vs~stceg <> ' '.
        IF sy-subrc = 0.
          et_tax_number = CORRESPONDING #( BASE ( et_tax_number ) lt_lfas ).
        ELSE.
          CLEAR lt_lfas.
        ENDIF.
        LOOP AT et_tax_number ASSIGNING FIELD-SYMBOL(<ls_tax>).
          <ls_tax>-tax_number_type = <ls_tax>-country && '0'.
        ENDLOOP.
        IF lv_active_indicator = abap_true.
          SELECT v~lifnr      AS ven_id,
                 tax~taxtype  AS tax_number_type,
                 tax~taxnum   AS tax_number,
                 tax~taxnumxl AS tax_number_xl
             INTO CORRESPONDING FIELDS OF TABLE @lt_tax_number
               FROM  dfkkbptaxnum AS tax
                  JOIN but000 AS b ON tax~partner = b~partner
                  JOIN cvi_vend_link AS cvi ON b~partner_guid = cvi~partner_guid
                  JOIN lfa1 AS v  ON v~lifnr = cvi~vendor
              FOR ALL ENTRIES IN @ct_supplier
                WHERE v~lifnr = @ct_supplier-id.
          IF sy-subrc = 0.
            et_tax_number = CORRESPONDING #( BASE ( et_tax_number ) lt_tax_number ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-supplier
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-supplier
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-tax_number
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_tax_number.

    LOOP AT ct_supplier ASSIGNING FIELD-SYMBOL(<ls_supplier>).
      <ls_supplier>-tax_numbers = VALUE #( FOR <ls_tax_number> IN et_tax_number
                                               WHERE ( ven_id = <ls_supplier>-id )
                                             ( tax_number      = COND #( WHEN <ls_tax_number>-tax_number IS NOT INITIAL
                                                                           THEN <ls_tax_number>-tax_number
                                                                             ELSE <ls_tax_number>-tax_number_xl )
                                               tax_number_type = <ls_tax_number>-tax_number_type ) ).

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
