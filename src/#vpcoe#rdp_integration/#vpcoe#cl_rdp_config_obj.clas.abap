class /VPCOE/CL_RDP_CONFIG_OBJ definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_json,
        source   TYPE string,
        elements TYPE REF TO data,
      END OF gty_s_json .
  types:
    gty_t_uom_t TYPE SORTED TABLE OF /vpcoe/s_uom_t WITH NON-UNIQUE KEY language .
  types GTY_S_UOM type /VPCOE/S_UOM .
  types:
    BEGIN OF gty_s_uom_t,
        id TYPE msehi.
    INCLUDE TYPE /vpcoe/s_uom_t AS text.
    TYPES:
    END OF gty_s_uom_t .
  types:
    gty_t_uom_dimension_t TYPE SORTED TABLE OF /vpcoe/s_uom_dimension_t WITH NON-UNIQUE KEY language .
  types GTY_S_UOM_DIMENSION type /VPCOE/S_UOM_DIMENSION .
  types:
    BEGIN OF gty_s_uom_dimension_t,
        id TYPE dimid.
    INCLUDE TYPE /vpcoe/s_uom_dimension_t AS text.
    TYPES:
    END OF gty_s_uom_dimension_t .
  types:
    gty_t_uom_iso_code_t     TYPE SORTED TABLE OF /vpcoe/s_uom_iso_code_t WITH NON-UNIQUE KEY language .
  types GTY_S_UOM_ISO_CODE type /VPCOE/S_UOM_ISO_CODE .
  types:
    BEGIN OF gty_s_uom_iso_code_t,
        id TYPE isocd_unit.
    INCLUDE TYPE /vpcoe/s_uom_iso_code_t AS text.
    TYPES:
    END OF gty_s_uom_iso_code_t .
  types:
    gty_t_currency_t         TYPE SORTED TABLE OF /vpcoe/s_currency_t WITH NON-UNIQUE KEY language .
  types GTY_S_CURRENCY type /VPCOE/S_CURRENCY .
  types:
    BEGIN OF gty_ls_currency,
        id                        TYPE waers_curc,
        iso_code                  TYPE isocd,
        is_primary_i_s_o_currency TYPE /vpcoe/de_is_primary_iso_cur,
        name                      TYPE ltext,
        language                  TYPE spras,
      END OF gty_ls_currency .
  types:
    gty_tt_currency TYPE SORTED TABLE OF gty_ls_currency WITH NON-UNIQUE KEY id .
  types:
    BEGIN OF gty_s_currency_t,
        id TYPE waers.
    INCLUDE TYPE /vpcoe/s_currency_t AS text.
    TYPES:
    END OF gty_s_currency_t .
  types:
    gty_t_country_t          TYPE SORTED TABLE OF /vpcoe/s_country_t WITH NON-UNIQUE KEY language .
  types GTY_S_COUNTRY type /VPCOE/S_COUNTRY .
  types:
    BEGIN OF gty_s_country_t,
        id TYPE land1.
    INCLUDE TYPE /vpcoe/s_country_t AS text.
    TYPES:
    END OF gty_s_country_t .
  types:
    gty_t_region_t           TYPE SORTED TABLE OF /vpcoe/s_region_t WITH NON-UNIQUE KEY language .
  types GTY_S_REGION type /VPCOE/S_REGION .
  types:
    BEGIN OF gty_s_region_t,
        id TYPE regio.
    INCLUDE TYPE /vpcoe/s_region_t_xml AS text.
    TYPES:
    END OF gty_s_region_t .
  types:
    gty_t_product_type_t     TYPE SORTED TABLE OF /vpcoe/s_product_type_t WITH NON-UNIQUE KEY language .
  types GTY_S_PRODUCT_TYPE type /VPCOE/S_PRODUCT_TYPE .
  types:
    BEGIN OF gty_s_product_type_t,
        id TYPE mtart.
    INCLUDE TYPE /vpcoe/s_product_type_t AS text.
    TYPES:
    END OF gty_s_product_type_t .
  types:
    gty_t_product_group_t    TYPE SORTED TABLE OF /vpcoe/s_product_group_t WITH NON-UNIQUE KEY language .
  types GTY_S_PRODUCT_GROUP type /VPCOE/S_PRODUCT_GROUP .
  types:
    BEGIN OF gty_s_product_group_t,
        id TYPE matkl.
    INCLUDE TYPE /vpcoe/s_product_group_t AS text.
    TYPES:
    END OF gty_s_product_group_t .
  types:
    BEGIN OF gty_s_product_hierarchy_t,
        id TYPE prodh.
    INCLUDE TYPE /vpcoe/s_product_hierarchy_t AS text.
    TYPES:
    END OF gty_s_product_hierarchy_t .
  types:
    BEGIN OF gty_s_dd_type_t,
        id TYPE lfart.
    INCLUDE TYPE /vpcoe/s_dlvr_doc_type_t AS text.
    TYPES:
    END OF gty_s_dd_type_t .
  types:
    BEGIN OF gty_dd_item_category_t,
        id TYPE pstyv.
    INCLUDE TYPE /vpcoe/s_dlvr_doc_item_type_t AS text.
    TYPES:
    END OF gty_dd_item_category_t .
  types:
    BEGIN OF gty_s_movement_type_t,
        id TYPE bwart.
    INCLUDE TYPE /vpcoe/s_movement_type_t AS text.
    TYPES:
    END OF gty_s_movement_type_t .
  types:
    BEGIN OF gty_s_incoterms_t,
        id TYPE inco1.
    INCLUDE TYPE /vpcoe/s_incoterms_t AS text.
    TYPES:
    END OF gty_s_incoterms_t .
  types:
    gty_t_dd_type_t          TYPE SORTED TABLE OF /vpcoe/s_dlvr_doc_type_t WITH NON-UNIQUE KEY language .
  types:
    gty_t_dd_item_category_t TYPE SORTED TABLE OF /vpcoe/s_dlvr_doc_item_type_t WITH NON-UNIQUE KEY language .
  types:
    gty_t_movement_type_t    TYPE SORTED TABLE OF /vpcoe/s_movement_type_t WITH NON-UNIQUE KEY language .
  types:
    gty_t_uom                TYPE SORTED TABLE OF gty_s_uom WITH NON-UNIQUE KEY id .
  types:
    gty_t_uom_dimension      TYPE SORTED TABLE OF gty_s_uom_dimension WITH NON-UNIQUE KEY id .
  types:
    gty_t_uom_iso_code       TYPE SORTED TABLE OF  /vpcoe/s_uom_iso_code WITH NON-UNIQUE KEY id .
  types:
    gty_t_currency           TYPE SORTED TABLE OF /vpcoe/s_currency WITH NON-UNIQUE KEY id .
  types:
    gty_t_country            TYPE SORTED TABLE OF /vpcoe/s_country WITH NON-UNIQUE KEY id .
  types:
    gty_t_region             TYPE SORTED  TABLE OF /vpcoe/s_region WITH NON-UNIQUE KEY id .
  types:
    gty_t_product_type       TYPE SORTED TABLE OF /vpcoe/s_product_type WITH NON-UNIQUE KEY id .
  types:
    gty_t_product_group      TYPE SORTED TABLE OF /vpcoe/s_product_group WITH NON-UNIQUE KEY id .
  types:
    gty_t_dd_type            TYPE SORTED TABLE OF /vpcoe/s_dlvr_doc_type WITH NON-UNIQUE KEY id .
  types:
    gty_t_dd_item_category   TYPE SORTED TABLE OF /vpcoe/s_dlvr_doc_item_type WITH NON-UNIQUE KEY id .
  types:
    gty_t_movement_type      TYPE SORTED TABLE OF /vpcoe/s_movement_type WITH NON-UNIQUE KEY id .
  types:
    BEGIN OF gty_s_config_tables,
        uom_dimension       TYPE gty_t_uom_dimension,
        uom_dimension_t     TYPE SORTED TABLE OF gty_s_uom_dimension_t WITH NON-UNIQUE KEY id,
        uom                 TYPE gty_t_uom,
        uom_t               TYPE SORTED TABLE OF gty_s_uom_t WITH NON-UNIQUE KEY id,
        uom_iso_code        TYPE gty_t_uom_iso_code,
        uom_iso_code_t      TYPE SORTED TABLE OF gty_s_uom_iso_code_t WITH NON-UNIQUE KEY id,
        currency            TYPE gty_t_currency,
        currency_t          TYPE SORTED TABLE OF gty_s_currency_t WITH NON-UNIQUE KEY id,
        country             TYPE gty_t_country,
        country_t           TYPE SORTED TABLE OF gty_s_country_t WITH NON-UNIQUE KEY id,
        region              TYPE gty_t_region,
        region_t            TYPE SORTED TABLE OF gty_s_region_t WITH NON-UNIQUE KEY id,
        product_type        TYPE gty_t_product_type,
        product_type_t      TYPE SORTED TABLE OF gty_s_product_type_t WITH NON-UNIQUE KEY id,
        product_group       TYPE gty_t_product_group,
        product_group_t     TYPE SORTED TABLE OF gty_s_product_group_t WITH NON-UNIQUE KEY id,
        product_hierarchy   TYPE /vpcoe/tt_product_hierarchy,
        product_hierarchy_t TYPE SORTED TABLE OF gty_s_product_hierarchy_t WITH NON-UNIQUE KEY id,
        dd_type             TYPE gty_t_dd_type,
        dd_type_t           TYPE SORTED TABLE OF gty_s_dd_type_t WITH NON-UNIQUE KEY id,
        dd_item_category    TYPE gty_t_dd_item_category,
        dd_item_category_t  TYPE SORTED TABLE OF gty_dd_item_category_t WITH NON-UNIQUE KEY id,
        movement_type       TYPE gty_t_movement_type,
        movement_type_t     TYPE SORTED TABLE OF gty_s_movement_type_t WITH NON-UNIQUE KEY id,

      END OF gty_s_config_tables .
  types:
    BEGIN OF gty_s_inco_tables,
        incoterms   TYPE /vpcoe/tt_incoterms,
        incoterms_t TYPE SORTED TABLE OF gty_s_incoterms_t WITH NON-UNIQUE KEY id,
      END OF gty_s_inco_tables .

  methods CONSTRUCTOR
    importing
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER optional
      !IV_API_TYPE type /VPCOE/DE_API_TYPE optional .
  methods DOWNLOAD_EXCEL
    importing
      !IV_FILE_PATH type STRING
      !IV_INCOTERMS type FLAG default ABAP_FALSE
      !IV_SAVE_BACKGROUND type ABAP_BOOL
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
  methods GET_COUNTRY
    exporting
      !ET_COUNTRY type GTY_T_COUNTRY
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_CURRENCY
    exporting
      !ET_CURRENCY type GTY_T_CURRENCY
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_DLVR_DOC_ITEM_CAT
    exporting
      !ET_DLVR_DOC_ITEM_CAT type GTY_T_DD_ITEM_CATEGORY
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_DLVR_DOC_TYPE
    exporting
      !ET_DLVR_DOC_TYPE type GTY_T_DD_TYPE
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_INCOTERMS
    exporting
      !ET_INCOTERMS type /VPCOE/TT_INCOTERMS
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_MOVEMENT_TYPE
    exporting
      !ET_MOVEMENT_TYPE type GTY_T_MOVEMENT_TYPE
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_PRODUCT_GROUP
    exporting
      !ET_PRODUCT_GROUP type GTY_T_PRODUCT_GROUP
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_PRODUCT_HIERARCHY
    exporting
      !ET_PRODUCT_HIERARCHY type /VPCOE/TT_PRODUCT_HIERARCHY
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_PRODUCT_TYPE
    exporting
      !ET_PRODUCT_TYPE type GTY_T_PRODUCT_TYPE
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_REGION
    exporting
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !ET_REGION type GTY_T_REGION .
  methods GET_UOM
    exporting
      !ET_UOM type GTY_T_UOM
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_UOM_DIMENSION
    exporting
      !ET_UOM_D type GTY_T_UOM_DIMENSION
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_UOM_ISO_CODE
    exporting
      !ET_ISO_UOM type GTY_T_UOM_ISO_CODE
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods SEND
    importing
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IT_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !IO_CUST type ref to /VPCOE/CL_COMMON_HELPER
    exporting
      !EV_STATUS type I
      !EV_REASON type STRING
    raising
      CX_OA2C
      CX_UUID_ERROR .
protected section.

  data MO_LOG type ref to /VPCOE/CL_RDP_LOG .
  data MO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER .
  data MS_CONFIG_TABLES type GTY_S_CONFIG_TABLES .
  data MS_INCO_TABLES type GTY_S_INCO_TABLES .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_MODE type /VPCOE/DE_MODE value 1 ##NO_TEXT.
  data MV_SRV_ID type /VPCOE/DE_SERVICE_ID .

  methods ADJUST_DATA_RETRIEVAL
    changing
      !CT_DATA type DATA .
  methods ADJUST_JSON
    changing
      !CT_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods ADJUST_MAPPING
    importing
      !IS_DATA type DATA
    changing
      !CS_DATA type DATA .
  methods ADJUST_TEXT_MAPPING
    importing
      !IT_DATA type DATA
    changing
      !CT_DATA type ANY TABLE .
  methods GET_EXCEL_HEADER
    importing
      !IV_SHEET_NAME type CHAR40
    exporting
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_DATA type ref to DATA .
  methods GET_EXCEL_SHEET
    importing
      !IV_INCOTERMS type FLAG default ABAP_FALSE
    exporting
      !ET_SHEETS type /VPCOE/TT_SHEETS_STR .
  methods SKIP_SELECTION
    importing
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
    returning
      value(RV_SKIP) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS /VPCOE/CL_RDP_CONFIG_OBJ IMPLEMENTATION.


  METHOD adjust_data_retrieval.

    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    GET BADI lo_badi.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
        iv_srv_id   = me->mv_srv_id
        iv_api_type = me->mv_api_type
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = ct_data.

  ENDMETHOD.


  METHOD adjust_json.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    GET BADI lo_badi.

    CALL BADI lo_badi->adjust_json
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
        iv_srv_id   = me->mv_srv_id
        iv_api_type = me->mv_api_type
        io_log      = me->mo_log
      CHANGING
        ct_json     = ct_json.

  ENDMETHOD.


  METHOD adjust_mapping.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    GET BADI lo_badi.

    CALL BADI lo_badi->adjust_mapping
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
        iv_srv_id   = me->mv_srv_id
        iv_api_type = me->mv_api_type
        is_data     = is_data
        io_log      = me->mo_log
      CHANGING
        cs_data     = cs_data.
  ENDMETHOD.


  METHOD adjust_text_mapping.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    GET BADI lo_badi.

    CALL BADI lo_badi->adjust_text_mapping
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
        iv_srv_id   = me->mv_srv_id
        iv_api_type = me->mv_api_type
        it_data     = it_data
        io_log      = me->mo_log
      CHANGING
        ct_data     = ct_data.
  ENDMETHOD.


  METHOD constructor.

    me->mv_api_type   = io_rdp_helper->get_api_type( ).
    me->mv_mode       = iv_mode.
    me->mo_log        = io_log.
    me->mo_rdp_helper = io_rdp_helper.

  ENDMETHOD.


  METHOD download_excel.
    DATA: lr_table  TYPE REF TO data.
    FIELD-SYMBOLS: <lt_data> TYPE any.

    me->get_excel_sheet(
         EXPORTING
           iv_incoterms = iv_incoterms
         IMPORTING
           et_sheets    = DATA(lt_sheets) ).

    DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path            = iv_file_path
                                                   iv_save_background = iv_save_background ).

    LOOP AT lt_sheets ASSIGNING FIELD-SYMBOL(<ls_sheets>).

      me->get_excel_header(
            EXPORTING
              iv_sheet_name =  <ls_sheets>
            IMPORTING
              et_header     = DATA(lt_header)
              et_data       = lr_table ).

      ASSIGN lr_table->* TO <lt_data>.

      lo_xls_file->execute( iv_name = CONV #( <ls_sheets> ) ).

      lo_xls_file->fill_data( it_item_tab = <lt_data>
                              it_title    = lt_header ).

    ENDLOOP.

    lo_xls_file->save_xls_file( io_log = io_log ).

  ENDMETHOD.


  METHOD get_country.
    DATA: lt_text     TYPE gty_t_country_t,
          ls_json     TYPE gty_s_json,
          ls_elements TYPE /vpcoe/s_country,
          lv_json     TYPE string,
          ls_jsons    LIKE LINE OF et_json,
          lt_mapping  TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_country.

    CLEAR: et_country,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-country ).

      SELECT h~land1 AS id,
             h~intca AS iso_code,
             h~intcn3 AS three_digit_iso_code,
             h~intca3 AS three_letter_iso_code,
             CASE WHEN h~xegld = 'X' THEN 'X'
                                     ELSE '-'
                  END AS iseuropeanunionmember,
              CASE WHEN t~landx50 = ' ' THEN t~landx
                                        ELSE t~landx50
                  END AS name,
              t~spras AS language
         FROM t005 AS h JOIN t005t AS t ON h~land1 = t~land1
              INTO TABLE @DATA(lt_data).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Country' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_country.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    lt_mapping = VALUE #( ( abap = `iseuropeanunionmember`  json = `isEuropeanUnionMember` ) ) .

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      lt_text = VALUE gty_t_country_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                          language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING ct_data = lt_text ).

      ls_elements-id                    = <lt_data>-id.
      ls_elements-iso_code              = <lt_data>-iso_code.
      ls_elements-three_digit_iso_code  = <lt_data>-three_digit_iso_code.
      ls_elements-three_letter_iso_code = <lt_data>-three_letter_iso_code.
      ls_elements-iseuropeanunionmember = <lt_data>-iseuropeanunionmember.
      ls_elements-texts                 = lt_text.

      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data  = ls_elements ).

      INSERT ls_elements INTO TABLE et_country.
      INSERT ls_elements INTO TABLE <lt_elements>.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_json
                                                                  iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                  it_name_mappings  = lt_mapping
                                                                  iv_numc_as_string = abap_true ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.

        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_json
                                                                iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                it_name_mappings  = lt_mapping
                                                                iv_numc_as_string = abap_true ).
      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.

      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF '"threeDigitIsoCode":0,' IN <ls_json>-elements WITH '' ##no_text.
      REPLACE ALL OCCURRENCES OF '"threeLetterIsoCode":"",' IN <ls_json>-elements WITH '' ##no_text.
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-country = et_country.
      ms_config_tables-country_t = VALUE #( FOR <ls_data> IN lt_data ( id       = <ls_data>-id
                                                                       name     = <ls_data>-name
                                                                       language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_currency.
    DATA: ls_json     TYPE gty_s_json,
          lv_json     TYPE string,
          ls_elements TYPE /vpcoe/s_currency,
          ls_jsons    LIKE LINE OF et_json.

    DATA: lt_currency TYPE TABLE OF gty_ls_currency.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_currency.

    CLEAR: et_currency,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-currency ).
      SELECT h~waers AS id,
             h~isocd AS iso_code,
             CASE WHEN h~xprimary = 'X'
                    THEN 'true' ELSE 'false' END AS is_primary_i_s_o_currency,
             t~ltext AS name,
             t~spras AS language
        FROM tcurc AS h
        JOIN tcurt AS t ON h~waers = t~waers
      INTO TABLE @DATA(lt_data).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Currency' ) ).
    ENDIF.

    LOOP AT lt_data INTO DATA(ls_data)
      GROUP BY ( isocd = ls_data-iso_code
                 spras = ls_data-language
                 size = GROUP SIZE )
       ASCENDING REFERENCE INTO DATA(ls_group_ref).

      IF ls_group_ref->*-size > 1.
        lt_currency = VALUE gty_tt_currency( BASE lt_currency FOR <isocd> IN GROUP ls_group_ref ( <isocd> ) ).
        DELETE lt_currency WHERE is_primary_i_s_o_currency = /vpcoe/cl_rdp_helper=>sc_bool-false AND iso_code = ls_group_ref->isocd.
      ELSEIF ls_group_ref->*-size = 1.
        lt_currency = VALUE gty_tt_currency( BASE lt_currency FOR <isocd> IN GROUP ls_group_ref ( <isocd> ) ).
      ENDIF.
    ENDLOOP.

    me->adjust_data_retrieval( CHANGING ct_data = lt_currency ).

    CREATE DATA ls_json-elements TYPE gty_t_currency.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_currency ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_currency_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                                 language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements = VALUE #( id       = <lt_data>-id
                             iso_code = <lt_data>-iso_code
                             is_primary_i_s_o_currency = <lt_data>-is_primary_i_s_o_currency
                             texts    = lt_text ).

      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING  cs_data  = ls_elements ).

      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_currency.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
        ls_jsons-elements = lv_json.
        ls_jsons-count    = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
      ls_jsons-elements = lv_json.
      ls_jsons-count    = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-currency = et_currency.
      ms_config_tables-currency_t = VALUE #( FOR <ls_currency> IN lt_data ( id = <ls_currency>-id
                                                                            name = <ls_currency>-name
                                                                            language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_currency>-language ) ) )."CORRESPONDING #( lt_currency ).
    ENDIF.

  ENDMETHOD.


  METHOD get_dlvr_doc_item_cat.
    DATA: ls_json     TYPE gty_s_json,
          lv_json     TYPE string,
          ls_elements TYPE /vpcoe/s_dlvr_doc_item_type,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_dd_item_category.

    CLEAR: et_dlvr_doc_item_cat,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-delivery_doc_item_cat ).
      SELECT h~pstyv AS id,
             t~vtext AS name,
             t~spras AS language
      INTO TABLE @DATA(lt_data)
        FROM tvap AS h
        JOIN tvapt AS t ON h~pstyv = t~pstyv .

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Delivery Document Item Category' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_dd_item_category.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_dd_item_category_t( FOR <ls_data> IN GROUP <lt_data> ( name     = <ls_data>-name
                                                                                         language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).
      ls_elements-id    = <lt_data>-id.
      ls_elements-texts = lt_text.

      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data  = ls_elements ).

      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_dlvr_doc_item_cat.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.

    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-dd_item_category = et_dlvr_doc_item_cat.
      ms_config_tables-dd_item_category_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                                name = <ls_data>-name
                                                                                language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) )."CORRESPONDING #( lt_data ).
    ENDIF.
  ENDMETHOD.


  METHOD get_dlvr_doc_type.
    DATA: ls_json     TYPE gty_s_json,
          lv_json     TYPE string,
          ls_elements TYPE /vpcoe/s_dlvr_doc_type,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_dd_type.

    CLEAR: et_dlvr_doc_type,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-delivery_doc_type ).
      SELECT h~lfart AS id,
             t~vtext AS name,
             t~spras AS language
      INTO TABLE @DATA(lt_data)
        FROM tvlk AS h JOIN tvlkt AS t ON h~lfart = t~lfart.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      me->mo_log->add_msg_progress( iv_level = CONV #( 'Delivery Document Type' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_dd_type.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_dd_type_t( FOR <ls_data> IN GROUP <lt_data> ( name     = <ls_data>-name
                                                                                language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id    = <lt_data>-id.
      ls_elements-texts = lt_text.

      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING  cs_data = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_dlvr_doc_type.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-dd_type = et_dlvr_doc_type.
      ms_config_tables-dd_type_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                       name = <ls_data>-name
                                                                       language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) )."CORRESPONDING #( lt_data ).
    ENDIF.

  ENDMETHOD.


  METHOD get_excel_header.
    CLEAR: et_header, et_data.

    FIELD-SYMBOLS <lt_data> TYPE any.

    CASE iv_sheet_name.
      WHEN 'MovementTypeText'.
        CREATE DATA et_data LIKE ms_config_tables-movement_type_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-movement_type_t.
        et_header = VALUE #( ( description      = 'Movement Type'
                           internal_name    = 'movementTypeId'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'BWART'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(20)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'BTEXT'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'MovementType'.
        CREATE DATA et_data LIKE ms_config_tables-movement_type.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-movement_type.
        et_header = VALUE #( ( description      = 'Movement Type'
                           internal_name    = 'id'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'BWART'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).
      WHEN 'DeliveryDocumentItemCategoryTex'.
        CREATE DATA et_data LIKE ms_config_tables-dd_item_category_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-dd_item_category_t.
        et_header = VALUE #( ( description      = 'Delivery Document Item Category'
                           internal_name    = 'deliveryDocumentItemCategoryId'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'PSTYV'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(20)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'VTEXT'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'DeliveryDocumentItemCategory'.
        CREATE DATA et_data LIKE ms_config_tables-dd_item_category.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-dd_item_category.
        et_header = VALUE #( ( description      = 'Delivery Document Item Category'
                           internal_name    = 'id'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'PSTYV'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).
      WHEN 'DeliveryDocumentTypeText'.
        CREATE DATA et_data LIKE ms_config_tables-dd_type_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-dd_type_t.
        et_header = VALUE #( ( description      = 'Delivery Document Type'
                           internal_name    = 'deliveryDocumentTypeId'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LFART'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(20)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'VTEXT'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'DeliveryDocumentType'.
        CREATE DATA et_data LIKE ms_config_tables-dd_type.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-dd_type.
        et_header = VALUE #( ( description      = 'Delivery Document Type'
                           internal_name    = 'id'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LFART'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).
      WHEN 'ProductHierarchyText'.
        CREATE DATA et_data LIKE ms_config_tables-product_hierarchy_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-product_hierarchy_t.
        et_header = VALUE #( ( description      = 'Product Hierarchy'
                           internal_name    = 'productHierarchyId'
                           data_type        = 'CHAR(18)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'PRODH'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'VTEXT'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'ProductHierarchy'.
        CREATE DATA et_data LIKE ms_config_tables-product_hierarchy.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-product_hierarchy.
        et_header = VALUE #( ( description      = 'Product Hierarchy'
                           internal_name    = 'id'
                           data_type        = 'CHAR(18)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'PRODH'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Number of level'
                           internal_name    = 'level'
                           data_type        = 'NUMC(1)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'STUFE'
                           vpcoe_attribute  = 'LEVEL' ) ).
      WHEN 'ProductGroupText'.
        CREATE DATA et_data LIKE ms_config_tables-product_group_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-product_group_t.
        et_header = VALUE #( ( description      = 'Product Group'
                           internal_name    = 'productGroupId'
                           data_type        = 'CHAR(9)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATKL'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(20)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'WGBEZ'
                           vpcoe_attribute  = 'NAME' )
                         ( description      = 'Product Group Text'
                           internal_name    = 'text'
                           data_type        = 'CHAR(60)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'WGBEZ60'
                           vpcoe_attribute  = 'DESCRIPTION' ) ).
      WHEN 'ProductGroup'.
        CREATE DATA et_data LIKE ms_config_tables-product_group.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-product_group.
        et_header = VALUE #( ( description      = 'Product Group'
                           internal_name    = 'id'
                           data_type        = 'CHAR(9)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATKL'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).
      WHEN 'ProductTypeText'.
        CREATE DATA et_data LIKE ms_config_tables-product_type_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-product_type_t.
        et_header = VALUE #( ( description      = 'Product Type'
                           internal_name    = 'productTypeId'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MTART'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(25)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MTBEZ'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'ProductType'.
        CREATE DATA et_data LIKE ms_config_tables-product_type.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-product_type.
        et_header = VALUE #( ( description      = 'Product Type'
                           internal_name    = 'id'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MTART'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).
      WHEN 'RegionText'.
        CREATE DATA et_data LIKE ms_config_tables-region_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-region_t.
        et_header = VALUE #( ( description      = 'Country/Region'
                           internal_name    = 'countryId'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LAND1'
                           vpcoe_attribute  = 'COUNTRY'
                           is_key           = abap_true )
                         ( description      = 'State/Province'
                           internal_name    = 'regionId'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'BLAND'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(20)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'BEZEI'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'Region'.
        CREATE DATA et_data LIKE ms_config_tables-region.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-region.
        et_header = VALUE #( ( description      = 'Country/Region'
                           internal_name    = 'countryId'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LAND1'
                           vpcoe_attribute  = 'COUNTRY'
                           is_key           = abap_true )
                         ( description      = 'State/Province'
                           internal_name    = 'id'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'BLAND'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).
      WHEN 'CountryText'.
        CREATE DATA et_data LIKE ms_config_tables-country_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-country_t.
        et_header = VALUE #( ( description      = 'Country/Region'
                           internal_name    = 'countryId'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LAND1'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(50)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LANDX50'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'Country'.
        CREATE DATA et_data LIKE ms_config_tables-country.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-country.
        et_header = VALUE #( ( description      = 'Country/Region'
                           internal_name    = 'id'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LAND1'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Country ISO Code'
                           internal_name    = 'isoCode'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'INTCA'
                           vpcoe_attribute  = 'ISO_CODE' )
                         ( description      = 'European Union Member'
                           internal_name    = 'isEuropeanUnionMember'
                           data_type        = 'CHAR(5)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'XEGLD'
                           vpcoe_attribute  = 'ISEUROPEANUNIONMEMBER' ) ).
      WHEN 'CurrencyText'.
        CREATE DATA et_data LIKE ms_config_tables-currency_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-currency_t.
        et_header = VALUE #( ( description      = 'Currency'
                           internal_name    = 'currencyId'
                           data_type        = 'CHAR(5)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'WAERS'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'LTEXT'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'Currency'.
        CREATE DATA et_data LIKE ms_config_tables-currency.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-currency.
        et_header = VALUE #( ( description      = 'Currency'
                           internal_name    = 'id'
                           data_type        = 'CUKY(5)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'WAERS'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Currency ISO code '
                           internal_name    = 'isoCode'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'ISOCD'
                           vpcoe_attribute  = 'ISO_CODE' )
                         ( description      = 'Primary Currency for ISO code'
                           internal_name    = 'isPrimaryISOCurrency'
                           data_type        = 'CHAR(5)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'XPRIMARY'
                           vpcoe_attribute  = 'IS_PRIMARY_I_S_O_CURRENCY' ) ).
      WHEN 'UnitOfMeasureISOCodeText'.
        CREATE DATA et_data LIKE ms_config_tables-uom_iso_code_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-uom_iso_code_t.
        et_header = VALUE #( ( description      = 'Unit of Measure ISO Code'
                           internal_name    = 'unitOfMeasureCodeId'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'ISOCODE'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(25)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'ISOTXT'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'UnitOfMeasureISOCode'.
        CREATE DATA et_data LIKE ms_config_tables-uom_iso_code.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-uom_iso_code.
        et_header = VALUE #( ( description      = 'Unit of Measure ISO Code'
                           internal_name    = 'id'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'ISOCODE'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).
      WHEN 'UnitOfMeasureText'.
        CREATE DATA et_data LIKE ms_config_tables-uom_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-uom_t.
        et_header = VALUE #( ( description      = 'Unit of Measure'
                           internal_name    = 'unitOfMeasureId'
                           data_type        = 'UNIT(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MSEHI'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MSEHT'
                           vpcoe_attribute  = 'NAME' )
                         ( description      = 'Unit of Measure Technical Name'
                           internal_name    = 'technicalName'
                           data_type        = 'CHAR(6)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MSEH6'
                           vpcoe_attribute  = 'TECHNICAL_NAME' )
                         ( description      = 'Unit of Measure Commercial Name'
                           internal_name    = 'commercialName'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MSEH3'
                           vpcoe_attribute  = 'COMMERCIAL_NAME' ) ).
      WHEN 'UnitOfMeasure'.
        CREATE DATA et_data LIKE ms_config_tables-uom.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-uom.
        et_header = VALUE #( ( description      = 'Unit of Measure'
                           internal_name    = 'id'
                           data_type        = 'UNIT(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MSEHI'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Unit of Measure ISO Code'
                           internal_name    = 'isoCode'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ISOCODE'
                           vpcoe_attribute  = 'ISO_CODE' )
                         ( description      = 'Dimension'
                           internal_name    = 'dimension'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'DIMID'
                           vpcoe_attribute  = 'DIMENSION' )
                         ( description      = 'SI Unit Conversion Rate Numerator'
                           internal_name    = 'siUnitCnvrsnRateNumerator'
                           data_type        = 'INT4(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'ZAEHL'
                           vpcoe_attribute  = 'SI_UNIT_CNVRSN_RATE_NUMR' )
                         ( description      = 'SI Unit Conversion Rate Denominator'
                           internal_name    = 'siUnitCnvrsnRateDenominator'
                           data_type        = 'INT4(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'NENNR'
                           vpcoe_attribute  = 'SI_UNIT_CNVRSN_RATE_DENOMR' )
                         ( description      = 'SI Unit Conversion Rate Exponent'
                           internal_name    = 'siUnitCnvrsnRateExponent'
                           data_type        = 'INT2(5,0)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'EXP10'
                           vpcoe_attribute  = 'SI_UNIT_CNVRSN_RATE_EXPONENT' )
                         ( description      = 'SI Unit Conversion Additive Value'
                           internal_name    = 'siUnitCnvrsnAdditiveValue'
                           data_type        = 'DEC(9,6)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'ADDKO'
                           vpcoe_attribute  = 'SI_UNIT_CNVRSN_ADDITIVE_VALUE' ) ).
      WHEN 'UnitOfMeasureDimensionText'.
        CREATE DATA et_data LIKE ms_config_tables-uom_dimension_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-uom_dimension_t.
        et_header = VALUE #( ( description      = 'Unit of Measure Dimension'
                           internal_name    = 'unitOfMeasureDimensionId'
                           data_type        = 'CHAR(6)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'DIMID'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'TXDIM'
                           vpcoe_attribute  = 'NAME' ) ).
      WHEN 'UnitOfMeasureDimension'.
        CREATE DATA et_data LIKE ms_config_tables-uom_dimension.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_config_tables-uom_dimension.
        et_header = VALUE #( ( description      = 'Unit of Measure Dimension'
                           internal_name    = 'id'
                           data_type        = 'CHAR(6)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'DIMID'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Unit of Measure SI Unit'
                           internal_name    = 'siUnit'
                           data_type        = 'UNIT(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MSSIE'
                           vpcoe_attribute  = 'SI_UNIT') ).
      WHEN 'Incoterms'.
        CREATE DATA et_data LIKE ms_inco_tables-incoterms.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_inco_tables-incoterms.
        et_header = VALUE #( ( description      = 'Incoterms'
                           internal_name    = 'id'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'INCO1'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).
      WHEN 'IncotermsText'.
        CREATE DATA et_data LIKE ms_inco_tables-incoterms_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_inco_tables-incoterms_t.
        et_header = VALUE #( ( description      = 'Incoterms'
                           internal_name    = 'incotermsId'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'INCO1'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                           ( description      = 'ISO Language Code'
                           internal_name    = 'locale'
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'name'
                           data_type        = 'CHAR(30)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'BEZEI'
                           vpcoe_attribute  = 'NAME' ) ).
    ENDCASE.
  ENDMETHOD.


  METHOD get_excel_sheet.

    IF iv_incoterms = abap_true.
      et_sheets = VALUE #( ( 'IncotermsText' )
                           ( 'Incoterms' ) ).
    ELSE.
      et_sheets = VALUE #(  ( 'MovementTypeText' )
                            ( 'MovementType' )
                            ( 'DeliveryDocumentItemCategoryTex' )
                            ( 'DeliveryDocumentItemCategory' )
                            ( 'DeliveryDocumentTypeText' )
                            ( 'DeliveryDocumentType' )
                            ( 'ProductHierarchyText' )
                            ( 'ProductHierarchy' )
                            ( 'ProductGroupText' )
                            ( 'ProductGroup' )
                            ( 'ProductTypeText' )
                            ( 'ProductType' )
                            ( 'RegionText' )
                            ( 'Region' )
                            ( 'CountryText' )
                            ( 'Country' )
                            ( 'CurrencyText' )
                            ( 'Currency' )
                            ( 'UnitOfMeasureISOCodeText' )
                            ( 'UnitOfMeasureISOCode' )
                            ( 'UnitOfMeasureText' )
                            ( 'UnitOfMeasure' )
                            ( 'UnitOfMeasureDimensionText' )
                            ( 'UnitOfMeasureDimension' ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_incoterms.

    DATA: ls_json     TYPE gty_s_json,
          lv_json     TYPE string,
          ls_elements TYPE /vpcoe/s_incoterms,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_incoterms.

    CLEAR: et_incoterms,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-incoterms ).

      SELECT h~inco1 AS id,
             t~bezei AS name,
             t~spras AS language
      INTO TABLE @DATA(lt_data)
        FROM tinc AS h
        JOIN tinct AS t ON h~inco1 = t~inco1.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    ls_json-elements = NEW /vpcoe/tt_incoterms( ).
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE /vpcoe/tt_incoterms_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                                      language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).

      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id    = <lt_data>-id.
      ls_elements-texts = lt_text.

      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING  cs_data = ls_elements ).

      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_incoterms.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        CASE me->mv_mode.
          WHEN /vpcoe/cl_rdp_helper=>sc_mode-screen.
            IF lines( et_incoterms ) >= /vpcoe/cl_rdp_helper=>gc_display_amount.
              RETURN.
            ENDIF.

          WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.
            ls_json = VALUE #( BASE ls_json source = lv_source ).
            ls_jsons-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
            ls_jsons-count    = lines( <lt_elements> ).
            INSERT ls_jsons INTO TABLE et_json.
        ENDCASE.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
      ENDIF.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_inco_tables-incoterms = et_incoterms.
      ms_inco_tables-incoterms_t = VALUE #( FOR <ls_data_t> IN lt_data ( id       = <ls_data_t>-id
                                                                         name     = <ls_data_t>-name
                                                                         language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data_t>-language ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_movement_type.
    DATA: ls_json     TYPE gty_s_json,
          lv_json     TYPE string,
          ls_elements TYPE /vpcoe/s_movement_type,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_movement_type.

    CLEAR: et_movement_type,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-movement_type ).
      SELECT h~bwart AS id,
             t~btext AS name,
             t~spras AS language
      INTO TABLE @DATA(lt_data)
        FROM t156 AS h
        JOIN t156ht AS t ON h~bwart = t~bwart.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      me->mo_log->add_msg_progress( iv_level = CONV #( 'Movement Type' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_movement_type.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_movement_type_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                                      language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).

      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id    = <lt_data>-id.
      ls_elements-texts = lt_text.

      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING  cs_data = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_movement_type.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-movement_type = et_movement_type.
      ms_config_tables-movement_type_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                             name = <ls_data>-name
                                                                             language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_product_group.
    DATA: ls_json     TYPE gty_s_json,
          ls_elements TYPE /vpcoe/s_product_group,
          lv_json     TYPE string,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_product_group.

    CLEAR: et_product_group,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-product_group ).
      SELECT h~matkl AS id,
             t~wgbez AS name,
             t~wgbez60 AS description,"text
             t~spras AS language
      INTO TABLE @DATA(lt_data)
        FROM t023 AS h
        JOIN t023t AS t ON h~matkl = t~matkl.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Product Group' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_product_group.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_product_group_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                                      language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language )
                                                                                      description = <ls_data>-description ) ).

      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id    = <lt_data>-id.
      ls_elements-texts = lt_text.
      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data  = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_product_group.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-product_group = et_product_group.
      ms_config_tables-product_group_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                             name = <ls_data>-name
                                                                             language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language )
                                                                             description = <ls_data>-description ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_product_hierarchy.
    DATA: ls_json     TYPE gty_s_json,
          ls_elements TYPE /vpcoe/s_product_hierarchy,
          lv_json     TYPE string,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_product_hierarchy.

    CLEAR: et_product_hierarchy,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-product_hierarchy ).

      SELECT h~prodh AS id,
             h~stufe AS level,
             t~vtext AS name,
             l~laiso AS language
      INTO TABLE @DATA(lt_data)
        FROM t179 AS h
        JOIN t179t AS t ON h~prodh = t~prodh
        LEFT JOIN t002 AS l ON t~spras = l~spras.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Product Hierarchy' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE /vpcoe/tt_product_hierarchy.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE /vpcoe/tt_product_hierarchy_t( FOR <ls_data> IN GROUP <lt_data> ( name     = <ls_data>-name
                                                                                              language = <ls_data>-language ) ).

      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id    = <lt_data>-id.
      ls_elements-level = <lt_data>-level.
      ls_elements-texts = lt_text.

      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data  = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_product_hierarchy.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-product_hierarchy = et_product_hierarchy.
      ms_config_tables-product_hierarchy_t = CORRESPONDING #( lt_data ).
    ENDIF.

  ENDMETHOD.


  METHOD get_product_type.
    DATA: ls_json     TYPE gty_s_json,
          ls_elements TYPE /vpcoe/s_product_type,
          lv_json     TYPE string,
          ls_jsons    LIKE LINE OF et_json.
    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_product_type.

    CLEAR: et_product_type,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-product_type ).
      SELECT h~mtart AS id,
             t~mtbez AS name,
             t~spras AS language
      INTO TABLE @DATA(lt_data)
        FROM t134 AS h
        JOIN t134t AS t ON h~mtart = t~mtart .

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Product Type' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_product_type.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_product_type_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                                     language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id    = <lt_data>-id.
      ls_elements-texts = lt_text.
      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data  = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_product_type.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-product_type = et_product_type.
      ms_config_tables-product_type_t = VALUE #( FOR <ls_data> IN lt_data ( id       = <ls_data>-id
                                                                            name     = <ls_data>-name
                                                                            language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD get_region.
    DATA: ls_json     TYPE gty_s_json,
          lv_json     TYPE string,
          ls_elements TYPE /vpcoe/s_region,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_region.

    CLEAR: et_region,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-region ).
      SELECT h~bland AS id,
             h~land1 AS country,
             t~bezei AS name,
             t~spras AS language
      FROM t005s AS h
        JOIN t005u AS t ON h~bland = t~bland AND h~land1 = t~land1
        INTO TABLE @DATA(lt_data).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Region' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_region.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id      = <lt_data>-id
                                                                 country = <lt_data>-country ).

      DATA(lt_text) = VALUE gty_t_region_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                               language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).

      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING ct_data  = lt_text ).

      ls_elements-id      = <lt_data>-id.
      ls_elements-country = <lt_data>-country.
      ls_elements-texts   = lt_text.
      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING  cs_data = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_region.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-region = et_region.
      ms_config_tables-region_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                      name = <ls_data>-name
                                                                      language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language )
                                                                      country = <ls_data>-country ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_uom.
    DATA: ls_json          TYPE gty_s_json,
          lv_json          TYPE string,
          ls_elements      TYPE /vpcoe/s_uom,
          lt_name_mappings TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
          ls_jsons         LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_uom.

    CLEAR: et_uom,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    INSERT VALUE #( abap = 'si_unit_cnvrsn_rate_numr' json = 'siUnitCnvrsnRateNumerator' ) INTO TABLE lt_name_mappings.
    INSERT VALUE #( abap = 'si_unit_cnvrsn_rate_denomr' json = 'siUnitCnvrsnRateDenominator' ) INTO TABLE lt_name_mappings.

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-uom ).
      SELECT DISTINCT h~msehi AS id,
             h~isocode AS iso_code,
             h~dimid AS dimension,
             h~zaehl AS si_unit_cnvrsn_rate_numr,
             h~nennr AS si_unit_cnvrsn_rate_denomr,
             h~exp10 AS si_unit_cnvrsn_rate_exponent,
             h~addko AS si_unit_cnvrsn_additive_value,
             t~mseht AS name,
             t~mseh6 AS technical_name,
             t~mseh3 AS commercial_name,
             t~spras AS language,
             t~msehl AS long_name

      INTO TABLE @DATA(lt_data)
        FROM t006 AS h JOIN t006a AS t ON h~msehi = t~msehi.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Unit of Measure' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_uom.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) WHERE id IS NOT INITIAL
      GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_uom_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                            language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language )
                                                                            technical_name = <ls_data>-technical_name
                                                                            commercial_name = <ls_data>-commercial_name
                                                                            long_name       = <ls_data>-long_name ) ).

      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id                             = <lt_data>-id.
      ls_elements-iso_code                       = <lt_data>-iso_code.
      ls_elements-dimension                      = <lt_data>-dimension.
      ls_elements-si_unit_cnvrsn_rate_numr       = <lt_data>-si_unit_cnvrsn_rate_numr.
      ls_elements-si_unit_cnvrsn_rate_denomr     = <lt_data>-si_unit_cnvrsn_rate_denomr.
      ls_elements-si_unit_cnvrsn_rate_exponent   = <lt_data>-si_unit_cnvrsn_rate_exponent.
      ls_elements-si_unit_cnvrsn_additive_value  = <lt_data>-si_unit_cnvrsn_additive_value.
      ls_elements-texts                          = lt_text.

      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING  cs_data = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_uom.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data          = ls_json
                                                                  it_name_mappings = lt_name_mappings
                                                                  iv_pretty_name   = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data          = ls_json
                                                                it_name_mappings = lt_name_mappings
                                                                iv_pretty_name   = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null'.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-uom = et_uom.
      ms_config_tables-uom_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                   technical_name = <ls_data>-technical_name
                                                                   commercial_name = <ls_data>-commercial_name
                                                                   long_name = <ls_data>-long_name
                                                                   name = <ls_data>-name
                                                                   language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_uom_dimension.
    DATA: ls_json          TYPE gty_s_json,
          ls_elements      TYPE /vpcoe/s_uom_dimension,
          ls_jsons         LIKE LINE OF et_json,
          lt_name_mappings TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
          lv_json          TYPE string.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_uom_dimension.

    CLEAR: et_uom_d,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-uom_dimension ).
      SELECT h~dimid     AS id,
             h~mssie     AS si_unit,
             t~spras     AS language,
             t~txdim     AS name
        INTO TABLE @DATA(lt_data)
          FROM t006d AS h JOIN t006t AS t ON h~dimid = t~dimid.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Unit of Measure Dimensions' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_uom_dimension.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_uom_dimension_t( FOR <ls_data> IN GROUP <lt_data>
                                                     ( name     = <ls_data>-name
                                                       language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language )  ) ).
      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ) .
      ls_elements-id                             = <lt_data>-id.
      ls_elements-si_unit                        = <lt_data>-si_unit.
      ls_elements-texts                          = lt_text.
      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data  = ls_elements ) .

      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_uom_d.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data          = ls_json
                                                                  it_name_mappings = lt_name_mappings
                                                                  iv_pretty_name   = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data          = ls_json
                                                                it_name_mappings = lt_name_mappings
                                                                iv_pretty_name   = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-uom_dimension = et_uom_d.
      ms_config_tables-uom_dimension_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                             name = <ls_data>-name
                                                                             language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_uom_iso_code.
    DATA: ls_json     TYPE gty_s_json,
          ls_elements TYPE /vpcoe/s_uom_iso_code,
          lv_json     TYPE string,
          ls_jsons    LIKE LINE OF et_json.
    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_uom_iso_code.

    CLEAR: et_iso_uom,
           et_json.

    DATA(lv_source)       = me->mo_rdp_helper->get_source_id( ).
    DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

    IF NOT me->skip_selection( /vpcoe/cl_rdp_helper=>sc_service_id-uom_code ).
      SELECT h~isocode AS id,
             t~isotxt AS name,
             t~langu AS language
      INTO TABLE @DATA(lt_data)
        FROM t006i AS h JOIN t006j AS t ON h~isocode = t~isocode.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = CONV #( 'Unit of Measure ISO Code' ) ).
    ENDIF.

    me->adjust_data_retrieval( CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_uom_iso_code.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_uom_iso_code_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                                     language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id = <lt_data>-id.
      ls_elements-texts = lt_text.
      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data  = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_iso_uom.
      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        ls_json = VALUE #( BASE ls_json source = lv_source ).
        lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                  iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

        ls_jsons-elements = lv_json.
        ls_jsons-count = lines( <lt_elements> ).
        INSERT ls_jsons INTO TABLE et_json.
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      ls_json = VALUE #( BASE ls_json source = lv_source ).
      lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                                iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

      ls_jsons-elements = lv_json.
      ls_jsons-count = lines( <lt_elements> ).
      INSERT ls_jsons INTO TABLE et_json.
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_config_tables-uom_iso_code = et_iso_uom.
      ms_config_tables-uom_iso_code_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                            name = <ls_data>-name
                                                                            language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD send.
    CLEAR: ev_status,
           ev_reason.

    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-configuration
                                          iv_extnumber  = CONV #( iv_srv_id ) ).

    NEW /vpcoe/cl_rdp_payload_handler( )->send_payload(
      EXPORTING
        io_cust     = io_cust
        it_json     = it_json
        io_log      = lo_log
        iv_save_log = abap_true
      IMPORTING
        ev_status   = ev_status
        ev_reason   = ev_reason ).

    lo_log->display_message( ).

  ENDMETHOD.


  METHOD skip_selection.

    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    me->mv_srv_id = iv_srv_id.

    GET BADI lo_badi.

    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
        iv_srv_id   = me->mv_srv_id
        iv_api_type = me->mv_api_type
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = rv_skip.

  ENDMETHOD.
ENDCLASS.
