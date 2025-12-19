class /VPCOE/CL_RDP_ORG_DATA_OBJ definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_json,
        source   TYPE string,
        elements TYPE REF TO data,
      END OF gty_s_json .
  types:
    gty_t_sales_org_t           TYPE STANDARD TABLE OF /vpcoe/s_sales_org_t WITH NON-UNIQUE KEY language .
  types:
    BEGIN OF gty_s_sales_org_t,
        id TYPE vkorg.
    INCLUDE TYPE /vpcoe/s_sales_org_t AS text.
    TYPES:
    END OF gty_s_sales_org_t .
  types:
    BEGIN OF gty_s_division_org_t,
        id TYPE spart.
    INCLUDE TYPE /vpcoe/s_division_t AS text.
    TYPES:
    END OF gty_s_division_org_t .
  types:
    BEGIN OF gty_s_distr_channel_org_t,
        id TYPE vtweg.
    INCLUDE TYPE /vpcoe/s_distr_channel_t AS text.
    TYPES:
    END OF gty_s_distr_channel_org_t .
  types GTY_S_VAL_AREA_COMPANY_CODE type /VPCOE/S_VAL_AREA_COMPANY_CODE .
  types:
    gty_t_val_area_company_code TYPE  STANDARD TABLE OF /vpcoe/s_val_area_company_code .
  types GTY_S_PLANT_ADDR type /VPCOE/S_PLANT_ADDR .
  types GTY_S_PLANT type /VPCOE/S_PLANT .
  types:
    gty_t_distr_channel_t       TYPE STANDARD TABLE OF /vpcoe/s_distr_channel_t WITH NON-UNIQUE KEY language .
  types:
    gty_t_division_t            TYPE STANDARD TABLE OF /vpcoe/s_division_t WITH NON-UNIQUE KEY language .
  types:
    gty_t_company_code          TYPE  STANDARD TABLE OF /vpcoe/s_company_code WITH NON-UNIQUE KEY id .
  types:
    gty_t_sales_org             TYPE  STANDARD TABLE OF /vpcoe/s_sales_org WITH NON-UNIQUE KEY id .
  types:
    gty_t_plant                 TYPE  STANDARD TABLE OF gty_s_plant WITH NON-UNIQUE KEY id .
  types:
    gty_t_distr_channel         TYPE  STANDARD TABLE OF /vpcoe/s_distr_channel WITH NON-UNIQUE KEY id .
  types:
    gty_t_division              TYPE  STANDARD TABLE OF /vpcoe/s_division WITH NON-UNIQUE KEY id .
  types:
    gty_t_sales_area             TYPE  STANDARD TABLE OF /vpcoe/s_sales_area WITH NON-UNIQUE KEY division .
  types:
    BEGIN OF gty_s_org_tables,
        company_code         TYPE gty_t_company_code,
        plant                TYPE gty_t_plant,
        sales_organization   TYPE gty_t_sales_org,
        sales_organization_t TYPE STANDARD TABLE OF gty_s_sales_org_t WITH NON-UNIQUE KEY id,
        distr_channel        TYPE gty_t_distr_channel,
        distr_channel_t      TYPE STANDARD TABLE OF gty_s_distr_channel_org_t WITH NON-UNIQUE KEY id,
        division             TYPE gty_t_division,
        division_t           TYPE STANDARD TABLE OF gty_s_division_org_t WITH NON-UNIQUE KEY id,
        sales_area           TYPE gty_t_sales_area,
      END OF gty_s_org_tables .

  methods CONSTRUCTOR
    importing
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_RDP_HELPER=>SC_MODE-SEND
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional .
  methods DOWNLOAD_EXCEL
    importing
      !IV_FILE_PATH type STRING
      !IV_SAVE_BACKGROUND type ABAP_BOOL
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
  methods GET_COMPANY_CODE
    importing
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
    exporting
      !ET_COMPANY_CODE type GTY_T_COMPANY_CODE
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_DISTR_CHANNEL
    importing
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
    exporting
      !ET_DISTR_CHANNEL type GTY_T_DISTR_CHANNEL
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_DIVISION
    importing
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
    exporting
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !ET_DIVISION type GTY_T_DIVISION .
  methods GET_PLANT
    importing
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
    exporting
      !ET_PLANT type GTY_T_PLANT
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !ET_BAPIRET2 type BAPIRET2_T .
  methods GET_SALES_AREA
    importing
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
    exporting
      !ET_SALES_AREA type GTY_T_SALES_AREA
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_SALES_ORG
    importing
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
    exporting
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !ET_SALES_ORG type GTY_T_SALES_ORG .
  methods SEND
    importing
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
      !IT_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON
      !IT_BAPIRET2 type BAPIRET2_T optional
    exporting
      !EV_STATUS type I
      !EV_REASON type STRING
    raising
      CX_UUID_ERROR
      CX_OA2C .
protected section.

  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_SRV_ID type /VPCOE/DE_SERVICE_ID .
  data MS_ORG_TABLES type GTY_S_ORG_TABLES .
  data MV_MODE type /VPCOE/DE_MODE value 1 ##NO_TEXT.
  data MO_LOG type ref to /VPCOE/CL_RDP_LOG .

  methods CREATE_JSON
    importing
      !IS_JSON type GTY_S_JSON
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
      !IV_LINES type INT4
    changing
      !CT_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods ADJUST_DATA_RETRIEVAL
    importing
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
    changing
      !CT_DATA type DATA .
  methods ADJUST_JSON
    importing
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
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
      !CT_DATA type STANDARD TABLE .
  methods GET_EXCEL_HEADER
    importing
      !IV_SHEET_NAME type CHAR40
    exporting
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      value(ET_DATA) type ref to DATA .
  methods GET_EXCEL_SHEET
    exporting
      !ET_SHEETS type /VPCOE/TT_SHEETS_STR .
  methods SKIP_SELECTION
    importing
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
    returning
      value(RV_SKIP) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS /VPCOE/CL_RDP_ORG_DATA_OBJ IMPLEMENTATION.


  METHOD adjust_data_retrieval.

    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    GET BADI lo_badi.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-configuration
        iv_srv_id   = iv_srv_id
        iv_api_type = iv_api_type
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
        iv_srv_id   = iv_srv_id
        iv_api_type = iv_api_type
        io_log      = me->mo_log
      CHANGING
        ct_json     = ct_json.

  ENDMETHOD.


  METHOD adjust_mapping.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    GET BADI lo_badi.

    CALL BADI lo_badi->adjust_mapping
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
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
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-organization
        iv_srv_id   = me->mv_srv_id
        iv_api_type = me->mv_api_type
        it_data     = it_data
        io_log      = me->mo_log
      CHANGING
        ct_data     = ct_data.
  ENDMETHOD.


  METHOD constructor.
    me->mv_api_type     = iv_api_type.
    me->mv_mode         = iv_mode.
    me->mo_log          = io_log.
  ENDMETHOD.


  METHOD create_json.
    DATA: ls_json TYPE gty_s_json,
          lv_json TYPE string.

    ls_json = VALUE #( BASE is_json source = io_cust->get_source_id( ) ).
    lv_json = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data        = ls_json
                                                              iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

    INSERT VALUE #( elements = lv_json
                    count    = iv_lines ) INTO TABLE ct_json.

  ENDMETHOD.


  METHOD download_excel.

    DATA: lr_table  TYPE REF TO data.
    FIELD-SYMBOLS: <lt_data> TYPE any.

    get_excel_sheet(
      IMPORTING
        et_sheets     = DATA(lt_sheets) ).

    DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path = iv_file_path
                                                   iv_save_background = iv_save_background   ).

    LOOP AT lt_sheets ASSIGNING FIELD-SYMBOL(<ls_sheets>).

      get_excel_header(
              EXPORTING
                iv_sheet_name     = <ls_sheets>
              IMPORTING
                et_header         = DATA(lt_header)
                et_data           = lr_table ).

      ASSIGN lr_table->* TO <lt_data>.

      lo_xls_file->execute( iv_name = CONV #( <ls_sheets> ) ).
      lo_xls_file->fill_data( it_item_tab = <lt_data>
                              it_title    = lt_header ).

    ENDLOOP.
    lo_xls_file->save_xls_file( io_log = io_log ).

  ENDMETHOD.


  METHOD get_company_code.
    DATA: ls_json  TYPE gty_s_json,
          lv_json  TYPE string,
          ls_jsons LIKE LINE OF et_json.
    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_company_code.

    CLEAR: et_company_code,
           et_json.

    DATA(lv_package_size) = io_cust->get_package_size( ).

    IF NOT me->skip_selection( iv_api_type = io_cust->get_api_type( )
                               iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-company_code ).
      SELECT bukrs AS id,
             butxt AS name,
             land1 AS country,
             waers AS currency,
             spras AS language
         INTO CORRESPONDING FIELDS OF TABLE @et_company_code
            FROM t001.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      me->mo_log->add_msg_progress( iv_level = 'Company Code' ).
    ENDIF.

    me->adjust_data_retrieval( EXPORTING iv_api_type = io_cust->get_api_type( )
                                         iv_srv_id   = io_cust->get_srv_id( )
                               CHANGING ct_data = et_company_code ).

    CASE mv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.

        CREATE DATA ls_json-elements TYPE gty_t_company_code.
        ASSIGN ls_json-elements->* TO <lt_elements>.

        LOOP AT et_company_code ASSIGNING FIELD-SYMBOL(<ls_data>).
          <ls_data>-language = /vpcoe/cl_rdp_helper=>convert_langu_code( CONV #( <ls_data>-language ) ).
          INSERT <ls_data> INTO TABLE <lt_elements>.

          IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
            me->create_json( EXPORTING is_json = ls_json
                                       io_cust = io_cust
                                       iv_lines = lines( <lt_elements> )
                             CHANGING ct_json = et_json ).

            CLEAR <lt_elements>.
          ENDIF.
        ENDLOOP.

        IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
          me->create_json( EXPORTING is_json = ls_json
                                     io_cust = io_cust
                                     iv_lines = lines( <lt_elements> )
                           CHANGING ct_json = et_json ).
        ENDIF.

        LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
        ENDLOOP.

        me->adjust_json( EXPORTING iv_api_type = io_cust->get_api_type( )
                                   iv_srv_id   = io_cust->get_srv_id( )
                         CHANGING ct_json = et_json ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
        ms_org_tables-company_code = VALUE #( FOR <ls_comp_cod> IN et_company_code ( id = <ls_comp_cod>-id
                                                                                     name = <ls_comp_cod>-name
                                                                                     language = /vpcoe/cl_rdp_helper=>convert_langu_code( CONV #( <ls_comp_cod>-language ) )
                                                                                     country = <ls_comp_cod>-country
                                                                                     currency = <ls_comp_cod>-currency ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_distr_channel.
    DATA: ls_json     TYPE gty_s_json,
          lv_json     TYPE string,
          ls_elements TYPE /vpcoe/s_distr_channel,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_distr_channel.

    CLEAR: et_distr_channel,
           et_json.

    DATA(lv_package_size) = io_cust->get_package_size( ).

    IF NOT me->skip_selection( iv_api_type = io_cust->get_api_type( )
                               iv_srv_id = /vpcoe/cl_rdp_helper=>sc_service_id-distribution_channel ).
      SELECT h~vtweg AS id,
             t~vtext AS name,
             t~spras AS language
        FROM tvtw AS h JOIN tvtwt AS t ON h~vtweg = t~vtweg
        INTO TABLE @DATA(lt_data).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = 'Distr.Channel' ).
    ENDIF.

    me->adjust_data_retrieval( EXPORTING iv_api_type = io_cust->get_api_type( )
                                         iv_srv_id   = io_cust->get_srv_id( )
                               CHANGING ct_data = lt_data ).
    CREATE DATA ls_json-elements TYPE gty_t_distr_channel.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_distr_channel_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                                      language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id    = <lt_data>-id.
      ls_elements-texts = lt_text.
      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data  = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_distr_channel.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        me->create_json( EXPORTING is_json = ls_json
                                   io_cust = io_cust
                                   iv_lines = lines( <lt_elements> )
                         CHANGING ct_json = et_json ).

        CLEAR <lt_elements>.
      ENDIF.

    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      me->create_json( EXPORTING is_json = ls_json
                                 io_cust = io_cust
                                 iv_lines = lines( <lt_elements> )
                       CHANGING ct_json = et_json ).
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( EXPORTING iv_api_type = io_cust->get_api_type( )
                               iv_srv_id   = io_cust->get_srv_id( )
                     CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_org_tables-distr_channel = et_distr_channel.
      ms_org_tables-distr_channel_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                          name = <ls_data>-name
                                                                          language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_division.
    DATA: ls_json     TYPE gty_s_json,
          ls_elements TYPE /vpcoe/s_division,
          lv_json     TYPE string,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_division.

    CLEAR: et_division,
           et_json.

    DATA(lv_package_size) = io_cust->get_package_size( ).

    IF NOT me->skip_selection( iv_api_type = io_cust->get_api_type( )
                                iv_srv_id = /vpcoe/cl_rdp_helper=>sc_service_id-division ).
      SELECT h~spart AS id,
             t~vtext AS name,
             t~spras AS language
        FROM tspa AS h JOIN tspat AS t ON h~spart = t~spart
        INTO TABLE @DATA(lt_data).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = 'Division' ).
    ENDIF.

    me->adjust_data_retrieval( EXPORTING iv_api_type = io_cust->get_api_type( )
                                         iv_srv_id   = io_cust->get_srv_id( )
                               CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_division.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_division_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                                 language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id       = <lt_data>-id.
      ls_elements-texts     = lt_text.
      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data = ls_elements ).

      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_division.

      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        me->create_json( EXPORTING is_json = ls_json
                                   io_cust = io_cust
                                   iv_lines = lines( <lt_elements> )
                         CHANGING ct_json = et_json ).

        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      me->create_json( EXPORTING is_json = ls_json
                                 io_cust = io_cust
                                 iv_lines = lines( <lt_elements> )
                       CHANGING ct_json = et_json ).
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json(  EXPORTING iv_api_type = io_cust->get_api_type( )
                                iv_srv_id   = io_cust->get_srv_id( )
                      CHANGING ct_json = et_json ).

    IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_org_tables-division = et_division.
      ms_org_tables-division_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                     name = <ls_data>-name
                                                                     language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_excel_header.

    FIELD-SYMBOLS <lt_data> TYPE any.

    CLEAR et_header.

    CASE iv_sheet_name.
      WHEN 'SalesArea'.
        CREATE DATA et_data LIKE ms_org_tables-sales_area.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_org_tables-sales_area.
        et_header = VALUE #(
                         ( description      = 'Sales Organization'
                           internal_name    = 'salesOrganization'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'VKORG'
                           vpcoe_attribute  = 'SALES_ORGANIZATION' )
                         ( description      = 'Distribution Channel'
                           internal_name    = 'distributionChannel'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'VTWEG'
                           vpcoe_attribute  = 'DISTRIBUTION_CHANNEL' )
                         ( description      = 'Division'
                           internal_name    = 'division'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPART'
                           vpcoe_attribute  = 'DIVISION') ).

      WHEN 'DivisionText'.
        CREATE DATA et_data LIKE ms_org_tables-division_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_org_tables-division_t.
        et_header = VALUE #(
                         ( description      = 'Division'
                           internal_name    = 'divisionId'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPART'
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

      WHEN 'Division'.
        CREATE DATA et_data LIKE ms_org_tables-division.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_org_tables-division.
        et_header = VALUE #(
                         ( description      = 'Division'
                           internal_name    = 'id'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPART'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).

      WHEN 'DistributionChannelText'.
        CREATE DATA et_data LIKE ms_org_tables-distr_channel_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_org_tables-distr_channel_t.
        et_header = VALUE #(
                         ( description      = 'Distribution Channel'
                           internal_name    = 'distributionChannelId'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'VTWEG'
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

      WHEN 'DistributionChannel'.
        CREATE DATA et_data LIKE ms_org_tables-distr_channel.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_org_tables-distr_channel.
        et_header = VALUE #(
                         ( description      = 'Distribution Channel'
                           internal_name    = 'id'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'VTWEG'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true ) ).

      WHEN 'SalesOrganizationText'.
        CREATE DATA et_data LIKE ms_org_tables-sales_organization_t.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_org_tables-sales_organization_t.
        et_header = VALUE #(
                          ( description      = 'Sales Organization'
                            internal_name    = 'salesOrganizationId'
                            data_type        = 'CHAR(4)'
                            mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                            s4hana_attribute = 'VKORG'
                            vpcoe_attribute  = 'ID'
                            is_key           = abap_true )
                          ( description      = 'Language'
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

      WHEN 'SalesOrganization'.
        CREATE DATA et_data LIKE ms_org_tables-sales_organization.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_org_tables-sales_organization.
        et_header = VALUE #(
                         ( description      = 'Sales Organization'
                           internal_name    = 'id'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'VKORG'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Company Code'
                           internal_name    = 'company'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'BUKRS'
                           vpcoe_attribute  = 'COMPANY' )
                         ( description      = 'Currency'
                           internal_name    = 'currency'
                           data_type        = 'CUKY(5)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'WAERS'
                           vpcoe_attribute  = 'CURRENCY' ) ).

      WHEN 'Plant'.
        CREATE DATA et_data LIKE ms_org_tables-plant.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_org_tables-plant.
        et_header = VALUE #(
                         ( description      = 'Plant'
                           internal_name    = 'id'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'WERKS'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'languageIsoCode '
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE' )
                         ( description      = 'Plant Name'
                           internal_name    = 'name'
                           data_type        = 'CHAR(30)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'NAME1'
                           vpcoe_attribute  = 'NAME' )
                         ( description      = 'Company Code'
                           internal_name    = 'companyCode'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'BUKRS'
                           vpcoe_attribute  = 'COMPANY_CODE' )
                         ( description      = 'Country/Region'
                           internal_name    = 'country'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'LAND1'
                           vpcoe_attribute  = 'PLANT_ADDRESS-COUNTRY' )
                         ( description      = 'State/Province'
                           internal_name    = 'region'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'REGIO'
                           vpcoe_attribute  = 'PLANT_ADDRESS-REGION' ) ).

      WHEN 'CompanyCode'.
        CREATE DATA et_data LIKE ms_org_tables-company_code.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = ms_org_tables-company_code.
        et_header = VALUE #(
                         ( description      = 'Company Code'
                           internal_name    = 'id'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'BUKRS'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'languageIsoCode '
                           data_type        = 'LANG(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE' )
                         ( description      = 'Company Code Name'
                           internal_name    = 'name'
                           data_type        = 'CHAR(25)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'BUTXT'
                           vpcoe_attribute  = 'NAME' )
                         ( description      = 'Country/Region'
                           internal_name    = 'country'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'LAND1'
                           vpcoe_attribute  = 'COUNTRY' )
                         ( description      = 'Currency'
                           internal_name    = 'currency'
                           data_type        = 'CUKY(5)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'WAERS'
                           vpcoe_attribute  = 'CURRENCY' ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_excel_sheet.
    et_sheets = VALUE #(  ( 'SalesArea' )
                          ( 'DivisionText' )
                          ( 'Division' )
                          ( 'DistributionChannelText' )
                          ( 'DistributionChannel' )
                          ( 'SalesOrganizationText' )
                          ( 'SalesOrganization' )
                          ( 'Plant' )
                          ( 'CompanyCode' ) ).
  ENDMETHOD.


  METHOD get_plant.
    DATA: ls_json  TYPE gty_s_json,
          lv_json  TYPE string,
          ls_jsons LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_plant.

    CLEAR: et_plant, et_bapiret2, et_json.

    DATA(lv_package_size) = io_cust->get_package_size( ).

    IF NOT me->skip_selection( iv_api_type = io_cust->get_api_type( )
                               iv_srv_id = /vpcoe/cl_rdp_helper=>sc_service_id-plant ).
      SELECT t001w~werks AS id,
             t001w~name1 AS name,
             t001w~spras AS language,
             t001w~adrnr AS plant_address-id,
             t001w~land1 AS plant_address-country,
             t001w~regio AS plant_address-region,
             t001k~bukrs AS company_code
        FROM t001w INNER JOIN t001k ON t001w~werks = t001k~bwkey
      INTO CORRESPONDING FIELDS OF TABLE @et_plant.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = 'Plant' ).
    ENDIF.

    me->adjust_data_retrieval( EXPORTING iv_api_type = io_cust->get_api_type( )
                                         iv_srv_id   = io_cust->get_srv_id( )
                               CHANGING ct_data = et_plant ).

    CASE mv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.

        CREATE DATA ls_json-elements TYPE gty_t_plant.
        ASSIGN ls_json-elements->* TO <lt_elements>.

        LOOP AT et_plant ASSIGNING FIELD-SYMBOL(<ls_data>).
          <ls_data>-language = /vpcoe/cl_rdp_helper=>convert_langu_code( CONV #( <ls_data>-language ) ).
          INSERT <ls_data> INTO TABLE <lt_elements>.

          IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
            me->create_json( EXPORTING is_json = ls_json
                                       io_cust = io_cust
                                       iv_lines = lines( <lt_elements> )
                             CHANGING ct_json = et_json ).
            CLEAR <lt_elements>.
          ENDIF.
        ENDLOOP.

        IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
          me->create_json( EXPORTING is_json = ls_json
                                     io_cust = io_cust
                                     iv_lines = lines( <lt_elements> )
                           CHANGING ct_json = et_json ).
          CLEAR <lt_elements>.
        ENDIF.

        LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
        ENDLOOP.

        me->adjust_json( EXPORTING iv_api_type = io_cust->get_api_type( )
                                   iv_srv_id   = io_cust->get_srv_id( )
                         CHANGING ct_json = et_json ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
        ms_org_tables-plant = VALUE #( FOR <ls_plant> IN et_plant ( id            = <ls_plant>-id
                                                                    name          = <ls_plant>-name
                                                                    company_code  = <ls_plant>-company_code
                                                                    language      = /vpcoe/cl_rdp_helper=>convert_langu_code( CONV #( <ls_plant>-language ) )
                                                                    plant_address = <ls_plant>-plant_address ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_sales_area.

    DATA: ls_json  TYPE gty_s_json,
          lv_json  TYPE string,
          ls_jsons LIKE LINE OF et_json.
    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_sales_area.

    CLEAR: et_sales_area,
           et_json.

    DATA(lv_package_size) = io_cust->get_package_size( ).

    IF NOT me->skip_selection( iv_api_type = io_cust->get_api_type( )
                               iv_srv_id = /vpcoe/cl_rdp_helper=>sc_service_id-sales_area ).
      SELECT vkorg AS sales_organization,
           vtweg AS distribution_channel,
           spart AS division
     INTO CORRESPONDING FIELDS OF TABLE @et_sales_area
        FROM tvta.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = 'Sales Area' ).
    ENDIF.

    me->adjust_data_retrieval( EXPORTING iv_api_type = io_cust->get_api_type( )
                                         iv_srv_id   = io_cust->get_srv_id( )
                               CHANGING ct_data = et_sales_area ).

    CASE mv_mode.
      WHEN /vpcoe/cl_rdp_helper=>sc_mode-send.

        CREATE DATA ls_json-elements TYPE gty_t_sales_area.
        ASSIGN ls_json-elements->* TO <lt_elements>.

        LOOP AT et_sales_area ASSIGNING FIELD-SYMBOL(<ls_data>).
          INSERT <ls_data> INTO TABLE <lt_elements>.

          IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
            me->create_json( EXPORTING is_json = ls_json
                                       io_cust = io_cust
                                       iv_lines = lines( <lt_elements> )
                             CHANGING ct_json = et_json ).
            CLEAR <lt_elements>.
          ENDIF.
        ENDLOOP.

        IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
          me->create_json( EXPORTING is_json = ls_json
                                     io_cust = io_cust
                                     iv_lines = lines( <lt_elements> )
                           CHANGING ct_json = et_json ).
        ENDIF.

        LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
        ENDLOOP.

        me->adjust_json( EXPORTING iv_api_type = io_cust->get_api_type( )
                                   iv_srv_id   = io_cust->get_srv_id( )
                         CHANGING ct_json = et_json ).

      WHEN /vpcoe/cl_rdp_helper=>sc_mode-document.
        ms_org_tables-sales_area = et_sales_area.

    ENDCASE.

  ENDMETHOD.


  METHOD get_sales_org.
    DATA: ls_json     TYPE gty_s_json,
          ls_elements TYPE /vpcoe/s_sales_org,
          lv_json     TYPE string,
          ls_jsons    LIKE LINE OF et_json.

    FIELD-SYMBOLS: <lt_elements> TYPE gty_t_sales_org.

    CLEAR: et_sales_org,
           et_json.

    DATA(lv_package_size) = io_cust->get_package_size( ).

    IF NOT me->skip_selection( iv_api_type = io_cust->get_api_type( )
                               iv_srv_id = /vpcoe/cl_rdp_helper=>sc_service_id-sales_organization ).
      SELECT h~vkorg AS id,
             h~waers AS currency,
             h~bukrs AS company_code,
             t~vtext AS name,
             t~spras AS language
        FROM tvko AS h
        JOIN tvkot AS t ON h~vkorg = t~vkorg
        INTO TABLE @DATA(lt_data).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      me->mo_log->add_msg_progress( iv_level = 'Sales Org.' ).
    ENDIF.
    me->adjust_data_retrieval( EXPORTING iv_api_type = io_cust->get_api_type( )
                                         iv_srv_id   = io_cust->get_srv_id( )
                               CHANGING ct_data = lt_data ).

    CREATE DATA ls_json-elements TYPE gty_t_sales_org.
    ASSIGN ls_json-elements->* TO <lt_elements>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) GROUP BY ( id = <lt_data>-id ).
      DATA(lt_text) = VALUE gty_t_sales_org_t( FOR <ls_data> IN GROUP <lt_data> ( name = <ls_data>-name
                                                                                  language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).

      me->adjust_text_mapping( EXPORTING it_data = <lt_data>
                               CHANGING  ct_data = lt_text ).

      ls_elements-id           = <lt_data>-id.
      ls_elements-currency     = <lt_data>-currency.
      ls_elements-company_code = <lt_data>-company_code.
      ls_elements-texts        = lt_text.
      me->adjust_mapping( EXPORTING is_data = <lt_data>
                          CHANGING cs_data  = ls_elements ).
      INSERT ls_elements INTO TABLE <lt_elements>.
      INSERT ls_elements INTO TABLE et_sales_org.
      IF lines( <lt_elements> ) = lv_package_size AND lv_package_size IS NOT INITIAL.
        me->create_json( EXPORTING is_json = ls_json
                                   io_cust = io_cust
                                   iv_lines = lines( <lt_elements> )
                         CHANGING ct_json = et_json ).
        CLEAR <lt_elements>.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0 AND <lt_elements> IS NOT INITIAL.
      me->create_json( EXPORTING is_json = ls_json
                                 io_cust = io_cust
                                 iv_lines = lines( <lt_elements> )
                       CHANGING ct_json = et_json ).
      CLEAR <lt_elements>.
    ENDIF.

    LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
    ENDLOOP.

    me->adjust_json( EXPORTING iv_api_type = io_cust->get_api_type( )
                               iv_srv_id   = io_cust->get_srv_id( )
                     CHANGING ct_json = et_json ).

    IF mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      ms_org_tables-sales_organization = et_sales_org.
      ms_org_tables-sales_organization_t = VALUE #( FOR <ls_data> IN lt_data ( id = <ls_data>-id
                                                                               name = <ls_data>-name
                                                                               language = /vpcoe/cl_rdp_helper=>convert_langu_code( <ls_data>-language ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD send.
    CLEAR: ev_status,
           ev_reason.

    DATA(lo_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-organization
                                          iv_extnumber  = CONV #( io_cust->get_srv_id( ) ) ).

    lo_log->add_bapiret( EXPORTING it_bapiret2_t = it_bapiret2 ).

    NEW /vpcoe/cl_rdp_payload_handler( )->send_payload(
      EXPORTING
        io_cust     = io_cust
        it_json     = it_json
        io_log      = lo_log
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
        iv_srv_id   = iv_srv_id
        iv_api_type = iv_api_type
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = rv_skip.

  ENDMETHOD.
ENDCLASS.
