class /VPCOE/CL_COMMON_HELPER definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_name_mapping,
        abap TYPE abap_compname,
        json TYPE string,
      END OF gty_s_name_mapping .
  types:
    gty_t_name_mappings    TYPE HASHED TABLE OF gty_s_name_mapping WITH UNIQUE KEY abap .
  types:
    BEGIN OF gty_s_splitter,
        sc     TYPE REF TO cl_gui_splitter_container,
        row    TYPE i,
        column TYPE i,
      END OF gty_s_splitter .
  types:
    BEGIN OF gty_s_sel_opt,
        field TYPE fieldname,
        value TYPE REF TO data,
      END OF gty_s_sel_opt .
  types:
    gty_t_change_pointers TYPE STANDARD TABLE OF bdcp .
  types:
    gty_t_cpi TYPE STANDARD TABLE OF bdicpident .
  types:
    gty_t_sel_names TYPE STANDARD TABLE OF /vpcoe/de_sel_name .
  types:
    gty_t_sel_opt TYPE STANDARD TABLE OF gty_s_sel_opt .
  types:
    gty_t_column_names TYPE STANDARD TABLE OF lvc_fname .
  types:
    BEGIN OF gty_s_columns_text,
        column TYPE lvc_fname,
        text   TYPE scrtext_m,
      END OF gty_s_columns_text .
  types:
    gty_t_columns_text TYPE STANDARD TABLE OF gty_s_columns_text .

  constants:
    BEGIN OF gc_api_version,
        rdp  TYPE /vpcoe/rdp_version VALUE 'RDP',
        suma TYPE /vpcoe/rdp_version VALUE 'SUMA',
      END OF gc_api_version .
  constants GC_DISPLAY_AMOUNT type INT4 value '50000' ##NO_TEXT.
  constants GC_VP_VERSION type STRING value 'v6.1.0' ##NO_TEXT.
  constants:
    BEGIN OF sc_api_type,
        rdp TYPE /vpcoe/de_api_type VALUE 'RDP',
*        pfm TYPE /vpcoe/de_api_type VALUE 'PFM',
        plm TYPE /vpcoe/de_api_type VALUE 'PLM',
      END OF sc_api_type .
  constants SC_BATCH_CHARACTERISTIC type STRING value 'ZRDP_BATCH_PACKCOMP' ##NO_TEXT.
  constants:
    BEGIN OF sc_config_param,
        source_id        TYPE /vpcoe/de_config_name VALUE 'SOURCE_ID',
        company_id       TYPE /vpcoe/de_config_name VALUE 'COMPANY_ID',
        max_lines_count  TYPE /vpcoe/de_config_name VALUE 'MAX_LINES_COUNT',
        material_type    TYPE /vpcoe/de_config_name VALUE 'MATERIAL_TYPE',
        db_connection    TYPE /vpcoe/de_config_name VALUE 'DB_CONNECTION',
        enable_trace     TYPE /vpcoe/de_config_name VALUE 'ENABLE_TRACE',
        dlv_catch_weight TYPE /vpcoe/de_config_name VALUE 'DLV_CATCH_WEIGHT_ON',
      END OF sc_config_param .
  constants:
    BEGIN OF sc_grp_id,
        organization  TYPE /vpcoe/de_service_group VALUE 'ORG',
        configuration TYPE /vpcoe/de_service_group VALUE 'CONF',
        company_code  TYPE /vpcoe/de_service_group VALUE 'CODE',
        customer      TYPE /vpcoe/de_service_group VALUE 'CSTMR',
        product       TYPE /vpcoe/de_service_group VALUE 'PRD',
        delivery      TYPE /vpcoe/de_service_group VALUE 'DLVR',
        packaging     TYPE /vpcoe/de_service_group VALUE 'PCKG',
        batch         TYPE /vpcoe/de_service_group VALUE 'BATCH',
        material_doc  TYPE /vpcoe/de_service_group VALUE 'DOC',
        supplier      TYPE /vpcoe/de_service_group VALUE 'SUPLR',
        inventory     TYPE /vpcoe/de_service_group VALUE 'INVEN',
        hu            TYPE /vpcoe/de_service_group VALUE 'HU',
        incoterms     TYPE /vpcoe/de_service_group VALUE 'INCO',
        plm           TYPE /vpcoe/de_service_group VALUE 'PLM',
        billdocit     TYPE /vpcoe/de_service_group VALUE 'BDI',
        supinvi       TYPE /vpcoe/de_service_group VALUE 'SII',
      END OF sc_grp_id .
  constants:
    BEGIN OF sc_level,
        hdr               TYPE /vpcoe/level VALUE 'HEADER',
        adjust_hdr        TYPE /vpcoe/level VALUE 'ADJUST_HEADER',
        hdr_bckgrnd       TYPE /vpcoe/level VALUE 'HDRBCK',
        itm               TYPE /vpcoe/level VALUE 'ITEM',
        dscr              TYPE /vpcoe/level VALUE 'DESCR',
        uom               TYPE /vpcoe/level VALUE 'UOM',
        uom_code          TYPE /vpcoe/level VALUE 'UOMCD',
        plants            TYPE /vpcoe/level VALUE 'PLANTS',
        sales             TYPE /vpcoe/level VALUE 'SALES',
        supplier          TYPE /vpcoe/level VALUE 'SUPPLIER',
        dlvr_bckgrnd      TYPE /vpcoe/level VALUE 'DLVRBCK',
        customer          TYPE /vpcoe/level VALUE 'CUSTOMER',
        customer_del      TYPE /vpcoe/level VALUE 'CUSTOMER_DEL',
        batch             TYPE /vpcoe/level VALUE 'BATCH',
        cost_est          TYPE /vpcoe/level VALUE 'COSTEST',
        tax_number        TYPE /vpcoe/level VALUE 'TAX_NUM',
        build_json        TYPE /vpcoe/level VALUE 'BLD_JSON',
        dlvr_billdoc      TYPE /vpcoe/level VALUE 'DLVR_BLDC',
        mtrl_billdoc      TYPE /vpcoe/level VALUE 'MTRL_BLDC',
        mtrl_suppinv      TYPE /vpcoe/level VALUE 'MTRL_SUPPINV',
        gl_trade_item_num TYPE /vpcoe/level VALUE 'GLTRDINUM',
        quantity_conv     TYPE /vpcoe/level VALUE 'QUANCONV',
        quantity_dimen     TYPE /vpcoe/level VALUE 'QUANDIME',
      END OF sc_level .
  constants:
    BEGIN OF sc_log_doc_cat,
        sales_order                    TYPE /vpcoe/log_doc_category VALUE 'SO',
        sales_order_wo_charge          TYPE /vpcoe/log_doc_category VALUE 'OC',
        sales_quotation                TYPE /vpcoe/log_doc_category VALUE 'SQ',
        sales_contract                 TYPE /vpcoe/log_doc_category VALUE 'SC',
        scheduling_agreement           TYPE /vpcoe/log_doc_category VALUE 'SA',
        sales_inquiry                  TYPE /vpcoe/log_doc_category VALUE 'SI', " Not used

        outbound_delivery              TYPE /vpcoe/log_doc_category VALUE 'LF',

        freight_order                  TYPE /vpcoe/log_doc_category VALUE 'FO',
        freight_unit                   TYPE /vpcoe/log_doc_category VALUE 'FU',
        freight_booking                TYPE /vpcoe/log_doc_category VALUE 'FB',

        standard_purchase_order        TYPE /vpcoe/log_doc_category VALUE 'PO',
        purchase_order_w_transp_rlvnce TYPE /vpcoe/log_doc_category VALUE 'PT',
        stock_transport_order          TYPE /vpcoe/log_doc_category VALUE 'TO',
        ico_stock_transport_order      TYPE /vpcoe/log_doc_category VALUE 'TI',
        ico_sales_purchase_order       TYPE /vpcoe/log_doc_category VALUE 'PI',
        purchase_contract              TYPE /vpcoe/log_doc_category VALUE 'PC',
        purchase_scheduling_agreement  TYPE /vpcoe/log_doc_category VALUE 'PS',
        purchase_info_record           TYPE /vpcoe/log_doc_category VALUE 'PR',
        quantity_contract              TYPE /vpcoe/log_doc_category VALUE 'MK', " Not used
        value_contract                 TYPE /vpcoe/log_doc_category VALUE 'WK', " Not used
      END OF                          sc_log_doc_cat .
  constants:
    BEGIN OF sc_mode,
        send     TYPE /vpcoe/de_mode VALUE 1,
        screen   TYPE /vpcoe/de_mode VALUE 2,
        document TYPE /vpcoe/de_mode VALUE 3,
      END OF sc_mode .
  constants:
    BEGIN OF sc_pretty_mode,
        none       TYPE char1  VALUE ``,
        low_case   TYPE char1  VALUE `L`,
        camel_case TYPE char1  VALUE `X`,
        extended   TYPE char1  VALUE `Y`,
      END OF  sc_pretty_mode .
  constants:
    BEGIN OF sc_service_id,
        uom_dimension         TYPE /vpcoe/de_service_id VALUE 'UoM Dimension',
        uom                   TYPE /vpcoe/de_service_id VALUE 'UoM',
        uom_code              TYPE /vpcoe/de_service_id VALUE 'UoM Code',
        currency              TYPE /vpcoe/de_service_id VALUE 'Currency',
        country               TYPE /vpcoe/de_service_id VALUE 'Country',
        company_code_exe      TYPE /vpcoe/de_service_id VALUE 'Company Code Exemptions',
        region                TYPE /vpcoe/de_service_id VALUE 'Region',
        product_type          TYPE /vpcoe/de_service_id VALUE 'Product Type',
        product_ext           TYPE /vpcoe/de_service_id VALUE 'Product Extension',
        product_group         TYPE /vpcoe/de_service_id VALUE 'Product Group',
        product_hierarchy     TYPE /vpcoe/de_service_id VALUE 'Product Hierarchy',
        delivery_doc_type     TYPE /vpcoe/de_service_id VALUE 'Delivery Document Type',
        delivery_doc_item_cat TYPE /vpcoe/de_service_id VALUE 'Delivery Document Item Category',
        movement_type         TYPE /vpcoe/de_service_id VALUE 'Movement Type',
        company_code          TYPE /vpcoe/de_service_id VALUE 'Company Code',
        sales_organization    TYPE /vpcoe/de_service_id VALUE 'Sales Organization',
        plant                 TYPE /vpcoe/de_service_id VALUE 'Plant',
        distribution_channel  TYPE /vpcoe/de_service_id VALUE 'Distribution Channel',
        division              TYPE /vpcoe/de_service_id VALUE 'Division',
        customer              TYPE /vpcoe/de_service_id VALUE 'Customer',
        customer_role         TYPE /vpcoe/de_service_id VALUE 'Customer Roles',
        customer_exemptions   TYPE /vpcoe/de_service_id VALUE 'Customer Exemptions',
        customer_ext          TYPE /vpcoe/de_service_id VALUE 'Customer Extension',
        product               TYPE /vpcoe/de_service_id VALUE 'Product',
        delivery              TYPE /vpcoe/de_service_id VALUE 'Delivery',
        packaging             TYPE /vpcoe/de_service_id VALUE 'Batch',
        material_doc          TYPE /vpcoe/de_service_id VALUE 'Material Documents',
        supplier              TYPE /vpcoe/de_service_id VALUE 'Suppliers',
*        supplier_ext                TYPE /vpcoe/de_service_id VALUE 'Suppliers Extension',
*        valuation_area_company_code TYPE /vpcoe/de_service_id VALUE 'Valuation Area with Company Code',
        run_id                TYPE /vpcoe/de_service_id VALUE 'Run Id',
        inventory             TYPE /vpcoe/de_service_id VALUE 'Inventory',
        cost_estimate         TYPE /vpcoe/de_service_id VALUE 'Cost Estimate',
        sales_area            TYPE /vpcoe/de_service_id VALUE 'Sales Area',
        inflow_supplier       TYPE /vpcoe/de_service_id VALUE 'Inflow from Suppliers',
        outflow_opt           TYPE /vpcoe/de_service_id VALUE 'Outflow for Operations',
        org                   TYPE /vpcoe/de_service_id VALUE 'OrganizationData',
        conf                  TYPE /vpcoe/de_service_id VALUE 'ConfigurationData',
        incoterms             TYPE /vpcoe/de_service_id VALUE 'Incoterms',
        bom                   TYPE /vpcoe/de_service_id VALUE 'BOM',
        plm                   TYPE /vpcoe/de_service_id VALUE 'PLM',
        rcp                   TYPE /vpcoe/de_service_id VALUE 'RCP',
        upload_data           TYPE /vpcoe/de_service_id VALUE 'UPLOAD_DATA',
        mcl                   TYPE /vpcoe/de_service_id VALUE 'MCL',
        handling_units        TYPE /vpcoe/de_service_id VALUE 'Handling Units',
        packaging_composition TYPE /vpcoe/de_service_id VALUE 'Packaging Composition',
        billdocit             TYPE /vpcoe/de_service_id VALUE 'Billing Document Items',
        supinvi               TYPE /vpcoe/de_service_id VALUE 'Supplier Invoice Items',
        product_val           TYPE /vpcoe/de_service_id VALUE 'End of validity a product',
      END OF sc_service_id .
  constants:
    BEGIN OF sc_status,
        success TYPE /vpcoe/de_status VALUE 'Success',
        failed  TYPE /vpcoe/de_status VALUE 'Failed',
      END OF sc_status .
  constants:
    BEGIN OF sc_dimension_name,
        net_weight   TYPE /vpcoe/de_dimension_name VALUE 'NET_WEIGHT',
        gross_weight TYPE /vpcoe/de_dimension_name VALUE 'GROSS_WEIGHT',
      END OF sc_dimension_name .
  constants:
    BEGIN OF sc_tvarv_param,
        source_id       TYPE rvari_vnam VALUE '/VPCOE/RDP_SOURCE_ID',
        max_lines_count TYPE rvari_vnam VALUE 'MAX_LINES_COUNT',
      END OF sc_tvarv_param .
  class-data GV_IS_SCREEN_INITIALIZED type XFELD .

  class-methods ADD_SYMSG_TO_BAPIRET
    changing
      !CT_BAPIRET2 type BAPIRET2_T .
  class-methods CONVERT_LANGU_CODE
    importing
      !IV_INT_LANGU type SPRAS
    returning
      value(RV_EXT_LANGU) type LAISO .
  class-methods DESERIALIZE_JSON
    importing
      !IV_JSON type STRING
      !IV_PRETTY_NAME type CHAR1 default ``
      !IV_ASSOC_ARRAYS type CHAR1 default ``
      !IT_NAME_MAPPINGS type GTY_T_NAME_MAPPINGS optional
    changing
      !CS_DATA type DATA .
  class-methods GENERATE_SESSION_ID
    returning
      value(RV_SESSION_ID) type /VPCOE/S_JSN_CLOUD-SESSION_ID .
  class-methods GET_CONF_PARAM_VALUE
    importing
      !IV_PARAM type /VPCOE/DE_CONFIG_NAME
    returning
      value(RV_VALUE) type /VPCOE/DE_CONFIG_CHAR_VALUE .
  class-methods GET_COUNT_OF_PROCESSES
    importing
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
    returning
      value(RV_WP_COUNT) type /VPCOE/RDP_SRVID-COUNT_OF_PROCESSES .
  class-methods GET_DB_CONNECTION
    importing
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IV_API_TYPE type /VPCOE/DE_API_TYPE
      value(IS_SEL_OPT) type DATA
      value(IV_LEVEL) type /VPCOE/LEVEL
    returning
      value(RV_DB_CONNECTION) type DBCON_NAME .
  class-methods GET_MATERIAL_TYPE
    exporting
      value(ET_R_MATERIAL_TYPE) type /VPCOE/TT_R_MTART .
  class-methods GET_MAX_LINES_COUNT
    returning
      value(RV_MAX_LINES) type INT4 .
  class-methods GET_MONTH_YEAR_LAST_RUN
    importing
      !IV_LAST_RUN type DATS
    exporting
      !EV_MONTH type CHAR2
      !EV_YEAR type CHAR4
      !EV_FAILED_INTERVAL type ABAP_BOOL .
  class-methods GET_COMPANY_ID
    returning
      value(RV_COMPANY_ID) type /VPCOE/DE_CONFIG_CHAR_VALUE .
  class-methods GET_SOURCE_ID
    returning
      value(RV_SOURCE_ID) type STRING .
  class-methods GET_TRACE_ENABLE
    returning
      value(RV_IS_ENABLE) type FLAG .
  class-methods GET_VERSION_TEXT
    returning
      value(RV_VERSION) type STRINGVAL .
  class-methods READ_CHANGE_POINTERS
    importing
      !IV_CHNG_POINTER_ID type EDI_MESTYP
      !IT_R_CREATE_DATE type /VPCOE/TT_R_LAEDA optional
      !IT_R_CHANGE_DATE type /VPCOE/TT_R_LAEDA optional
    exporting
      !ET_R_OBJID_DEL type ANY TABLE
      !ET_R_OBJID type ANY TABLE
      !ET_CPI type GTY_T_CPI .
  class-methods SERIALIZE_JSON
    importing
      !IS_DATA type DATA
      !IV_COMPRESS type CHAR1 default ``
      !IV_NAME type STRING optional
      !IV_PRETTY_NAME type CHAR1 default ``
      !IO_TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
      !IV_ASSOC_ARRAYS type CHAR1 default ``
      !IV_TS_AS_ISO8601 type CHAR1 default ``
      !IV_EXPAND_INCLUDES type CHAR1 default `X`
      !IV_NUMC_AS_STRING type CHAR1 default ``
      !IT_NAME_MAPPINGS type GTY_T_NAME_MAPPINGS optional
    returning
      value(RV_JSON) type STRING .
  class-methods SET_MODE
    importing
      !IV_SCREEN type XFELD
      !IV_EXCEL type XFELD
    returning
      value(RV_MODE) type /VPCOE/DE_MODE .
  methods ADD_SEL_OPT
    importing
      !IT_SO_GEN type STANDARD TABLE
    changing
      !CT_SO type STANDARD TABLE .
  methods BUILD_FIELD_LIST
    importing
      !IV_SOURCE_FIELD type /VPCOE/DE_SOURCE_FIELD
      !IV_TARGET_FIELD type /VPCOE/DE_TARGET_FIELD
      !IV_MAPPING_TYPE type /VPCOE/DE_MAPPING_TYPE default 'O'
      !IV_TAB_NAME type TABNAME16
    changing
      !CV_ATTR type STRING .
  methods CONSTRUCTOR
    importing
      !IV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP
      !IV_SRV_ID type /VPCOE/DE_SERVICE_ID
      !IV_API_TYPE type /VPCOE/DE_API_TYPE .
  methods CONVERT_DEEP_TAB_TO_FLAT
    importing
      !IT_DEEP_DATA type ANY TABLE
    returning
      value(RO_FLAT_DATA) type ref to DATA .
  methods DISPLAY_SELECTED_DATA
    importing
      !IT_DEEP_DATA type ANY TABLE
      !IV_LABLE type /VPCOE/DE_SERVICE_ID optional
      !IS_SPLITTER type /VPCOE/CL_COMMON_HELPER=>GTY_S_SPLITTER optional
      !IT_EXCLUDE_COLUMNS type /VPCOE/CL_COMMON_HELPER=>GTY_T_COLUMN_NAMES optional
      !IT_COLUMNS_TITLE type /VPCOE/CL_COMMON_HELPER=>GTY_T_COLUMNS_TEXT optional .
  methods GET_API_TYPE
    returning
      value(RV_API_TYPE) type /VPCOE/DE_API_TYPE .
  methods GET_GENERIC_RFC_NAME
    returning
      value(RV_GEN_RFC_NAME) type RFCDOC_D .
  methods GET_HTTP_CLIENT
    exporting
      !ET_BAPIRET2 type BAPIRET2_T
      !EV_HTTP_VERSION type RFCDISPLAY-RFCTYPE
    returning
      value(RO_HTTP_CLIENT) type ref to /VPCOE/CL_HTTP_COMMUNICATION .
  methods GET_PACKAGE_SIZE
    returning
      value(RV_PCKG_SIZE) type /VPCOE/DE_PACKAGE_SIZE .
  methods GET_SERVICE_URL
    exporting
      !ET_BAPIRET2 type BAPIRET2_T
    returning
      value(RV_URL) type STRING .
  methods GET_SRV_GRP
    returning
      value(RV_SRV_GRP) type /VPCOE/DE_SERVICE_GROUP .
  methods GET_SRV_ID
    returning
      value(RV_SRV_ID) type /VPCOE/DE_SERVICE_ID .
  methods GET_URL_FROM_RFC
    importing
      !IV_RFC_NAME type RFCDES-RFCDEST
    exporting
      !ET_BAPIRET2 type BAPIRET2_T
      !EV_SERVER type RFCDISPLAY-RFCHOST
      !EV_PROXY_HOST type RFCDISPLAY-RFCGWHOST
      !EV_PROXY_SERVICE type RFCDISPLAY-RFCGWSERV
      !EV_HTTP_VERSION type RFCDISPLAY-RFCTYPE .
protected section.

  class-data SV_SOURCE_ID type STRING .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_PCKG_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SRV_GRP type /VPCOE/DE_SERVICE_GROUP .
  data MV_SRV_ID type /VPCOE/DE_SERVICE_ID .
  data MV_URL type STRING .
private section.

  class-data ST_LANGU type TT_T002 .
ENDCLASS.



CLASS /VPCOE/CL_COMMON_HELPER IMPLEMENTATION.


  METHOD ADD_SEL_OPT.

    LOOP AT it_so_gen ASSIGNING FIELD-SYMBOL(<lr_s_so_gen>).
      ASSIGN <lr_s_so_gen>->* TO FIELD-SYMBOL(<ls_so_gen>).
      ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_so_gen> TO FIELD-SYMBOL(<lv_sign>).
      ASSIGN <lv_sign>->* TO FIELD-SYMBOL(<lv_sign_val>).
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_so_gen> TO FIELD-SYMBOL(<lv_option>).
      ASSIGN <lv_option>->* TO FIELD-SYMBOL(<lv_option_val>).
      ASSIGN COMPONENT 'LOW'    OF STRUCTURE <ls_so_gen> TO FIELD-SYMBOL(<lv_low>).
      ASSIGN <lv_low>->* TO FIELD-SYMBOL(<lv_low_val>).
      ASSIGN COMPONENT 'HIGH'   OF STRUCTURE <ls_so_gen> TO FIELD-SYMBOL(<lv_high>).
      ASSIGN <lv_high>->* TO FIELD-SYMBOL(<lv_high_val>).

      APPEND INITIAL LINE TO ct_so ASSIGNING FIELD-SYMBOL(<ls_new_line>).
      ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_new_line> TO FIELD-SYMBOL(<lv_sign_new>).
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_new_line> TO FIELD-SYMBOL(<lv_option_new>).
      ASSIGN COMPONENT 'LOW'    OF STRUCTURE <ls_new_line> TO FIELD-SYMBOL(<lv_low_new>).
      ASSIGN COMPONENT 'HIGH'   OF STRUCTURE <ls_new_line> TO FIELD-SYMBOL(<lv_high_new>).

      <lv_sign_new>   = <lv_sign_val>.
      <lv_option_new> = <lv_option_val>.

      DATA(ls_ddic_header) = cl_abap_typedescr=>describe_by_data( <lv_low_new> )->get_ddic_header( ).
      IF sy-subrc <> 0 OR ls_ddic_header-refname <> 'DATUM'.
        <lv_low_new>    = <lv_low_val>.
        <lv_high_new>   = <lv_high_val>.
      ELSE.
        REPLACE ALL OCCURRENCES OF '-' IN <lv_low_val> WITH ''.
        REPLACE ALL OCCURRENCES OF '-' IN <lv_high_val> WITH ''.
        <lv_low_new>    = <lv_low_val>.
        <lv_high_new>   = <lv_high_val>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD ADD_SYMSG_TO_BAPIRET.

    INSERT VALUE #( type       = sy-msgty
                    id         = sy-msgid
                    number     = sy-msgno
                    message_v1 = sy-msgv1
                    message_v2 = sy-msgv2
                    message_v3 = sy-msgv3
                    message_v4 = sy-msgv4 ) INTO TABLE ct_bapiret2.

  ENDMETHOD.


  METHOD BUILD_FIELD_LIST.
    DATA: lv_target_field	TYPE /vpcoe/de_target_field,
          lv_attr         TYPE string.

    IF cv_attr IS NOT INITIAL.
      CONCATENATE cv_attr `,` INTO cv_attr SEPARATED BY space.
    ENDIF.

    lv_attr = iv_tab_name && `~` && iv_source_field.
    CONCATENATE cv_attr lv_attr INTO cv_attr SEPARATED BY space.

    CONCATENATE cv_attr `AS` lv_target_field INTO cv_attr SEPARATED BY space.
  ENDMETHOD.


  METHOD CONSTRUCTOR.

    me->mv_api_type = iv_api_type.
    me->mv_srv_grp  = iv_srv_grp.
    me->mv_srv_id   = iv_srv_id.

  ENDMETHOD.


  METHOD convert_deep_tab_to_flat.

    DATA: lo_tab             TYPE REF TO cl_abap_tabledescr,
          lo_line            TYPE REF TO cl_abap_structdescr,
          lo_include         TYPE REF TO cl_abap_structdescr,
          lo_flat_table_type TYPE REF TO cl_abap_tabledescr,
          lt_flat_component  TYPE abap_component_tab,
          lv_flat_struct     TYPE abap_bool VALUE abap_true.

    FIELD-SYMBOLS: <lt_flat_table>   TYPE STANDARD TABLE.

    lo_tab ?= cl_abap_tabledescr=>describe_by_data( it_deep_data ).
    lo_line ?= lo_tab->get_table_line_type( ).
    DATA(lt_deep_components) = lo_line->get_components( ).

    LOOP AT lo_line->get_components( ) ASSIGNING FIELD-SYMBOL(<ls_component>) WHERE as_include = abap_true.
      lo_include ?= <ls_component>-type.
      INSERT LINES OF lo_include->get_components( ) INTO TABLE lt_deep_components.
    ENDLOOP.

    LOOP AT lt_deep_components ASSIGNING <ls_component> WHERE as_include = abap_false.
      DATA(ls_ddic_header) = <ls_component>-type->get_ddic_header( ).

      IF ls_ddic_header-tabtype <> 'E' AND ls_ddic_header-tabtype <> 'J'.
        CONTINUE.
      ENDIF.
      IF ls_ddic_header-tabtype = 'J'.
        lv_flat_struct = abap_false.
      ENDIF.
      INSERT VALUE #( name = <ls_component>-name
                      type = <ls_component>-type ) INTO TABLE lt_flat_component.
    ENDLOOP.

    lo_flat_table_type = cl_abap_tabledescr=>create( p_line_type = cl_abap_structdescr=>create( p_components = lt_flat_component ) ).

    CREATE DATA ro_flat_data TYPE HANDLE lo_flat_table_type.

    ASSIGN ro_flat_data->* TO <lt_flat_table>.
*    <lt_flat_table> = CORRESPONDING #( it_deep_data ).
    ASSIGN ro_flat_data->* TO <lt_flat_table>.
    LOOP AT it_deep_data  ASSIGNING FIELD-SYMBOL(<ls_deep_data>).
      IF lv_flat_struct = abap_true.
        APPEND INITIAL LINE TO <lt_flat_table> ASSIGNING FIELD-SYMBOL(<ls_flat_data>).
        MOVE-CORRESPONDING <ls_deep_data> TO <ls_flat_data>  .
      ELSE.
        APPEND <ls_deep_data> TO <lt_flat_table>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD CONVERT_LANGU_CODE.

    rv_ext_langu = VALUE #( st_langu[ spras = iv_int_langu ]-laiso OPTIONAL ).

    IF rv_ext_langu IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
        EXPORTING
          input  = iv_int_langu
        IMPORTING
          output = rv_ext_langu.

      IF rv_ext_langu IS NOT INITIAL.
        INSERT VALUE #( spras = iv_int_langu
                        laiso = rv_ext_langu ) INTO TABLE st_langu.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method DESERIALIZE_JSON.

*  zcl_json=>deserialize(
    /ui2/cl_json=>deserialize(
      EXPORTING
        json          = iv_json
        pretty_name   = iv_pretty_name
        assoc_arrays  = iv_assoc_arrays
        name_mappings = it_name_mappings
      CHANGING
        data          = cs_data ).

  endmethod.


  METHOD display_selected_data.

    DATA: lr_table TYPE REF TO cl_salv_table.

    FIELD-SYMBOLS: <lt_flat_table> TYPE STANDARD TABLE.

    TRY.

        DATA(lt_flat_table) = me->convert_deep_tab_to_flat( it_deep_data ).

        ASSIGN lt_flat_table->* TO <lt_flat_table>.

        IF is_splitter-sc IS BOUND.

          DATA(lo_container) = is_splitter-sc->get_container(
                                                          EXPORTING
                                                            row       = COND #( WHEN is_splitter-row IS NOT INITIAL
                                                                                THEN is_splitter-row ELSE 1 )
                                                            column    = COND #( WHEN is_splitter-column IS NOT INITIAL
                                                                                THEN is_splitter-column ELSE 1 ) ).

          cl_salv_table=>factory( EXPORTING r_container = lo_container
                                  IMPORTING r_salv_table = lr_table
                                  CHANGING t_table      = <lt_flat_table> ).

          IF iv_lable IS NOT INITIAL.
            DATA(lo_display_settings) = lr_table->get_display_settings( ).
            lo_display_settings->set_list_header( CONV #( iv_lable ) ).
            lo_display_settings->set_list_header_size( cl_salv_display_settings=>c_header_size_medium ).
          ENDIF.

        ELSE.
          cl_salv_table=>factory( IMPORTING r_salv_table = lr_table
                                  CHANGING t_table      = <lt_flat_table> ).

          IF iv_lable IS NOT INITIAL.
            DATA(lo_header) = NEW cl_salv_form_layout_grid( ).
            DATA(lo_h_label) = lo_header->create_label( row = 3 column = 1 ).
            lo_h_label->set_text( iv_lable ).
            lr_table->set_top_of_list( lo_header ).
          ENDIF.

        ENDIF.

        lr_table->get_functions( )->set_all( abap_true ).
        DATA(lr_columns) = lr_table->get_columns( ).
        lr_columns->set_optimize( abap_true ).

        LOOP AT it_exclude_columns ASSIGNING FIELD-SYMBOL(<ls_column>).
          TRY.
              DATA(lr_column) = lr_columns->get_column( <ls_column> ).
              lr_column->set_visible( if_salv_c_bool_sap=>false ).
            CATCH cx_salv_not_found.
              CONTINUE.
          ENDTRY.
        ENDLOOP.

        LOOP AT it_columns_title ASSIGNING FIELD-SYMBOL(<ls_column_title>).
          TRY.
              lr_column = lr_columns->get_column( <ls_column_title>-column ).
              lr_column->set_medium_text( <ls_column_title>-text ).
              lr_column->set_short_text( CONV #( <ls_column_title>-text ) ).
            CATCH cx_salv_not_found.
              CONTINUE.
          ENDTRY.
        ENDLOOP.
        lr_table->display( ).

      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD GENERATE_SESSION_ID.

    TRY.

        rv_session_id = cl_system_uuid=>create_uuid_x16_static( ).

      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        MESSAGE lx_uuid_error TYPE 'X'.
    ENDTRY.

  ENDMETHOD.


  METHOD GET_API_TYPE.
    rv_api_type = me->mv_api_type.
  ENDMETHOD.


METHOD get_company_id.

  rv_company_id = /vpcoe/cl_common_helper=>get_conf_param_value( /vpcoe/cl_common_helper=>sc_config_param-company_id ).

ENDMETHOD.


METHOD get_conf_param_value.

  SELECT SINGLE config_value
     INTO rv_value
        FROM /vpcoe/config
          WHERE config_name = iv_param.

  IF sy-subrc <> 0.
    CLEAR rv_value.
  ENDIF.

ENDMETHOD.


  METHOD GET_COUNT_OF_PROCESSES.

    SELECT SINGLE count_of_processes
      INTO @rv_wp_count
        FROM /vpcoe/rdp_srvid
          WHERE service_grp = @iv_srv_grp
            AND service_id  = @iv_srv_id
            AND api_type    = @iv_api_type.

    IF sy-subrc <> 0 OR rv_wp_count IS INITIAL.
      rv_wp_count = 5.
    ENDIF.

  ENDMETHOD.


  METHOD get_db_connection.

    DATA: lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    GET BADI lo_badi.

    CALL BADI lo_badi->get_db_connection
      EXPORTING
        iv_srv_grp       = iv_srv_grp
        iv_srv_id        = iv_srv_id
        iv_api_type      = iv_api_type
        is_sel_opt       = is_sel_opt
        iv_level         = iv_level
      CHANGING
        rv_db_connection = rv_db_connection.

    IF rv_db_connection IS INITIAL.
      rv_db_connection = /vpcoe/cl_common_helper=>get_conf_param_value( /vpcoe/cl_common_helper=>sc_config_param-db_connection ).

      IF rv_db_connection IS INITIAL.
        rv_db_connection = 'R/3*RDP'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_generic_rfc_name.
*
*    rv_gen_rfc_name = SWITCH #( me->mv_api_type
**                                  WHEN /VPCOE/CL_rdp_CUSTOM_HELPER=>sc_api_type-pfm
**                                      THEN me->get_pfm_generic_rfc_name( )
*                                  WHEN /vpcoe/cl_rdp_custom_helper=>sc_api_type-rdp
*                                      THEN me->get_rdp_generic_rfc_name( )
*                                   WHEN /vpcoe/cl_rdp_custom_helper=>sc_api_type-plm
*                                      THEN me->get_plm_generic_rfc_name( ) ).
  ENDMETHOD.


  METHOD get_http_client.
    DATA: lv_server        TYPE rfcdisplay-rfchost,
          lv_proxy_host    TYPE rfcdisplay-rfcgwhost,
          lv_proxy_service TYPE rfcdisplay-rfcgwserv,
          lt_bapiret2	     TYPE bapiret2_t.

    CLEAR: et_bapiret2, ev_http_version.

    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = CONV rfcdes-rfcdest( me->get_generic_rfc_name( ) )
        authority_check         = abap_false
        bypass_buf              = abap_false
      IMPORTING
        server                  = lv_server
        proxy_host              = lv_proxy_host
        proxy_service           = lv_proxy_service
        http_version            = ev_http_version
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.

    IF sy-subrc <> 0.
      /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
      RETURN.
    ENDIF.

    DATA(lv_url) = me->get_service_url( IMPORTING et_bapiret2 = lt_bapiret2 ).
    INSERT LINES OF lt_bapiret2 INTO TABLE et_bapiret2.
    IF line_exists( et_bapiret2[ type = 'E' ] ).
      RETURN.
    ENDIF.

    TRY.
        ro_http_client = NEW /vpcoe/cl_http_communication( iv_api_type      = me->get_api_type( )
                                                           iv_url           = |https://{ lv_server }{ lv_url }|
                                                           iv_proxy_host    = lv_proxy_host
                                                           iv_proxy_service = lv_proxy_service ).
      CATCH cx_oa2c INTO DATA(lx_oa2c).
        MESSAGE e003(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
        MESSAGE e000(/vpcoe/common) WITH lx_oa2c->get_text( ) '' '' '' INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_material_type.

    CLEAR et_r_material_type.

    SELECT SINGLE *
       INTO @DATA(ls_config)
          FROM /vpcoe/config
            WHERE config_name = @/vpcoe/cl_common_helper=>sc_config_param-material_type.

    IF sy-subrc = 0 AND ls_config-config_value IS NOT INITIAL.
      IF ls_config-multiple_values = abap_true.
        " Multiple Product Types
        SPLIT ls_config-config_value AT ';' INTO TABLE DATA(lt_config_values).
        LOOP AT lt_config_values ASSIGNING FIELD-SYMBOL(<ls_config_values>).
          INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_config_values> ) INTO TABLE et_r_material_type.
        ENDLOOP.
      ELSE.
        "Single Value
        INSERT VALUE #( sign = 'I' option = 'EQ' low = ls_config-config_value ) INTO TABLE et_r_material_type.
      ENDIF.
    ELSE.
      "Default Value
      INSERT VALUE #( sign = 'I' option = 'EQ' low = 'VERP' ) INTO TABLE et_r_material_type.

    ENDIF.

  ENDMETHOD.


METHOD get_max_lines_count.

  SELECT SINGLE low
    FROM tvarvc
      INTO @DATA(lv_max_lines)
    WHERE name = @/vpcoe/cl_common_helper=>sc_tvarv_param-max_lines_count
      AND type = 'P'
      AND numb = 0.

  IF sy-subrc <> 0.
    /vpcoe/cl_common_helper=>get_conf_param_value( /vpcoe/cl_common_helper=>sc_config_param-max_lines_count ).
  ENDIF.

  IF lv_max_lines IS INITIAL.
    rv_max_lines = 1000000.
  ELSE.
    rv_max_lines = lv_max_lines.
  ENDIF.

ENDMETHOD.


  METHOD GET_MONTH_YEAR_LAST_RUN.
    CLEAR: ev_month, ev_year, ev_failed_interval.

    IF iv_last_run IS INITIAL.
      RETURN.
    ENDIF.

    ev_month = iv_last_run+4(2).
    ev_year = iv_last_run+0(4).

    IF ev_month = sy-datum+4(2) AND ev_year = iv_last_run+0(4).
      ev_failed_interval = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD GET_PACKAGE_SIZE.

    IF me->mv_pckg_size IS INITIAL.
      SELECT SINGLE *
        INTO @DATA(ls_rdp_srvid)
          FROM /vpcoe/rdp_srvid
            WHERE service_grp = @me->mv_srv_grp
              AND service_id  = @me->mv_srv_id
              AND api_type    = @me->mv_api_type.
      IF sy-subrc = 0.
        me->mv_url = ls_rdp_srvid-prefix.
        me->mv_pckg_size = ls_rdp_srvid-package_size.
      ENDIF.
    ENDIF.

    rv_pckg_size = me->mv_pckg_size.

  ENDMETHOD.


  METHOD GET_SERVICE_URL.

    CLEAR et_bapiret2.

    IF me->mv_url IS INITIAL.
      SELECT SINGLE *
        INTO @DATA(ls_rdp_srvid)
          FROM /vpcoe/rdp_srvid
            WHERE service_grp = @me->mv_srv_grp
              AND service_id  = @me->mv_srv_id
              AND api_type    = @me->mv_api_type.
      IF sy-subrc = 0.
        me->mv_url = ls_rdp_srvid-prefix.
        me->mv_pckg_size = ls_rdp_srvid-package_size.
      ELSE.
        MESSAGE e045(/vpcoe/common) WITH me->mv_srv_grp && '/' && me->mv_srv_id INTO /VPCOE/CL_rdp_LOG=>sv_msg_text.
        /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
      ENDIF.
    ENDIF.

    rv_url = me->mv_url.

  ENDMETHOD.


  METHOD get_source_id.

    IF /vpcoe/cl_common_helper=>sv_source_id IS INITIAL.
      SELECT SINGLE tvarvc~low
         INTO /vpcoe/cl_common_helper=>sv_source_id
            FROM tvarvc
              WHERE name = /vpcoe/cl_common_helper=>sc_tvarv_param-source_id.

      IF sy-subrc <> 0.

        /vpcoe/cl_common_helper=>sv_source_id = /vpcoe/cl_common_helper=>get_conf_param_value( /vpcoe/cl_common_helper=>sc_config_param-source_id ).

      ENDIF.
    ENDIF.

    rv_source_id = /vpcoe/cl_common_helper=>sv_source_id.

  ENDMETHOD.


  METHOD GET_SRV_GRP.
    rv_srv_grp = me->mv_srv_grp.
  ENDMETHOD.


  METHOD GET_SRV_ID.

    rv_srv_id = me->mv_srv_id.

  ENDMETHOD.


  METHOD get_trace_enable.

    DATA(lv_is_enable) = /vpcoe/cl_common_helper=>get_conf_param_value( /vpcoe/cl_common_helper=>sc_config_param-enable_trace ).

    rv_is_enable = COND #( WHEN lv_is_enable = 'X' THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD get_url_from_rfc.

    CLEAR: et_bapiret2,
           ev_server,
           ev_proxy_host,
           ev_proxy_service,
           ev_http_version.

    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = iv_rfc_name
        authority_check         = abap_false
        bypass_buf              = abap_false
      IMPORTING
        server                  = ev_server
        proxy_host              = ev_proxy_host
        proxy_service           = ev_proxy_service
        http_version            = ev_http_version
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.

    IF sy-subrc <> 0.
      /vpcoe/cl_common_helper=>add_symsg_to_bapiret( CHANGING ct_bapiret2 = et_bapiret2 ).
    ENDIF.

  ENDMETHOD.


  method GET_VERSION_TEXT.
    rv_version = |RDP Integration Accelerator { gc_vp_version }|.
  endmethod.


  METHOD read_change_pointers.
    DATA: lt_change_pointers        TYPE TABLE OF bdcp,
          lt_change_pointers_sorted TYPE SORTED TABLE OF bdcp WITH NON-UNIQUE KEY acttime cdobjid,
          lt_r_objid                TYPE RANGE OF cdobjectv,
          lt_r_objid_del            TYPE RANGE OF cdobjectv.

    CLEAR: et_r_objid,
           et_r_objid_del,
           et_cpi.

    CALL FUNCTION 'CHANGE_POINTERS_READ'
      EXPORTING
        message_type           = iv_chng_pointer_id
      TABLES
        change_pointers        = lt_change_pointers
      EXCEPTIONS
        error_in_date_interval = 1
        error_in_time_interval = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lt_change_pointers_sorted = lt_change_pointers.

    LOOP AT lt_change_pointers_sorted ASSIGNING FIELD-SYMBOL(<ls_chng_point>).
      DATA(lv_proceed) = abap_false.

      IF it_r_create_date IS INITIAL AND it_r_change_date IS INITIAL .
        lv_proceed = abap_true.
      ELSE.
        IF it_r_create_date IS NOT INITIAL AND <ls_chng_point>-cdchgid = 'I' AND <ls_chng_point>-acttime(8) IN it_r_create_date.
          lv_proceed = abap_true.
        ENDIF.
        IF it_r_change_date IS NOT INITIAL AND <ls_chng_point>-cdchgid <> 'I' AND <ls_chng_point>-acttime(8) IN it_r_change_date.
          lv_proceed = abap_true.
        ENDIF.
      ENDIF.

      CHECK lv_proceed = abap_true.

      IF <ls_chng_point>-cdchgid = 'D'.
        INSERT VALUE #( sign    = 'I'
                        option = 'EQ'
                        low    = <ls_chng_point>-cdobjid ) INTO TABLE lt_r_objid_del.
      ELSE.
        INSERT VALUE #( sign   = 'I'
                        option = 'EQ'
                        low    = <ls_chng_point>-cdobjid ) INTO TABLE lt_r_objid.
      ENDIF.
      INSERT VALUE #( cpident = <ls_chng_point>-cpident ) INTO TABLE et_cpi.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_r_objid.
    DELETE ADJACENT DUPLICATES FROM lt_r_objid_del.

    et_r_objid = lt_r_objid.
    et_r_objid_del = lt_r_objid_del.

  ENDMETHOD.


  METHOD serialize_json.

*  zcl_json=>serialize(
    /ui2/cl_json=>serialize(
      EXPORTING
        data             = is_data
        compress         = iv_compress
        name             = iv_name
        pretty_name      = iv_pretty_name
        type_descr       = io_type_descr
        assoc_arrays     = iv_assoc_arrays
        ts_as_iso8601    = iv_ts_as_iso8601
        expand_includes  = iv_expand_includes
        numc_as_string   = iv_numc_as_string
        name_mappings    = it_name_mappings
      RECEIVING
        r_json           = rv_json ).

  ENDMETHOD.


  METHOD set_mode.

    rv_mode = COND #( WHEN iv_screen = abap_true
                              THEN /vpcoe/cl_common_helper=>sc_mode-screen
                                ELSE COND #( WHEN iv_excel = abap_true
                                               THEN /vpcoe/cl_common_helper=>sc_mode-document
                                                 ELSE /vpcoe/cl_common_helper=>sc_mode-send ) ).

  ENDMETHOD.
ENDCLASS.
