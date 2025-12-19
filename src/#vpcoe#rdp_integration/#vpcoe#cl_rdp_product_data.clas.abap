CLASS /vpcoe/cl_rdp_product_data DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gty_s_prod_family,
        country    TYPE land1,
        valid_from TYPE dats,
        prod_code  TYPE char10,
        descr_fr   TYPE string,
        descr_en   TYPE string,
      END OF gty_s_prod_family .
    TYPES:
      gty_t_prod_family TYPE STANDARD TABLE OF gty_s_prod_family .
    TYPES:
      BEGIN OF gty_s_sel_opt,
        date            TYPE RANGE OF mara-laeda,
        grp             TYPE RANGE OF matkl,
        matnr           TYPE RANGE OF matnr,
        type            TYPE RANGE OF mtart,
        product_content TYPE RANGE OF /vpcoe/de_product_content,
        type_harmon     TYPE RANGE OF /vpcoe/de_product_type_harmon,
        valid_from      TYPE RANGE OF /vpcoe/de_valid_from,
      END OF gty_s_sel_opt .
    TYPES:
      gty_r_date     TYPE RANGE OF mara-laeda .
    TYPES:
      gty_r_grp    TYPE RANGE OF matkl .
    TYPES:
      gty_r_matnr    TYPE RANGE OF matnr .
    TYPES:
      gty_r_vkorg    TYPE RANGE OF mvke-vkorg .
    TYPES:
      gty_r_product_content     TYPE RANGE OF /vpcoe/de_product_content .
    TYPES:
      gty_r_product_type_harmon TYPE RANGE OF /vpcoe/de_product_type_harmon .
    TYPES:
      gty_r_type    TYPE RANGE OF mtart .
    TYPES:
      gty_r_valid_from          TYPE RANGE OF /vpcoe/de_valid_from .
    TYPES:
      BEGIN OF gty_prd_ext_sel_opt,
        delta_only          TYPE abap_bool,
        matnr	              TYPE gty_r_matnr,
        type                TYPE gty_r_type,
        grp	                TYPE gty_r_grp,
        chng_date	          TYPE gty_r_date,
        vkorg               TYPE gty_r_vkorg,
        werks               TYPE /vpcoe/tt_r_werks,
        mstae               TYPE /vpcoe/tt_r_mstae,
        valid_from          TYPE gty_r_valid_from,
        product_type_harmon	TYPE gty_r_product_type_harmon,
        product_content	    TYPE gty_r_product_content,
      END OF gty_prd_ext_sel_opt .
    TYPES gty_s_prd_json_descr TYPE /vpcoe/str_prd_json_descr .
    TYPES gty_s_prd_json_sales TYPE /vpcoe/str_prd_json_sales .
    TYPES gty_s_prd_json_uom TYPE /vpcoe/str_prd_json_uom .
    TYPES:
      BEGIN OF gty_s_product,
        id                      TYPE matnr,
        type                    TYPE mtart,
        is_marked_for_deletion  TYPE boolean,
        base_unit_of_measure    TYPE meins,
        division                TYPE spart,
        product_group           TYPE matkl,
        product_hierarchy       TYPE mara-prdha,
        net_weight              TYPE ntgew,
        gross_weight            TYPE brgew,
        weight_unit_of_measure  TYPE gewei,
        volume                  TYPE volum,
        volume_unit_of_measure  TYPE voleh,
        status_plant            TYPE mstae,
        status_plant_date       TYPE dats,
        name                    TYPE string,
        is_procurement_relevant TYPE beskz,
        brand                   TYPE wrf_brand_id,
      END OF gty_s_product .

    TYPES:
      BEGIN OF gty_s_product_commodity_code,
        product_id              TYPE matnr,
        trd_classfctn_nmbr_schm TYPE char2,
        validity_start_date     TYPE char10,
        country                 TYPE land1,
        validity_end_date       TYPE char10,
        commodity_code          TYPE stawn,
      END OF gty_s_product_commodity_code .
    TYPES:
      BEGIN OF gty_s_product_descr,
        product_id  TYPE matnr,
        language    TYPE laiso,
        description TYPE maktx,
      END OF gty_s_product_descr .
    TYPES gty_s_product_json TYPE /vpcoe/str_product_json .
    TYPES:
      BEGIN OF gty_s_product_plants,
        product_id TYPE matnr,
        plant_id   TYPE werks_d,
      END OF gty_s_product_plants .
    TYPES:
      BEGIN OF gty_s_product_sales,
        product_id           TYPE matnr,
        sales_organization   TYPE vkorg,
        distribution_channel TYPE vtweg,
        sales_measure_unit   TYPE vrkme,
      END OF gty_s_product_sales .
    TYPES:
      BEGIN OF gty_s_product_uom,
        product_id             TYPE matnr,
        measurement_unit       TYPE meinh,
        numerator              TYPE umrez,
        denominator	           TYPE umren,
        gross_weight           TYPE brgew,
        net_weight             TYPE ntgew,
        weight_unit_of_measure TYPE gewei,
*        product_measurement_unit TYPE  meabm,
*        base_unit                TYPE  meins,
      END OF gty_s_product_uom .
    TYPES:
      BEGIN OF gty_s_quantity_dimensions,
        product_id                 TYPE matnr,
        unit_of_measure            TYPE meinh,
        dimension_name             TYPE /vpcoe/de_dimension_name,
        dimension_quantity_content TYPE brgew,
        dimension_quantity_uom     TYPE gewei,
      END OF gty_s_quantity_dimensions .
    TYPES:
      BEGIN OF gty_s_quantity_conversions,
        product_id                     TYPE matnr,
        quantity_unit_of_measure       TYPE meins,
        quantity_content               TYPE umrez,
        corresponding_quantity_u_of_m  TYPE meinh,
        corresponding_quantity_content TYPE umren,
      END OF gty_s_quantity_conversions.

    TYPES:
      BEGIN OF  gty_s_global_trade_item_num,
        product_id               TYPE matnr,
        unit_of_measure          TYPE meinh,
        global_trade_item_number TYPE ean11,
        is_main                  TYPE char5,
      END OF  gty_s_global_trade_item_num.

    TYPES:
      gty_t_cpi TYPE STANDARD TABLE OF bdicpident .
    TYPES:
      gty_t_product TYPE SORTED TABLE OF gty_s_product WITH NON-UNIQUE KEY id .
    TYPES:
      gty_t_product_comb_json TYPE SORTED TABLE OF /vpcoe/str_product_comb_json WITH NON-UNIQUE KEY id .
    TYPES:
      gty_t_product_commodity_code           TYPE SORTED TABLE OF gty_s_product_commodity_code WITH NON-UNIQUE KEY country commodity_code .
    TYPES:
      gty_t_product_json TYPE SORTED TABLE OF gty_s_product_json .
    TYPES:
      gty_t_product_plants           TYPE SORTED TABLE OF gty_s_product_plants WITH NON-UNIQUE KEY plant_id .
    TYPES:
      gty_t_product_sales           TYPE SORTED TABLE OF gty_s_product_sales WITH NON-UNIQUE KEY product_id sales_organization distribution_channel .
    TYPES:
      gty_t_quantity_conversions TYPE STANDARD TABLE OF gty_s_quantity_conversions WITH NON-UNIQUE KEY product_id.
    TYPES:
      gty_t_global_trade_item_num TYPE STANDARD TABLE OF gty_s_global_trade_item_num WITH NON-UNIQUE KEY product_id.
    TYPES:
     gty_t_quantity_dimensions TYPE STANDARD TABLE OF gty_s_quantity_dimensions WITH NON-UNIQUE KEY product_id.

    TYPES:
      gty_t_product_sales_json           TYPE SORTED TABLE OF gty_s_prd_json_sales WITH NON-UNIQUE KEY sales_organization .
    TYPES:
      gty_t_product_unit_of_measure TYPE SORTED TABLE OF gty_s_product_uom WITH NON-UNIQUE KEY product_id measurement_unit .
    TYPES:
      gty_t_product_uom_json TYPE SORTED TABLE OF gty_s_prd_json_uom WITH NON-UNIQUE KEY measurement_unit .
    TYPES:
      gty_t_texts TYPE SORTED TABLE OF gty_s_product_descr WITH NON-UNIQUE KEY product_id language .
    TYPES:
      gty_t_texts_json       TYPE SORTED TABLE OF gty_s_prd_json_descr WITH NON-UNIQUE KEY language .
    TYPES gty_s_product_combined_base TYPE /vpcoe/str_product_combined .
    TYPES:
      BEGIN OF gty_s_product_combined,
        name                    TYPE char30,
        is_procurement_relevant TYPE char5,
        brand                   TYPE wrf_brand_id,
        plants                  type gty_t_product_plants ,
        quantity_dimensions_sm  TYPE gty_t_quantity_dimensions,
        quantity_conversions    TYPE gty_t_quantity_conversions,
        global_trade_item_num   TYPE gty_t_global_trade_item_num.
    INCLUDE TYPE /vpcoe/str_product_combined AS base.
    TYPES: END OF gty_s_product_combined.

    TYPES:
     gty_t_product_combined TYPE SORTED TABLE OF gty_s_product_combined WITH NON-UNIQUE KEY id .
    TYPES:
      BEGIN OF gty_s_product_tables,
        product             TYPE gty_t_product,
        product_ext         TYPE /vpcoe/tt_product_ext,
        product_sales       TYPE gty_t_product_sales,
        product_uom         TYPE gty_t_product_unit_of_measure,
        product_description TYPE gty_t_texts,
        product_country     TYPE /vpcoe/tt_product_countries,
      END OF gty_s_product_tables .
    TYPES:
      BEGIN OF gty_s_excel_ext,
        type_id          TYPE domvalue_l,
        type_description TYPE string,
        content_id       TYPE domvalue_l,
        cont_description TYPE string,
      END OF gty_s_excel_ext .
    TYPES:
      gty_t_excel_ext TYPE STANDARD TABLE OF  gty_s_excel_ext .

    METHODS change_status
      IMPORTING
        !iv_test_run TYPE xfeld .
    METHODS constructor
      IMPORTING
        !iv_package_size TYPE /vpcoe/de_package_size
        !iv_source       TYPE string
        !iv_api_type     TYPE /vpcoe/de_api_type
        !iv_mode         TYPE /vpcoe/de_mode DEFAULT /vpcoe/cl_common_helper=>sc_mode-send
        !io_log          TYPE REF TO /vpcoe/cl_rdp_log OPTIONAL .
    METHODS download_excel
      IMPORTING
        !is_product_tables  TYPE gty_s_product_tables
        !iv_service_id      TYPE /vpcoe/de_service_id DEFAULT /vpcoe/cl_common_helper=>sc_service_id-product
        !iv_file_path       TYPE string
        !iv_save_background TYPE abap_bool .
    METHODS get_log
      RETURNING
        VALUE(ro_log) TYPE REF TO /vpcoe/cl_rdp_log .
    METHODS get_product
      IMPORTING
        !is_sel_opt        TYPE /vpcoe/s_selopt_product
        !iv_gen            TYPE abap_bool OPTIONAL
        !iv_send           TYPE abap_bool DEFAULT abap_true
      EXPORTING
        !et_product        TYPE gty_t_product_combined
        !es_product_tables TYPE gty_s_product_tables .
    METHODS get_product_delta
      IMPORTING
        !iv_gen             TYPE abap_bool OPTIONAL
        !is_sel_opt         TYPE /vpcoe/s_selopt_product
        !iv_chng_pointer_id TYPE edi_mestyp
      EXPORTING
        !et_product         TYPE gty_t_product_combined
        !et_json            TYPE /vpcoe/cl_rdp_http=>gty_t_json
        !es_product_tables  TYPE gty_s_product_tables .
    METHODS get_product_ext
      IMPORTING
        !is_sel_opt        TYPE gty_prd_ext_sel_opt
        !it_product        TYPE gty_t_product_combined
        !io_log            TYPE REF TO /vpcoe/cl_rdp_log
      EXPORTING
        !et_product_ext    TYPE /vpcoe/tt_product_ext
      CHANGING
        !cs_product_tables TYPE gty_s_product_tables .
protected section.

  data MO_LOG type ref to /VPCOE/CL_RDP_LOG .
  data MT_CPI type /VPCOE/CL_RDP_PRODUCT_DATA=>GTY_T_CPI .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_CHNG_POINTER_ID type EDI_MESTYP .
  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SOURCE type STRING .
  data MV_MODE type /VPCOE/DE_MODE value 1 ##NO_TEXT.

  methods BUILD_JSONS
    importing
      !IT_PRODUCT_COMBINED type GTY_T_PRODUCT_COMBINED
    exporting
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_COM_CODE
    importing
      !IT_PRODUCT type GTY_T_PRODUCT
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
    exporting
      !ET_COMMODITY_CODE type GTY_T_PRODUCT_COMMODITY_CODE .
  methods GET_DESCRIPTION
    importing
      !IT_PRODUCT type GTY_T_PRODUCT
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
    exporting
      !ET_DESCRIPTION type GTY_T_TEXTS .
  methods GET_EPR_PRODUCT_FAMILY
    exporting
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_TABLE type GTY_T_PROD_FAMILY .
  methods GET_EXCEL_HEADER
    importing
      !IV_SHEET_NAME type CHAR40
      !IS_PRODUCT_TABLES type GTY_S_PRODUCT_TABLES
      !IV_DOMEN_NAME type DOMNAME optional
    exporting
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_DATA type ref to DATA .
  methods GET_EXCEL_SHEET
    importing
      !IV_SERVICE_ID type /VPCOE/DE_SERVICE_ID
    exporting
      !ET_SHEETS type /VPCOE/TT_SHEETS_STR .
  methods GET_HEADER
    importing
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
    exporting
      !ET_PRODUCT type GTY_T_PRODUCT .
  methods GET_HEADER_EXT
    importing
      !IV_DOMEN_NAME_CONTENT type DOMNAME
      !IV_DOMEN_NAME_TYPE type DOMNAME
    exporting
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE
      !ET_DATA type GTY_T_EXCEL_EXT .
  methods GET_PLANTS
    importing
      !IT_PRODUCT type GTY_T_PRODUCT
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
    exporting
      !ET_PLANTS type GTY_T_PRODUCT_PLANTS .
  methods GET_SALES
    importing
      !IT_PRODUCT type GTY_T_PRODUCT
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
    exporting
      !ET_SALES type GTY_T_PRODUCT_SALES .
  methods GET_UOM
    importing
      !IT_PRODUCT type GTY_T_PRODUCT
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
    exporting
      !ET_UOM type GTY_T_PRODUCT_UNIT_OF_MEASURE .
  methods RETRIEVE_AND_PROCESS_FURTHER
    importing
      !IV_GEN type ABAP_BOOL optional
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
      !IT_PRODUCT type GTY_T_PRODUCT
      !IV_SEND type ABAP_BOOL default ABAP_TRUE
    changing
      !CT_PRODUCT type GTY_T_PRODUCT_COMBINED
      !CS_PRODUCT_TABLES type GTY_S_PRODUCT_TABLES .
private section.
ENDCLASS.



CLASS /VPCOE/CL_RDP_PRODUCT_DATA IMPLEMENTATION.


  METHOD build_jsons.
    DATA:  lo_badi             TYPE REF TO /vpcoe/adjust_data_retrieval,
*           lt_product_pack TYPE gty_t_product_combined,
           lt_product_pack     TYPE gty_t_product_comb_json,
           lt_product_combined TYPE gty_t_product_comb_json,
           ls_data             TYPE gty_s_product_json.

    lt_product_combined = CORRESPONDING #( it_product_combined ).

*    LOOP AT it_product_combined ASSIGNING FIELD-SYMBOL(<ls_product_combined>).
    LOOP AT lt_product_combined ASSIGNING FIELD-SYMBOL(<ls_product_combined>).
      INSERT <ls_product_combined> INTO TABLE lt_product_pack.

      IF lines( lt_product_pack ) = me->mv_package_size AND me->mv_package_size IS NOT INITIAL.
        APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
        ls_data-source   = me->mv_source.
        ls_data-elements = lt_product_pack.
        DATA(lo_writer_json) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
        CALL TRANSFORMATION /vpcoe/rdp_product_to_json SOURCE root = ls_data RESULT XML lo_writer_json.
        <ls_json>-elements = cl_abap_codepage=>convert_from( lo_writer_json->get_output( ) ).
        <ls_json>-count = lines( lt_product_pack ).

        CLEAR: ls_data, lt_product_pack.
      ENDIF.

    ENDLOOP.

    IF lt_product_pack IS NOT INITIAL.
      APPEND INITIAL LINE TO et_json ASSIGNING <ls_json>.
      lo_writer_json = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
      ls_data-source   = me->mv_source.
      ls_data-elements = lt_product_pack.
      CALL TRANSFORMATION /vpcoe/rdp_product_to_json SOURCE root = ls_data RESULT XML lo_writer_json.
      <ls_json>-elements = cl_abap_codepage=>convert_from( lo_writer_json->get_output( ) ).
      <ls_json>-count = lines( lt_product_pack ).
    ENDIF.

    LOOP AT et_json ASSIGNING <ls_json>.
      REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
      REPLACE ALL OCCURRENCES OF ':"false"' IN <ls_json>-elements WITH ': false' ##no_text.
      REPLACE ALL OCCURRENCES OF ':"true"' IN <ls_json>-elements WITH ': true' ##no_text.
    ENDLOOP.

    GET BADI lo_badi.
    CALL BADI lo_badi->adjust_json
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        io_log      = me->mo_log
      CHANGING
        ct_json     = et_json.

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


  METHOD constructor.
    me->mv_api_type     = iv_api_type.
    me->mv_package_size = iv_package_size.
    me->mv_source       = iv_source.
    me->mv_mode         = iv_mode.
    IF io_log IS INITIAL.
      me->mo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-product ).
    ELSE.
      me->mo_log = io_log.
    ENDIF.

  ENDMETHOD.


  METHOD download_excel.
    DATA: lr_table  TYPE REF TO data,
          lt_header TYPE /vpcoe/cl_xls_handler=>gty_t_title.
    FIELD-SYMBOLS: <lt_data> TYPE any.

    get_excel_sheet(
      EXPORTING
        iv_service_id = iv_service_id
      IMPORTING
        et_sheets     = DATA(lt_sheets) ).

    DATA(lo_xls_file) = NEW /vpcoe/cl_xls_handler( iv_path = iv_file_path
                                                   iv_save_background = iv_save_background ).

    LOOP AT lt_sheets ASSIGNING FIELD-SYMBOL(<ls_sheets>).

      IF <ls_sheets> = 'Code lists EPR product family'.
        get_epr_product_family( IMPORTING et_table  = DATA(lt_data)
                                          et_header = DATA(lt_header_code) ).

        lo_xls_file->execute( iv_name = CONV #( <ls_sheets> ) ).
        lo_xls_file->set_column( iv_index = 5 iv_width = 200 ).
        lo_xls_file->set_column( iv_index = 6 iv_width = 200 ).
        lo_xls_file->add_comment( iv_text         = 'Citéo 2021'
                                  iv_start_row    = 2
                                  iv_start_column = 2 ).
        lo_xls_file->add_comment( iv_text         = 'French'
                                  iv_start_row    = 3
                                  iv_start_column = 5 ).
        lo_xls_file->add_comment( iv_text         = 'English'
                                  iv_start_row    = 3
                                  iv_start_column = 6
                                  iv_skip_row     = abap_true ).
        lo_xls_file->fill_data( it_item_tab         = lt_data
                                it_title            = lt_header_code
                                iv_additional_table = abap_true
                                iv_start_column     = 2
                                iv_start_row        = 4 ).
      ELSEIF <ls_sheets> = 'Additional Code lists'.
        lo_xls_file->execute( iv_name = CONV #( <ls_sheets> ) ).

        get_header_ext( EXPORTING iv_domen_name_content = '/VPCOE/PRODUCT_CONTENT'
                                  iv_domen_name_type    = '/VPCOE/PRODUCT_TYPE_HARMON'
                        IMPORTING et_header = lt_header
                                  et_data   = DATA(lt_table) ).
        IF lt_table IS NOT INITIAL.
          lo_xls_file->fill_prod_ext( EXPORTING iv_code_list_content = 'Product Content'
                                                iv_code_list_type    = 'Product Type harmonized'
                                                it_title             = lt_header
                                                it_item_tab          = lt_table
                                                iv_start_column = 2
                                                iv_start_row    = 3 ).
        ENDIF.
        CONTINUE.
      ELSE.
        get_excel_header(
        EXPORTING
          iv_sheet_name     =  <ls_sheets>
          is_product_tables =  is_product_tables
        IMPORTING
          et_header         = lt_header
          et_data           = lr_table
      ).
        ASSIGN lr_table->* TO <lt_data>.

        lo_xls_file->execute( iv_name = CONV #( <ls_sheets> ) ).
        lo_xls_file->fill_data( it_item_tab = <lt_data>
                                it_title    = lt_header ).

      ENDIF.
    ENDLOOP.
    lo_xls_file->save_xls_file( io_log = mo_log ).

  ENDMETHOD.


  METHOD get_com_code.

    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    DATA: lv_skip           TYPE abap_bool,
          lv_commodity_code TYPE marc-stawn,
          lv_idx            TYPE sy-index,
          ls_sel_opt        TYPE gty_s_sel_opt,
          lt_commodity_code	TYPE STANDARD TABLE OF gty_s_product_commodity_code.

    CLEAR et_commodity_code.

    IF it_product IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_product ASSIGNING FIELD-SYMBOL(<ls_prdct>).
      ls_sel_opt-matnr = VALUE #( BASE ls_sel_opt-matnr ( low    = <ls_prdct>-id
                                                          option = 'EQ'
                                                          sign   = 'I' ) ).
    ENDLOOP.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-uom_code
        is_sel_opt  = ls_sel_opt-matnr
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT DISTINCT marc~matnr   AS product_id,
                      marc~stawn   AS commodity_code,
                      t604~land1   AS country
         INTO CORRESPONDING FIELDS OF TABLE @lt_commodity_code
           FROM marc LEFT JOIN t604 ON marc~stawn = t604~stawn
             FOR ALL ENTRIES IN @it_product
                WHERE marc~matnr = @it_product-id
                  AND marc~stawn <> ''.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      LOOP AT lt_commodity_code ASSIGNING FIELD-SYMBOL(<ls_commodity_code>).
        CLEAR lv_commodity_code.
        DO strlen( <ls_commodity_code>-commodity_code ) TIMES.
          lv_idx = sy-index - 1.
          IF <ls_commodity_code>-commodity_code+lv_idx(1) CA '0123456789'.
            lv_commodity_code = lv_commodity_code &&  <ls_commodity_code>-commodity_code+lv_idx(1).
            IF strlen( lv_commodity_code ) = 6.
              EXIT.
            ENDIF.
          ENDIF.
        ENDDO.
        IF lv_commodity_code IS NOT INITIAL.
          <ls_commodity_code>-commodity_code = lv_commodity_code.
        ENDIF.
        <ls_commodity_code>-trd_classfctn_nmbr_schm = 'HS'.
        <ls_commodity_code>-validity_start_date = '2000-01-01'.
        <ls_commodity_code>-validity_end_date = '9999-01-01'.
      ENDLOOP.

      INSERT LINES OF lt_commodity_code INTO TABLE et_commodity_code.
      DELETE ADJACENT DUPLICATES FROM et_commodity_code COMPARING ALL FIELDS.
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-uom_code
        is_sel_opt  = ls_sel_opt-matnr
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_commodity_code.
  ENDMETHOD.


  METHOD get_description.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.
    DATA: lv_skip    TYPE abap_bool.

    CLEAR et_description.

    IF it_product IS INITIAL.
      RETURN.
    ENDIF.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-dscr
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT matnr AS product_id,
             spras AS language,
             maktx AS description
         INTO CORRESPONDING FIELDS OF TABLE @et_description
           FROM makt
             FOR ALL ENTRIES IN @it_product
                WHERE matnr = @it_product-id.

      IF sy-subrc <> 0.
        CLEAR et_description.
      ENDIF.
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-dscr
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_description.

  ENDMETHOD.


  METHOD get_epr_product_family.

    CLEAR: et_table, et_header.

    SELECT *
       FROM /vpcoe/code_list
         INTO CORRESPONDING FIELDS OF TABLE @et_table.

    IF sy-subrc <> 0.
      CLEAR et_table.
    ENDIF.

    et_header = VALUE #( ( description = 'Country' vpcoe_attribute = 'COUNTRY' )
                         ( description = 'Valid From' vpcoe_attribute = 'VALID_FROM' )
                         ( description = 'Product Code' vpcoe_attribute = 'PROD_CODE' )
                         ( description = 'Désignation Code Produit' vpcoe_attribute = 'DESCR_FR' )
                         ( description = 'Description Product Code for FR - Citeo' vpcoe_attribute = 'DESCR_EN' ) ).

  ENDMETHOD.


  METHOD get_excel_header.

    FIELD-SYMBOLS <lt_data> TYPE any.

    CLEAR: et_header,
           et_data.

    CASE iv_sheet_name.
      WHEN 'ProductExtension'.
        CREATE DATA et_data LIKE is_product_tables-product_ext.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = is_product_tables-product_ext.
        et_header = VALUE #(
                         ( description      = 'Product Number'
                           internal_name    = 'id'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATNR'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Reference Quantity'
                           internal_name    = 'referenceQuantity'
                           data_type        = 'DEC(7,0)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'REFERENCE_QUANTITY' )
                         ( description      = 'Product Type harmonized'
                           internal_name    = 'productTypeHarmonized'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'PRODUCT_TYPE_HARMONIZED' )
                         ( description      = 'Product Content'
                           internal_name    = 'productContent'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'PRODUCT_CONTENT')
                         ( description      = 'Brand ID'
                           internal_name    = 'brand'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'BRAND')
                         ( description      = 'Hazardous Product CLPEU'
                           internal_name    = 'ishazardousCLPEU'
                           data_type        = 'CHAR(5) boolean'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'IS_HAZARDOUS_CLP_EU')
*                         ( description      = 'Dangerous Product EU'
*                           internal_name    = 'isDgProductEu'
*                           data_type        = 'CHAR(5) boolean'
*                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
*                           s4hana_attribute = '[customer specific]'
*                           vpcoe_attribute  = 'IS_DG_PRODUCT_EU')
                         ( description      = 'Product comes without packaging'
                           internal_name    = 'hasNoPackaging'
                           data_type        = 'CHAR(5) boolean'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'HAS_NO_PACKAGING')
                           ).
      WHEN 'ProductCountry'.
        CREATE DATA et_data LIKE is_product_tables-product_country.
        ASSIGN et_data->* TO <lt_data>.
        DATA: lt_countries TYPE /vpcoe/tt_product_countries.
        LOOP AT is_product_tables-product_ext ASSIGNING FIELD-SYMBOL(<ls_product_ext>).
          lt_countries = CORRESPONDING #( BASE ( lt_countries ) <ls_product_ext>-product_countries ).
        ENDLOOP.
        <lt_data>   = lt_countries.
        et_header = VALUE #(
                         ( description      = 'Product Number'
                           internal_name    = 'productId'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATNR'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Country/Region'
                           internal_name    = 'country'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = '[LAND1]'
                           vpcoe_attribute  = 'COUNTRY'
                           is_key           = abap_true )
                         ( description      = 'Valid From Date'
                           internal_name    = 'validFrom'
                           data_type        = 'Date'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'VALID_FROM'
                           is_key           = abap_true )
                         ( description      = 'Product Tag'
                           internal_name    = 'tag'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'TAG' )
                         ( description      = 'Exclude from EPR report'
                           internal_name    = 'isNotEprRelevant'
                           data_type        = 'CHAR(5) boolean'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'IS_NOT_EPR_RELEVANT')
                         ( description      = 'EPR Product Family'
                           internal_name    = 'eprProductFamily'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[PRO specific]'
                           vpcoe_attribute  = 'EPR_PRODUCT_FAMILY')
                          ( description      = 'inhouse Production Percent'
                           internal_name    = 'inhouseProductionPercent'
                           data_type        = 'DEC(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[PRO specific]'
                           vpcoe_attribute  = 'INHOUSE_PRODUCTION_PERCENT')
                          ( description      = 'Deposit '
                           internal_name    = 'hasDeposit'
                           data_type        = 'CHAR(5) boolean'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = '[customer specific]'
                           vpcoe_attribute  = 'HAS_DEPOSIT') ).
      WHEN 'ProductDescription'.
        CREATE DATA et_data LIKE is_product_tables-product_description.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = is_product_tables-product_description.
        et_header = VALUE #(
                         ( description      = 'Product Number'
                           internal_name    = 'productId'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATNR'
                           vpcoe_attribute  = 'PRODUCT_ID'
                           is_key           = abap_true )
                         ( description      = 'ISO Language Code'
                           internal_name    = 'language'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'SPRAS'
                           vpcoe_attribute  = 'LANGUAGE'
                           is_key           = abap_true )
                         ( description      = 'Description'
                           internal_name    = 'description'
                           data_type        = 'CHAR(255)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MAKTX'
                           vpcoe_attribute  = 'DESCRIPTION' ) ).
      WHEN 'ProductUoM'.
        CREATE DATA et_data LIKE is_product_tables-product_uom.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = is_product_tables-product_uom.
        et_header = VALUE #(
                         ( description      = 'Product Number'
                           internal_name    = 'productId'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATNR'
                           vpcoe_attribute  = 'PRODUCT_ID'
                           is_key           = abap_true )
                         ( description      = 'Alternative Unit of Measure'
                           internal_name    = 'measurementUnit'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MEINH'
                           vpcoe_attribute  = 'MEASUREMENT_UNIT'
                           is_key           = abap_true )
                         ( description      = 'Quantity Numerator'
                           internal_name    = 'numerator'
                           data_type        = 'DEC(5,0)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'UMREZ'
                           vpcoe_attribute  = 'NUMERATOR' )
                         ( description      = 'Quantity Denominator'
                           internal_name    = 'denominator'
                           data_type        = 'DEC(5,0)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'UMREN'
                           vpcoe_attribute  = 'DENOMINATOR' ) ).
      WHEN 'ProductSales'.
        CREATE DATA et_data LIKE is_product_tables-product_sales.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = is_product_tables-product_sales.
        et_header = VALUE #(
                         ( description      = 'Product Number'
                           internal_name    = 'productId'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATNR'
                           vpcoe_attribute  = 'PRODUCT_ID'
                           is_key           = abap_true )
                         ( description      = 'Sales Organization'
                           internal_name    = 'salesOrganization'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'VKORG'
                           vpcoe_attribute  = 'SALES_ORGANIZATION'
                           is_key           = abap_true )
                         ( description      = 'Distribution Channel'
                           internal_name    = 'distributionChannel'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'VTWEG'
                           vpcoe_attribute  = 'DISTRIBUTION_CHANNEL'
                           is_key           = abap_true )
                         ( description      = 'Sales Unit of Measure'
                           internal_name    = 'salesMeasureUnit'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'VRKME'
                           vpcoe_attribute  = 'SALES_MEASURE_UNIT' ) ).
      WHEN 'Product'.
        CREATE DATA et_data LIKE is_product_tables-product.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = is_product_tables-product.
        et_header = VALUE #(
                         ( description      = 'Product Number'
                           internal_name    = 'id'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATNR'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Product Type'
                           internal_name    = 'type'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MTART'
                           vpcoe_attribute  = 'TYPE' )
                         ( description      = 'Base Unit of Measure'
                           internal_name    = 'baseUnitOfMeasure'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MEINS'
                           vpcoe_attribute  = 'BASE_UNIT_OF_MEASURE' )
                         ( description      = 'Product Division'
                           internal_name    = 'division'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'SPART'
                           vpcoe_attribute  = 'DIVISION')
                         ( description      = 'Product Group'
                           internal_name    = 'productGroup'
                           data_type        = 'CHAR(9)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATLK'
                           vpcoe_attribute  = 'PRODUCT_GROUP')
                         ( description      = 'Product Hierachy'
                           internal_name    = 'productHierarchy'
                           data_type        = 'CHAR(18)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'PRDHA'
                           vpcoe_attribute  = 'product_group')
                         ( description      = 'Product is marked for deletion'
                           internal_name    = 'isMarkedForDeletion'
                           data_type        = 'CHAR(5)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'LVORM'
                           vpcoe_attribute  = 'IS_MARKED_FOR_DELETION')
                         ( description      = 'Gross Weight'
                           internal_name    = 'grossWeight'
                           data_type        = 'DEC(13,3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'BRGEW'
                           vpcoe_attribute  = 'GROSS_WEIGHT')
                         ( description      = 'Net Weight'
                           internal_name    = 'netWeight'
                           data_type        = 'DEC(13,3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'NTGEW'
                           vpcoe_attribute  = 'NET_WEIGHT')
                         ( description      = 'Weight Unit Of Measure'
                           internal_name    = 'weightUnitOfMeasure'
                           data_type        = 'CHAR(128)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'GEWEI'
                           vpcoe_attribute  = 'WEIGHT_UNIT_OF_MEASURE') ).
      WHEN 'Additional Code lists'.

        SELECT domvalue_l AS id,
                      ddtext AS description
             FROM dd07v
             WHERE domname = @iv_domen_name
           INTO TABLE @DATA(lt_data).
        CREATE DATA et_data LIKE lt_data.
        ASSIGN et_data->* TO <lt_data>.
        <lt_data> = lt_data.
        et_header = VALUE #(
                         ( description      = 'ID'
                           vpcoe_attribute  = 'ID' )
                         ( description      = 'Description'
                           vpcoe_attribute  = 'DESCRIPTION' ) ).

    ENDCASE.
  ENDMETHOD.


  METHOD get_excel_sheet.

    CLEAR: et_sheets.

    CASE iv_service_id.
      WHEN /vpcoe/cl_common_helper=>sc_service_id-product.
        et_sheets = VALUE #( ( 'ProductDescription' ) ( 'ProductUoM' ) ( 'ProductSales' ) ( 'Product' ) ).
      WHEN /vpcoe/cl_common_helper=>sc_service_id-product_ext.
        et_sheets = VALUE #( ( 'ProductExtension' ) ( 'ProductCountry' ) ( 'Code lists EPR product family' ) ( 'Additional Code lists' )    ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_header.

    DATA: lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval,
          lv_skip TYPE abap_bool.

    CLEAR: et_product.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-hdr
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.

      /vpcoe/cl_common_helper=>get_material_type( IMPORTING et_r_material_type = DATA(lr_mat_type) ).

      SELECT DISTINCT mara~matnr AS id,
                      mara~mtart AS type,
                      mara~spart AS division,
                      mara~meins AS base_unit_of_measure,
                      mara~prdha AS product_hierarchy,
                      mara~matkl AS product_group,
                      CASE WHEN mara~lvorm = 'X' THEN 'X'
                                                 ELSE ' ' END AS is_marked_for_deletion,
                      mara~volum AS volume,
                      mara~voleh AS volume_unit_of_measure,
                      mara~mstae AS status_plant,
                      mara~mstde AS status_plant_date
        INTO CORRESPONDING FIELDS OF TABLE @et_product
          FROM mara LEFT JOIN mvke ON mara~matnr = mvke~matnr
                    LEFT JOIN marc ON marc~matnr = mara~matnr
              WHERE mara~matnr IN @is_sel_opt-matnr
              AND mara~mtart IN @is_sel_opt-type
              AND mara~matkl IN @is_sel_opt-grp
              AND ( mara~laeda IN @is_sel_opt-chng_date OR mara~ersda IN @is_sel_opt-chng_date )
              AND ( mara~mtart IN @lr_mat_type OR mara~mtart NOT IN @lr_mat_type AND mvke~vkorg IS NOT NULL )
              AND ( ( mara~mstae IN @is_sel_opt-mstae AND mara~mstde IN @is_sel_opt-mstde ) )
              AND mara~datab IN @is_sel_opt-datab
              AND marc~werks IN @is_sel_opt-werks
              AND mvke~vkorg IN @is_sel_opt-vkorg.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_level    = /vpcoe/cl_common_helper=>sc_level-hdr
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_product.
  ENDMETHOD.


  METHOD get_header_ext.

    SELECT type~domvalue_l AS type_id,
           type~ddtext AS type_description,
           cont~domvalue_l AS content_id,
           cont~ddtext AS cont_description
     FROM dd07v AS cont
            LEFT JOIN  dd07v AS type ON type~valpos = cont~valpos AND type~domname = @iv_domen_name_type
       WHERE cont~domname = @iv_domen_name_content
         ORDER BY cont~valpos
           INTO CORRESPONDING FIELDS OF TABLE @et_data.

    IF sy-subrc <> 0.
      CLEAR et_data.
    ENDIF.

    et_header = VALUE #( ( description      = 'ID'
                           vpcoe_attribute  = 'CONTENT_ID' )
                         ( description      = 'Description'
                           vpcoe_attribute  = 'CONT_DESCRIPTION' )
                         ( description      = 'ID'
                           vpcoe_attribute  = 'TYPE_ID' )
                         ( description      = 'Description'
                           vpcoe_attribute  = 'TYPE_DESCRIPTION' )  ).

  ENDMETHOD.


  METHOD get_log.

    ro_log = me->mo_log.

  ENDMETHOD.


  METHOD get_plants.

    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.
    DATA: lv_skip TYPE abap_bool.

    CLEAR et_plants.

    IF it_product IS INITIAL.
      RETURN.
    ENDIF.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-plants
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT matnr AS product_id,
             werks AS plant_id
         INTO CORRESPONDING FIELDS OF TABLE @et_plants
           FROM mard
             FOR ALL ENTRIES IN @it_product
                WHERE matnr = @it_product-id.

      IF sy-subrc <> 0.
        CLEAR et_plants.
      ENDIF.
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-plants
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_plants.

  ENDMETHOD.


  METHOD get_product.
    DATA: lo_badi         TYPE REF TO /vpcoe/adjust_data_retrieval,
          lt_product_pack TYPE gty_t_product.

    CLEAR: et_product,
           es_product_tables.

    me->get_header(
      EXPORTING
        is_sel_opt = is_sel_opt
      IMPORTING
        et_product = DATA(lt_product) ).

    "don't select if lt_product empty
    IF lt_product IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_product ASSIGNING FIELD-SYMBOL(<ls_product>).
      INSERT <ls_product> INTO TABLE lt_product_pack.
      IF lines( lt_product_pack ) = me->mv_package_size AND me->mv_package_size IS NOT INITIAL.
        DATA(lv_progress) = ( sy-tabix * 100 ) DIV lines( lt_product ).
        me->mo_log->add_msg_progress( EXPORTING iv_progress = lv_progress iv_level = conv #( 'Product' ) ).

        me->retrieve_and_process_further( EXPORTING is_sel_opt = is_sel_opt
                                                    it_product = lt_product_pack
                                                    iv_send    = iv_send
                                                    iv_gen     = iv_gen
                                          CHANGING ct_product        = et_product
                                                   cs_product_tables = es_product_tables ).

        IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-screen.
          IF lines( et_product ) > /vpcoe/cl_rdp_helper=>gc_display_amount.
            RETURN.
          ENDIF.
        ENDIF.

        CLEAR lt_product_pack.
      ENDIF.
    ENDLOOP.

    IF lt_product_pack IS NOT INITIAL.
      me->mo_log->add_msg_progress( EXPORTING iv_progress = 100 iv_level = conv #( 'Product' ) ).
      me->retrieve_and_process_further( EXPORTING is_sel_opt = is_sel_opt
                                                  it_product = lt_product_pack
                                                  iv_send    = iv_send
                                                  iv_gen     = iv_gen
                                        CHANGING ct_product        = et_product
                                                 cs_product_tables = es_product_tables ).

      CLEAR lt_product_pack.
    ENDIF.

  ENDMETHOD.


  METHOD get_product_delta.
    DATA: lt_r_product_id   TYPE gty_r_matnr,
          lt_product_pack   TYPE gty_t_product,
          lt_product        TYPE gty_t_product,
          lt_json	          TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          ls_product_tables	TYPE gty_s_product_tables,
          ls_sel_opt        TYPE /vpcoe/s_selopt_product.

    CLEAR: et_product,
           et_json,
           es_product_tables.

    mv_chng_pointer_id = iv_chng_pointer_id.

    /vpcoe/cl_common_helper=>read_change_pointers(
        EXPORTING
          iv_chng_pointer_id = mv_chng_pointer_id
          it_r_change_date   = is_sel_opt-chng_date
        IMPORTING
           et_r_objid        = lt_r_product_id
           et_cpi            = mt_cpi ).

    IF lt_r_product_id IS INITIAL.
      RETURN.
    ENDIF.

    ls_sel_opt = is_sel_opt.
    "For Delta Mode Create/Change date shouldn't be taken into consideration for 'regular' select
    CLEAR: ls_sel_opt-chng_date,
           ls_sel_opt-matnr.

    LOOP AT lt_r_product_id ASSIGNING FIELD-SYMBOL(<ls_r_product_id>).
      IF <ls_r_product_id>-low NOT IN is_sel_opt-matnr.
        CONTINUE.
      ENDIF.
      INSERT <ls_r_product_id> INTO TABLE ls_sel_opt-matnr.

      IF lines( ls_sel_opt-matnr ) = 500.
        me->get_header(
          EXPORTING
            is_sel_opt = ls_sel_opt
          IMPORTING
            et_product = DATA(lt_product_tmp) ).

        INSERT LINES OF lt_product_tmp INTO TABLE lt_product.

        CLEAR ls_sel_opt-matnr.
      ENDIF.

    ENDLOOP.

    IF ls_sel_opt-matnr IS NOT INITIAL.
      me->get_header(
        EXPORTING
          is_sel_opt = ls_sel_opt
        IMPORTING
          et_product = lt_product_tmp ).

      INSERT LINES OF lt_product_tmp INTO TABLE lt_product.
    ENDIF.

    LOOP AT lt_product ASSIGNING FIELD-SYMBOL(<ls_product>).
      INSERT <ls_product> INTO TABLE lt_product_pack.

      IF lines( lt_product_pack ) = me->mv_package_size AND me->mv_package_size IS NOT INITIAL.
        me->retrieve_and_process_further( EXPORTING is_sel_opt = is_sel_opt
                                                    it_product = lt_product_pack
                                                    iv_gen     = iv_gen
                                          CHANGING ct_product        = et_product
                                                   cs_product_tables = es_product_tables ).

        CLEAR lt_product_pack.
      ENDIF.
    ENDLOOP.

    IF lt_product_pack IS NOT INITIAL.
      me->retrieve_and_process_further( EXPORTING is_sel_opt = is_sel_opt
                                                  it_product = lt_product_pack
                                                  iv_gen     = iv_gen
                                        CHANGING ct_product        = et_product
                                                 cs_product_tables = es_product_tables ).

    ENDIF.

    IF NOT me->mo_log->check( ).
      me->change_status( EXPORTING iv_test_run = COND #( WHEN me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send THEN abap_false
                                                                                                               ELSE abap_true ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_product_ext.

    DATA: lo_badi             TYPE REF TO /vpcoe/adjust_data_retrieval,
          lt_product_ext      TYPE /vpcoe/tt_product_ext,
          lt_product_pack_ext TYPE /vpcoe/tt_product_ext,
          lt_json             TYPE  /vpcoe/cl_rdp_http=>gty_t_json.

    CLEAR: et_product_ext.

    DATA(lo_cust_ext) = NEW /vpcoe/cl_rdp_helper( iv_api_type = me->mv_api_type
                                                  iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-product
                                                  iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-product_ext ).

    GET BADI lo_badi.

    CALL BADI lo_badi->get_ext_data
      EXPORTING
        iv_api_type = me->mv_api_type
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product_ext
        is_sel_opt  = is_sel_opt
        it_data     = it_product
      CHANGING
        ct_ext_data = lt_product_ext.

    IF lt_product_ext IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_product_ext ASSIGNING FIELD-SYMBOL(<ls_product_ext>).
      INSERT <ls_product_ext> INTO TABLE lt_product_pack_ext.

      IF lines( lt_product_pack_ext ) = mv_package_size AND mv_package_size IS NOT INITIAL.

        IF mv_mode = /vpcoe/cl_common_helper=>sc_mode-send.

          APPEND INITIAL LINE TO lt_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          <ls_json>-elements = /vpcoe/cl_common_helper=>serialize_json( EXPORTING is_data = VALUE /vpcoe/str_product_ext_json( source   = mv_source
                                                                                                                               elements = lt_product_pack_ext )
                                                                                  iv_pretty_name    = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                                                                  iv_numc_as_string = abap_true ).
          <ls_json>-count = lines( lt_product_pack_ext ).

          CALL BADI lo_badi->adjust_json
            EXPORTING
              iv_api_type = me->mv_api_type
              iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
              iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product_ext
            CHANGING
              ct_json     = lt_json.

          NEW /vpcoe/cl_payload_handler( )->send_payload(
            EXPORTING
              io_cust     = lo_cust_ext
              it_json     = lt_json
              io_log      = io_log
              iv_save_log = abap_false ).
          CLEAR lt_json.

        ELSE.
          INSERT LINES OF lt_product_pack_ext INTO TABLE et_product_ext.
        ENDIF.

        CLEAR lt_product_pack_ext.
      ENDIF.

    ENDLOOP.

    IF lt_product_pack_ext IS NOT INITIAL.
      IF mv_mode = /vpcoe/cl_common_helper=>sc_mode-send.
        APPEND INITIAL LINE TO lt_json ASSIGNING <ls_json>.
        <ls_json>-elements = /vpcoe/cl_common_helper=>serialize_json(
                                              EXPORTING
                                                is_data = VALUE /vpcoe/str_product_ext_json( source   = mv_source
                                                                                             elements = lt_product_pack_ext )
                                                iv_pretty_name    = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                                iv_numc_as_string = abap_true ).
        <ls_json>-count = lines( lt_product_pack_ext ).

        CALL BADI lo_badi->adjust_json
          EXPORTING
            iv_api_type = me->mv_api_type
            iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
            iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product_ext
          CHANGING
            ct_json     = lt_json.

        NEW /vpcoe/cl_payload_handler( )->send_payload(
          EXPORTING
            io_cust     = lo_cust_ext
            it_json     = lt_json
            io_log      = io_log
            iv_save_log = abap_false ).
      ELSE.
        INSERT LINES OF lt_product_pack_ext INTO TABLE et_product_ext.
      ENDIF.
    ENDIF.

    IF mv_mode = /vpcoe/cl_common_helper=>sc_mode-document.
      cs_product_tables-product_ext = et_product_ext.
    ENDIF.

  ENDMETHOD.


  METHOD get_sales.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.
    DATA: lv_skip TYPE abap_bool.

    CLEAR et_sales.

    IF it_product IS INITIAL.
      RETURN.
    ENDIF.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-sales
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT matnr AS product_id,
             vkorg AS sales_organization,
             vtweg AS distribution_channel,
             vrkme AS sales_measure_unit
         INTO TABLE @et_sales
           FROM mvke
             FOR ALL ENTRIES IN @it_product
                WHERE matnr = @it_product-id.
      IF sy-subrc <> 0.
        CLEAR et_sales.
      ENDIF.
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-sales
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_sales.

  ENDMETHOD.


  METHOD get_uom.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.
    DATA: lv_skip TYPE abap_bool.

    CLEAR et_uom.

    IF it_product IS INITIAL.
      RETURN.
    ENDIF.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-uom
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT marm~matnr AS product_id,
             marm~meinh AS measurement_unit,
             marm~umrez AS numerator,
             marm~umren AS denominator,
             mara~ntgew as net_weight,
             mara~brgew AS gross_weight,
             mara~gewei AS weight_unit_of_measure
         INTO CORRESPONDING FIELDS OF TABLE @et_uom
          FROM marm JOIN mara ON marm~matnr = mara~matnr
             FOR ALL ENTRIES IN @it_product
                WHERE marm~matnr = @it_product-id.

      IF sy-subrc <> 0.
        CLEAR et_uom.
      ENDIF.
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-uom
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_uom.

  ENDMETHOD.


 METHOD retrieve_and_process_further.
   DATA: lo_badi             TYPE REF TO /vpcoe/adjust_data_retrieval,
         lt_product_combined TYPE gty_t_product_combined,
         lt_json             TYPE  /vpcoe/cl_rdp_http=>gty_t_json,
         lr_data_combined    TYPE REF TO gty_s_product_tables,
         ls_product_combined TYPE gty_s_product_combined.

   me->get_description(
     EXPORTING
       it_product     = it_product
       is_sel_opt     = is_sel_opt
     IMPORTING
       et_description = DATA(lt_description) ).

   me->get_uom(
     EXPORTING
       it_product = it_product
       is_sel_opt = is_sel_opt
     IMPORTING
       et_uom     = DATA(lt_uom) ).

   me->get_sales(
     EXPORTING
       is_sel_opt = is_sel_opt
       it_product = it_product
     IMPORTING
       et_sales   = DATA(lt_sales) ).

   GET BADI lo_badi.

   IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-document.
     INSERT LINES OF it_product INTO TABLE cs_product_tables-product.
     INSERT LINES OF lt_sales INTO TABLE cs_product_tables-product_sales.
     INSERT LINES OF lt_uom INTO TABLE cs_product_tables-product_uom.
     INSERT LINES OF VALUE gty_t_texts( FOR <ls_data> IN lt_description ( product_id = <ls_data>-product_id
                                                                          description = <ls_data>-description
                                                                          language = /vpcoe/cl_rdp_helper=>convert_langu_code( CONV #( <ls_data>-language ) ) ) )
                                 INTO TABLE cs_product_tables-product_description.
   ENDIF.

   lr_data_combined = NEW #( ).
   lr_data_combined->product             = it_product.
   lr_data_combined->product_description = lt_description.
   lr_data_combined->product_sales       = lt_sales.
   lr_data_combined->product_uom         = lt_uom.

   LOOP AT it_product ASSIGNING FIELD-SYMBOL(<ls_product>).
     ls_product_combined =  VALUE #(
         id                       = <ls_product>-id
         type                     = <ls_product>-type
         is_marked_for_deletion   = COND #( WHEN <ls_product>-is_marked_for_deletion = abap_true THEN 'true'
                                                                                                 ELSE 'false' ) ##NO_TEXT
         base_unit_of_measure     = <ls_product>-base_unit_of_measure
         division                 = <ls_product>-division
         product_group            = <ls_product>-product_group
         product_hierarchy        = <ls_product>-product_hierarchy
         status_plant             = <ls_product>-status_plant
         status_plant_date        = <ls_product>-status_plant_date

         texts                    = VALUE #( FOR <ls_f_text> IN lt_description WHERE ( product_id = <ls_product>-id )
                                                ( language    = /vpcoe/cl_common_helper=>convert_langu_code( CONV #( <ls_f_text>-language ) )
                                                  name = <ls_f_text>-description ) )

         quantity_dimensions = VALUE #( FOR <ls_f_uom> IN lt_uom WHERE ( product_id = <ls_product>-id )
                                                ( measurement_unit        = <ls_f_uom>-measurement_unit
                                                  net_weight              = <ls_f_uom>-net_weight
                                                  gross_weight            = <ls_f_uom>-gross_weight
                                                  weight_measurement_unit = <ls_f_uom>-weight_unit_of_measure  ) )

         product_unit_of_measures = VALUE #( FOR <ls_f_uom> IN lt_uom WHERE ( product_id = <ls_product>-id )
                                                ( measurement_unit             = <ls_f_uom>-measurement_unit
                                                  numerator                    = <ls_f_uom>-numerator
                                                  denominator                  = <ls_f_uom>-denominator  ) )
         product_sales            = VALUE #( FOR <ls_f_sales> IN lt_sales WHERE ( product_id = <ls_product>-id )
                                                     ( sales_organization   = <ls_f_sales>-sales_organization
                                                       distribution_channel = <ls_f_sales>-distribution_channel
                                                       sales_measure_unit   = <ls_f_sales>-sales_measure_unit ) )
                                                        ) .

     CALL BADI lo_badi->adjust_mapping
       EXPORTING
         iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-product
         iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-product
         is_data     = lr_data_combined
         iv_api_type = me->mv_api_type
       CHANGING
         cs_data     = ls_product_combined.

     INSERT ls_product_combined INTO TABLE lt_product_combined.

   ENDLOOP.

   IF me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-send AND iv_send = abap_true AND iv_gen = abap_true.
     me->build_jsons( EXPORTING it_product_combined = lt_product_combined
                      IMPORTING et_json             = lt_json ).

     NEW /vpcoe/cl_rdp_payload_handler( )->send_payload(
       EXPORTING
         io_cust     = NEW /vpcoe/cl_rdp_helper( iv_api_type = me->mv_api_type
                                                 iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-product
                                                 iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-product )
         it_json     = lt_json
         io_log      = me->mo_log
         iv_save_log = abap_false ).
   ENDIF.

   INSERT LINES OF lt_product_combined INTO TABLE ct_product.

 ENDMETHOD.
ENDCLASS.
