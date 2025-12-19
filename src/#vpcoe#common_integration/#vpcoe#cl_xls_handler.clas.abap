class /VPCOE/CL_XLS_HANDLER definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_tax_header,
        description TYPE string,
      END OF gty_s_tax_header .
  types:
    BEGIN OF gty_s_title,
        description      TYPE char40,
        internal_name    TYPE char40,
        data_type        TYPE char40,
        mandatory        TYPE char40,
        s4hana_attribute TYPE char40,
        vpcoe_attribute  TYPE char40,
        is_key           TYPE flag,
      END OF   gty_s_title .
  types:
    gty_t_title TYPE STANDARD TABLE OF gty_s_title .
  types:
    BEGIN OF gty_s_codelist,
        id          TYPE char10,
        description TYPE string,
      END OF gty_s_codelist .
  types:
    gty_t_codelist TYPE STANDARD TABLE OF gty_s_codelist .

  constants:
    BEGIN OF sc_styles,
        usual_style      TYPE string VALUE 'UsualStyle',
        key_style        TYPE string VALUE 'Key',
        mandatory_style  TYPE string VALUE 'Mandatory',
        optional_style   TYPE string VALUE 'Optional',
        no_style         TYPE string VALUE 'NoStyle',
        attribute_style  TYPE string VALUE 'Attribute',
        code_list_style  TYPE string VALUE 'CodeList',
        tax_hdr_style    TYPE string VALUE 'TaxNumHdr',
        tax_title_style  TYPE string VALUE 'TaxTitle',
        tax_number_style TYPE string VALUE 'TaxNumber',
        title_style      TYPE string VALUE 'Title',
      END OF sc_styles .
  constants:
    BEGIN OF sc_mandatory,
        mandatory TYPE char30 VALUE 'mandatory',
        optional  TYPE char30 VALUE 'optional',
      END OF sc_mandatory .
  constants:
    BEGIN OF sc_attributes,
        description    TYPE string VALUE 'Description',
        remarks        TYPE string VALUE 'Your Remarks',
        internal_name  TYPE string VALUE 'Internal Name',
        data_type      TYPE string VALUE 'Data Type',
        mandatory      TYPE string VALUE 'Mandatory',
        sap_attribute  TYPE string VALUE 'SAP S/4 HANA Attribute',
        part_data_load TYPE string VALUE '(not part of the data load)',
        attribute      TYPE string VALUE 'Attribute',
        formula        TYPE string VALUE 'FORMULA',
        explanation    TYPE string VALUE 'Explanation',
        type_id        TYPE string VALUE 'TYPE_ID',
        string        TYPE string VALUE 'String',
      END OF sc_attributes .
  constants:
    BEGIN OF sc_type,
                 type_bool TYPE string VALUE '\TYPE=BOOLEAN',
               END OF sc_type .
  data MT_RETURN type BAPIRET2_TAB .
  data MV_TAX_TITLE type STRING .
  data MV_CODE_LIST type STRING .
  data MS_TAX_HEADER type GTY_S_TAX_HEADER .
  data:
    mt_tax_header TYPE TABLE OF gty_s_tax_header .
  data:
    mt_tax_number TYPE TABLE OF gty_s_tax_header .

  methods CONSTRUCTOR
    importing
      !IV_SAVE_BACKGROUND type ABAP_BOOL
      !IV_PATH type STRING .
  methods CREATE_RANGE_TABLE
    importing
      !IV_NAME type STRING
      !IV_FORMULA type STRING .
  methods EXECUTE
    importing
      !IV_NAME type STRING
      !IV_CODE_LIST type STRING optional
      !IV_START_ROW type I optional
      !IV_START_COLUMN type I optional
      !IV_TAX_NUMBER type STRING optional
      !IT_ITEM_TAB type ANY TABLE optional
      !IT_TITLE type ANY TABLE optional
    returning
      value(RT_BAPIRET2) type BAPIRET2_T .
  methods FILL_DATA
    importing
      !IV_CODE_LIST type STRING optional
      !IV_TAX_NUMBER type STRING optional
      !IV_CODE_LIST_CATEGORY type STRING optional
      !IV_START_ROW type I optional
      !IV_START_COLUMN type I optional
      !IT_ITEM_TAB type ANY TABLE optional
      !IT_TITLE type ANY TABLE optional
      !IV_ADDITIONAL_TABLE type ABAP_BOOL optional
      !IV_STYLE type STRING optional
    returning
      value(RT_BAPIRET2) type BAPIRET2_T .
  methods FILL_PROD_EXT
    importing
      !IV_CODE_LIST_CONTENT type STRING
      !IV_CODE_LIST_TYPE type STRING
      !IV_START_ROW type I optional
      !IV_START_COLUMN type I optional
      !IT_ITEM_TAB type ANY TABLE optional
      !IT_TITLE type ANY TABLE optional
    returning
      value(RT_BAPIRET2) type BAPIRET2_T .
  methods SAVE_XLS_FILE
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional .
  methods SET_COLUMN
    importing
      !IV_INDEX type I
      !IV_WIDTH type I .
  methods ADD_COMMENT
    importing
      !IV_TEXT type STRING
      !IV_START_COLUMN type I
      !IV_START_ROW type I
      !IV_SKIP_ROW type ABAP_BOOL optional .
  PROTECTED SECTION.
private section.

  types GTY_DREF type ref to DATA .
  types:
    gty_xmlline TYPE x LENGTH 255 .
  types:
    gty_t_xmlline TYPE STANDARD TABLE OF gty_xmlline .
  types:
    gty_t_dref TYPE TABLE OF gty_dref .
  types:
    BEGIN OF gty_s_total,
      fieldname TYPE fieldname,
      value     TYPE REF TO data,
    END OF gty_s_total .

  data MV_CURRENT_COLUMN type I value 1 ##NO_TEXT.
  data MV_START_COLUMN type I .
  data MV_START_ROW type I .
  data MV_NAME type STRING .
  data MS_HEADER_LIST type GTY_S_CODELIST .
  data:
    mt_code_list TYPE TABLE OF gty_s_codelist .
  data MT_TITLE type GTY_T_TITLE .
  data:
    mt_attr TYPE TABLE OF char40 .
  data MS_XLS_HEADER type GTY_DREF .
  data MT_DFIES_HEADER type DFIES_TAB .
  data MT_XLS_ITEM type GTY_DREF .
  data MS_XLS_ITEM type GTY_DREF .
  data MT_DFIES_ITEM type DFIES_TAB .
  data MV_HEADER_CDLIST type STRING .
  data MV_PATH type STRING .
  data MV_SAVE_BACKGROUND type ABAP_BOOL .
  data MV_WORKBOOK type STRING .
  data MV_WORKSHEET type STRING .
  data MV_AUTHOR type STRING .
  data MV_COMPANY type STRING .
  data MV_XLS_FILENAME type STRING .
  data MV_CURRENT_ROW type I value 1 ##NO_TEXT.
  constants GC_PATH type STRING value 'C:\TEMP\' ##NO_TEXT.
  constants GC_FILE_EXTENSION type STRING value 'xlsx' ##NO_TEXT.
  constants GC_FILE_FILTER type STRING value '*.xlsx' ##NO_TEXT.
  data MS_XLS_BIN type SOLIX .
  data MT_XLS_BIN type SOLIX_TAB .
  data XML_SIZE type I .
  data XML_TABLE type GTY_T_XMLLINE .
  data MO_IXML type ref to IF_IXML .
  data MO_STREAMFACTORY type ref to IF_IXML_STREAM_FACTORY .
  data MO_OSTREAM type ref to IF_IXML_OSTREAM .
  data MO_RENDERER type ref to IF_IXML_RENDERER .
  data MO_DOCUMENT type ref to IF_IXML_DOCUMENT .
  data MO_ELEMENT_ROOT type ref to IF_IXML_ELEMENT .
  data MO_WORKSHEET type ref to IF_IXML_ELEMENT .
  data MO_STYLE type ref to IF_IXML_ELEMENT .
  data MO_STYLES type ref to IF_IXML_ELEMENT .
  data MO_BORDER type ref to IF_IXML_ELEMENT .
  data MO_TABLE type ref to IF_IXML_ELEMENT .
  data MO_ROW type ref to IF_IXML_ELEMENT .
  data MO_CELL type ref to IF_IXML_ELEMENT .
  data MO_COLUMN type ref to IF_IXML_ELEMENT .
  data MO_RANGE_NAMES type ref to IF_IXML_ELEMENT .
  data MO_RANGE_NAME type ref to IF_IXML_ELEMENT .

  methods ADD_BORDER_TO_STYLE .
  methods ADD_CELL
    importing
      !IV_STYLE type STRING
      !IV_FORMULA type STRING optional .
  methods ADD_ROW .
  methods SET_POSITION
    importing
      !IV_START_COLUMN type I optional
      !IV_START_ROW type I optional .
  methods ADD_TABLE .
  methods ADD_WORKSHEET
    importing
      !IV_WORKSHEET type STRING .
  methods CREATE_DOCUMENT .
  methods CREATE_STYLE
    importing
      !IV_STYLE type STRING .
  methods CREATE_STYLES .
  methods CREATE_XLS_FILE .
  methods CREATE_XML_DOCUMENT
    importing
      !IT_ITEM_TAB type ANY TABLE optional .
  methods DDIF_FIELDINFO_GET .
  methods DOCUMENT_PROPERTIES .
  methods FILL_DATA_CELL
    importing
      !IV_CELL_NAME type STRING
      !IV_DATATYPE type STRING .
  methods FILL_HEADER_ROWS
    importing
      !IT_ITEM_TAB type ANY TABLE optional
      !IV_ADDITIONAL_TABLE type ABAP_BOOL optional
      !IV_STYLE type STRING optional .
  methods FILL_ITEM_ROWS
    importing
      !IV_ADDITIONAL_TABLE type ABAP_BOOL optional .
  methods FORMAT_ALIGNMENT
    importing
      !IV_H_ALIGN type STRING optional
      !IV_V_ALIGN type STRING optional
      !IV_WRAPTEXT type STRING optional .
  methods FORMAT_BORDER
    importing
      !IV_POSITION type STRING optional
      !IV_LINESTYLE type STRING optional
      !IV_WEIGHT type STRING optional
      !IV_COLOR type STRING optional .
  methods FORMAT_FONT
    importing
      !IV_FONTNAME type STRING optional
      !IV_SIZE type STRING optional
      !IV_BOLD type STRING optional
      !IV_ITALIC type STRING optional
      !IV_UNDERLINE type STRING optional
      !IV_COLOR type STRING optional .
  methods FORMAT_HEIGHT_ROW
    importing
      !IV_AUTOFITHEIGHT type STRING .
  methods FORMAT_INTERIOR
    importing
      !IV_COLOR type STRING optional
      !IV_PATTERN type STRING optional .
  methods FORMAT_NUMBER
    importing
      !IV_NUMBERFORMAT type STRING .
  methods FORMAT_WIDTH_COLUMN .
  methods GET_DATA_REFERENCE
    importing
      !IT_ITEM_TAB type ANY TABLE .
  methods MERGE_CELL
    importing
      !IV_STYLE type STRING
      !IV_MERGE_VALUE type STRING .
  methods MERGE_DOWN_CELL
    importing
      !IV_STYLE type STRING
      !IV_MERGE_VALUE type STRING .
  methods RENDER_XML_DOCUMENT .
  methods SET_GRIDLINES .
ENDCLASS.



CLASS /VPCOE/CL_XLS_HANDLER IMPLEMENTATION.


  METHOD add_border_to_style.
    CLEAR me->mo_border.
    me->mo_border = mo_document->create_simple_element( name = 'Borders' parent = me->mo_style ).
  ENDMETHOD.


  METHOD add_cell.
    CLEAR me->mo_cell.
    DATA(lv_current_column) = CONV string( me->mv_current_column ).
    CONDENSE lv_current_column NO-GAPS.
    me->mo_cell = me->mo_document->create_simple_element( name = 'Cell' parent = me->mo_row )."Cell
    me->mo_cell->set_attribute_ns( name = 'StyleID' prefix = 'ss' value = iv_style ).

    IF iv_formula IS NOT INITIAL.
      me->mo_cell->set_attribute_ns( name = 'Formula' prefix = 'ss' value = iv_formula ).
    ENDIF.

    me->mo_cell->set_attribute_ns( name = 'Index' prefix = 'ss' value = lv_current_column ).
    ADD 1 TO me->mv_current_column.

  ENDMETHOD.


  METHOD add_comment.
    me->set_position( iv_start_column = iv_start_column iv_start_row = iv_start_row ).

    IF iv_skip_row <> abap_true.
      me->add_row( ).
      me->format_height_row( iv_autofitheight = '1' ).
    ENDIF.

    me->add_cell( iv_style = 'UsualStyle' ).
    me->fill_data_cell( iv_cell_name = CONV #( iv_text ) iv_datatype = 'String' ).
  ENDMETHOD.


  METHOD add_row.
    CLEAR: me->mo_row.
    DATA(lv_current_row) = CONV string( me->mv_current_row ).
    CONDENSE lv_current_row NO-GAPS.
    mv_current_column = mv_start_column.
    me->mo_row = me->mo_document->create_simple_element( name = 'Row' parent = me->mo_table ).

    me->mo_row->set_attribute_ns( name = 'Index' prefix = 'ss' value = lv_current_row ).
    ADD 1 TO me->mv_current_row.
  ENDMETHOD.


  METHOD add_table.
    CLEAR me->mo_table.
    me->mo_table = mo_document->create_simple_element( name = 'Table' parent = me->mo_worksheet ).
    me->mo_table->set_attribute_ns( name = 'FullColumns' prefix = 'x' value = '1' ).
    me->mo_table->set_attribute_ns( name = 'FullRows' prefix = 'x' value = '1' ).
  ENDMETHOD.


  METHOD add_worksheet.
    me->mo_worksheet = me->mo_document->create_simple_element( name = 'Worksheet' parent = me->mo_element_root ).
    me->mo_worksheet->set_attribute_ns( name = 'Name' prefix = 'ss' value = iv_worksheet ).
  ENDMETHOD.


  METHOD constructor.
    me->mt_attr = VALUE #( ( 'Description' )
                     ( 'Internal Name' )
                     ( 'Data Type' )
                     ( 'Mandatory' )
                     ( 'SAP S/4 HANA Attribute' ) ).

    me->mv_save_background = iv_save_background.
    me->mv_path = iv_path.

    me->create_document( ).
    me->document_properties( ).
    me->create_styles( ).
  ENDMETHOD.


  METHOD create_document.
    CLEAR: me->mo_ixml, me->mo_document, me->mo_element_root.

    me->mo_ixml = cl_ixml=>create( ).

    me->mo_document = me->mo_ixml->create_document( ).

    me->mo_element_root = me->mo_document->create_simple_element( name = 'Workbook' parent = me->mo_document ).
    me->mo_element_root->set_attribute( name = 'xmlns' value = 'urn:schemas-microsoft-com:office:spreadsheet' ).

    DATA(ns_attribute) = me->mo_document->create_namespace_decl( name = 'ss' prefix = 'xmlns' uri = 'urn:schemas-microsoft-com:office:spreadsheet ' ).
    me->mo_element_root->set_attribute_node( ns_attribute ).

    ns_attribute = me->mo_document->create_namespace_decl( name = 'x' prefix = 'xmlns' uri = 'urn:schemas-microsoft-com:office:excel' ).
    me->mo_element_root->set_attribute_node( ns_attribute ).
  ENDMETHOD.


  METHOD create_range_table.
    CLEAR: mo_range_names, mo_range_name.
    mo_range_names = mo_document->create_simple_element( name = 'Names' parent = me->mo_worksheet ).
    mo_range_name = mo_document->create_simple_element( name = 'NamedRange' parent = mo_range_names ).
    mo_range_name->set_attribute_ns( name = 'Name' prefix = 'ss' value = iv_name ).
    mo_range_name->set_attribute_ns( name = 'RefersTo' prefix = 'ss' value = iv_formula ).
    mo_range_name->set_attribute_ns( name = 'Hidden' prefix = 'ss' value = '1' ).
  ENDMETHOD.


  METHOD create_style.
    CHECK iv_style IS NOT INITIAL.
    CLEAR: me->mo_style.
    me->mo_style = me->mo_document->create_simple_element( name = 'Style' parent = me->mo_styles ).
    me->mo_style->set_attribute_ns( name = 'ID' prefix = 'ss' value = iv_style ).
  ENDMETHOD.


  METHOD create_styles.

    format_width_column( ).

    me->mo_styles = me->mo_document->create_simple_element( name = 'Styles' parent = me->mo_element_root ).

    me->create_style( iv_style = sc_styles-usual_style ).
    me->format_font( iv_fontname = 'Calibri' iv_italic = '1' iv_size = '11' ).
    me->format_width_column( ).

    me->create_style( iv_style = sc_styles-key_style ).
    me->format_font( iv_bold = '1' ).
    me->format_interior( iv_color = '#ED7D31' iv_pattern = 'Solid' ).
    me->add_border_to_style( ).
    me->format_border( iv_position = 'Top' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Left' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Right' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Bottom' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_width_column( ).

    me->create_style( iv_style = sc_styles-mandatory_style ).
    me->format_font( iv_bold = '1' ).
    me->format_interior( iv_color = '#A5A5A5' iv_pattern = 'Solid' ).
    me->add_border_to_style( ).
    me->format_border( iv_position = 'Top' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Left' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Right' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Bottom' iv_linestyle = 'Continuous' iv_weight = '1' ).
    format_width_column( ).
    me->format_width_column( ).

    me->create_style( iv_style = sc_styles-optional_style ).
    me->format_font( iv_bold = '1' ).
    me->format_interior( iv_color = '#E7E6E6' iv_pattern = 'Solid' ).
    me->add_border_to_style( ).
    me->format_border( iv_position = 'Top' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Left' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Right' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Bottom' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_width_column( ).

    me->create_style( iv_style = sc_styles-no_style ).

    me->create_style( iv_style = sc_styles-title_style ).
    me->format_font( iv_bold = '1' ).
    me->format_alignment( iv_h_align = 'Center' iv_v_align = 'Center' iv_wraptext = '1' ).
    me->format_width_column( ).

    me->create_style( iv_style = sc_styles-attribute_style ).
    me->format_number( iv_numberformat = 'Short Date' ).
    me->format_font( iv_bold = '1' ).
    me->format_interior( iv_color = '#5B9BD5' iv_pattern = 'Solid' ).
    me->add_border_to_style( ).
    me->format_border( iv_position = 'Top' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Left' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Right' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Bottom' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_width_column( ).

    me->create_style( iv_style = sc_styles-code_list_style ).
    me->format_font( iv_bold = '1' ).
    me->format_interior( iv_color = '#BFBFBF' iv_pattern = 'Solid' ).
    me->add_border_to_style( ).
    me->format_border( iv_position = 'Top' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Left' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Right' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Bottom' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_width_column( ).

    me->create_style( iv_style = sc_styles-tax_hdr_style ).
    me->format_font( iv_bold = '1' iv_color = '#FFFFFF' ).
    me->format_interior( iv_color = '#002060' iv_pattern = 'Solid' ).
    me->add_border_to_style( ).
    me->format_border( iv_position = 'Top' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Left' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Right' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Bottom' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_width_column( ).

    me->create_style( iv_style = sc_styles-tax_title_style ).
    me->format_font( iv_bold = '1').
    me->format_interior( iv_color = '#BFBFBF' iv_pattern = 'Solid' ).
    me->add_border_to_style( ).
    me->format_border( iv_position = 'Top' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Left' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Right' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Bottom' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_width_column( ).

    me->create_style( iv_style = sc_styles-tax_number_style ).
    me->format_font( iv_bold = '1' ).
    me->format_interior( iv_color = '#FFF2CC' iv_pattern = 'Solid' ).
    me->add_border_to_style( ).
    me->format_border( iv_position = 'Top' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Left' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Right' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_border( iv_position = 'Bottom' iv_linestyle = 'Continuous' iv_weight = '1' ).
    me->format_width_column( ).

  ENDMETHOD.


  METHOD create_xls_file.
    LOOP AT me->xml_table INTO DATA(wa_xml).
      me->ms_xls_bin-line = wa_xml.
      APPEND me->ms_xls_bin TO me->mt_xls_bin.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_xml_document.
    me->add_worksheet( iv_worksheet = me->mv_name ).
    me->add_table( ).
  ENDMETHOD.


  METHOD ddif_fieldinfo_get.
    IF me->mt_xls_item IS NOT INITIAL.
      FIELD-SYMBOLS: <fs_xls_item> TYPE any.
      ASSIGN me->mt_xls_item->* TO <fs_xls_item>.
      DATA(lv_header_name) = CONV string( cl_abap_typedescr=>describe_by_data( <fs_xls_item> )->absolute_name+6 ).

      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = CONV tabname( lv_header_name )
        TABLES
          dfies_tab      = me->mt_dfies_header
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.
    ENDIF.
  ENDMETHOD.


  METHOD document_properties.
    DATA(lr_element_properties) = me->mo_document->create_simple_element( name = 'DocumentProperties' parent = me->mo_element_root ).
    lr_element_properties->set_attribute( name = 'xmlns' value = 'urn:schemas-microsoft-com:office:office' ).
    me->mo_document->create_simple_element( name = 'Author' value = me->mv_author parent = lr_element_properties ).
  ENDMETHOD.


  METHOD execute.
    me->mv_name = iv_name.

    TRY.

        me->create_xml_document( it_item_tab = it_title ).

      CATCH cx_root INTO DATA(lo_root).
        rt_bapiret2 = me->mt_return.
        APPEND VALUE bapiret2( type    = sy-abcde+4(1)
                               message = lo_root->get_text( )
                             ) TO rt_bapiret2.

        RETURN.
    ENDTRY.

    rt_bapiret2 = me->mt_return.

  ENDMETHOD.


  METHOD fill_data.

    me->set_position( iv_start_column = iv_start_column iv_start_row = iv_start_row ).

    IF iv_code_list IS NOT INITIAL.
      me->mv_code_list = iv_code_list.
    ENDIF.

    IF iv_tax_number IS NOT INITIAL.
      me->mv_tax_title = iv_tax_number.
    ENDIF.

    me->get_data_reference( it_item_tab = it_item_tab ).

    me->fill_header_rows( it_item_tab = it_title iv_additional_table = iv_additional_table iv_style = iv_style ).
    me->fill_item_rows( iv_additional_table = iv_additional_table ).
    me->set_gridlines( ).
  ENDMETHOD.


  METHOD fill_data_cell.
    DATA(lv_aux_str) = iv_cell_name.

    DATA(lr_data) = me->mo_document->create_simple_element( name = 'Data' value = lv_aux_str parent = me->mo_cell ).
    lr_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = iv_datatype ).
  ENDMETHOD.


  METHOD fill_header_rows.
    DATA: lv_style TYPE string,
          lv_merge TYPE i,
          lv_title TYPE string.

    FIELD-SYMBOLS: <ls_header> TYPE gty_s_title.
    me->mt_title = it_item_tab.
    lv_merge = lines( me->mt_title ) - 1.

    IF me->mv_tax_title IS NOT INITIAL.
      lv_style = sc_styles-tax_hdr_style.
      lv_title = me->mv_tax_title.
    ELSEIF me->mv_code_list IS NOT INITIAL.
      lv_style = sc_styles-code_list_style.
      lv_title = me->mv_code_list.
    ENDIF.

    IF iv_additional_table = abap_true .
      me->add_row( ).
      LOOP AT me->mt_title ASSIGNING <ls_header>.
        me->add_cell( iv_style = sc_styles-usual_style ).
        me->fill_data_cell( iv_cell_name = CONV #( <ls_header>-description ) iv_datatype = me->sc_attributes-string ).
      ENDLOOP.
      me->add_row( ).
    ELSEIF me->mv_code_list IS NOT INITIAL OR me->mv_tax_title IS NOT INITIAL.
      me->set_column( iv_index = lines( me->mt_title )  iv_width = 200 ).
      me->add_row( ).
      me->format_height_row( iv_autofitheight = '1' ).

      me->merge_cell( iv_style = lv_style iv_merge_value = CONV #( lv_merge ) ).
      me->fill_data_cell( iv_cell_name = lv_title iv_datatype = me->sc_attributes-string ).
      me->add_row( ).

      LOOP AT me->mt_title ASSIGNING <ls_header>.
        me->add_cell( iv_style = sc_styles-tax_title_style ).
        me->fill_data_cell( iv_cell_name = CONV #( <ls_header>-description ) iv_datatype = me->sc_attributes-string ).
      ENDLOOP.

      me->add_row( ).
    ELSE.
      me->mo_table->set_attribute_ns( name = 'DefaultColumnWidth' prefix = 'ss' value = '150' ).
      LOOP AT me->mt_attr ASSIGNING  FIELD-SYMBOL(<ls_attr>).
        me->add_row( ).
        me->add_cell( iv_style = sc_styles-usual_style ).
        me->fill_data_cell( iv_cell_name = CONV #( <ls_attr> ) iv_datatype = me->sc_attributes-string ).
        LOOP AT me->mt_title INTO DATA(ls_title).
          CASE ls_title-is_key.
            WHEN abap_true.
              lv_style = sc_styles-key_style.
            WHEN abap_false.
              CASE ls_title-mandatory.
                WHEN sc_mandatory-mandatory.
                  lv_style = sc_styles-mandatory_style.
                WHEN sc_mandatory-optional.
                  lv_style = sc_styles-optional_style.
                WHEN OTHERS.
                  lv_style = sc_styles-optional_style.
              ENDCASE.
          ENDCASE.
          CASE <ls_attr>.
            WHEN sc_attributes-description.
              IF ls_title-description  = sc_attributes-remarks.
                me->add_cell( iv_style = sc_styles-usual_style ).
                me->fill_data_cell( iv_cell_name = CONV #( ls_title-description ) iv_datatype = me->sc_attributes-string ).
              ELSE.
                me->add_cell( iv_style = lv_style ).
                me->fill_data_cell( iv_cell_name = CONV #( ls_title-description ) iv_datatype = me->sc_attributes-string ).
              ENDIF.
            WHEN sc_attributes-internal_name.
              me->add_cell( iv_style = lv_style ).
              me->fill_data_cell( iv_cell_name = CONV #( ls_title-internal_name ) iv_datatype = me->sc_attributes-string ).
            WHEN sc_attributes-data_type.
              me->add_cell( iv_style = lv_style ).
              me->fill_data_cell( iv_cell_name = CONV #( ls_title-data_type ) iv_datatype = me->sc_attributes-string ).
            WHEN sc_attributes-mandatory.
              me->add_cell( iv_style = lv_style ).
              me->fill_data_cell( iv_cell_name = CONV #( ls_title-mandatory ) iv_datatype = me->sc_attributes-string ).
            WHEN sc_attributes-sap_attribute.
              IF ls_title-s4hana_attribute = sc_attributes-part_data_load.
                me->add_cell( iv_style = sc_styles-usual_style ).
                me->fill_data_cell( iv_cell_name = CONV #( ls_title-s4hana_attribute ) iv_datatype = me->sc_attributes-string ).
              ELSE.
                lv_style = sc_attributes-attribute.
                me->add_cell( iv_style = lv_style ).
                me->fill_data_cell( iv_cell_name = CONV #( ls_title-s4hana_attribute ) iv_datatype = me->sc_attributes-string ).
              ENDIF.
          ENDCASE.
        ENDLOOP.
        IF <ls_attr> = sc_attributes-description.
          add_cell( iv_style = sc_styles-usual_style ).
          fill_data_cell( iv_cell_name = sc_attributes-remarks iv_datatype = me->sc_attributes-string ).
        ELSEIF <ls_attr> = sc_attributes-sap_attribute.
          add_cell( iv_style = sc_styles-usual_style ).
          fill_data_cell( iv_cell_name = sc_attributes-part_data_load iv_datatype = me->sc_attributes-string ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD fill_item_rows.
    DATA: lv_date_out(10),
          lv_num_out      TYPE char20,
          lv_yy           TYPE numc4,
          lv_mm(2)        TYPE n,
          lv_dd(2)        TYPE n.

    FIELD-SYMBOLS: <fs_xls_item> TYPE ANY TABLE.
    ASSIGN me->mt_xls_item->* TO <fs_xls_item>.

    IF me->mv_code_list IS INITIAL AND me->mv_tax_title IS INITIAL AND iv_additional_table = abap_false.

      LOOP AT <fs_xls_item> ASSIGNING FIELD-SYMBOL(<fs_xls_item_line>).
        me->add_row( ).
        me->add_cell( iv_style = sc_styles-usual_style ).
        LOOP AT me->mt_title ASSIGNING FIELD-SYMBOL(<ls_dfies_item>).
          ASSIGN COMPONENT <ls_dfies_item>-vpcoe_attribute OF STRUCTURE <fs_xls_item_line> TO FIELD-SYMBOL(<fs_item_value>).
          IF sy-subrc = 0.
            DATA(lr_descr_ref) = cl_abap_typedescr=>describe_by_data( <fs_item_value> ).
            IF <ls_dfies_item>-vpcoe_attribute <> sc_attributes-formula.
              CASE lr_descr_ref->type_kind.
                WHEN lr_descr_ref->typekind_date.
                  IF <fs_item_value> IS NOT INITIAL.
                    lv_yy = <fs_item_value>+0(4).
                    lv_mm = <fs_item_value>+4(2).
                    lv_dd = <fs_item_value>+6(2).
                    CONCATENATE lv_mm '-' lv_dd '-' lv_yy INTO DATA(lv_date).
                    me->add_cell( iv_style = sc_styles-usual_style ).
                    me->fill_data_cell( iv_cell_name = CONV #( lv_date ) iv_datatype = me->sc_attributes-string ).
                  ELSE.
                    me->add_cell( iv_style = sc_styles-usual_style ).
                    me->fill_data_cell( iv_cell_name = '' iv_datatype = me->sc_attributes-string ).
                  ENDIF.
                WHEN lr_descr_ref->typekind_time.
                  IF <fs_item_value> IS NOT INITIAL.
                    CONCATENATE <fs_item_value>+0(2) ':' <fs_item_value>+2(2) ':' <fs_item_value>+4(2)  INTO DATA(lv_time).
                    me->add_cell( iv_style = sc_styles-usual_style ).
                    me->fill_data_cell( iv_cell_name = CONV #( lv_time ) iv_datatype = me->sc_attributes-string ).
                  ELSE.
                    me->add_cell( iv_style = sc_styles-usual_style ).
                    me->fill_data_cell( iv_cell_name = '' iv_datatype = me->sc_attributes-string ).
                  ENDIF.
                WHEN OTHERS.
                  me->add_cell( iv_style = sc_styles-usual_style ).
                  IF lr_descr_ref->absolute_name = sc_type-type_bool.
                    CASE <fs_item_value>.
                      WHEN 'X'.
                        me->fill_data_cell( iv_cell_name = 'true' iv_datatype = me->sc_attributes-string ).
                      WHEN OTHERS.
                        me->fill_data_cell( iv_cell_name = 'false' iv_datatype = me->sc_attributes-string ).
                    ENDCASE.
                  ELSE.
                    me->fill_data_cell( iv_cell_name = CONV #( <fs_item_value> ) iv_datatype = me->sc_attributes-string ).
                  ENDIF.
              ENDCASE.
            ELSE.
              me->add_cell( iv_style = sc_styles-usual_style iv_formula = <fs_item_value> ).
              me->fill_data_cell( iv_cell_name = CONV #( <fs_item_value> ) iv_datatype = me->sc_attributes-string ).
            ENDIF.
          ELSE.
            me->add_cell( iv_style = sc_styles-usual_style ).
            me->fill_data_cell( iv_cell_name = '' iv_datatype = me->sc_attributes-string ).
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      LOOP AT <fs_xls_item> ASSIGNING FIELD-SYMBOL(<ls_tax_num>).
        LOOP AT me->mt_title ASSIGNING FIELD-SYMBOL(<ls_item>).
          ASSIGN COMPONENT <ls_item>-vpcoe_attribute OF STRUCTURE <ls_tax_num> TO FIELD-SYMBOL(<ls_value>).
          IF <ls_item>-description = sc_attributes-explanation.
            me->add_cell( iv_style = sc_styles-tax_number_style ).
            me->fill_data_cell( iv_cell_name = CONV #( <ls_value> ) iv_datatype = me->sc_attributes-string ).
          ELSE.
            me->add_cell( iv_style = sc_styles-usual_style ).
            me->fill_data_cell( iv_cell_name = CONV #( <ls_value> ) iv_datatype = me->sc_attributes-string ).
          ENDIF.
        ENDLOOP.
        me->add_row( ).
      ENDLOOP.
    ENDIF.

    CLEAR:  me->mv_code_list, me->mv_tax_title.

  ENDMETHOD.


  METHOD fill_prod_ext.
    DATA: lv_style    TYPE string,
          lv_merge    TYPE i,
          lt_item_tab TYPE gty_dref.

    me->set_position( iv_start_column = iv_start_column iv_start_row = iv_start_row ).

    me->mt_title = it_title.

    me->set_column( iv_index = 3  iv_width = 300 ).
    me->set_column( iv_index = 8  iv_width = 200 ).

    lv_style = sc_styles-code_list_style.
    me->add_row( ).
    me->format_height_row( iv_autofitheight = '1' ).

    me->merge_cell( iv_style = lv_style iv_merge_value = '1' ).
    me->fill_data_cell( iv_cell_name = CONV #( iv_code_list_content ) iv_datatype = me->sc_attributes-string ).
    me->mv_current_column = 7.
    me->format_height_row( iv_autofitheight = '1' ).
    me->merge_cell( iv_style = lv_style iv_merge_value = '1' ).
    me->fill_data_cell( iv_cell_name = CONV #( iv_code_list_content ) iv_datatype = me->sc_attributes-string ).

    me->add_row( ).
    LOOP AT me->mt_title ASSIGNING FIELD-SYMBOL(<ls_header_cont>).
      IF <ls_header_cont>-vpcoe_attribute = sc_attributes-type_id.
        me->mv_current_column = 7.
      ENDIF.
      me->add_cell( iv_style = sc_styles-code_list_style ).
      me->fill_data_cell( iv_cell_name = CONV #( <ls_header_cont>-description ) iv_datatype = me->sc_attributes-string ).
    ENDLOOP.

    me->add_row( ).

    GET REFERENCE OF it_item_tab INTO lt_item_tab.
    FIELD-SYMBOLS: <fs_item> TYPE ANY TABLE.

    ASSIGN lt_item_tab->* TO <fs_item>.

    LOOP AT <fs_item> ASSIGNING FIELD-SYMBOL(<ls_item>).
      LOOP AT me->mt_title INTO DATA(ls_item).
        IF ls_item-vpcoe_attribute = sc_attributes-type_id.
          me->mv_current_column = 7.
        ENDIF.
        ASSIGN COMPONENT ls_item-vpcoe_attribute OF STRUCTURE <ls_item> TO FIELD-SYMBOL(<ls_value>).
        me->add_cell( iv_style = sc_styles-usual_style ).
        me->fill_data_cell( iv_cell_name = CONV #( <ls_value> ) iv_datatype = me->sc_attributes-string ).
      ENDLOOP.
      me->add_row( ).
    ENDLOOP.
    me->set_gridlines( ).
  ENDMETHOD.


  METHOD format_alignment.
    DATA(lr_format) = me->mo_document->create_simple_element( name = 'Alignment' parent = me->mo_style ).

    IF iv_h_align IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Horizontal' prefix = 'ss' value = iv_h_align ).
    ENDIF.

    IF iv_v_align IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Vertical' prefix = 'ss' value = iv_v_align ).
    ENDIF.

    IF iv_wraptext IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'WrapText' prefix = 'ss' value = iv_wraptext ).
    ENDIF.
  ENDMETHOD.


  METHOD format_border.
    DATA(lr_format) = me->mo_document->create_simple_element( name = 'Border' parent = me->mo_border ).

    IF iv_position IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Position' prefix = 'ss' value = iv_position ).
    ENDIF.

    IF iv_linestyle IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'LineStyle' prefix = 'ss' value = iv_linestyle ).
    ENDIF.

    IF iv_weight IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Weight' prefix = 'ss' value = iv_weight ).
    ENDIF.

    IF iv_color IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Color' prefix = 'ss' value = iv_color ).
    ENDIF.
  ENDMETHOD.


  METHOD format_font.
    DATA(lr_format) = me->mo_document->create_simple_element( name = 'Font' parent = me->mo_style ).

    IF iv_fontname IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'FontName' prefix = 'ss' value = iv_fontname ).
    ENDIF.

    IF iv_size IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Size' prefix = 'ss' value = iv_size ).
    ENDIF.

    IF iv_bold IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Bold' prefix = 'ss' value = iv_bold ).
    ENDIF.

    IF iv_italic IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Italic' prefix = 'ss' value = iv_italic ).
    ENDIF.

    IF iv_underline IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Underline' prefix = 'ss' value = iv_underline ).
    ENDIF.

    IF iv_color IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Color' prefix = 'ss' value = iv_color ).
    ENDIF.
  ENDMETHOD.


  METHOD format_height_row.
    IF iv_autofitheight IS NOT INITIAL.
      me->mo_row->set_attribute_ns( name = 'AutoFitHeight' prefix = 'ss' value = iv_autofitheight ).
    ENDIF.
  ENDMETHOD.


  METHOD format_interior.
    DATA(lr_format) = me->mo_document->create_simple_element( name = 'Interior' parent = me->mo_style ).

    IF iv_color IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Color' prefix = 'ss' value = iv_color ).
    ENDIF.

    IF iv_pattern IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Pattern' prefix = 'ss' value = iv_pattern ).
    ENDIF.
  ENDMETHOD.


  METHOD format_number.
    DATA(lr_format) = me->mo_document->create_simple_element( name = 'NumberFormat' parent = me->mo_style ).

    IF iv_numberformat IS NOT INITIAL.
      lr_format->set_attribute_ns( name = 'Format' prefix = 'ss' value = iv_numberformat ).
    ENDIF.
  ENDMETHOD.


  METHOD format_width_column.
    DATA lv_count_line TYPE i.
    IF me->mt_title IS NOT INITIAL.
      lv_count_line = lines( me->mt_title ) + 1.
      DO lv_count_line TIMES.
        DATA(lr_column) = me->mo_document->create_simple_element( name = 'Column' parent = me->mo_table ).
        lr_column->set_attribute_ns( name = 'AutoFitWidth' prefix = 'ss' value = '1' ).
        lr_column->set_attribute_ns( name = 'Width' prefix = 'ss' value = '120' ).
      ENDDO.
    ELSE.
      RETURN.
    ENDIF.

    DATA(o_table) = me->mo_document->create_simple_element( name = 'Table' parent = me->mo_worksheet ).
    o_table->set_attribute_ns( name = 'DefaultColumnWidth' prefix = 'x' value = '120' ).

  ENDMETHOD.


  METHOD get_data_reference.

    GET REFERENCE OF it_item_tab INTO me->mt_xls_item.

    me->ddif_fieldinfo_get( ).

  ENDMETHOD.


  METHOD merge_cell.
    me->add_cell( iv_style = iv_style ).
    me->mo_cell->set_attribute_ns( name = 'MergeAcross' prefix = 'ss' value = iv_merge_value ).
  ENDMETHOD.


  METHOD merge_down_cell.
    me->add_cell( iv_style ).
    me->mo_cell->set_attribute_ns( name = 'MergeDown' prefix = 'ss' value = iv_merge_value ).
  ENDMETHOD.


  METHOD render_xml_document.
    " Creating a Stream Factory
    DATA(lr_streamfactory) = me->mo_ixml->create_stream_factory( ).

    " Connect Internal XML Table to Stream Factory
    DATA(lr_ostream) = lr_streamfactory->create_ostream_itable( table = xml_table ).

    " Rendering the Document
    DATA(lr_renderer) = me->mo_ixml->create_renderer( ostream = lr_ostream document = mo_document ).

    " Renders the attached XML document into the output stream
    DATA(lr_rc) = lr_renderer->render( ).

    " Saving the XML Document
    me->xml_size = lr_ostream->get_num_written_raw( ).
  ENDMETHOD.


  METHOD save_xls_file.
    DATA: lv_filename    TYPE string,
          lv_path        TYPE string,
          lv_fullpath    TYPE string,
          lv_user_action TYPE i,
          lo_badi_xls    TYPE REF TO /vpcoe/xls_processing.

    DATA lv_string TYPE xstring.

    me->render_xml_document( ).
    me->create_xls_file( ).

    TRY.
        GET BADI lo_badi_xls.

        CALL BADI lo_badi_xls->adjust_file_saving
          EXPORTING
            iv_background            = me->mv_save_background
            io_log                   = io_log
          IMPORTING
            ev_skip_standard_process = DATA(lv_skip)
          CHANGING
            cv_file_path             = me->mv_path
            ct_xls_data              = mt_xls_bin.

        IF lv_skip = abap_true.
          RETURN.
        ENDIF.

      CATCH cx_badi_not_implemented.                    "#EC NO_HANDLER
*      BAdI isn't implemented
    ENDTRY.

    IF me->mv_save_background = abap_true.
      cl_vsi=>itab_to_xstring(
         EXPORTING
           it_itab    = mt_xls_bin
         IMPORTING
           ef_xstring = lv_string ).

      OPEN DATASET me->mv_path FOR OUTPUT IN BINARY MODE.

      IF sy-subrc <> 0.
        io_log->add_sy_msg( ).
        RETURN.
      ENDIF.

      TRANSFER lv_string TO me->mv_path.
      CLOSE DATASET me->mv_path.

    ELSE.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename                = me->mv_path
          filetype                = 'BIN'
        TABLES
          data_tab                = mt_xls_bin
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.
      IF sy-subrc <> 0.
        io_log->add_sy_msg( ).
        RETURN.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD set_column.
    CLEAR: me->mo_column.
    DATA: lv_index TYPE string,
          lv_width TYPE string.

    lv_width = CONV #( iv_width ).
    CONDENSE lv_width NO-GAPS.
    lv_index = CONV #( iv_index ).
    CONDENSE lv_index NO-GAPS.

    me->mo_column = me->mo_document->create_simple_element( name = 'Column' parent = me->mo_table ).
    me->mo_column->set_attribute_ns( name = 'Index' prefix = 'ss' value = lv_index ).
    me->mo_column->set_attribute_ns( name = 'Width' prefix = 'ss' value = lv_width ).

  ENDMETHOD.


  METHOD set_gridlines.
    DATA(r_worksheetoptions) = me->mo_document->create_simple_element( name = 'WorksheetOptions' parent = me->mo_worksheet ).
    r_worksheetoptions->set_attribute( name = 'xmlns' value = 'urn:schemas-microsoft-com:office:excel' ).
  ENDMETHOD.


  METHOD set_position.
    IF iv_start_row IS NOT INITIAL.
      me->mv_start_row = iv_start_row.
      me->mv_current_row = iv_start_row.
    ELSE.
      me->mv_start_row = 1.
      me->mv_current_row = 1.
    ENDIF.

    IF iv_start_column IS NOT INITIAL.
      me->mv_start_column = iv_start_column.
      me->mv_current_column = iv_start_column.
    ELSE.
      me->mv_start_column = 1.
      me->mv_current_column = 1.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
