class /VPCOE/CL_RDP_CONFIG_OBJ_SM definition
  public
  inheriting from /VPCOE/CL_RDP_CONFIG_OBJ
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER optional
      !IV_API_TYPE type /VPCOE/DE_API_TYPE optional .

  methods GET_COUNTRY
    redefinition .
  methods GET_CURRENCY
    redefinition .
  methods GET_DLVR_DOC_ITEM_CAT
    redefinition .
  methods GET_DLVR_DOC_TYPE
    redefinition .
  methods GET_INCOTERMS
    redefinition .
  methods GET_MOVEMENT_TYPE
    redefinition .
  methods GET_PRODUCT_GROUP
    redefinition .
  methods GET_PRODUCT_HIERARCHY
    redefinition .
  methods GET_PRODUCT_TYPE
    redefinition .
  methods GET_REGION
    redefinition .
  methods GET_UOM
    redefinition .
  methods GET_UOM_DIMENSION
    redefinition .
  methods GET_UOM_ISO_CODE
    redefinition .
protected section.

  data MO_SUMA_HELPER type ref to /VPCOE/CL_RDP_SUMA_HELPER .
  data MV_RUN_ID type /VPCOE/DE_RUN_ID .
private section.

  types:
    BEGIN OF gty_s_json_sm,
           replication_run_id TYPE /vpcoe/de_run_id,
           elements           TYPE string,
           count              TYPE i,
         END OF gty_s_json_sm .
  types:
    gty_t_json_sm TYPE STANDARD TABLE OF gty_s_json_sm WITH EMPTY KEY .
ENDCLASS.



CLASS /VPCOE/CL_RDP_CONFIG_OBJ_SM IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
      EXPORTING
        iv_mode       = iv_mode
        io_log        = io_log
        io_rdp_helper = io_rdp_helper
        iv_api_type   = iv_api_type ).

    IF iv_mode = /vpcoe/cl_common_helper=>sc_mode-send.
      me->mo_suma_helper = /vpcoe/cl_rdp_suma_helper=>get_instance( io_rdp_helper = me->mo_rdp_helper
                                                                    iv_srv_prfx   = 'configuration' ).

      me->mo_suma_helper->start_replication( EXPORTING io_log = io_log ).
      me->mv_run_id = me->mo_suma_helper->get_current_run_id( ).
    ENDIF.

  ENDMETHOD.


METHOD get_country.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE  /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_country.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_country   TYPE REF TO data,
        lt_mapping     TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json        LIKE LINE OF et_json,
        ls_source_data TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_country.

  super->get_country(
    IMPORTING
      et_country = et_country ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_country TYPE /vpcoe/tt_country.
  ASSIGN lr_t_country->* TO <lt_elements>.

  lt_mapping = VALUE #( ( abap = `iseuropeanunionmember`  json = `isEuropeanUnionMember` ) ) .

  LOOP AT et_country INTO DATA(lr_country) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_country ASSIGNING FIELD-SYMBOL(<ls_country>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_country> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_currency.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE  /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_currency.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_currency  TYPE REF TO data,
        lt_mapping     TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json        LIKE LINE OF et_json,
        ls_source_data TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_currency.

  super->get_currency(
    IMPORTING
      et_currency = et_currency ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_currency TYPE /vpcoe/tt_currency.
  ASSIGN lr_t_currency->* TO <lt_elements>.

  LOOP AT et_currency INTO DATA(lr_currency) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_currency ASSIGNING FIELD-SYMBOL(<ls_currency>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_currency> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_dlvr_doc_item_cat.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE  /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_dlvr_doc_item_type.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_dlvr_doc_item_cat TYPE REF TO data,
        lt_mapping             TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json                LIKE LINE OF et_json,
        ls_source_data         TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_dlvr_doc_item_type.

  super->get_dlvr_doc_item_cat(
    IMPORTING
      et_dlvr_doc_item_cat = et_dlvr_doc_item_cat ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_dlvr_doc_item_cat TYPE /vpcoe/tt_dlvr_doc_item_type.
  ASSIGN lr_t_dlvr_doc_item_cat->* TO <lt_elements>.

  LOOP AT et_dlvr_doc_item_cat INTO DATA(lr_dlvr_doc_item_cat) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_dlvr_doc_item_cat ASSIGNING FIELD-SYMBOL(<ls_dlvr_doc_item_cat>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_dlvr_doc_item_cat> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_dlvr_doc_type.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE  /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_dlvr_doc_type.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_dlvr_doc_type TYPE REF TO data,
        lt_mapping         TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json            LIKE LINE OF et_json,
        ls_source_data     TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_dlvr_doc_type.

  super->get_dlvr_doc_type(
    IMPORTING
      et_dlvr_doc_type = et_dlvr_doc_type ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_dlvr_doc_type TYPE /vpcoe/tt_dlvr_doc_type.
  ASSIGN lr_t_dlvr_doc_type->* TO <lt_elements>.

  LOOP AT et_dlvr_doc_type INTO DATA(lr_dlvr_doc_type) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_dlvr_doc_type ASSIGNING FIELD-SYMBOL(<ls_dlvr_doc_type>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_dlvr_doc_type> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_incoterms.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE  /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_incoterms.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_incoterms TYPE REF TO data,
        lt_mapping     TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json        LIKE LINE OF et_json,
        ls_source_data TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_incoterms.

  super->get_incoterms(
    IMPORTING
      et_incoterms = et_incoterms ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_incoterms TYPE /vpcoe/tt_incoterms.
  ASSIGN lr_t_incoterms->* TO <lt_elements>.

  LOOP AT et_incoterms INTO DATA(lr_incoterms) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_incoterms ASSIGNING FIELD-SYMBOL(<ls_incoterms>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_incoterms> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_movement_type.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE  /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_movement_type.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_movement_type TYPE REF TO data,
        lt_mapping         TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json            LIKE LINE OF et_json,
        ls_source_data     TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_movement_type.

  super->get_movement_type(
    IMPORTING
      et_movement_type = et_movement_type ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_movement_type TYPE /vpcoe/tt_movement_type.
  ASSIGN lr_t_movement_type->* TO <lt_elements>.

  LOOP AT et_movement_type INTO DATA(lr_movement_type) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_movement_type ASSIGNING FIELD-SYMBOL(<ls_movement_type>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_movement_type> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_product_group.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE  /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_product_group.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_product_group TYPE REF TO data,
        lt_mapping         TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json            LIKE LINE OF et_json,
        ls_source_data     TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_product_group.

  super->get_product_group(
    IMPORTING
      et_product_group = et_product_group ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_product_group TYPE /vpcoe/tt_product_group.
  ASSIGN lr_t_product_group->* TO <lt_elements>.

  LOOP AT et_product_group INTO DATA(lr_product_group) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_product_group ASSIGNING FIELD-SYMBOL(<ls_product_group>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_product_group> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_product_hierarchy.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE  /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_product_hierarchy.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_product_hierarchy TYPE REF TO data,
        lt_mapping             TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json                LIKE LINE OF et_json,
        ls_source_data         TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_product_hierarchy.

  super->get_product_hierarchy(
    IMPORTING
      et_product_hierarchy = et_product_hierarchy ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_product_hierarchy TYPE /vpcoe/tt_product_hierarchy.
  ASSIGN lr_t_product_hierarchy->* TO <lt_elements>.

  LOOP AT et_product_hierarchy INTO DATA(lr_product_hierarchy) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_product_hierarchy ASSIGNING FIELD-SYMBOL(<ls_product_hierarchy>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_product_hierarchy> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_product_type.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE  /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_product_type.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_product_type TYPE REF TO data,
        lt_mapping        TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json           LIKE LINE OF et_json,
        ls_source_data    TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_product_type.

  super->get_product_type(
    IMPORTING
      et_product_type = et_product_type ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_product_type TYPE /vpcoe/tt_product_type.
  ASSIGN lr_t_product_type->* TO <lt_elements>.

  LOOP AT et_product_type INTO DATA(lr_product_type) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_product_type ASSIGNING FIELD-SYMBOL(<ls_product_type>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_product_type> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_region.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_region.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_region    TYPE REF TO data,
        lt_mapping     TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json        LIKE LINE OF et_json,
        ls_source_data TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_region.

  super->get_region(
    IMPORTING
      et_region = et_region ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_region TYPE /vpcoe/tt_region.
  ASSIGN lr_t_region->* TO <lt_elements>.

  LOOP AT et_region INTO DATA(lr_region) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_region ASSIGNING FIELD-SYMBOL(<ls_region>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_region> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_uom.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_uom.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_uom       TYPE REF TO data,
        lt_mapping     TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json        LIKE LINE OF et_json,
        ls_source_data TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_uom.

  super->get_uom(
    IMPORTING
      et_uom = et_uom ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_uom TYPE /vpcoe/tt_uom.
  ASSIGN lr_t_uom->* TO <lt_elements>.

  LOOP AT et_uom INTO DATA(lr_uom) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_uom ASSIGNING FIELD-SYMBOL(<ls_uom>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_uom> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_uom_dimension.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_uom_dimension.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_uom_dimension TYPE REF TO data,
        lt_mapping         TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json            LIKE LINE OF et_json,
        ls_source_data     TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_uom_dimension.

  super->get_uom_dimension(
    IMPORTING
      et_uom_d = et_uom_d ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_uom_dimension TYPE /vpcoe/tt_uom_dimension.
  ASSIGN lr_t_uom_dimension->* TO <lt_elements>.

  LOOP AT et_uom_d INTO DATA(lr_uom_dimension) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_uom_dimension ASSIGNING FIELD-SYMBOL(<ls_uom_dimension>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_uom_dimension> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.


METHOD get_uom_iso_code.

  TYPES: BEGIN OF lty_s_source_data,
           replication_run_id TYPE /vpcoe/de_run_id,
           elements           TYPE /vpcoe/tt_uom_iso_code.
  TYPES: END OF lty_s_source_data.

  DATA: lr_t_uom_iso_code TYPE REF TO data,
        lt_mapping        TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings,
        ls_json           LIKE LINE OF et_json,
        ls_source_data    TYPE lty_s_source_data.

  FIELD-SYMBOLS: <lt_elements> TYPE /vpcoe/tt_uom_iso_code.

  super->get_uom_iso_code(
    IMPORTING
      et_iso_uom = et_iso_uom ).

  DATA(lv_package_size) = me->mo_rdp_helper->get_package_size( ).

  CREATE DATA lr_t_uom_iso_code TYPE /vpcoe/tt_uom_iso_code.
  ASSIGN lr_t_uom_iso_code->* TO <lt_elements>.

  LOOP AT et_iso_uom INTO DATA(lr_uom_iso_code) GROUP BY ( sy-tabix - 1 ) DIV lv_package_size + 1.

    CLEAR <lt_elements>.

    LOOP AT GROUP lr_uom_iso_code ASSIGNING FIELD-SYMBOL(<ls_uom_iso_code>).
      <lt_elements> = VALUE #( BASE <lt_elements> ( <ls_uom_iso_code> ) ).
    ENDLOOP.

    ls_source_data-replication_run_id = me->mv_run_id.
    ls_source_data-elements = <lt_elements>.

    ls_json-elements = /vpcoe/cl_rdp_helper=>serialize_json( EXPORTING is_data           = ls_source_data
                                                                        iv_pretty_name    = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case
                                                                        it_name_mappings  = lt_mapping
                                                                        iv_numc_as_string = abap_true ).

    ls_json-count = lines( <lt_elements> ).
    INSERT ls_json INTO TABLE et_json.

  ENDLOOP.

  me->adjust_json( CHANGING ct_json = et_json ).

ENDMETHOD.
ENDCLASS.
