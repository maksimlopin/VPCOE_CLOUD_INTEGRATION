class /VPCOE/CL_RDP_PRODUCT_DATA_SM definition
  public
  inheriting from /VPCOE/CL_RDP_PRODUCT_DATA
  create public .

public section.

  types:
    BEGIN OF gty_s_product_sm_json,
        replication_run_id TYPE  /vpcoe/de_run_id,
        elements           TYPE gty_t_product_combined,
      END OF gty_s_product_sm_json .

   types:
    BEGIN OF gty_s_product_ext_sm_json,
        replication_run_id TYPE /vpcoe/de_run_id,
        elements           TYPE /vpcoe/tt_product_ext,
      END OF gty_s_product_ext_sm_json .

  methods CONSTRUCTOR
    importing
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER .
  methods CLOSE_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .

  methods GET_PRODUCT_EXT
    redefinition .
protected section.

  methods GET_QUANTITY_CONVERSIONS
    importing
      !IT_PRODUCT type GTY_T_PRODUCT
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
    exporting
      !ET_QUANTITY_CONVERSIONS type GTY_T_QUANTITY_CONVERSIONS .
  methods GET_GLOBAL_TRADE_ITEM_NUM
    importing
      !IT_PRODUCT type GTY_T_PRODUCT
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
    exporting
      !ET_GLOBAL_TRADE_ITEM_NUM type GTY_T_GLOBAL_TRADE_ITEM_NUM .

  methods BUILD_JSONS
    redefinition .
  methods GET_HEADER
    redefinition .
  methods RETRIEVE_AND_PROCESS_FURTHER
    redefinition .
private section.

  data MO_SUMA_HELPER type ref to /VPCOE/CL_RDP_SUMA_HELPER .
  data MO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER .

  methods GET_QUANTITY_DIMENSIONS
    importing
      !IT_PRODUCT type GTY_T_PRODUCT
      !IS_SEL_OPT type /VPCOE/S_SELOPT_PRODUCT
    exporting
      !ET_QUANTITY_DIMENSIONS type GTY_T_QUANTITY_DIMENSIONS .
ENDCLASS.



CLASS /VPCOE/CL_RDP_PRODUCT_DATA_SM IMPLEMENTATION.


  METHOD build_jsons.

    DATA: ls_source_data      TYPE gty_s_product_sm_json,
          lo_badi             TYPE REF TO /vpcoe/adjust_data_retrieval,
          lt_product_pack     TYPE gty_t_product_combined,
          lt_product_combined TYPE gty_t_product_combined.

    me->mo_suma_helper->start_replication( EXPORTING io_log = mo_log ).

    ls_source_data-replication_run_id = me->mo_suma_helper->get_current_run_id( ).
    IF ls_source_data-replication_run_id IS INITIAL.
      RETURN.
    ENDIF.

    lt_product_combined = CORRESPONDING #( it_product_combined ).

    LOOP AT lt_product_combined ASSIGNING FIELD-SYMBOL(<ls_product_combined>).
      INSERT <ls_product_combined> INTO TABLE lt_product_pack.

      IF lines( lt_product_pack ) = me->mv_package_size AND me->mv_package_size IS NOT INITIAL.
        APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
        ls_source_data-elements = lt_product_pack.
        DATA(lo_writer_json) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
        CALL TRANSFORMATION /vpcoe/rdp_product_sm_to_json SOURCE root = ls_source_data RESULT XML lo_writer_json.
        <ls_json>-elements = cl_abap_codepage=>convert_from( lo_writer_json->get_output( ) ).
        <ls_json>-count = lines( lt_product_pack ).

        CLEAR: ls_source_data, lt_product_pack.
      ENDIF.

    ENDLOOP.

    IF lt_product_pack IS NOT INITIAL.
      APPEND INITIAL LINE TO et_json ASSIGNING <ls_json>.
      lo_writer_json = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
      ls_source_data-elements = lt_product_pack.
      CALL TRANSFORMATION /vpcoe/rdp_product_sm_to_json SOURCE root = ls_source_data RESULT XML lo_writer_json.
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


  METHOD close_replication.
    " Close Replication
    IF io_log IS BOUND AND io_log->check( ).
      me->mo_suma_helper->cancel_replication( io_log = io_log ).
    ELSE.
      me->mo_suma_helper->finish_replication( io_log = io_log ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
     super->constructor(
      EXPORTING
        iv_package_size = io_rdp_helper->get_package_size( )
        iv_source       = io_rdp_helper->get_source_id( )
        iv_api_type     = io_rdp_helper->get_api_type( )
        iv_mode         = iv_mode
        io_log          = io_log ).

    me->mo_rdp_helper = io_rdp_helper.

    me->mo_suma_helper = /vpcoe/cl_rdp_suma_helper=>get_instance( io_rdp_helper = me->mo_rdp_helper
                                                                  iv_srv_prfx   = 'product' ).
  ENDMETHOD.


  METHOD get_global_trade_item_num.

    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    DATA: lv_skip    TYPE abap_bool,
          ls_sel_opt TYPE gty_s_sel_opt.

    CLEAR et_global_trade_item_num.

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
        iv_level    = /vpcoe/cl_common_helper=>sc_level-gl_trade_item_num
        is_sel_opt  = ls_sel_opt-matnr
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT DISTINCT mean~matnr AS product_id,
                      mean~meinh AS unit_of_measure,
                      mean~ean11 AS global_trade_item_number,
                      mean~hpean AS is_main
         INTO CORRESPONDING FIELDS OF TABLE @et_global_trade_item_num
           FROM mean
             FOR ALL ENTRIES IN @it_product
                WHERE mean~matnr = @it_product-id.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-gl_trade_item_num
        is_sel_opt  = ls_sel_opt-matnr
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = et_global_trade_item_num.
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
                      mara~prdha AS product_hierarchy,
                      CASE WHEN mara~lvorm = 'X' THEN 'X'
                                                 ELSE ' ' END AS is_marked_for_deletion,
                      mara~matkl AS product_group,
                      mara~spart AS division,
                      CASE WHEN marc~beskz = 'X'
                             OR marc~beskz = 'F' THEN 'X'
                                                 ELSE ' ' END AS is_procurement_relevant,
                      mara~meins AS base_unit_of_measure
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

    IF mv_mode = /vpcoe/cl_common_helper=>sc_mode-send.
      me->mo_suma_helper->start_replication( EXPORTING io_log = mo_log ).

      DATA(lv_replication_run_id) = me->mo_suma_helper->get_current_run_id( ).
      IF lv_replication_run_id IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT lt_product_ext ASSIGNING FIELD-SYMBOL(<ls_product_ext>).
      INSERT <ls_product_ext> INTO TABLE lt_product_pack_ext.

      IF lines( lt_product_pack_ext ) = mv_package_size AND mv_package_size IS NOT INITIAL.

        IF mv_mode = /vpcoe/cl_common_helper=>sc_mode-send.

          APPEND INITIAL LINE TO lt_json ASSIGNING FIELD-SYMBOL(<ls_json>).
          <ls_json>-elements = /vpcoe/cl_common_helper=>serialize_json(
                                   EXPORTING is_data = VALUE gty_s_product_ext_sm_json( replication_run_id   = lv_replication_run_id
                                                                                        elements = lt_product_pack_ext )
                                             iv_pretty_name    = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                             it_name_mappings = VALUE #(
                                                 ( abap = 'PRODUCT_TYPE_HARMONIZED' json = 'productTypeHarmonizedCode' )
                                                 ( abap = 'PRODUCT_CONTENT'         json = 'productContentCode' )
                                                 ( abap = 'EPR_PRODUCT_FAMILY'      json = 'productFamily' ) )
                                             iv_numc_as_string = abap_true ) .
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
                                                is_data = VALUE gty_s_product_ext_sm_json( replication_run_id   = lv_replication_run_id
                                                                                             elements = lt_product_pack_ext )
                                                iv_pretty_name    = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                                it_name_mappings = VALUE #(
                                                 ( abap = 'PRODUCT_TYPE_HARMONIZED' json = 'productTypeHarmonizedCode' )
                                                 ( abap = 'PRODUCT_CONTENT'         json = 'productContentCode' )
                                                 ( abap = 'EPR_PRODUCT_FAMILY'      json = 'productFamily' )
                                                 ( abap = 'PRODUCT_COUNTRIES'       json = 'countries' ) )
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


  METHOD get_quantity_conversions.

    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    DATA: lv_skip    TYPE abap_bool,
          ls_sel_opt TYPE gty_s_sel_opt.

    CLEAR et_quantity_conversions.

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
      SELECT DISTINCT mara~matnr   AS product_id,
                      mara~meins   AS quantity_unit_of_measure,
                      marm~umrez   AS quantity_content,
                      marm~meinh AS corresponding_quantity_u_of_m,
                      marm~umren AS corresponding_quantity_content
         INTO CORRESPONDING FIELDS OF TABLE @et_quantity_conversions
           FROM mara INNER JOIN marm ON mara~matnr = marm~matnr
             FOR ALL ENTRIES IN @it_product
                WHERE mara~matnr = @it_product-id.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
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
        ct_data     = et_quantity_conversions.
  ENDMETHOD.


  METHOD get_quantity_dimensions.

    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.
    DATA: lv_skip TYPE abap_bool,
          lt_uom  TYPE gty_t_product_unit_of_measure,
          ls_quantity_dimensions TYPE gty_s_quantity_dimensions.

    IF it_product IS INITIAL.
      RETURN.
    ENDIF.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-quantity_dimen
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      SELECT mara~matnr AS product_id,
             mara~meins AS measurement_unit,
             mara~ntgew AS net_weight,
             mara~brgew AS gross_weight,
             mara~gewei AS weight_unit_of_measure
         INTO CORRESPONDING FIELDS OF TABLE @lt_uom
          FROM mara
             FOR ALL ENTRIES IN @it_product
                WHERE mara~matnr = @it_product-id.

      IF sy-subrc <> 0.
        CLEAR lt_uom.
      ENDIF.
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-product
        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product
        iv_api_type = me->mv_api_type
        iv_level    = /vpcoe/cl_common_helper=>sc_level-quantity_dimen
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        ct_data     = lt_uom.

      LOOP AT lt_uom ASSIGNING FIELD-SYMBOL(<ls_uom>).
        ls_quantity_dimensions = VALUE #( product_id             = <ls_uom>-product_id
                                          unit_of_measure        = <ls_uom>-measurement_unit
                                          dimension_quantity_uom = <ls_uom>-weight_unit_of_measure ).
        IF <ls_uom>-gross_weight IS NOT INITIAL.
          ls_quantity_dimensions-dimension_name = /vpcoe/cl_common_helper=>sc_dimension_name-gross_weight.
          ls_quantity_dimensions-dimension_quantity_content = <ls_uom>-gross_weight.
          INSERT ls_quantity_dimensions INTO TABLE et_quantity_dimensions.
        ENDIF.

        IF <ls_uom>-net_weight IS NOT INITIAL.
          ls_quantity_dimensions-dimension_name = /vpcoe/cl_common_helper=>sc_dimension_name-net_weight.
          ls_quantity_dimensions-dimension_quantity_content = <ls_uom>-net_weight.
          INSERT ls_quantity_dimensions INTO TABLE et_quantity_dimensions.
        ENDIF.
      ENDLOOP.

  ENDMETHOD.


  METHOD retrieve_and_process_further.

    DATA: lo_badi                TYPE REF TO /vpcoe/adjust_data_retrieval,
          lt_product_combined    TYPE gty_t_product_combined,
          lt_json                TYPE  /vpcoe/cl_rdp_http=>gty_t_json,
          lr_data_combined       TYPE REF TO gty_s_product_tables,
          ls_product_combined    TYPE gty_s_product_combined,
          ls_quantity_dimensions TYPE gty_s_quantity_dimensions.

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
    me->get_global_trade_item_num(
      EXPORTING
        is_sel_opt = is_sel_opt
        it_product = it_product
      IMPORTING
        et_global_trade_item_num = DATA(lt_global_trade_item_num) ).
    me->get_quantity_conversions(
      EXPORTING
        is_sel_opt = is_sel_opt
        it_product = it_product
      IMPORTING
        et_quantity_conversions = DATA(lt_quantity_conversions) ).
    me->get_plants(
      EXPORTING
        is_sel_opt = is_sel_opt
        it_product = it_product
      IMPORTING
        et_plants  = DATA(lt_plants) ).
    me->get_quantity_dimensions(
      EXPORTING
        is_sel_opt = is_sel_opt
        it_product = it_product
      IMPORTING
        et_quantity_dimensions = DATA(lt_quantity_dimensions) ).

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
          brand                    = <ls_product>-brand
          is_procurement_relevant  =  COND #( WHEN <ls_product>-is_procurement_relevant = abap_true THEN 'true'
                                                                                                    ELSE 'false' ) ##NO_TEXT

          texts                    = VALUE #( FOR <ls_f_text> IN lt_description WHERE ( product_id = <ls_product>-id )
                                                 ( language    = /vpcoe/cl_common_helper=>convert_langu_code( CONV #( <ls_f_text>-language ) )
                                                   name = <ls_f_text>-description ) )

          product_unit_of_measures = VALUE #( FOR <ls_f_uom> IN lt_uom WHERE ( product_id = <ls_product>-id )
                                                 ( measurement_unit             = <ls_f_uom>-measurement_unit
                                                   numerator                    = <ls_f_uom>-numerator
                                                   denominator                  = <ls_f_uom>-denominator  ) )
          product_sales            = VALUE #( FOR <ls_f_sales> IN lt_sales WHERE ( product_id = <ls_product>-id )
                                                      ( sales_organization   = <ls_f_sales>-sales_organization
                                                        distribution_channel = <ls_f_sales>-distribution_channel
                                                        sales_measure_unit   = <ls_f_sales>-sales_measure_unit ) )
          quantity_conversions     = VALUE #( FOR <ls_qc> IN lt_quantity_conversions WHERE ( product_id =  <ls_product>-id )
                                                  ( quantity_unit_of_measure       = <ls_qc>-quantity_unit_of_measure
                                                    quantity_content               = <ls_qc>-quantity_content
                                                    corresponding_quantity_u_of_m  = <ls_qc>-corresponding_quantity_u_of_m
                                                    corresponding_quantity_content = <ls_qc>-corresponding_quantity_content
                                                    ) )
          global_trade_item_num    = VALUE #( FOR <ls_gtin> IN lt_global_trade_item_num WHERE ( product_id = <ls_product>-id )
                                                  ( unit_of_measure          = <ls_gtin>-unit_of_measure
                                                    global_trade_item_number = <ls_gtin>-global_trade_item_number
                                                    is_main                  = COND #( WHEN <ls_gtin>-is_main = abap_true THEN 'true'
                                                                                                                          ELSE 'false' ) ) )
          plants                   = VALUE #( FOR <ls_plant> IN lt_plants  WHERE ( product_id = <ls_product>-id )
                                                  ( plant_id = <ls_plant>-plant_id ) )
          quantity_dimensions_sm   =  VALUE #( FOR <ls_qd> IN lt_quantity_dimensions WHERE ( product_id = <ls_product>-id )
                                                  ( unit_of_measure            = <ls_qd>-unit_of_measure
                                                    dimension_name             = <ls_qd>-dimension_name
                                                    dimension_quantity_content = <ls_qd>-dimension_quantity_content
                                                    dimension_quantity_uom     = <ls_qd>-dimension_quantity_uom ) ) ) .

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
