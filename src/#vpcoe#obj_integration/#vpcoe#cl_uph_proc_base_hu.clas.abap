class /VPCOE/CL_UPH_PROC_BASE_HU definition
  public
  inheriting from /VPCOE/CL_UPH_PROC_BASE
  abstract
  create public .

public section.

  interfaces /VPCOE/IF_UPH_ENTITY_HU_PROC .

  methods RETRIEVE_HU_DATA_TEMP .
  methods CONSTRUCTOR .

  methods /VPCOE/IF_UPH_ENTITY_PROC~DESERIALIZE_SELECTION_PARAMS
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_PROC~GET_PARAMETERS
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_PROC~INIT_PROCESSOR
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_PROC~PREPARE_PROCESS
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_PROC~PROCESS_PACKAGE
    redefinition .
protected section.

  types:
    BEGIN OF gty_s_product_units,
        product   TYPE matnr,
        base_unit TYPE meins,
      END OF gty_s_product_units .
  types:
    gty_t_product_units TYPE SORTED TABLE OF gty_s_product_units WITH UNIQUE KEY product .
  types:
    BEGIN OF gty_s_product,
        product   TYPE matnr,
        base_unit TYPE meins,
        quantity  TYPE lgmng,
      END OF gty_s_product .
  types:
    gty_t_product TYPE SORTED TABLE OF gty_s_product WITH UNIQUE KEY primary_key COMPONENTS product base_unit .
  types:
    BEGIN OF gty_s_hu_headers,
        internal_number_hu TYPE venum,
        load_carrier       TYPE vhilm,
        load_carrier_unit  TYPE meins,
        delivery           TYPE vbeln_gen,
      END OF gty_s_hu_headers .
  types:
    gty_t_hu_headers TYPE SORTED TABLE OF gty_s_hu_headers WITH UNIQUE KEY primary_key COMPONENTS delivery internal_number_hu .
  types:
    BEGIN OF gty_s_handling_unit,
        delivery           TYPE vbeln_gen,
        internal_number_hu TYPE venum,
        number_hu          TYPE venum,
        item_hu            TYPE vepos,
        load_carrier       TYPE vhilm,
        load_carrier_unit  TYPE meins,
        pack_amount        TYPE veanz,
        low_level          TYPE unvel,
        product            TYPE matnr,
        item_category      TYPE pstyv,
        packed_amount      TYPE vmeng,
        base_unit          TYPE vemeh,
      END OF gty_s_handling_unit .
  types:
    gty_t_handling_units TYPE SORTED TABLE OF gty_s_handling_unit
                                WITH NON-UNIQUE KEY primary_key COMPONENTS internal_number_hu item_hu
                                WITH NON-UNIQUE SORTED KEY level COMPONENTS low_level .
  types:
    BEGIN OF gty_s_value_packcomp,
        value TYPE /vpcoe/if_uph_entity_hu_proc=>gty_t_api_rp_packcomp_for_prod,
      END OF gty_s_value_packcomp .
  types:
    BEGIN OF gty_s_hu_resolved,
        internal_number TYPE venum,
        handling_unit   TYPE vhilm,
        amount          TYPE p LENGTH 14 DECIMALS 6,
        base_unit       TYPE meins,
        category        TYPE pstyv,
        delivery        TYPE vbeln_gen,
        packed_products TYPE gty_t_product,
        aux_products    TYPE gty_t_product,
      END OF gty_s_hu_resolved .
  types:
    gty_t_hu_resolved TYPE SORTED TABLE OF gty_s_hu_resolved WITH UNIQUE KEY primary_key COMPONENTS internal_number .
  types:
    BEGIN OF gty_s_sel_period,
        from TYPE dats,
        to   TYPE dats,
      END OF gty_s_sel_period .
  types:
    gty_t_prd_assigments TYPE STANDARD TABLE OF /vpcoe/if_uph_entity_hu_proc=>gty_s_api_sel_criteria WITH DEFAULT KEY .
  types:
    BEGIN OF gty_s_hu_json,
        source              TYPE c LENGTH 10,
        selection_period    TYPE gty_s_sel_period,
        product_assignments TYPE gty_t_prd_assigments,
      END OF gty_s_hu_json .

  constants:
    BEGIN OF sc_item_type,
        addit_item TYPE pstyv      VALUE 'HUPM',
        pack_mat   TYPE pstyv      VALUE 'LEIH',
      END OF sc_item_type .
  data MS_PARAMETERS type /VPCOE/S_SELOPT_HU .     "/vpcoe/if_uph_pckg_cmp_hu_load=>gty_s_hu_input.
  data MT_PRODUCT_UNITS type GTY_T_PRODUCT_UNITS .
  data MT_HANDLING_UNITS type GTY_T_HANDLING_UNITS .
  data MT_HU_HEADERS type GTY_T_HU_HEADERS .
  data MT_CALCULATED_HU type /VPCOE/IF_UPH_ENTITY_HU_PROC=>GTY_T_HU_FOR_PRODUCT .
  constants MC_PACK_COMP_PRODUCT type STRING value '/GetPackagingCompositionsForProduct' ##NO_TEXT.

  methods RETRIEVE_COMPOSITION_DATA
    importing
      !IT_HANDLING_UNITS type /VPCOE/IF_UPH_ENTITY_HU_PROC=>GTY_T_HU_FOR_PRODUCT
    returning
      value(RO_RESULT) type ref to /VPCOE/CL_UPH_ENT_PCKG_CMP_GP .
  methods CALCULATE_HU_DISTRIBUTION .
  methods RESOLVE_HU_ITEM
    importing
      !IV_HU type VENUM
      !IV_SUBORD_QUANTITY type VEANZ
    changing
      !CT_HU_ITEMS type GTY_T_HU_RESOLVED
      !CT_PACKED_PRODUCTS type GTY_T_PRODUCT
      !CT_AUX_PRODUCTS type GTY_T_PRODUCT .
  methods DETERMINE_BASE_UNIT
    importing
      !IV_PRODUCT type MATNR
    returning
      value(RV_RESULT) type MEINS .
  methods CONVERT_TO_BASE_UNIT
    importing
      !IV_PRODUCT type MATNR
      !IV_QUANTITY type ANY
      !IV_UNIT type MEINS
      !IV_PLANT type WERKS_D optional
    returning
      value(RV_RESULT) type QUAN_15 .
private section.
ENDCLASS.



CLASS /VPCOE/CL_UPH_PROC_BASE_HU IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_hu_proc~map_hu_data.
    " Need to be redefined
  ENDMETHOD.


METHOD /vpcoe/if_uph_entity_proc~deserialize_selection_params.
  DATA ls_selection_params TYPE /vpcoe/s_selopt_hu.

  CLEAR es_selection_params.

  /ui2/cl_json=>deserialize(
    EXPORTING
      json   = iv_json_str
    CHANGING
      data   = ls_selection_params ).

  es_selection_params = ls_selection_params.

ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~get_parameters.
    rv_result = REF #( ms_parameters ).
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~init_processor.

    super->/vpcoe/if_uph_entity_proc~init_processor( iv_upload_entity = iv_upload_entity iv_upload_mode = iv_upload_mode ).

    "store the input parameter only in case it is a full load
    CLEAR ms_parameters.

    IF is_parameters IS NOT INITIAL AND mv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.

      MOVE-CORRESPONDING is_parameters TO ms_parameters.

    ENDIF.

    mv_initialized = abap_true.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~prepare_process.

    DATA: lt_table_name       TYPE ddfields,
          lv_columns          TYPE string,
          lv_ship_to_party    TYPE string,
          lt_delivery_hdr	    TYPE /vpcoe/t_delivery_hdr,
          lv_value_for_status TYPE string.

    lv_columns = `likp~vbeln     AS id,`
              && `likp~lfart     AS type,`
              && `likp~vkorg     AS sales_organization,`
              && `likp~kunnr     AS ship_to_party,`
              && `likp~erdat     AS create_date,`
              && `likp~aedat     AS change_date,`
              && `likp~wadat_ist AS actual_goods_movement_date,`
              && `likp~inco1     AS incoterms,`
              && `CASE WHEN likp~kunag = ' ' THEN likp~kunnr `
              &&                            `ELSE likp~kunag END AS sold_to_party, `
              && `CASE WHEN adrc~country IS NOT NULL THEN adrc~country `
              &&                                     `ELSE CASE WHEN kna1~land1 IS NOT NULL THEN kna1~land1 `
              &&                                                                           `ELSE kna_sh~land1 END `
              &&            `END AS ship_to_country, `
              && `CASE WHEN adrc~region IS NOT NULL THEN adrc~region `
              &&                                    `ELSE CASE WHEN kna1~regio IS NOT NULL THEN kna1~regio `
              &&                                    `ELSE kna_sh~regio END `
              &&            `END AS ship_to_region,`.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'LIKP'
        langu     = sy-langu
      TABLES
        dfies_tab = lt_table_name.

    IF sy-saprl >= '754' AND line_exists( lt_table_name[ fieldname = 'WBSTK' ] ).
      lv_columns = lv_columns && `LIKP~WBSTK AS overall_goods_movement_status`  ##NO_TEXT.
      lv_value_for_status  =  `( likp~wbstk = 'C' )`  ##NO_TEXT.
    ELSE.
      lv_columns = lv_columns && `VBUK~WBSTK AS overall_goods_movement_status`  ##NO_TEXT.
      lv_value_for_status  =  `( vbuk~wbstk = 'C' )`  ##NO_TEXT.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
      EXPORTING
        input  = 'SH'
      IMPORTING
        output = lv_ship_to_party.

    "Retrieve deliveries/returns to deliveries with posted goods movements and applied filters
    "kunag: sold to party, kunnr: ship to party
    "wbstk: goods movement status
    "vbtyp j: deliveries, t: returns to deliveries
    SELECT DISTINCT (lv_columns)
     FROM likp LEFT JOIN vbuk ON likp~vbeln = vbuk~vbeln
               LEFT JOIN kna1 ON likp~kunnr = kna1~kunnr
                LEFT JOIN kna1 AS kna_sh ON likp~kunag = kna1~kunnr
                       JOIN vbpa ON likp~vbeln = vbpa~vbeln
                        JOIN adrc ON vbpa~adrnr = adrc~addrnumber
   WHERE likp~lfart    IN @ms_parameters-document_type
      AND likp~vkorg     IN @ms_parameters-sales_org
      AND likp~kunnr     IN @ms_parameters-ship_to_party
      AND likp~vbeln     IN @ms_parameters-vbeln
      AND likp~wadat_ist IN @ms_parameters-act_goods_mvt_date
      AND (lv_value_for_status)
      AND (    ( likp~vbtyp = 'J' AND vbpa~parvw  = @lv_ship_to_party )
            OR ( likp~vbtyp = 'T' AND vbpa~parvw  = @lv_ship_to_party )
            OR ( likp~vbtyp <> 'J' AND likp~vbtyp <> 'T' ) )
      AND likp~spe_loekz = @abap_false
      AND adrc~country   IN @ms_parameters-country
     INTO CORRESPONDING FIELDS OF TABLE @lt_delivery_hdr.

    IF lt_delivery_hdr IS NOT INITIAL.

      "Retrieve delivery items for retrieved headers
      SELECT lips~vbeln AS vbeln
        INTO TABLE @DATA(lt_delivery_item)
        FROM lips
               LEFT JOIN
                 t001w ON lips~werks = t001w~werks
        FOR ALL ENTRIES IN @lt_delivery_hdr
        WHERE lips~vbeln   = @lt_delivery_hdr-id
          AND lips~vtweg  IN @ms_parameters-distribution
          AND lips~spart  IN @ms_parameters-division
          AND t001w~land1 IN @ms_parameters-country
          AND lips~matnr  IN @ms_parameters-material
          AND lips~matkl  IN @ms_parameters-material_group
          AND lips~mtart  IN @ms_parameters-material_type
          AND lips~werks  IN @ms_parameters-plant
          AND lips~pstyv  IN @ms_parameters-category.  "#EC CI_BUFFJOIN

    ENDIF.

    IF lt_delivery_item IS NOT INITIAL.

      " Retrieve handling unit headers
      SELECT vekp~venum     AS internal_number_hu,
             vekp~vhilm     AS load_carrier,
             vekp~meins     AS load_carrier_unit,
             vekp~vbeln_gen AS delivery
        INTO CORRESPONDING FIELDS OF TABLE @mt_hu_headers
          FROM vekp
            FOR ALL ENTRIES IN @lt_delivery_item
              WHERE vekp~vbeln_gen = @lt_delivery_item-vbeln. "#EC CI_NOFIELD

      " Retrieve handling unit position and header data for retrieved deliveries
      SELECT vekp~vbeln_gen AS delivery,
             vekp~venum     AS internal_number_hu,
             vekp~vhilm     AS load_carrier,
             vekp~meins     AS load_carrier_unit,
             vepo~vepos     AS item_hu,
             vepo~venum     AS number_hu,
             vepo~veanz     AS pack_amount,
             vepo~unvel     AS low_level,
             vepo~matnr     AS product,
             vepo~vemng     AS packed_amount,
             vepo~vemeh     AS base_unit,
             vepo~pstyv     AS item_category
        INTO CORRESPONDING FIELDS OF TABLE @mt_handling_units
        FROM vekp
               LEFT JOIN
                 vepo ON vekp~venum = vepo~venum
        FOR ALL ENTRIES IN @lt_delivery_item
        WHERE vekp~vbeln_gen = @lt_delivery_item-vbeln.

    ENDIF.

    calculate_hu_distribution( ).

    mv_prepared = abap_true.

    rv_record_cnt = lines( mt_calculated_hu ).
    MESSAGE s047(/vpcoe/plm) WITH rv_record_cnt INTO DATA(lv_msg_str) ##NEEDED.
    mo_logger->add_messages(
        it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~process_package.
    DATA lv_top            TYPE i.
    DATA lt_handling_units TYPE /vpcoe/if_uph_entity_hu_proc=>gty_t_hu_for_product.

    " check if preparation did already take place
    IF mv_prepared = abap_false.
      /vpcoe/if_uph_entity_proc~prepare_process( ).
    ENDIF.

    " collect relevant keys for the current package
    lv_top = ( iv_act_package - 1 ) * iv_package_size + 1.

    DO iv_package_size TIMES.
      IF lv_top <= lines( mt_calculated_hu ).

        READ TABLE mt_calculated_hu INDEX lv_top REFERENCE INTO DATA(lr_hu).
        APPEND lr_hu->* TO lt_handling_units.

        lv_top = lv_top + 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    DATA(lo_entity_data) = retrieve_composition_data( lt_handling_units ).

    DATA(lt_packcomp_data) = VALUE /vpcoe/if_uph_entity_hu_proc=>gty_t_api_rp_packcomp_for_prod( ).

    LOOP AT lo_entity_data->get_product_assignments( ) REFERENCE INTO DATA(lr_prod_assignment).

      APPEND VALUE #(

        selection_criteria = VALUE #(  product_id = lr_prod_assignment->product_id
                                       business_process_direction = lr_prod_assignment->business_process_direction
                                       supplier_id = lr_prod_assignment->supplier_id )
        packaging_compositions = VALUE #( FOR <lf_pack_comp> IN lr_prod_assignment->packaging_compositions (
                                       id = <lf_pack_comp>-id
                                       valid_from = <lf_pack_comp>-valid_from
                                       valid_to = <lf_pack_comp>-valid_to
                                       supplier_id = <lf_pack_comp>-supplier_id
                                       business_process_direction = <lf_pack_comp>-business_process_direction
                                       ) )
        ) TO lt_packcomp_data.
    ENDLOOP.

    rt_entity_data = /vpcoe/if_uph_entity_hu_proc~map_hu_data(
                         it_packcomp_data  = lt_packcomp_data
                         it_handling_units = lt_handling_units ).

  ENDMETHOD.


  METHOD calculate_hu_distribution.

    DATA:
      lv_delivery          TYPE vbeln_gen,
      lv_act_share         TYPE p LENGTH 16 DECIMALS 6,
      lt_hu_items          TYPE gty_t_hu_resolved,
      lt_delivery_hu_items TYPE gty_t_hu_resolved,
      lt_packaged_products TYPE gty_t_product,
      lt_aux_products      TYPE gty_t_product.

    LOOP AT mt_hu_headers REFERENCE INTO DATA(lr_hu_header).

      " detect delivery group change
      IF lv_delivery IS INITIAL OR lv_delivery <> lr_hu_header->delivery.

        INSERT LINES OF lt_delivery_hu_items INTO TABLE lt_hu_items.

        lv_delivery = lr_hu_header->delivery.

        CLEAR lt_delivery_hu_items.

      ENDIF.

      CLEAR: lt_packaged_products,
             lt_aux_products.

      " lookup items
      resolve_hu_item( EXPORTING iv_hu              = lr_hu_header->internal_number_hu
                                 iv_subord_quantity = 1
                       CHANGING  ct_hu_items        = lt_delivery_hu_items
                                 ct_packed_products = lt_packaged_products
                                 ct_aux_products    = lt_aux_products ).
      READ TABLE lt_delivery_hu_items WITH TABLE KEY primary_key COMPONENTS internal_number = lr_hu_header->internal_number_hu REFERENCE INTO DATA(lr_hu_item).
      IF sy-subrc = 0.
        lr_hu_item->packed_products = lt_packaged_products.
      ENDIF.

    ENDLOOP.

    INSERT LINES OF lt_delivery_hu_items INTO TABLE lt_hu_items.

    "generate result matrix (product x handling unit / auxiliary product) and calculate share
    mt_calculated_hu = VALUE /vpcoe/if_uph_entity_hu_proc=>gty_t_hu_for_product( ).

    LOOP AT lt_hu_items REFERENCE INTO lr_hu_item.

      DATA(lv_total_packed_products) = 0.
      LOOP AT lr_hu_item->packed_products REFERENCE INTO DATA(lr_packed_product).
        lv_total_packed_products = lv_total_packed_products + lr_packed_product->quantity.
      ENDLOOP.

      LOOP AT lr_hu_item->packed_products REFERENCE INTO lr_packed_product.

        lv_act_share = lr_packed_product->quantity / lv_total_packed_products * lr_hu_item->amount.

        READ TABLE mt_calculated_hu WITH TABLE KEY primary_key COMPONENTS product = lr_packed_product->product
                                                                          handling_unit = lr_hu_item->handling_unit REFERENCE INTO DATA(lr_calculated_hu).
        IF sy-subrc = 0.
          lr_calculated_hu->amount     = lr_calculated_hu->amount + lv_act_share.
          lr_calculated_hu->prd_amount = lr_calculated_hu->prd_amount + lr_packed_product->quantity.
        ELSE.
          INSERT VALUE #( product       = lr_packed_product->product
                          handling_unit = lr_hu_item->handling_unit
                          amount        = lv_act_share
                          base_unit     = lr_packed_product->base_unit
                          prd_amount    = lr_packed_product->quantity ) INTO TABLE mt_calculated_hu.
        ENDIF.

        LOOP AT lr_hu_item->aux_products REFERENCE INTO DATA(lr_aux_product).
          lv_act_share = lr_packed_product->quantity / lv_total_packed_products * lr_aux_product->quantity.

          READ TABLE mt_calculated_hu WITH TABLE KEY primary_key COMPONENTS  product       = lr_packed_product->product
                                                                             handling_unit = lr_aux_product->product REFERENCE INTO lr_calculated_hu.
          IF sy-subrc = 0.

            lr_calculated_hu->amount     = lr_calculated_hu->amount + lv_act_share.
            lr_calculated_hu->prd_amount = lr_calculated_hu->prd_amount + lr_packed_product->quantity.
          ELSE.

            INSERT VALUE #( product       = lr_packed_product->product
                            handling_unit = lr_aux_product->product
                            amount        = lv_act_share
                            base_unit     = lr_aux_product->base_unit
                            prd_amount    = lr_packed_product->quantity ) INTO TABLE mt_calculated_hu.

          ENDIF.
        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    LOOP AT mt_calculated_hu REFERENCE INTO lr_calculated_hu.
      lr_calculated_hu->amount = lr_calculated_hu->amount / lr_calculated_hu->prd_amount.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.


  METHOD convert_to_base_unit.

    DATA lv_base_unit TYPE meins.

    rv_result = iv_quantity.

    lv_base_unit = determine_base_unit( iv_product ).

    IF iv_unit = lv_base_unit.
      RETURN.
    ENDIF.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE_OLD'
      EXPORTING
        input                = iv_quantity
        unit_in              = iv_unit
        unit_out             = lv_base_unit
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10.

    IF sy-subrc <> 0.

      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
        EXPORTING
          input                = iv_quantity
          kzmeinh              = abap_true
          matnr                = iv_product
          meinh                = iv_unit
          meins                = lv_base_unit
          werks                = iv_plant
        IMPORTING
          output               = rv_result
        EXCEPTIONS
          conversion_not_found = 1
          input_invalid        = 2
          material_not_found   = 3
          meinh_not_found      = 4
          meins_missing        = 5
          no_meinh             = 6
          output_invalid       = 7
          overflow             = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
        rv_result = iv_quantity.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD determine_base_unit.

    READ TABLE mt_product_units WITH KEY product = iv_product REFERENCE INTO DATA(lr_product_units).
    IF sy-subrc = 0.
      rv_result = lr_product_units->base_unit.
    ELSE.
      SELECT SINGLE meins FROM mara INTO @rv_result WHERE matnr = @iv_product.
      IF sy-subrc = 0.
        INSERT VALUE #( product = iv_product base_unit = rv_result ) INTO TABLE mt_product_units.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD resolve_hu_item.

    DATA lr_item TYPE REF TO gty_s_hu_resolved.

    LOOP AT mt_handling_units USING KEY primary_key REFERENCE INTO DATA(lr_handling_unit) WHERE internal_number_hu = iv_hu.

      DATA(lv_subord_quantity) = 1.

      READ TABLE ct_hu_items WITH TABLE KEY primary_key COMPONENTS internal_number = lr_handling_unit->internal_number_hu REFERENCE INTO lr_item.
      IF sy-subrc <> 0.

        DATA(ls_item) = VALUE gty_s_hu_resolved( handling_unit    = lr_handling_unit->load_carrier
                                         amount          = iv_subord_quantity * lv_subord_quantity
                                         base_unit       = lr_handling_unit->load_carrier_unit
                                         internal_number = lr_handling_unit->internal_number_hu
                                         delivery        = lr_handling_unit->delivery ).

        INSERT ls_item INTO TABLE ct_hu_items REFERENCE INTO lr_item.
      ENDIF.

      "collect handling unit / auxuliary product together with packed material
      IF lr_handling_unit->product IS NOT INITIAL.

        IF lr_handling_unit->item_category <> /vpcoe/cl_uph_proc_base_hu=>sc_item_type-addit_item
           AND lr_handling_unit->item_category <> /vpcoe/cl_uph_proc_base_hu=>sc_item_type-pack_mat.

          DATA(ls_packed_product) = VALUE gty_s_product(
                            product = lr_handling_unit->product
                            quantity = lr_handling_unit->packed_amount
                            base_unit = lr_handling_unit->base_unit
                             ).

          READ TABLE ct_packed_products WITH TABLE KEY primary_key COMPONENTS product = ls_packed_product-product base_unit = ls_packed_product-base_unit REFERENCE INTO DATA(lr_packed_product).
          IF sy-subrc = 0.
            lr_packed_product->quantity = lr_packed_product->quantity + ls_packed_product-quantity.
          ELSE.
            INSERT ls_packed_product INTO TABLE ct_packed_products.
          ENDIF.

        ELSE.

          lv_subord_quantity = lr_handling_unit->pack_amount.

          DATA(ls_aux_product) = VALUE gty_s_product(
                            product = lr_handling_unit->product
                            quantity = convert_to_base_unit( iv_quantity = iv_subord_quantity * lv_subord_quantity iv_product = lr_handling_unit->product iv_unit = lr_handling_unit->base_unit )
                            base_unit = determine_base_unit( iv_product = lr_handling_unit->product )
                            ).

          READ TABLE lr_item->aux_products WITH TABLE KEY primary_key COMPONENTS product = ls_aux_product-product base_unit = ls_aux_product-base_unit REFERENCE INTO DATA(lr_aux_product).
          IF sy-subrc <> 0.
            INSERT ls_aux_product INTO TABLE lr_item->aux_products.
          ENDIF.

        ENDIF.

      ENDIF.

      DATA(lv_lower_level) = lr_handling_unit->low_level.

      "look deeper
      resolve_hu_item( EXPORTING
                    iv_hu    = lv_lower_level
                    iv_subord_quantity = lv_subord_quantity
                   CHANGING
                      ct_hu_items = ct_hu_items
                      ct_packed_products = ct_packed_products
                      ct_aux_products    = ct_aux_products
                     ).

    ENDLOOP.

  ENDMETHOD.


  METHOD retrieve_composition_data.

    DATA: ls_json                  TYPE gty_s_hu_json,
          lv_json                  TYPE string,
          lr_response_data         TYPE REF TO data,
          ls_pack_comp             TYPE gty_s_value_packcomp,
          lt_messages              TYPE /vpcoe/t_uph_msg,
          lv_upload_target         TYPE /vpcoe/upload_target,
          lv_upload_target_version TYPE i.

    DATA lv_rfc_des  TYPE rfcdest.
    FIELD-SYMBOLS: <fs_rfc_des> TYPE rfcdest.

    DATA(lr_parameters) = /vpcoe/if_uph_entity_proc~get_parameters( ).
    IF lr_parameters IS BOUND.
      ASSIGN lr_parameters->('RFC_DES') TO <fs_rfc_des>.
      IF sy-subrc = 0.
        lv_rfc_des = <fs_rfc_des>.
      ENDIF.

      detect_upload_target(
        EXPORTING
          iv_rfcdest               = lv_rfc_des
        IMPORTING
          ev_upload_target         = lv_upload_target
          ev_upload_target_version = lv_upload_target_version ).
    ENDIF.

    DATA(lo_entity_mapper) = determine_fallback_mapper( iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_gprd
                                                        iv_upload_target         = lv_upload_target
                                                        iv_upload_target_version = lv_upload_target_version ).

    DATA(lt_prd_assignments) = VALUE /vpcoe/cl_uph_ent_pckg_cmp_gp=>lty_t_prd_assigments( FOR <ls_product> IN it_handling_units ( product_id = <ls_product>-product
                                                                                                                                  business_process_direction = 'ALL'
                                                                                                                                  supplier_id = ' ' ) ).
    SORT lt_prd_assignments BY product_id supplier_id business_process_direction.
    DELETE ADJACENT DUPLICATES FROM lt_prd_assignments.

    DATA(lo_entity_data) = NEW /vpcoe/cl_uph_ent_pckg_cmp_gp( is_sel_period = VALUE #( from               = ms_parameters-act_goods_mvt_date[ 1 ]-low
                                                                                       to                 = ms_parameters-act_goods_mvt_date[ 1 ]-high )
                                                                                       it_prd_assignments = lt_prd_assignments ).

    lv_json = lo_entity_mapper->prepare_payload(
          it_entity_data  = VALUE #( ( lo_entity_data ) )
          is_parameters   = ms_parameters ).

    DATA(lo_http_util) = /vpcoe/cl_uph_factory=>get_instance(  )->get_http_util( ).

    lo_http_util->get_http_client(
      EXPORTING
        iv_rfc_des     =  ms_parameters-rfc_des
        iv_uri_suffix  =  lo_entity_mapper->get_entity_url_suffix( )
        iv_request_method = 'POST' ).

    lo_http_util->post_data_to_api(
      EXPORTING
        iv_entity_data  = lv_json
      IMPORTING
        ev_code   = DATA(lv_code)
        ev_reason   = DATA(lv_reason)
        ev_response_txt = DATA(lv_response_txt)
    ).

    lo_http_util->close_connection(  ).

    lo_entity_mapper->evaluate_response(
                                         EXPORTING
                                           iv_response = lv_response_txt
                                           it_entity_data = VALUE #( ( lo_entity_data ) )
                                         IMPORTING
                                           et_messages = lt_messages ).
    IF NOT ( lv_code EQ 200 OR lv_code EQ 204 ).
      mo_logger->add_messages( it_messages = lt_messages ).
    ENDIF.

    ro_result = lo_entity_data.

  ENDMETHOD.


  method RETRIEVE_HU_DATA_TEMP.
  endmethod.
ENDCLASS.
