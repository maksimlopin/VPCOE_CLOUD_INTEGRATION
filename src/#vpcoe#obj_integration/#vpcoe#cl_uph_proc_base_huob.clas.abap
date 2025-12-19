class /VPCOE/CL_UPH_PROC_BASE_HUOB definition
  public
  inheriting from /VPCOE/CL_UPH_PROC_BASE
  abstract
  create public .

public section.

  interfaces /VPCOE/IF_UPH_ENTITY_HU_PROC .

  methods RETRIEVE_HU_DATA_TMP .
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
    BEGIN OF gty_s_prd,
      material TYPE matnr,
      quantity TYPE lgmng,
    END OF gty_s_prd .
  types:
    BEGIN OF gty_s_uom,
      material TYPE matnr,
      uom      TYPE meins,
    END OF gty_s_uom .
  types:
    gty_t_uom TYPE SORTED TABLE OF gty_s_uom WITH NON-UNIQUE KEY material .
  types:
    gty_t_prd TYPE SORTED TABLE OF gty_s_prd WITH NON-UNIQUE KEY material .
  types:
    BEGIN OF gty_s_versions,
      objek TYPE cuobn,
      datuv TYPE datuv,
      datub type datub,
    END OF gty_s_versions .
  types:
    gty_t_versions TYPE STANDARD TABLE OF gty_s_versions WITH DEFAULT KEY .

  constants:
    BEGIN OF sc_item_type,
      addit_item TYPE pstyv VALUE 'HUPM',
      pack_mat   TYPE pstyv VALUE 'LEIH',
    END OF sc_item_type .
  data MS_PARAMETERS type /VPCOE/S_SELOPT_HU .
  data MT_DELIVERY_ITEM type /VPCOE/T_DELIVERY_ITEMS .
  data MT_HANDLING_UNITS type /VPCOE/T_HANDLING_UNIT .
  data MT_CALCULATED_HU type /VPCOE/T_HU_FOR_PRODUCT .
  data MT_RELVNT_HU type /VPCOE/IF_UPH_ENTITY_MCL_PROC=>GTY_T_MAT_CLASS .
  data MT_VERSIONS type GTY_T_VERSIONS .

  methods DETERMINE_DELIVERY .
  methods GET_PACKAGING_COMPOSITION
    importing
      !IV_JSON type STRING
    exporting
      !ES_DATA type /VPCOE/T_VALUE_PACK_COMP .
  methods DETERMINE_HU
    importing
      !IV_HU type VENUM optional
      !IV_COUNT type I optional
    exporting
      !EV_HANDLING_UNIT type MATNR .
  methods DETERMINE_HU_PARAMS .
PRIVATE SECTION.

  DATA ms_hu TYPE /vpcoe/s_hu_for_product .
  DATA mt_items TYPE /vpcoe/t_hu .
  DATA mv_prod TYPE char50 .
  DATA mc_pack_comp_product TYPE string VALUE '/GetPackagingCompositionsForProduct' ##NO_TEXT.
ENDCLASS.



CLASS /VPCOE/CL_UPH_PROC_BASE_HUOB IMPLEMENTATION.


  METHOD /VPCOE/IF_UPH_ENTITY_HU_PROC~MAP_HU_DATA.
    RETURN.
  ENDMETHOD.


  METHOD /VPCOE/IF_UPH_ENTITY_PROC~DESERIALIZE_SELECTION_PARAMS.

    CLEAR es_selection_params.

    DATA ls_selection_params TYPE /vpcoe/s_selopt_hu.

    /vpcoe/cl_plm_helper=>deserialize_json(
      EXPORTING
        iv_json   = iv_json_str
      CHANGING
        cs_data   = ls_selection_params ).

    es_selection_params = ls_selection_params.

  ENDMETHOD.


  METHOD /VPCOE/IF_UPH_ENTITY_PROC~GET_PARAMETERS.
    rv_result = REF #( ms_parameters ).
  ENDMETHOD.


  METHOD /VPCOE/IF_UPH_ENTITY_PROC~INIT_PROCESSOR.
    super->/vpcoe/if_uph_entity_proc~init_processor( iv_upload_entity = iv_upload_entity iv_upload_mode = iv_upload_mode ).

    "store the input parameter only in case it is a full load
    CLEAR ms_parameters.

    IF is_parameters IS NOT INITIAL AND mv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.

      MOVE-CORRESPONDING is_parameters TO ms_parameters.

    ENDIF.

    mv_initialized = abap_true.
  ENDMETHOD.


  METHOD /VPCOE/IF_UPH_ENTITY_PROC~PREPARE_PROCESS.

*    "This method is used to determine the relevant specifications to retrieve according the current
*    "upload mode and the given parameters for the upload entity
*    DATA: lv_params_string         TYPE string,
*          ls_input_params_frm_prot TYPE /vpcoe/s_selopt_hu,
*          lt_messages              TYPE /vpcoe/t_uph_msg,
*          lr_protocol_delta        TYPE REF TO /vpcoe/uph_prot,
*          lr_protocol_selection    TYPE REF TO /vpcoe/uph_prot.
*
*    IF mv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
*      DATA(lt_protocol)  =  /vpcoe/if_uph_entity_proc~read_from_protocol( iv_upload_entity = mv_upload_entity
*                                                                          iv_only_success  = abap_true ).
*      SORT lt_protocol BY start_timestamp DESCENDING.
*
*      "get last successful protocol entry, might be a full or delta load: relevant for delta timestamp
*      READ TABLE lt_protocol INDEX 1 REFERENCE INTO lr_protocol_delta.
*
*      "get last successful full load protocol: relevant for selection
*      READ TABLE lt_protocol REFERENCE INTO lr_protocol_selection WITH KEY upload_mode = /vpcoe/if_uph_entity_proc~gc_upload_mode-gc_upload_mode_full.
*
*      IF lr_protocol_selection IS INITIAL.
*
*        MESSAGE s018(/vpcoe/plm).
*        APPEND INITIAL LINE TO lt_messages ASSIGNING FIELD-SYMBOL(<fs_messages>).
*        IF <fs_messages> IS ASSIGNED.
*          <fs_messages>-msgty = /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_w.
*          <fs_messages>-msgid = /vpcoe/cl_uph_exec_load_wrap=>gc_default_msgclass.
*          <fs_messages>-msgno = 018.
*        ENDIF.
*        mo_logger->add_messages( it_messages = lt_messages ).
*        RETURN.
*
*      ELSE.
*
*        "deserialize parameter
*        lv_params_string = lr_protocol_selection->selection.
*        /vpcoe/if_uph_entity_proc~deserialize_selection_params( EXPORTING  iv_json_str = lv_params_string
*                                                                IMPORTING  es_selection_params = ls_input_params_frm_prot ).
*        ms_parameters = ls_input_params_frm_prot.
*      ENDIF.
*
*    ENDIF.
*
*    "Retrieve with filter criteria:
*    determine_hu_params( ).
*
*
*    CASE mv_upload_mode.
*      WHEN /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.
*
*      WHEN /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
*
*    ENDCASE.
*
*    mv_prepared = abap_true.
*
*    rv_record_cnt = lines( mt_hu ).
*    MESSAGE s047(/vpcoe/plm) WITH rv_record_cnt INTO DATA(lv_msg_str).
*    mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

  ENDMETHOD.


  METHOD /VPCOE/IF_UPH_ENTITY_PROC~PROCESS_PACKAGE.
*
**    DATA: lv_top            TYPE i,
**          lt_handling_units TYPE /vpcoe/t_hu_for_product.
**
**    "check if preparation did already take place
**    IF mv_prepared = abap_false.
**      /vpcoe/if_uph_entity_proc~prepare_process( ).
**    ENDIF.
**
**    "collect relevant keys for the current package
**    lv_top = ( iv_act_package - 1 ) * iv_package_size + 1.
**
**    DO iv_package_size TIMES.
**      IF lv_top <= lines( mt_hu ) .
**
**        READ TABLE mt_hu INDEX lv_top REFERENCE INTO DATA(lr_hu).
**        APPEND INITIAL LINE TO lt_handling_units REFERENCE INTO DATA(lr_handling_unit).
**        MOVE-CORRESPONDING lr_hu->* TO lr_handling_unit->*.
**
**        ADD 1 TO lv_top.
**      ELSE.
**        EXIT.
**      ENDIF.
**    ENDDO.
**
**    rt_entity_data = /vpcoe/if_uph_entity_hu_proc~map_hu_data( it_hu_data = /vpcoe/if_uph_entity_hu_proc~retrieve_hu_data( lt_handling_units )
**                                                               it_handling_units = lt_handling_units ).
**
**    IF rt_entity_data IS NOT INITIAL.
**      DATA(lv_pack_comp_count) = lines( rt_entity_data ).
**      DATA(lv_handl_unt_count) = lines( lt_handling_units ).
**
**      MESSAGE s121(/vpcoe/common) WITH lv_handl_unt_count lv_pack_comp_count INTO /vpcoe/cl_rdp_log=>sv_msg_text.
**      mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 ) ) ).
**    ENDIF.
*
*    DATA: lv_top            TYPE i,
*          lt_handling_units TYPE /vpcoe/t_hu_for_product.
*
*    " check if preparation did already take place
*    IF mv_prepared = abap_false.
*      /vpcoe/if_uph_entity_proc~prepare_process( ).
*    ENDIF.
*
*    " collect relevant keys for the current package
*    lv_top = ( iv_act_package - 1 ) * iv_package_size + 1.
*
*    DO iv_package_size TIMES.
*      IF lv_top <= lines( mt_calculated_hu ).
*
*        READ TABLE mt_calculated_hu INDEX lv_top REFERENCE INTO DATA(lr_hu).
*        APPEND lr_hu->* TO lt_handling_units.
*
*        lv_top = lv_top + 1.
*      ELSE.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    DATA(lo_entity_data) = retrieve_composition_data( lt_handling_units ).
*
*    DATA(lt_packcomp_data) = VALUE if_surdp_uph_entity_hu_proc=>gty_t_api_rp_packcomp_for_prod( ).
*
*    LOOP AT lo_entity_data->get_product_assignments( ) REFERENCE INTO DATA(lr_prod_assignment).
*
*      APPEND VALUE #(
*
*        selection_criteria = VALUE #(  product_id = lr_prod_assignment->product_id
*                                       business_process_direction = lr_prod_assignment->business_process_direction
*                                       supplier_id = lr_prod_assignment->supplier_id )
*        packaging_compositions = VALUE #( FOR <lf_pack_comp> IN lr_prod_assignment->packaging_compositions (
*                                       id = <lf_pack_comp>-id
*                                       valid_from = <lf_pack_comp>-valid_from
*                                       valid_to = <lf_pack_comp>-valid_to
*                                       supplier_id = <lf_pack_comp>-supplier_id
*                                       business_process_direction = <lf_pack_comp>-business_process_direction
*                                       ) )
*        ) TO lt_packcomp_data.
*    ENDLOOP.
*
*    rt_entity_data = if_surdp_uph_entity_hu_proc~map_hu_data(
*                         it_packcomp_data  = lt_packcomp_data
*                         it_handling_units = lt_handling_units ).

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    super->constructor(  ).

  ENDMETHOD.


METHOD DETERMINE_DELIVERY.

  DATA: lt_table_name       TYPE ddfields,
        lv_columns          TYPE string,
        lv_ship_to_party    TYPE string,
        lt_delivery_hdr	    TYPE /vpcoe/t_delivery_hdr,
        lv_value_for_status TYPE string.

  lv_columns = `likp~vbeln     AS id,`  ##NO_TEXT
            && `likp~lfart     AS type,`  ##NO_TEXT
            && `likp~vkorg     AS sales_organization,`  ##NO_TEXT
            && `likp~kunnr     AS ship_to_party,`  ##NO_TEXT
            && `likp~erdat     AS create_date,`  ##NO_TEXT
            && `likp~aedat     AS change_date,`  ##NO_TEXT
            && `likp~wadat_ist AS actual_goods_movement_date,`  ##NO_TEXT
            && `likp~inco1     AS incoterms,`  ##NO_TEXT
            && `CASE WHEN likp~kunag = ' ' THEN likp~kunnr `  ##NO_TEXT
            &&                            `ELSE likp~kunag END AS sold_to_party, `  ##NO_TEXT
            && `CASE WHEN adrc~country IS NOT NULL THEN adrc~country `  ##NO_TEXT
            &&                                     `ELSE CASE WHEN kna1~land1 IS NOT NULL THEN kna1~land1 `  ##NO_TEXT
            &&                                                                           `ELSE kna_sh~land1 END ` ##NO_TEXT
            &&            `END AS ship_to_country, `  ##NO_TEXT
            && `CASE WHEN adrc~region IS NOT NULL THEN adrc~region `  ##NO_TEXT
            &&                                    `ELSE CASE WHEN kna1~regio IS NOT NULL THEN kna1~regio `  ##NO_TEXT
            &&                                    `ELSE kna_sh~regio END `  ##NO_TEXT
            &&            `END AS ship_to_region,`  ##NO_TEXT.

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

  SELECT DISTINCT (lv_columns)
         FROM likp JOIN vbuk ON likp~vbeln = vbuk~vbeln
                   LEFT JOIN lips ON likp~vbeln = lips~vbeln
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

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SELECT lips~vbeln AS vbeln,
         lips~posnr AS id,
         lips~pstyv AS category,
         lips~matnr AS product,
         lips~lgmng AS base_quantity,
         lips~meins AS base_unit,
         lips~lfimg AS sales_quantity,
         lips~vrkme AS sales_unit,
         lips~charg AS batch,
         lips~werks AS plant,
         lips~vtweg AS distribution_channel,
         lips~spart AS division,
         lips~lgort AS lgort
     INTO CORRESPONDING FIELDS OF TABLE @mt_delivery_item
    FROM lips LEFT JOIN t001w ON lips~werks = t001w~werks
      FOR ALL ENTRIES IN @lt_delivery_hdr
         WHERE lips~vbeln = @lt_delivery_hdr-id
           AND lips~vtweg  IN @ms_parameters-distribution
           AND lips~spart  IN @ms_parameters-division
           AND t001w~land1 IN @ms_parameters-plnt_country
           AND lips~matkl IN @ms_parameters-material_group
           AND lips~mtart IN @ms_parameters-material_type
           AND lips~werks IN @ms_parameters-plant
           AND lips~pstyv IN @ms_parameters-category.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDMETHOD.


  METHOD DETERMINE_HU.
*    DATA: lv_hu    TYPE venum,
*          ls_items TYPE /vpcoe/s_hu.
*
*    CLEAR: ev_handling_unit.
*
*    LOOP AT mt_handling_units ASSIGNING FIELD-SYMBOL(<ls_hu>) WHERE internal_number_hu = iv_hu.
*
*      IF <ls_hu>-low_level IS NOT INITIAL.
*        me->determine_hu( EXPORTING iv_hu    = <ls_hu>-low_level
*                                    iv_count = <ls_hu>-pack_amount ).
*
*        IF <ls_hu>-item_category <> /vpcoe/cl_uph_proc_base_hu=>sc_item_type-addit_item
*            AND <ls_hu>-item_category <> /vpcoe/cl_uph_proc_base_hu=>sc_item_type-pack_mat AND <ls_hu>-mat_number IS INITIAL.
*
*          ev_handling_unit = <ls_hu>-load_carrier.
*        ENDIF.
*
*        DELETE TABLE mt_handling_units FROM <ls_hu>.
*
*      ELSE.
*        IF <ls_hu>-mat_number IS NOT INITIAL AND <ls_hu>-item_category <> /vpcoe/cl_uph_proc_base_hu=>sc_item_type-addit_item
*                                             AND <ls_hu>-item_category <> /vpcoe/cl_uph_proc_base_hu=>sc_item_type-pack_mat.
*          mv_prod = <ls_hu>-mat_number.
*          ls_items = VALUE #( handling_unit = <ls_hu>-load_carrier
*                              amount        = iv_count
*                              uom           = <ls_hu>-uom ).
*        ELSE.
*          ls_items = VALUE #( handling_unit = <ls_hu>-mat_number
*                              amount        = <ls_hu>-pack_amount
*                              uom           = <ls_hu>-uom ).
*        ENDIF.
*        COLLECT ls_items INTO mt_items.
*        DELETE TABLE mt_handling_units FROM <ls_hu>.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.


  METHOD DETERMINE_HU_PARAMS.
*
*    DATA: lt_hu_product        TYPE /vpcoe/t_handling_unit_comp,
*          ls_prd               TYPE gty_s_prd,
*          lt_prd               TYPE gty_t_prd,
*          ls_items             TYPE /vpcoe/s_hu,
*          lv_internal_number   TYPE venum,
*          lv_nadling_unit_name TYPE vhilm,
*          lv_delivery          TYPE vbeln_gen.
*
*    me->determine_delivery( ).
*
*    IF mt_delivery_item IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    SELECT vekp~vbeln_gen AS delivery,
*           vekp~venum     AS internal_number_hu,
*           vepo~vepos     AS item_hu,
*           vepo~venum     AS number_hu,
*           vepo~vemng     AS packed_amount,
*           vepo~pstyv     AS item_category,
*           vepo~matnr     AS mat_number,
*           vepo~veanz     AS pack_amount,
*           vepo~altme     AS uom,
*           vepo~unvel     AS low_level,
*           vekp~vhilm     AS load_carrier
*      INTO CORRESPONDING FIELDS OF TABLE @mt_handling_units
*         FROM vekp LEFT JOIN vepo ON vekp~venum = vepo~venum
*           FOR ALL ENTRIES IN @mt_delivery_item
*           WHERE vekp~vbeln_gen = @mt_delivery_item-vbeln.
*
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    LOOP AT mt_handling_units ASSIGNING FIELD-SYMBOL(<ls_handling_unit>) WHERE item_category <> /vpcoe/cl_uph_proc_base_hu=>sc_item_type-addit_item
*                                                                           AND item_category <> /vpcoe/cl_uph_proc_base_hu=>sc_item_type-pack_mat.
*      CLEAR: mv_prod, mt_items, lv_nadling_unit_name.
*
*      IF line_exists( mt_handling_units[ KEY level COMPONENTS low_level = <ls_handling_unit>-internal_number_hu ] ).
*        CONTINUE.
*      ENDIF.
*
*      IF lv_internal_number <> <ls_handling_unit>-internal_number_hu AND <ls_handling_unit>-mat_number IS INITIAL.
*
*        lv_internal_number = <ls_handling_unit>-internal_number_hu.
*        ls_items = VALUE #( handling_unit = <ls_handling_unit>-load_carrier
*                            amount        = 1 ).
*        COLLECT ls_items INTO mt_items.
*
*      ELSEIF lv_internal_number <> <ls_handling_unit>-internal_number_hu AND <ls_handling_unit>-mat_number IS NOT INITIAL.
*
*        mv_prod = <ls_handling_unit>-mat_number.
*        ls_items = VALUE #( handling_unit = <ls_handling_unit>-load_carrier
*                            amount         = 1 ).
*        COLLECT ls_items INTO mt_items.
*
*      ENDIF.
*
*      lv_delivery = <ls_handling_unit>-delivery.
*
*      me->determine_hu(  EXPORTING iv_hu            = COND #( WHEN <ls_handling_unit>-low_level IS INITIAL THEN <ls_handling_unit>-internal_number_hu
*                                                              ELSE <ls_handling_unit>-low_level )
*                                   iv_count         = <ls_handling_unit>-pack_amount
*                         IMPORTING ev_handling_unit = lv_nadling_unit_name ).
*
*      IF lv_nadling_unit_name IS NOT INITIAL.
*        ls_items = VALUE #( handling_unit = lv_nadling_unit_name
*                            amount        = <ls_handling_unit>-pack_amount ).
*        COLLECT ls_items INTO mt_items.
*
*      ENDIF.
*      lt_hu_product = VALUE #( BASE lt_hu_product FOR <ls_hu> IN mt_items
*                                ( product       = mv_prod
*                                  handling_unit = <ls_hu>-handling_unit
*                                  amount        = <ls_hu>-amount
*                                  uom           = <ls_hu>-uom
*                                  delivery      = lv_delivery ) ).
*
*      IF <ls_handling_unit> IS ASSIGNED.
*        DELETE TABLE mt_handling_units FROM <ls_handling_unit>.
*      ENDIF.
*
*    ENDLOOP.
*
*    LOOP AT mt_delivery_item ASSIGNING FIELD-SYMBOL(<ls_item>).
*      IF ( line_exists( mt_delivery_item[ KEY vbeln_itm_cat COMPONENTS vbeln    = <ls_item>-vbeln
*                                                                       category = /vpcoe/cl_uph_proc_base_hu=>sc_item_type-addit_item ] )
*        OR line_exists( mt_delivery_item[ KEY vbeln_itm_cat COMPONENTS vbeln    = <ls_item>-vbeln
*                                                                       category = /vpcoe/cl_uph_proc_base_hu=>sc_item_type-pack_mat ] ) ).
*
*        ls_prd-material = <ls_item>-product.
*        ls_prd-quantity = <ls_item>-base_quantity.
*        COLLECT ls_prd INTO lt_prd.
*      ENDIF.
*
*    ENDLOOP.
*
*    CLEAR: mt_delivery_item.
*
*    mt_hu = VALUE /vpcoe/t_hu_for_product( FOR GROUPS <ls_group_key> OF <ls_group> IN lt_hu_product GROUP BY ( product = <ls_group>-product
*                                                                                                               handling_unit = <ls_group>-handling_unit )
*                      LET coll_line = REDUCE /vpcoe/s_hu_for_product( INIT line TYPE /vpcoe/s_hu_for_product FOR <ls_hu_prod> IN GROUP <ls_group_key>
*                      NEXT line-product       = <ls_hu_prod>-product
*                           line-handling_unit = <ls_hu_prod>-handling_unit
*                           line-amount        = line-amount + <ls_hu_prod>-amount
*                           line-uom           = <ls_hu_prod>-uom
*                           line-prd_amount    = <ls_hu_prod>-prd_amount ) IN ( coll_line ) ).
*
*    CLEAR: lt_hu_product.
*
*    LOOP AT mt_hu ASSIGNING FIELD-SYMBOL(<ls_handling_units>).
*      ASSIGN lt_prd[ material = <ls_handling_units>-product ] TO FIELD-SYMBOL(<ls_prd>).
*      IF sy-subrc = 0.
*        <ls_handling_units>-amount = <ls_handling_units>-amount / <ls_prd>-quantity.
*      ENDIF.
*    ENDLOOP.
*
*    CLEAR: lt_prd.

  ENDMETHOD.


  METHOD GET_PACKAGING_COMPOSITION.
    DATA: lr_response_data TYPE REF TO data.

    CLEAR: es_data.

    DATA(lo_cust) = NEW /vpcoe/cl_plm_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-plm
                                              iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-plm
                                              iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-packaging_composition ).

    TRY.
        DATA(lo_rdp_http) = NEW /vpcoe/cl_plm_http( iv_url      = lo_cust->get_service_url( )
                                                    iv_rfc_name = CONV #( ms_parameters-rfc_des )
                                                    io_logger   = mo_logger ).

      CATCH cx_oa2c INTO DATA(lx_oa2c).
        MESSAGE e003(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).

        MESSAGE e000(/vpcoe/common) WITH lx_oa2c->get_text( ) '' '' '' INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).
    ENDTRY.

    IF mo_logger->has_error_messages( ).
      RETURN.
    ENDIF.

    lo_rdp_http->send(
      EXPORTING
        iv_body     = iv_json
        iv_method   = 'POST'
      IMPORTING
        ev_status   = DATA(lv_code)
        ev_reason   = DATA(lv_reason)
        es_response = DATA(ls_response_txt)
        ev_orig_response = DATA(lv_orig_response)
     ).

    /vpcoe/cl_plm_helper=>deserialize_json(
          EXPORTING
            iv_json    = lv_orig_response
          CHANGING
            cs_data    = es_data  ).

  ENDMETHOD.


  METHOD RETRIEVE_HU_DATA_TMP.

*    DATA: ls_json TYPE /vpcoe/str_hu_json,
*          lv_json TYPE string.
*
*    ls_json = VALUE #( source = 'EDW'
*                       selection_period-from = ms_parameters-actual_goods_movement_date[ 1 ]-low
*                       selection_period-to = ms_parameters-actual_goods_movement_date[ 1 ]-high
*                       product_assignments = VALUE #( FOR <ls_product> IN it_handling_units ( product_id = <ls_product>-product
*                                                                                              business_process_direction = 'ALL'
*                                                                                              supplier_id = ' ' ) ) ).
*
*    lv_json = /vpcoe/cl_plm_helper=>serialize_json( EXPORTING is_data = ls_json
*                                                              iv_pretty_name    = /vpcoe/cl_plm_helper=>sc_pretty_mode-camel_case
*                                                              iv_numc_as_string = abap_true ).
*
*    REPLACE ALL OCCURRENCES OF ':""' IN lv_json WITH ': null' ##no_text.
*
*    me->get_packaging_composition( EXPORTING iv_json = lv_json
*                                   IMPORTING es_data = DATA(ls_pack_comp) ).
*
*    IF ls_pack_comp IS NOT INITIAL.
*      rt_hu_data = ls_pack_comp-value.
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
