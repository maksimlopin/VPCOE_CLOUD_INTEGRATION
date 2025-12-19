class /VPCOE/CL_PCKCOMP_BOM_EXMPL definition
  public
  inheriting from /VPCOE/CL_UPH_PROC_BASE_BOM
  final
  create public .

public section.

  methods END_VALIDITY_PRODUCT .

  methods /VPCOE/IF_UPH_ENTITY_BOM_PROC~MAP_BOM_DATA
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_BOM_PROC~RETRIEVE_BOM_DATA
    redefinition .
protected section.

  types:
    BEGIN OF lty_class,
        class_id TYPE clint,
      END OF lty_class .
  types:
    ltty_class TYPE STANDARD TABLE OF lty_class WITH KEY class_id .

  constants MC_RELEVANT_CLASS_NAME type STRING value 'ZMCL_PACKELEM_ATTR' ##NO_TEXT.
  data MR_TCLA_MULTOBJ type ref to ABAP_BOOL .
  constants MC_COMPONENT_CLSFN_CLASS type KLASSE_D value 'ZRDP_PACKCOMP_ATTR' ##NO_TEXT.
  constants MC_SUBCOMPONENT_CLSFN_CLASS type KLASSE_D value 'ZRDP_PACKSUBC_ATTR' ##NO_TEXT.
  constants MC_PACKELEM_CLASS_NAME type KLASSE_D value 'ZMCL_PACKELEM_ATTR' ##NO_TEXT.
  constants MC_PACKCOMP_USAGE_CLSFN_ATTR type ATNAM value 'ZRDP_PACKCOMP_USAGE' ##NO_TEXT.
  constants MC_PACKCOMP_LEVEL_CLSFN_ATTR type ATNAM value 'ZRDP_PACKCOMP_LEVEL' ##NO_TEXT.
  constants MC_PACKCOMP_COVERAG_CLSFN_ATTR type ATNAM value 'ZRDP_PACKCOMP_COVERAGE' ##NO_TEXT.
  constants MC_PACKCOMP_SEPARAB_CLSFN_ATTR type ATNAM value 'ZRDP_PACKCOMP_SEPARABILITY' ##NO_TEXT.

  methods MAP_BOM_ITEMS
    importing
      !IT_BOM_ITEMS type /VPCOE/T_UPH_WRAP_BOM_ITEM
      !IO_PACKCOMP_HDR type ref to /VPCOE/CL_UPH_ENT_PCKG_CMP_HDR .
  methods IS_VALID_PACKCOMP_ITEM
    importing
      !IV_BOM_MATERIAL type MATNR
      !IV_BOM_MATL_TYPE type MTART
    returning
      value(RV_RESULT) type ABAP_BOOL .
private section.

  methods DETERMINE_MATCLAS_VALIDITY
    importing
      !IV_MATERIAL type MATNR
      !IV_BOM_ITEM_VALID_FROM type DATUV
    returning
      value(RV_RESULT) type DATUV .
  methods MAP_SUB_COMPONENTS
    importing
      !IT_BOM_ITEMS type /VPCOE/T_UPH_WRAP_BOM_ITEM
      !IO_PACKCOMP_ITEM type ref to /VPCOE/CL_UPH_ENT_PCKG_CMP_ITM .
ENDCLASS.



CLASS /VPCOE/CL_PCKCOMP_BOM_EXMPL IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_bom_proc~map_bom_data.
    DATA lo_packcomp TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_hdr.

    " loop all BOM header data
    LOOP AT it_bom_data INTO DATA(lo_bom_data).

      CLEAR lo_packcomp.

      DATA(ls_cmp_hdr_data) = VALUE /vpcoe/s_uph_ent_pack_cmp_hdr( source = ms_parameters-source_id ).
      ls_cmp_hdr_data-displayid           = |{ lo_bom_data->get_material( ) }-{ lo_bom_data->get_plant( ) }-{ lo_bom_data->get_bom_usage( ) }-{ lo_bom_data->get_bom_alternative_number( ) }-{ lo_bom_data->get_valid_from( ) }|.
      ls_cmp_hdr_data-description         = |{ lo_bom_data->get_material_description( ) }|.
      ls_cmp_hdr_data-basequantity        = lo_bom_data->get_base_amount( ).
      ls_cmp_hdr_data-baseunitofmeasureid = lo_bom_data->get_base_uom( ).
      ls_cmp_hdr_data-consumersalesunit   = lo_bom_data->get_material_base_uom( ).

      DATA(lt_products) = VALUE /vpcoe/t_uph_entity_data( ( NEW /vpcoe/cl_uph_ent_pckg_product(
                                                                      is_data = VALUE #(
                                                                      productid  = lo_bom_data->get_material( )
                                                                      valid_from = lo_bom_data->get_valid_from( )
                                                                      valid_to   = lo_bom_data->get_valid_to( )
                                                                      business_process_direction = 'ALL'
                                                                      supplier   = '' ) ) ) ).

      lo_packcomp = NEW /vpcoe/cl_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data = ls_cmp_hdr_data it_cmp_item_data = VALUE #( ) it_products = lt_products ).

      " map BOM items
      map_bom_items( it_bom_items            = lo_bom_data->get_items( )
                     io_packcomp_hdr         = lo_packcomp ).

      IF lo_packcomp->get_items( ) IS NOT INITIAL.

        INSERT lo_packcomp INTO TABLE rt_entity_data.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_bom_proc~retrieve_bom_data.
    TYPES:
      BEGIN OF lty_node_buffer,
        node_material TYPE matnr,
        items         TYPE REF TO /vpcoe/uph_wrap_bom_item,
      END OF lty_node_buffer,
      ltty_node_buffer TYPE HASHED TABLE OF lty_node_buffer WITH UNIQUE KEY node_material.

    DATA: lt_node_puffer    TYPE ltty_node_buffer,
          lt_exp_bom_items  TYPE TABLE OF stpox,
          lt_exp_bom_nodes  TYPE TABLE OF cscmat,
          ls_exp_bom_hdr    TYPE cstmat,
          lr_wrap_bom_items TYPE REF TO /vpcoe/uph_wrap_bom_item,
          lo_wrap_bom_item  TYPE REF TO /vpcoe/cl_uph_wrap_bom_item.

    LOOP AT it_bom_key REFERENCE INTO DATA(lr_bom_key).

      CLEAR: lt_node_puffer, ls_exp_bom_hdr, lt_exp_bom_items, lt_exp_bom_nodes,
             lr_wrap_bom_items, lo_wrap_bom_item.

*      mo_ext_modules_wrapper->call_bom_explosion(
*          EXPORTING
*            ir_bom_key       = lr_bom_key
*            is_parameters    = ms_parameters
*            io_logger        = mo_logger
*          IMPORTING
*            es_exp_bom_hdr   = ls_exp_bom_hdr
*            et_exp_bom_items = lt_exp_bom_items
*            et_exp_bom_nodes = lt_exp_bom_nodes
*      ).

      CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
        EXPORTING
          capid                 = /vpcoe/if_uph_entity_bom_proc=>gc_capid_mat_bom
          datuv                 = lr_bom_key->valfrdats
          mtnrv                 = lr_bom_key->material
          werks                 = lr_bom_key->plant
          mehrs                 = abap_true
          stpst                 = ms_parameters-max_exp_lvl
          stlal                 = lr_bom_key->stlal
          stlan                 = lr_bom_key->stlan
*         bom_versn             = ir_bom_key->bom_versn
        IMPORTING
          topmat                = ls_exp_bom_hdr
        TABLES
          stb                   = lt_exp_bom_items
          matcat                = lt_exp_bom_nodes
        EXCEPTIONS
          alt_not_found         = 1
          call_invalid          = 2
          material_not_found    = 3
          missing_authorization = 4
          no_bom_found          = 5
          no_plant_data         = 6
          no_suitable_bom_found = 7
          conversion_error      = 8
          OTHERS                = 9.
      IF ls_exp_bom_hdr IS NOT INITIAL.

        ls_exp_bom_hdr-datuv = lr_bom_key->valfr.
        ls_exp_bom_hdr-datub = lr_bom_key->valto.

        DATA mo_ext_modules_wrapper TYPE REF TO lif_ext_modules_wrapper."/vpcoe/uph_wrap_bom_item.
        mo_ext_modules_wrapper = NEW lcl_bom_expl_wrapper(  ).
        mo_ext_modules_wrapper->get_material_detail(
           EXPORTING
             iv_material = ls_exp_bom_hdr-matnr
             iv_plant = ls_exp_bom_hdr-werks
             io_logger = mo_logger
           IMPORTING
             es_material_general = DATA(ls_material_general)
             es_material_plant = DATA(ls_material_plant)
             es_material_valuation = DATA(ls_material_valuation)
        ).

        lr_wrap_bom_items = NEW #( ).
        DATA(lo_wrap_bom_hdr) = NEW /vpcoe/cl_uph_wrap_bom_hdr(
              is_header_material   = ls_exp_bom_hdr
              ir_items              = lr_wrap_bom_items
              is_material_general   = ls_material_general
              is_material_plant     = ls_material_plant
              is_material_valuation = ls_material_valuation
        ).
        INSERT VALUE #( node_material = ls_exp_bom_hdr-matnr
                        items         = lr_wrap_bom_items ) INTO TABLE lt_node_puffer.

        INSERT lo_wrap_bom_hdr INTO TABLE rt_bom_data.

        " Loop the items from BOM explosion
        LOOP AT lt_exp_bom_items REFERENCE INTO DATA(lr_exp_bom_items).

          lo_wrap_bom_item = NEW /vpcoe/cl_uph_wrap_bom_item( is_bom_item_data = lr_exp_bom_items->* ).

          " Is item a node -> add items table
          READ TABLE lt_exp_bom_nodes WITH KEY matnr = lo_wrap_bom_item->get_material( ) TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            lr_wrap_bom_items = NEW #( ).
            lo_wrap_bom_item->set_items( lr_wrap_bom_items ).
            INSERT VALUE #( node_material = lo_wrap_bom_item->get_material( ) items = lr_wrap_bom_items ) INTO TABLE lt_node_puffer.
          ENDIF.

          " Lookup parent material
          READ TABLE lt_exp_bom_nodes INDEX lr_exp_bom_items->ttidx REFERENCE INTO DATA(lr_exp_bom_nodes).
          IF sy-subrc = 0.

            " Lookup parent item table for bom item
            READ TABLE lt_node_puffer WITH KEY node_material = lr_exp_bom_nodes->matnr REFERENCE INTO DATA(lr_parent_items).
            IF sy-subrc = 0.
              APPEND lo_wrap_bom_item TO lr_parent_items->items->*.
            ENDIF.
          ENDIF.

          "Retrieve item batch classification
          retrieve_item_classification( ir_wrap_bom_item = lo_wrap_bom_item iv_keydate = lr_bom_key->valfr ).

        ENDLOOP.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD determine_matclas_validity.

    TYPES:
      BEGIN OF ltys_datuv,
        datuv TYPE datuv,
      END OF ltys_datuv,
      lty_t_datuv TYPE STANDARD TABLE OF ltys_datuv WITH DEFAULT KEY.

    DATA lt_result TYPE lty_t_datuv.

    "Check if multiple objects is active
    IF mr_tcla_multobj IS NOT BOUND.

      SELECT SINGLE multobj FROM tcla WHERE klart = '001' INTO @DATA(lv_multobj).
      mr_tcla_multobj = NEW abap_bool(  ).
      mr_tcla_multobj->* = lv_multobj.
    ENDIF.

    IF mr_tcla_multobj->* = abap_true.

      SELECT SINGLE cuobj FROM inob
        WHERE klart = '001' AND obtab  = 'MARA' AND objek = @iv_material
      INTO @DATA(lv_internal_matnr).                    "#EC CI_NOORDER

      IF lv_internal_matnr IS NOT INITIAL.
        SELECT DISTINCT ausp~datuv FROM ausp
         INNER JOIN inob ON ausp~objek = inob~cuobj
         INNER JOIN kssk ON kssk~objek = inob~cuobj AND kssk~mafid = 'O' AND kssk~klart = '001'
         INNER JOIN ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
         INNER JOIN klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
          WHERE ausp~klart = '001'
             AND ausp~lkenz = ''
             AND ausp~objek = @lv_internal_matnr
             AND klah~class = @mc_relevant_class_name
             AND ausp~datuv <= @iv_bom_item_valid_from
           ORDER BY ausp~datuv DESCENDING
           INTO TABLE @lt_result.
      ENDIF.

    ELSE.

      SELECT DISTINCT ausp~datuv FROM ausp
      INNER JOIN kssk ON kssk~objek = ausp~objek AND kssk~mafid = 'O' AND kssk~klart = '001'
      INNER JOIN ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
      INNER JOIN klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
      WHERE ausp~klart = '001'
        AND ausp~lkenz = ''
        AND ausp~objek = @iv_material
        AND klah~class = @mc_relevant_class_name
        AND ausp~datuv <= @iv_bom_item_valid_from
      ORDER BY ausp~datuv DESCENDING
      INTO TABLE @lt_result.

    ENDIF.

    IF lt_result IS NOT INITIAL.
      DATA(lv_datuv) = lt_result[ 1 ]-datuv.
      rv_result = COND #( WHEN lv_datuv = '00000000' THEN '00010101' ELSE lv_datuv ).
    ELSE.
      rv_result = iv_bom_item_valid_from.
    ENDIF.

  ENDMETHOD.


  METHOD end_validity_product.

    "Filling with test data.
    "The method is not currently used and is intended only as an example of sending.
    DATA: lt_products    TYPE /vpcoe/t_api_end_val_of_prod,
          ls_product_api TYPE /vpcoe/s_uph_api_eand_val_prod,
          lv_payload     TYPE string,
          lv_rfc_des     TYPE rfcdest,
          lv_uri_suffix  TYPE string,
          lv_api_type    TYPE /vpcoe/de_api_type,
          lt_messages    TYPE /vpcoe/t_uph_msg,
          lv_message     TYPE string,
          ls_payload     TYPE /vpcoe/s_jsn_cloud,
          lo_payload     TYPE REF TO /vpcoe/cl_rdp_payload_handler,
          lt_bapiret2    TYPE bapiret2_t.

    DATA(lo_cust) = NEW /vpcoe/cl_plm_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-plm
                                              iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-plm
                                              iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-product_val ).

    lt_products = VALUE #( ( product = 'TEST_MM_VS'
                             supplier                                                                                                                                                                = 'supplier1'
                             business_process_direction = 'INBOUND'
                             valid_to = '20231231' ) ).

    LOOP AT lt_products ASSIGNING FIELD-SYMBOL(<ls_product>).
      APPEND <ls_product> TO ls_product_api-elements.
    ENDLOOP.

    ls_product_api-source = lo_cust->get_source_id( ).

    lv_payload = /vpcoe/cl_plm_helper=>serialize_json( is_data        = ls_product_api
                                                       iv_compress    = abap_true
                                                       iv_pretty_name = /vpcoe/cl_plm_helper=>sc_pretty_mode-camel_case ).

    TRY.

        DATA(lo_rdp_http) = lo_cust->get_http_client( IMPORTING et_bapiret2 = lt_bapiret2 ).

      CATCH cx_oa2c INTO DATA(lx_oa2c).
        MESSAGE e003(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).

        MESSAGE e000(/vpcoe/common) WITH lx_oa2c->get_text( ) '' '' '' INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).
    ENDTRY.

    IF lo_rdp_http IS BOUND.
      "execute post with payload using util
      lo_rdp_http->send(
        EXPORTING
          iv_body     = lv_payload
          iv_method   = 'POST'
        IMPORTING
          ev_status   = DATA(lv_code)
          ev_reason   = DATA(lv_reason)
          es_response = DATA(ls_response_txt)  ).

      lo_rdp_http->close( ).
      IF lv_code <> /vpcoe/if_plm_constants=>gc_success_response.

        MESSAGE e020(/vpcoe/plm) WITH lv_code lv_reason INTO lv_message.
        APPEND VALUE #( msgty = sy-msgty
                        msgid = sy-msgid
                        msgno = sy-msgno
                        msgv1 = sy-msgv1
                        msgv2 = sy-msgv2 ) TO lt_messages.

        mo_logger->add_messages( it_messages = lt_messages ).
        mo_logger->add_http_response( EXPORTING is_response = ls_response_txt iv_reason = lv_reason ).

        lo_payload = NEW /vpcoe/cl_rdp_payload_handler( ).

        ls_payload = VALUE #( api_type         = lv_api_type
                              service_id       = lv_rfc_des
                              session_id       = /vpcoe/cl_plm_helper=>generate_session_id( )
                              json             = lv_payload
                              lines_count      = lines( lt_products )
                              response_message = lv_reason
                              response_status  = lv_code
                              created          = sy-datum
                              created_time     = sy-timlo
                              failed           = abap_true
                              repeated         = sy-datum ).

        DATA(lo_rdp_log) = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_log=>sc_log_sub_obj-plm  ).

        lo_payload->set_payload( EXPORTING iv_commit_work = abap_true
                                           iv_s_jsn_cloud = ls_payload
                                           io_log         = lo_rdp_log ).

        lo_rdp_log->get_messages( IMPORTING et_messages = DATA(lt_bapiret2_t) ).

        mo_logger->add_bapi_messages( lt_bapiret2_t ).

      ELSE.

        DATA(lv_entity_cnt) = lines( lt_products ).
        DATA(lv_upload_entity_dval) = VALUE dd07v(  ).
        DATA(lv_domvalue) = VALUE domvalue_l(  ).
        lv_domvalue = mv_upload_entity.

        " Get the upload entity text
        CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
          EXPORTING
            domname  = '/VPCOE/UPLOAD_ENTITY'
            value    = lv_domvalue
          IMPORTING
            dd07v_wa = lv_upload_entity_dval.

        MESSAGE s026(/vpcoe/plm) WITH lv_entity_cnt lv_upload_entity_dval-ddtext INTO lv_message.
        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD is_valid_packcomp_item.

    TYPES:
      BEGIN OF ltys_clint,
        clint TYPE clint,
      END OF ltys_clint,
      lty_t_clint TYPE STANDARD TABLE OF ltys_clint WITH DEFAULT KEY.

    DATA: lt_result TYPE lty_t_clint.

    IF iv_bom_matl_type = 'VERP'.

      "Check class type multiple object customizing is active
      IF mr_tcla_multobj IS NOT BOUND.

        SELECT SINGLE multobj FROM tcla WHERE klart = '001' INTO @DATA(lv_multobj).
        mr_tcla_multobj = NEW abap_bool(  ).
        mr_tcla_multobj->* = lv_multobj.
      ENDIF.

      "If yes, set Internal ID from table INOB as matnr.
      IF mr_tcla_multobj->* = abap_true.

        SELECT SINGLE cuobj FROM inob
          WHERE
            klart = '001' AND obtab = 'MARA' AND objek = @iv_bom_material
          INTO @DATA(lv_internal_matnr).                "#EC CI_NOORDER

        IF lv_internal_matnr IS NOT INITIAL.

          SELECT DISTINCT kssk~clint FROM kssk
           INNER JOIN klah ON kssk~clint = klah~clint
           WHERE
            klah~class = @mc_relevant_class_name AND kssk~objek = @lv_internal_matnr
            INTO TABLE @lt_result.
        ENDIF.

      ELSE.

        SELECT DISTINCT kssk~clint FROM kssk
        INNER JOIN klah ON kssk~clint = klah~clint
        WHERE
        klah~class = @mc_relevant_class_name AND kssk~objek = @iv_bom_material
          INTO TABLE @lt_result.

      ENDIF.

      IF lt_result IS NOT INITIAL.
        rv_result = abap_true.
      ELSE.
        rv_result = abap_false.
      ENDIF.
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD map_bom_items.
    " Recursive mapping of BOM items to packaging compositions and composition components

    DATA(lv_levelcode) = VALUE char10( ).
    DATA(lv_usage) = VALUE char10( ).

    LOOP AT it_bom_items INTO DATA(lo_bom_item).

      DATA(lv_item_category) = lo_bom_item->get_item_category( ).
      DATA(lv_material) = lo_bom_item->get_material( ).
      DATA(lv_material_type) = lo_bom_item->get_material_type( ).

      IF lv_item_category <> 'L' OR lv_material IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA lo_main_component_clsfn TYPE REF TO /vpcoe/cl_uph_wrap_mcl_version.
      CLEAR lo_main_component_clsfn.

      DATA(lo_bom_item_batch_clfn) = lo_bom_item->get_batch_clfn_data( ).
      IF lo_bom_item_batch_clfn IS BOUND.

        DATA(lt_clsfn_versions) = lo_bom_item_batch_clfn->get_versions_by_class( iv_class = mc_component_clsfn_class ).
        IF lt_clsfn_versions IS NOT INITIAL.
          lo_main_component_clsfn = lt_clsfn_versions[ 1 ].
        ENDIF.
      ENDIF.

      IF  is_valid_packcomp_item( iv_bom_material  = lo_bom_item->get_material( )
                                  iv_bom_matl_type = lv_material_type ).

        DATA(ls_packcomp_item_data) = VALUE /vpcoe/s_uph_ent_pack_cmp_item( ).

        ls_packcomp_item_data-packagingelementdisplayid = |{ lv_material }|.
        " get the correct material classification assignment valid from date
        DATA(lv_packcomp_item_valid_from) = determine_matclas_validity( iv_material            = lv_material
                                                                        iv_bom_item_valid_from = lo_bom_item->get_item_valid_from( ) ).

        ls_packcomp_item_data-packagingelementversion = |{ lv_packcomp_item_valid_from(4) }-{ lv_packcomp_item_valid_from+4(2) }-{ lv_packcomp_item_valid_from+6(2) }|.

        IF lo_bom_item->is_alternative_item( ).
          ls_packcomp_item_data-quantity = lo_bom_item->get_item_calculated_amount( ) * ( lo_bom_item->get_alt_item_usage_probability( ) / 100 ).
        ELSE.
          ls_packcomp_item_data-quantity = lo_bom_item->get_item_calculated_amount( ).
        ENDIF.

        ls_packcomp_item_data-quantityunitofmeasureid   = lo_bom_item->get_item_uom( ).

        CLEAR: lv_usage, lv_levelcode.
        IF lo_main_component_clsfn IS BOUND.

          lv_levelcode = lo_main_component_clsfn->get_neutral_val( mc_packcomp_level_clsfn_attr ).
          ls_packcomp_item_data-usage = lo_main_component_clsfn->get_neutral_val( mc_packcomp_usage_clsfn_attr ).
          ls_packcomp_item_data-coverage = lo_main_component_clsfn->get_number_val( mc_packcomp_coverag_clsfn_attr ).
        ENDIF.

        ls_packcomp_item_data-levelcode = COND #( WHEN lv_levelcode IS INITIAL
                                                  THEN '10'
                                                  ELSE lv_levelcode ).

        " check dimension of item UoM
        CALL FUNCTION 'DIMENSION_CHECK'
          EXPORTING
            dimid                  = 'AAAADL'
            msehi                  = lo_bom_item->get_item_uom( )
          EXCEPTIONS
            dimension_check_failed = 1
            unit_not_valid         = 2
            OTHERS                 = 3.
        IF sy-subrc = 0.
          " set count since item UoM has no dimension
          ls_packcomp_item_data-count  = lo_bom_item->get_item_amount( ).
        ENDIF.

        DATA(lo_packcomp_item) = NEW /vpcoe/cl_uph_ent_pckg_cmp_itm( is_data = ls_packcomp_item_data ).

        DATA(lt_cmp_item_data) = io_packcomp_hdr->get_items( ).
        INSERT lo_packcomp_item INTO TABLE lt_cmp_item_data.
        io_packcomp_hdr->set_items( lt_cmp_item_data ).

        IF lo_main_component_clsfn IS BOUND.

          " map sub component items
          map_sub_components(
            EXPORTING
                it_bom_items  = lo_bom_item->get_items( )
                io_packcomp_item = lo_packcomp_item
          ).
        ENDIF.

      ELSE.

        " go deeper
        map_bom_items(
          EXPORTING
              it_bom_items  = lo_bom_item->get_items( )
              io_packcomp_hdr = io_packcomp_hdr
        ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD map_sub_components.
    "Map BOM items as sub components of a packaging composition component

    LOOP AT it_bom_items INTO DATA(lo_bom_item).

      DATA(lv_item_category) = lo_bom_item->get_item_category( ).
      DATA(lv_material) = lo_bom_item->get_material( ).
      DATA(lv_material_type) = lo_bom_item->get_material_type( ).

      IF lv_item_category <> 'L' OR lv_material IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA lo_sub_component_clsfn TYPE REF TO /vpcoe/cl_uph_wrap_mcl_version.
      CLEAR lo_sub_component_clsfn.

      DATA(lo_bom_item_batch_clfn) = lo_bom_item->get_batch_clfn_data(  ).
      IF lo_bom_item_batch_clfn IS BOUND.

        DATA(lt_clsfn_versions) = lo_bom_item_batch_clfn->get_versions_by_class( iv_class = mc_subcomponent_clsfn_class ).
        IF lt_clsfn_versions IS NOT INITIAL.
          lo_sub_component_clsfn = lt_clsfn_versions[ 1 ].
        ENDIF.
      ENDIF.

      IF is_valid_packcomp_item( iv_bom_material  = lo_bom_item->get_material( )
                                 iv_bom_matl_type = lv_material_type ).

        DATA(ls_packcomp_subitem_data) = VALUE /vpcoe/s_ent_pack_cmp_subitm( ).

        ls_packcomp_subitem_data-packagingelementdisplayid = |{ lv_material }|.

        " get the correct material classification assignment valid from date
        DATA(lv_packcomp_item_valid_from) = determine_matclas_validity(
                                                iv_material            = lv_material
                                                iv_bom_item_valid_from = lo_bom_item->get_item_valid_from( ) ).

        " convert date to required format YYYY-MM-DD
        ls_packcomp_subitem_data-packagingelementversion = |{ lv_packcomp_item_valid_from(4) }-{ lv_packcomp_item_valid_from+4(2) }-{ lv_packcomp_item_valid_from+6(2) }|.

        IF lo_bom_item->is_alternative_item( ).
          ls_packcomp_subitem_data-quantity = lo_bom_item->get_item_calculated_amount( ) * ( lo_bom_item->get_alt_item_usage_probability( ) / 100 ).
        ELSE.
          ls_packcomp_subitem_data-quantity = lo_bom_item->get_item_calculated_amount( ).
        ENDIF.

        ls_packcomp_subitem_data-quantityunitofmeasureid = lo_bom_item->get_item_uom( ).

        IF lo_sub_component_clsfn IS BOUND.

          ls_packcomp_subitem_data-usage = lo_sub_component_clsfn->get_neutral_val( mc_packcomp_usage_clsfn_attr ).
          ls_packcomp_subitem_data-separability = lo_sub_component_clsfn->get_neutral_val( mc_packcomp_separab_clsfn_attr ).
        ENDIF.

        " check dimension of item UoM
        CALL FUNCTION 'DIMENSION_CHECK'
          EXPORTING
            dimid                  = 'AAAADL'
            msehi                  = lo_bom_item->get_item_uom( )
          EXCEPTIONS
            dimension_check_failed = 1
            unit_not_valid         = 2
            OTHERS                 = 3.
        IF sy-subrc = 0.
          " set count since item UoM has no dimension
          ls_packcomp_subitem_data-count = lo_bom_item->get_item_amount( ).
        ENDIF.

        DATA(lo_packcomp_item) = NEW /vpcoe/cl_uph_ent_pckg_cmp_sub( is_data = ls_packcomp_subitem_data ).

        DATA(lt_cmp_subitem_data) = io_packcomp_item->get_subitems( ).
        INSERT lo_packcomp_item INTO TABLE lt_cmp_subitem_data.
        io_packcomp_item->set_subitems( lt_cmp_subitem_data ).

      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
