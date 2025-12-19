class /VPCOE/CL_PCKCOMP_HU_EXMPL definition
  public
  inheriting from /VPCOE/CL_UPH_PROC_BASE_HU
  final
  create public .

public section.

  methods /VPCOE/IF_UPH_ENTITY_HU_PROC~MAP_HU_DATA
    redefinition .
protected section.

  constants MC_RELEVANT_CLASS_NAME type STRING value 'ZMCL_PACKELEM_ATTR' ##NO_TEXT.
private section.

  types:
    BEGIN OF gty_s_versions,
        objek TYPE cuobn,
        datuv TYPE datuv,
        datub TYPE datub,
      END OF gty_s_versions .
  types:
    gty_t_versions TYPE STANDARD TABLE OF gty_s_versions WITH DEFAULT KEY .

  data MR_TCLA_MULTOBJ type ref to ABAP_BOOL .
  data MT_VERSIONS type GTY_T_VERSIONS .

  methods DIMENSION_CHECK
    importing
      !IV_UOM type UNIT
    returning
      value(RV_UOM) type ABAP_BOOL .
  methods GET_VERSIONS .
  methods GET_PACKAGING_ELEMENT_VERSION
    importing
      !IV_PRODUCT type CUOBN
      !IV_VALID_FROM type DATS
      !IV_VALID_TO type DATS
    returning
      value(RV_VERSION) type CHAR40 .
  methods RETRIEVE_COMPOSTION_DATA_TMP .
ENDCLASS.



CLASS /VPCOE/CL_PCKCOMP_HU_EXMPL IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_hu_proc~map_hu_data.
    DATA lt_entity_comp       TYPE /vpcoe/t_uph_entity_data.
    DATA lt_entity_comp_items TYPE /vpcoe/t_uph_entity_data.
    DATA lt_entity_products   TYPE /vpcoe/t_uph_entity_data.

    get_versions( ).

    " map calculated handling unit data to packaging composition item entities
    LOOP AT it_packcomp_data REFERENCE INTO DATA(lr_packcomp_data).

      IF lr_packcomp_data->packaging_compositions IS INITIAL.

        MESSAGE s049(/vpcoe/plm) WITH lr_packcomp_data->selection_criteria-product_id INTO DATA(lv_message) ##NEEDED.
        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).
        CONTINUE.
      ENDIF.

      "iterate over existing packaging compositions in RDP
      LOOP AT lr_packcomp_data->packaging_compositions REFERENCE INTO DATA(lr_packaging_composition).

        CLEAR: lt_entity_comp_items, lt_entity_products.

        APPEND INITIAL LINE TO lt_entity_products REFERENCE INTO DATA(lr_entity_product).
        lr_entity_product->* = NEW /vpcoe/cl_uph_ent_pckg_product( is_data    = VALUE #( productid                  = lr_packcomp_data->selection_criteria-product_id
                                                                                         valid_from                 = lr_packaging_composition->valid_from
                                                                                         valid_to                   = lr_packaging_composition->valid_to
                                                                                         business_process_direction = lr_packcomp_data->selection_criteria-business_process_direction
                                                                                         supplier                   = lr_packcomp_data->selection_criteria-supplier_id )
                                                                   iv_deleted = abap_false ).

        "map calculated handling unit data to composition as additional item
        LOOP AT it_handling_units REFERENCE INTO DATA(lr_handling_unit) WHERE product = lr_packcomp_data->selection_criteria-product_id.

          APPEND INITIAL LINE TO lt_entity_comp_items REFERENCE INTO DATA(lr_comp_item).
          lr_comp_item->* = NEW /vpcoe/cl_uph_ent_pckg_cmp_itm( is_data    = VALUE #( packagingelementdisplayid = lr_handling_unit->handling_unit
                                                                                      levelcode                 = '30'
                                                                                      quantity                  = lr_handling_unit->amount
                                                                                      quantityunitofmeasureid   = lr_handling_unit->base_unit
                                                                                      count                     = COND #( WHEN me->dimension_check( lr_handling_unit->base_unit ) = abap_true
                                                                                                                          THEN lr_handling_unit->amount
                                                                                                                          ELSE 0 )
                                                                                      usage                     = 'H'
                                                                                      packagingelementversion   = get_packaging_element_version( iv_product    = lr_handling_unit->handling_unit
                                                                                                                                                 iv_valid_from = lr_packaging_composition->valid_from
                                                                                                                                                 iv_valid_to   = lr_packaging_composition->valid_to ) )
                                                               iv_deleted = abap_false ).
        ENDLOOP.

        IF lt_entity_comp_items IS NOT INITIAL.

          APPEND INITIAL LINE TO lt_entity_comp REFERENCE INTO DATA(lr_entity_comp).
          lr_entity_comp->* = NEW /vpcoe/cl_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = VALUE #( displayid = lr_packaging_composition->id )
                                                                  it_cmp_item_data = lt_entity_comp_items
                                                                  it_products      = lt_entity_products
                                                                  iv_deleted       = abap_false ).
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    rt_entity_data = lt_entity_comp.
  ENDMETHOD.


  METHOD dimension_check.
    CLEAR rv_uom.

    CALL FUNCTION 'DIMENSION_CHECK'
      EXPORTING
        dimid                  = 'AAAADL'
        msehi                  = iv_uom
      EXCEPTIONS
        dimension_check_failed = 1
        unit_not_valid         = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.
      rv_uom = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_packaging_element_version.
    DATA lv_version TYPE datuv.

    LOOP AT mt_versions ASSIGNING FIELD-SYMBOL(<ls_version>) WHERE     objek  = iv_product
                                                                   AND datuv <= iv_valid_from
                                                                   AND datub >= iv_valid_to.
      lv_version = <ls_version>-datuv.

    ENDLOOP.

    IF lv_version = '00000000'.
      lv_version = '00010101'.
    ENDIF.

    rv_version = |{ lv_version(4) }-{ lv_version+4(2) }-{ lv_version+6(2) }|.
  ENDMETHOD.


  METHOD get_versions.

    " get the relevant versions via checking for ZMCL_PACKELEM_ATTR and also check for multiple object type classification setting
    TYPES: BEGIN OF ltys_cuobj,
             cuobj TYPE cuobj,
           END OF ltys_cuobj,
           lty_t_cuobj TYPE STANDARD TABLE OF ltys_cuobj WITH DEFAULT KEY.

    DATA lt_internal_matnr TYPE lty_t_cuobj.

    " Check class type multiple object customizing is active
    IF mr_tcla_multobj IS NOT BOUND.
      SELECT SINGLE multobj FROM tcla WHERE klart = '001' INTO @DATA(lv_multobj).
      mr_tcla_multobj = NEW abap_bool( ).
      mr_tcla_multobj->* = lv_multobj.
    ENDIF.

    " when multobj is enabled
    IF mr_tcla_multobj->* = abap_true.

      LOOP AT mt_calculated_hu REFERENCE INTO DATA(lr_hu).
        SELECT cuobj FROM inob
          WHERE klart = '001' AND obtab = 'MARA' AND objek = @lr_hu->handling_unit
          INTO CORRESPONDING FIELDS OF TABLE @lt_internal_matnr.
      ENDLOOP.

      LOOP AT lt_internal_matnr REFERENCE INTO DATA(lr_internal_matnr).
        SELECT ausp~datuv,
               ausp~datub,
               ausp~objek
          FROM ausp
            INNER JOIN inob ON ausp~objek = inob~cuobj
            INNER JOIN kssk ON kssk~objek = inob~cuobj AND kssk~mafid = 'O' AND kssk~klart = '001'
            INNER JOIN ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
            INNER JOIN klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
          WHERE ausp~klart = '001'
            AND ausp~lkenz = ''
            AND ausp~objek = @lr_internal_matnr->cuobj
            AND klah~class = @mc_relevant_class_name
          INTO CORRESPONDING FIELDS OF TABLE @mt_versions.
      ENDLOOP.
      " when multobj is disabled
    ELSE.
      SELECT ausp~datuv,
             ausp~datub,
             ausp~objek
        FROM ausp
          INNER JOIN kssk ON kssk~objek = ausp~objek AND kssk~mafid = 'O' AND kssk~klart = '001'
          INNER JOIN ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
          INNER JOIN klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
        FOR ALL ENTRIES IN @mt_calculated_hu
        WHERE ausp~klart = '001'
          AND ausp~lkenz = ''
          AND ausp~objek = @mt_calculated_hu-handling_unit
          AND klah~class = @mc_relevant_class_name
        INTO CORRESPONDING FIELDS OF TABLE @mt_versions.

    ENDIF.
  ENDMETHOD.


  METHOD retrieve_compostion_data_tmp.

*    DATA: ls_json          TYPE gty_s_hu_json,
*          lv_json          TYPE string,
*          lr_response_data TYPE REF TO data,
*          ls_pack_comp     TYPE gty_s_value_packcomp..
*
*    DATA(lt_product_assignment) = VALUE gty_t_prd_assigments( FOR <ls_product> IN it_handling_units ( product_id = <ls_product>-product
*                                                                                                      business_process_direction = 'ALL'
*                                                                                                      supplier_id = ' ' ) ).
*
*    SORT lt_product_assignment BY product_id business_process_direction supplier_id.
*    DELETE ADJACENT DUPLICATES FROM lt_product_assignment.
*
*    ls_json = VALUE #( source                = ms_parameters-source_id
*                       selection_period-from = ms_parameters-actual_goods_movement_date[ 1 ]-low
*                       selection_period-to   = ms_parameters-actual_goods_movement_date[ 1 ]-high
*                       product_assignments   = lt_product_assignment ).
*
*    lv_json = /ui2/cl_json=>serialize( EXPORTING data          = ls_json
*                                                 pretty_name   = 'X'
*                                                 ts_as_iso8601 = abap_true ).
*
*    REPLACE ALL OCCURRENCES OF ':""' IN lv_json WITH ': null' ##no_text.
*
*    TRY.
*        DATA(lo_rdp_http) = NEW /vpcoe/cl_plm_http( iv_url      = mc_pack_comp_product
*                                                    iv_rfc_name = CONV #( ms_parameters-rfc_des )
*                                                    io_logger   = mo_logger ).
*
*      CATCH cx_oa2c INTO DATA(lx_oa2c).
*        MESSAGE e003(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
*        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).
*
*        MESSAGE e000(/vpcoe/common) WITH lx_oa2c->get_text( ) '' '' '' INTO /vpcoe/cl_rdp_log=>sv_msg_text.
*        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).
*    ENDTRY.
*
*    "execute post with payload using util
*    lo_rdp_http->send(
*      EXPORTING
*        iv_body     = lv_json
*        iv_method   = 'POST'
*      IMPORTING
*        ev_status   = DATA(lv_code)
*        ev_reason   = DATA(lv_reason)
*        es_response = DATA(ls_response_txt)
*        ev_orig_response = DATA(lv_orig_response)  ).
*
*    lo_rdp_http->close( ).
*
*    IF lv_code = 200.
*      /ui2/cl_json=>deserialize(
*            EXPORTING
*              json    = lv_orig_response
*              pretty_name = 'X'
*            CHANGING
*              data    = ls_pack_comp  ).
*
*
*      IF ls_pack_comp IS NOT INITIAL.
*        rt_hu_data = ls_pack_comp-value.
*      ENDIF.
*    ENDIF.


  ENDMETHOD.
ENDCLASS.
