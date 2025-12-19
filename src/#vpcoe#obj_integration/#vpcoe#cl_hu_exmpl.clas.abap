class /VPCOE/CL_HU_EXMPL definition
  public
  inheriting from /VPCOE/CL_UPH_PROC_BASE_HU
  create public .

public section.

  methods /VPCOE/IF_UPH_ENTITY_HU_PROC~MAP_HU_DATA
    redefinition .
*
*  methods /VPCOE/IF_UPH_ENTITY_MCL_PROC~GET_RELEVANT_MAT_CLAS
*    redefinition .
*  methods /VPCOE/IF_UPH_ENTITY_MCL_PROC~MAP_MAT_CLAS_DATA
*    redefinition .
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

  data MT_VERSIONS type GTY_T_VERSIONS .
  data MR_TCLA_MULTOBJ type ref to ABAP_BOOL .

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
ENDCLASS.



CLASS /VPCOE/CL_HU_EXMPL IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_hu_proc~map_hu_data.
    DATA lt_entity_comp       TYPE /vpcoe/t_uph_entity_data.
    DATA lt_entity_comp_items TYPE /vpcoe/t_uph_entity_data.
    DATA lt_entity_products   TYPE /vpcoe/t_uph_entity_data.

    get_versions( ).

    " map calculated handling unit data to packaging composition item entities
    LOOP AT it_packcomp_data REFERENCE INTO DATA(lr_packcomp_data).

      IF lr_packcomp_data->packaging_compositions IS INITIAL.

        MESSAGE s049(/vpcoe/plm) WITH lr_packcomp_data->selection_criteria-product_id INTO DATA(lv_message) ##NEEDED.
        mo_logger->add_messages(
            it_messages = VALUE #(
                ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).

        CONTINUE.
      ENDIF.

      "iterate over existing packaging compositions in RDP
      LOOP AT lr_packcomp_data->packaging_compositions REFERENCE INTO DATA(lr_packaging_composition).

        CLEAR: lt_entity_comp_items, lt_entity_products.

        APPEND INITIAL LINE TO lt_entity_products REFERENCE INTO DATA(lr_entity_product).
        lr_entity_product->* = NEW /vpcoe/cl_uph_ent_pckg_product(
            is_data    = VALUE #(
                productid                  = lr_packcomp_data->selection_criteria-product_id
                valid_from                 = lr_packaging_composition->valid_from
                valid_to                   = lr_packaging_composition->valid_to
                business_process_direction = lr_packcomp_data->selection_criteria-business_process_direction
                supplier                   = lr_packcomp_data->selection_criteria-supplier_id )
            iv_deleted = abap_false ).

        "map calculated handling unit data to composition as additional item
        LOOP AT it_handling_units REFERENCE INTO DATA(lr_handling_unit) WHERE product = lr_packcomp_data->selection_criteria-product_id.

          APPEND INITIAL LINE TO lt_entity_comp_items REFERENCE INTO DATA(lr_comp_item).
          lr_comp_item->* = NEW /vpcoe/cl_uph_ent_pckg_cmp_itm(
              is_data    = VALUE #(
                  packagingelementdisplayid = lr_handling_unit->handling_unit
                  levelcode                 = '30'
                  quantity                  = lr_handling_unit->amount
                  quantityunitofmeasureid   = lr_handling_unit->base_unit
                  count                     = COND #( WHEN me->dimension_check( lr_handling_unit->base_unit ) = abap_true
                                                      THEN lr_handling_unit->amount
                                                      ELSE 0 )
                  usage                     = 'H'
                  packagingelementversion   = get_packaging_element_version(
                                                  iv_product    = lr_handling_unit->handling_unit
                                                  iv_valid_from = lr_packaging_composition->valid_from
                                                  iv_valid_to   = lr_packaging_composition->valid_to )
                  )
              iv_deleted = abap_false ).
        ENDLOOP.

        IF lt_entity_comp_items IS NOT INITIAL.

          APPEND INITIAL LINE TO lt_entity_comp REFERENCE INTO DATA(lr_entity_comp).
          lr_entity_comp->* = NEW /vpcoe/cl_uph_ent_pckg_cmp_hdr(
                                      is_cmp_hdr_data  = VALUE #( displayid = lr_packaging_composition->id )
                                      it_cmp_item_data = lt_entity_comp_items
                                      it_products      = lt_entity_products
                                      iv_deleted       = abap_false ).
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    rt_entity_data = lt_entity_comp.

*    DATA: lt_hu_data TYPE SORTED TABLE OF /vpcoe/s_handling_unit_prd WITH NON-UNIQUE KEY id,
*          ls_hu_data TYPE /vpcoe/s_handling_unit_prd.
*
*    me->get_versions( ).
*
*    LOOP AT it_packcomp_data ASSIGNING FIELD-SYMBOL(<ls_value>) WHERE selection_criteria-product_id IN ms_parameters-mat_number .
*      IF <ls_value>-packaging_compositions IS NOT INITIAL.
*        lt_hu_data = VALUE #( BASE lt_hu_data FOR ls_pack_comp_prd IN <ls_value>-packaging_compositions
*                            ( id = ls_pack_comp_prd-id
*                              items = VALUE #( FOR ls_hu IN it_handling_units WHERE ( product = <ls_value>-selection_criteria-productid )
*                                                                      ( packaging_element_id     = ls_hu-handling_unit
*                                                                        level                    = '30'
*                                                                        quantity                 = ls_hu-amount
*                                                                        quantity_unit_of_measure = ls_hu-base_unit
*                                                                        count                    = COND #( WHEN me->dimension_check( ls_hu-base_unit ) = abap_true
*                                                                                                     THEN ls_hu-amount
*                                                                                                       ELSE 0 )
*                                                                        usage                     = 'H'
*                                                                        packaging_element_version = me->get_handling_unit_version( iv_handling_unit = ls_hu-handling_unit
*                                                                                                                                   iv_valid_from = ls_pack_comp_prd-valid_from
*                                                                                                                                   iv_valid_to = ls_pack_comp_prd-valid_to )
*                                                                        wwf_group                 = 'W1'
*                                                                        epr_group                 = 'E1' ) ) ) ).
*      ELSE.
*        MESSAGE s038(/vpcoe/plm) WITH <ls_value>-selectioncriteria-productid INTO DATA(lv_message).
*        mo_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).
*      ENDIF.
*
*    ENDLOOP.
*
*    LOOP AT lt_hu_data ASSIGNING FIELD-SYMBOL(<ls_hu_data>).
*      DATA(lo_hu_data) = NEW /vpcoe/cl_uph_ent_handl_unit( is_data = <ls_hu_data> ).
*
*      INSERT lo_hu_data INTO TABLE rt_entity_data.
*    ENDLOOP.

  ENDMETHOD.


  METHOD dimension_check.
    CLEAR: rv_uom.

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


  METHOD GET_PACKAGING_ELEMENT_VERSION.
    DATA: lv_valid_from TYPE datuv,
          lv_valid_to   TYPE datuv,
          lv_version    TYPE datuv.

*    lv_valid_from = |{ iv_valid_from(4) }{ iv_valid_from+5(2) }{ iv_valid_from+8(2) }|.
*    lv_valid_to = |{ iv_valid_to(4) }{ iv_valid_to+5(2) }{ iv_valid_to+8(2) }|.

    LOOP AT mt_versions ASSIGNING FIELD-SYMBOL(<ls_version>) WHERE objek = iv_product
                                                               AND datuv <= lv_valid_from
                                                               AND datub >= lv_valid_from .
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
                 INNER JOIN
                   inob ON ausp~objek = inob~cuobj
                     INNER JOIN
                       kssk ON kssk~objek = inob~cuobj AND kssk~mafid = 'O' AND kssk~klart = '001'
                         INNER JOIN
                           ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
                             INNER JOIN
                               klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
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
               INNER JOIN
                 kssk ON kssk~objek = ausp~objek AND kssk~mafid = 'O' AND kssk~klart = '001'
                   INNER JOIN
                     ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
                       INNER JOIN
                         klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
        FOR ALL ENTRIES IN @mt_calculated_hu
        WHERE ausp~klart = '001'
          AND ausp~lkenz = ''
          AND ausp~objek = @mt_calculated_hu-handling_unit
          AND klah~class = @mc_relevant_class_name
        INTO CORRESPONDING FIELDS OF TABLE @mt_versions.

    ENDIF.

*    SELECT ausp~objek, ausp~datuv, ausp~datub FROM ausp
*      FOR ALL ENTRIES IN @mt_hu
*      WHERE
*        ausp~klart = '001' AND ausp~lkenz = '' AND
*        ausp~objek = @mt_hu-handling_unit
*      INTO CORRESPONDING FIELDS OF TABLE @mt_versions.
*
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
