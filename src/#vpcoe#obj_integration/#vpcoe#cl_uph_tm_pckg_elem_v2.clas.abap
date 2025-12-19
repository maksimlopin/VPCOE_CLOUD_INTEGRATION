class /VPCOE/CL_UPH_TM_PCKG_ELEM_V2 definition
  public
  inheriting from /VPCOE/CL_UPH_TRANSFER_MAPPER
  create public .

public section.

  methods /VPCOE/IF_UPH_TRANSFER_MAPPER~CHECK_DEPRECATED_ATTRIBUTES
    redefinition .
  methods /VPCOE/IF_UPH_TRANSFER_MAPPER~EVALUATE_RESPONSE
    redefinition .
  methods /VPCOE/IF_UPH_TRANSFER_MAPPER~GET_ENTITY_URL_SUFFIX
    redefinition .
  methods /VPCOE/IF_UPH_TRANSFER_MAPPER~PREPARE_PAYLOAD
    redefinition .
protected section.

  methods GET_ELEMENT_ID_BY_INDEX
    redefinition .
private section.

  constants MC_PCKG_ELEMENT type STRING value '/PackagingElements' ##NO_TEXT.
  data MS_ENTITY_PACK_ELEM_DATA_API type /VPCOE/S_UPH_API_PCK_HDR .
  constants MC_PCKG_ELEMENT_TRUE type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  constants MC_PCKG_ELEMENT_FALSE type ABAP_BOOL value 'N' ##NO_TEXT.
  constants MC_ENTITY_NAME_PCKG_ELEM type STRING value 'Packaging Element' ##NO_TEXT.
  constants MC_ENTITY_NAME_PCKG_ELEM_FRAC type STRING value 'Packaging Element Fraction' ##NO_TEXT.
  constants MC_FIELDNAME_VALID_FROM type STRING value 'validFrom' ##NO_TEXT.
  constants MC_FIELDNAME_IS_COMPOUND type STRING value 'isCompound' ##NO_TEXT.
  constants MC_FIELDNAME_RENEWABLE_PERCENT type STRING value 'renewablePercent' ##NO_TEXT.
  constants MC_FIELDNAME_LAMINATION_TYPE type STRING value 'laminationType' ##NO_TEXT.
  constants MC_FIELDNAME_IS_COEXTRUDED_LAM type STRING value 'isCoextrudedLamination' ##NO_TEXT.

  methods CONVERT_BOOLEAN_ATTRIBUTE
    importing
      value(IV_VALUE) type BOOLE_D
    returning
      value(RV_RETURN_VALUE) type STRING .
ENDCLASS.



CLASS /VPCOE/CL_UPH_TM_PCKG_ELEM_V2 IMPLEMENTATION.


  METHOD /VPCOE/IF_UPH_TRANSFER_MAPPER~CHECK_DEPRECATED_ATTRIBUTES.
    DATA : lr_entity_pckg                TYPE REF TO /vpcoe/cl_uph_ent_pckg_element,
           lr_entity_pckg_frac           TYPE REF TO /vpcoe/cl_ent_pckg_fraction," /vpcoe/cl_uph_ent_pckg_frctn

           lv_validfrom_set              TYPE abap_bool,
           lv_iscompound_set             TYPE abap_bool,
           lv_renewablepercent_set       TYPE abap_bool,
           lv_laminationtype_set         TYPE abap_bool,
           lv_iscoextrudedlamination_set TYPE abap_bool.

    LOOP AT it_entity_data INTO DATA(lr_entity_data).

      lr_entity_pckg ?= lr_entity_data.

      DATA(ls_pckg_data) = lr_entity_pckg->get_data( ).

      check_deprecated_attribute( EXPORTING iv_attribute_value  = ls_pckg_data-validfrom
                                            iv_attribute_name   = mc_fieldname_valid_from
                                            iv_entity_type      = mc_entity_name_pckg_elem
                                  CHANGING  cv_is_attribute_set = lv_validfrom_set
                                            ct_messages         = rt_messages ).

      check_deprecated_attribute( EXPORTING iv_attribute_value  = ls_pckg_data-iscompound
                                            iv_attribute_name   = mc_fieldname_is_compound
                                            iv_entity_type      = mc_entity_name_pckg_elem
                                  CHANGING  cv_is_attribute_set = lv_iscompound_set
                                            ct_messages         = rt_messages ).

      check_deprecated_attribute( EXPORTING iv_attribute_value  = ls_pckg_data-renewablepercent
                                            iv_attribute_name   = mc_fieldname_renewable_percent
                                            iv_entity_type      = mc_entity_name_pckg_elem
                                  CHANGING  cv_is_attribute_set = lv_renewablepercent_set
                                            ct_messages         = rt_messages ).

      DATA(lt_pckg_fractions) = lr_entity_pckg->get_fractions( ).

      LOOP AT lt_pckg_fractions INTO DATA(lr_pckg_fractions).

        lr_entity_pckg_frac ?= lr_pckg_fractions.

        DATA(ls_pckg_fraction_data) = lr_entity_pckg_frac->get_data( ).

        check_deprecated_attribute( EXPORTING iv_attribute_value  = ls_pckg_fraction_data-laminationtype
                                              iv_attribute_name   = mc_fieldname_lamination_type
                                              iv_entity_type      = mc_entity_name_pckg_elem_frac
                                    CHANGING  cv_is_attribute_set = lv_laminationtype_set
                                              ct_messages         = rt_messages ).

        check_deprecated_attribute( EXPORTING iv_attribute_value  = ls_pckg_fraction_data-iscoextrudedlamination
                                              iv_attribute_name   = mc_fieldname_is_coextruded_lam
                                              iv_entity_type      = mc_entity_name_pckg_elem_frac
                                    CHANGING  cv_is_attribute_set = lv_iscoextrudedlamination_set
                                              ct_messages         = rt_messages ).

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD /VPCOE/IF_UPH_TRANSFER_MAPPER~EVALUATE_RESPONSE. "#EC CI_CYCLO

    " The method was refactored and is kept to ease downports
    super->/vpcoe/if_uph_transfer_mapper~evaluate_response( EXPORTING iv_response = iv_response
                                                           IMPORTING et_messages = et_messages ).

  ENDMETHOD. "#EC CI_CYCLO                                    "#EC CI_NESTING


  METHOD /vpcoe/if_uph_transfer_mapper~get_entity_url_suffix.
*    rv_entity_url_suffix = mc_pckg_element.
    rv_entity_url_suffix = NEW /vpcoe/cl_common_helper( iv_srv_grp  = 'PLM'
                                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-mcl
                                                        iv_api_type = 'PLM' )->get_service_url( ).
  ENDMETHOD.


  METHOD /vpcoe/if_uph_transfer_mapper~prepare_payload.

    DATA :lr_entity_pckg      TYPE REF TO /vpcoe/cl_uph_ent_pckg_element,
          lr_entity_pckg_frac TYPE REF TO /vpcoe/cl_ent_pckg_fraction,
          lr_entity_pckg_func TYPE REF TO /vpcoe/cl_uph_ent_pckg_func,
          lr_entity_pckg_prod TYPE REF TO /vpcoe/cl_uph_ent_pckg_product.

    DATA :ls_entity_pack_elem_data_api TYPE /vpcoe/s_uph_api_pck_hdr,
          ls_entity_pack_elem_api      TYPE /vpcoe/s_uph_api_pck_element,
          ls_entity_pack_frac_api      TYPE /vpcoe/s_uph_api_pck_frac,
          ls_entity_pack_func_api      TYPE /vpcoe/s_uph_api_pck_func,
          ls_entity_pack_prod_api      TYPE /vpcoe/s_uph_api_pck_prod.

    DATA :lt_pckg_fraction_data TYPE /vpcoe/t_uph_api_pck_frac,
          lt_pckg_function_data TYPE /vpcoe/t_uph_api_pck_func,
          lt_pckg_prod_data     TYPE /vpcoe/t_uph_api_pck_prod.

    CLEAR: rv_payload.
    LOOP AT it_entity_data INTO DATA(lr_entity_data) ##CLASS_FINAL.

      lr_entity_pckg ?= lr_entity_data.

      "do the mapping for packaging data
      DATA(ls_pckg_data) = lr_entity_pckg->get_data( ).
      ls_entity_pack_elem_api-id                        = ls_pckg_data-displayid.
      ls_entity_pack_elem_api-description               = ls_pckg_data-description.
      ls_entity_pack_elem_api-version                   = ls_pckg_data-version.
      ls_entity_pack_elem_api-unit_of_measure           = ls_pckg_data-unitofmeasure.
      ls_entity_pack_elem_api-usage                     = ls_pckg_data-usage_type.
      ls_entity_pack_elem_api-flexibility               = ls_pckg_data-flexibility.
      ls_entity_pack_elem_api-packaging_type            = ls_pckg_data-packagingtype.
      ls_entity_pack_elem_api-dimension_unit_of_measure = ls_pckg_data-dimensionunitofmeasure.
      ls_entity_pack_elem_api-dimension_length          = ls_pckg_data-dimensionlength.
      ls_entity_pack_elem_api-dimension_width           = ls_pckg_data-dimensionwidth.
      ls_entity_pack_elem_api-dimension_height          = ls_pckg_data-dimensionheight.
      ls_entity_pack_elem_api-volume                    = ls_pckg_data-volume.
      ls_entity_pack_elem_api-volume_unit_of_measure    = ls_pckg_data-volumeunitofmeasure .
      ls_entity_pack_elem_api-reuse_count               = ls_pckg_data-reusecount.
      ls_entity_pack_elem_api-recyclable_percent        = ls_pckg_data-recyclablepercent.
      ls_entity_pack_elem_api-compostable_percent       = ls_pckg_data-compostablepercent.
      ls_entity_pack_elem_api-is_reusable               = convert_boolean_attribute( iv_value = ls_pckg_data-isreusable ).
      ls_entity_pack_elem_api-is_aseptic                = convert_boolean_attribute( iv_value = ls_pckg_data-isaseptic ).
      ls_entity_pack_elem_api-is_not_empty              = convert_boolean_attribute( iv_value = ls_pckg_data-isnotempty ).
      ls_entity_pack_elem_api-is_deposit                = convert_boolean_attribute( iv_value = ls_pckg_data-isdeposit ).
      ls_entity_pack_elem_api-is_service_packaging      = convert_boolean_attribute( iv_value = ls_pckg_data-isservicepackaging ).
      ls_entity_pack_elem_api-is_optically_detectable   = convert_boolean_attribute( iv_value = ls_pckg_data-isopticallydetectable ).
      ls_entity_pack_elem_api-lamination_type           = ls_pckg_data-laminationtype.
      ls_entity_pack_elem_api-reuse_lifetime_in_years   = ls_pckg_data-reuselifetimeinyears.
      ls_entity_pack_elem_api-refill_consumer_type      = ls_pckg_data-refillconsumertype.

      " map also deprecated attributes
      ls_entity_pack_elem_api-valid_from                = ls_pckg_data-validfrom.
      ls_entity_pack_elem_api-renewable_percent         = ls_pckg_data-renewablepercent.
      ls_entity_pack_elem_api-is_compound               = convert_boolean_attribute( iv_value = ls_pckg_data-iscompound ).
      "map fractions
      DATA(lt_pckg_fractions) = lr_entity_pckg->get_fractions(  ).

      LOOP AT lt_pckg_fractions INTO DATA(lr_pckg_fractions) ##CLASS_FINAL.

        lr_entity_pckg_frac ?= lr_pckg_fractions.

        "do the mapping for the fractions
        DATA(ls_pckg_fraction_data) = lr_entity_pckg_frac->get_data( ).

        ls_entity_pack_frac_api-basic_material_fraction   = ls_pckg_fraction_data-basicmaterialfractionid.
        ls_entity_pack_frac_api-weight                    = ls_pckg_fraction_data-weight.
        ls_entity_pack_frac_api-weight_unit_of_measure    = ls_pckg_fraction_data-weightunitofmeasure.
        ls_entity_pack_frac_api-transparency              = ls_pckg_fraction_data-transparency.
        ls_entity_pack_frac_api-color                     = ls_pckg_fraction_data-color.
        ls_entity_pack_frac_api-recycled_content_percent  = ls_pckg_fraction_data-recycledcontentpercent.
        ls_entity_pack_frac_api-thickness                 = ls_pckg_fraction_data-thickness.
        ls_entity_pack_frac_api-thickness_unit_of_measure = ls_pckg_fraction_data-thicknessunitofmeasure.
        ls_entity_pack_frac_api-density                   = ls_pckg_fraction_data-density.
        ls_entity_pack_frac_api-density_unit_of_measure   = ls_pckg_fraction_data-densityunitofmeasure.
        ls_entity_pack_frac_api-is_reinforced             = convert_boolean_attribute( iv_value = ls_pckg_fraction_data-isreinforced ).
        ls_entity_pack_frac_api-is_expanded               = convert_boolean_attribute( iv_value = ls_pckg_fraction_data-isexpanded ).
        ls_entity_pack_frac_api-renewable_percent         = ls_pckg_fraction_data-renewablepercent.
        ls_entity_pack_frac_api-chmlrecycledcntntpct           = ls_pckg_fraction_data-chmlrecycledcntntpct.
        ls_entity_pack_frac_api-chmlrecycledcntntvalmeth       = ls_pckg_fraction_data-chmlrecycledcntntvalmeth.
        ls_entity_pack_frac_api-mechanicalrecycledcntntpct     = ls_pckg_fraction_data-mechanicalrecycledcntntpct.
        ls_entity_pack_frac_api-mechanicalrecycledcntntvalmeth = ls_pckg_fraction_data-mechanicalrecycledcntntvalmeth.

        " map also deprecated attributes
        ls_entity_pack_frac_api-lamination_type           = ls_pckg_fraction_data-laminationtype.
        ls_entity_pack_frac_api-is_co_extruded_lamination = convert_boolean_attribute( iv_value = ls_pckg_fraction_data-iscoextrudedlamination ).

        APPEND ls_entity_pack_frac_api TO lt_pckg_fraction_data.

        "map functions data
        DATA lt_pckg_functions TYPE /vpcoe/t_uph_api_pck_func.
        lt_pckg_functions = ls_pckg_fraction_data-functions.

        LOOP AT lt_pckg_functions ASSIGNING FIELD-SYMBOL(<fs_pckg_functions>) ##CLASS_FINAL.
          ls_entity_pack_func_api-function   = <fs_pckg_functions>-function.
          ls_entity_pack_func_api-weight_percent   = <fs_pckg_functions>-weight_percent.
          APPEND ls_entity_pack_func_api TO lt_pckg_function_data.
        ENDLOOP.

        lt_pckg_fraction_data[ sy-tabix ]-functions = lt_pckg_function_data.
        CLEAR: lt_pckg_function_data.

      ENDLOOP.
      ls_entity_pack_elem_api-fractions = lt_pckg_fraction_data.
      CLEAR lt_pckg_fraction_data.

      "map products
      DATA(lt_pckg_products) = lr_entity_pckg->get_products(  ).

      LOOP AT lt_pckg_products INTO DATA(lr_pckg_products) ##CLASS_FINAL.

        lr_entity_pckg_prod ?= lr_pckg_products.

        "do the mapping for the fractions
        DATA(ls_pckg_prod_data) = lr_entity_pckg_prod->get_data( ).

        ls_entity_pack_prod_api-product   = ls_pckg_prod_data-productid.
        APPEND ls_entity_pack_prod_api TO lt_pckg_prod_data.

      ENDLOOP.

      ls_entity_pack_elem_api-materials = lt_pckg_prod_data.
      CLEAR lt_pckg_prod_data.

      APPEND ls_entity_pack_elem_api TO ls_entity_pack_elem_data_api-elements.

      CLEAR ls_entity_pack_elem_api.

    ENDLOOP.

    SORT ls_entity_pack_elem_data_api-elements BY id.

    IF is_parameters IS NOT INITIAL.
      DATA ls_parameters TYPE /vpcoe/s_pckg_elem_input.
      ls_parameters = CORRESPONDING #( is_parameters ).
      ls_entity_pack_elem_data_api-source = ls_parameters-source_id.
    ENDIF.

    rv_payload = /vpcoe/cl_plm_helper=>serialize_json( is_data        = ls_entity_pack_elem_data_api
                                                       iv_pretty_name = /vpcoe/cl_plm_helper=>sc_pretty_mode-camel_case
                                                       iv_compress    = abap_true
                                                       it_name_mappings  = VALUE #( ( abap = 'chmlrecycledcntntpct'           json = 'chemicallyRecycledContentPercent ' )
                                                                                    ( abap = 'chmlrecycledcntntvalmeth'       json = 'chemicallyRecycledContentValueMethod ' )
                                                                                    ( abap = 'mechanicalrecycledcntntpct'     json = 'mechanicallyRecycledContentPercent ' )
                                                                                    ( abap = 'mechanicalrecycledcntntvalmeth' json = 'mechanicallyRecycledContentValueMethod ' ) ) ).

    REPLACE ALL OCCURRENCES OF '"isReinforced":"X"' IN rv_payload WITH '"isReinforced": true' ##no_text.
    REPLACE ALL OCCURRENCES OF '"isReinforced":""' IN rv_payload WITH '"isReinforced": false' ##no_text.
    REPLACE ALL OCCURRENCES OF '"isReinforced":"N"' IN rv_payload WITH '"isReinforced": false' ##no_text.
    REPLACE ALL OCCURRENCES OF '"isReinforced":"Y"' IN rv_payload WITH '"isReinforced": true' ##no_text.
    REPLACE ALL OCCURRENCES OF '"null"' IN rv_payload WITH 'null' ##no_text.
    ms_entity_pack_elem_data_api = ls_entity_pack_elem_data_api.

  ENDMETHOD.                                               "#EC CI_NOES


  METHOD CONVERT_BOOLEAN_ATTRIBUTE.
    IF iv_value = abap_true.
      rv_return_value = mc_pckg_element_true.
    ELSEIF iv_value = abap_false.
      rv_return_value = mc_pckg_element_false.
    ENDIF.
  ENDMETHOD.


  METHOD GET_ELEMENT_ID_BY_INDEX.
    READ TABLE ms_entity_pack_elem_data_api-elements REFERENCE INTO DATA(lr_api_element) INDEX iv_index.

    IF sy-subrc = 0.
      rv_result = lr_api_element->id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
