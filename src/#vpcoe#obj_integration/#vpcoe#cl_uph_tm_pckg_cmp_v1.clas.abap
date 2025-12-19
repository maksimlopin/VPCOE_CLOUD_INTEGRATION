class /VPCOE/CL_UPH_TM_PCKG_CMP_V1 definition
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

  constants MC_PCKG_COMPOSITION type STRING value '/PackagingCompositions' ##NO_TEXT.
  constants MC_ENTITY_NAME_PCKG_COMP_ITEM type STRING value 'Packaging Composition Item' ##NO_TEXT.
  constants MC_FIELDNAME_VALID_FROM type STRING value 'packagingElementValidFrom' ##NO_TEXT.
  data MS_ENTITY_PACK_CMP_DATA_API type /VPCOE/S_UPH_API_PACK_CMP .
ENDCLASS.



CLASS /VPCOE/CL_UPH_TM_PCKG_CMP_V1 IMPLEMENTATION.


  METHOD /vpcoe/if_uph_transfer_mapper~check_deprecated_attributes.
    DATA : lr_entity_cmp       TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_hdr,
           lr_entity_pckg_item TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_itm,"/vpcoe/cl_uph_ent_pcg_cmp_item,
           lv_validfrom_set    TYPE abap_bool.

    LOOP AT it_entity_data INTO DATA(lr_entity_data).
      lr_entity_cmp ?= lr_entity_data.

      DATA(lt_pckg_items) = lr_entity_cmp->get_items( ).

      LOOP AT lt_pckg_items INTO DATA(lr_pckg_items).
        lr_entity_pckg_item ?= lr_pckg_items.

        DATA(ls_pckg_item_data) = lr_entity_pckg_item->get_data( ).

        check_deprecated_attribute( EXPORTING iv_attribute_value  = ls_pckg_item_data-packagingelementvalidfrom
                                              iv_attribute_name   = mc_fieldname_valid_from
                                              iv_entity_type      = mc_entity_name_pckg_comp_item
                                    CHANGING  cv_is_attribute_set = lv_validfrom_set
                                              ct_messages         = rt_messages ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_transfer_mapper~evaluate_response. "#EC CI_CYCLO "#EC CI_NOES

    " The method was refactored and is kept to ease downports
    super->/vpcoe/if_uph_transfer_mapper~evaluate_response( EXPORTING iv_response = iv_response
                                                           IMPORTING et_messages = et_messages ).

  ENDMETHOD. "#EC CI_CYCLO       "#EC CI_NOES                    "#EC CI_NESTING


  method /VPCOE/IF_UPH_TRANSFER_MAPPER~GET_ENTITY_URL_SUFFIX.
*    rv_entity_url_suffix = mc_pckg_composition.
    rv_entity_url_suffix = NEW /vpcoe/cl_common_helper( iv_srv_grp  = 'PLM'
                                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-mcl
                                                        iv_api_type = 'PLM' )->get_service_url( ).
  endmethod.


  METHOD /vpcoe/if_uph_transfer_mapper~prepare_payload.

    DATA: lr_entity_cmp       TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_hdr,
          lr_entity_pckg_item TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_itm,
          lr_entity_pckg_prod TYPE REF TO /vpcoe/cl_uph_ent_pckg_product.

    DATA: ls_entity_pack_cmp_data_api TYPE /vpcoe/s_uph_api_pack_cmp,
          ls_entity_pack_cmp_api      TYPE /vpcoe/s_uph_api_cmp_hdr,
          ls_entity_pack_item_api     TYPE /vpcoe/s_uph_api_cmp_item,
          ls_entity_pack_prod_api     TYPE /vpcoe/s_uph_api_cmp_prod.

    DATA: lt_pckg_cmp_item_data TYPE /vpcoe/t_uph_api_cmp_item,
          lt_pckg_cmp_prod_data TYPE /vpcoe/t_uph_api_cmp_prod.

    CLEAR: rv_payload.

    LOOP AT it_entity_data INTO DATA(lr_entity_data).
      lr_entity_cmp ?= lr_entity_data.

      "do the mapping for composition data
      DATA(ls_cmp_data) = lr_entity_cmp->get_data( ).

      ls_entity_pack_cmp_api-id                           = ls_cmp_data-displayid.
      ls_entity_pack_cmp_api-description                  = ls_cmp_data-description.
      ls_entity_pack_cmp_api-base_quantity                = me->convert_dec_to_char( ls_cmp_data-basequantity ).
      ls_entity_pack_cmp_api-base_unit_of_measure         = ls_cmp_data-baseunitofmeasureid.
      ls_entity_pack_cmp_api-consumer_sales_unit          = ls_cmp_data-consumersalesunit.

*      "map products
      DATA(lt_pckg_products) = lr_entity_cmp->get_products(  ).

      LOOP AT lt_pckg_products INTO DATA(lr_pckg_products).

        lr_entity_pckg_prod ?= lr_pckg_products.

        "do the mapping for the products
        DATA(ls_pckg_prod_data) = lr_entity_pckg_prod->get_data( ).

        ls_entity_pack_prod_api-product                    = ls_pckg_prod_data-productid.
        ls_entity_pack_prod_api-valid_from                 = ls_pckg_prod_data-valid_from.
        ls_entity_pack_prod_api-valid_to                   = ls_pckg_prod_data-valid_to.
        ls_entity_pack_prod_api-supplier                   = ls_pckg_prod_data-supplier.
        ls_entity_pack_prod_api-business_process_direction = ls_pckg_prod_data-business_process_direction.
        APPEND ls_entity_pack_prod_api TO lt_pckg_cmp_prod_data.

      ENDLOOP.


      "map items
      DATA(lt_pckg_items) = lr_entity_cmp->get_items(  ).

      LOOP AT lt_pckg_items INTO DATA(lr_pckg_items).
        lr_entity_pckg_item ?= lr_pckg_items.
        "do the mapping for the items
        DATA(ls_pckg_item_data) = lr_entity_pckg_item->get_data( ).

        ls_entity_pack_item_api-packaging_element_id         = ls_pckg_item_data-packagingelementdisplayid.
        ls_entity_pack_item_api-packaging_element_version    = ls_pckg_item_data-packagingelementversion.
        ls_entity_pack_item_api-level                        = ls_pckg_item_data-levelcode.
        ls_entity_pack_item_api-quantity                     = me->convert_dec_to_char( ls_pckg_item_data-quantity ).
        ls_entity_pack_item_api-quantity_unit_of_measure     = ls_pckg_item_data-quantityunitofmeasureid.
        ls_entity_pack_item_api-wwf_group                    = ls_pckg_item_data-wwfgroup.
        ls_entity_pack_item_api-epr_group                    = ls_pckg_item_data-eprgroup.
        ls_entity_pack_item_api-usage                        = ls_pckg_item_data-usage.
        ls_entity_pack_item_api-count                        = me->convert_dec_to_char( ls_pckg_item_data-count ).
        " map also deprecated attribute packagingelementvalidfrom
        ls_entity_pack_item_api-packaging_element_valid_from = ls_pckg_item_data-packagingelementvalidfrom.
        APPEND ls_entity_pack_item_api TO lt_pckg_cmp_item_data.

      ENDLOOP.
      ls_entity_pack_cmp_api-items = lt_pckg_cmp_item_data.
      CLEAR lt_pckg_cmp_item_data.

      ls_entity_pack_cmp_api-products = lt_pckg_cmp_prod_data.
      CLEAR lt_pckg_cmp_prod_data.

      APPEND ls_entity_pack_cmp_api TO ls_entity_pack_cmp_data_api-elements.
      CLEAR ls_entity_pack_cmp_api.

    ENDLOOP.

    SORT ls_entity_pack_cmp_data_api-elements BY id.

    IF is_parameters IS NOT INITIAL.
      DATA ls_parameters TYPE /vpcoe/s_pckg_elem_input.
      ls_parameters = CORRESPONDING #( is_parameters ).
      ls_entity_pack_cmp_data_api-source = ls_parameters-source_id.
    ENDIF.

    rv_payload = /vpcoe/cl_plm_helper=>serialize_json( is_data        = ls_entity_pack_cmp_data_api
                                                       iv_compress    = abap_true
                                                       iv_pretty_name = /vpcoe/cl_plm_helper=>sc_pretty_mode-camel_case ).

    REPLACE ALL OCCURRENCES OF '"null"' IN rv_payload WITH 'null' ##no_text.

    ms_entity_pack_cmp_data_api = ls_entity_pack_cmp_data_api.

  ENDMETHOD.                                               "#EC CI_NOES


  METHOD get_element_id_by_index.
    READ TABLE ms_entity_pack_cmp_data_api-elements REFERENCE INTO DATA(lr_api_element) INDEX iv_index.

    IF sy-subrc = 0.
      rv_result = lr_api_element->id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
