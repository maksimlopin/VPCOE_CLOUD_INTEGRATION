class /VPCOE/CL_UPH_TM_PCKG_CMP_V2 definition
  public
  inheriting from /VPCOE/CL_UPH_TRANSFER_MAPPER
  create public .

public section.

  methods /VPCOE/IF_UPH_TRANSFER_MAPPER~GET_ENTITY_URL_SUFFIX
    redefinition .
  methods /VPCOE/IF_UPH_TRANSFER_MAPPER~PREPARE_PAYLOAD
    redefinition .
protected section.

  methods GET_ELEMENT_ID_BY_INDEX
    redefinition .
  PRIVATE SECTION.
    TYPES: BEGIN OF ltys_uph_api_cmp_prod,
             product                    TYPE c LENGTH 40,
             valid_from                 TYPE dats,
             valid_to                   TYPE dats,
             supplier                   TYPE c LENGTH 10,
             business_process_direction TYPE c LENGTH 20,
           END OF ltys_uph_api_cmp_prod,
           ltyt_uph_api_cmp_prod TYPE STANDARD TABLE OF ltys_uph_api_cmp_prod WITH DEFAULT KEY.

    TYPES: BEGIN OF ltys_uph_api_cmp_subitem,
             packaging_element_id      TYPE c LENGTH 50,
             packaging_element_version TYPE c LENGTH 40,
             quantity                  TYPE p LENGTH 16 DECIMALS 6,
             quantity_unit_of_measure  TYPE c LENGTH 3,
             usage                     TYPE c LENGTH 10,
             count                     TYPE p LENGTH 10 DECIMALS 0,
             separability              TYPE c LENGTH 30,
           END OF ltys_uph_api_cmp_subitem,
           ltyt_uph_api_cmp_subitem TYPE STANDARD TABLE OF ltys_uph_api_cmp_subitem WITH DEFAULT KEY.

    TYPES: BEGIN OF ltys_uph_api_cmp_item,
             packaging_element_id      TYPE c LENGTH 50,
             packaging_element_version TYPE c LENGTH 40,
             level                     TYPE c LENGTH 10,
             quantity                  TYPE p LENGTH 16              DECIMALS 6,
             quantity_unit_of_measure  TYPE c LENGTH 3,
             usage                     TYPE c LENGTH 10,
             coverage                  TYPE p LENGTH 16              DECIMALS 2,
             count                     TYPE p LENGTH 10              DECIMALS 0,
             sub_items                 TYPE ltyt_uph_api_cmp_subitem,
           END OF ltys_uph_api_cmp_item,
           ltyt_uph_api_cmp_item TYPE STANDARD TABLE OF ltys_uph_api_cmp_item WITH DEFAULT KEY.

    TYPES: BEGIN OF ltys_uph_api_cmp_hdr,
             id                   TYPE c LENGTH 60,
             description          TYPE string,
             base_quantity        TYPE p LENGTH 16 DECIMALS 6,
             base_unit_of_measure TYPE c LENGTH 3,
             consumer_sales_unit  TYPE c LENGTH 3,
             products             TYPE ltyt_uph_api_cmp_prod,
             items                TYPE ltyt_uph_api_cmp_item,
           END OF ltys_uph_api_cmp_hdr,
           ltyt_uph_api_cmp_hdr TYPE STANDARD TABLE OF ltys_uph_api_cmp_hdr WITH DEFAULT KEY.

    TYPES: BEGIN OF ltys_uph_api_pack_cmp,
             source   TYPE c LENGTH 64,
             elements TYPE ltyt_uph_api_cmp_hdr,
           END OF ltys_uph_api_pack_cmp.

    CONSTANTS mc_url_suffix_pckg_composition TYPE string VALUE '/PackagingCompositions' ##NO_TEXT.

    DATA ms_entity_pack_cmp_data_api TYPE ltys_uph_api_pack_cmp.
ENDCLASS.



CLASS /VPCOE/CL_UPH_TM_PCKG_CMP_V2 IMPLEMENTATION.


  METHOD /vpcoe/if_uph_transfer_mapper~get_entity_url_suffix.
*    rv_entity_url_suffix = mc_url_suffix_pckg_composition.
    rv_entity_url_suffix = NEW /vpcoe/cl_common_helper( iv_srv_grp  = 'PLM'
                                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-plm
                                                        iv_api_type = 'PLM' )->get_service_url( ).

  ENDMETHOD.


  METHOD /VPCOE/IF_UPH_TRANSFER_MAPPER~PREPARE_PAYLOAD.
    DATA : lr_entity_cmp          TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_hdr,
           lr_entity_pckg_item    TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_itm,
           lr_entity_pckg_subitem TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_sub,
           lr_entity_pckg_prod    TYPE REF TO /vpcoe/cl_uph_ent_pckg_product.

    DATA : ls_entity_pack_cmp_data_api TYPE ltys_uph_api_pack_cmp,
           ls_entity_pack_cmp_api      TYPE ltys_uph_api_cmp_hdr,
           ls_entity_pack_item_api     TYPE ltys_uph_api_cmp_item,
           ls_entity_pack_subitem_api  TYPE ltys_uph_api_cmp_subitem,
           ls_entity_pack_prod_api     TYPE ltys_uph_api_cmp_prod.

    DATA : lt_pckg_cmp_item_data TYPE ltyt_uph_api_cmp_item,
           lt_pckg_cmp_prod_data TYPE ltyt_uph_api_cmp_prod.

    CLEAR rv_payload.

    LOOP AT it_entity_data INTO DATA(lr_entity_data).
      lr_entity_cmp ?= lr_entity_data.

      " do the mapping for composition data
      DATA(ls_cmp_data) = lr_entity_cmp->get_data( ).

      CLEAR ls_entity_pack_cmp_api.
      ls_entity_pack_cmp_api-id                   = ls_cmp_data-displayid.
      ls_entity_pack_cmp_api-description          = ls_cmp_data-description.
      ls_entity_pack_cmp_api-base_quantity        = ls_cmp_data-basequantity.
      ls_entity_pack_cmp_api-base_unit_of_measure = ls_cmp_data-baseunitofmeasureid.
      ls_entity_pack_cmp_api-consumer_sales_unit  = ls_cmp_data-consumersalesunit.

      "map products
      DATA(lt_pckg_products) = lr_entity_cmp->get_products( ).

      LOOP AT lt_pckg_products INTO DATA(lr_pckg_products).

        lr_entity_pckg_prod ?= lr_pckg_products.

        " do the mapping for the products
        DATA(ls_pckg_prod_data) = lr_entity_pckg_prod->get_data( ).

        CLEAR ls_entity_pack_prod_api.
        ls_entity_pack_prod_api-product                    = ls_pckg_prod_data-productid.
        ls_entity_pack_prod_api-valid_from                 = ls_pckg_prod_data-valid_from.
        ls_entity_pack_prod_api-valid_to                   = ls_pckg_prod_data-valid_to.
        ls_entity_pack_prod_api-supplier                   = ls_pckg_prod_data-supplier.
        ls_entity_pack_prod_api-business_process_direction = ls_pckg_prod_data-business_process_direction.
        APPEND ls_entity_pack_prod_api TO lt_pckg_cmp_prod_data.

      ENDLOOP.

      " map items
      LOOP AT lr_entity_cmp->get_items( ) INTO DATA(lr_pckg_items).
        lr_entity_pckg_item ?= lr_pckg_items.

        " do the mapping for the items
        DATA(ls_pckg_item_data) = lr_entity_pckg_item->get_data( ).

        CLEAR ls_entity_pack_item_api.
        ls_entity_pack_item_api-packaging_element_id         = ls_pckg_item_data-packagingelementdisplayid.
        ls_entity_pack_item_api-packaging_element_version    = ls_pckg_item_data-packagingelementversion.
        ls_entity_pack_item_api-level                        = ls_pckg_item_data-levelcode.
        ls_entity_pack_item_api-quantity                     = ls_pckg_item_data-quantity.
        ls_entity_pack_item_api-quantity_unit_of_measure     = ls_pckg_item_data-quantityunitofmeasureid.
        ls_entity_pack_item_api-coverage                     = ls_pckg_item_data-coverage.
        ls_entity_pack_item_api-usage                        = ls_pckg_item_data-usage.
        ls_entity_pack_item_api-count                        = ls_pckg_item_data-count.

        LOOP AT lr_entity_pckg_item->get_subitems(  ) INTO DATA(lr_pckg_subitems).
          lr_entity_pckg_subitem ?= lr_pckg_subitems.

          " do the mapping for the subitem
          DATA(ls_pckg_subitem_data) = lr_entity_pckg_subitem->get_data( ).

          CLEAR ls_entity_pack_subitem_api.
          ls_entity_pack_subitem_api-packaging_element_id         = ls_pckg_subitem_data-packagingelementdisplayid.
          ls_entity_pack_subitem_api-packaging_element_version    = ls_pckg_subitem_data-packagingelementversion.
          ls_entity_pack_subitem_api-quantity                     = ls_pckg_subitem_data-quantity.
          ls_entity_pack_subitem_api-quantity_unit_of_measure     = ls_pckg_subitem_data-quantityunitofmeasureid.
          ls_entity_pack_subitem_api-separability                 = ls_pckg_subitem_data-separability.
          ls_entity_pack_subitem_api-usage                        = ls_pckg_subitem_data-usage.
          ls_entity_pack_subitem_api-count                        = ls_pckg_subitem_data-count.

          APPEND ls_entity_pack_subitem_api TO ls_entity_pack_item_api-sub_items.

        ENDLOOP.

        APPEND ls_entity_pack_item_api TO lt_pckg_cmp_item_data.

      ENDLOOP.

      ls_entity_pack_cmp_api-items = lt_pckg_cmp_item_data.
      CLEAR lt_pckg_cmp_item_data.

      ls_entity_pack_cmp_api-products = lt_pckg_cmp_prod_data.
      CLEAR lt_pckg_cmp_prod_data.

      APPEND ls_entity_pack_cmp_api TO ls_entity_pack_cmp_data_api-elements.

    ENDLOOP.

    SORT ls_entity_pack_cmp_data_api-elements BY id.

    IF is_parameters IS NOT INITIAL.

      ASSIGN COMPONENT 'SOURCE_ID' OF STRUCTURE is_parameters TO FIELD-SYMBOL(<fs>).
      IF sy-subrc = 0.
        ls_entity_pack_cmp_data_api-source = <fs>.
      ENDIF.

    ENDIF.

    rv_payload = /ui2/cl_json=>serialize( data        = ls_entity_pack_cmp_data_api
                                          compress    = abap_true
                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    ms_entity_pack_cmp_data_api = ls_entity_pack_cmp_data_api.
  ENDMETHOD.


  METHOD GET_ELEMENT_ID_BY_INDEX.
    READ TABLE ms_entity_pack_cmp_data_api-elements REFERENCE INTO DATA(lr_api_element) INDEX iv_index.

    IF sy-subrc = 0.
      rv_result = lr_api_element->id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
