CLASS /vpcoe/cl_tm_pckg_cmp_it_v2 DEFINITION
  PUBLIC
  INHERITING FROM /vpcoe/cl_uph_transfer_mapper
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /vpcoe/if_uph_transfer_mapper~get_entity_url_suffix
         REDEFINITION .
    METHODS /vpcoe/if_uph_transfer_mapper~prepare_payload
         REDEFINITION .
  PROTECTED SECTION.

    METHODS get_element_id_by_index
         REDEFINITION .
  PRIVATE SECTION.

    TYPES: BEGIN OF mtys_uph_api_cmp_subitem,
             packaging_element_id      TYPE c LENGTH 50,
             packaging_element_version TYPE c LENGTH 40,
             quantity                  TYPE p LENGTH 16 DECIMALS 6,
             quantity_unit_of_measure  TYPE c LENGTH 3,
             usage                     TYPE c LENGTH 10,
             count                     TYPE p LENGTH 10 DECIMALS 0,
             separability              TYPE c LENGTH 30,
           END OF mtys_uph_api_cmp_subitem,
           mtyt_uph_api_cmp_subitem TYPE STANDARD TABLE OF mtys_uph_api_cmp_subitem WITH DEFAULT KEY.

    TYPES: BEGIN OF mtys_uph_api_cmp_item,
             packaging_element_id      TYPE c LENGTH 50,
             packaging_element_version TYPE c LENGTH 40,
             level                     TYPE c LENGTH 10,
             quantity                  TYPE p LENGTH 16              DECIMALS 6,
             quantity_unit_of_measure  TYPE c LENGTH 3,
             usage                     TYPE c LENGTH 10,
             coverage                  TYPE p LENGTH 16              DECIMALS 2,
             count                     TYPE p LENGTH 10              DECIMALS 0,
             sub_items                 TYPE mtyt_uph_api_cmp_subitem,
           END OF mtys_uph_api_cmp_item,
           mtyt_uph_api_cmp_item TYPE STANDARD TABLE OF mtys_uph_api_cmp_item WITH DEFAULT KEY.

    TYPES: BEGIN OF mty_s_api_pack_comp,
             id    TYPE c LENGTH 60,
             items TYPE mtyt_uph_api_cmp_item,
           END OF mty_s_api_pack_comp,
           mtyt_uph_api_pack_comp TYPE STANDARD TABLE OF mty_s_api_pack_comp WITH DEFAULT KEY.

    TYPES: BEGIN OF mtys_uph_api_reg_pack_cmp_item,
             source   TYPE c LENGTH 64,
             elements TYPE mtyt_uph_api_pack_comp,
           END OF mtys_uph_api_reg_pack_cmp_item.

    CONSTANTS mc_entity_url_suffix TYPE string    VALUE '/PackagingCompositionItems'.

    DATA ms_entity_hu_data_api TYPE mtys_uph_api_reg_pack_cmp_item.

ENDCLASS.



CLASS /VPCOE/CL_TM_PCKG_CMP_IT_V2 IMPLEMENTATION.


  METHOD /vpcoe/if_uph_transfer_mapper~get_entity_url_suffix.
*    rv_entity_url_suffix = mc_entity_url_suffix.
    rv_entity_url_suffix = NEW /vpcoe/cl_common_helper( iv_srv_grp  = 'PLM'
                                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-handling_units
                                                        iv_api_type = 'PLM' )->get_service_url( ).
  ENDMETHOD.


  METHOD /vpcoe/if_uph_transfer_mapper~prepare_payload.
    DATA ls_api_req_pack_comp_item TYPE mtys_uph_api_reg_pack_cmp_item.
    DATA ls_api_pack_comp          TYPE mty_s_api_pack_comp.
    DATA ls_api_pack_comp_item     TYPE mtys_uph_api_cmp_item.
    DATA ls_api_pack_comp_subitem  TYPE mtys_uph_api_cmp_subitem.
    DATA lr_entity_pckg_subitem    TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_sub.
    DATA lr_ent_pckg_comp          TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_hdr.
    DATA lr_ent_pckg_comp_item     TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_itm.

    CLEAR rv_payload.

    DATA(lt_api_pack_comp) = VALUE mtyt_uph_api_pack_comp( ).

    LOOP AT it_entity_data INTO DATA(lr_entity_data).

      lr_ent_pckg_comp ?= lr_entity_data.

      DATA(ls_comp_hdr_data) = lr_ent_pckg_comp->get_data( ).
      DATA(lt_comp_items) = lr_ent_pckg_comp->get_items( ).

      DATA(lt_api_pack_comp_item) = VALUE mtyt_uph_api_cmp_item( ).
      LOOP AT lt_comp_items INTO lr_entity_data.

        lr_ent_pckg_comp_item ?= lr_entity_data.
        DATA(ls_ent_pckg_comp_item) = lr_ent_pckg_comp_item->get_data( ).

        ls_api_pack_comp_item = VALUE #( packaging_element_id      = ls_ent_pckg_comp_item-packagingelementdisplayid
                                         packaging_element_version = ls_ent_pckg_comp_item-packagingelementversion
                                         level                     = ls_ent_pckg_comp_item-levelcode
                                         quantity                  = ls_ent_pckg_comp_item-quantity
                                         quantity_unit_of_measure  = ls_ent_pckg_comp_item-quantityunitofmeasureid
                                         usage                     = ls_ent_pckg_comp_item-usage
                                         coverage                  = ls_ent_pckg_comp_item-coverage
                                         count                     = ls_ent_pckg_comp_item-count ).

        LOOP AT lr_ent_pckg_comp_item->get_subitems( ) INTO DATA(lr_pckg_subitems).

          lr_entity_pckg_subitem ?= lr_pckg_subitems.

          " do the mapping for the subitem
          DATA(ls_pckg_subitem_data) = lr_entity_pckg_subitem->get_data( ).

          ls_api_pack_comp_subitem = VALUE #( packaging_element_id         = ls_pckg_subitem_data-packagingelementdisplayid
                                              packaging_element_version    = ls_pckg_subitem_data-packagingelementversion
                                              quantity                     = ls_pckg_subitem_data-quantity
                                              quantity_unit_of_measure     = ls_pckg_subitem_data-quantityunitofmeasureid
                                              separability                 = ls_pckg_subitem_data-separability
                                              usage                        = ls_pckg_subitem_data-usage
                                              count                        = ls_pckg_subitem_data-count ).

          APPEND ls_api_pack_comp_subitem TO ls_api_pack_comp_item-sub_items.

        ENDLOOP.

        APPEND ls_api_pack_comp_item TO lt_api_pack_comp_item.

      ENDLOOP.

      CLEAR ls_api_pack_comp.
      ls_api_pack_comp-id    = ls_comp_hdr_data-displayid.
      ls_api_pack_comp-items = lt_api_pack_comp_item.
      APPEND ls_api_pack_comp TO lt_api_pack_comp.

    ENDLOOP.

    ls_api_req_pack_comp_item-elements = lt_api_pack_comp.

    IF is_parameters IS NOT INITIAL.

      ASSIGN COMPONENT 'SOURCE_ID' OF STRUCTURE is_parameters TO FIELD-SYMBOL(<fs>).
      IF sy-subrc = 0.
        ls_api_req_pack_comp_item-source = <fs>.
      ENDIF.

    ENDIF.

    rv_payload = /ui2/cl_json=>serialize( data        = ls_api_req_pack_comp_item
                                          compress    = abap_true
                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    ms_entity_hu_data_api = ls_api_req_pack_comp_item.
  ENDMETHOD.


  METHOD get_element_id_by_index.
    READ TABLE ms_entity_hu_data_api-elements REFERENCE INTO DATA(lr_api_element) INDEX iv_index.

    IF sy-subrc = 0.
      rv_result = lr_api_element->id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
