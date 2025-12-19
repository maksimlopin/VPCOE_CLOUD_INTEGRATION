CLASS /vpcoe/cl_pckg_cmp_it_v1 DEFINITION
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

    TYPES:
      BEGIN OF mty_s_api_pack_comp,
        id    TYPE c LENGTH 40,
        items TYPE /vpcoe/t_uph_api_cmp_item,
      END OF mty_s_api_pack_comp,
      mty_t_api_pack_comp TYPE STANDARD TABLE OF mty_s_api_pack_comp WITH DEFAULT KEY,

      BEGIN OF mty_s_api_req_pack_comp_item,
        source   TYPE c LENGTH 64,
        elements TYPE mty_t_api_pack_comp,
      END OF mty_s_api_req_pack_comp_item.

    CONSTANTS mc_entity_url_suffix TYPE string    VALUE '/PackagingCompositionItems'.

    DATA ms_entity_hu_data_api TYPE mty_s_api_req_pack_comp_item.
ENDCLASS.



CLASS /VPCOE/CL_PCKG_CMP_IT_V1 IMPLEMENTATION.


  METHOD /vpcoe/if_uph_transfer_mapper~get_entity_url_suffix.
*    rv_entity_url_suffix = mc_entity_url_suffix.
    rv_entity_url_suffix = NEW /vpcoe/cl_common_helper( iv_srv_grp  = 'PLM'
                                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-handling_units
                                                        iv_api_type = 'PLM' )->get_service_url( ).
  ENDMETHOD.


  METHOD /vpcoe/if_uph_transfer_mapper~prepare_payload.
    DATA ls_api_req_pack_comp_item TYPE mty_s_api_req_pack_comp_item.
    DATA ls_api_pack_comp          TYPE mty_s_api_pack_comp.
    DATA ls_api_pack_comp_item     TYPE /vpcoe/s_uph_api_cmp_item.
    DATA lr_ent_pckg_comp          TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_hdr.
    DATA lr_ent_pckg_comp_item     TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_itm.

    CLEAR rv_payload.

    DATA(lt_api_pack_comp) = VALUE mty_t_api_pack_comp( ).

    LOOP AT it_entity_data INTO DATA(lr_entity_data).

      lr_ent_pckg_comp ?= lr_entity_data.

      DATA(ls_comp_hdr_data) = lr_ent_pckg_comp->get_data( ).
      DATA(lt_comp_items) = lr_ent_pckg_comp->get_items( ).

      DATA(lt_api_pack_comp_item) = VALUE /vpcoe/t_uph_api_cmp_item( ).
      LOOP AT lt_comp_items INTO lr_entity_data.

        lr_ent_pckg_comp_item ?= lr_entity_data.
        DATA(ls_ent_pckg_comp_item) = lr_ent_pckg_comp_item->get_data( ).

        ls_api_pack_comp_item = VALUE #( packaging_element_id      = ls_ent_pckg_comp_item-packagingelementdisplayid
                                         level                     = ls_ent_pckg_comp_item-levelcode
                                         quantity                  = ls_ent_pckg_comp_item-quantity
                                         quantity_unit_of_measure  = ls_ent_pckg_comp_item-quantityunitofmeasureid
                                         count                     = ls_ent_pckg_comp_item-count
                                         usage                     = ls_ent_pckg_comp_item-usage
                                         packaging_element_version = ls_ent_pckg_comp_item-packagingelementversion
                                         wwf_group                 = ls_ent_pckg_comp_item-wwfgroup
                                         epr_group                 = ls_ent_pckg_comp_item-eprgroup   ).
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
