class /VPCOE/CL_UPH_TM_HU definition
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

  constants MC_HU type STRING value '/PackagingCompositionItems' ##NO_TEXT.
  data MS_ENTITY_HU_DATA_API type /VPCOE/STR_HU_PRD_JSON .
  constants MC_HU_TRUE type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  constants MC_HU_FALSE type ABAP_BOOL value 'N' ##NO_TEXT.
  constants MC_ENTITY_NAME_HU type STRING value 'Handling_unit' ##NO_TEXT.
ENDCLASS.



CLASS /VPCOE/CL_UPH_TM_HU IMPLEMENTATION.


  METHOD /VPCOE/IF_UPH_TRANSFER_MAPPER~CHECK_DEPRECATED_ATTRIBUTES.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_transfer_mapper~evaluate_response. "#EC CI_CYCLO

    " The method was refactored and is kept to ease downports
    super->/vpcoe/if_uph_transfer_mapper~evaluate_response( EXPORTING iv_response = iv_response
                                                           IMPORTING et_messages = et_messages ).

  ENDMETHOD. "#EC CI_CYCLO                                    "#EC CI_NESTING


  METHOD /VPCOE/IF_UPH_TRANSFER_MAPPER~GET_ENTITY_URL_SUFFIX.
*    rv_entity_url_suffix = mc_hu.
    rv_entity_url_suffix = NEW /vpcoe/cl_common_helper( iv_srv_grp  = 'PLM'
                                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-handling_units
                                                        iv_api_type = 'PLM' )->get_service_url( ).
  ENDMETHOD.


  METHOD /vpcoe/if_uph_transfer_mapper~prepare_payload.

    DATA: ls_entity_hu_data_api TYPE /vpcoe/str_hu_prd_json,
          ls_entity_hu_api      TYPE /vpcoe/s_jsn_hu,
          lr_entity_hu          TYPE REF TO /vpcoe/cl_uph_ent_handl_unit.

    CLEAR: rv_payload.

    LOOP AT it_entity_data INTO DATA(lr_entity_data).

      lr_entity_hu ?= lr_entity_data.

      DATA(ls_hu_data) = lr_entity_hu->get_data( ).

      ls_entity_hu_api = VALUE #( id = ls_hu_data-id
                                  items = VALUE #( FOR ls_hu IN ls_hu_data-items ( packaging_element_id = ls_hu-packaging_element_id
                                                                                   level                = ls_hu-level
                                                                                   quantity             = me->convert_dec_to_char( ls_hu-quantity )
                                                                                   quantity_unit_of_measure = ls_hu-quantity_unit_of_measure
                                                                                   count                = ls_hu-count
                                                                                   usage                = ls_hu-usage
                                                                                   packaging_element_version = ls_hu-packaging_element_version
                                                                                   wwf_group            = ls_hu-wwf_group
                                                                                   epr_group            = ls_hu-epr_group )  ) ).
      APPEND ls_entity_hu_api TO ls_entity_hu_data_api-elements.

      CLEAR ls_entity_hu_api.

    ENDLOOP.

    IF is_parameters IS NOT INITIAL.
      DATA ls_parameters TYPE /vpcoe/s_selopt_hu.
      ls_parameters = CORRESPONDING #( is_parameters ).
      ls_entity_hu_data_api-source = ls_parameters-source_id.
    ENDIF.

    rv_payload = /vpcoe/cl_plm_helper=>serialize_json( is_data        = ls_entity_hu_data_api
                                                       iv_pretty_name = /vpcoe/cl_plm_helper=>sc_pretty_mode-camel_case
                                                       iv_compress    = abap_true ).

    REPLACE ALL OCCURRENCES OF '"isReinforced":"X"' IN rv_payload WITH '"isReinforced": true' ##no_text.
    REPLACE ALL OCCURRENCES OF '"isReinforced":""' IN rv_payload WITH '"isReinforced": false' ##no_text.
    REPLACE ALL OCCURRENCES OF '"isReinforced":"N"' IN rv_payload WITH '"isReinforced": false' ##no_text.
    REPLACE ALL OCCURRENCES OF '"isReinforced":"Y"' IN rv_payload WITH '"isReinforced": true' ##no_text.
    REPLACE ALL OCCURRENCES OF '"null"' IN rv_payload WITH 'null' ##no_text.

    ms_entity_hu_data_api = ls_entity_hu_data_api.

  ENDMETHOD.                                               "#EC CI_NOES


  METHOD GET_ELEMENT_ID_BY_INDEX.
    READ TABLE ms_entity_hu_data_api-elements REFERENCE INTO DATA(lr_api_element) INDEX iv_index.

    IF sy-subrc = 0.
      rv_result = lr_api_element->id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
