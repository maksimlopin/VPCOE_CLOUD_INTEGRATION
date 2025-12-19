class /VPCOE/CL_UPH_TM_PCKG_CMP_GP_2 definition
  public
  inheriting from /VPCOE/CL_UPH_TRANSFER_MAPPER
  create public .

public section.

  methods /VPCOE/IF_UPH_TRANSFER_MAPPER~EVALUATE_RESPONSE
    redefinition .
  methods /VPCOE/IF_UPH_TRANSFER_MAPPER~GET_ENTITY_URL_SUFFIX
    redefinition .
  methods /VPCOE/IF_UPH_TRANSFER_MAPPER~PREPARE_PAYLOAD
    redefinition .
  PROTECTED SECTION.
    METHODS: get_element_id_by_index REDEFINITION.

private section.

  types:
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "-- Request input structures
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    BEGIN OF lty_s_sel_period,
        from TYPE dats,
        to   TYPE dats,
      END OF lty_s_sel_period .
  types:
    BEGIN OF lty_s_prd_assigment,
        product_id                 TYPE string,
        business_process_direction TYPE string,
        supplier_id                TYPE string,
      END OF lty_s_prd_assigment .
  types:
    lty_t_prd_assigments TYPE STANDARD TABLE OF lty_s_prd_assigment WITH DEFAULT KEY .
  types:
    BEGIN OF lty_s_get_packcomp_input,
        selection_period    TYPE lty_s_sel_period,
        product_assignments TYPE lty_t_prd_assigments,
      END OF lty_s_get_packcomp_input .
  types:
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "-- Request output structures
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    BEGIN OF lty_s_packcomp_for_prod,
        id                         TYPE c LENGTH 40,
        valid_from                 TYPE dats,
        valid_to                   TYPE dats,
        supplier_id                TYPE c LENGTH 10,
        business_process_direction TYPE c LENGTH 20,
      END OF lty_s_packcomp_for_prod .
  types:
    lty_t_packcomp_for_prod TYPE STANDARD TABLE OF lty_s_packcomp_for_prod WITH DEFAULT KEY .
  types:
    BEGIN OF lty_s_get_packcomp_return,
        selection_criteria     TYPE lty_s_prd_assigment,
        packaging_compositions TYPE lty_t_packcomp_for_prod,
      END OF lty_s_get_packcomp_return .
  types:
    lty_t_get_packcomp_return TYPE STANDARD TABLE OF lty_s_get_packcomp_return WITH DEFAULT KEY .
  types:
    BEGIN OF lty_s_get_packcomp_output,
        value TYPE lty_t_get_packcomp_return,
      END OF lty_s_get_packcomp_output .

  constants MC_URL_SUFFIX_GET_PCKG_COMP type STRING value '/GetPackagingCompositionsForProduct' ##NO_TEXT.
ENDCLASS.



CLASS /VPCOE/CL_UPH_TM_PCKG_CMP_GP_2 IMPLEMENTATION.


  METHOD /vpcoe/if_uph_transfer_mapper~evaluate_response.

    super->/vpcoe/if_uph_transfer_mapper~evaluate_response( EXPORTING iv_response = iv_response IMPORTING et_messages = et_messages ).

    DATA ls_packcomp_output TYPE lty_s_get_packcomp_output.

    DATA(lv_response) = iv_response.

    IF lv_response IS NOT INITIAL.

      /ui2/cl_json=>deserialize( EXPORTING json        = lv_response
                                           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                 CHANGING  data        = ls_packcomp_output ).
    ENDIF.

    IF ls_packcomp_output IS NOT INITIAL.

      LOOP AT it_entity_data INTO DATA(lr_entity_data).

        DATA(lr_entity) = CAST /vpcoe/cl_uph_ent_pckg_cmp_gp( lr_entity_data ).

        LOOP AT ls_packcomp_output-value INTO DATA(ls_packcomp_return).

          LOOP AT ls_packcomp_return-packaging_compositions INTO DATA(ls_packaging_composition).

            lr_entity->add_packcomp_for_product( is_product_assignment = VALUE #( product_id = ls_packcomp_return-selection_criteria-product_id
                                                                                             business_process_direction = ls_packcomp_return-selection_criteria-business_process_direction
                                                                                             supplier_id = ls_packcomp_return-selection_criteria-supplier_id
                                                                                           )
                                                            is_packaging_composition = VALUE #(
                                                                                  id = ls_packaging_composition-id
                                                                                  valid_from = ls_packaging_composition-valid_from
                                                                                  valid_to   = ls_packaging_composition-valid_to
                                                                                  supplier_id  = ls_packaging_composition-supplier_id
                                                                                  business_process_direction = ls_packaging_composition-business_process_direction )   ).
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_transfer_mapper~get_entity_url_suffix.
*    rv_entity_url_suffix = mc_url_suffix_get_pckg_comp.

    rv_entity_url_suffix = NEW /vpcoe/cl_common_helper( iv_srv_grp  = 'PLM'
                                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-packaging_composition
                                                        iv_api_type = 'PLM' )->get_service_url( ).

  ENDMETHOD.


  METHOD /vpcoe/if_uph_transfer_mapper~prepare_payload.
    DATA lr_entity             TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_gp.

    DATA ls_get_packcomp_input TYPE lty_s_get_packcomp_input.
    DATA ls_prd_assignment     TYPE lty_s_prd_assigment.

    CLEAR rv_payload.

    LOOP AT it_entity_data INTO DATA(lr_entity_data).
      lr_entity ?= lr_entity_data.

      " do the mapping for the input
      CLEAR ls_get_packcomp_input.
      ls_get_packcomp_input-selection_period-from = lr_entity->get_selection_period( )-from.
      ls_get_packcomp_input-selection_period-to   = lr_entity->get_selection_period( )-to.

      " map products
      DATA(lt_prod_assignments) = lr_entity->get_product_assignments( ).

        LOOP AT lt_prod_assignments INTO DATA(ls_prod_assignments).

          CLEAR ls_prd_assignment.
          ls_prd_assignment-product_id                 = ls_prod_assignments-product_id.
          ls_prd_assignment-business_process_direction = ls_prod_assignments-business_process_direction.
          ls_prd_assignment-supplier_id                = ls_prod_assignments-supplier_id.

          APPEND ls_prd_assignment TO ls_get_packcomp_input-product_assignments.
        ENDLOOP.

    ENDLOOP.

    rv_payload = /ui2/cl_json=>serialize( data        = ls_get_packcomp_input
                                          compress    = abap_true
                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    REPLACE ALL OCCURRENCES OF ':""' IN rv_payload WITH ': null' ##NO_TEXT.
  ENDMETHOD.                                               "#EC CI_NOES


  METHOD get_element_id_by_index.
  ENDMETHOD.
ENDCLASS.
