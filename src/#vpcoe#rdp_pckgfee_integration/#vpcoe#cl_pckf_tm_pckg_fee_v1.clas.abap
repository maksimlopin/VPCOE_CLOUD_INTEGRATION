CLASS /vpcoe/cl_pckf_tm_pckg_fee_v1 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /vpcoe/if_pckf_transfer_mapper .

    TYPES:
      BEGIN OF ltys_entity_error_details,
        code    TYPE string,
        message TYPE string,
        target  TYPE string,
      END OF ltys_entity_error_details .
    TYPES:
      lty_entity_error_details TYPE STANDARD TABLE OF ltys_entity_error_details WITH NON-UNIQUE DEFAULT KEY .

    TYPES:
      BEGIN OF ltys_pckg_fee_error,
        code    TYPE string,
        message TYPE string,
        details TYPE lty_entity_error_details,
      END OF ltys_pckg_fee_error .

    TYPES:
      BEGIN OF ltys_errorlog_api,
        error TYPE ltys_pckg_fee_error,
      END OF ltys_errorlog_api .

    TYPES:
      BEGIN OF ltys_pckf_api_pckg_fee_ext_flt,
        production_order_check        TYPE boole_d,
        inhouse_prodn_chargeable_fee  TYPE float,
        inhouse_prodn_statistical_fee	TYPE float,
        plastic_weight_in_kg          TYPE float,
        non_recycled_plastic_in_kg    TYPE float,
        exemptions                    TYPE /vpcoe/t_pckf_api_exemption,
        tax_for_packaging_as_material	TYPE float,
      END OF ltys_pckf_api_pckg_fee_ext_flt.

     TYPES:
      BEGIN OF ltys_pckf_api_conai_pc_fee_ext,
        packaging_code       TYPE string,
        report_fraction      TYPE string,
        report_fraction_name TYPE string,
        fee_per_tonne        TYPE float,
      END OF ltys_pckf_api_conai_pc_fee_ext.


    TYPES:
      BEGIN OF ltys_pckf_api_pckg_fee_float,
        context                       TYPE string,
        product                       TYPE string,
        valid_from                    TYPE dats,
        valid_to                      TYPE dats,
        report_configuration          TYPE string,
        report_category_country       TYPE string,
        report_category_region        TYPE string,
        reference_quantity            TYPE float,
        base_unit_of_measure          TYPE string,
        base_unit_of_measure_iso_code TYPE string,
        total_fee                     TYPE float,
        currency                      TYPE string,
        last_changed_at               TYPE timestampl,
        applicable_organization_data  TYPE /vpcoe/t_pckf_api_org_data,
        ship_to_party_role            TYPE /vpcoe/t_pckf_api_ship_parrole,
        report_category               TYPE string,
        packaging_composition         TYPE string,
        business_process_direction    TYPE string,
        packaging_fee_extension       TYPE ltys_pckf_api_pckg_fee_ext_flt,
        conai_packaging_fee_extension TYPE ltys_pckf_api_conai_pc_fee_ext,
        suppliers                     TYPE /vpcoe/t_pckf_api_suplr,
        is_fallback_fee               TYPE boole_d,
      END OF ltys_pckf_api_pckg_fee_float.

    TYPES ltyt_pckf_api_pckg_fee_float TYPE STANDARD TABLE OF ltys_pckf_api_pckg_fee_float WITH NON-UNIQUE KEY product.

    TYPES:
      BEGIN OF ltys_pckg_fee_api_float,
        value      TYPE ltyt_pckf_api_pckg_fee_float,
        next_link  TYPE string,
        delta_link TYPE string,
      END OF ltys_pckg_fee_api_float.

    TYPES:
      BEGIN OF ltys_pckg_fee_api,
        value      TYPE /vpcoe/t_pckf_api_pckg_fee,
        next_link  TYPE string,
        delta_link TYPE string,
      END OF ltys_pckg_fee_api.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS mc_pckg_fee_suffix TYPE string VALUE 'PackagingFees' ##NO_TEXT.

    METHODS remove_prefix_context
      IMPORTING
        !iv_attribute       TYPE data
        !iv_prefix          TYPE string
      RETURNING
        VALUE(rv_attribute) TYPE string .
ENDCLASS.



CLASS /VPCOE/CL_PCKF_TM_PCKG_FEE_V1 IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_transfer_mapper~evaluate_response. "#EC CI_CYCLO "#EC CI_NOES

    CLEAR et_messages.

    DATA ls_errorlog_api TYPE ltys_errorlog_api.

    DATA: lt_messages       TYPE /vpcoe/t_uph_msg,
          ls_message        TYPE /vpcoe/s_uph_msg,
          lt_message_detail TYPE /vpcoe/t_uph_msg_detail,
          ls_message_detail TYPE /vpcoe/s_uph_msg_detail,
          lv_message        TYPE string,
          lv_string_val     TYPE string,
          lv_target_str     TYPE string,
          lv_index          TYPE i.

*deserialize json string json into abap structure
    DATA(lv_response) = iv_response .

    IF lv_response IS NOT INITIAL.

      /vpcoe/cl_common_helper=>deserialize_json( EXPORTING iv_json = lv_response
                                                           iv_pretty_name = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                                 CHANGING cs_data = ls_errorlog_api ).
    ENDIF.

    IF ls_errorlog_api-error IS NOT INITIAL.

      DATA(ls_pckg_error) = ls_errorlog_api-error.

      lv_message =  |{ ls_pckg_error-code } { ls_pckg_error-message }|.
      cl_message_helper=>set_msg_vars_for_clike( lv_message ).

      MESSAGE e024(/vpcoe/plm) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.
      ls_message-msgty = sy-msgty.
      ls_message-msgid = sy-msgid.
      ls_message-msgno = sy-msgno.
      ls_message-msgv1 = sy-msgv1.
      ls_message-msgv2 = sy-msgv2.
      ls_message-msgv3 = sy-msgv3.
      ls_message-msgv4 = sy-msgv4.
      CLEAR ls_message-details.

      IF ls_pckg_error-details IS NOT INITIAL.

        CLEAR lt_message_detail.

        LOOP AT ls_pckg_error-details REFERENCE INTO DATA(lr_pckg_detail).

          lv_message =  |{ lr_pckg_detail->code } { lr_pckg_detail->message }|.
          cl_message_helper=>set_msg_vars_for_clike( lv_message ).

          MESSAGE e025(/vpcoe/plm) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.
          ls_message_detail-msgty = sy-msgty.
          ls_message_detail-msgid = sy-msgid.
          ls_message_detail-msgno = sy-msgno.
          ls_message_detail-msgv1 = sy-msgv1.
          ls_message_detail-msgv2 = sy-msgv2.
          ls_message_detail-msgv3 = sy-msgv3.
          ls_message_detail-msgv4 = sy-msgv4.

          APPEND ls_message_detail TO lt_message_detail.
        ENDLOOP.

        ls_message-details = lt_message_detail.

      ENDIF.

      APPEND ls_message TO lt_messages.
    ENDIF.

    et_messages = lt_messages.

  ENDMETHOD. "#EC CI_CYCLO       "#EC CI_NOES                    "#EC CI_NESTING


  METHOD /vpcoe/if_pckf_transfer_mapper~get_entity_url_suffix.
    rv_entity_url_suffix = mc_pckg_fee_suffix.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_transfer_mapper~parse_payload.

    DATA: ls_pckg_fee_api_payload TYPE ltys_pckg_fee_api_float,
          ls_pckg_fee_entity      TYPE /vpcoe/s_pckf_ent_pckg_fee,
          lt_entity_data          TYPE /vpcoe/t_pckf_entity_data.


    CLEAR et_entity_data.

*deserialize json string json into abap structure
    DATA(lv_payload) = iv_payload.

    IF lv_payload IS NOT INITIAL.

      /vpcoe/cl_common_helper=>deserialize_json( EXPORTING iv_json          = lv_payload
                                                           iv_pretty_name   = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                                           it_name_mappings = VALUE #( ( abap = 'inhouse_prodn_chargeable_fee'   json = 'InhouseProductionChargeableFee'  )
                                                                                       ( abap = 'inhouse_prodn_statistical_fee'  json = 'InhouseProductionStatisticalFee' )
                                                                                       ( abap = 'conai_packaging_fee_extension'  json = 'CONAIPackagingFeeExtension' ) )
                                                 CHANGING cs_data = ls_pckg_fee_api_payload ).

      LOOP AT ls_pckg_fee_api_payload-value INTO DATA(ls_pckg_fee_api).

        CLEAR ls_pckg_fee_entity.

        ls_pckg_fee_entity-context                        = ls_pckg_fee_api-context.
        ls_pckg_fee_entity-business_process_direction     = ls_pckg_fee_api-business_process_direction.
        ls_pckg_fee_entity-product                        = remove_prefix_context( iv_attribute = ls_pckg_fee_api-product
                                                                                   iv_prefix = ls_pckg_fee_api-context ).
        ls_pckg_fee_entity-validfrom                      = ls_pckg_fee_api-valid_from.
        ls_pckg_fee_entity-validto                        = ls_pckg_fee_api-valid_to.
        ls_pckg_fee_entity-reportconfiguration            = ls_pckg_fee_api-report_configuration.
        ls_pckg_fee_entity-reportcategory                 = ls_pckg_fee_api-report_category.
        ls_pckg_fee_entity-reportcategorycountry          = remove_prefix_context( iv_attribute = ls_pckg_fee_api-report_category_country
                                                                                   iv_prefix = ls_pckg_fee_api-context ).
        ls_pckg_fee_entity-reportcategoryregion           = remove_prefix_context( iv_attribute = ls_pckg_fee_api-report_category_region
                                                                                   iv_prefix = ls_pckg_fee_api-context ).
        ls_pckg_fee_entity-packagingcomposition           = remove_prefix_context( iv_attribute = ls_pckg_fee_api-packaging_composition
                                                                                   iv_prefix = ls_pckg_fee_api-context ).
        ls_pckg_fee_entity-referencequantity              = ls_pckg_fee_api-reference_quantity.
        ls_pckg_fee_entity-baseunitofmeasure              = remove_prefix_context( iv_attribute = ls_pckg_fee_api-base_unit_of_measure
                                                                                   iv_prefix = ls_pckg_fee_api-context ).
        ls_pckg_fee_entity-totalfee                       = ls_pckg_fee_api-total_fee.
        ls_pckg_fee_entity-currency                       = remove_prefix_context( iv_attribute = ls_pckg_fee_api-currency
                                                                                   iv_prefix = ls_pckg_fee_api-context ).

        IF ls_pckg_fee_api-packaging_fee_extension IS NOT INITIAL.
          ls_pckg_fee_entity-packagingfeeextension-productionordercheck           = ls_pckg_fee_api-packaging_fee_extension-production_order_check.
          ls_pckg_fee_entity-packagingfeeextension-plasticweightinkg              = ls_pckg_fee_api-packaging_fee_extension-plastic_weight_in_kg.
          ls_pckg_fee_entity-packagingfeeextension-nonrecycledplasticinkg         = ls_pckg_fee_api-packaging_fee_extension-non_recycled_plastic_in_kg.
          ls_pckg_fee_entity-packagingfeeextension-inhouseproductionchargeablefee = ls_pckg_fee_api-packaging_fee_extension-inhouse_prodn_chargeable_fee.
          ls_pckg_fee_entity-packagingfeeextension-inhouseprodnstatisticalfee     = ls_pckg_fee_api-packaging_fee_extension-inhouse_prodn_statistical_fee.
          ls_pckg_fee_entity-packagingfeeextension-taxforpackagingasmaterial      = ls_pckg_fee_api-packaging_fee_extension-tax_for_packaging_as_material.
          ls_pckg_fee_entity-packagingfeeextension-exemptions = ls_pckg_fee_api-packaging_fee_extension-exemptions.
        ENDIF.
        IF ls_pckg_fee_api-conai_packaging_fee_extension IS NOT INITIAL.
          ls_pckg_fee_entity-conaipackagingfeeextension-packagingcode      = ls_pckg_fee_api-conai_packaging_fee_extension-packaging_code.
          ls_pckg_fee_entity-conaipackagingfeeextension-reportfraction     = ls_pckg_fee_api-conai_packaging_fee_extension-report_fraction.
          ls_pckg_fee_entity-conaipackagingfeeextension-reportfractionname = ls_pckg_fee_api-conai_packaging_fee_extension-report_fraction_name.
          ls_pckg_fee_entity-conaipackagingfeeextension-feepertonne        = ls_pckg_fee_api-conai_packaging_fee_extension-fee_per_tonne.
        ENDIF.

        DATA ls_pckg_fee_ent_suppliers TYPE /vpcoe/s_pckf_ent_suplr.
        LOOP AT ls_pckg_fee_api-suppliers INTO DATA(ls_pckg_fee_api_suppliers).

          CLEAR ls_pckg_fee_ent_suppliers.
          ls_pckg_fee_ent_suppliers-supplier = remove_prefix_context( iv_attribute = ls_pckg_fee_api_suppliers-supplier
                                                                      iv_prefix = ls_pckg_fee_api-context ).
          ls_pckg_fee_ent_suppliers-is_excluded_from_report_config = ls_pckg_fee_api_suppliers-is_excluded_from_report_config.

          APPEND ls_pckg_fee_ent_suppliers TO ls_pckg_fee_entity-suppliers.
        ENDLOOP.

        ls_pckg_fee_entity-isfallbackfee                  = ls_pckg_fee_api-is_fallback_fee.
        ls_pckg_fee_entity-lastchangeddate                = ls_pckg_fee_api-last_changed_at.

        DATA ls_pckg_fee_ent_orgdata TYPE /vpcoe/s_pckf_ent_org_data.
        LOOP AT ls_pckg_fee_api-applicable_organization_data INTO DATA(ls_pckg_fee_api_org_data).

          CLEAR ls_pckg_fee_ent_orgdata.
          ls_pckg_fee_ent_orgdata-companycode         = remove_prefix_context( iv_attribute = ls_pckg_fee_api_org_data-company_code
                                                                               iv_prefix = ls_pckg_fee_api-context ).
          ls_pckg_fee_ent_orgdata-salesorganization   = remove_prefix_context( iv_attribute = ls_pckg_fee_api_org_data-sales_organization
                                                                               iv_prefix = ls_pckg_fee_api-context ).
          ls_pckg_fee_ent_orgdata-division            = remove_prefix_context( iv_attribute = ls_pckg_fee_api_org_data-division
                                                                               iv_prefix = ls_pckg_fee_api-context ).
          ls_pckg_fee_ent_orgdata-distributionchannel = remove_prefix_context( iv_attribute = ls_pckg_fee_api_org_data-distribution_channel
                                                                               iv_prefix = ls_pckg_fee_api-context ).
          APPEND ls_pckg_fee_ent_orgdata TO ls_pckg_fee_entity-applicableorganizationdata.

        ENDLOOP.

        LOOP AT ls_pckg_fee_api-ship_to_party_role INTO DATA(lv_pckg_fee_api_shiprole).
          APPEND lv_pckg_fee_api_shiprole TO ls_pckg_fee_entity-shiptopartyrole.
        ENDLOOP.

        DATA(lr_entity) = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = ls_pckg_fee_entity ).
        APPEND lr_entity TO et_entity_data.

      ENDLOOP.

      ev_delta_link = ls_pckg_fee_api_payload-delta_link.
      ev_next_link = ls_pckg_fee_api_payload-next_link.


    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_transfer_mapper~prepare_payload.

    DATA: lr_entity       TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee.

    DATA: ls_entity_data_api      TYPE /vpcoe/s_pckf_api_pckg_fee,
          ls_pckg_fee_api         TYPE /vpcoe/s_pckf_api_pckg_fee,
          ls_pckg_fee_api_payload TYPE ltys_pckg_fee_api.

    CLEAR: rv_payload.

    LOOP AT it_entity_data INTO DATA(lr_entity_data).
      lr_entity ?= lr_entity_data.

      "do the mapping
      DATA(ls_pckg_fee_entity) = lr_entity->get_data( ).

      CLEAR ls_pckg_fee_api.

      ls_pckg_fee_api-context                                               = ls_pckg_fee_entity-context.
      ls_pckg_fee_api-business_process_direction                            = ls_pckg_fee_entity-business_process_direction.
      ls_pckg_fee_api-product                                               = ls_pckg_fee_entity-product.
      ls_pckg_fee_api-valid_from                                            = ls_pckg_fee_entity-validfrom.
      ls_pckg_fee_api-valid_to                                              = ls_pckg_fee_entity-validto.
      ls_pckg_fee_api-report_configuration                                  = ls_pckg_fee_entity-reportconfiguration.
      ls_pckg_fee_api-report_category                                       = ls_pckg_fee_entity-reportcategory.
      ls_pckg_fee_api-report_category_country                               = ls_pckg_fee_entity-reportcategorycountry.
      ls_pckg_fee_api-report_category_region                                = ls_pckg_fee_entity-reportcategoryregion.
      ls_pckg_fee_api-packaging_composition                                 = ls_pckg_fee_entity-packagingcomposition.
      ls_pckg_fee_api-reference_quantity                                    = ls_pckg_fee_entity-referencequantity.
      ls_pckg_fee_api-base_unit_of_measure                                  = ls_pckg_fee_entity-baseunitofmeasure.
      ls_pckg_fee_api-total_fee                                             = ls_pckg_fee_entity-totalfee.
      ls_pckg_fee_api-currency                                              = ls_pckg_fee_entity-currency.

      IF ls_pckg_fee_entity-packagingfeeextension IS NOT INITIAL.
        ls_pckg_fee_api-packaging_fee_extension-production_order_check         = ls_pckg_fee_entity-packagingfeeextension-productionordercheck.
        ls_pckg_fee_api-packaging_fee_extension-plastic_weight_in_kg           = ls_pckg_fee_entity-packagingfeeextension-plasticweightinkg.
        ls_pckg_fee_api-packaging_fee_extension-non_recycled_plastic_in_kg     = ls_pckg_fee_entity-packagingfeeextension-nonrecycledplasticinkg.
        ls_pckg_fee_api-packaging_fee_extension-inhouse_prodn_chargeable_fee    = ls_pckg_fee_entity-packagingfeeextension-inhouseproductionchargeablefee.
        ls_pckg_fee_api-packaging_fee_extension-inhouse_prodn_statistical_fee   = ls_pckg_fee_entity-packagingfeeextension-inhouseprodnstatisticalfee.
        ls_pckg_fee_api-packaging_fee_extension-tax_for_packaging_as_material  = ls_pckg_fee_entity-packagingfeeextension-taxforpackagingasmaterial.
        ls_pckg_fee_api-packaging_fee_extension-exemptions                      = ls_pckg_fee_entity-packagingfeeextension-exemptions.
      ENDIF.

       IF ls_pckg_fee_entity-conaipackagingfeeextension IS NOT INITIAL.
          ls_pckg_fee_api-conai_packaging_fee_extension-packaging_code = ls_pckg_fee_entity-conaipackagingfeeextension-packagingcode.
          ls_pckg_fee_api-conai_packaging_fee_extension-report_fraction = ls_pckg_fee_entity-conaipackagingfeeextension-reportfraction.
          ls_pckg_fee_api-conai_packaging_fee_extension-report_fraction_name = ls_pckg_fee_entity-conaipackagingfeeextension-reportfractionname.
          ls_pckg_fee_api-conai_packaging_fee_extension-fee_per_tonne = ls_pckg_fee_entity-conaipackagingfeeextension-feepertonne.
        ENDIF.

      DATA ls_pckg_fee_api_suppliers TYPE /vpcoe/s_pckf_api_suplr.
      LOOP AT ls_pckg_fee_entity-suppliers INTO DATA(ls_pckg_fee_ety_suppliers).
        CLEAR ls_pckg_fee_api_suppliers.
        ls_pckg_fee_api_suppliers-supplier         =  ls_pckg_fee_ety_suppliers-supplier.
        ls_pckg_fee_api_suppliers-is_excluded_from_report_config   =  ls_pckg_fee_ety_suppliers-is_excluded_from_report_config.
        APPEND ls_pckg_fee_api_suppliers TO ls_pckg_fee_api-suppliers.
      ENDLOOP.

      ls_pckg_fee_api-is_fallback_fee                                     = ls_pckg_fee_entity-isfallbackfee.
      ls_pckg_fee_api-last_changed_at                                     = ls_pckg_fee_entity-lastchangeddate.

      DATA ls_pckg_fee_api_orgdata TYPE /vpcoe/s_pckf_api_org_data.
      LOOP AT ls_pckg_fee_entity-applicableorganizationdata INTO DATA(ls_pckg_fee_ety_org_data).
        CLEAR ls_pckg_fee_api_orgdata.
        ls_pckg_fee_api_orgdata-company_code         =  ls_pckg_fee_ety_org_data-companycode.
        ls_pckg_fee_api_orgdata-sales_organization   =  ls_pckg_fee_ety_org_data-salesorganization.
        ls_pckg_fee_api_orgdata-division             =  ls_pckg_fee_ety_org_data-division             .
        ls_pckg_fee_api_orgdata-distribution_channel =  ls_pckg_fee_ety_org_data-distributionchannel.
        APPEND ls_pckg_fee_api_orgdata TO ls_pckg_fee_api-applicable_organization_data.

      ENDLOOP.

      LOOP AT ls_pckg_fee_entity-shiptopartyrole INTO DATA(lv_pckg_fee_ety_shiprole).
        APPEND lv_pckg_fee_ety_shiprole TO ls_pckg_fee_api-ship_to_party_role.
      ENDLOOP.

      APPEND ls_pckg_fee_api TO ls_pckg_fee_api_payload-value.

    ENDLOOP.

    rv_payload = /vpcoe/cl_common_helper=>serialize_json( is_data          = ls_pckg_fee_api_payload
                                                          iv_compress      = abap_true
                                                          iv_pretty_name   = /vpcoe/cl_common_helper=>sc_pretty_mode-camel_case
                                                          iv_ts_as_iso8601 = abap_true
                                                          it_name_mappings = VALUE #( ( abap = 'inhouse_prodn_chargeable_fee' json = 'InhouseProductionChargeableFee' )
                                                                                      ( abap = 'inhouse_prodn_statistical_fee' json = 'InhouseProductionStatisticalFee' )
                                                                                      ( abap = 'conai_packaging_fee_extension'  json = 'CONAIPackagingFeeExtension' ) )  ).

  ENDMETHOD.                                               "#EC CI_NOES


  METHOD remove_prefix_context.

    DATA lv_replace_len TYPE i.

    rv_attribute = iv_attribute.
    IF iv_prefix IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_replace) = |{ iv_prefix }:|.
    lv_replace_len = strlen( lv_replace ).

    IF strlen( iv_attribute ) < lv_replace_len.
      RETURN.
    ENDIF.

    "ensure only occurence at position 0 is replaced
    IF substring( val = iv_attribute off = 0 len = lv_replace_len ) = lv_replace.
      rv_attribute = replace( val = iv_attribute sub = lv_replace with = '' occ = 1 ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
