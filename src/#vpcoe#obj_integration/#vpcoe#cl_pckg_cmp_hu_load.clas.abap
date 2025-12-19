class /VPCOE/CL_PCKG_CMP_HU_LOAD definition
  public
  inheriting from /VPCOE/CL_UPH_REPORT_BASE
  final
  create public .

public section.

  interfaces /VPCOE/IF_PCKG_CMP_HU_LOAD .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS /VPCOE/CL_PCKG_CMP_HU_LOAD IMPLEMENTATION.


  METHOD /vpcoe/if_pckg_cmp_hu_load~derive_input_from_parameters.
    rv_value-source_id          = iv_source_id.
    rv_value-rfc_des            = iv_rfc_destination.
    rv_value-act_goods_mvt_date = iv_act_goods_mvt_date_range.
    rv_value-category           = iv_category_range.
    rv_value-country            = iv_country_range.
    rv_value-distribution       = iv_distribution_range.
    rv_value-division           = iv_division_range.
    rv_value-document_type      = iv_document_type_range.
    rv_value-material           = iv_material_range.
    rv_value-material_group     = iv_material_group_range.
    rv_value-material_type      = iv_material_type_range.
    rv_value-plant              = iv_plant_range.
    rv_value-plant_country      = iv_plant_country_rage.
    rv_value-sales_org          = iv_sales_org_range.
    rv_value-sddoc              = iv_sddoc_range.
    rv_value-ship_to_party      = iv_ship_to_party_range.
  ENDMETHOD.


  METHOD /vpcoe/if_pckg_cmp_hu_load~is_authorized.

    AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD'  FIELD /vpcoe/if_pckg_cmp_hu_load=>gc_tcode.

    IF sy-subrc <> 0.
      rv_value = abap_false.
    ELSE.
      rv_value = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
