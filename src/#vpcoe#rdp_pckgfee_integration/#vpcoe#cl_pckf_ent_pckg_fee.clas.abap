class /VPCOE/CL_PCKF_ENT_PCKG_FEE definition
  public
  create public .

public section.

  interfaces /VPCOE/IF_PCKF_ENTITY_DATA .

  aliases GET_ENTITY_TYPE
    for /VPCOE/IF_PCKF_ENTITY_DATA~GET_ENTITY_TYPE .
  aliases GET_MESSAGES
    for /VPCOE/IF_PCKF_ENTITY_DATA~GET_MESSAGES .
  aliases GET_REPORTCONFIGURATIONID
    for /VPCOE/IF_PCKF_ENTITY_DATA~GET_REPORT_CONFIG_ID .
  aliases GET_UUID
    for /VPCOE/IF_PCKF_ENTITY_DATA~GET_UUID .
  aliases IS_FAILED
    for /VPCOE/IF_PCKF_ENTITY_DATA~IS_FAILED .
  aliases SET_FAILED
    for /VPCOE/IF_PCKF_ENTITY_DATA~SET_FAILED .
  aliases SET_MESSAGES
    for /VPCOE/IF_PCKF_ENTITY_DATA~SET_MESSAGES .
  aliases SET_UUID
    for /VPCOE/IF_PCKF_ENTITY_DATA~SET_UUID .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_PCKF_ENT_PCKG_FEE
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_DATA) type /VPCOE/S_PCKF_ENT_PCKG_FEE .
  methods GET_PRODUCTID
    returning
      value(RV_RESULT) type MATNR .
  methods GET_VALIDFROM
    returning
      value(RV_RESULT) type DATUV .
  methods GET_VALIDTO
    returning
      value(RV_RESULT) type DATUB .
  methods GET_REPORTCATEGORYCOUNTRY
    returning
      value(RV_RESULT) type CHAR40 .
  methods GET_REPORTCATEGORYREGION
    returning
      value(RV_RESULT) type CHAR40 .
  methods GET_REFERENCEQUANTITY
    returning
      value(RV_RESULT) type KPEIN .
  methods GET_BASEUNITOFMEASURE
    returning
      value(RV_RESULT) type MEINS .
  methods GET_TOTALFEE
    returning
      value(RV_RESULT) type KBETR .
  methods GET_CURRENCY
    returning
      value(RV_RESULT) type KONWA .
  methods GET_LASTCHANGEDDATE
    returning
      value(RV_RESULT) type TIMESTAMPL .
  methods GET_ORGANIZATIONDATA
    returning
      value(RV_RESULT) type /VPCOE/T_PCKF_ENT_ORG_DATA .
  methods GET_SHIPTOPARTYROLE
    returning
      value(RV_RESULT) type /VPCOE/T_PCKF_ENT_SHIP_PARROLE .
  methods GET_REPORTCATEGORYID
    returning
      value(RV_RESULT) type /VPCOE/PCKF_REPORT_CATEG_ID .
  methods GET_EXTENSION
    returning
      value(RV_RESULT) type /VPCOE/S_PCKF_ENT_PCKG_FEE_EXT .
  methods GET_BUSINESS_PROCESS_DIRECTION
    returning
      value(RV_RESULT) type /VPCOE/PCKF_PROC_DRCTN .
  methods IS_FALLBACK_FEE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods GET_SUPPLIERS
    returning
      value(RV_RESULT) type /VPCOE/T_PCKF_ENT_SUPLR .
  methods GET_CONAI_EXTENSION
    returning
      value(RV_RESULT) type /VPCOE/S_PCKF_CONAI_PC_FEE_EXT .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_uuid TYPE /vpcoe/pckf_entity_uuid.
    DATA ms_data TYPE /vpcoe/s_pckf_ent_pckg_fee .
    DATA mv_deleted TYPE abap_bool .
    DATA mv_failed TYPE abap_bool.
    DATA mt_messages TYPE tab_bdcmsgcoll.

    DATA mt_business_partners TYPE /vpcoe/t_pckf_entity_data.

ENDCLASS.



CLASS /VPCOE/CL_PCKF_ENT_PCKG_FEE IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_entity_data~entity_to_json.

    "get mapper
    DATA(lr_entity_mapper) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_mapper( /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).

    DATA(lt_entity_data) = VALUE /vpcoe/t_pckf_entity_data( ).
    APPEND me TO lt_entity_data.

    rv_json = lr_entity_mapper->prepare_payload( it_entity_data = lt_entity_data ).

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~get_entity_type.

    rv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~get_messages.

    rv_result = mt_messages.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~get_report_config_id.

    rv_report_config_id = ms_data-reportconfiguration.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~get_uuid.

    rv_uuid = mv_uuid.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~is_failed.

    rv_failed = mv_failed.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~is_marked_deleted.
    rv_mark_deleted = mv_deleted.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~json_to_entity.

    "get mapper
    DATA(lr_entity_mapper) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_mapper( /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).

    lr_entity_mapper->parse_payload(
      EXPORTING
        iv_payload = iv_json
      IMPORTING
        et_entity_data = et_entity_data
        ev_next_link = ev_next_link
        ev_delta_link = ev_delta_link
      ).

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~set_failed.

    mv_failed = iv_failed.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~set_messages.

    mt_messages = it_messages.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~set_uuid.

    mv_uuid = iv_uuid.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~to_console.
  ENDMETHOD.


  METHOD constructor.
    ms_data = is_data.
    mv_deleted = iv_deleted.

    TRY.
        mv_uuid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lr_exp_uuid).
    ENDTRY.

  ENDMETHOD.


  METHOD       get_baseunitofmeasure.
    rv_result = ms_data-baseunitofmeasure.
  ENDMETHOD.


  METHOD get_business_process_direction.

    rv_result = ms_data-business_process_direction.

  ENDMETHOD.


  method GET_CONAI_EXTENSION.
    rv_result = ms_data-conaipackagingfeeextension.
  endmethod.


  METHOD       get_currency.
    rv_result = ms_data-currency.
  ENDMETHOD.


  METHOD get_data.

    rs_data = ms_data.

  ENDMETHOD.


  METHOD get_extension.
    rv_result = ms_data-packagingfeeextension.
  ENDMETHOD.


  METHOD       get_lastchangeddate.
    rv_result = ms_data-lastchangeddate.
  ENDMETHOD.


  METHOD       get_organizationdata.
    rv_result = ms_data-applicableorganizationdata.
  ENDMETHOD.


  METHOD get_productid.
    rv_result = ms_data-product.
  ENDMETHOD.


  METHOD       get_referencequantity.
    rv_result = ms_data-referencequantity.
  ENDMETHOD.


  METHOD       get_reportcategorycountry.
    rv_result = ms_data-reportcategorycountry.
  ENDMETHOD.


  METHOD       get_reportcategoryid.
    rv_result = ms_data-reportcategory.
  ENDMETHOD.


  METHOD       get_reportcategoryregion.
    rv_result = ms_data-reportcategoryregion.
  ENDMETHOD.


  METHOD       get_shiptopartyrole.
    rv_result = ms_data-shiptopartyrole.
  ENDMETHOD.


  METHOD get_suppliers.
    rv_result = ms_data-suppliers.
  ENDMETHOD.


  METHOD       get_totalfee.
    rv_result = ms_data-totalfee.
  ENDMETHOD.


  METHOD       get_validfrom.
    rv_result = ms_data-validfrom.
  ENDMETHOD.


  METHOD get_validto.
    rv_result = ms_data-validto.
  ENDMETHOD.


  METHOD is_fallback_fee.
    rv_result = ms_data-isfallbackfee.
  ENDMETHOD.
ENDCLASS.
