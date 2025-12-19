class /VPCOE/CL_PCKF_ENT_COMP_CODE definition
  public
  create public .

public section.

  interfaces /VPCOE/IF_PCKF_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_PCKF_COMP_CODE
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_DATA) type /VPCOE/S_PCKF_COMP_CODE .
  methods GET_COMPANY_CODE
    returning
      value(RV_RESULT) type STRING .
  methods GET_REPORT_CATEGORY
    returning
      value(RV_RESULT) type STRING .
  methods GET_REPORT_FRACTION
    returning
      value(RV_RESULT) type STRING .
  methods GET_EXEMPTION_PRECENT
    returning
      value(RV_RESULT) type STRING .
  methods GET_VALID_FROM
    returning
      value(RV_RESULT) type STRING .
  methods GET_VALID_TO
    returning
      value(RV_RESULT) type STRING .
  PROTECTED SECTION.
private section.

  data MV_UUID type /VPCOE/PCKF_ENTITY_UUID .
  data MS_DATA type /VPCOE/S_PCKF_COMP_CODE .
  data MV_DELETED type ABAP_BOOL .
  data MV_FAILED type ABAP_BOOL .
  data MT_MESSAGES type TAB_BDCMSGCOLL .
  data MT_BUSINESS_PARTNERS type /VPCOE/T_PCKF_ENTITY_DATA .
ENDCLASS.



CLASS /VPCOE/CL_PCKF_ENT_COMP_CODE IMPLEMENTATION.


  METHOD /VPCOE/IF_PCKF_ENTITY_DATA~ENTITY_TO_JSON.
    "get mapper
    DATA(lr_entity_mapper) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_mapper( /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_comp_code ).

    DATA(lt_entity_data) = VALUE /vpcoe/t_pckf_entity_data( ).
    APPEND me TO lt_entity_data.

    rv_json = lr_entity_mapper->prepare_payload( it_entity_data = lt_entity_data ).
  ENDMETHOD.


  METHOD /VPCOE/IF_PCKF_ENTITY_DATA~GET_ENTITY_TYPE.
    rv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_comp_code.
  ENDMETHOD.


  method /VPCOE/IF_PCKF_ENTITY_DATA~GET_MESSAGES.
    rv_result = mt_messages.
  endmethod.


  method /VPCOE/IF_PCKF_ENTITY_DATA~GET_REPORT_CONFIG_ID.
  endmethod.


  METHOD /VPCOE/IF_PCKF_ENTITY_DATA~GET_UUID.
    rv_uuid = mv_uuid.
  ENDMETHOD.


  method /VPCOE/IF_PCKF_ENTITY_DATA~IS_FAILED.
    rv_failed = mv_failed.
  endmethod.


  METHOD /VPCOE/IF_PCKF_ENTITY_DATA~IS_MARKED_DELETED.
    rv_mark_deleted = mv_deleted.
  ENDMETHOD.


  METHOD /VPCOE/IF_PCKF_ENTITY_DATA~JSON_TO_ENTITY.
    DATA(lr_entity_mapper) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_mapper( /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_comp_code ).

    lr_entity_mapper->parse_payload(
      EXPORTING
        iv_payload = iv_json
      IMPORTING
        et_entity_data = et_entity_data
      ).
  ENDMETHOD.


  method /VPCOE/IF_PCKF_ENTITY_DATA~SET_FAILED.
    mv_failed = iv_failed.
  endmethod.


  method /VPCOE/IF_PCKF_ENTITY_DATA~SET_MESSAGES.
    mt_messages = it_messages.
  endmethod.


  METHOD /VPCOE/IF_PCKF_ENTITY_DATA~SET_UUID.
    mv_uuid = iv_uuid.
  ENDMETHOD.


  method /VPCOE/IF_PCKF_ENTITY_DATA~TO_CONSOLE.
  endmethod.


  METHOD CONSTRUCTOR.
    ms_data = is_data.
    mv_deleted = iv_deleted.

    TRY.
        mv_uuid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lr_exp_uuid).
    ENDTRY.

  ENDMETHOD.


  METHOD GET_COMPANY_CODE.
    rv_result = ms_data-company_code.
  ENDMETHOD.


  METHOD GET_DATA.

    rs_data = ms_data.

  ENDMETHOD.


  method GET_EXEMPTION_PRECENT.
    rv_result = ms_data-exemption_percent.
  endmethod.


  METHOD       GET_REPORT_CATEGORY.
    rv_result = ms_data-report_category.
  ENDMETHOD.


  method GET_REPORT_FRACTION.
    rv_result = ms_data-report_fraction.
  endmethod.


  method GET_VALID_FROM.
    rv_result = ms_data-valid_from.
  endmethod.


  method GET_VALID_TO.
    rv_result = ms_data-valid_to.
  endmethod.
ENDCLASS.
