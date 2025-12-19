class /VPCOE/CL_PCKF_ENT_CSTM_ROLE definition
  public
  create public .

public section.

  interfaces /VPCOE/IF_PCKF_ENTITY_DATA .

  methods CONSTRUCTOR
    importing
      !IS_DATA type /VPCOE/S_PCKF_CUSTOM_ROLE
      !IV_DELETED type ABAP_BOOL default ABAP_FALSE .
  methods GET_DATA
    returning
      value(RS_DATA) type /VPCOE/S_PCKF_CUSTOM_ROLE .
  methods GET_CUSTOMER
    returning
      value(RV_RESULT) type STRING .
  methods GET_ROLE
    returning
      value(RV_RESULT) type STRING .
  PROTECTED SECTION.
private section.

  data MV_UUID type /VPCOE/PCKF_ENTITY_UUID .
  data MS_DATA type /VPCOE/S_PCKF_CUSTOM_ROLE .
  data MV_DELETED type ABAP_BOOL .
  data MV_FAILED type ABAP_BOOL .
  data MT_MESSAGES type TAB_BDCMSGCOLL .
  data MT_BUSINESS_PARTNERS type /VPCOE/T_PCKF_ENTITY_DATA .
ENDCLASS.



CLASS /VPCOE/CL_PCKF_ENT_CSTM_ROLE IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_entity_data~entity_to_json.
    "get mapper
    DATA(lr_entity_mapper) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_mapper( /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role ).

    DATA(lt_entity_data) = VALUE /vpcoe/t_pckf_entity_data( ).
    APPEND me TO lt_entity_data.

    rv_json = lr_entity_mapper->prepare_payload( it_entity_data = lt_entity_data ).
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~get_entity_type.
    rv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role.
  ENDMETHOD.


  method /VPCOE/IF_PCKF_ENTITY_DATA~GET_MESSAGES.
    rv_result = mt_messages.
  endmethod.


  method /VPCOE/IF_PCKF_ENTITY_DATA~GET_REPORT_CONFIG_ID.
  endmethod.


  METHOD /vpcoe/if_pckf_entity_data~get_uuid.
    rv_uuid = mv_uuid.
  ENDMETHOD.


  method /VPCOE/IF_PCKF_ENTITY_DATA~IS_FAILED.
    rv_failed = mv_failed.
  endmethod.


  METHOD /vpcoe/if_pckf_entity_data~is_marked_deleted.
    rv_mark_deleted = mv_deleted.
  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_data~json_to_entity.
    DATA(lr_entity_mapper) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_mapper( /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role ).

    lr_entity_mapper->parse_payload(
      EXPORTING
        iv_payload = iv_json
      IMPORTING
        et_entity_data = et_entity_data
        ev_next_link = ev_next_link
        ev_delta_link = ev_delta_link
      ).
  ENDMETHOD.


  method /VPCOE/IF_PCKF_ENTITY_DATA~SET_FAILED.
    mv_failed = iv_failed.
  endmethod.


  method /VPCOE/IF_PCKF_ENTITY_DATA~SET_MESSAGES.
    mt_messages = it_messages.
  endmethod.


  METHOD /vpcoe/if_pckf_entity_data~set_uuid.
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


  METHOD GET_CUSTOMER.
    rv_result = ms_data-customer.
  ENDMETHOD.


  METHOD GET_DATA.

    rs_data = ms_data.

  ENDMETHOD.


  METHOD       GET_ROLE.
    rv_result = ms_data-role.
  ENDMETHOD.
ENDCLASS.
