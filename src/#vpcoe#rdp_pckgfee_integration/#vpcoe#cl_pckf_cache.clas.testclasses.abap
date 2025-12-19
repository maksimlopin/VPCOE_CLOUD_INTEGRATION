CLASS ltcl_pckf_cache DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: mt_entity_data TYPE /vpcoe/t_pckf_entity_data,
          mo_cut         TYPE REF TO /vpcoe/cl_pckf_cache.  "class under test

    METHODS:
      setup,
      teardown,
      set_entities FOR TESTING,
      get_entities FOR TESTING,
      delete_entity FOR TESTING,
      set_failed FOR TESTING,
      get_references FOR TESTING.

ENDCLASS.

CLASS ltcl_pckf_cache IMPLEMENTATION.

  METHOD setup.

    "use cache in test mode with mocked table
    mo_cut = NEW /vpcoe/cl_pckf_cache( ).

    CLEAR mt_entity_data.

  ENDMETHOD.

  METHOD teardown.

    "ensure no changes stay persisted
    ROLLBACK WORK. "#EC CI_ROLLBACK

  ENDMETHOD.

  METHOD set_entities.

    DATA: lr_act_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee,
          lr_exp_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee.

    "Given
    APPEND INITIAL LINE TO mt_entity_data REFERENCE INTO DATA(lo_entity_data).
    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = VALUE #(
        product = 'P1'
        validfrom = '20221006'
        validto = '20221231'
        reportconfiguration = 'REP_CFG_1'
        reportcategorycountry = 'DE'
        reportcategoryregion = 'BW'
        referencequantity = '1'
        baseunitofmeasure = 'KG'
        totalfee = '1.5'
        currency = 'EUR'
        lastchangeddate = '20221006080000.0000000'
    ) ).

    APPEND INITIAL LINE TO mt_entity_data REFERENCE INTO lo_entity_data.
    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = VALUE #(
        product = 'P2'
        validfrom = '20221006'
        validto = '20221231'
        reportconfiguration = 'REP_CFG_2'
        reportcategorycountry = 'DE'
        reportcategoryregion = 'BW'
        referencequantity = '100'
        baseunitofmeasure = 'KG'
        totalfee = '5.0'
        currency = 'EUR'
        lastchangeddate = '20221006080000.0000000'
    ) ).

    TRY.
        DATA(lv_ref_prot_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lr_exp_uuid).
    ENDTRY.

    "When
    mo_cut->/vpcoe/if_pckf_cache~set_entities( it_entities = mt_entity_data iv_ref_prot_uuid = lv_ref_prot_uuid ).

    "Then
    DATA(lt_entity_data) = mo_cut->/vpcoe/if_pckf_cache~get_entities( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee iv_ref_prot_uuid = lv_ref_prot_uuid ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_entity_data ) exp = 2 ).

    "Check data and FIFO order too
    lr_act_entity_data ?= lt_entity_data[ 1 ].
    lr_exp_entity_data ?= mt_entity_data[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_act_entity_data->get_data( ) exp = lr_exp_entity_data->get_data( ) ).

    lr_act_entity_data ?= lt_entity_data[ 2 ].
    lr_exp_entity_data ?= mt_entity_data[ 2 ].
    cl_abap_unit_assert=>assert_equals( act = lr_act_entity_data->get_data( ) exp = lr_exp_entity_data->get_data( ) ).

  ENDMETHOD.

  METHOD delete_entity.

    DATA: lr_act_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee,
          lr_exp_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee.

    "Given
    APPEND INITIAL LINE TO mt_entity_data REFERENCE INTO DATA(lo_entity_data).
    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = VALUE #(
        product = 'P1'
        validfrom = '20221006'
        validto = '20221231'
        reportconfiguration = 'REP_CFG_1'
        reportcategorycountry = 'DE'
        reportcategoryregion = 'BW'
        referencequantity = '1'
        baseunitofmeasure = 'KG'
        totalfee = '1.5'
        currency = 'EUR'
        lastchangeddate = '20221006080000.0000000'
    ) ).

    TRY.
        DATA(lv_ref_prot_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lr_exp_uuid).
    ENDTRY.

    mo_cut->/vpcoe/if_pckf_cache~set_entities( it_entities = mt_entity_data iv_ref_prot_uuid = lv_ref_prot_uuid ).

    "When
    mo_cut->/vpcoe/if_pckf_cache~delete_entity( lo_entity_data->*->get_uuid( ) ).

    "Then
    DATA(lt_entity_data) = mo_cut->/vpcoe/if_pckf_cache~get_entities( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee iv_ref_prot_uuid = lv_ref_prot_uuid ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_entity_data ) exp = 0 ).

  ENDMETHOD.

  METHOD get_entities.

    DATA: lr_act_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee,
          lr_exp_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee.

    "Given
    APPEND INITIAL LINE TO mt_entity_data REFERENCE INTO DATA(lo_entity_data).
    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = VALUE #(
        product = 'P1'
        validfrom = '20221006'
        validto = '20221231'
        reportconfiguration = 'REP_CFG_1'
        reportcategorycountry = 'DE'
        reportcategoryregion = 'BW'
        referencequantity = '1'
        baseunitofmeasure = 'KG'
        totalfee = '1.5'
        currency = 'EUR'
        lastchangeddate = '20221006080000.0000000'
    ) ).

    APPEND INITIAL LINE TO mt_entity_data REFERENCE INTO lo_entity_data.
    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = VALUE #(
        product = 'P1'
        validfrom = '20221006'
        validto = '20221231'
        reportconfiguration = 'REP_CFG_2'
        reportcategorycountry = 'DE'
        reportcategoryregion = 'BW'
        referencequantity = '1'
        baseunitofmeasure = 'KG'
        totalfee = '2.5'
        currency = 'EUR'
        lastchangeddate = '20221006080000.0000000'
    ) ).

    TRY.
        DATA(lv_ref_prot_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lr_exp_uuid).
    ENDTRY.

*    APPEND INITIAL LINE TO mt_entity_data REFERENCE INTO lo_entity_data.
*    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_appl_partie( is_data = VALUE #(
*        reportconfigurationid = 'REP_CFG_2'
*    ) ).

    "When
    mo_cut->/vpcoe/if_pckf_cache~set_entities( it_entities = mt_entity_data iv_ref_prot_uuid = lv_ref_prot_uuid ).

    "Then
    DATA(lt_entity_data) = mo_cut->/vpcoe/if_pckf_cache~get_entities( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee iv_report_config_id = 'REP_CFG_1' iv_ref_prot_uuid = lv_ref_prot_uuid ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_entity_data ) exp = 1 ).

    "Check data and FIFO order too
    lr_act_entity_data ?= lt_entity_data[ 1 ].
    lr_exp_entity_data ?= mt_entity_data[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_act_entity_data->get_data( ) exp = lr_exp_entity_data->get_data( ) ).

  ENDMETHOD.

  METHOD set_failed.

    DATA: lr_act_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee,
          lr_exp_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee.

    "Given
    APPEND INITIAL LINE TO mt_entity_data REFERENCE INTO DATA(lo_entity_data).
    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = VALUE #(
        product = 'P1'
        reportconfiguration = 'REP_CFG_1'
    ) ).

    TRY.
        DATA(lv_ref_prot_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lr_exp_uuid).
    ENDTRY.

    mo_cut->/vpcoe/if_pckf_cache~set_entities( it_entities = mt_entity_data iv_ref_prot_uuid = lv_ref_prot_uuid  ).

    "When
    mo_cut->/vpcoe/if_pckf_cache~set_failed( iv_uuid = lo_entity_data->*->get_uuid( ) ).

    "Then
    DATA(lt_entity_data) = mo_cut->/vpcoe/if_pckf_cache~get_entities( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee iv_ref_prot_uuid = lv_ref_prot_uuid ).
    DATA(lr_entity) = lt_entity_data[ 1 ].

    cl_abap_unit_assert=>assert_equals( act = lines( lt_entity_data ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_entity->is_failed( ) exp = abap_true ).

  ENDMETHOD.

  METHOD get_references.

    DATA: lr_act_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee,
          lr_exp_entity_data TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee.
    DATA: lt_entity_data_1 TYPE /vpcoe/t_pckf_entity_data,
          lt_entity_data_2 TYPE /vpcoe/t_pckf_entity_data.

    "Given
    APPEND INITIAL LINE TO lt_entity_data_1 REFERENCE INTO DATA(lo_entity_data).
    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = VALUE #(
        product = 'REF1_P1'
    ) ).
    APPEND INITIAL LINE TO lt_entity_data_1 REFERENCE INTO lo_entity_data.
    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = VALUE #(
        product = 'REF1_P2'
    ) ).

    APPEND INITIAL LINE TO lt_entity_data_2 REFERENCE INTO lo_entity_data.
    lo_entity_data->* = NEW /vpcoe/cl_pckf_ent_pckg_fee( is_data = VALUE #(
        product = 'REF2_P1'
    ) ).

    TRY.
        DATA(lv_ref_prot_uuid_1) = cl_system_uuid=>create_uuid_x16_static( ).
        DATA(lv_ref_prot_uuid_2) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(lr_exp_uuid).
    ENDTRY.

    "When
    GET TIME STAMP FIELD DATA(lv_compare_timestamp).
    mo_cut->/vpcoe/if_pckf_cache~set_entities( it_entities = lt_entity_data_1 iv_ref_prot_uuid = lv_ref_prot_uuid_1 ).
    mo_cut->/vpcoe/if_pckf_cache~set_entities( it_entities = lt_entity_data_2 iv_ref_prot_uuid = lv_ref_prot_uuid_2 ).

    "Then
    DATA(lt_references) = mo_cut->/vpcoe/if_pckf_cache~get_references( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).

    "remove references which don't belong to this test
    LOOP AT lt_references INTO DATA(ls_reference) WHERE creationdatetime < lv_compare_timestamp.
      DELETE lt_references INDEX sy-tabix.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals( act = lines( lt_references ) exp = 2 ).

    "Check references and FIFO order
    cl_abap_unit_assert=>assert_equals( act = lt_references[ 1 ]-ref_prot_uuid exp = lv_ref_prot_uuid_1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_references[ 2 ]-ref_prot_uuid exp = lv_ref_prot_uuid_2 ).

  ENDMETHOD.

ENDCLASS.
