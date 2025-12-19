CLASS /vpcoe/cl_rdp_billdoc_data_ut DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    DATA: f_cut   TYPE REF TO /vpcoe/cl_rdp_billdocit_data,
          lo_cust TYPE REF TO /vpcoe/cl_rdp_helper.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: process_billing_doc_items FOR TESTING.
    METHODS: build_json FOR TESTING.

ENDCLASS.


CLASS /vpcoe/cl_rdp_billdoc_data_ut IMPLEMENTATION.

  DEFINE add_billdocit_expected.
    APPEND VALUE #( BILLING_DOCUMENT      = &1
                    BILLING_DOCUMENT_ITEM = &2
                    BILLING_DOCUMENT_DATE = &3
                    REFERENCESDDOC        = &4
                    REFERENCESDDOCITEM    = &5
                    REFERENCESDDOCCAT     = &6 ) TO lt_billdocit_exp.
  END-OF-DEFINITION.

  METHOD class_setup.

  ENDMETHOD.


  METHOD class_teardown.

  ENDMETHOD.


  METHOD setup.

    lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_common_helper=>sc_api_type-rdp
                                        iv_srv_grp  = /vpcoe/cl_common_helper=>sc_grp_id-billdocit
                                        iv_srv_id   = /vpcoe/cl_common_helper=>sc_service_id-billdocit ).

    f_cut = NEW /vpcoe/cl_rdp_billdocit_data( iv_mode = /vpcoe/cl_common_helper=>sc_mode-document ).

  ENDMETHOD.


  METHOD teardown.

  ENDMETHOD.

  METHOD process_billing_doc_items.

    DATA: lt_billdocit_act   TYPE /vpcoe/t_billdi_data,
          lt_billdocit_exp   TYPE /vpcoe/t_billdi_data,
          ls_sel_opt         TYPE /vpcoe/s_selopt_billdi,
          lt_billdi          TYPE STANDARD TABLE OF /vpcoe/billdi,
          lv_count           TYPE i,
          lt_session_id      TYPE /vpcoe/cl_rdp_payload_handler=>gty_r_sesion_id,
          lt_total_count_act TYPE /vpcoe/tt_log_sum,
          lt_total_count_exp TYPE /vpcoe/tt_log_sum.

    lt_billdi = VALUE #( ( session_id                 = /vpcoe/cl_common_helper=>generate_session_id( )
                          mat_doc                    = '0080000186'
                          added_on                   = '20250501'
                          delivery_document_item     = '900001'
                          delivery_document          = '0000000083'
                          reference_sd_document_item = '000010'
                          ship_to_country            = 'US'
                          ship_from_country          = 'DE' )
                        ( session_id                 = /vpcoe/cl_common_helper=>generate_session_id( )
                          mat_doc                    = '0080000186'
                          added_on                   = '20250501'
                          delivery_document_item     = '900002'
                          delivery_document          = '0000000083'
                          reference_sd_document_item = '000010'
                          ship_to_country            = 'US'
                          ship_from_country          = 'DE' )
                        ( session_id                 = /vpcoe/cl_common_helper=>generate_session_id( )
                          mat_doc                    = '0080000186'
                          added_on                   = '20250501'
                          delivery_document_item     = '000010'
                          delivery_document          = '0000000083'
                          reference_sd_document_item = '000010'
                          ship_to_country            = 'US'
                          ship_from_country          = 'DE' )
                        ( session_id                 = /vpcoe/cl_common_helper=>generate_session_id( )
                          mat_doc                    = '0080000187'
                          added_on                   = '20250501'
                          delivery_document_item     = '000010'
                          delivery_document          = '0000000084'
                          reference_sd_document_item = '000010'
                          ship_to_country            = 'US'
                          ship_from_country          = 'DE' )
                       ).

    MODIFY /vpcoe/billdi FROM TABLE lt_billdi.
    CALL FUNCTION 'DB_COMMIT'.

    LOOP AT lt_billdi ASSIGNING FIELD-SYMBOL(<ls_billdi>).
      INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_billdi>-session_id ) INTO TABLE ls_sel_opt-billdoc_session_id.
    ENDLOOP.

    INSERT VALUE #( sign = 'I' option = 'BT' low = '0090000060' high = '0090000062' ) INTO TABLE ls_sel_opt-billdoc.
    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'DE' ) INTO TABLE ls_sel_opt-billdi_country.
    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'J' ) INTO TABLE ls_sel_opt-bsddoccat.

    lt_total_count_exp = VALUE #( ( total = 5 total_failed = 0 sub_object = 'BILLDOCIT' ) ).

    add_billdocit_expected `0090000060` `000010` `20230607` `0080000187` `000010` `J`.
    add_billdocit_expected `0090000061` `000010` `20230607` `0080000187` `000010` `J`.
    add_billdocit_expected `0090000062` `000010` `20230607` `0080000186` `000010` `J`.
    add_billdocit_expected `0090000062` `000011` `20230607` `0080000186` `900001` `J`.
    add_billdocit_expected `0090000062` `000012` `20230607` `0080000186` `900002` `J`.


    f_cut->process_billing_doc_items(
      EXPORTING
        is_sel_opt    = ls_sel_opt
        io_log        = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-billdocit )
*        iv_send       = abap_true
        iv_service_id = /vpcoe/cl_common_helper=>sc_service_id-delivery
     IMPORTING
       et_billdi = lt_billdocit_act
     CHANGING
       ct_total_count_billdoc = lt_total_count_act ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_billdocit_act
      exp   = lt_billdocit_exp ).

*    cl_abap_unit_assert=>assert_equals(
*     act   = lt_total_count_act
*     exp   = lt_total_count_exp ).

  ENDMETHOD.


  METHOD build_json.

    DATA: lt_json_exp      TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_json_act      TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_billdocit_exp TYPE /vpcoe/t_billdi_data,
          lo_log           TYPE REF TO /vpcoe/cl_rdp_log.

    add_billdocit_expected `0090000060` `000010` `20230607` `0080000187` `000010` `J`.
    add_billdocit_expected `0090000061` `000010` `20230607` `0080000187` `000010` `J`.
    add_billdocit_expected `0090000062` `000010` `20230607` `0080000186` `000010` `J`.

    lt_json_exp = VALUE #( ( elements = `{"source":"ECC","elements":[{"billingDocument":"0090000060","billingDocumentItem":10,` &&
                                        `"billingDocumentDate":"2023-06-07","referenceSDDocument":"0080000187","referenceSDDocumentItem":10,` &&
                                        `"referenceSDDocumentCategory":"J"},{"billingDocument":"0090000061","billingDocumentItem":10,` &&
                                        `"billingDocumentDate":"2023-06-07","referenceSDDocument":"0080000187","referenceSDDocumentItem":10,` &&
                                        `"referenceSDDocumentCategory":"J"},{"billingDocument":"0090000062","billingDocumentItem":10,"billingDocumentDate":"2023-06-07",` &&
                                        `"referenceSDDocument":"0080000186","referenceSDDocumentItem":10,"referenceSDDocumentCategory":"J"}]}`
                             count = 3 ) ).
    f_cut->build_json(
      EXPORTING
        it_billdi = lt_billdocit_exp
        iv_level  = /vpcoe/cl_common_helper=>sc_level-dlvr_billdoc
        io_log    = lo_log
      IMPORTING
        et_json   = lt_json_act ).

    cl_abap_unit_assert=>assert_equals(
    act   = lt_json_act
    exp   = lt_json_exp ).
  ENDMETHOD.

ENDCLASS.
