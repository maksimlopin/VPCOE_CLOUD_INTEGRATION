CLASS /vpcoe/cl_batch_data_ut DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_rdp_batch_data.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: get_batch FOR TESTING.
    METHODS: get_batch_delta FOR TESTING.


ENDCLASS.


CLASS /vpcoe/cl_batch_data_ut IMPLEMENTATION.
  DEFINE add_batch_expected.
    APPEND VALUE #( id                    = &1
                    product               = &2
                    plant                 = &3
                    manufacture_date      = &4
                    packaging_composition = &5 ) TO lt_batch_exp.
  END-OF-DEFINITION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.
    DATA(lo_cust)       = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                    iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-batch
                                                    iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-packaging ).

    f_cut = NEW /vpcoe/cl_rdp_batch_data( iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                          iv_package_size = lo_cust->get_package_size( )
                                          iv_source       = lo_cust->get_source_id( ) ).

  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD get_batch.
    DATA: lt_batch_act TYPE /vpcoe/cl_rdp_batch_data=>gty_t_batch,
          lt_json_act  TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_batch_exp TYPE /vpcoe/cl_rdp_batch_data=>gty_t_batch,
          lt_json_exp  TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          ls_sel_opt   TYPE /vpcoe/s_selopt_batch.

    INSERT VALUE #( sign = 'I' option = 'EQ' low = '0000000013' ) INTO TABLE ls_sel_opt-display_id.
    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'STBL'       ) INTO TABLE ls_sel_opt-plant.
    INSERT VALUE #( sign = 'I' option = 'EQ' low = 'RAW01'      ) INTO TABLE ls_sel_opt-material.

    add_batch_expected `0000000013` `RAW01` `STBL` `00000000` `000000000043`.
    add_batch_expected `0000000013` `RAW01` `STBL` `00000000` `000000000044`.
    add_batch_expected `0000000013` `RAW01` `STBL` `00000000` `000000000052`.
    add_batch_expected `0000000013` `RAW01` `STBL` `00000000` `000000000054`.
    add_batch_expected `0000000013` `RAW01` `STBL` `00000000` `000000000062`.
    add_batch_expected `0000000013` `RAW01` `STBL` `00000000` `000000000068`.
    add_batch_expected `0000000013` `RAW01` `STBL` `00000000` `000000000072`.

    INSERT VALUE #( elements = `{"source":"ECC",`
                                  && `"elements":[{"id":"0000000013","product":"RAW01","plant":"STBL","packagingComposition":"000000000072",`
                                  && `"manufactureDate": null},{"id":"0000000013","product":"RAW01","plant":"STBL",`
                                  && `"packagingComposition":"000000000068","manufactureDate": null},{"id":"0000000013",`
                                  && `"product":"RAW01","plant":"STBL","packagingComposition":"000000000062","manufactureDate": null},`
                                  && `{"id":"0000000013","product":"RAW01","plant":"STBL","packagingComposition":"000000000054","manufactureDate": null}`
                                  && `,{"id":"0000000013","product":"RAW01","plant":"STBL","packagingComposition":"000000000052","manufactureDate": null},`
                                  && `{"id":"0000000013","product":"RAW01","plant":"STBL","packagingComposition":"000000000044","manufactureDate": null},`
                                  && `{"id":"0000000013","product":"RAW01","plant":"STBL","packagingComposition":"000000000043","manufactureDate": null}]}`
                  count = 7 ) INTO TABLE lt_json_exp.

    f_cut->get_batch(
      EXPORTING
        is_sel_opt = ls_sel_opt
        io_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-packaging )
     IMPORTING
       et_batch = lt_batch_act
       et_json  = lt_json_act ).

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_batch_act
      exp   = lt_batch_exp
      msg   = `Testing Batch Retrieval` ).

    cl_abap_unit_assert=>assert_equals(
     act   = lt_json_act
     exp   = lt_json_exp
     msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
       iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
       iv_msg = lv_msg )
    ).

  ENDMETHOD.


  METHOD get_batch_delta.
    DATA: lt_batch_act TYPE /vpcoe/cl_rdp_batch_data=>gty_t_batch,
          lt_json_act  TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          lt_batch_exp TYPE /vpcoe/cl_rdp_batch_data=>gty_t_batch,
          lt_json_exp  TYPE /vpcoe/cl_rdp_http=>gty_t_json,
          ls_sel_opt   TYPE /vpcoe/s_selopt_batch.

    DATA(lo_cust) = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                              iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-batch
                                              iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-packaging ).

    INSERT VALUE #( sign = 'I' option = 'EQ' low = '0000000122' ) INTO TABLE ls_sel_opt-display_id.

    INSERT VALUE #( elements = `{"source":"ECC",`
                                  && `"elements":[{"id":"0000000122","product":"RAW01","plant":"DE02","packagingComposition":"000000000072","manufactureDate": null}]}`
                    count = 1 ) INTO TABLE lt_json_exp.

    add_batch_expected `0000000122` `RAW01` `DE02` `` `000000000072`.

    f_cut = NEW /vpcoe/cl_rdp_batch_data( iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                          iv_package_size = 1
                                          iv_source       = lo_cust->get_source_id( ) ).
    f_cut->get_batch_delta(
      EXPORTING
        iv_chng_pointer_id = '/VPCOE/BATCH'
        is_sel_opt         = ls_sel_opt
     IMPORTING
       et_batch            = lt_batch_act
       et_json             = DATA(lt_json) ).

    DELETE lt_batch_act WHERE id NE '0000000122'.       "#EC CI_SORTSEQ
    DELETE lt_batch_act WHERE packaging_composition NE '000000000072'. "#EC CI_SORTSEQ

    LOOP AT lt_json ASSIGNING FIELD-SYMBOL(<ls_json>) WHERE elements CS `{"source":"ECC","elements":[{"id":"0000000122","product":"RAW01","plant":"DE02","packagingComposition":"000000000072",`.
      INSERT <ls_json> INTO TABLE lt_json_act.
      EXIT.
    ENDLOOP.

    IF lt_json_act NE lt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json_act
        it_json_exp = lt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_batch_act
      exp   = lt_batch_exp
      msg   = 'Testing Batch Delta Retrieval' ).

    cl_abap_unit_assert=>assert_equals(
     act   = lt_json_act
     exp   = lt_json_exp
     msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
       iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
       iv_msg = lv_msg )
    ).

  ENDMETHOD.

ENDCLASS.
