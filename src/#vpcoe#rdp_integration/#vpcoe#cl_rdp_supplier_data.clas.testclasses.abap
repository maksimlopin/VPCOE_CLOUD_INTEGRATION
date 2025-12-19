*"* use this source file for your ABAP unit test classes
CLASS /vpcoe/cl_supplier_ut DEFINITION DEFERRED.
CLASS /vpcoe/cl_rdp_supplier_data DEFINITION LOCAL FRIENDS /vpcoe/cl_supplier_ut.

CLASS /vpcoe/cl_supplier_ut DEFINITION FOR TESTING FINAL
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      f_cut           TYPE REF TO /vpcoe/cl_rdp_supplier_data,
      mt_supplier_exp TYPE /vpcoe/cl_rdp_supplier_data=>gty_t_supplier,
      ms_sel_opt      TYPE /vpcoe/s_selopt_supplier,
      mt_json_exp     TYPE /vpcoe/cl_rdp_http=>gty_t_json.

    CLASS-METHODS: class_setup.
    METHODS: setup.
    METHODS: get_supplier FOR TESTING.
    METHODS: get_supplier_delta FOR TESTING.
    METHODS: get_supplier_ext FOR TESTING.
    METHODS: get_tax_number FOR TESTING.
ENDCLASS.       "/vpcoe/cl_Supplier_Ut


CLASS /vpcoe/cl_supplier_ut IMPLEMENTATION.

  METHOD class_setup.

  ENDMETHOD.

  METHOD setup.

    CREATE OBJECT f_cut
      EXPORTING
        iv_api_type     = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
        iv_package_size = 10
        iv_source       = /vpcoe/cl_rdp_helper=>get_source_id( )
        iv_mode         = '1'
        io_log          = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-supplier ).

    INSERT VALUE #( id                            = '0000100000'
                    name                          = 'Krystsina Kastsiukevich'
                    country	                      = 'FR'
                    region                        = ''
                    is_marked_for_deletion        = '-'
*                    is_business_purpose_completed  = '-'
*                    tax_numbers                   = VALUE #( ( tax_number       = 'DE1234556'
*                                                               tax_number_type = 'DE1' ) )
                  ) INTO TABLE me->mt_supplier_exp.

    me->ms_sel_opt = VALUE #( supplier_id = VALUE #( ( sign = 'I' option = 'EQ' low = '0000100000' ) ) ).

    INSERT VALUE #( elements = '{"source":"ECC","elements":[{"id":"0000100000","name":"Krystsina Kastsiukevich","country":"FR","region": null,'
                            && '"isMarkedForDeletion":false,"isBusinessPurposeCompleted":false,"nodel":false,"taxNumbers":[]}]}'
                    count = 1
                  ) INTO TABLE me->mt_json_exp.

  ENDMETHOD.

  METHOD get_supplier.
    DATA: lt_supplier	TYPE /vpcoe/cl_rdp_supplier_data=>gty_t_supplier_jsn,
          lt_json	    TYPE /vpcoe/cl_rdp_http=>gty_t_json.

    me->f_cut->get_supplier(
      EXPORTING
        is_sel_opt  = me->ms_sel_opt
      IMPORTING
        et_supplier = lt_supplier
        et_json     = lt_json ).

    IF lt_json NE me->mt_json_exp.
      DATA(lv_msg) = /vpcoe/cl_rdp_aunit_helper=>get_json_differences(
        it_json_act = lt_json
        it_json_exp = me->mt_json_exp
        ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_supplier
      exp   = me->mt_supplier_exp
      msg   = 'Testing Supplier Retrieval' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_json
      exp   = me->mt_json_exp
      msg   = /vpcoe/cl_rdp_aunit_helper=>get_error_message(
        iv_test_type = /vpcoe/cl_rdp_aunit_helper=>gc_json
        iv_msg = lv_msg )
       ).

  ENDMETHOD.


  METHOD get_supplier_delta.
    RETURN.
  ENDMETHOD.


  METHOD get_supplier_ext.
    RETURN.
  ENDMETHOD.


  METHOD get_tax_number.
    DATA: lt_supplier       TYPE /vpcoe/cl_rdp_supplier_data=>gty_t_supplier,
          lt_tax_number_exp TYPE /vpcoe/cl_rdp_supplier_data=>gty_t_tax_number.

    lt_supplier = me->mt_supplier_exp.

    f_cut->get_tax_number(
      EXPORTING
        is_sel_opt    = me->ms_sel_opt
      IMPORTING
        et_tax_number = DATA(lt_tax_number)
      CHANGING
        ct_supplier   = lt_supplier ).

*    INSERT VALUE #( ven_id          = '0000100000'
*                    tax_number_xl   = ''
*                    tax_number      = 'DE1234556'
*                    tax_number_type = 'DE1' ) INTO TABLE lt_tax_number_exp.

    cl_abap_unit_assert=>assert_equals(
      act   = lt_tax_number
      exp   = lt_tax_number_exp
      msg   = 'Testing Tax Number Retrieval' ).

  ENDMETHOD.

ENDCLASS.
