CLASS tcl_surdp_uph_logger DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_uph_logger.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: add_messages FOR TESTING.
    METHODS: commit_application_log FOR TESTING.
    METHODS: get_app_log_object FOR TESTING.
    METHODS: get_messages FOR TESTING.
    METHODS: has_error_messages FOR TESTING.
    METHODS: has_warning_messages FOR TESTING.
    METHODS: add_bapi_messages FOR TESTING.
    METHODS: get_bapi_messages FOR TESTING.


ENDCLASS.


CLASS tcl_surdp_uph_logger IMPLEMENTATION.

  METHOD class_setup.

  ENDMETHOD.


  METHOD class_teardown.

  ENDMETHOD.


  METHOD setup.

    f_cut = NEW /vpcoe/cl_uph_logger( iv_repid = sy-repid iv_test_mode = abap_true ).

  ENDMETHOD.


  METHOD teardown.

  ENDMETHOD.


  METHOD add_messages.

  ENDMETHOD.

  METHOD get_messages.

    DATA lt_messages_act TYPE /vpcoe/T_UPH_MSG.

    f_cut->/vpcoe/if_uph_logger~get_messages(
     IMPORTING
       et_messages = lt_messages_act ).

    cl_abap_unit_assert=>assert_not_initial(
      act   = lt_messages_act ) .

  ENDMETHOD.

  METHOD commit_application_log.

    f_cut->/vpcoe/if_uph_logger~commit_application_log( iv_test_mode = abap_true ).

  ENDMETHOD.


  METHOD get_app_log_object.

    DATA lv_object_act TYPE balobj_d.
    DATA lv_subobject_act TYPE balsubobj.

    f_cut->/vpcoe/if_uph_logger~get_app_log_object(
     IMPORTING
       ev_object = lv_object_act
       ev_subobject = lv_subobject_act ).

  ENDMETHOD.

  METHOD has_error_messages.

    DATA lv_flg_has_errors TYPE abap_bool.

    lv_flg_has_errors = f_cut->/vpcoe/if_uph_logger~has_error_messages(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_flg_has_errors
      exp   = abap_false   ).

  ENDMETHOD.


  METHOD has_warning_messages.

    DATA lv_flg_has_warnings TYPE abap_bool.

    lv_flg_has_warnings = f_cut->/vpcoe/if_uph_logger~has_warning_messages(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_flg_has_warnings
      exp   = abap_false    ).

  ENDMETHOD.

  METHOD add_bapi_messages.
    DATA : lt_bapi_messages TYPE bapiret2_t.
    APPEND INITIAL LINE TO lt_bapi_messages ASSIGNING FIELD-SYMBOL(<lfs_bapi_message>).
    <lfs_bapi_message>-type = /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_i.
*    <lfs_bapi_message>-message = 'Message to test method ADD_BAPI_MESSAGES'.
    <lfs_bapi_message>-id = /vpcoe/cl_uph_exec_load_wrap=>gc_default_msgclass.
    <lfs_bapi_message>-number = '000'.
    <lfs_bapi_message>-message_v1 = 'Test class message' .

    f_cut->/vpcoe/if_uph_logger~add_bapi_messages( lt_bapi_messages ) .
  ENDMETHOD.

  METHOD get_bapi_messages.
    DATA : lt_bapi_messages_exp TYPE bapiret2_t.

    " Message updated from Constructor
    APPEND INITIAL LINE TO lt_bapi_messages_exp ASSIGNING FIELD-SYMBOL(<lfs_message>).
    <lfs_message>-id = /vpcoe/cl_uph_exec_load_wrap=>gc_default_msgclass .
    <lfs_message>-type = /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_i.
    <lfs_message>-number = '015' .
    WRITE: sy-datum TO <lfs_message>-message_v1,
           sy-uzeit TO <lfs_message>-message_v2 .

    DATA(lt_bapi_messages_act) = f_cut->/vpcoe/if_uph_logger~get_bapi_messages(  ) .

    cl_abap_unit_assert=>assert_equals(
      act   = lt_bapi_messages_act
      exp   = lt_bapi_messages_exp   ).

  ENDMETHOD.

ENDCLASS.
