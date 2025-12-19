*"* use this source file for your ABAP unit test classes
CLASS /vpcoe/tcl_uph_http_util DEFINITION DEFERRED.
CLASS /vpcoe/cl_uph_http_util DEFINITION LOCAL FRIENDS /vpcoe/tcl_uph_http_util.

CLASS /vpcoe/tcl_uph_http_util DEFINITION FOR TESTING    ##CLASS_FINAL
  DURATION SHORT
  RISK LEVEL HARMLESS
.


  PRIVATE SECTION.
    DATA:
      f_cut                  TYPE REF TO /vpcoe/cl_uph_http_util.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: close_connection FOR TESTING.
    METHODS: get_http_client FOR TESTING.
    METHODS: post_data_to_api FOR TESTING.
    METHODS: create_by_destination FOR TESTING.
    METHODS: get_oauth2_client FOR TESTING.
    METHODS: header_and_body_data_prep FOR TESTING.
    METHODS: response_from_http FOR TESTING.
    METHODS: set_oauth2_token FOR TESTING.
ENDCLASS.       "tcl_Surdp_Uph_Http_Util


CLASS /vpcoe/tcl_uph_http_util IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.

*    /vpcoe/th_uph_factory_injector=>inject_factory_double( io_double = NEW /vpcoe/td_uph_factory(  ) ).
*
*    f_cut = NEW #( ).
*
*    f_cut->/vpcoe/if_uph_http_util~get_http_client( iv_rfc_des  = 'RDP_REPL_API'
*    iv_uri_suffix = '/PackagingElements'
*    ).
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD close_connection.

*    f_cut->/vpcoe/if_uph_http_util~get_http_client( iv_rfc_des  = 'RDP_REPL_API'
*                                                   iv_uri_suffix = '/PackagingElements' ).
*    f_cut->/vpcoe/if_uph_http_util~close_connection(  ).


  ENDMETHOD.


  METHOD get_http_client.

*    DATA ro_http_client TYPE REF TO if_http_client.
*
*    ro_http_client = f_cut->/vpcoe/if_uph_http_util~get_http_client( iv_rfc_des  = 'RDP_REPL_API'
*    iv_uri_suffix = '/PackagingElements' ).
*
*    IF ro_http_client IS BOUND.
*      cl_abap_unit_assert=>assert_bound(
*        act   = ro_http_client
*      ).
*    ELSE.
*      cl_abap_unit_assert=>assert_not_bound(
*     act   = ro_http_client
*   ).
*    ENDIF.

  ENDMETHOD.


  METHOD post_data_to_api.
*    DATA iv_entity_data TYPE string.
*
*    f_cut->/vpcoe/if_uph_http_util~post_data_to_api( EXPORTING
*
*       iv_entity_data = iv_entity_data
*
*       IMPORTING ev_error_flg = DATA(lv_error_flg) ).
*
*    cl_abap_unit_assert=>assert_not_initial(
*      act   = lv_error_flg
*            ).

  ENDMETHOD.


  METHOD create_by_destination.

*    DATA iv_destination TYPE rfcdest.
*    DATA ro_http_client TYPE REF TO if_http_client.
*
*    iv_destination = 'RDP_REPL_API'.
*
*    ro_http_client = f_cut->create_by_destination( iv_destination ).
*
*    cl_abap_unit_assert=>assert_bound(
*      act   = ro_http_client
*      ).
*
*    CLEAR: ro_http_client, iv_destination .
*
*
*    ro_http_client = f_cut->create_by_destination( iv_destination ).
*
*    cl_abap_unit_assert=>assert_not_bound(
*      act   = ro_http_client
*      ).
  ENDMETHOD.


  METHOD get_oauth2_client.

*    f_cut->/vpcoe/if_uph_http_util~get_http_client( iv_rfc_des  = 'RDP_REPL_API'
*    iv_uri_suffix = '/PackagingElements' ).
*
*    DATA ro_oauth2_client TYPE REF TO if_oauth2_client.
*    ro_oauth2_client = f_cut->get_oauth2_client( iv_rfcdest = 'RDP_REPL_API' ).
*
*    IF ro_oauth2_client IS BOUND.
*      cl_abap_unit_assert=>assert_bound(
*        act   = ro_oauth2_client
*      ).
*    ELSE.
*
*      cl_abap_unit_assert=>assert_not_bound(
*     act   = ro_oauth2_client
*   ).
*    ENDIF.

  ENDMETHOD.


  METHOD header_and_body_data_prep.

*    DATA iv_entity_json_data TYPE string.
*
*    f_cut->set_body_data_send_and_receive(
*        iv_entity_json_data = iv_entity_json_data ).


  ENDMETHOD.


  METHOD response_from_http.

*    f_cut->/vpcoe/if_uph_http_util~get_http_client( iv_rfc_des  = 'RDP_REPL_API' iv_uri_suffix = '/PackagingElements' ).
*
*    f_cut->response_from_http(  ).

  ENDMETHOD.


  METHOD set_oauth2_token.
*    f_cut->/vpcoe/if_uph_http_util~get_http_client( iv_rfc_des  = 'RDP_REPL_API' iv_uri_suffix = '/PackagingElements'  ).
*
*    f_cut->set_oauth2_token( iv_rfc_des = 'RDP_REPL_API'  ).

  ENDMETHOD.

ENDCLASS.
