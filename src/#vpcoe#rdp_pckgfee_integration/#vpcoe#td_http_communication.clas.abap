class /VPCOE/TD_HTTP_COMMUNICATION definition
  public
  inheriting from /vpcoe/cl_plm_http
  final
  create public
  for testing .

public section.

  methods CONSTRUCTOR
    raising
      CX_OA2C .
  methods SET_RESULT
    importing
      !IV_STATUS type I
      !IV_PAYLOAD type STRING .
  methods GET_LAST_URI_SUFFIX
    returning
      value(RV_RESULT) type STRING .

  methods CLOSE
    redefinition .
  methods SEND
    redefinition .
protected section.
  PRIVATE SECTION.

    DATA:
      mv_result_status  TYPE i,
      mv_result_payload TYPE string,
      mv_last_uri_suffix type string.

ENDCLASS.



CLASS /VPCOE/TD_HTTP_COMMUNICATION IMPLEMENTATION.


  METHOD close.
    CLEAR: mv_result_status, mv_result_payload.
  ENDMETHOD.


  METHOD constructor.

    super->constructor(
      EXPORTING
        iv_api_type = 'RDP'
        iv_url      = 'http://test'
    ).

  ENDMETHOD.


  METHOD get_last_uri_suffix.
    rv_result = mv_last_uri_suffix.
  ENDMETHOD.


  METHOD send.
    mv_last_uri_suffix = iv_uri_suffix.
    ev_status = mv_result_status.
    ev_orig_response = mv_result_payload.
  ENDMETHOD.


  METHOD set_result.
    mv_result_status = iv_status.
    mv_result_payload = iv_payload.
  ENDMETHOD.
ENDCLASS.
