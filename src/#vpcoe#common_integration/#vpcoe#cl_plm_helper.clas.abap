class /VPCOE/CL_PLM_HELPER definition
  public
  inheriting from /VPCOE/CL_COMMON_HELPER
  final
  create public .

public section.

  methods SET_PLM_GENERIC_RFC_NAME
    importing
      !IV_GEN_RFC_NAME type RFCDOC_D .
  methods COMPARE_PASS_PREFIXES
    importing
      !IV_RFC_DES type /VPCOE/UPH_RFCDEST
      !IO_LOGGER type ref to /VPCOE/IF_UPH_LOGGER
    returning
      value(RV_COINCIDENCE) type ABAP_BOOL .

  methods GET_GENERIC_RFC_NAME
    redefinition .
protected section.
private section.

  data MV_PLM_GEN_RFC_NAME type RFCDOC_D value 'VPCOE_RDP_PLM' ##NO_TEXT.
ENDCLASS.



CLASS /VPCOE/CL_PLM_HELPER IMPLEMENTATION.


  METHOD compare_pass_prefixes.

    DATA: lt_bapiret2    TYPE bapiret2_t,
          lv_path_prefix TYPE string,
          lv_api_version TYPE i.

    rv_coincidence = abap_false.

    DATA(lv_service_url) = get_service_url( IMPORTING et_bapiret2 = lt_bapiret2 ).

    IF line_exists( lt_bapiret2[ type = 'E' ] ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = iv_rfc_des
        authority_check         = abap_false
      IMPORTING
        path_prefix             = lv_path_prefix
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.

    IF sy-subrc = 0.
      IF lv_path_prefix = lv_service_url.
        rv_coincidence = abap_true.
      ELSE.
        MESSAGE i123(/vpcoe/common) WITH '/VPCOE/RDP_SRV' iv_rfc_des INTO DATA(lv_message).
        io_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 ) ) ).
        MESSAGE i124(/vpcoe/common) WITH iv_rfc_des INTO lv_message.
        io_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 ) ) ).
        io_logger->commit_application_log( iv_test_mode = abap_false iv_flg_commit = abap_false ).
      ENDIF.
    ELSE.
      RETURN.
    ENDIF.

  ENDMETHOD.


METHOD get_generic_rfc_name.

  rv_gen_rfc_name = me->mv_plm_gen_rfc_name.

ENDMETHOD.


  METHOD set_plm_generic_rfc_name.

    me->mv_plm_gen_rfc_name = iv_gen_rfc_name.

  ENDMETHOD.
ENDCLASS.
