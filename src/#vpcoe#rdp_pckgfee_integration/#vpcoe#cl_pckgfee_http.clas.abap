class /VPCOE/CL_PCKGFEE_HTTP definition
  public
  inheriting from /VPCOE/CL_HTTP_COMMUNICATION
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_API_TYPE type /VPCOE/DE_API_TYPE optional
      !IV_RFC_NAME type RFCDOC_D optional
      !IV_URL type STRING optional
      !IV_PROXY_HOST type RFCDISPLAY-RFCGWHOST optional
      !IV_PROXY_SERVICE type RFCDISPLAY-RFCGWSERV optional
      !IV_USER_AGENT type STRING optional
      !IO_LOGGER type ref to /VPCOE/IF_PCKF_LOGGER optional
    raising
      CX_OA2C .
  PROTECTED SECTION.
private section.

  data MO_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  data MV_TOKEN type STRING .
  data MV_USER_AGENT type STRING .
ENDCLASS.



CLASS /VPCOE/CL_PCKGFEE_HTTP IMPLEMENTATION.


METHOD constructor.
  DATA: lv_server        TYPE rfcdisplay-rfchost,
        lv_proxy_host    TYPE rfcdisplay-rfcgwhost,
        lv_proxy_service TYPE rfcdisplay-rfcgwserv,
        lv_url           TYPE string.

  CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
    EXPORTING
      destination             = CONV rfcdes-rfcdest( iv_rfc_name )
      authority_check         = abap_false
      bypass_buf              = abap_false
    IMPORTING
      server                  = lv_server
      proxy_host              = lv_proxy_host
      proxy_service           = lv_proxy_service
    EXCEPTIONS
      authority_not_available = 1
      destination_not_exist   = 2
      information_failure     = 3
      internal_failure        = 4
      no_http_destination     = 5
      OTHERS                  = 6.

  IF sy-subrc <> 0.
    IF io_logger IS NOT INITIAL.
      io_logger->add_messages( it_messages = VALUE #( ( msgty = sy-msgty msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).
    ENDIF.
  ENDIF.

  IF iv_url IS NOT INITIAL.
    lv_url = |https://{ lv_server }{ iv_url }|.
  ENDIF.

  CALL METHOD super->constructor
    EXPORTING
      iv_rfc_name      = iv_rfc_name
      iv_url           = lv_url
      iv_proxy_host    = lv_proxy_host
      iv_proxy_service = lv_proxy_service.

ENDMETHOD.
ENDCLASS.
