class /VPCOE/CL_RDP_BILLDOCIT_DATASM definition
  public
  inheriting from /VPCOE/CL_RDP_BILLDOCIT_DATA
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND .

  methods BUILD_JSON
    redefinition .
  methods PROCESS_BILLING_DOC_ITEMS
    redefinition .
  methods CLOSE_REPLICATION
    redefinition .
protected section.
private section.

  data MO_RDP_HELPER type ref to /VPCOE/CL_RDP_HELPER .
  data MO_SUMA_HELPER type ref to /VPCOE/CL_RDP_SUMA_HELPER .
ENDCLASS.



CLASS /VPCOE/CL_RDP_BILLDOCIT_DATASM IMPLEMENTATION.


METHOD build_json.
  DATA: ls_source_data TYPE /vpcoe/str_batch_json_sm.

  super->build_json(
    EXPORTING
      it_billdi = it_billdi
      iv_level  = iv_level
      io_log    = io_log
    IMPORTING
      et_json   = et_json ).

  DATA(lv_rund_id) = mo_suma_helper->start_replication( EXPORTING io_log = io_log ).

  DATA(lv_rplc_tag) = 'source:"' &&  /vpcoe/cl_rdp_helper=>get_source_id( ) && '"'.

  LOOP AT et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
    REPLACE lv_rplc_tag IN <ls_json>-elements WITH 'replicationRunId:"' && lv_rund_id && '"'.
  ENDLOOP.

ENDMETHOD.


  METHOD close_replication.

    " Close Replication
    IF io_log IS BOUND AND io_log->check( ).
      me->mo_suma_helper->cancel_replication( io_log = io_log ).
    ELSE.
      me->mo_suma_helper->finish_replication( io_log = io_log ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( iv_mode ).

    me->mo_rdp_helper = NEW /vpcoe/cl_rdp_helper( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                  iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-billdocit
                                                  iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-billdocit ).


    me->mo_suma_helper = /vpcoe/cl_rdp_suma_helper=>get_instance( io_rdp_helper = me->mo_rdp_helper
                                                                  iv_srv_prfx   = 'billdoc' ).

  ENDMETHOD.


  METHOD process_billing_doc_items.
    CALL METHOD super->process_billing_doc_items(
      EXPORTING
        iv_service_id          = iv_service_id
        iv_save_background     = iv_save_background
        iv_file_path           = iv_file_path
        iv_code                = iv_code
        is_sel_opt             = is_sel_opt
        io_log                 = io_log
        iv_send                = iv_send
      IMPORTING
        et_billdi              = et_billdi
      CHANGING
        ct_total_count_billdoc = ct_total_count_billdoc ).

    IF me->mv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send AND iv_send = abap_true.
      me->close_replication( io_log ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
