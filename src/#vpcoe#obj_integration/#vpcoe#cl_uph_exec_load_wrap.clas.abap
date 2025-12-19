class /VPCOE/CL_UPH_EXEC_LOAD_WRAP definition
  public
  final
  create public .

public section.             "#EC INTF_IN_CLASS     "#EC NUM_PUBLIC_ATTR

  interfaces /VPCOE/IF_UPH_EXEC_LOAD_WRAP .

  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE value '500' ##NO_TEXT.
  constants GC_DEFAULT_MSGCLASS type SYMSGID value '/VPCOE/PLM' ##NO_TEXT.
  constants GC_MSGTY_A type SYMSGTY value 'A' ##NO_TEXT.
  constants GC_MSGTY_E type SYMSGTY value 'E' ##NO_TEXT.
  constants GC_MSGTY_I type SYMSGTY value 'I' ##NO_TEXT.
  constants GC_MSGTY_S type SYMSGTY value 'S' ##NO_TEXT.
  constants GC_MSGTY_W type SYMSGTY value 'W' ##NO_TEXT.
  constants GC_MSGTY_X type SYMSGTY value 'X' ##NO_TEXT.
  constants GC_DOMNAME_UPMODE type DOMNAME value '/VPCOE/UPLOAD_MODE' ##NO_TEXT.
  constants GC_DOMNAME_UPRESULT type DOMNAME value '/VPCOE/UPLOAD_RESULT' ##NO_TEXT.

  methods CONSTRUCTOR .
  methods EXECUTE_UPLOAD                                "#EC CI_NESTING
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IS_PARAMETERS type ANY optional
      !IV_TEST type BOOLE_D optional .
  class-methods GET_DOMAIN_FIX_VAL_TEXT
    importing
      !IV_DOMAIN_NAME type ANY
      !IV_FIXED_VALUE type ANY
    returning
      value(RV_FIX_VAL_TEXT) type DDTEXT .
  class-methods GET_F4_HELP
    importing
      !IV_RETFIELD type FIELDNAME
      !IV_DYNPROFIELD type DYNFNAM
      !IT_VALUE_TAB type STANDARD TABLE
    returning
      value(RV_RETURN_CODE) type I .
protected section.

  data MS_PROTOCOL type /VPCOE/UPH_PROT .
private section.

  methods COLLECT_ENTITIES_TO_TRANSFER
    importing
      !IS_PARAMETERS type ANY
      !IO_ENTITY_PROC type ref to /VPCOE/IF_UPH_ENTITY_PROC
    changing
      !CT_ENTITY_DATA_TRANSFER type /VPCOE/T_UPH_ENTITY_DATA
      !CT_ENTITY_DATA type /VPCOE/T_UPH_ENTITY_DATA .
  methods TRANSFER_ENTITIES_AS_PACKAGE
    importing
      !IS_PARAMETERS type ANY
      !IO_ENTITY_PROC type ref to /VPCOE/IF_UPH_ENTITY_PROC
      !IV_FINAL_TRANSFER type ABAP_BOOL
    changing
      !CT_ENTITY_DATA_TRANSFER type /VPCOE/T_UPH_ENTITY_DATA
      !CT_ENTITY_DATA type /VPCOE/T_UPH_ENTITY_DATA .
  methods PROCESS_UPLOAD
    importing
      !IV_TEST type ABAP_BOOL
      !IS_PARAMETERS type ANY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IO_ENTITY_PROC type ref to /VPCOE/IF_UPH_ENTITY_PROC
      !IO_LOGGER type ref to /VPCOE/IF_UPH_LOGGER .
  methods LOG_FINAL_STATUS
    importing
      !IV_TEST type ABAP_BOOL
      !IO_LOGGER type ref to /VPCOE/IF_UPH_LOGGER .
ENDCLASS.



CLASS /VPCOE/CL_UPH_EXEC_LOAD_WRAP IMPLEMENTATION.


  METHOD /vpcoe/if_uph_exec_load_wrap~execute_upload.

    DATA lo_logger               TYPE REF TO /vpcoe/if_uph_logger.
    DATA lo_entity_proc          TYPE REF TO /vpcoe/if_uph_entity_proc.

    " Get the logger
    lo_logger = /vpcoe/cl_uph_factory=>get_instance( )->get_logger( iv_test_mode   = iv_test
                                                                    iv_upload_mode = iv_upload_mode ).

    "get upload entity processor
    lo_entity_proc = /vpcoe/cl_uph_factory=>get_instance(  )->get_entity_processor( iv_upload_entity = iv_upload_entity
                                                                                    iv_upload_mode   = iv_upload_mode
                                                                                    is_parameters    = is_parameters ).

    IF lo_entity_proc IS NOT BOUND.
      MESSAGE e002(/vpcoe/plm) INTO DATA(lv_msg_str)  ##NEEDED.
      lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).
    ENDIF.

    IF lo_entity_proc IS BOUND.

      me->process_upload( iv_test        = iv_test
                          is_parameters  = is_parameters
                          iv_upload_mode = iv_upload_mode
                          io_entity_proc = lo_entity_proc
                          io_logger      = lo_logger ).

    ENDIF.

    " close http connection as not being used
    DATA(lo_upl_util) = /vpcoe/cl_uph_factory=>get_instance(  )->get_http_util( ).
    IF lo_upl_util IS BOUND.
      lo_upl_util->close_connection( ).
    ENDIF.

    "Log final status
    log_final_status( iv_test = iv_test io_logger = lo_logger ).

  ENDMETHOD.


  METHOD collect_entities_to_transfer.
    " The method was refactored and is kept to ease downports.
  ENDMETHOD.


  METHOD CONSTRUCTOR.

    " populate the start time stamp
    CONCATENATE sy-datum sy-uzeit INTO DATA(lv_timestamp).
    ms_protocol-start_timestamp = lv_timestamp.

  ENDMETHOD.


  METHOD execute_upload.                                "#EC CI_NESTING

    DATA:
      lo_logger               TYPE REF TO /vpcoe/if_uph_logger,
      lo_entity_proc          TYPE REF TO /vpcoe/if_uph_entity_proc,
      lo_upl_util             TYPE REF TO /vpcoe/if_uph_http_util,
      lv_upload_result        LIKE sy-msgty,
      lv_record_cnt           TYPE i,
      lv_package_size         TYPE i,
      lv_transfer_size        TYPE i,
      lv_num_packages         TYPE i,
      lv_act_package          TYPE i,
      lv_progress_perc_flt    TYPE f,
      lt_entity_data_transfer TYPE /vpcoe/t_uph_entity_data.

    " Get the logger
    lo_logger = /vpcoe/cl_uph_factory=>get_instance( )->get_logger(
          iv_test_mode   = iv_test
          iv_upload_mode = iv_upload_mode ).

    "get upload entity processor
    lo_entity_proc = /vpcoe/cl_uph_factory=>get_instance(  )->get_entity_processor(
                            iv_upload_entity = iv_upload_entity
                            iv_upload_mode = iv_upload_mode
                            is_parameters = is_parameters ).

    IF lo_entity_proc IS NOT BOUND.
      MESSAGE e047(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).
    ENDIF.

    IF lo_entity_proc IS BOUND.

      " prepare processing of upload entity
      lv_record_cnt = lo_entity_proc->prepare_process( ).

      " when no relevant data found
      IF lv_record_cnt IS NOT INITIAL.
        DATA(lo_cust) = NEW /vpcoe/cl_plm_helper( iv_api_type = /vpcoe/cl_plm_helper=>sc_api_type-plm
                                                  iv_srv_grp  = /vpcoe/cl_plm_helper=>sc_grp_id-plm
                                                  iv_srv_id   = /vpcoe/cl_plm_helper=>sc_service_id-plm ).
        "define package size for retrieval and for transfer
        lv_package_size = lo_cust->get_package_size( ).
        lv_transfer_size = 1.

        IF lv_record_cnt MOD lv_package_size = 0.
          lv_num_packages = lv_record_cnt DIV lv_package_size.
        ELSE.
          lv_num_packages = ( lv_record_cnt DIV lv_package_size ) + 1.
        ENDIF.

        "process packages of upload entity
        lv_act_package = 1 .

        CLEAR lt_entity_data_transfer.

        WHILE lv_act_package <= lv_num_packages.

          DATA(lt_entity_data) = lo_entity_proc->process_package(
              iv_act_package  = lv_act_package
              iv_package_size = lv_package_size ).

          IF lt_entity_data IS NOT INITIAL AND iv_test IS INITIAL.

            "collect entity data to transfer
            LOOP AT lt_entity_data REFERENCE INTO DATA(lr_entity_data).

              APPEND lr_entity_data->* TO lt_entity_data_transfer.

              IF lines( lt_entity_data_transfer ) >= lv_transfer_size.

                " transfer package data
                lo_entity_proc->transfer_package( it_entity_data = lt_entity_data_transfer
                                                  is_parameters  = is_parameters ).

                CLEAR lt_entity_data_transfer.
              ENDIF.

            ENDLOOP.

          ENDIF.

          "calculate progress
          lv_progress_perc_flt = lv_act_package / lv_num_packages * 100.
          MESSAGE s048(/vpcoe/common) WITH lv_act_package lv_num_packages INTO /vpcoe/cl_rdp_log=>sv_msg_text.

          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = lv_progress_perc_flt
              text       = /vpcoe/cl_rdp_log=>sv_msg_text.

          lv_act_package = lv_act_package + 1.
        ENDWHILE.

        "final transfer
        IF lt_entity_data_transfer IS NOT INITIAL AND iv_test IS INITIAL.

          " transfer package data
          lo_entity_proc->transfer_package( it_entity_data = lt_entity_data_transfer is_parameters = is_parameters ).

          CLEAR lt_entity_data_transfer.
        ENDIF.

      ENDIF.

      "Write protocol entries (if not in testmode)
      IF iv_test = abap_false.

        ms_protocol-failed = COND #(  WHEN lo_logger->has_error_messages(  )  IS NOT INITIAL
                                  THEN abap_true ELSE abap_false ).

        " in case of successful full load: delete previous entries from the protocol table .
        IF ms_protocol-failed = abap_false AND
           iv_upload_mode EQ /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.

          lo_entity_proc->delete_from_protocol( ).
        ENDIF.

        " write protocol entry
        lo_entity_proc->write_to_protocol(
                  is_protocol = ms_protocol is_selection_params = is_parameters ).

      ENDIF.

    ENDIF.

    " close http connection as not being used
    lo_upl_util = /vpcoe/cl_uph_factory=>get_instance(  )->get_http_util( ).
    IF lo_upl_util IS BOUND.
      lo_upl_util->close_connection( ).
    ENDIF.

    "Log final status
    lv_upload_result = gc_msgty_s.
    IF lo_logger->has_error_messages(  ) = abap_true.
      lv_upload_result = gc_msgty_e.
    ELSEIF lo_logger->has_warning_messages(  ) = abap_true.
      lv_upload_result = gc_msgty_w.
    ENDIF.

    DATA(lv_upload_result_txt) = /vpcoe/cl_uph_exec_load_wrap=>get_domain_fix_val_text(
         iv_domain_name = /vpcoe/cl_uph_exec_load_wrap=>gc_domname_upresult
         iv_fixed_value = lv_upload_result ).

    MESSAGE s049(/vpcoe/common) WITH lv_upload_result_txt DISPLAY LIKE lv_upload_result.
    lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

    "commit log
    lo_logger->commit_application_log( iv_test_mode = iv_test iv_flg_commit = abap_false ) .

  ENDMETHOD.                          "#EC CI_NESTING     "#EC CI_CYCLO


  METHOD GET_DOMAIN_FIX_VAL_TEXT.

    DATA : ls_dom_val TYPE dd07v,
           lv_subrc   TYPE sy-subrc,
           lv_domname TYPE domname,
           lv_value   TYPE domvalue_l.

    lv_domname = iv_domain_name.
    lv_value =      iv_fixed_value.

    " Get the domain fixed value text
    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        domname  = lv_domname
        value    = lv_value
      IMPORTING
        dd07v_wa = ls_dom_val
        rc       = lv_subrc.


    IF lv_subrc IS INITIAL.
      rv_fix_val_text = ls_dom_val-ddtext.
    ENDIF.

  ENDMETHOD.


  METHOD GET_F4_HELP.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = iv_retfield
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = iv_dynprofield
        value_org        = 'S'
        callback_program = sy-repid
*       CALLBACK_METHOD  =
      TABLES
        value_tab        = it_value_tab
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.
    IF sy-subrc IS NOT INITIAL.
      rv_return_code = sy-subrc.
    ENDIF.
  ENDMETHOD.


  METHOD log_final_status.
    DATA lv_upload_result TYPE syst-msgty.

    lv_upload_result = gc_msgty_s.
    IF io_logger->has_error_messages(  ) = abap_true.
      lv_upload_result = gc_msgty_e.
    ELSEIF io_logger->has_warning_messages(  ) = abap_true.
      lv_upload_result = gc_msgty_w.
    ENDIF.

    DATA(lv_upload_result_txt) = /vpcoe/cl_uph_exec_load_wrap=>get_domain_fix_val_text(
         iv_domain_name = /vpcoe/cl_uph_exec_load_wrap=>gc_domname_upresult
         iv_fixed_value = lv_upload_result ).

    MESSAGE s001(/vpcoe/plm) WITH lv_upload_result_txt DISPLAY LIKE lv_upload_result.
    io_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

    "commit log
    io_logger->commit_application_log( iv_test_mode = iv_test iv_flg_commit = abap_false ) .
  ENDMETHOD.


  METHOD process_upload.

    DATA lv_msg_str TYPE string.
    DATA lt_entity_data_transfer TYPE /vpcoe/t_uph_entity_data.
    DATA lv_progress_perc_flt TYPE f.
    DATA lv_act_package TYPE i.
    DATA lv_num_packages TYPE i.

    IF io_entity_proc->fits_param_size_into_protocol( is_parameters ) NE abap_true.
      RETURN.
    ENDIF.

    " prepare processing of upload entity
    DATA(lv_record_cnt) = io_entity_proc->prepare_process( ).

    " when no relevant data found
    IF lv_record_cnt IS INITIAL.
      RETURN.
    ENDIF.

    IF lv_record_cnt MOD mv_package_size = 0.
      lv_num_packages = lv_record_cnt DIV mv_package_size.
    ELSE.
      lv_num_packages = ( lv_record_cnt DIV mv_package_size ) + 1.
    ENDIF.

    "process packages of upload entity
    lv_act_package = 1 .

    CLEAR lt_entity_data_transfer.

    WHILE lv_act_package <= lv_num_packages.

      DATA(lt_entity_data) = io_entity_proc->process_package(
          iv_act_package  = lv_act_package
          iv_package_size = mv_package_size ).

      IF lt_entity_data IS NOT INITIAL AND iv_test IS INITIAL.

        transfer_entities_as_package( EXPORTING is_parameters           = is_parameters
                                                io_entity_proc          = io_entity_proc
                                                iv_final_transfer       = COND #( WHEN lv_act_package = lv_num_packages THEN abap_true ELSE abap_false )
                                      CHANGING  ct_entity_data_transfer = lt_entity_data_transfer
                                                ct_entity_data          = lt_entity_data ).

      ENDIF.

      "calculate progress
      lv_progress_perc_flt = lv_act_package / lv_num_packages * 100.
      MESSAGE s010(/vpcoe/plm) WITH lv_act_package lv_num_packages INTO lv_msg_str.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_progress_perc_flt
          text       = lv_msg_str.

      lv_act_package = lv_act_package + 1.
    ENDWHILE.

    "final transfer
    IF lt_entity_data_transfer IS NOT INITIAL AND iv_test IS INITIAL.

      " transfer package data
      io_entity_proc->transfer_package( it_entity_data = lt_entity_data_transfer iv_final_transfer = abap_true ).

      CLEAR lt_entity_data_transfer.
    ENDIF.


    "Write protocol entries (if not in testmode)
    IF iv_test = abap_false.

      ms_protocol-failed = COND #(  WHEN io_logger->has_error_messages(  )  IS NOT INITIAL
                                THEN abap_true ELSE abap_false ).

      " in case of successful full load: delete previous entries from the protocol table .
      IF ms_protocol-failed = abap_false AND
         iv_upload_mode EQ /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.

        io_entity_proc->delete_from_protocol( ).
      ENDIF.

      " write protocol entry
      io_entity_proc->write_to_protocol( is_protocol = ms_protocol ).

    ENDIF.

  ENDMETHOD.


  METHOD transfer_entities_as_package.
    DATA: lv_act_transfer  TYPE i,
          lv_num_transfers TYPE i.

    DATA(lv_record_cnt) = lines( ct_entity_data ).

    IF lv_record_cnt MOD mv_package_size = 0.
      lv_num_transfers = lv_record_cnt DIV mv_package_size.
    ELSE.
      lv_num_transfers = ( lv_record_cnt DIV mv_package_size ) + 1.
    ENDIF.

    "process packages of upload entity
    lv_act_transfer = 1 .

    LOOP AT ct_entity_data REFERENCE INTO DATA(lr_entity_data).

      APPEND lr_entity_data->* TO ct_entity_data_transfer.

      IF lines( ct_entity_data_transfer ) >= mv_package_size.

        " transfer package data
        io_entity_proc->transfer_package( it_entity_data = ct_entity_data_transfer
                                          iv_final_transfer = COND #( WHEN iv_final_transfer = abap_true AND lv_act_transfer = lv_num_transfers THEN abap_true ELSE abap_false ) ).

        CLEAR ct_entity_data_transfer.

        lv_act_transfer = lv_act_transfer + 1.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
