class /VPCOE/CL_PCKF_CUSTOMER_ROLE definition
  public
  final
  create public .

public section.             "#EC INTF_IN_CLASS     "#EC NUM_PUBLIC_ATTR

  interfaces /VPCOE/IF_PCKF_EXEC_RETRIEVAL .

  constants GC_MSGTY_E type SYMSGTY value 'E' ##NO_TEXT.
  constants GC_MSGTY_I type SYMSGTY value 'I' ##NO_TEXT.
  constants GC_MSGTY_S type SYMSGTY value 'S' ##NO_TEXT.
  constants GC_MSGTY_W type SYMSGTY value 'W' ##NO_TEXT.
  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS retrieve_entity
      IMPORTING
        !iv_entity_type TYPE /vpcoe/pckf_entity
        !iv_upload_mode TYPE /vpcoe/upload_mode
        !is_parameters  TYPE any OPTIONAL
        !iv_test        TYPE /vpcoe/ehfnd_bool OPTIONAL .

ENDCLASS.



CLASS /VPCOE/CL_PCKF_CUSTOMER_ROLE IMPLEMENTATION.


  METHOD  /VPCOE/IF_PCKF_EXEC_RETRIEVAL~DERIVE_RFC_TARGET_DESTINATION.

    /vpcoe/cl_pckf_factory=>get_instance( )->get_target_destination( IMPORTING ev_endpoint_dest = DATA(lv_target_destination) ).

    IF lv_target_destination IS INITIAL.
      rv_result = /vpcoe/if_pckf_exec_retrieval~gc_default_rfc_destination.
    ELSE.
      rv_result = lv_target_destination.
    ENDIF.

  ENDMETHOD.


  METHOD /VPCOE/IF_PCKF_EXEC_RETRIEVAL~EXECUTE_RETRIEVAL.

    DATA:
      lo_logger            TYPE REF TO /vpcoe/if_pckf_logger,
      lv_progress_perc_flt TYPE f.

    " Get the logger
    lo_logger = /vpcoe/cl_pckf_factory=>get_instance( )->get_logger(
          iv_test_mode   = iv_test
          iv_upload_mode = iv_upload_mode ).

    "Log report start
    IF iv_test = abap_false.
      MESSAGE s026(/vpcoe/pckf) WITH sy-datum sy-uzeit INTO DATA(lv_message).
    ELSE.
      MESSAGE s027(/vpcoe/pckf) WITH sy-datum sy-uzeit INTO lv_message.
    ENDIF.
    lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid  msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).

    "Retrieve packaging fees
    retrieve_entity(
      EXPORTING
        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role
        iv_upload_mode = iv_upload_mode
        is_parameters  = is_parameters
        iv_test        = iv_test
    ).

    "Log final status
    DATA(lv_result) = gc_msgty_s.
    IF lo_logger->has_error_messages(  ) = abap_true.
      lv_result = gc_msgty_e.
    ELSEIF lo_logger->has_warning_messages(  ) = abap_true.
      lv_result = gc_msgty_w.
    ENDIF.

    DATA(lv_result_txt) = lo_logger->get_domain_fix_val_text(
         iv_domain_name = '/VPCOE/UPLOAD_RESULT'
         iv_fixed_value = lv_result ).

    MESSAGE s018(/vpcoe/pckf) WITH lv_result_txt DISPLAY LIKE lv_result.
    lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

    "commit log
    lo_logger->commit_application_log( iv_test_mode = iv_test iv_flg_commit = abap_false ) .

  ENDMETHOD.


  METHOD retrieve_entity.

    DATA:
      lo_logger              TYPE REF TO /vpcoe/if_pckf_logger,
      lo_entity_proc         TYPE REF TO /vpcoe/if_pckf_entity_proc,
      lo_entity_proc_related TYPE REF TO /vpcoe/if_pckf_entity_proc,
      lv_package_size        TYPE i,
      lv_act_package         TYPE i,
      lv_lines               TYPE i,
      lv_related_package     TYPE i,
      lv_progress_perc_flt   TYPE f,
      ls_protocol            TYPE /vpcoe/cstm_prot,
      lt_report_config_ids   TYPE HASHED TABLE OF /vpcoe/pckf_report_config_id WITH UNIQUE DEFAULT KEY,
      lv_next_link           TYPE string,
      lv_delta_link          TYPE string.

    " Get the logger
    lo_logger = /vpcoe/cl_pckf_factory=>get_instance( )->get_logger(
          iv_test_mode   = iv_test
          iv_upload_mode = iv_upload_mode ).

    " Get the cache service
    DATA(lo_cache) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_cache( iv_upload_entity = iv_entity_type ).

    "get entity processors
    lo_entity_proc = /vpcoe/cl_pckf_factory=>get_instance(  )->get_entity_processor(
                            iv_entity_type = iv_entity_type
                            iv_upload_mode = iv_upload_mode
                            is_parameters = is_parameters ).

    IF lo_logger->has_error_messages( ).
      RETURN.
    ENDIF.

    "generate a unique run uuid
    CLEAR ls_protocol.
    ls_protocol-uuid = /vpcoe/cl_common_helper=>generate_session_id( ).

    " populate the start time stamp
    CONCATENATE sy-datum sy-uzeit INTO DATA(lv_timestamp).
    ls_protocol-start_timestamp = lv_timestamp.

    DATA(lv_more_available) = abap_true.

    "retrieve data of packaging fee entity
    lo_entity_proc->prepare_retrieve( ).

    lo_entity_proc->set_parameter_value( iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-test_mode iv_value = iv_test ).
    lo_entity_proc->get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-package_size IMPORTING ev_value = lv_package_size ).

    IF lv_package_size <= 0.
      lv_package_size = /vpcoe/if_pckf_entity_proc=>gc_defaults-retrieve_package_size.
    ENDIF.

    WHILE lv_more_available = abap_true .

      lv_act_package = lv_act_package + 1.

      DATA(lt_entity_data) = lo_entity_proc->retrieve_package( iv_act_package  = lv_act_package
                                                               iv_package_size = lv_package_size  ).

      "write entity data to cache
      IF iv_test = abap_false.
        lo_cache->set_entities( it_entities = lt_entity_data  iv_ref_prot_uuid = ls_protocol-uuid ).
      ENDIF.

      lv_lines = lines( lt_entity_data ).

      IF lv_lines < lv_package_size.
        lv_more_available = abap_false.
      ENDIF.

    ENDWHILE.

    "Write protocol entries (if not in testmode)
    IF iv_test = abap_false.

      DATA(lo_protocol_access) = /vpcoe/cl_pckf_factory=>get_instance( )->get_protocol_access( iv_upload_entity = iv_entity_type ).

      ls_protocol-failed = COND #(  WHEN lo_logger->has_error_messages(  )  IS NOT INITIAL
                                THEN abap_true ELSE abap_false ).

      " in case of successful full load: delete previous entries from the protocol table .
      IF ls_protocol-failed = abap_false AND
         iv_upload_mode EQ /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.

        lo_protocol_access->delete_from_protocol(
          EXPORTING
            iv_entity_type = iv_entity_type
        ).
      ENDIF.

      " write protocol entry
      DATA(ls_parameters) = VALUE /vpcoe/s_pckf_cstm_role_input( ).
      MOVE-CORRESPONDING is_parameters TO ls_parameters.

      lo_protocol_access->write_to_protocol(
                iv_entity_type = iv_entity_type
                iv_upload_mode = iv_upload_mode
                is_protocol = ls_protocol is_selection_params = ls_parameters ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
