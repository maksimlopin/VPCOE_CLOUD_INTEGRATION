CLASS /vpcoe/cl_pckf_exec_retrieval DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.           "#EC INTF_IN_CLASS     "#EC NUM_PUBLIC_ATTR

    INTERFACES /vpcoe/if_pckf_exec_retrieval.

    CONSTANTS gc_msgty_e TYPE symsgty VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_msgty_i TYPE symsgty VALUE 'I' ##NO_TEXT.
    CONSTANTS gc_msgty_s TYPE symsgty VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_msgty_w TYPE symsgty VALUE 'W' ##NO_TEXT.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS retrieve_entity
      IMPORTING
        !iv_entity_type TYPE /vpcoe/pckf_entity
        !iv_upload_mode TYPE /vpcoe/upload_mode
        !is_parameters  TYPE any OPTIONAL
        !iv_test        TYPE /vpcoe/ehfnd_bool OPTIONAL .

ENDCLASS.



CLASS /VPCOE/CL_PCKF_EXEC_RETRIEVAL IMPLEMENTATION.


  METHOD  /vpcoe/if_pckf_exec_retrieval~derive_rfc_target_destination.

    /vpcoe/cl_pckf_factory=>get_instance( )->get_target_destination( IMPORTING ev_endpoint_dest = DATA(lv_target_destination) ).

    IF lv_target_destination IS INITIAL.
      rv_result = /vpcoe/if_pckf_exec_retrieval~gc_default_rfc_destination.
    ELSE.
      rv_result = lv_target_destination.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_exec_retrieval~execute_retrieval.

    DATA:
      lo_logger            TYPE REF TO /vpcoe/if_pckf_logger,
      lv_progress_perc_flt TYPE f.

    " Get the logger
    lo_logger = /vpcoe/cl_pckf_factory=>get_instance( )->get_logger(
          iv_test_mode   = iv_test
          iv_upload_mode = iv_upload_mode ).

    "Log report start
    IF iv_test = abap_false.
      MESSAGE s015(/vpcoe/pckf) WITH sy-datum sy-uzeit INTO DATA(lv_message).
    ELSE.
      MESSAGE s016(/vpcoe/pckf) WITH sy-datum sy-uzeit INTO lv_message.
    ENDIF.
    lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid  msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).

    "Retrieve packaging fees
    retrieve_entity(
      EXPORTING
        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
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

    MESSAGE s017(/vpcoe/pckf) WITH lv_result_txt DISPLAY LIKE lv_result.
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
      lv_related_package     TYPE i,
      lv_progress_perc_flt   TYPE f,
      ls_protocol            TYPE /vpcoe/pckf_prot,
      lt_report_config_ids   TYPE HASHED TABLE OF /vpcoe/pckf_report_config_id WITH UNIQUE DEFAULT KEY,
      lv_next_link           TYPE string,
      lv_delta_link          TYPE string,
      lv_load_id             TYPE /vpcoe/load_id.

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
    IF lo_logger->has_error_messages( ).
      RETURN.
    ENDIF.
    lo_entity_proc->set_parameter_value( iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-test_mode iv_value = iv_test ).
    lo_entity_proc->get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-package_size IMPORTING ev_value = lv_package_size ).

    IF lv_package_size <= 0.
      lv_package_size = /vpcoe/if_pckf_entity_proc=>gc_defaults-retrieve_package_size.
    ENDIF.

    WHILE lv_more_available = abap_true .

      lv_act_package = lv_act_package + 1.

      DATA(lt_entity_data) = lo_entity_proc->retrieve_package( EXPORTING iv_act_package  = lv_act_package
                                                                         iv_package_size = lv_package_size ).

      "collect related report configuration ids
      LOOP AT lt_entity_data INTO DATA(ls_entity_data).
        INSERT ls_entity_data->get_report_config_id( ) INTO TABLE lt_report_config_ids.
      ENDLOOP.

      "write entity data to cache
      IF iv_test = abap_false.
        lo_cache->set_entities( it_entities = lt_entity_data  iv_ref_prot_uuid = ls_protocol-uuid ).
      ENDIF.

      lo_entity_proc->get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-next_link IMPORTING ev_value = lv_next_link ).
      IF lv_next_link IS INITIAL.
        lv_more_available = abap_false.
      ENDIF.

    ENDWHILE.

    "Write protocol entries (if not in testmode)
    IF iv_test = abap_false AND iv_upload_mode EQ /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.

      DATA(lo_protocol_access) = /vpcoe/cl_pckf_factory=>get_instance( )->get_protocol_access( iv_upload_entity = iv_entity_type ).

      ls_protocol-failed = COND #(  WHEN lo_logger->has_error_messages(  )  IS NOT INITIAL
                                THEN abap_true ELSE abap_false ).

*      " in case of successful full load: delete previous entries from the protocol table .
*      IF ls_protocol-failed = abap_false AND
*         iv_upload_mode EQ /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.
*
*        lo_protocol_access->delete_from_protocol(
*          EXPORTING
*            iv_entity_type = iv_entity_type
*        ).
*      ENDIF.

      " write protocol entry
      DATA(ls_parameters) = VALUE /vpcoe/s_pckf_retrieval_input( ).
      MOVE-CORRESPONDING is_parameters TO ls_parameters.

      lo_entity_proc->get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-delta_link IMPORTING ev_value = lv_delta_link ).
      IF lv_delta_link IS NOT INITIAL.
        ls_parameters-delta_link = lv_delta_link.
      ENDIF.
      lo_entity_proc->get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-load_id
                                                      IMPORTING ev_value = lv_load_id ).
      IF lv_load_id IS NOT INITIAL.
        ls_parameters-initial_load_id = lv_load_id.
      ENDIF.

      lo_protocol_access->write_to_protocol(
                iv_entity_type = iv_entity_type
                iv_upload_mode = iv_upload_mode
                is_protocol = ls_protocol
                is_selection_params = ls_parameters ).

    ENDIF.

*    IF lv_delta_link IS NOT INITIAL AND iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
*      AND iv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full AND iv_test IS INITIAL.
*      lo_entity_proc->get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-load_id
*                                                       IMPORTING ev_value = lv_load_id ).
*
*
*      lo_entity_proc->get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-delta_link IMPORTING ev_value = lv_delta_link ).
*      IF lv_delta_link IS NOT INITIAL.
*        ls_parameters-delta_link = lv_delta_link.
*      ENDIF.
*
*      DATA(ls_delta_token) = VALUE /vpcoe/pckfvar(  mandt = sy-mandt
*                                          load_id = lv_load_id
*                                          delta_token = /vpcoe/cl_common_helper=>serialize_json(
*                                                         is_data        = ls_parameters
*                                                         iv_compress    = abap_true
*                                                          iv_pretty_name = 'L' )
*                                          upload_entity = iv_entity_type
*                                           ).
*      INSERT  /vpcoe/pckfvar FROM ls_delta_token.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
