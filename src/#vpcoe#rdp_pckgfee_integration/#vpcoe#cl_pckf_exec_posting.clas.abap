CLASS /vpcoe/cl_pckf_exec_posting DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.           "#EC INTF_IN_CLASS     "#EC NUM_PUBLIC_ATTR

    INTERFACES /vpcoe/if_pckf_exec_posting .

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS gc_msgty_e TYPE symsgty VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_msgty_i TYPE symsgty VALUE 'I' ##NO_TEXT.
    CONSTANTS gc_msgty_s TYPE symsgty VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_msgty_w TYPE symsgty VALUE 'W' ##NO_TEXT.

    METHODS post_entity_changes
      IMPORTING
        !iv_entity_type TYPE /vpcoe/pckf_entity
        !is_parameters  TYPE /vpcoe/s_pckf_posting_input OPTIONAL
        !iv_test        TYPE /vpcoe/ehfnd_bool OPTIONAL .

    METHODS cleanup_cache
      IMPORTING
        !iv_failed_retention_days TYPE i
        !iv_test                  TYPE /vpcoe/ehfnd_bool OPTIONAL .

    METHODS transfer_package
      IMPORTING
        !it_entity_data_transfer TYPE /vpcoe/t_pckf_entity_data
        !io_entity_proc          TYPE REF TO /vpcoe/if_pckf_entity_proc
        !io_cache                TYPE REF TO /vpcoe/if_pckf_cache
        !io_logger               TYPE REF TO /vpcoe/if_pckf_logger
        !iv_act_package          TYPE i
        !iv_num_packages         TYPE i
        !is_parameters           TYPE /vpcoe/s_pckf_posting_input OPTIONAL
        !iv_test                 TYPE /vpcoe/ehfnd_bool OPTIONAL.

    METHODS log_transfer_result
      IMPORTING
        !ir_ent_pckg_fee TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee.

ENDCLASS.



CLASS /VPCOE/CL_PCKF_EXEC_POSTING IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_exec_posting~execute_posting.

    "Get the logger
    DATA(lo_logger) = /vpcoe/cl_pckf_factory=>get_instance( )->get_logger(
          iv_test_mode   = iv_test ).

    "Log report start
    IF iv_test = abap_false.
      MESSAGE s050(/vpcoe/pckf) WITH sy-datum sy-uzeit INTO DATA(lv_message).
    ELSE.
      MESSAGE s051(/vpcoe/pckf) WITH sy-datum sy-uzeit INTO lv_message.
    ENDIF.
    lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid  msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 ) ) ).

    DATA(ls_parameters) = VALUE /vpcoe/s_pckf_posting_input( ).
    IF is_parameters IS NOT INITIAL.
      MOVE-CORRESPONDING is_parameters TO ls_parameters.
    ENDIF.

    "Post packaging fees changes
    post_entity_changes(
      EXPORTING
        iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
        is_parameters  = ls_parameters
        iv_test        = iv_test
    ).

    "Final cache cleanup
    cleanup_cache(
      EXPORTING
        iv_failed_retention_days = ls_parameters-failed_retention_days
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

    MESSAGE s052(/vpcoe/pckf) WITH lv_result_txt DISPLAY LIKE lv_result.
    lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

    "commit log
    lo_logger->commit_application_log( iv_test_mode = iv_test iv_flg_commit = abap_false ) .

  ENDMETHOD.


  METHOD cleanup_cache.

    IF iv_test = abap_true.
      RETURN.
    ENDIF.

    " Get the cache service
    DATA(lo_cache) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_cache( ).

    "Are there still packaging fee entities in cache?
    DATA(lt_cache_ref) = lo_cache->get_references( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).

    "Remove failed entries which miss the retention period
    GET TIME STAMP FIELD DATA(lv_compare_timestamp).
    DATA(lv_retention_period_secs) = 86400 * iv_failed_retention_days . "days retention period in seconds

    lv_compare_timestamp = cl_abap_tstmp=>subtractsecs( tstmp = lv_compare_timestamp secs = lv_retention_period_secs ).

    LOOP AT lt_cache_ref REFERENCE INTO DATA(lr_cache_ref).

      IF lr_cache_ref->failed_cnt > 0 AND lr_cache_ref->creationdatetime <= lv_compare_timestamp.

        lo_cache->get_entities(
          EXPORTING
            iv_entity_type      = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
            iv_ref_prot_uuid    = lr_cache_ref->ref_prot_uuid
            iv_failed_only      = abap_true
          RECEIVING
            rv_result           = DATA(lt_entities)
        ).

        LOOP AT lt_entities INTO DATA(lr_entity).
          IF lr_entity->is_failed( ) = abap_true.
            lo_cache->delete_entity( iv_uuid = lr_entity->get_uuid( ) ).
            SUBTRACT 1 FROM lr_cache_ref->rec_cnt.
          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD log_transfer_result.

    DATA: lv_log_success TYPE abap_bool,
          lv_message     TYPE string,
          lv_valid_from  TYPE bdc_fval,
          lv_valid_to    TYPE bdc_fval.

    "Get the logger
    DATA(lo_logger) = /vpcoe/cl_pckf_factory=>get_instance( )->get_logger( ).

    WRITE ir_ent_pckg_fee->get_validfrom( ) TO lv_valid_from LEFT-JUSTIFIED NO-GROUPING.
    WRITE ir_ent_pckg_fee->get_validto( ) TO lv_valid_to LEFT-JUSTIFIED NO-GROUPING.

    DATA(lv_product)    =  ir_ent_pckg_fee->get_productid( ).
    DATA(lv_validity)   =  lv_valid_from && '-' && lv_valid_to.

    DATA(lt_org_data) = ir_ent_pckg_fee->get_organizationdata( ).

    IF lines( lt_org_data ) = 1.
      DATA(ls_org_data) = lt_org_data[ 1 ].

      DATA(lv_sales_area) =  ls_org_data-salesorganization && '/'  && ls_org_data-division && '/'
                             && ls_org_data-distributionchannel && '/' && ls_org_data-companycode.
    ENDIF.

    DATA(lv_failed) = ir_ent_pckg_fee->is_failed( ).
    DATA(lt_messages) = ir_ent_pckg_fee->get_messages( ).

    IF lv_failed = abap_true.
      MESSAGE e065(/vpcoe/pckf) WITH lv_product lv_validity lv_sales_area INTO lv_message.

      IF lo_logger IS BOUND.
        lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 ) ) ).
      ENDIF.
    ELSE.
      lv_log_success = abap_false.
      LOOP AT lt_messages REFERENCE INTO DATA(lr_s_bdcmsgcoll) WHERE msgtyp = 'E' OR msgtyp = 'W'.
        lv_log_success = abap_true.
        EXIT.
      ENDLOOP.

      IF lv_log_success = abap_true.
        MESSAGE w066(/vpcoe/pckf) WITH lv_product lv_validity lv_sales_area INTO lv_message.

        IF lo_logger IS BOUND.
          lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 ) ) ).
        ENDIF.
      ENDIF.

    ENDIF.

    "Log messages
    LOOP AT lt_messages REFERENCE INTO lr_s_bdcmsgcoll.

      "Filter messages with no additional information value
      IF lv_failed = abap_false AND lv_log_success = abap_false.
        CONTINUE.
      ENDIF.

      IF lr_s_bdcmsgcoll->msgid = 'VK' AND lr_s_bdcmsgcoll->msgnr = '023'.
        CONTINUE.
      ENDIF.

      MESSAGE ID lr_s_bdcmsgcoll->msgid TYPE lr_s_bdcmsgcoll->msgtyp NUMBER lr_s_bdcmsgcoll->msgnr WITH lr_s_bdcmsgcoll->msgv1 lr_s_bdcmsgcoll->msgv2 lr_s_bdcmsgcoll->msgv3 lr_s_bdcmsgcoll->msgv4 INTO lv_message.

      IF lo_logger IS BOUND.
        lo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD post_entity_changes.

    DATA:
      lo_logger               TYPE REF TO /vpcoe/if_pckf_logger,
      lo_entity_proc          TYPE REF TO /vpcoe/if_pckf_entity_proc,
      lo_proc_appl_part       TYPE REF TO /vpcoe/if_pckf_entity_proc,
      lv_package_size         TYPE i,
      lv_act_package          TYPE i,
      lv_progress_perc_flt    TYPE f,
      lv_record_cnt           TYPE i,
      lv_transfer_cnt         TYPE i,
      lv_num_packages         TYPE i,
      lt_entity_data_transfer TYPE /vpcoe/t_pckf_entity_data,
      lt_entity_pckg_fee      TYPE /vpcoe/t_pckf_entity_data,
      ls_parameters           TYPE /vpcoe/s_pckf_posting_input,
      lv_message              TYPE string.

    "Get the logger
    lo_logger = /vpcoe/cl_pckf_factory=>get_instance( )->get_logger(
          iv_test_mode   = iv_test ).

    IF is_parameters IS NOT INITIAL.
      ls_parameters = is_parameters.
    ENDIF.

    " Get the cache service
    DATA(lo_cache) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_cache( ).

    "get entity processor
    lo_entity_proc = /vpcoe/cl_pckf_factory=>get_instance(  )->get_entity_processor(
                            iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
                            iv_upload_mode = /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_full
                            is_parameters = is_parameters ).

    "Read changed entities from worklist
    DATA(lt_cache_ref) = lo_cache->get_references( iv_entity_type = iv_entity_type ).

    LOOP AT lt_cache_ref REFERENCE INTO DATA(lr_cache_ref).
      ADD lr_cache_ref->rec_cnt TO lv_record_cnt.
    ENDLOOP.

    "Define package size and packages for transfer
    lv_package_size = ls_parameters-package_size.
    IF lv_package_size <= 0.
      lv_package_size = /vpcoe/if_pckf_entity_proc=>gc_defaults-posting_package_size.
    ENDIF.

    IF lv_record_cnt MOD lv_package_size = 0.
      lv_num_packages = lv_record_cnt DIV lv_package_size.
    ELSE.
      lv_num_packages = ( lv_record_cnt DIV lv_package_size ) + 1.
    ENDIF.

    lv_act_package = 1.

    "Process entities
    LOOP AT lt_cache_ref REFERENCE INTO lr_cache_ref.

      DATA(lt_entities) = lo_cache->get_entities(
        EXPORTING
          iv_entity_type      = lr_cache_ref->entity_type
          iv_ref_prot_uuid    = lr_cache_ref->ref_prot_uuid
      ).

      IF lt_entities IS NOT INITIAL.

        "Read related entities
        DATA(lt_related_entities) = lo_cache->get_entities(
        EXPORTING
          iv_entity_type      =  /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee
          iv_ref_prot_uuid    = lr_cache_ref->ref_prot_uuid
      ).

      ENDIF.

      lt_entity_pckg_fee = COND #( WHEN iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee THEN lt_entities ELSE lt_related_entities ).

      LOOP AT lt_entity_pckg_fee INTO DATA(lr_entity).

        APPEND lr_entity TO lt_entity_data_transfer.

        lv_transfer_cnt = lines( lt_entity_data_transfer ).

        IF lv_transfer_cnt >= lv_package_size.

          transfer_package(
            EXPORTING
              it_entity_data_transfer = lt_entity_data_transfer
              io_entity_proc          = lo_entity_proc
              io_cache                = lo_cache
              io_logger               = lo_logger
              iv_act_package          = lv_act_package
              iv_num_packages         = lv_num_packages
              is_parameters           = ls_parameters
              iv_test                 = iv_test
          ).

          CLEAR lt_entity_data_transfer.

          "calculate progress
          lv_progress_perc_flt = lv_act_package / lv_num_packages * 100.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = lv_progress_perc_flt
              text       = lv_message.

          lv_act_package = lv_act_package + 1.

        ENDIF.


      ENDLOOP.

    ENDLOOP.

    "final transfer
    IF lt_entity_data_transfer IS NOT INITIAL.

      lv_transfer_cnt = lines( lt_entity_data_transfer ).

      transfer_package(
        EXPORTING
          it_entity_data_transfer = lt_entity_data_transfer
          io_entity_proc          = lo_entity_proc
          io_cache                = lo_cache
          io_logger               = lo_logger
          iv_act_package          = lv_act_package
          iv_num_packages         = lv_num_packages
          is_parameters           = ls_parameters
          iv_test                 = iv_test
      ).

      "calculate progress
      lv_progress_perc_flt = lv_act_package / lv_num_packages * 100.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_progress_perc_flt
          text       = lv_message.

    ENDIF.

  ENDMETHOD.


  METHOD transfer_package.

    DATA:
      lv_message    TYPE string,
      lv_total_cnt  TYPE i,
      lv_failed_cnt TYPE i.

    lv_total_cnt = lines( it_entity_data_transfer ).

    "reset failed indicator
    LOOP AT it_entity_data_transfer INTO DATA(lr_entity).

      lr_entity->set_failed( abap_false ).
      lr_entity->set_messages( VALUE #( ) ).

    ENDLOOP.

    " transfer package data
    io_entity_proc->transfer_package( it_entity_data = it_entity_data_transfer ).

    lv_failed_cnt = 0.
    LOOP AT it_entity_data_transfer INTO lr_entity.

      IF lr_entity->is_failed( ) = abap_true.

        IF iv_test = abap_false.
          io_cache->set_failed( lr_entity->get_uuid( ) ).
        ENDIF.

        ADD 1 TO lv_failed_cnt.
      ELSE.

        IF iv_test = abap_false.
          io_cache->delete_entity( iv_uuid = lr_entity->get_uuid( ) ).
        ENDIF.
      ENDIF.

      log_transfer_result(
        EXPORTING
          ir_ent_pckg_fee = CAST #( lr_entity )
      ).

    ENDLOOP.

    IF lv_failed_cnt = 0.
      MESSAGE s060(/vpcoe/pckf) WITH iv_act_package iv_num_packages lv_total_cnt INTO lv_message.
    ELSE.
      MESSAGE s061(/vpcoe/pckf) WITH iv_act_package iv_num_packages lv_total_cnt lv_failed_cnt INTO lv_message.
    ENDIF.

    io_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid  msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) ) ).

  ENDMETHOD.
ENDCLASS.
