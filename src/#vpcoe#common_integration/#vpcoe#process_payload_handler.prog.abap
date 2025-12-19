*&---------------------------------------------------------------------*
*&  Include           /VPCOE/CLASS_DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_payload_handler DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_t_json_selected TYPE TABLE OF /vpcoe/jsn_cloud.
    TYPES: BEGIN OF ty_s_sel_opt,
             api         TYPE RANGE OF /vpcoe/rdp_srvid-api_type,
             srv_grp     TYPE RANGE OF /vpcoe/rdp_srvid-service_grp,
             srv_id      TYPE RANGE OF /vpcoe/rdp_srvid-service_id,
             session_id  TYPE RANGE OF /vpcoe/session_id,
             session_itm TYPE RANGE OF /vpcoe/jsn_cloud-session_item,
             date        TYPE RANGE OF dats,
             time        TYPE RANGE OF tims,
           END OF ty_s_sel_opt.

    DATA: mt_json_selected TYPE ty_t_json_selected,
          mv_processed     TYPE bool,
          mo_log           TYPE REF TO /vpcoe/cl_rdp_log.

    CONSTANTS: BEGIN OF gc_s_user_command,
                 dspl         TYPE sy-ucomm VALUE 'DSPL',
                 request      TYPE sy-ucomm VALUE 'RQST',
                 request_auto TYPE sy-ucomm VALUE 'RQST_AUTO',
                 remove       TYPE sy-ucomm VALUE 'DEL',
                 dele         TYPE sy-ucomm VALUE 'CLNP',
               END OF gc_s_user_command,
               sc_payload_count TYPE int4 VALUE '100000'.

    CLASS-METHODS: set_search_help
      IMPORTING is_interface    TYPE shlp_descr-interface
                ir_t_api        TYPE ty_s_sel_opt-api
                ir_t_srv_grp    TYPE ty_s_sel_opt-srv_grp
                ir_t_srv_id     TYPE ty_s_sel_opt-srv_id
      RETURNING VALUE(rl_value) TYPE shvalue_d.

    METHODS:
      constructor,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      process_auto,

      process_deletion
        IMPORTING iv_payloads_count TYPE int4,

      process_manual
        IMPORTING iv_payloads_count TYPE int4,

      select_payloads
        IMPORTING is_selopt                TYPE ty_s_sel_opt
        RETURNING VALUE(rv_payloads_count) TYPE int4.

ENDCLASS.

CLASS lcl_payload_handler IMPLEMENTATION.

  METHOD set_search_help.

    DATA: ls_shlp TYPE shlp_descr,
          lt_val  TYPE tfw_ddshretval_tab.

    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = '/VPCOE/F4_SESSION_ID'
        shlptype = 'SH'
      IMPORTING
        shlp     = ls_shlp.

    IF ir_t_api IS NOT INITIAL.
      ls_shlp-selopt = VALUE #( BASE ls_shlp-selopt FOR <ls_api> IN ir_t_api ( shlpname  = '/VPCOE/F4_SESSION_ID'
                                                                               shlpfield = 'API_TYPE'
                                                                               sign      = <ls_api>-sign
                                                                               option    = <ls_api>-option
                                                                               high      = <ls_api>-high
                                                                               low       = <ls_api>-low ) ) .
    ENDIF.
    IF ir_t_srv_grp IS NOT INITIAL.
      ls_shlp-selopt = VALUE #( BASE ls_shlp-selopt FOR <ls_srvg> IN ir_t_srv_grp ( shlpname  = '/VPCOE/F4_SESSION_ID'
                                                                                 shlpfield = 'SERVICE_GROUP'
                                                                                 sign      = <ls_srvg>-sign
                                                                                 option    = <ls_srvg>-option
                                                                                 high      = <ls_srvg>-high
                                                                                 low       = <ls_srvg>-low ) ).
    ENDIF.
    IF ir_t_srv_id IS NOT INITIAL.
      ls_shlp-selopt = VALUE #( BASE ls_shlp-selopt FOR <ls_srvi> IN ir_t_srv_id ( shlpname  = '/VPCOE/F4_SESSION_ID'
                                                                                 shlpfield = 'SERVICE_ID'
                                                                                 sign      = <ls_srvi>-sign
                                                                                 option    = <ls_srvi>-option
                                                                                 high      = <ls_srvi>-high
                                                                                 low       = <ls_srvi>-low ) ).
    ENDIF.

    ls_shlp-interface = is_interface.

    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = ls_shlp
      TABLES
        return_values = lt_val.

    rl_value = VALUE #( lt_val[ 1 ]-fieldval OPTIONAL ).

  ENDMETHOD.
* CONSTRUCTOR
  METHOD constructor.

    me->mo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-reprocess ).

  ENDMETHOD.

* SELECT_PAYLOADS
  METHOD select_payloads.

    CLEAR me->mt_json_selected.

    SELECT COUNT(*)
        INTO @rv_payloads_count
          FROM /vpcoe/jsn_cloud
             WHERE session_id    IN @is_selopt-session_id
               AND session_item  IN @is_selopt-session_itm
               AND service_group IN @is_selopt-srv_grp
               AND service_id    IN @is_selopt-srv_id
               AND api_type      IN @is_selopt-api
               AND created       IN @is_selopt-date
               AND created_time  IN @is_selopt-time.

    IF sy-subrc <> 0.

      MESSAGE i046(/vpcoe/common) INTO me->mo_log->sv_msg_text.
      me->mo_log->add_sy_msg( ).

    ELSEIF rv_payloads_count < me->sc_payload_count.

      SELECT *
         UP TO @me->sc_payload_count ROWS
          INTO TABLE @me->mt_json_selected
             FROM /vpcoe/jsn_cloud
              WHERE session_id    IN @is_selopt-session_id
                AND session_item  IN @is_selopt-session_itm
                AND service_group IN @is_selopt-srv_grp
                AND service_id    IN @is_selopt-srv_id
                AND api_type      IN @is_selopt-api
                AND created       IN @is_selopt-date
                AND created_time  IN @is_selopt-time.
      IF sy-subrc <> 0.
        CLEAR rv_payloads_count.
      ENDIF.

    ENDIF.

  ENDMETHOD.

* PROCESS_AUTO
  METHOD process_auto.

    me->handle_user_command(
      EXPORTING
        e_ucomm = me->gc_s_user_command-request_auto ).

  ENDMETHOD.

* PROCESS_DELETION
  METHOD process_deletion.
    DATA: lv_answer    TYPE char1,
          lv_count_txt TYPE char10,
          lv_proceed   TYPE abap_bool,
          lv_msg       TYPE text100.

    IF iv_payloads_count IS INITIAL.
      MESSAGE i015(/vpcoe/common) INTO lv_msg.
      mo_log->add_sy_msg( ).
      RETURN.
    ENDIF.

    IF sy-batch = abap_true.
      lv_proceed = abap_true.
      MESSAGE i000(/vpcoe/common) WITH iv_payloads_count 'lines will be deleted.' '' '' INTO lv_msg.
      mo_log->add_sy_msg( ).
    ELSE.
      lv_count_txt = iv_payloads_count.
      CONCATENATE 'Delete ' lv_count_txt ' entries?' INTO lv_msg SEPARATED BY space.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Removing payload'
          text_question         = lv_msg
          display_cancel_button = ' '
        IMPORTING
          answer                = lv_answer.
      IF lv_answer = 1.
        lv_proceed = abap_true.
      ENDIF.
    ENDIF.

    IF lv_proceed = abap_true.
      me->handle_user_command(
        EXPORTING
          e_ucomm = me->gc_s_user_command-dele ).
    ENDIF.

  ENDMETHOD.

* PROCESS_MANUAL
  METHOD process_manual.

    IF iv_payloads_count > 1000.
      MESSAGE i035(/vpcoe/common) WITH iv_payloads_count.
    ELSE.
      CALL SCREEN 100.

    ENDIF.

  ENDMETHOD.

* HANDLE_TOOLBAR
  METHOD handle_toolbar.

    DATA: ls_toolbar TYPE stb_button.

    INSERT VALUE #( function  = gc_s_user_command-request
                    icon      = icon_execute_object
                    text      = text-001
                    quickinfo = text-001
                    disabled  = ' ' ) INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( butn_type = 3 ) INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( function  = gc_s_user_command-dspl
                    icon      = icon_select_detail
                    text      = text-002
                    quickinfo = text-002
                    disabled  = ' ' ) INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( butn_type = 3 ) INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( function  = gc_s_user_command-remove
                    icon      = icon_remove
                    text      = text-003
                    quickinfo = 'Remove w/o processing'
                    disabled  = ' ' ) INTO TABLE e_object->mt_toolbar.

  ENDMETHOD.

* HANDLE_USER_COMMAND
  METHOD handle_user_command.
    DATA: lt_rows            TYPE lvc_t_row,
          lt_log_sum         TYPE  /vpcoe/tt_log_sum,
          lt_json_to_process TYPE me->ty_t_json_selected,
          lt_messages        TYPE bapiret2_t,
          lv_faield          TYPE xfeld,
          lt_text            TYPE STANDARD TABLE OF txline,
          lv_text            TYPE string,
          lv_function        TYPE sy-ucomm,
          lv_format          TYPE tdformat,
          lv_reason          TYPE string,
          lv_status          TYPE i,
          ls_payload         TYPE /vpcoe/s_jsn_cloud.

    CONSTANTS: cv_api_plm TYPE /vpcoe/de_api_type VALUE 'PLM'.

    IF e_ucomm = gc_s_user_command-request_auto OR e_ucomm = gc_s_user_command-dele.
      lt_json_to_process = me->mt_json_selected.
    ELSE.
      lo_table->get_selected_rows( IMPORTING et_index_rows = lt_rows ).
      lo_table->set_toolbar_interactive( ).

      LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<ls_rows>).
        READ TABLE me->mt_json_selected INDEX <ls_rows>-index ASSIGNING FIELD-SYMBOL(<ls_jsn_cloud>).
        IF sy-subrc = 0.
          INSERT <ls_jsn_cloud> INTO TABLE lt_json_to_process.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CASE e_ucomm.
      WHEN gc_s_user_command-request
        OR gc_s_user_command-request_auto.

        IF lines( lt_json_to_process ) < 1.
          MESSAGE i005(/vpcoe/common) INTO me->mo_log->sv_msg_text.
          me->mo_log->add_sy_msg( ).
        ELSE.
          me->mv_processed = abap_true.
          APPEND INITIAL LINE TO me->mo_log->mt_sum ASSIGNING FIELD-SYMBOL(<ls_log_sum>).
          <ls_log_sum>-sub_object = me->mo_log->get_subobject( ).

          SORT lt_json_to_process BY api_type service_group service_id.
          LOOP AT lt_json_to_process INTO DATA(ls_jsn_cloud_grp) GROUP BY ( api_type      = ls_jsn_cloud_grp-api_type
                                                                            service_group = ls_jsn_cloud_grp-service_group
                                                                            service_id    = ls_jsn_cloud_grp-service_id )
                                                                       ASSIGNING FIELD-SYMBOL(<ls_jsn_cloud_grp>).

            CASE <ls_jsn_cloud_grp>-api_type.
              WHEN /vpcoe/cl_common_helper=>sc_api_type-rdp.
                lo_cust = NEW /vpcoe/cl_rdp_helper( iv_api_type = <ls_jsn_cloud_grp>-api_type
                                                    iv_srv_grp  = <ls_jsn_cloud_grp>-service_group
                                                    iv_srv_id   = <ls_jsn_cloud_grp>-service_id ).

              WHEN /vpcoe/cl_common_helper=>sc_api_type-plm.
                lo_cust = NEW /vpcoe/cl_plm_helper( iv_api_type = <ls_jsn_cloud_grp>-api_type
                                                    iv_srv_grp  = <ls_jsn_cloud_grp>-service_group
                                                    iv_srv_id   = <ls_jsn_cloud_grp>-service_id ).
            ENDCASE.

            DATA(lv_url) = lo_cust->get_service_url( IMPORTING et_bapiret2 = lt_messages ).
            lo_cust->get_url_from_rfc(
              EXPORTING
                iv_rfc_name      = CONV #( lo_cust->get_generic_rfc_name( ) )
              IMPORTING
                et_bapiret2      = lt_bapiret2
                ev_server        = DATA(lv_server)
                ev_proxy_host    = DATA(lv_proxy_host)
                ev_proxy_service = DATA(lv_proxy_service) ).

            me->mo_log->add_bapiret( EXPORTING it_bapiret2_t = lt_messages ).

            IF line_exists( lt_bapiret2[ type = 'E' ] ).
              CONTINUE.
            ENDIF.

            LOOP AT GROUP <ls_jsn_cloud_grp> ASSIGNING FIELD-SYMBOL(<ls_jsn_cloud_2>).
              CALL FUNCTION '/VPCOE/SEND_JSON_BCKGRND'
                EXPORTING
                  iv_rfc_name      = lo_cust->get_generic_rfc_name( )
                  iv_json          = <ls_jsn_cloud_2>-json
                  iv_count         = <ls_jsn_cloud_2>-lines_count
                  iv_api_type      = <ls_jsn_cloud_2>-api_type
                  iv_sub_object    = me->mo_log->get_subobject( )
                  iv_full_url      = 'https://' && lv_server && lv_url
                  iv_proxy_host    = lv_proxy_host
                  iv_proxy_service = lv_proxy_service
                IMPORTING
                  et_log_sum       = lt_log_sum
                  ev_failed        = <ls_jsn_cloud_2>-failed
                  ev_status        = lv_status
                  ev_reason        = lv_reason
                TABLES
                  et_messages      = lt_messages.

              IF lv_status <> /vpcoe/cl_rdp_http=>gc_s_http_status-ok.
                MOVE-CORRESPONDING <ls_jsn_cloud_2> TO ls_payload.
                "send e-mail notification only once per session
                /vpcoe/cl_rdp_helper=>send_error_payload_to_email(
                   EXPORTING
                     is_payload = ls_payload
                     io_log     = me->mo_log ).
              ENDIF.

              UPDATE /vpcoe/jsn_cloud
                  SET repeated         = sy-datum
                      response_status  = lv_status
                      response_message = lv_reason
                   WHERE session_id = <ls_jsn_cloud_2>-session_id.

              ASSIGN lt_log_sum[ 1 ] TO FIELD-SYMBOL(<ls_log_sum_ret>).
              IF sy-subrc = 0.
                <ls_log_sum>-total = <ls_log_sum>-total + <ls_log_sum_ret>-total.
                <ls_log_sum>-total_failed = <ls_log_sum>-total_failed + <ls_log_sum_ret>-total_failed.
              ENDIF.
              me->mo_log->add_bapiret( EXPORTING it_bapiret2_t = lt_messages ).
            ENDLOOP.
          ENDLOOP.

          SORT lt_json_to_process BY failed.
          DELETE lt_json_to_process WHERE failed = abap_true.
          DELETE /vpcoe/jsn_cloud FROM TABLE lt_json_to_process.

          IF e_ucomm <> gc_s_user_command-request_auto.
            LEAVE TO SCREEN 0.
          ENDIF.
        ENDIF.

      WHEN gc_s_user_command-dspl.
        IF lines( lt_json_to_process ) > 1.
          MESSAGE i006(/vpcoe/common).
        ELSEIF lines( lt_json_to_process ) < 1.
          MESSAGE i005(/vpcoe/common).
        ELSE.
          IF <ls_jsn_cloud>-json IS NOT INITIAL.
            TRY.
                CALL TRANSFORMATION sjson2html SOURCE XML <ls_jsn_cloud>-json
                                  RESULT XML DATA(lvc_html).
                IF sy-subrc = 0.
                  cl_abap_browser=>show_html(
                    title        = 'JSON'
                    html_string  = cl_abap_codepage=>convert_from( lvc_html ) ).
                ENDIF.
              CATCH cx_xslt_runtime_error.
                lv_text = <ls_jsn_cloud>-json.
                DATA(lv_strlen) = strlen( lv_text ).
                DATA(lv_sep) = 70.
                DATA(lv_count) = 0.
                WHILE lv_strlen - lv_count > lv_sep.
                  DATA(lv_substr) = CONV string( substring( val = lv_text off = lv_count len = lv_sep ) ).
                  APPEND lv_substr TO lt_text.
                  lv_count = lv_count + lv_sep.
                ENDWHILE.

                lv_substr = substring( val = lv_text off = lv_count ).
                APPEND lv_substr TO lt_text.

                CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
                  EXPORTING
                    im_title        = text-002
                    im_display_mode = 'X'
                  CHANGING
                    ch_text         = lt_text.
            ENDTRY.
          ELSE.
            MESSAGE i013(/vpcoe/common) WITH 'Payload'.
          ENDIF.
        ENDIF.


      WHEN gc_s_user_command-remove.
        IF lines( me->mt_json_selected ) < 1.
          MESSAGE i005(/vpcoe/common).
        ELSE.
          DATA lv_answer TYPE char1.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Removing payload'
              text_question         = 'Delete selected entries?'
              text_button_1         = 'Yes'
              icon_button_1         = 'ICON_CHECKED'
              text_button_2         = 'Cancel'
              icon_button_2         = 'ICON_CANCEL'
              display_cancel_button = ' '
            IMPORTING
              answer                = lv_answer.

          IF lv_answer = 1.
            LOOP AT lt_json_to_process ASSIGNING <ls_jsn_cloud_2>.
              DELETE FROM /vpcoe/jsn_cloud WHERE session_id = <ls_jsn_cloud_2>-session_id.
              DELETE me->mt_json_selected WHERE session_id = <ls_jsn_cloud_2>-session_id.
            ENDLOOP.
            lo_table->refresh_table_display( ).
            cl_gui_cfw=>flush( ).
          ELSE.
            MESSAGE e008(/vpcoe/common).
          ENDIF.
        ENDIF.

      WHEN gc_s_user_command-dele.
        DELETE /vpcoe/jsn_cloud FROM TABLE lt_json_to_process.
        IF sy-subrc = 0.
          MESSAGE i096(/vpcoe/common) INTO DATA(lv_msg).
          mo_log->add_sy_msg( ).
        ENDIF.
        COMMIT WORK.
        CLEAR me->mt_json_selected.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
