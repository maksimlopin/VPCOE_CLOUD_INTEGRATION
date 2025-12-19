class /VPCOE/CL_LOG definition
  public
  create public .

public section.

  types:
    BEGIN OF gty_s_sel_crit_msg,
        id     TYPE text40,
        sign   TYPE text1,
        option TYPE text2,
        low    TYPE text255,
        high   TYPE text255,
      END OF gty_s_sel_crit_msg .
  types:
    gty_t_sel_crit_msg TYPE STANDARD TABLE OF gty_s_sel_crit_msg .

  constants:
    BEGIN OF sc_log_sub_obj,
        organization  TYPE /vpcoe/de_log_sub_obj VALUE 'ORGANIZATION',
        configuration TYPE /vpcoe/de_log_sub_obj VALUE 'CONFIG',
        customer      TYPE /vpcoe/de_log_sub_obj VALUE 'CUSTOMER',
        customer_ext  TYPE /vpcoe/de_log_sub_obj VALUE 'CUSTOMER_EXT',
        product       TYPE /vpcoe/de_log_sub_obj VALUE 'PRODUCT',
        product_ext   TYPE /vpcoe/de_log_sub_obj VALUE 'PRODUCT_EXT',
        delivery      TYPE /vpcoe/de_log_sub_obj VALUE 'DELIVERY',
        packaging     TYPE /vpcoe/de_log_sub_obj VALUE 'BATCH',
        material_doc  TYPE /vpcoe/de_log_sub_obj VALUE 'MATERIAL_DOCUMENTS',
        supplier      TYPE /vpcoe/de_log_sub_obj VALUE 'SUPPLIER',
        supplier_ext  TYPE /vpcoe/de_log_sub_obj VALUE 'SUPPLIER_EXT',
*        runid         TYPE /vpcoe/de_log_sub_obj VALUE 'PFM',
        inventory     TYPE /vpcoe/de_log_sub_obj VALUE 'INVENTORY',
        cost_estimate TYPE /vpcoe/de_log_sub_obj VALUE 'COST_ESTIMATE',
        plm           TYPE /vpcoe/de_log_sub_obj VALUE 'PLM',
        reprocess     TYPE /vpcoe/de_log_sub_obj VALUE 'REPROC',
        incoterms     TYPE /vpcoe/de_log_sub_obj VALUE 'INCOTERMS',
        billdocit     TYPE /vpcoe/de_log_sub_obj VALUE 'BILLDOCIT',
        supinvi       TYPE /vpcoe/de_log_sub_obj VALUE 'SUPPINVI',
      END OF sc_log_sub_obj .
  class-data SV_MSG_TEXT type STRING .
  data MT_SUM type /VPCOE/TT_LOG_SUM .

  class-methods GET_LOG_BY_SRV_GROUP
    importing
      !IV_SERVICE_GROUP type /VPCOE/DE_SERVICE_GROUP
      !IV_API_TYPE type /VPCOE/DE_API_TYPE optional
    exporting
      !EO_LOG type ref to /VPCOE/CL_RDP_LOG .
  methods ADD_BAPIRET
    importing
      !IS_BAPIRET2 type BAPIRET2 optional
      !IT_BAPIRET2_T type BAPIRET2_T optional .
  methods ADD_FROM_EXCEPTION
    importing
      !IR_EXCP type ref to CX_ROOT .
  methods ADD_HTTP_RESPONSE
    importing
      !IS_RESPONSE type /VPCOE/CL_HTTP_COMMUNICATION=>GTY_S_RESPONSE
      !IV_STATUS type I
      !IV_REASON type STRING
      !IV_LOG_PARAM type /VPCOE/BALOBJ optional
      !IV_COUNT type /VPCOE/DE_COUNT optional .
  methods ADD_MESSAGES_TO_LOG
    importing
      !IT_BAPIRET2 type BAPIRET2_T .
  methods ADD_MESSAGE_TO_LOG
    importing
      !IS_BAPIRET2 type BAPIRET2 .
  methods ADD_MSG_PROGRESS
    importing
      !IV_SAVE_LOG type ABAP_BOOL default ABAP_TRUE
      !IV_PROGRESS type INT4 default 0
      !IV_LEVEL type STRING optional
      !IV_MSG_TEXT type STRING optional
      !IV_ADD_MESSAGE_TO_LOG type ABAP_BOOL optional .
  methods ADD_SEL_CRITERIA_AS_MSG
    importing
      !IV_ID type TEXT255
      !IT_R_VALUE type ANY TABLE optional
      !IV_VALUE type ANY optional .
  methods ADD_SY_MSG
    importing
      !IV_LOG_PARAM type /VPCOE/BALOBJ optional .
  methods CHECK
    returning
      value(RV_EXISTS) type XFELD .
  methods CONSTRUCTOR
    importing
      !IV_OBJECT type /VPCOE/BALOBJ default '/VPCOE/RDP'
      !IV_SUB_OBJECT type /VPCOE/BALSUBOBJ optional
      !IV_EXTNUMBER type BALNREXT optional
      !IV_MODE type /VPCOE/DE_MODE optional .
  methods MESSAGES_TO_DISPLAY
    importing
      !IV_SUB_OBJECT type /VPCOE/BALOBJ optional .
  methods DISPLAY_MESSAGE
    importing
      !IV_SUB_OBJECT type /VPCOE/BALOBJ optional
      !IV_TEST_MODE type XFELD optional
      !IV_FAILED type ABAP_BOOL optional
      !IV_WITH_TOTAL type ABAP_BOOL default 'X'
      !IV_EXCEL type ABAP_BOOL optional .
  methods SET_SORT_INDEX
    importing
      !IV_SORT_INDEX type BAPI_LINE
    changing
      !CT_MESSAGES type BAPIRET2_T .
  methods GET_MESSAGES
    exporting
      !ET_MESSAGES type BAPIRET2_T .
  methods GET_SUBOBJECT
    returning
      value(RV_SUBOBJECT) type /VPCOE/BALSUBOBJ .
  methods REMOVE_INFO .
  methods SAVE
    importing
      !IV_TEST_MODE type XFELD optional
      !IV_SUB_OBJECT type /VPCOE/BALOBJ optional
      !IV_NO_TOTAL type FLAG optional .
  methods CONVERT_SYMSG_TO_BAPIRET2
    importing
      !IV_LOG_PARAM type /VPCOE/BALOBJ optional
    returning
      value(RS_BAPIRET2) type BAPIRET2 .
  class-methods CONVERT_BAPIRET2_TO_SYMSG
    importing
      !IR_S_BAPIRET2 type ref to BAPIRET2
    exporting
      !ES_SYMSG type SYMSG .
protected section.

  data MV_SUB_OBJECT type /VPCOE/BALSUBOBJ .
  data MT_BAPIRET2_T type BAPIRET2_T .
  data MV_BAL_NAME type /VPCOE/BALOBJ .
  data MV_EXTNUMBER type BALNREXT .
  data MV_LOG_HANDLE type BALLOGHNDL .
  data MV_MODE type /VPCOE/DE_MODE .

  methods ADD_BAPIRET_TO_LOG .
  methods CLEAR_BAPIRET .
  methods SAVE_LOG .
private section.
ENDCLASS.



CLASS /VPCOE/CL_LOG IMPLEMENTATION.


  METHOD ADD_BAPIRET.
    IF is_bapiret2 IS NOT INITIAL.
      INSERT is_bapiret2 INTO TABLE mt_bapiret2_t.
    ENDIF.

    IF it_bapiret2_t IS NOT INITIAL.
      INSERT LINES OF it_bapiret2_t INTO TABLE mt_bapiret2_t.
    ENDIF.
  ENDMETHOD.


  METHOD add_bapiret_to_log.

    LOOP AT mt_bapiret2_t ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).
      me->add_message_to_log( <ls_bapiret2> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD ADD_FROM_EXCEPTION.

    CALL FUNCTION 'RS_EXCEPTION_TO_BAPIRET2'
      EXPORTING
        i_r_exception = ir_excp
      CHANGING
        c_t_bapiret2  = me->mt_bapiret2_t.

  ENDMETHOD.


  METHOD add_http_response.

    IF iv_status = /vpcoe/cl_http_communication=>gc_s_http_status-ok OR iv_status = /vpcoe/cl_http_communication=>gc_s_http_status-created.
      MESSAGE i040(/vpcoe/common) WITH iv_count INTO sv_msg_text.
    ELSE.
      MESSAGE e041(/vpcoe/common) WITH iv_status iv_reason is_response-message(50) is_response-message+50 INTO sv_msg_text.
    ENDIF.
    me->add_sy_msg( iv_log_param ).

    DATA(lv_log_obj) = COND #( WHEN iv_log_param IS NOT INITIAL THEN iv_log_param ELSE mv_sub_object ).

    IF iv_status = /vpcoe/cl_http_communication=>gc_s_http_status-ok.
      ASSIGN mt_sum[ sub_object = lv_log_obj ] TO FIELD-SYMBOL(<ls_sum>).
      IF sy-subrc = 0.
        <ls_sum>-total = <ls_sum>-total + iv_count.
      ELSE.
        INSERT VALUE #( sub_object = lv_log_obj
                        total      = iv_count ) INTO TABLE mt_sum.
      ENDIF.
    ELSE.
      ASSIGN mt_sum[ sub_object = lv_log_obj ] TO <ls_sum>.
      IF sy-subrc = 0.
        <ls_sum>-total_failed = <ls_sum>-total_failed + iv_count.
      ELSE.
        INSERT VALUE #( sub_object   = lv_log_obj
                        total_failed = iv_count ) INTO TABLE mt_sum.
      ENDIF.
    ENDIF.

    LOOP AT is_response-details ASSIGNING FIELD-SYMBOL(<ls_details>).
      CONCATENATE <ls_details>-code ':' INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      CONCATENATE <ls_details>-target <ls_details>-message INTO /vpcoe/cl_rdp_log=>sv_msg_text SEPARATED BY space.
      INSERT VALUE #( type    = 'E'
                      message = /vpcoe/cl_rdp_log=>sv_msg_text ) INTO TABLE mt_bapiret2_t.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_messages_to_log.

    LOOP AT it_bapiret2 ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).
      me->add_message_to_log( <ls_bapiret2>  ).
    ENDLOOP.

  ENDMETHOD.


  METHOD add_message_to_log.

    IF is_bapiret2-id IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = mv_log_handle
          i_s_msg          = VALUE bal_s_msg( msgty = is_bapiret2-type
                                              msgid = is_bapiret2-id
                                              msgno = is_bapiret2-number
                                              msgv1 = is_bapiret2-message_v1
                                              msgv2 = is_bapiret2-message_v2
                                              msgv3 = is_bapiret2-message_v3
                                              msgv4 = is_bapiret2-message_v4 )
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle     = mv_log_handle
          i_msgty          = is_bapiret2-type
          i_text           = is_bapiret2-message
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD add_msg_progress.
  DATA: lv_progress TYPE int4,
        ls_msg      TYPE bapiret2.

  IF iv_msg_text IS NOT INITIAL.
    me->sv_msg_text = iv_msg_text.
    ls_msg-message = iv_msg_text.
  ELSE.
    lv_progress = iv_progress.
    IF lv_progress > 100 OR lv_progress = 0.
      lv_progress = 100.
    ENDIF.

    MESSAGE i000(/vpcoe/common) WITH COND #( WHEN iv_level IS NOT INITIAL THEN iv_level && `:`
                                                                          ELSE `` )
                                     lv_progress
                                     '% processed.' INTO me->sv_msg_text.

    ls_msg = me->convert_symsg_to_bapiret2( ).

  ENDIF.

  me->add_bapiret( EXPORTING is_bapiret2 = ls_msg ).
  IF iv_add_message_to_log = abap_false.
    me->add_message_to_log( ls_msg ).
  ENDIF.

  cl_progress_indicator=>progress_indicate(
    EXPORTING
      i_text               = me->sv_msg_text
      i_output_immediately = 'X' ).

  IF iv_save_log = abap_true AND me->mv_mode = /vpcoe/cl_common_helper=>sc_mode-send.
    me->save_log( ).
  ENDIF.
ENDMETHOD.


  METHOD ADD_SEL_CRITERIA_AS_MSG.

    IF iv_value IS NOT INITIAL.
      MESSAGE i000(/vpcoe/common) WITH iv_value INTO sv_msg_text.
      me->add_sy_msg( ).
    ENDIF.

    LOOP AT it_r_value ASSIGNING FIELD-SYMBOL(<ls_range>).
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_range> TO FIELD-SYMBOL(<lv_sign>).
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_range> TO FIELD-SYMBOL(<lv_option>).
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_range> TO FIELD-SYMBOL(<lv_low>).
      ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_range> TO FIELD-SYMBOL(<lv_high>).
      MESSAGE i000(/vpcoe/common) WITH <lv_sign> <lv_option> <lv_low> <lv_high> INTO sv_msg_text.
      me->add_sy_msg( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD add_sy_msg.

    INSERT me->convert_symsg_to_bapiret2( iv_log_param ) INTO TABLE mt_bapiret2_t.

  ENDMETHOD.


  METHOD CHECK.

    IF line_exists( me->mt_bapiret2_t[ type = 'E' ] ).
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD clear_bapiret.

    CLEAR me->mt_bapiret2_t.

  ENDMETHOD.


  METHOD constructor.

    me->mv_sub_object = iv_sub_object.
    me->mv_extnumber  = iv_extnumber.
    me->mv_mode = COND #( WHEN iv_mode IS INITIAL THEN /vpcoe/cl_common_helper=>sc_mode-screen
                                                  ELSE iv_mode ).

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = VALUE bal_s_log( object    = iv_object
                                                   subobject = me->mv_sub_object
                                                   extnumber = me->mv_extnumber )
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD convert_bapiret2_to_symsg.
    CLEAR es_symsg.
    IF NOT ir_s_bapiret2 IS INITIAL.
      es_symsg-msgid = ir_s_bapiret2->id.
      es_symsg-msgno = ir_s_bapiret2->number.
      es_symsg-msgty = ir_s_bapiret2->type.
      es_symsg-msgv1 = ir_s_bapiret2->message_v1.
      es_symsg-msgv2 = ir_s_bapiret2->message_v2.
      es_symsg-msgv3 = ir_s_bapiret2->message_v3.
      es_symsg-msgv4 = ir_s_bapiret2->message_v4.
    ENDIF.
  ENDMETHOD.


METHOD convert_symsg_to_bapiret2.

  rs_bapiret2 = VALUE #( type       = sy-msgty
                         id         = sy-msgid
                         number     = sy-msgno
                         message_v1 = sy-msgv1
                         message_v2 = sy-msgv2
                         message_v3 = sy-msgv3
                         message_v4 = sy-msgv4
                         parameter  = iv_log_param ).

ENDMETHOD.


  METHOD display_message.

    DATA: lv_msg2        TYPE string,
          lv_total_msg   TYPE string,
          lv_total_sum   TYPE i,
          lv_total_sum_f TYPE i,
          lv_sub_object  TYPE /vpcoe/balobj,
          lv_msg_result  TYPE string,
          lt_bapiret2_t  TYPE bapiret2_t.

    lt_bapiret2_t = mt_bapiret2_t.
    IF iv_with_total = abap_true.
      CLEAR mt_bapiret2_t. "Messages were added to the log already
    ENDIF.

    lv_sub_object = COND #( WHEN iv_sub_object IS SUPPLIED THEN iv_sub_object
                                                           ELSE me->mv_sub_object ).

    DATA(lv_sum) = VALUE #( mt_sum[ sub_object = lv_sub_object ]-total OPTIONAL ).
    DATA(lv_sum_failed) = VALUE #( mt_sum[ sub_object = lv_sub_object ]-total_failed OPTIONAL ).

    IF iv_failed IS INITIAL.
      IF iv_with_total = abap_true AND iv_excel = abap_false.
        IF iv_test_mode = abap_false.
          MESSAGE i036(/vpcoe/common) WITH lv_sum INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          me->add_sy_msg( iv_sub_object ).
          IF lv_sum_failed > 0.
            MESSAGE i037(/vpcoe/common) WITH lv_sum_failed INTO /vpcoe/cl_rdp_log=>sv_msg_text.
            me->add_sy_msg( iv_sub_object ).
          ENDIF.
        ELSE.
          MESSAGE i038(/vpcoe/common) WITH lv_sum INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          me->add_sy_msg( iv_sub_object ).
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE e039(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      me->add_sy_msg( iv_sub_object ).
    ENDIF.

    IF lines( mt_sum ) > 1.
      LOOP AT mt_sum ASSIGNING FIELD-SYMBOL(<ls_sum>).
        lv_total_msg = COND #( WHEN lv_total_msg IS INITIAL THEN |{ <ls_sum>-sub_object }|
                                                            ELSE |{ lv_total_msg } , { <ls_sum>-sub_object }| ).
        lv_total_sum = lv_total_sum + <ls_sum>-total.
        lv_total_sum_f = lv_total_sum_f + <ls_sum>-total_failed.
      ENDLOOP.
      lv_total_msg = |__________ { lv_total_msg } __________|.
      me->add_bapiret( EXPORTING is_bapiret2 = VALUE #( type      = 'I'
                                                        message   = lv_total_msg
                                                        parameter = iv_sub_object ) ).
      IF iv_test_mode = abap_false.
        MESSAGE i036(/vpcoe/common) WITH lv_total_sum INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        me->add_sy_msg( iv_sub_object ).
        IF lv_total_sum_f > 0.
          MESSAGE i037(/vpcoe/common) WITH lv_total_sum_f INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          me->add_sy_msg( iv_sub_object ).
        ENDIF.
      ELSE.
        MESSAGE i038(/vpcoe/common) WITH lv_total_sum INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        me->add_sy_msg( iv_sub_object ).
      ENDIF.
    ENDIF.

    WRITE: /.
    DATA(lv_header) = |********************* { lv_sub_object } { COND #( WHEN me->mv_extnumber IS NOT INITIAL
                                                                  THEN '- ' &&  me->mv_extnumber ) }  ********************* |.
    WRITE: lv_header.

    IF iv_excel = abap_false.
      APPEND LINES OF mt_bapiret2_t TO lt_bapiret2_t.
    ENDIF.

    LOOP AT lt_bapiret2_t ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).
      IF lv_sub_object <> iv_sub_object AND <ls_bapiret2>-parameter <> iv_sub_object.
        CONTINUE.
      ENDIF.
      IF <ls_bapiret2>-type = 'E'.
        WRITE: /, '<<<                    ERROR                    <<<'.
      ENDIF.
      IF <ls_bapiret2>-id IS INITIAL.
        /vpcoe/cl_rdp_log=>sv_msg_text = <ls_bapiret2>-message.
        SPLIT /vpcoe/cl_rdp_log=>sv_msg_text AT cl_abap_char_utilities=>newline INTO /vpcoe/cl_rdp_log=>sv_msg_text lv_msg2.
      ELSE.
        MESSAGE ID <ls_bapiret2>-id TYPE <ls_bapiret2>-type NUMBER <ls_bapiret2>-number
          WITH <ls_bapiret2>-message_v1 <ls_bapiret2>-message_v2 <ls_bapiret2>-message_v3 <ls_bapiret2>-message_v4 INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      ENDIF.
      IF lv_msg2 IS NOT INITIAL.
        WRITE: /, /vpcoe/cl_rdp_log=>sv_msg_text, /, lv_msg2.
      ELSE.
        WRITE: /, /vpcoe/cl_rdp_log=>sv_msg_text.
      ENDIF.
      IF <ls_bapiret2>-type = 'E'.
        WRITE: /, '>>>                                          >>>'.
      ENDIF.
    ENDLOOP.

    WRITE: /, '-------------------------------------------------------------------', /.

  ENDMETHOD.


  METHOD GET_LOG_BY_SRV_GROUP.

    CONSTANTS: lc_api_plm TYPE /vpcoe/de_api_type VALUE 'PLM'.

    CLEAR eo_log.

    CASE iv_service_group.
      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-batch.
        eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-packaging ).

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-configuration.
        eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-configuration ).

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-customer.
        eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-customer ).

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-delivery.
        eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-delivery ).

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc.
        eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ).

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-organization.
        eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-organization ).

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-inventory.
        eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-inventory ).

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-product.
        eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-product ).

      WHEN /vpcoe/cl_rdp_helper=>sc_grp_id-supplier.
        eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-supplier ).

      WHEN ''.
        IF iv_api_type = lc_api_plm.
          eo_log = NEW /vpcoe/cl_rdp_log( iv_sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-plm ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD GET_MESSAGES.
    et_messages = me->mt_bapiret2_t.
  ENDMETHOD.


  METHOD GET_SUBOBJECT.
    rv_subobject = me->mv_sub_object.
  ENDMETHOD.


  METHOD messages_to_display.

    DATA: lv_msg2        TYPE string,
          lv_total_msg   TYPE string,
          lv_total_sum   TYPE i,
          lv_total_sum_f TYPE i,
          lv_sub_object  TYPE /vpcoe/balobj,
          lv_msg_result  TYPE string,
          lt_bapiret2_t  TYPE bapiret2_t.

    lt_bapiret2_t = mt_bapiret2_t.

    lv_sub_object = COND #( WHEN iv_sub_object IS SUPPLIED THEN iv_sub_object
                                                           ELSE me->mv_sub_object ).

    DATA(lv_header) = |********************* { lv_sub_object } { COND #( WHEN me->mv_extnumber IS NOT INITIAL
                                                                  THEN '- ' &&  me->mv_extnumber ) }  ********************* |.
    WRITE: lv_header.

    LOOP AT lt_bapiret2_t ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).
      IF lv_sub_object <> iv_sub_object AND <ls_bapiret2>-parameter <> iv_sub_object.
        CONTINUE.
      ENDIF.
      IF <ls_bapiret2>-type = 'E'.
        WRITE: /, '<<<                    ERROR                    <<<'.
      ENDIF.
      IF <ls_bapiret2>-id IS INITIAL.
        /vpcoe/cl_rdp_log=>sv_msg_text = <ls_bapiret2>-message.
        SPLIT /vpcoe/cl_rdp_log=>sv_msg_text AT cl_abap_char_utilities=>newline INTO /vpcoe/cl_rdp_log=>sv_msg_text lv_msg2.
      ELSE.
        MESSAGE ID <ls_bapiret2>-id TYPE <ls_bapiret2>-type NUMBER <ls_bapiret2>-number
          WITH <ls_bapiret2>-message_v1 <ls_bapiret2>-message_v2 <ls_bapiret2>-message_v3 <ls_bapiret2>-message_v4 INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      ENDIF.
      IF lv_msg2 IS NOT INITIAL.
        WRITE: /, /vpcoe/cl_rdp_log=>sv_msg_text, /, lv_msg2.
      ELSE.
        WRITE: /, /vpcoe/cl_rdp_log=>sv_msg_text.
      ENDIF.
      IF <ls_bapiret2>-type = 'E'.
        WRITE: /, '>>>                                          >>>'.
      ENDIF.
    ENDLOOP.

    WRITE: /, '-------------------------------------------------------------------', /.

  ENDMETHOD.


  METHOD REMOVE_INFO.

    DELETE mt_bapiret2_t WHERE type = 'I'.

  ENDMETHOD.


  METHOD save.

    IF iv_no_total IS INITIAL.

      DATA(lv_sub_object) = COND #( WHEN iv_sub_object IS SUPPLIED THEN iv_sub_object
                                                             ELSE me->mv_sub_object ).

      DATA(lv_sum) = VALUE #( mt_sum[ sub_object = lv_sub_object ]-total OPTIONAL ).

      IF iv_test_mode = abap_false.
        MESSAGE i042(/vpcoe/common) WITH lv_sum INTO sv_msg_text.
      ELSE.
        MESSAGE i038(/vpcoe/common) WITH lv_sum INTO sv_msg_text.
      ENDIF.
      me->add_sy_msg( ).

    ENDIF.

    me->add_bapiret_to_log( ).

    me->save_log( ).

  ENDMETHOD.


METHOD save_log.
  DATA: lt_log_handle TYPE bal_t_logh,
        ptask         TYPE char4 VALUE 'rrr'.

  INSERT me->mv_log_handle INTO TABLE lt_log_handle.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle = lt_log_handle
    EXCEPTIONS
      log_not_found  = 0
      OTHERS         = 1.

  IF sy-subrc = 0.
    CALL FUNCTION 'DB_COMMIT'.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD set_sort_index.

  LOOP AT ct_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
    <ls_message>-row = iv_sort_index.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
