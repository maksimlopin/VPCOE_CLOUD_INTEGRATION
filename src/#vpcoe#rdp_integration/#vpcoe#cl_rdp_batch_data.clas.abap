class /VPCOE/CL_RDP_BATCH_DATA definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_s_sel_opt,
        batch_num TYPE RANGE OF mch1-charg,
        plant     TYPE RANGE OF mcha-werks,
        date      TYPE RANGE OF mch1-hsdat,
        mat_num   TYPE RANGE OF mara-matnr,
      END OF ty_s_sel_opt .
  types:
    gty_r_id    TYPE RANGE OF mch1-charg .
  types:
    gty_r_plant TYPE RANGE OF mcha-werks .
  types:
    gty_r_date  TYPE RANGE OF mch1-hsdat .
  types:
    gty_r_matnr  TYPE RANGE OF mara-matnr .
  types GTY_S_BATCH type /VPCOE/STR_BATCH .
  types:
    gty_t_batch TYPE SORTED TABLE OF gty_s_batch WITH NON-UNIQUE KEY id .
  types GTY_S_BATCH_JSON type /VPCOE/STR_BATCH_JSON .

  methods BUILD_JSON
    importing
      !IT_BATCH_DATA type /VPCOE/CL_RDP_BATCH_DATA=>GTY_T_BATCH
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
    returning
      value(RS_JSON) type /VPCOE/CL_RDP_HTTP=>GTY_S_JSON .
  methods CHANGE_STATUS
    importing
      !IV_TEST_RUN type XFELD .
  methods CONSTRUCTOR
    importing
      !IV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE
      !IV_SOURCE type STRING
      !IV_API_TYPE type /VPCOE/DE_API_TYPE .
  methods GET_BATCH
    importing
      !IS_SEL_OPT type /VPCOE/S_SELOPT_BATCH
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IV_MODE type /VPCOE/DE_MODE default /VPCOE/CL_COMMON_HELPER=>SC_MODE-SEND
    exporting
      !ET_BATCH type GTY_T_BATCH
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_BATCH_DELTA
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG optional
      !IV_CHNG_POINTER_ID type EDI_MESTYP
      !IS_SEL_OPT type /VPCOE/S_SELOPT_BATCH optional
    exporting
      !ET_BATCH type GTY_T_BATCH
      !ET_JSON type /VPCOE/CL_RDP_HTTP=>GTY_T_JSON .
  methods GET_HEADER_EXCEL
    exporting
      !EV_HEADER type STRING
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE .
  methods CLOSE_REPLICATION
    importing
      !IO_LOG type ref to /VPCOE/CL_RDP_LOG .
  PROTECTED SECTION.
private section.

  data MT_CPI type /VPCOE/CL_RDP_DELIVERY_DATA=>GTY_T_CPI .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_CHNG_POINTER_ID type EDI_MESTYP .
  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SOURCE type STRING .
ENDCLASS.



CLASS /VPCOE/CL_RDP_BATCH_DATA IMPLEMENTATION.


METHOD build_json.

  rs_json-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                              EXPORTING is_data = VALUE gty_s_batch_json( source   = mv_source
                                                                                          elements = it_batch_data )
                                                                                          iv_pretty_name = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).
  rs_json-count = lines( it_batch_data ).

  REPLACE ALL OCCURRENCES OF ':"0000-00-00"' IN rs_json-elements WITH ': null' ##no_text.
  REPLACE ALL OCCURRENCES OF ':""' IN rs_json-elements WITH ': null' ##no_text.

ENDMETHOD.


  METHOD change_status.
    IF iv_test_run = abap_false.
      CALL FUNCTION 'CHANGE_POINTERS_STATUS_WRITE'
        EXPORTING
          message_type           = mv_chng_pointer_id
        TABLES
          change_pointers_idents = mt_cpi.
    ENDIF.
  ENDMETHOD.


METHOD close_replication.

  "Shoud be redefined in SUMA Handler
  RETURN.

ENDMETHOD.


  METHOD constructor.

    me->mv_api_type     = iv_api_type.
    me->mv_package_size = iv_package_size.
    me->mv_source       = iv_source.

  ENDMETHOD.


  METHOD get_batch.
    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    DATA: lt_batch_pack TYPE gty_t_batch.

    DATA: lv_skip TYPE abap_bool.

    CLEAR: et_batch,
           et_json.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-batch
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-packaging
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-batch
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        io_log      = io_log
        iv_mode     = iv_mode
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.

      DATA: lv_where_clause TYPE string,
            lv_fields       TYPE string.

      lv_where_clause = 'mch1~charg IN @is_sel_opt-display_id AND mcha~werks IN @is_sel_opt-plant AND mch1~hsdat IN @is_sel_opt-date AND mch1~ersda IN @is_sel_opt-ersda AND mara~matnr IN @is_sel_opt-material'.
      lv_fields = 'mch1~charg AS id, mara~matnr AS product, mcha~werks AS plant, mch1~hsdat AS manufacture_date'.
      IF is_sel_opt-class IS NOT INITIAL.
        lv_where_clause = |{ lv_where_clause } AND klah~class = @is_sel_opt-class|.
      ENDIF.

      IF is_sel_opt-characteristic IS NOT INITIAL.
        lv_where_clause = |{ lv_where_clause } AND cabn~atnam = @is_sel_opt-characteristic|.
        lv_fields = |{ lv_fields }, ausp~atwrt AS packaging_composition|.
      ENDIF.

      SELECT DISTINCT (lv_fields)
         INTO CORRESPONDING FIELDS OF TABLE @et_batch
           FROM mch1 LEFT JOIN mcha ON mch1~charg = mcha~charg
           JOIN mara ON mch1~matnr = mara~matnr
           LEFT JOIN kssk ON kssk~objek = mch1~cuobj_bm
           LEFT JOIN klah ON kssk~clint = klah~clint
           LEFT JOIN ausp ON ausp~objek = mch1~cuobj_bm
           LEFT JOIN cabn ON cabn~atinn = ausp~atinn
             WHERE (lv_where_clause).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      io_log->add_msg_progress( EXPORTING iv_level = CONV #( 'Batch' )
                                          iv_add_message_to_log = abap_true
                                          iv_save_log = abap_false ).
    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-batch
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-packaging
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-batch
        iv_api_type = me->mv_api_type
        is_sel_opt  = is_sel_opt
        iv_mode     = iv_mode
        io_log      = io_log
      CHANGING
        ct_data     = et_batch.

    IF iv_mode = /vpcoe/cl_rdp_helper=>sc_mode-send.
      IF mv_package_size IS INITIAL.
        mv_package_size = lines( et_batch ).
      ENDIF.

      LOOP AT et_batch INTO DATA(lr_gr_batch) GROUP BY ( sy-tabix - 1 ) DIV mv_package_size + 1.
        LOOP AT GROUP lr_gr_batch ASSIGNING FIELD-SYMBOL(<ls_batch>).
          INSERT <ls_batch> INTO TABLE lt_batch_pack.
        ENDLOOP.

        APPEND me->build_json( EXPORTING it_batch_data = lt_batch_pack
                                         io_log        = io_log ) TO et_json.
        CLEAR lt_batch_pack.

      ENDLOOP.

      CALL BADI lo_badi->adjust_json
        EXPORTING
          iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-batch
          iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-packaging
          iv_api_type = me->mv_api_type
          io_log      = io_log
        CHANGING
          ct_json     = et_json.


    ENDIF.
  ENDMETHOD.


  METHOD get_batch_delta.

    DATA: lt_r_material   TYPE gty_r_matnr,
          lt_r_display_id TYPE gty_r_id,
          lt_r_objid      TYPE RANGE OF cdobjectv,
          ls_sel_opt      TYPE /vpcoe/s_selopt_batch,
          lv_display_id   TYPE mch1-charg,
          lv_material     TYPE mara-matnr.

    CLEAR: et_batch,
           et_json.

    mv_chng_pointer_id = iv_chng_pointer_id.

    /vpcoe/cl_rdp_helper=>read_change_pointers(
       EXPORTING
         iv_chng_pointer_id = mv_chng_pointer_id
         it_r_change_date   = is_sel_opt-date
       IMPORTING
         et_r_objid        = lt_r_objid
         et_cpi            = mt_cpi ).

    IF lt_r_objid IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_r_objid ASSIGNING FIELD-SYMBOL(<ls_r_objid>).
      CONDENSE <ls_r_objid>-low.
      SPLIT <ls_r_objid>-low AT space INTO lv_material lv_display_id.

      IF lv_material IN is_sel_opt-material.
        INSERT VALUE #( sign   = 'I'
                        option = 'EQ'
                        low    = lv_material ) INTO TABLE lt_r_material.
      ENDIF.
      IF lv_display_id IN is_sel_opt-display_id.
        INSERT VALUE #( sign   = 'I'
                        option = 'EQ'
                        low    = lv_display_id ) INTO TABLE lt_r_display_id.
      ENDIF.
    ENDLOOP.

    ls_sel_opt = is_sel_opt.
    "For Delta Mode Create/Change date shouldn't be taken into consideration for 'regular' select
    CLEAR ls_sel_opt-date.

    ls_sel_opt-material   = lt_r_material.
    ls_sel_opt-display_id = lt_r_display_id.

    me->get_batch(
      EXPORTING
        is_sel_opt = ls_sel_opt
        io_log     = io_log
      IMPORTING
        et_batch   = et_batch
        et_json    = et_json ).

  ENDMETHOD.


  METHOD get_header_excel.

    CLEAR: ev_header, et_header.

    ev_header = 'Batch'.

    et_header = VALUE #( ( description      = 'Batch Number'
                       internal_name    = 'id'
                       data_type        = 'CHAR(10)'
                       mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                       s4hana_attribute = 'MCH1.CHARG / MCHA.CHARG'
                       vpcoe_attribute  = 'ID'
                       is_key           = abap_true )
                     ( description      = 'Product'
                       internal_name    = 'productId'
                       data_type        = 'CHAR(40)'
                       mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                       s4hana_attribute = 'MCHA.MATNR / MCH1.MATNR'
                       vpcoe_attribute  = 'PRODUCT'
                       is_key           = abap_true )
                     ( description      = 'Plant'
                       internal_name    = 'plantId'
                       data_type        = 'CHAR(4)'
                       mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                       s4hana_attribute = 'MCHA.WERKS'
                       vpcoe_attribute  = 'PLANT' )
                     ( description      = 'Packaging Composition'
                       internal_name    = 'packagingCompositionId'
                       data_type        = 'CHAR(40)'
                       mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                       s4hana_attribute = 'ESESUBID'
                       vpcoe_attribute  = 'PACKAGING_COMPOSITION')
                     ( description      = 'Manufacture Date'
                       internal_name    = 'manufactureDate'
                       data_type        = 'Date'
                       mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                       s4hana_attribute = 'MCH1.HSDAT / MCHA.HSDAT'
                       vpcoe_attribute  = 'MANUFACTURE_DATE') ).
  ENDMETHOD.
ENDCLASS.
