*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

INTERFACE lif_ext_modules_wrapper.

  TYPES:
    ltty_stpox  TYPE STANDARD TABLE OF stpox,
    ltty_cscmat TYPE STANDARD TABLE OF cscmat,
    ltyt_stpov  TYPE STANDARD TABLE OF stpov WITH DEFAULT KEY.

  METHODS:

    get_material_detail
      IMPORTING
        !iv_material          TYPE matnr
        !iv_plant             TYPE werks_d
        !io_logger            TYPE REF TO /vpcoe/if_uph_logger
      EXPORTING
        es_material_general   TYPE bapimatdoa
        es_material_plant     TYPE bapimatdoc
        es_material_valuation TYPE bapimatdobew,

    call_bom_explosion
      IMPORTING
        !ir_bom_key      TYPE REF TO /vpcoe/s_uph_bom_key
        !is_parameters   TYPE /vpcoe/s_pckg_bom_input
        !io_logger       TYPE REF TO /vpcoe/if_uph_logger
      EXPORTING
        es_exp_bom_hdr   TYPE cstmat
        et_exp_bom_items TYPE ltty_stpox
        et_exp_bom_nodes TYPE ltty_cscmat,

    call_clsfn_get_classes
      IMPORTING
                !iv_objectkey_long TYPE cuobn90
                !iv_objecttable    TYPE tabelle
                !iv_keydate        TYPE bapi_keydate
                !iv_classtype      TYPE klassenart
                !io_logger         TYPE REF TO /vpcoe/if_uph_logger
      EXPORTING et_alloc_list      TYPE tt_bapi1003_alloc_list,

    call_clsfn_get_detail
      IMPORTING
                !iv_objectkey_long TYPE cuobn90
                !iv_objecttable    TYPE tabelle
                !iv_keydate        TYPE bapi_keydate
                !iv_classnum       TYPE klasse_d
                !iv_classtype      TYPE klassenart
                !io_logger         TYPE REF TO /vpcoe/if_uph_logger
      EXPORTING et_values_num      TYPE tt_bapi1003_alloc_values_num
                et_values_char     TYPE tt_bapi1003_alloc_values_char
                et_values_curr     TYPE tt_bapi1003_alloc_values_curr,

    call_bom_where_used
      IMPORTING
        !iv_valfr    TYPE datuv
        !iv_valto    TYPE datub
        !iv_material TYPE matnr
        !iv_plant    TYPE werks_d OPTIONAL
        !iv_stlan    TYPE stlan OPTIONAL
      EXPORTING
        et_stpo_used TYPE ltyt_stpov.

ENDINTERFACE.

CLASS lcl_bom_expl_wrapper DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES: lif_ext_modules_wrapper.

ENDCLASS.

CLASS lcl_bom_expl_wrapper IMPLEMENTATION.
* Wrapper implementation to encapsulate dependent module calls

  METHOD lif_ext_modules_wrapper~get_material_detail.

    CLEAR: es_material_general, es_material_plant, es_material_valuation.

    DATA: ls_bapireturn TYPE bapireturn.

    CHECK: iv_material IS NOT INITIAL, iv_plant IS NOT INITIAL.

    CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
      EXPORTING
        plant                 = iv_plant
        material              = CONV bapimatdet-material( iv_material ) "conversion for S4
      IMPORTING
        return                = ls_bapireturn
        material_general_data = es_material_general
        materialplantdata     = es_material_plant
        materialvaluationdata = es_material_valuation.

    IF ls_bapireturn IS NOT INITIAL AND ls_bapireturn-type <> 'S'.
      io_logger->add_bapi_messages( VALUE #( ( CORRESPONDING #( ls_bapireturn ) ) ) ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_ext_modules_wrapper~call_bom_explosion.

    CLEAR: es_exp_bom_hdr, et_exp_bom_items, et_exp_bom_nodes.

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        capid                 = /vpcoe/if_uph_entity_bom_proc=>gc_capid_mat_bom
        datuv                 = ir_bom_key->valfrdats
        mtnrv                 = ir_bom_key->material
        werks                 = ir_bom_key->plant
        mehrs                 = abap_true
        stpst                 = is_parameters-max_exp_lvl
        stlal                 = ir_bom_key->stlal
        stlan                 = ir_bom_key->stlan
*       bom_versn             = ir_bom_key->bom_versn
      IMPORTING
        topmat                = es_exp_bom_hdr
      TABLES
        stb                   = et_exp_bom_items
        matcat                = et_exp_bom_nodes
      EXCEPTIONS
        alt_not_found         = 1
        call_invalid          = 2
        material_not_found    = 3
        missing_authorization = 4
        no_bom_found          = 5
        no_plant_data         = 6
        no_suitable_bom_found = 7
        conversion_error      = 8
        OTHERS                = 9.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE e083(/vpcoe/common) WITH ir_bom_key->material INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        WHEN 2.
          MESSAGE e084(/vpcoe/common) WITH ir_bom_key->material INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        WHEN 3.
          MESSAGE e085(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        WHEN 4.
          MESSAGE e086(/vpcoe/common) INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        WHEN 5.
          MESSAGE e087(/vpcoe/common) WITH ir_bom_key->material INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        WHEN 6.
          MESSAGE e088(/vpcoe/common) WITH ir_bom_key->material INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        WHEN 7.
          MESSAGE e089(/vpcoe/common) WITH ir_bom_key->material INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        WHEN 8.
          MESSAGE e090(/vpcoe/common) WITH ir_bom_key->material INTO /vpcoe/cl_rdp_log=>sv_msg_text.
        WHEN 9.
          MESSAGE e082(/vpcoe/common) WITH ir_bom_key->material INTO /vpcoe/cl_rdp_log=>sv_msg_text.
      ENDCASE.
      io_logger->add_messages( VALUE #(
          ( msgty = 'W' msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) ) ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_ext_modules_wrapper~call_clsfn_get_detail.

    DATA: lt_bapi_return TYPE TABLE OF bapiret2.
    DATA: lv_status TYPE  clstatus.
    DATA: lv_standardcls TYPE stdclass.

    CLEAR: et_values_char, et_values_curr, et_values_num.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey        = CONV objnum( iv_objectkey_long )
        objecttable      = iv_objecttable
        keydate          = iv_keydate
        classnum         = iv_classnum
        classtype        = iv_classtype
        unvaluated_chars = 'X'
      IMPORTING
        status           = lv_status
        standardclass    = lv_standardcls
      TABLES
        allocvalueschar  = et_values_char
        allocvaluescurr  = et_values_curr
        allocvaluesnum   = et_values_num
        return           = lt_bapi_return.

    LOOP AT lt_bapi_return REFERENCE INTO DATA(lr_bapi_return).
      IF lr_bapi_return->* IS NOT INITIAL AND lr_bapi_return->type = 'E'.
        io_logger->add_bapi_messages( VALUE #( ( CORRESPONDING #( lr_bapi_return->* ) ) ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_ext_modules_wrapper~call_clsfn_get_classes.

    DATA: lt_bapi_return TYPE TABLE OF bapiret2.

    CLEAR et_alloc_list.

    CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
      EXPORTING
        objecttable_imp = iv_objecttable
        classtype_imp   = iv_classtype
        keydate         = iv_keydate
        objectkey_imp   = CONV objnum( iv_objectkey_long )
      TABLES
        alloclist       = et_alloc_list
        return          = lt_bapi_return.

    LOOP AT lt_bapi_return REFERENCE INTO DATA(lr_bapi_return).
      IF lr_bapi_return->* IS NOT INITIAL AND lr_bapi_return->type = 'E'.
        io_logger->add_bapi_messages( VALUE #( ( CORRESPONDING #( lr_bapi_return->* ) ) ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_ext_modules_wrapper~call_bom_where_used.

    CLEAR et_stpo_used.

    DATA: lt_equicat TYPE STANDARD TABLE OF cscequi,
          lt_kndcat  TYPE STANDARD TABLE OF cscknd,
          lt_matcat  TYPE STANDARD TABLE OF cscmat,
          lt_stdcat  TYPE STANDARD TABLE OF cscstd,
          lt_tplcat  TYPE STANDARD TABLE OF csctpl.

    CALL FUNCTION 'CS_WHERE_USED_MAT'
      EXPORTING
        datub                      = iv_valto
        datuv                      = iv_valfr
        matnr                      = iv_material
        stlan                      = iv_stlan
        werks                      = iv_plant
        stltp                      = 'M'
      TABLES
        wultb                      = et_stpo_used
        equicat                    = lt_equicat
        kndcat                     = lt_kndcat
        matcat                     = lt_matcat
        stdcat                     = lt_stdcat
        tplcat                     = lt_tplcat
      EXCEPTIONS
        call_invalid               = 1
        material_not_found         = 2
        no_where_used_rec_found    = 3
        no_where_used_rec_selected = 4
        no_where_used_rec_valid    = 5
        OTHERS                     = 6.

    IF sy-subrc <> 0.
      CLEAR et_stpo_used.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
