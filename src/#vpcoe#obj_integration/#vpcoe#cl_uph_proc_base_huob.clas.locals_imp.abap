*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

**INTERFACE lif_ext_modules_wrapper.
**
**  TYPES:
**    ltty_stpox  TYPE STANDARD TABLE OF stpox,
**    ltty_cscmat TYPE STANDARD TABLE OF cscmat.
**
**  METHODS:
**
**    get_material_detail
**      IMPORTING
**        !iv_material          TYPE matnr
**        !iv_plant             TYPE werks_d
**        !io_logger            TYPE REF TO /vpcoe/if_uph_logger
**      EXPORTING
**        es_material_general   TYPE bapimatdoa
**        es_material_plant     TYPE bapimatdoc
**        es_material_valuation TYPE bapimatdobew,
**
**    call_bom_explosion
**      IMPORTING
**        !ir_bom_key      TYPE REF TO /vpcoe/surdps_uph_bom_key
**        !is_parameters   TYPE /vpcoe/s_pckg_bom_input
**        !io_logger       TYPE REF TO /vpcoe/if_uph_logger
**      EXPORTING
**        es_exp_bom_hdr   TYPE cstmat
**        et_exp_bom_items TYPE ltty_stpox
**        et_exp_bom_nodes TYPE ltty_cscmat.
**
**ENDINTERFACE.
**
**CLASS lcl_bom_expl_wrapper DEFINITION FINAL CREATE PUBLIC.
**
**  PUBLIC SECTION.
**
**    INTERFACES: lif_ext_modules_wrapper.
**
**ENDCLASS.
**
**CLASS lcl_bom_expl_wrapper IMPLEMENTATION.
*** Wrapper implementation to encapsulate dependent module calls
**
**  METHOD lif_ext_modules_wrapper~get_material_detail.
**
**    CLEAR: es_material_general, es_material_plant, es_material_valuation.
**
**    DATA: ls_bapireturn TYPE bapireturn.
**
**    CHECK: iv_material IS NOT INITIAL, iv_plant IS NOT INITIAL.
**
**    CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
**      EXPORTING
**        plant                 = iv_plant
**        material              = iv_material
**      IMPORTING
**        return                = ls_bapireturn
**        material_general_data = es_material_general
**        materialplantdata     = es_material_plant
**        materialvaluationdata = es_material_valuation.
**
**    IF ls_bapireturn IS NOT INITIAL AND ls_bapireturn-type <> 'S'.
**      io_logger->add_bapi_messages( VALUE #( ( CORRESPONDING #( ls_bapireturn ) ) ) ).
**    ENDIF.
**
**  ENDMETHOD.
**
**  METHOD lif_ext_modules_wrapper~call_bom_explosion.
**
**    CLEAR: es_exp_bom_hdr, et_exp_bom_items, et_exp_bom_nodes.
**
**    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
**      EXPORTING
**        capid                 = /vpcoe/if_uph_entity_bom_proc=>gc_capid_mat_bom
**        datuv                 = ir_bom_key->valfrdats
**        mtnrv                 = ir_bom_key->material
**        werks                 = ir_bom_key->plant
**        mehrs                 = abap_true
**        stpst                 = is_parameters-max_exp_lvl
**        stlal                 = ir_bom_key->stlal
**        stlan                 = ir_bom_key->stlan
***        bom_versn             = ir_bom_key->bom_versn
**      IMPORTING
**        topmat                = es_exp_bom_hdr
**      TABLES
**        stb                   = et_exp_bom_items
**        matcat                = et_exp_bom_nodes
**      EXCEPTIONS
**        alt_not_found         = 1
**        call_invalid          = 2
**        material_not_found    = 3
**        missing_authorization = 4
**        no_bom_found          = 5
**        no_plant_data         = 6
**        no_suitable_bom_found = 7
**        conversion_error      = 8
**        OTHERS                = 9.


*    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*      EXPORTING
*        capid                 = /vpcoe/if_uph_entity_bom_proc=>gc_capid_mat_bom
*        datuv                 = ir_bom_key->valfrdats
*        mtnrv                 = ir_bom_key->material
*        werks                 = ir_bom_key->plant
*        mehrs                 = abap_true
*        stpst                 = is_parameters-max_exp_lvl
*        stlal                 = ir_bom_key->stlal
*        stlan                 = ir_bom_key->stlan
**        bom_versn             = ir_bom_key->bom_versn
*      IMPORTING
*        topmat                = es_exp_bom_hdr
*      TABLES
*        stb                   = et_exp_bom_items
*        matcat                = et_exp_bom_nodes
*      EXCEPTIONS
*        alt_not_found         = 1
*        call_invalid          = 2
*        material_not_found    = 3
*        missing_authorization = 4
*        no_bom_found          = 5
*        no_plant_data         = 6
*        no_suitable_bom_found = 7
*        conversion_error      = 8
*        OTHERS                = 9.

**    IF sy-subrc <> 0.
**      io_logger->add_messages( VALUE #(
**          ( msgty = 'E' msgid = sy-msgid msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ) ) ).
**    ENDIF.

*  ENDMETHOD.

*ENDCLASS.
