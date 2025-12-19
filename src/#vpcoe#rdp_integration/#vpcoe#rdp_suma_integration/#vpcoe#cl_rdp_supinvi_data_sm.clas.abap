class /VPCOE/CL_RDP_SUPINVI_DATA_SM definition
  public
  inheriting from /VPCOE/CL_RDP_SUPINVI_DATA
  final
  create public .

public section.

  methods BUILD_JSON
    redefinition .
protected section.
PRIVATE SECTION.

  TYPES:
    BEGIN OF gty_s_supp_inv_sm_json,
      replication_run_id TYPE  /vpcoe/de_run_id,
      elements           TYPE /vpcoe/t_supinvi_data,
    END OF gty_s_supp_inv_sm_json .

  DATA mo_suma_helper TYPE REF TO /vpcoe/cl_rdp_suma_helper .
ENDCLASS.



CLASS /VPCOE/CL_RDP_SUPINVI_DATA_SM IMPLEMENTATION.


  METHOD build_json.
*    DATA: lo_badi        TYPE REF TO /vpcoe/adjust_data_retrieval,
*          ls_source_data TYPE gty_s_supp_inv_sm_json.
*
*    me->mo_suma_helper->start_replication( EXPORTING io_log = mo_log ).
*
*    ls_source_data-replication_run_id = me->mo_suma_helper->get_current_run_id( ).
*    IF ls_source_data-replication_run_id IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    APPEND INITIAL LINE TO et_json ASSIGNING FIELD-SYMBOL(<ls_json>).
*    <ls_json>-count = lines( it_supinvi ).
*    DATA(lo_writer_json) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
*    CALL TRANSFORMATION /vpcoe/rdp_sup_inv_sm_to_json SOURCE root = ls_source_data RESULT XML lo_writer_json.
*    <ls_json>-elements = cl_abap_codepage=>convert_from( lo_writer_json->get_output( ) ).
*
*    REPLACE ALL OCCURRENCES OF ':""' IN <ls_json>-elements WITH ': null' ##no_text.
*
*    me->mv_session_item = me->mv_session_item + 1.
*
*    GET BADI lo_badi.
*    DATA(lv_srv_grp) = me->mo_cust->get_srv_grp( ).
*    DATA(lv_srvid) = me->mo_cust->get_srv_id( ).
*    CALL BADI lo_badi->adjust_json
*      EXPORTING
*        iv_srv_grp  = lv_srv_grp
*        iv_srv_id   = lv_srvid
*        iv_api_type = me->mv_api_type
*        iv_level    = /vpcoe/cl_common_helper=>sc_level-mtrl_suppinv
*      CHANGING
*        ct_json     = et_json.
*
*    CALL FUNCTION '/VPCOE/STORE_JSON_BCKGRND'
*      EXPORTING
*        iv_api_type     = me->mv_api_type
*        iv_srv_grp      = me->mo_cust->get_srv_grp( )
*        iv_srv_id       = me->mo_cust->get_srv_id( )
*        iv_session_id   = me->mv_session_id
*        iv_session_item = me->mv_session_item
*        is_json         = <ls_json>.
  ENDMETHOD.
ENDCLASS.
