CLASS /vpcoe/cl_pckf_proc_pckg_fee DEFINITION
  PUBLIC
  INHERITING FROM /vpcoe/cl_pckf_proc_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.

    METHODS /vpcoe/if_pckf_entity_proc~deserialize_selection_params
         REDEFINITION .
    METHODS /vpcoe/if_pckf_entity_proc~prepare_retrieve
         REDEFINITION .

  PROTECTED SECTION.

    DATA:
          mo_matclass_dac TYPE REF TO /vpcoe/if_pckf_matclass_dac.

    METHODS: prepare_query_parameter REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /VPCOE/CL_PCKF_PROC_PCKG_FEE IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_entity_proc~deserialize_selection_params.
    CLEAR es_selection_params.

    DATA ls_selection_params TYPE /vpcoe/s_pckf_retrieval_input.

    /vpcoe/cl_common_helper=>deserialize_json(
      EXPORTING
        iv_json = iv_json_str
      CHANGING
        cs_data = ls_selection_params ).

    es_selection_params = ls_selection_params.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_entity_proc~prepare_retrieve.      "#EC CI_NOES

    "This preparation method is used to determine stored parameters in case of a delta load after a
    "successful full load

    DATA : lv_params_string         TYPE string,
           ls_input_params_frm_prot TYPE /vpcoe/s_pckf_retrieval_input,
           lt_messages              TYPE /vpcoe/t_uph_msg,
           lr_protocol_delta        TYPE REF TO /vpcoe/pckf_prot,
           lr_protocol_selection    TYPE REF TO /vpcoe/pckf_prot,
           lv_load_id               TYPE /vpcoe/load_id.

    CLEAR: mv_prepared, lv_load_id.

    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_param_exemption-load_id
                                                       IMPORTING ev_value = lv_load_id ).

    " When the Delta load then get the last successful full load selection screen parameters from Protocol
    IF mv_upload_mode EQ /vpcoe/if_pckf_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
      "retrieve last protocol entries
      DATA(lo_protocol_access) = /vpcoe/cl_pckf_factory=>get_instance( )->get_protocol_access( iv_upload_entity = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).

      DATA(lt_protocol) = lo_protocol_access->read_from_protocol(
        iv_entity_type = mv_entity_type
        iv_only_success  = abap_true
        iv_load_id = lv_load_id
        ).

      SORT lt_protocol BY start_timestamp DESCENDING.

      "get last successful protocol entry, might be a full or delta load: relevant for delta timestamp
*      READ TABLE lt_protocol INDEX 1 REFERENCE INTO lr_protocol_delta.

      "get last successful full load protocol: relevant for selection
      READ TABLE lt_protocol REFERENCE INTO lr_protocol_selection WITH KEY upload_mode = /vpcoe/if_pckf_entity_proc~gc_upload_mode-gc_upload_mode_full.

      IF lr_protocol_selection IS INITIAL.

        MESSAGE e053(/vpcoe/common).
        APPEND VALUE #( msgty = sy-msgty msgid = sy-msgid
                                         msgno = sy-msgno
                                         msgv1 = sy-msgv1
                                         msgv2 = sy-msgv2 ) TO lt_messages.
        mo_logger->add_messages( it_messages = lt_messages ).
        RETURN.

      ELSE.

        "deserialize parameter
*        IF lv_load_id IS INITIAL.
          lv_params_string = lr_protocol_selection->selection.
*        ELSE.
*          SELECT SINGLE delta_token
*             INTO @lv_params_string
*             FROM /vpcoe/pckfvar
*             WHERE load_id = @lv_load_id.
*        ENDIF.
        /vpcoe/if_pckf_entity_proc~deserialize_selection_params( EXPORTING iv_json_str = lv_params_string IMPORTING es_selection_params = ls_input_params_frm_prot ).
        "create a modifiable parameter reference
        DATA lr_parameters TYPE REF TO /vpcoe/s_pckf_retrieval_input.
        CREATE DATA lr_parameters.
        MOVE-CORRESPONDING ls_input_params_frm_prot TO lr_parameters->*.
        mr_parameters = lr_parameters.
      ENDIF.
    ENDIF.

    "return record count and set as prepared
    rv_record_cnt = -1.

    mv_prepared = abap_true.

  ENDMETHOD.                      "#EC CI_NOES            "#EC CI_CYCLO


  METHOD constructor.
    super->constructor( ).

    mo_matclass_dac = /vpcoe/cl_pckf_factory=>get_instance(  )->get_matclass_access( ).

  ENDMETHOD.


  METHOD prepare_query_parameter.

    DATA: lv_criteria   TYPE string,
          lt_context    TYPE /vpcoe/t_r_context,
          lv_proc_drctn TYPE /vpcoe/pckf_proc_drctn.

    CLEAR: ev_query_string, ev_uri_suffix, ev_query_finalized.

    "Prepare query string with default parameters (initial / delta load, report configuration)
    super->prepare_query_parameter(
      IMPORTING
        ev_uri_suffix   = ev_uri_suffix
        ev_query_string = ev_query_string
        ev_query_finalized = ev_query_finalized
    ).

    IF ev_query_finalized = abap_true.
      RETURN.
    ENDIF.

    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-context IMPORTING ev_value = lt_context ).
    IF lt_context IS NOT INITIAL.

      IF ev_query_string IS NOT INITIAL.
        ev_query_string = |{ ev_query_string }&|.
      ENDIF.

      CLEAR lv_criteria.
      LOOP AT lt_context REFERENCE INTO DATA(lr_context).
        IF lr_context->low IS NOT INITIAL.
          IF lv_criteria IS NOT INITIAL.
            lv_criteria = |{ lv_criteria },|.
          ENDIF.
          lv_criteria = |{ lv_criteria }{ lr_context->low }|.
        ENDIF.
      ENDLOOP.
      ev_query_string = |{ ev_query_string }Context={ cl_http_client=>escape_url( lv_criteria ) }|.
    ENDIF.

    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-proc_drctn IMPORTING ev_value = lv_proc_drctn ).
    IF lv_proc_drctn IS NOT INITIAL.
      IF ev_query_string IS NOT INITIAL.
        ev_query_string = |{ ev_query_string }&|.
      ENDIF.
      lv_criteria = lv_proc_drctn.
      ev_query_string = |{ ev_query_string }BusinessProcessDirection={ cl_http_client=>escape_url( lv_criteria ) }|.
    ENDIF.

    IF ev_query_string IS NOT INITIAL.
      ev_query_string = |{ ev_query_string }&|.
    ENDIF.
    ev_query_string = |{ ev_query_string }$expand=PackagingFeeExtension,Suppliers,CONAIPackagingFeeExtension|.

    ev_query_finalized = abap_true.

  ENDMETHOD.
ENDCLASS.
