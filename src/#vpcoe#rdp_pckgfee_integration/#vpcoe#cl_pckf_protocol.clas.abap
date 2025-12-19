CLASS /vpcoe/cl_pckf_protocol DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /vpcoe/if_pckf_protocol .

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /VPCOE/CL_PCKF_PROTOCOL IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_protocol~delete_from_protocol.

    IF it_protocol IS NOT INITIAL.
      DELETE /vpcoe/pckf_prot FROM TABLE it_protocol.
    ELSE.
      DELETE FROM /vpcoe/pckf_prot WHERE entity_type = iv_entity_type.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_protocol~read_from_protocol.

    DATA: lt_protocol TYPE /vpcoe/t_pckf_prot.

    CLEAR rt_protocol.

    " Get the previous successful input parameters from the Protocol table
    SELECT  mandt, uuid ,entity_type, upload_mode, start_timestamp, end_timestamp, failed, selection ,delta_token , load_id
     FROM /vpcoe/pckf_prot
     WHERE entity_type = @iv_entity_type AND
           load_id = @iv_load_id
     ORDER BY start_timestamp DESCENDING
     INTO TABLE @lt_protocol.

    LOOP AT lt_protocol REFERENCE INTO DATA(lr_protocol).
      IF iv_upload_mode IS NOT INITIAL AND lr_protocol->upload_mode NE iv_upload_mode.
        CONTINUE.
      ENDIF.

      IF iv_only_success EQ abap_true AND lr_protocol->failed = abap_true.
        CONTINUE.
      ENDIF.

      APPEND lr_protocol->* TO rt_protocol.
    ENDLOOP.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_protocol~write_to_protocol.
    DATA :lt_messages         TYPE /vpcoe/t_uph_msg,
          ls_selection_params TYPE /vpcoe/s_pckf_retrieval_input.

    DATA(ls_protocol) = is_protocol.
    ls_selection_params = is_selection_params.
    CONCATENATE sy-datum sy-uzeit INTO DATA(lv_timestamp).
    ls_protocol-end_timestamp = lv_timestamp .
    ls_protocol-upload_mode = iv_upload_mode.
    ls_protocol-entity_type = iv_entity_type.
    ls_protocol-load_id = ls_selection_params-initial_load_id.
    ls_protocol-delta_token = ls_selection_params-delta_link.
    IF is_protocol-uuid IS INITIAL.
      ls_protocol-uuid = /vpcoe/cl_common_helper=>generate_session_id( ).
    ENDIF.


    " Serialize the parameters
    ls_protocol-selection = /vpcoe/cl_common_helper=>serialize_json(
       is_data        = is_selection_params
       iv_compress    = abap_true
       iv_pretty_name = 'L' ) .

    SELECT SINGLE uuid,load_id,delta_token , selection INTO @DATA(ls_pckf_prot)
      FROM /vpcoe/pckf_prot WHERE load_id = @ls_selection_params-initial_load_id.
    IF sy-subrc <> 0.
      " Insert protocol table
      INSERT /vpcoe/pckf_prot FROM ls_protocol.
    ELSE.
      UPDATE /vpcoe/pckf_prot
      SET uuid = @ls_protocol-uuid,
          delta_token = @ls_protocol-delta_token,
          selection = @ls_protocol-selection,
          end_timestamp = @ls_protocol-end_timestamp,
          entity_type = @ls_protocol-entity_type,
          failed      = @ls_protocol-failed
          WHERE uuid = @ls_pckf_prot-uuid AND
                load_id = @ls_pckf_prot-load_id.

    ENDIF.


  ENDMETHOD.
ENDCLASS.
