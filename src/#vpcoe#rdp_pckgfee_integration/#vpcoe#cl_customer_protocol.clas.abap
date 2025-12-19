class /VPCOE/CL_CUSTOMER_PROTOCOL definition
  public
  create public .

public section.

  interfaces /VPCOE/IF_PCKF_PROTOCOL .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /VPCOE/CL_CUSTOMER_PROTOCOL IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_protocol~delete_from_protocol.

    IF it_protocol IS NOT INITIAL.
      DELETE /vpcoe/cstm_prot FROM TABLE it_protocol.
    ELSE.
      DELETE FROM /vpcoe/cstm_prot WHERE entity_type = iv_entity_type.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_pckf_protocol~read_from_protocol.

    DATA: lt_protocol TYPE /vpcoe/t_pckf_prot.

    CLEAR rt_protocol.

    " Get the previous successful input parameters from the Protocol table
    SELECT  mandt, uuid, entity_type, upload_mode, start_timestamp, end_timestamp, failed, selection
     FROM /vpcoe/cstm_prot
     WHERE entity_type = @iv_entity_type
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
    DATA lt_messages TYPE /vpcoe/t_uph_msg.
    DATA(ls_protocol) = is_protocol.
    CONCATENATE sy-datum sy-uzeit INTO DATA(lv_timestamp).

    ls_protocol-end_timestamp = lv_timestamp .
    ls_protocol-upload_mode = iv_upload_mode.
    ls_protocol-entity_type = iv_entity_type.

    IF is_protocol-uuid IS INITIAL.
      ls_protocol-uuid = /vpcoe/cl_common_helper=>generate_session_id( ).
    ENDIF.

    " Serialize the parameters
    ls_protocol-selection = /vpcoe/cl_common_helper=>serialize_json(
       is_data        = is_selection_params
       iv_compress    = abap_true
       iv_pretty_name = 'L' ) .

    " Insert protocol table
    INSERT /vpcoe/cstm_prot FROM ls_protocol.

  ENDMETHOD.
ENDCLASS.
