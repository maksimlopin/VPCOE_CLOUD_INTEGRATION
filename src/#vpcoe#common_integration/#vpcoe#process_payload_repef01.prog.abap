*----------------------------------------------------------------------*
***INCLUDE /VPCOE/PROCESS_PAYLOAD_REPEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  REPEAT_REQEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM repeat_reqest .

  DATA: lo_log TYPE REF TO /vpcoe/cl_log,
        ls_log TYPE /vpcoe/s_bal_log.

  IF lines( lt_jsn_cloud_2 ) < 1.
    MESSAGE i005(/vpcoe/common).
  ELSE.
    LOOP AT lt_jsn_cloud_2 ASSIGNING FIELD-SYMBOL(<ls_jsn_cloud_2>).
      lo_cust = NEW /vpcoe/cl_rdp_custom_helper( iv_api_type = <ls_jsn_cloud_2>-api_type
                                                  iv_srv_grp  = <ls_jsn_cloud_2>-service_group
                                                  iv_srv_id   = <ls_jsn_cloud_2>-service_id ).

      /vpcoe/cl_log=>get_log_by_srv_group( EXPORTING iv_service_group = <ls_jsn_cloud_2>-service_group
                                           IMPORTING eo_log = lo_log ).

      CALL FUNCTION '/VPCOE/SEND_JSON_BCKGRND'
        EXPORTING
          iv_rfc_name    = lo_cust->get_generic_rfc_name( )
          iv_json        = <ls_jsn_cloud_2>-json
          iv_lines_count = <ls_jsn_cloud_2>-lines_count
          io_log         = lo_log.

      ls_log-bal_log = lo_log.
      INSERT ls_log INTO TABLE lt_logs.
    ENDLOOP.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.
