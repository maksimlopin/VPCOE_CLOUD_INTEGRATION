*----------------------------------------------------------------------*
***INCLUDE /VPCOE/PROCESS_PAYLOAD_DISPF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PAYLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_payload .

  IF lines( lt_jsn_cloud_2 ) > 1.
    MESSAGE i006(/vpcoe/common).
  ELSEIF lines( lt_jsn_cloud_2 ) < 1.
    MESSAGE i005(/vpcoe/common).
  ELSE.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Payload'
        txt1  = `JSON:`
        txt2  = <ls_jsn_cloud>-json.
  ENDIF.

ENDFORM.
