*&---------------------------------------------------------------------*
*&  Include           /VPCOE/TRACE_MAINT_FILLF01
*&---------------------------------------------------------------------*
FORM build_fieldcatalog.
  CLEAR gt_fieldcat.

  DEFINE add_field.
    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = &1.
    gs_fieldcat-coltext   = &2.
    gs_fieldcat-outputlen = &3.
    APPEND gs_fieldcat TO gt_fieldcat.
  END-OF-DEFINITION.

  add_field: 'API_TYPE'         'API Type'      '4',
             'SERVICE_GROUP'    'Service Group' '10',
             'SERVICE_ID'       'Service ID'    '15',
             'OBJECT_ID'        'Object ID'     '20',
             'STATUS'           'Status'        '10',
             'RESPONSE_MESSAGE' 'Response'      '10',
             'LAST_SENDED_DATE' 'Date'          '10',
             'LAST_SENDED_TIME' 'Time'          '10'.
ENDFORM.
