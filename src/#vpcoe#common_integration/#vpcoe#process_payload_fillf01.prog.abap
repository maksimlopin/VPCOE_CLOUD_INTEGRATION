*----------------------------------------------------------------------*
***INCLUDE /VPCOE/PROCESS_PAYLOAD_FILLF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FILL_COLUMN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_column .

  CLEAR lv_fcat.
  lv_fcat-fieldname = 'API_TYPE'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 2.
  lv_fcat-coltext = 'Api_type'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  CLEAR lv_fcat.
  lv_fcat-fieldname = 'SERVICE_GROUP'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 3.
  lv_fcat-coltext = 'Service Group'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  CLEAR lv_fcat.
  lv_fcat-fieldname = 'SERVICE_ID'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 2.
  lv_fcat-coltext = 'Service Id'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  CLEAR lv_fcat.
  lv_fcat-fieldname = 'SESSION_ID'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 0.
  lv_fcat-coltext = 'Session Id'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  CLEAR lv_fcat.
  lv_fcat-fieldname = 'SESSION_ITEM'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 1.
  lv_fcat-coltext = 'Session Item'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  CLEAR lv_fcat.
  lv_fcat-fieldname = 'JSON'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 3.
  lv_fcat-coltext = 'Json'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  CLEAR lv_fcat.
  lv_fcat-fieldname = 'LINES_COUNT'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 6.
  lv_fcat-coltext = 'Lines Count'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  CLEAR lv_fcat.
  lv_fcat-fieldname = 'CREATED'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 7.
  lv_fcat-coltext = 'Created'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  CLEAR lv_fcat.
  lv_fcat-fieldname = 'CREATED_TIME'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 7.
  lv_fcat-coltext = 'Created Time'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  lv_fcat-fieldname = 'FAILED'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 7.
  lv_fcat-coltext = 'Failed'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  lv_fcat-fieldname = 'REPEATED'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 7.
  lv_fcat-coltext = 'Repeated'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  lv_fcat-fieldname = 'RESPONSE_STATUS'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 7.
  lv_fcat-coltext = 'Status'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

  lv_fcat-fieldname = 'RESPONSE_MESSAGE'.
  lv_fcat-tabname = '/VPCOE/JSN_CLOUD'.
  lv_fcat-col_pos = 7.
  lv_fcat-coltext = 'Reason'.
  INSERT lv_fcat INTO TABLE ls_fieldcatalog.

ENDFORM.
