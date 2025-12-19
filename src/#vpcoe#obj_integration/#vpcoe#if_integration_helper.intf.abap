interface /VPCOE/IF_INTEGRATION_HELPER
  public .


  methods GET_REMOTE_TABLE_CONTENT
    importing
      !IV_TABLENAME type TABNAME
      !IV_STRUCT_TABLENAME type TABNAME optional
      !IV_RFC_DESTINATION type RFC_DEST optional
      !IT_SELOPT type /VPCOE/T_SEL_OPTIONS optional
      !IV_ROWCOUNT type I optional
      !IV_ROWSKIPS type I optional
    changing
      !CT_FIELDS type /VPCOE/T_DB_FIELDS optional
      !CT_CONTENT type STANDARD TABLE .
endinterface.
