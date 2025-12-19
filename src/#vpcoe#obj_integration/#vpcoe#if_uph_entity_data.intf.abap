interface /VPCOE/IF_UPH_ENTITY_DATA
  public .


  methods IS_MARKED_DELETED
    returning
      value(RV_MARK_DELETED) type ABAP_BOOL .
  methods TO_CONSOLE .
endinterface.
