interface /VPCOE/IF_UPH_ENTITY_BOM_PROC
  public .


  constants GC_STLTY_MAT_BOM type STLTY value 'M' ##NO_TEXT.
  constants GC_CAPID_MAT_BOM type CAPID value 'PP01' ##NO_TEXT.

  "! Retrieve data from BOM based on the selection screen parameters.
  methods RETRIEVE_BOM_DATA default ignore
    importing
      !IT_BOM_KEY type /VPCOE/T_UPH_BOM_KEY
    returning
      value(RT_BOM_DATA) type /VPCOE/T_UPH_WRAP_BOM_HDR .
  "! All mappings between BOM data and the target Entity Data (Packaging Composition/Element, Packaging Fraction, ...) must be implemented in this method.
  methods MAP_BOM_DATA default ignore
    importing
      !IT_BOM_DATA type /VPCOE/T_UPH_WRAP_BOM_HDR
    returning
      value(RT_ENTITY_DATA) type /VPCOE/UPH_ENTITY_DATA .
endinterface.
