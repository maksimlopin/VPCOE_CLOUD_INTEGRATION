interface /VPCOE/IF_BADI_RDP_MAT_DOC
  public .


  interfaces IF_BADI_INTERFACE .

  methods DEFINE_COUNTRIES
    importing
      !IV_CODE type /VPCOE/DE_MAT_DOC_VARIANTS
    changing
      !CT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
endinterface.
