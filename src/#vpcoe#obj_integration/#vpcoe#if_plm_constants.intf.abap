interface /VPCOE/IF_PLM_CONSTANTS
  public .


  constants:
    BEGIN OF gc_rfc_plm_destinations ,
      pkg_compositions     TYPE rfcdest    VALUE 'VPCOE_RDP_PLM',
      pkg_elements         TYPE rfcdest    VALUE 'VPCOE_RDP_PLM_PKG_ELMNT',
    END OF gc_rfc_plm_destinations .
  constants GC_SCREEN_GRP1_M1 type CHAR3 value 'M1' ##NO_TEXT.
  constants GC_SCREEN_GRP1_M2 type CHAR3 value 'M2' ##NO_TEXT.
  constants GC_SCREEN_GRP1_M3 type CHAR3 value 'M3' ##NO_TEXT.
  constants GC_SCREEN_GRP1_M4 type CHAR3 value 'M4' ##NO_TEXT.
  constants GC_RETFLD_OBJID type FIELDNAME value 'OBJID' ##NO_TEXT.
  constants GC_RETFLD_SUBID type FIELDNAME value 'SUBID' ##NO_TEXT.
  constants GC_DYNPFLD_P_OBJID type DYNFNAM value 'P_HITOBJ' ##NO_TEXT.
  constants GC_DYNPFLD_P_SPECID type DYNFNAM value 'P_SPECID' ##NO_TEXT.
  constants GC_GRPID_HITLIST type ESEGRPID value 'HITLIST' ##NO_TEXT.
  constants GC_UCOMM_ONLI type SYST_UCOMM value 'ONLI' ##NO_TEXT.
  constants GC_RETFLD_RCPID type FIELDNAME value 'Recipe' ##NO_TEXT.
  constants GC_DYNPFLD_P_RCPID type DYNFNAM value 'P_RCPID' ##NO_TEXT.
  constants GC_RETFLD_RCPSTA type FIELDNAME value 'recipestatus' ##NO_TEXT.
  constants GC_DYNPFLD_P_RCPSTA type DYNFNAM value 'P_RCPSTA' ##NO_TEXT.
  constants GC_RCPSTAT_RELEASD type I value '200' ##NO_TEXT.
  constants GC_SUCCESS_RESPONSE type I value '204' ##NO_TEXT.
  constants GC_OK_RESPONSE type I value '200' ##NO_TEXT.
  constants:
    BEGIN OF gc_plm_reports,
      pckg_cmp_plm TYPE sy-repid VALUE '/VPCOE/R_UPH_PCKG_CMP_PLM_LOAD',
      pckg_cmp_rcp TYPE sy-repid VALUE '/VPCOE/R_UPH_PCKG_CMP_RCP_LOAD',
      pckg_data    TYPE sy-repid VALUE '/VPCOE/R_UPH_PCKG_DATA_LOAD',
   END OF  gc_plm_reports .
endinterface.
