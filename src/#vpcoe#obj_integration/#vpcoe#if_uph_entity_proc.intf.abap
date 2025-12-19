interface /VPCOE/IF_UPH_ENTITY_PROC
  public .


  constants:
    BEGIN OF gc_upload_entities,
      gc_ue_pckg_plm TYPE /vpcoe/upload_entity VALUE 'PE_PLM',
      gc_ue_pc_plm   TYPE /vpcoe/upload_entity VALUE 'PC_PLM',
      gc_ue_pc_rcp   TYPE /vpcoe/upload_entity VALUE 'PC_RCP',
      gc_ue_pc_bom   TYPE /vpcoe/upload_entity VALUE 'PC_BOM',
      gc_ue_pe_mcl   TYPE /vpcoe/upload_entity VALUE 'PE_MCL',
      gc_ue_pc_hu    TYPE /vpcoe/upload_entity VALUE 'PC_HU',
      gc_ue_pc_gprd  type /vpcoe/upload_entity VALUE 'PC_GPRD',
    END OF gc_upload_entities .
  constants:
    BEGIN OF gc_upload_mode,
      gc_upload_mode_full  TYPE /vpcoe/upload_mode VALUE 'F',
      gc_upload_mode_delta TYPE /vpcoe/upload_mode VALUE 'D',
    END OF gc_upload_mode .
  constants GC_DATE_BOT type DATUM value '00010101' ##NO_TEXT.
  constants:
    "!The upload target APIs defined in the system:
    BEGIN OF gc_upload_target,
      gc_upload_target_rdp TYPE /vpcoe/upload_target VALUE 'RDP',
      gc_upload_target_sdi TYPE /vpcoe/upload_target VALUE 'SDI',
    END OF gc_upload_target .

  methods INIT_PROCESSOR default ignore
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IS_PARAMETERS type ANY optional .
  methods GET_UPLOAD_ENTITY default ignore
    returning
      value(RV_UPLOAD_ENTITY) type /VPCOE/UPLOAD_ENTITY .
  methods GET_UPLOAD_MODE
    returning
      value(RV_UPLOAD_MODE) type /VPCOE/UPLOAD_MODE .
  methods GET_PARAMETERS default ignore
    returning
      value(RV_RESULT) type ref to DATA .
  methods PREPARE_PROCESS default ignore
    returning
      value(RV_RECORD_CNT) type I .
  methods PROCESS_PACKAGE default ignore
    importing
      !IV_ACT_PACKAGE type I optional
      !IV_PACKAGE_SIZE type I optional
    returning
      value(RT_ENTITY_DATA) type /VPCOE/T_UPH_ENTITY_DATA .
  methods TRANSFER_PACKAGE default ignore
    importing
      !IT_ENTITY_DATA type /VPCOE/T_UPH_ENTITY_DATA
      !IS_PARAMETERS type ANY optional
      !IV_FINAL_TRANSFER type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_ERROR_FLG) type BOOLE_D .
  methods WRITE_TO_PROTOCOL default ignore
    importing
      !IS_PROTOCOL type /VPCOE/UPH_PROT
      !IS_SELECTION_PARAMS type ANY optional .
  methods READ_FROM_PROTOCOL default ignore
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE optional
      !IV_ONLY_SUCCESS type BOOLE_D optional
    returning
      value(RT_PROTOCOL) type /VPCOE/T_UPH_PROT .
  methods DELETE_FROM_PROTOCOL default ignore
    importing
      !IT_PROTOCOL type /VPCOE/T_UPH_PROT optional .
  methods DESERIALIZE_SELECTION_PARAMS default ignore
    importing
      !IV_JSON_STR type STRING
    exporting
      !ES_SELECTION_PARAMS type ANY .
  methods FITS_PARAM_SIZE_INTO_PROTOCOL default ignore
    importing
      !IS_SELECTION_PARAMS type ANY
    returning
      value(RV_RESULT) type ABAP_BOOL .
endinterface.
