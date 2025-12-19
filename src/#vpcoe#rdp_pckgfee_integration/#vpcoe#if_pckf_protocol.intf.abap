interface /VPCOE/IF_PCKF_PROTOCOL
  public .


  "! Once the Transfer process is successful then write all the selection screen parameters along with timestamp,
  "! upload mode and upload entity to the protocol table.
  methods WRITE_TO_PROTOCOL
    importing
      !IV_ENTITY_TYPE type /VPCOE/PCKF_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IS_PROTOCOL type /VPCOE/PCKF_PROT
      !IS_SELECTION_PARAMS type ANY optional .
  "! Get the previous successful input parameters from the Protocol table if the upload entity is same as the
  "! current one and discard the failed uploads and delta uploads.
  methods READ_FROM_PROTOCOL
    importing
      !IV_ENTITY_TYPE type /VPCOE/PCKF_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE optional
      !IV_ONLY_SUCCESS type ABAP_BOOL optional
      !IV_LOAD_ID type /VPCOE/LOAD_ID optional
    returning
      value(RT_PROTOCOL) type /VPCOE/T_PCKF_PROT .
  "! Delete the protocol table entries if necessary.
  methods DELETE_FROM_PROTOCOL
    importing
      !IV_ENTITY_TYPE type /VPCOE/PCKF_ENTITY
      !IT_PROTOCOL type /VPCOE/T_PCKF_PROT optional .
endinterface.
