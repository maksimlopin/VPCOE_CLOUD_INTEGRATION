interface /VPCOE/IF_UPH_CUSTOM
  public .


  interfaces IF_BADI_INTERFACE .

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Return the entity processor for the given upload entity.
  "! @parameter iv_upload_entity | <p class="shorttext synchronized" lang="en">the upload entity</p>
  "! @parameter ro_processor | <p class="shorttext synchronized" lang="en">entity processor implementation</p>
  methods GET_ENTITY_PROCESSOR DEFAULT IGNORE
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IS_PARAMETERS type ANY
    returning
      value(RO_PROCESSOR) type ref to /VPCOE/IF_UPH_ENTITY_PROC .
  methods GET_TARGET_ENDPOINT_DEST DEFAULT IGNORE
    exporting
      !EV_ENDPOINT_DEST type RFCDEST .
  methods GET_ENTITY_MAPPER DEFAULT IGNORE
    importing
      !IV_UPLOAD_ENTITY type /VPCOE/UPLOAD_ENTITY
    returning
      value(RO_MAPPER) type ref to /VPCOE/IF_UPH_TRANSFER_MAPPER .
endinterface.
