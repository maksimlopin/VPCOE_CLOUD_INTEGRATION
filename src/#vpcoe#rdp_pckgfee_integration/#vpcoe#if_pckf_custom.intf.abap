interface /VPCOE/IF_PCKF_CUSTOM
  public .


  interfaces IF_BADI_INTERFACE .

  "! <p class="shorttext synchronized" lang="en"></p>
  "! Return the entity processor for the given entity.
  "! @parameter iv_entity_type | <p class="shorttext synchronized" lang="en">the entity type</p>
  "! @parameter ro_processor | <p class="shorttext synchronized" lang="en">entity processor implementation</p>
  methods GET_ENTITY_PROCESSOR
    importing
      !IV_ENTITY_TYPE type /VPCOE/PCKF_ENTITY
      !IV_UPLOAD_MODE type /VPCOE/UPLOAD_MODE
      !IS_PARAMETERS type ANY
    returning
      value(RO_PROCESSOR) type ref to /VPCOE/IF_PCKF_ENTITY_PROC .
  methods GET_TARGET_ENDPOINT_DEST
    exporting
      !EV_ENDPOINT_DEST type RFCDEST .
  methods DETERMINE_VERSION
    importing
      !IV_RFC_DEST type RFCDEST optional
    changing
      !CV_VERSION type I
      !CV_SKIP type ABAP_BOOL .
endinterface.
