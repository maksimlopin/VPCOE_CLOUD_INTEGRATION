interface /VPCOE/IF_NOTIFICTN_PROCESSING
  public .


  interfaces IF_BADI_INTERFACE .

  methods NOTIFICATION_CHANGE_BODY default ignore
    importing
      !IS_PAYLOAD type /VPCOE/S_JSN_CLOUD optional
      !IO_LOG type ref to /VPCOE/CL_rdp_LOG optional
    changing
      !CT_MESSAGE type BCSY_TEXT
      !CV_SUBJECT type SO_OBJ_DES .
  methods NOTIFICATION_CHANGE_SENDER default ignore
    importing
      !IS_PAYLOAD type /VPCOE/S_JSN_CLOUD optional
    changing
      !CV_SENDER type ADR6-SMTP_ADDR .
  methods NOTIFICATION_OVERWRITE default ignore
    importing
      !IS_PAYLOAD type /VPCOE/S_JSN_CLOUD
      !IO_LOG type ref to /VPCOE/CL_rdp_LOG optional
    changing
      !CV_SKIP type ABAP_BOOL default ABAP_FALSE .
endinterface.
