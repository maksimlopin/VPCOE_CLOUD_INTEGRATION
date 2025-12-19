*&---------------------------------------------------------------------*
*&  Include           /VPCOE/REPROCESS_PAYLOAD_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK srvid WITH FRAME TITLE text-004.
SELECT-OPTIONS:
  so_api  FOR /vpcoe/rdp_srvid-api_type,                           " Api Type
  so_srvg FOR /vpcoe/rdp_srvid-service_grp,                        " Service group
  so_srvi FOR /vpcoe/rdp_srvid-service_id,                         " Service id
  so_id   FOR lv_session_id MODIF ID mrs,                          " Session id
  so_itm  FOR lv_session_item,                                     " Session Item
  so_date FOR lv_date,                                             " Created date
  so_time FOR lv_time.                                             " Created time
SELECTION-SCREEN END OF BLOCK srvid.

SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE text-007 .
PARAMETERS:
  p_manu RADIOBUTTON GROUP mode DEFAULT 'X' USER-COMMAND mode,
  p_auto RADIOBUTTON GROUP mode,
  p_dele RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK mode.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'TRL'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_id-low.
  ls_interface = VALUE #( ( shlpfield = 'SESSION_ID' valfield = 'SO_ID-LOW' f4field = 'X' ) ).
  so_id-low = lcl_payload_handler=>set_search_help( ir_t_api     = so_api[]
                                                    ir_t_srv_grp = so_srvg[]
                                                    ir_t_srv_id  = so_srvi[]
                                                    is_interface = ls_interface ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_id-high.
  ls_interface = VALUE #( ( shlpfield = 'SESSION_ID' valfield = 'SO_ID-HIGH' f4field = 'X' ) ).
  so_id-high = lcl_payload_handler=>set_search_help( ir_t_api     = so_api[]
                                                     ir_t_srv_grp = so_srvg[]
                                                     ir_t_srv_id  = so_srvi[]
                                                     is_interface = ls_interface  ).

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/REPROCESS' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.
