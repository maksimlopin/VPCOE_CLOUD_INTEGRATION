*&---------------------------------------------------------------------*
*& Include surdp_uph_pckg_bom_sel_screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME  TITLE text-005.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME  TITLE text-005.

"Full or Delta mode radiobuttons
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11) text-002 FOR FIELD p_iload .
PARAMETERS : p_iload TYPE char1 RADIOBUTTON GROUP r2 DEFAULT 'X' USER-COMMAND fcode1 .
SELECTION-SCREEN COMMENT 31(10) text-003 FOR FIELD p_dload .
PARAMETERS : p_dload TYPE char1 RADIOBUTTON GROUP r2 .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

"Test mode
PARAMETERS p_test TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.
PARAMETERS: p_varn TYPE /vpcoe/load_id MODIF ID m5.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) text-006 FOR FIELD p_varn MODIF ID m4.
PARAMETERS: p_dvar TYPE char32 AS LISTBOX VISIBLE LENGTH 32 USER-COMMAND fdvar MODIF ID m4.
SELECTION-SCREEN PUSHBUTTON 68(15) text-007 USER-COMMAND fdelv MODIF ID m4." VISIBLE LENGTH 10.
SELECTION-SCREEN END OF LINE.

PARAMETERS:
  p_source TYPE /vpcoe/uph_source_id LOWER CASE VISIBLE LENGTH 36 MODIF ID m2,
  p_rfcdes TYPE /vpcoe/uph_rfcdest VISIBLE LENGTH 16 DEFAULT /vpcoe/if_uph_pcg_cmp_bom_load=>gc_default_rfc_destination MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b3  .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_mat     FOR lv_material       MODIF ID m1.
SELECT-OPTIONS: s_mattyp  FOR lv_material_type  MODIF ID m1 MATCHCODE OBJECT h_t134.
SELECT-OPTIONS: s_plant   FOR lv_plant          MODIF ID m1.
SELECT-OPTIONS: s_usage   FOR lv_bom_usage      MODIF ID m1 MATCHCODE OBJECT h_t416.
SELECT-OPTIONS: s_altbom  FOR lv_alt_bom        MODIF ID m1.
PARAMETERS:     p_bomst   TYPE stlst            MODIF ID m1 MATCHCODE OBJECT h_t415s.
PARAMETERS:     p_valdat  TYPE sy-datum         MODIF ID m3 DEFAULT '99991231'.  "BOM validity date
SELECT-OPTIONS: s_chdate  FOR sy-datum          MODIF ID m1.                   "Change date of BOM .
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
PARAMETERS:     p_explvl  TYPE cs_maxst         MODIF ID m1 DEFAULT 10.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p_iload IS NOT INITIAL.
      CASE screen-group1.
        WHEN gc_screen_grp1_m1 .
          screen-input = 1.
        WHEN 'M4'.
          screen-input = 0.
          screen-active = 0.
        WHEN 'M5'.
          screen-input = 1.
      ENDCASE.
    ELSEIF p_dload IS NOT INITIAL.
      IF screen-group1 EQ gc_screen_grp1_m1 OR
         screen-group1 EQ gc_screen_grp1_m2.
        screen-input = 0.
*        screen-active = 0.
      ENDIF.
      IF screen-group1 = 'M5'.
        screen-input = 0.
        screen-active = 0.
      ENDIF.
    ENDIF.
    IF screen-group1 = gc_screen_grp1_m3.
      IF lv_display_validity = abap_true AND p_iload IS NOT INITIAL.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.                                                "#EC CI_CYCLO

AT SELECTION-SCREEN.
  DATA: lt_load_id TYPE STANDARD TABLE OF vrm_value WITH DEFAULT KEY.

  CASE sy-ucomm.
    WHEN gc_ucomm_display_validity.
      lv_display_validity = abap_true.

    WHEN gc_ucomm_onli.
      "Perform mandatory field checks
      IF p_source IS INITIAL AND p_dload IS INITIAL.
        MESSAGE s027(/vpcoe/plm).
        CLEAR : sscrfields.
        STOP.
      ENDIF.

      IF p_rfcdes IS INITIAL AND p_dload IS INITIAL.
        MESSAGE s032(/vpcoe/plm).
        CLEAR : sscrfields.
        STOP.
      ENDIF.

      IF p_explvl IS INITIAL AND p_dload IS INITIAL.
        MESSAGE s036(/vpcoe/plm).
        CLEAR : sscrfields.
        STOP.
      ENDIF.

      "If not running in background: Check if no selection criteria is given and issue warning
      IF sy-batch = abap_false AND p_iload = abap_true AND
         ( s_mat IS INITIAL AND
           s_plant IS INITIAL AND
           s_mattyp IS INITIAL AND
           s_altbom IS INITIAL AND
           s_usage IS INITIAL AND
           s_chdate IS INITIAL AND
           p_bomst IS INITIAL ).

        MESSAGE s000(/vpcoe/plm) INTO /vpcoe/cl_rdp_log=>sv_msg_text.

        DATA lv_answer TYPE char1.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = /vpcoe/cl_rdp_log=>sv_msg_text
            display_cancel_button = ''
          IMPORTING
            answer                = lv_answer.

        IF lv_answer <> '1'.
          CLEAR sy-ucomm.
          STOP.
        ENDIF.
      ENDIF.

    WHEN 'FDVAR'.
      p_varn = p_dvar.

    WHEN 'FCODE1'.
      DATA(lo_entity_proc) = /vpcoe/cl_uph_factory=>get_instance(  )->get_entity_processor(
                                                                              EXPORTING iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
                                                                                        iv_upload_mode   = lv_upload_mode
                                                                                        is_parameters    = ls_surdp_s_input ).
      DATA(lt_protocol) = lo_entity_proc->read_from_protocol(
                                                  EXPORTING
                                                    iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
                                                    iv_upload_mode   = lv_upload_mode ).

      LOOP AT lt_protocol ASSIGNING FIELD-SYMBOL(<ls_protocol>).
        INSERT VALUE #( key = <ls_protocol>-uuid
                        text = COND #( WHEN <ls_protocol>-id IS INITIAL THEN <ls_protocol>-uuid
                                                                        ELSE <ls_protocol>-id ) ) INTO TABLE lt_load_id.
      ENDLOOP.

      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id              = 'P_DVAR'
          values          = lt_load_id
        EXCEPTIONS
          id_illegal_name = 1
          OTHERS          = 2.

    WHEN 'FDELV'.
      lo_entity_proc = /vpcoe/cl_uph_factory=>get_instance(  )->get_entity_processor(
                                                                        EXPORTING iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
                                                                                  iv_upload_mode   = lv_upload_mode
                                                                                  is_parameters    = ls_surdp_s_input ).
      lt_protocol = lo_entity_proc->read_from_protocol(
                                                  EXPORTING
                                                    iv_upload_entity = /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
                                                    iv_upload_mode   = lv_upload_mode ).

      lo_entity_proc->delete_from_protocol(
          it_protocol = VALUE #( ( VALUE #( lt_protocol[ uuid = p_dvar ] ) ) ) ).

  ENDCASE.
