*&---------------------------------------------------------------------*
*& Include /VPCOE/PCKF_SEL_SCR_RETRIEVE
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
PARAMETERS: p_varn TYPE /vpcoe/load_id MODIF ID m3.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) text-010 FOR FIELD p_varn MODIF ID m4.
PARAMETERS: p_dvar TYPE char32 AS LISTBOX VISIBLE LENGTH 32 USER-COMMAND fdvar MODIF ID m4.
SELECTION-SCREEN PUSHBUTTON 68(15) text-011 USER-COMMAND fdelv MODIF ID m4.
SELECTION-SCREEN END OF LINE.
PARAMETERS:
  p_rfcdes TYPE /vpcoe/uph_rfcdest VISIBLE LENGTH 16 MODIF ID m2,
  p_pckgsz TYPE i VISIBLE LENGTH 3 MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b3  .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_rptcnf FOR lv_report_config MODIF ID m1 NO INTERVALS NO-EXTENSION.
SELECT-OPTIONS: s_rptcat FOR lv_report_categ MODIF ID m1 NO INTERVALS NO-EXTENSION.
PARAMETERS: p_cntry TYPE t005-land1 MODIF ID m1.
SELECT-OPTIONS: s_contx FOR lv_context MODIF ID m1 DEFAULT 'RDP_DEFAULT' NO INTERVALS.
SELECT-OPTIONS: s_dirctn FOR lv_busi_proc_directn MODIF ID m1 NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-009.
PARAMETERS: r_cust  RADIOBUTTON GROUP req.
PARAMETERS: r_scnr  RADIOBUTTON GROUP req.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN END OF BLOCK b.
DATA: lt_load_id TYPE STANDARD TABLE OF vrm_value WITH DEFAULT KEY.

AT SELECTION-SCREEN OUTPUT.

  lv_version = /vpcoe/cl_pckf_proc_base=>determine_version( p_rfcdes ).

  LOOP AT SCREEN.

    IF p_iload IS NOT INITIAL.
      CASE screen-group1.
        WHEN gc_screen_grp1_m1 .
          screen-input = 1.
        WHEN gc_screen_grp1_m4.
          screen-input = 0.
          screen-active = 0.
        WHEN gc_screen_grp1_m3.
          screen-input = 1.
      ENDCASE.

    ELSEIF p_dload IS NOT INITIAL.
      IF screen-group1 = gc_screen_grp1_m1 OR
         screen-group1 = gc_screen_grp1_m2.
        screen-input = 0.
      ENDIF.
      IF screen-group1 = gc_screen_grp1_m3.
        screen-input = 0.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
    IF screen-name CS 'S_CONTX' AND lv_version = 2.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.                                                "#EC CI_CYCLO
  SELECT * INTO TABLE @DATA(lt_load_data)
  FROM /vpcoe/pckf_prot
  WHERE entity_type = @/vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee.

  lt_load_id = VALUE #( FOR ls_load_data IN lt_load_data
                         ( key = ls_load_data-load_id
                           text = ls_load_data-load_id ) ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_DVAR'
      values          = lt_load_id
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF lt_load_id IS NOT INITIAL AND p_dload = abap_true.
    CLEAR: s_rptcnf[], s_rptcat[], s_contx[], s_dirctn, p_cntry.
    DATA lv_sel_criteria TYPE string.
    lv_sel_criteria = VALUE #( lt_load_data[ load_id = p_dvar ]-selection OPTIONAL ).

    IF lv_sel_criteria IS NOT INITIAL.
      CLEAR: ls_retrieval_input.
      /vpcoe/cl_common_helper=>deserialize_json( EXPORTING iv_json = lv_sel_criteria
                                                 CHANGING  cs_data = ls_retrieval_input ).
      s_rptcnf[] = ls_retrieval_input-report_config_id.
      s_rptcat[] = ls_retrieval_input-report_categ_id.
      p_cntry = ls_retrieval_input-country.
      s_contx[] = ls_retrieval_input-context.
      s_dirctn = ls_retrieval_input-bus_proc_dir.

    ENDIF.
  ENDIF.

AT SELECTION-SCREEN.

  IF p_iload = abap_true.
    CLEAR: s_rptcnf[], s_rptcat[], s_contx[], s_dirctn, p_cntry.
  ENDIF.
*  IF sscrfields-ucomm EQ gc_ucomm_onli.
  CASE sy-ucomm.
    WHEN gc_ucomm_onli.
      "Perform mandatory field checks
      IF p_rfcdes IS INITIAL AND p_dload IS INITIAL.
        MESSAGE e032(/vpcoe/pckf).
      ENDIF.

      IF p_pckgsz <= 0.
        p_pckgsz = /vpcoe/if_pckf_entity_proc=>gc_defaults-retrieve_package_size.
      ENDIF.

      IF s_dirctn-low IS NOT INITIAL AND
         s_dirctn-low <> 'INBOUND' AND s_dirctn-low <> 'OUTBOUND'.
        MESSAGE e025(/vpcoe/pckf).
      ENDIF.

      LOOP AT s_rptcat[] INTO DATA(ls_rptcat).
        IF ls_rptcat-sign NE 'I' OR ls_rptcat-option NE 'EQ'.
          MESSAGE e012(/vpcoe/pckf) WITH text-006.
        ENDIF.
      ENDLOOP.

      LOOP AT s_rptcnf[] INTO DATA(ls_rptcnf).
        IF ls_rptcnf-sign NE 'I' OR ls_rptcnf-option NE 'EQ'.
          MESSAGE e012(/vpcoe/pckf) WITH text-007.
        ENDIF.
      ENDLOOP.

    WHEN 'FDVAR'.
      p_varn = p_dvar.

    WHEN 'FDELV'.
      DELETE FROM /vpcoe/pckf_prot WHERE load_id = p_dvar AND entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee.
      COMMIT WORK.
      CLEAR p_dvar.
      SELECT * INTO TABLE @DATA(lt_load_data)
      FROM /vpcoe/pckf_prot
      WHERE entity_type = @/vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee.

      lt_load_id = VALUE #( FOR ls_load_data IN lt_load_data
                             ( key = ls_load_data-load_id
                               text = ls_load_data-load_id ) ).

      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id              = 'P_DVAR'
          values          = lt_load_id
        EXCEPTIONS
          id_illegal_name = 1
          OTHERS          = 2.

  ENDCASE.
