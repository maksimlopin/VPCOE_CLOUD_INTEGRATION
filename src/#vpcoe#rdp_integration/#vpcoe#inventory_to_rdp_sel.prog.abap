*&---------------------------------------------------------------------*
*&  Include           /VPCOE/INVENTORY_TO_RDP_SEL
*&---------------------------------------------------------------------*
INCLUDE /vpcoe/version.
INCLUDE /vpcoe/i_delta_ss.

SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-005.
PARAMETERS: p_cselid TYPE /vpcoe/de_sel_name.
PARAMETERS: r_screen RADIOBUTTON GROUP req USER-COMMAND et,
            r_send   RADIOBUTTON GROUP req,
            r_excel  RADIOBUTTON GROUP req.
INCLUDE /vpcoe/i_excel_selscr.
SELECTION-SCREEN END OF BLOCK options.

SELECTION-SCREEN BEGIN OF BLOCK report WITH FRAME TITLE text-101.

TABLES:t001w,t001,iseg,mara.
SELECT-OPTIONS:
  so_co_co  FOR t001-bukrs OBLIGATORY,
  so_plant  FOR t001w-werks OBLIGATORY,
  so_pro    FOR mara-matnr,
  so_year   FOR iseg-mjahr NO INTERVALS NO-EXTENSION,
  so_month   FOR gv_month NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK report.

SELECTION-SCREEN BEGIN OF BLOCK addit WITH FRAME TITLE text-102.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_lgbst TYPE am07m-lgbst AS CHECKBOX DEFAULT 'X' USER-COMMAND lgbst.
SELECTION-SCREEN COMMENT 4(50) text-103 FOR FIELD p_lgbst.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_sbbst TYPE am07m-sbbst AS CHECKBOX USER-COMMAND sbbst.
SELECTION-SCREEN COMMENT 4(50) text-104 FOR FIELD p_sbbst.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS so_sobkz FOR mseg-sobkz.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_trans TYPE am07m-sbbst AS CHECKBOX USER-COMMAND trans.
SELECTION-SCREEN COMMENT 4(50) text-105 FOR FIELD p_trans.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK intrp WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_xtram LIKE am07m-mb5t_sel DEFAULT 'X' MODIF ID trn.
SELECTION-SCREEN COMMENT 4(50) text-i01 FOR FIELD p_xtram MODIF ID trn.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_xnlcc LIKE am07m-mb5t_selcc DEFAULT 'X' MODIF ID trn.
SELECTION-SCREEN COMMENT 4(50) text-i02 FOR FIELD p_xnlcc MODIF ID trn.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_xelik LIKE am07m-also_elikz DEFAULT 'X' MODIF ID trn.
SELECTION-SCREEN COMMENT 4(50) text-i03 FOR FIELD p_xelik MODIF ID trn.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK intrp.

SELECTION-SCREEN END OF BLOCK addit.

SELECTION-SCREEN BEGIN OF BLOCK arch WITH FRAME TITLE text-106.
PARAMETERS: p_arc    TYPE mb5b_archive AS CHECKBOX DEFAULT ' ',
            p_arcidx LIKE aind_str1-archindex.
SELECTION-SCREEN END OF BLOCK arch.

ENHANCEMENT-POINT /vpcoe/inventory_sel_scr SPOTS /vpcoe/enh_inventory_rdp .

AT SELECTION-SCREEN.
  INCLUDE /vpcoe/i_delta_ss_hndlr.

  IF p_delta = abap_true AND sscrfields-ucomm = 'CMD_SELID'.

    go_cust->get_delta_sel_opt(
      EXPORTING
        iv_sel_name = p_pselid
      IMPORTING
        et_sel_opt  = gt_sel_params
        ev_last_run = DATA(gv_last_run) ).

    LOOP AT gt_sel_params ASSIGNING FIELD-SYMBOL(<ls_sel_params>).
      CASE <ls_sel_params>-field.
        WHEN 'SO_PLANT'.
          ASSIGN <ls_sel_params>-value->* TO <gt_value>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          CLEAR so_plant[].
          go_cust->add_sel_opt(
            EXPORTING
              it_so_gen = <gt_value>
            CHANGING
              ct_so     = so_plant[] ).

        WHEN  'SO_PRO'.
          ASSIGN <ls_sel_params>-value->* TO <gt_value>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          CLEAR so_pro[].
          go_cust->add_sel_opt(
            EXPORTING
              it_so_gen = <gt_value>
            CHANGING
              ct_so     = so_pro[] ).

        WHEN 'SO_CO_CO'.
          ASSIGN <ls_sel_params>-value->* TO <gt_value>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          CLEAR so_co_co[].
          go_cust->add_sel_opt(
            EXPORTING
              it_so_gen = <gt_value>
            CHANGING
              ct_so     = so_co_co[] ).

        WHEN  'SO_SOBKZ'.
          ASSIGN <ls_sel_params>-value->* TO <gt_value>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          CLEAR so_sobkz[].
          go_cust->add_sel_opt(
            EXPORTING
              it_so_gen = <gt_value>
            CHANGING
              ct_so     = so_sobkz[] ).

        WHEN 'P_LGBST' OR 'P_SBBST' OR 'P_TRANS' OR 'P_ARC' OR 'P_ARCIDX'
          OR 'P_XTRAM' OR 'P_XNLCC' OR 'P_XELIK'.
          ASSIGN <ls_sel_params>-value->* TO <gv_value>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          CASE <ls_sel_params>-field.
            WHEN  'P_LGBST'.
              p_lgbst = <gv_value>.
            WHEN  'P_SBBST'.
              p_sbbst = <gv_value>.
            WHEN  'P_TRANS'.
              p_trans = <gv_value>.
            WHEN  'P_ARC'.
              p_arc = <gv_value>.
            WHEN  'P_ARCIDX'.
              p_arcidx = <gv_value>.
            WHEN 'P_XTRAM'.
              p_xtram = <gv_value>.
            WHEN 'P_XNLCC'.
              p_xnlcc = <gv_value>.
            WHEN 'P_XELIK'.
              p_xelik = <gv_value>.

          ENDCASE.
      ENDCASE.
    ENDLOOP.

    IF gv_last_run IS NOT INITIAL.
      /vpcoe/cl_rdp_helper=>get_month_year_last_run(
        EXPORTING
          iv_last_run =  gv_last_run
        IMPORTING
          ev_month    =  DATA(gv_month)
          ev_year     =  DATA(gv_year) ).

      so_month[] = VALUE #( ( sign = 'I' option = 'EQ' low = gv_month  ) ).
      so_year[] =  VALUE #( ( sign = 'I' option = 'EQ' low = gv_year  ) ).
    ENDIF.

ENHANCEMENT-POINT /vpcoe/inventory_sel_scr_delta SPOTS /vpcoe/enh_inventory_rdp .

  ENDIF.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  lv_ucomm = sy-ucomm.

  INCLUDE /vpcoe/i_excel_valreq.

AT SELECTION-SCREEN OUTPUT.

  btn_delc = text-010."'Delete from Previous Selection ID'.
  btn_dela = text-011."'Delete all enteries'.

  gv_scr_fld_state = COND #( WHEN p_delta = abap_true THEN 0
                                                      ELSE 1 ).

  LOOP AT SCREEN.
    IF screen-name CS 'SO_CO_CO'
      OR screen-name CS 'SO_PLANT'
      OR screen-name CS 'SO_PR_G'
      OR screen-name CS 'SO_PRO'.
      screen-input = gv_scr_fld_state.
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_CSELID'.
      screen-active = COND #( WHEN p_delta = abap_true THEN 0
                                                       ELSE 1 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_PSELID'.
      screen-active = COND #( WHEN p_delta = abap_true THEN 1
                                                       ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'SO_SOBKZ'.
      screen-active = COND #( WHEN p_sbbst = abap_true THEN 1
                                                       ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_FILE'.
      screen-active = COND #( WHEN r_excel = abap_true THEN 1
                                                    ELSE 0 ).
      MODIFY SCREEN.
    ELSEIF screen-name CS 'P_XTRAM'
        OR screen-name CS 'P_XNLCC'
        OR screen-name CS 'P_XELIK'.
      screen-active = COND #( WHEN p_trans = abap_false THEN 0
                                                        ELSE 1 ).
      MODIFY SCREEN.

    ENDIF.

    CASE screen-group1.
      WHEN 'TRL'.
        screen-intensified = '1'.
        MODIFY SCREEN.
      WHEN 'TRN'.
        screen-active = COND #( WHEN  p_trans = abap_true  THEN 1
                                                          ELSE 0 ).
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.
  INCLUDE /vpcoe/i_delta_ss_out.
  INCLUDE /vpcoe/i_excel_scrout.
  INCLUDE /vpcoe/version_set.

  IF p_sbbst = abap_false.
    CLEAR so_sobkz.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arcidx.
  DATA: g_f_f4_mode(01)  TYPE  c,
        g_f_f4_archindex LIKE  aind_str1-archindex.
  MOVE 'X' TO g_f_f4_mode.
  EXPORT g_f_f4_mode TO  MEMORY ID 'MB51_F4_MODE'.

  SUBMIT ('RM07DOCS')  AND RETURN.

  IMPORT g_f_f4_archindex FROM  MEMORY ID 'MB51_F4_ARCHINDEX'.
  MOVE g_f_f4_archindex TO  p_arcidx.

  CLEAR g_f_f4_mode.
  EXPORT g_f_f4_mode TO  MEMORY ID 'MB51_F4_MODE'.

ENHANCEMENT-POINT /vpcoe/inventory_scr_out SPOTS /vpcoe/enh_inventory_rdp .

INITIALIZATION.
  DATA(lv_tcode) = COND #( WHEN sy-batch = abap_false THEN sy-tcode
                                                      ELSE '/VPCOE/RDP_INVENTORY' ).
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD lv_tcode.
  IF sy-subrc <> 0.
    MESSAGE e062(/vpcoe/common) WITH lv_tcode.
  ENDIF.
  IF p_cselid IS INITIAL.
    p_cselid = 'RDP_' && /vpcoe/cl_rdp_helper=>sc_grp_id-inventory && '-' && sy-datum.
  ENDIF.

  go_cust = NEW #( iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                   iv_srv_grp = /vpcoe/cl_rdp_helper=>sc_grp_id-inventory
                   iv_srv_id  = /vpcoe/cl_rdp_helper=>sc_service_id-inventory ).

  go_cust->get_delta_ids( IMPORTING et_sel_names = DATA(gt_sel_opt) ).
  LOOP AT gt_sel_opt ASSIGNING FIELD-SYMBOL(<ls_mat_doc>).
    INSERT VALUE #( key  = <ls_mat_doc>
                    text = <ls_mat_doc>  ) INTO TABLE gt_selid_vrm.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_PSELID'
      values          = gt_selid_vrm
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    CLEAR gt_selid_vrm.
  ENDIF.
