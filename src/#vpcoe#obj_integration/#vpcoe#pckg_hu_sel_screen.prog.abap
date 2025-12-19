*&---------------------------------------------------------------------*
*&  Include           /VPCOE/PCKG_HU_SEL_SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 31(10) text-002 FOR FIELD p_test .
PARAMETERS : p_test TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-005.
"Source ID parameter
PARAMETERS: p_source TYPE /vpcoe/source_id LOWER CASE VISIBLE LENGTH 36 MODIF ID m4,
            p_rfcdes TYPE /vpcoe/uph_rfcdest   VISIBLE LENGTH 16 MODIF ID m4.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK seldata WITH FRAME TITLE text-001.
PARAMETERS: p_start TYPE begda,
            p_end   TYPE begda MODIF ID m1 DEFAULT sy-datum.

SELECT-OPTIONS:
  s_matnr FOR lips-matnr,"Material
  s_typem FOR lips-mtart,"Material
  s_group FOR lips-matkl,"Material Group
  s_plant FOR lips-werks,"Plant
  s_sales FOR likp-vkorg,"Sales Organization
  s_type  FOR likp-lfart, "DeliveryDocumentType
  s_kunnr FOR likp-kunnr, "CustomerId (ShipToParty)
  s_cntry FOR adrc-country, "Country
  s_categ FOR lips-pstyv, "itemCategory
  s_distr FOR lips-vtweg, "distributionChannel
  s_divis FOR lips-spart, "division
  s_vbeln FOR likp-vbeln,
  s_pcntr FOR t001w-land1. "Delivery Plant Country
SELECTION-SCREEN END OF BLOCK seldata.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = gc_screen_grp1_m1.
      IF lv_display_enddate = abap_true.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF sy-ucomm = gc_ucomm_display_enddate.
    lv_display_enddate = abap_true.
  ENDIF.

  IF sy-ucomm = gc_ucomm_onli.

    " Perform mandatory field and additional checks
    IF p_source IS INITIAL.
      MESSAGE e027(/vpcoe/plm).
    ENDIF.

    IF p_rfcdes IS INITIAL.
      MESSAGE e032(/vpcoe/plm).
    ENDIF.

    IF p_start IS INITIAL.
      MESSAGE e045(/vpcoe/plm).
    ENDIF.

    IF p_end IS INITIAL.
      MESSAGE e045(/vpcoe/plm).
    ENDIF.

    IF p_start > p_end.
      MESSAGE e045(/vpcoe/plm).
    ENDIF.

    DATA lv_months_between_dates TYPE i.
    CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES'
      EXPORTING
        i_datum_von = p_start
        i_datum_bis = p_end
      IMPORTING
        e_monate    = lv_months_between_dates.

    " If not running in background: Check if no selection criteria is given and issue warning
    IF     sy-batch                 = abap_false
       AND s_matnr                 IS INITIAL
       AND s_typem                 IS INITIAL
       AND s_group                 IS INITIAL
       AND s_plant                 IS INITIAL
       AND s_sales                 IS INITIAL
       AND s_type                  IS INITIAL
       AND s_kunnr                 IS INITIAL
       AND s_cntry                 IS INITIAL
       AND s_categ                 IS INITIAL
       AND s_distr                 IS INITIAL
       AND s_divis                 IS INITIAL
       AND s_vbeln                 IS INITIAL
       AND s_pcntr                 IS INITIAL
       AND lv_months_between_dates  > 6.
      MESSAGE w000(/vpcoe/plm) INTO DATA(lv_msg_txt).

      DATA lv_answer TYPE char1.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = lv_msg_txt
          popup_type            = 'ICON_MESSAGE_WARNING'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_answer.

      IF lv_answer <> '1'.
        CLEAR sy-ucomm.
        MESSAGE e045(/vpcoe/plm).
      ENDIF.
    ENDIF.

  ENDIF.
