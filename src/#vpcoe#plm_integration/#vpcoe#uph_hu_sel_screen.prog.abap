*&---------------------------------------------------------------------*
*& Include          SURDP_UPH_PCKG_SEL_SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME  TITLE text-005.
"Full or Delta mode radiobuttons
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11) text-002 FOR FIELD p_iload .
PARAMETERS : p_iload TYPE char1 RADIOBUTTON GROUP r2 DEFAULT 'X' USER-COMMAND fcode1 .
SELECTION-SCREEN COMMENT 31(10) text-003 FOR FIELD p_test .
PARAMETERS : p_test TYPE char1 RADIOBUTTON GROUP r2 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-005.
"Source ID parameter
PARAMETERS: p_source TYPE /vpcoe/uph_source_id LOWER CASE VISIBLE LENGTH 36 MODIF ID m4,
            p_rfcdes TYPE /vpcoe/uph_rfcdest   VISIBLE LENGTH 16 MODIF ID m4.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK seldata WITH FRAME TITLE text-006.
PARAMETERS: p_wadat TYPE begda MODIF ID m1 DEFAULT sy-datum,
            p_month TYPE dlymo MODIF ID m1 DEFAULT 3.
SELECT-OPTIONS:
  so_matnr FOR lips-matnr,"Material
  so_typem FOR lips-mtart,"Material
  so_group FOR lips-matkl,"Material Group
  so_plant FOR lips-werks,"Plant
  so_sales FOR likp-vkorg,"Sales Organization
  so_type  FOR likp-lfart, "DeliveryDocumentType
  so_kunnr FOR likp-kunnr, "CustomerId (ShipToParty)
  so_cntry FOR adrc-country, "Country
  so_categ FOR lips-pstyv, "itemCategory
  so_distr FOR lips-vtweg, "distributionChannel
  so_divis FOR lips-spart, "division
  so_vbeln FOR likp-vbeln,
  so_pcntr FOR t001w-land1. "Delivery Plant Country
SELECTION-SCREEN END OF BLOCK seldata.

AT SELECTION-SCREEN ON EXIT-COMMAND.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'TRL'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

    IF p_source IS INITIAL.
      p_source = 'ECC_EMEA'.
    ENDIF.

  IF p_rfcdes IS INITIAL.
        p_rfcdes = /vpcoe/if_plm_constants=>gc_rfc_plm_destinations-pkg_compositions.
  ENDIF.
