*&---------------------------------------------------------------------*
*& Include /VPCOE/PCKF_SEL_SCR_POSTING
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME  TITLE text-002.

SELECTION-SCREEN:
                   COMMENT /1(40) text-003,
                   COMMENT 41(10) t_cnt,
                   SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME  TITLE text-002.

  PARAMETERS: p_test   TYPE abap_bool AS CHECKBOX.

  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME  TITLE text-001.

    PARAMETERS: p_failrp TYPE i DEFAULT /vpcoe/if_pckf_entity_proc=>gc_defaults-failed_retention_days.

  SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.
