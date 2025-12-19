*&---------------------------------------------------------------------*
*&  Include           /VPCOE/PCKF_SEL_SCR_CSTM_ROLE
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME  TITLE text-001.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME  TITLE text-001.
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
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
PARAMETERS: p_role TYPE char40 AS LISTBOX VISIBLE LENGTH 40 USER-COMMAND lb_cmd MODIF ID rol OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN OUTPUT.

  IF p_dload = abap_true.
    CLEAR p_role.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name CS 'P_ROLE'.
      screen-input = COND #( WHEN p_dload = abap_true THEN 0
                                                      ELSE 1 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

INITIALIZATION.

  gt_customer_role = VALUE #( ( key = /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_end_consum text = `01 - ` && /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_end_consum )
                              ( key = /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_horeca     text = `02 - ` && /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_horeca )
                              ( key = /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_hospital   text = `03 - ` && /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_hospital )
                              ( key = /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_prof_custm text = `04 - ` && /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_prof_custm )
                              ( key = /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_retailer   text = `05 - ` && /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_retailer )
                              ( key = /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_no_role    text = `00 - ` && /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_no_role ) ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_ROLE'
      values          = gt_customer_role
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc = 0.
    p_role = /vpcoe/if_pckf_entity_proc=>gc_customer_role-gc_end_consum.
  ENDIF.
