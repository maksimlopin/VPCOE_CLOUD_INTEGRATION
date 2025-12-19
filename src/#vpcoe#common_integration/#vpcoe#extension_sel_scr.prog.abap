*&---------------------------------------------------------------------*
*&  Include           /VPCOE/EXTENSION_SEL_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK ext_sel_scr WITH FRAME TITLE text-rty.
PARAMETERS:
  pgen TYPE xfeld AS CHECKBOX DEFAULT 'X' USER-COMMAND pgen,
  pext TYPE xfeld AS CHECKBOX DEFAULT 'X' USER-COMMAND pext.
SELECTION-SCREEN END OF BLOCK ext_sel_scr.
