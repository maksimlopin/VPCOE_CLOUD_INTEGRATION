*&---------------------------------------------------------------------*
*&  Include           /VPCOE/BAREA
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK barea_1 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF BLOCK brdp_1 WITH FRAME.
PARAMETERS:
  prdp TYPE xfeld AS CHECKBOX DEFAULT 'X'  USER-COMMAND rdp. "Send to rdp
SELECTION-SCREEN END OF BLOCK brdp_1.
SELECTION-SCREEN BEGIN OF BLOCK bpfm WITH FRAME.
PARAMETERS:
  ppfm   TYPE xfeld AS CHECKBOX DEFAULT 'X' USER-COMMAND pfm. "Send to pfm
SELECTION-SCREEN END OF BLOCK bpfm.
SELECTION-SCREEN END OF BLOCK barea_1.
