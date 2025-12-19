*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_DELTA
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK delta WITH FRAME TITLE text-003.
PARAMETERS: p_delta  TYPE xfeld AS CHECKBOX USER-COMMAND usrdelta.
PARAMETERS: p_pselid TYPE /vpcoe/de_sel_name AS LISTBOX VISIBLE LENGTH 40 USER-COMMAND cmd_selid.
SELECTION-SCREEN PUSHBUTTON  74(30) btn_delc USER-COMMAND cmd_one .
SELECTION-SCREEN PUSHBUTTON /74(30) btn_dela USER-COMMAND cmd_all.
SELECTION-SCREEN END OF BLOCK delta.
