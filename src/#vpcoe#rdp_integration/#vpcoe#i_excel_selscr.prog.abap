*&---------------------------------------------------------------------*
*&  Include           /VPCOE/I_EXCEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl_front WITH FRAME TITLE text-b21.
PARAMETERS: r_local  RADIOBUTTON GROUP rxls USER-COMMAND filetrg DEFAULT 'X',
            r_server RADIOBUTTON GROUP rxls.
PARAMETERS: p_file TYPE localfile.
SELECTION-SCREEN END OF BLOCK bl_front.
