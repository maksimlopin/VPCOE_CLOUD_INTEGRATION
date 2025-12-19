*&---------------------------------------------------------------------*
*&  Include           /VPCOE/BILLDI
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK billdi WITH FRAME TITLE text-018.
PARAMETERS: p_bildi TYPE xfeld AS CHECKBOX USER-COMMAND bildi.
SELECT-OPTIONS: so_cntrb  FOR t001w-land1 MODIF ID bdi DEFAULT 'ES' ,
                so_bildi FOR vbrk-vbeln MODIF ID bdi,
                so_bdat  FOR vbrk-fkdat MODIF ID bdi,
                so_bsddc FOR vbrp-vgtyp MODIF ID bdi.
SELECTION-SCREEN END OF BLOCK billdi.
