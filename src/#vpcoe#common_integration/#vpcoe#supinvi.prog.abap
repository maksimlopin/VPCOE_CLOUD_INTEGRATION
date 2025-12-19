*&---------------------------------------------------------------------*
*&  Include           /VPCOE/SUPINVI
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK supinvi WITH FRAME TITLE text-019.
PARAMETERS: p_supinv TYPE xfeld AS CHECKBOX USER-COMMAND supinvi.
SELECT-OPTIONS: so_cntri FOR t001w-land1 MODIF ID sup DEFAULT 'ES',
                so_belnr FOR rbkp-belnr MODIF ID sup,
                so_buzei FOR rseg-buzei MODIF ID sup,
                so_ccode FOR rbkp-bukrs MODIF ID sup,
                so_plant FOR rseg-werks MODIF ID sup,
                so_gjahr FOR rbkp-gjahr MODIF ID sup.
SELECTION-SCREEN END OF BLOCK supinvi.
