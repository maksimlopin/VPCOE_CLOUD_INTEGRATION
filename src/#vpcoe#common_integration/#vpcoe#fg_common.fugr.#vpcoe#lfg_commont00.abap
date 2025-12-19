*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /VPCOE/CODE_LIST................................*
DATA:  BEGIN OF STATUS_/VPCOE/CODE_LIST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/VPCOE/CODE_LIST              .
CONTROLS: TCTRL_/VPCOE/CODE_LIST
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: */VPCOE/CODE_LIST              .
TABLES: /VPCOE/CODE_LIST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
