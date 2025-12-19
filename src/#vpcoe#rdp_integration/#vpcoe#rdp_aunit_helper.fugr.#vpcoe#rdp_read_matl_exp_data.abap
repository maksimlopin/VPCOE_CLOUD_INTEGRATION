FUNCTION /VPCOE/RDP_READ_MATL_EXP_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      TT_MATL_EXP STRUCTURE  /VPCOE/MATL_EXP
*"----------------------------------------------------------------------
  SELECT * FROM /vpcoe/matl_exp
  INTO CORRESPONDING FIELDS OF TABLE tt_matl_exp.

ENDFUNCTION.
