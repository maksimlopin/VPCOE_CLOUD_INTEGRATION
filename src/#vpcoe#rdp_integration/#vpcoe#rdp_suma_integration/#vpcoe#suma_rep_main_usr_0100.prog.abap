*&---------------------------------------------------------------------*
*&  Include           /VPCOE/TRACE_MAINT_USR_0100
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
