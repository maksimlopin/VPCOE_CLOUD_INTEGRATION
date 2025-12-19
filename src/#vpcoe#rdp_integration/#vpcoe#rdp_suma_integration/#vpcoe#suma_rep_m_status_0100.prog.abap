*&---------------------------------------------------------------------*
*&  Include           /VPCOE/TRACE_MAINT_STATUS_0100
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MAIN100'.

  go_app->pbo_0100( ).

ENDMODULE.
