class /VPCOE/CL_RDP_HTTP definition
  public
  inheriting from /VPCOE/CL_HTTP_COMMUNICATION
  create public .

public section.
  PROTECTED SECTION.
private section.

  data MO_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  data MV_TOKEN type STRING .
  data MV_USER_AGENT type STRING .
ENDCLASS.



CLASS /VPCOE/CL_RDP_HTTP IMPLEMENTATION.
ENDCLASS.
