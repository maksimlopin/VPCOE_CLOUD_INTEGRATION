class /VPCOE/CX_HRRCF_RFC_COMMUNICTN definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

*"* public components of class /VPCOE/CX_HRRCF_RFC_COMMUNICTN
*"* do not include other source files here!!!
public section.

  constants CX_HRRCF_RFC_SYSTEM_FAILURE type SOTR_CONC value '78349AAC582D5042BAA4961FB954BB8B' ##NO_TEXT.
  constants CX_HRRCF_RFC_COMM_FAILURE type SOTR_CONC value '2EB6F5574924D649A15557B65B20832C' ##NO_TEXT.
  constants /VPCOE/CX_HRRCF_RFC_COMMUNICTN type SOTR_CONC value '7FF22D77C572C54C867325EEC2B22B66' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
*"* protected components of class /VPCOE/CX_HRRCF_RFC_COMMUNICTN
*"* do not include other source files here!!!
private section.
*"* private components of class /VPCOE/CX_HRRCF_RFC_COMMUNICTN
*"* do not include other source files here!!!
ENDCLASS.



CLASS /VPCOE/CX_HRRCF_RFC_COMMUNICTN IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = /VPCOE/CX_HRRCF_RFC_COMMUNICTN .
 ENDIF.
  endmethod.
ENDCLASS.
