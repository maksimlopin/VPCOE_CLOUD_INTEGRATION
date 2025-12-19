CLASS /vpcoe/cl_config_access DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class CL_EHFND_CONFIG_ACCESS
*"* do not include other source files here!!!

    CLASS-METHODS get_default_language
      RETURNING
        VALUE(rv_language_key) TYPE langu .
    CLASS-METHODS get_default_currency_code
      RETURNING
        VALUE(rv_cur_code) TYPE sycurr .
    CLASS-METHODS get_default_country
      RETURNING
        VALUE(rv_country) TYPE land1 .
  PROTECTED SECTION.
*"* protected components of class CL_EHFND_CONFIG_ACCESS
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class CL_EHFND_CONFIG_ACCESS
*"* do not include other source files here!!!
ENDCLASS.



CLASS /VPCOE/CL_CONFIG_ACCESS IMPLEMENTATION.


  METHOD get_default_country.

************************************************************************
* define data
************************************************************************


************************************************************************
* functional body
************************************************************************

    rv_country = 'DE'.

  ENDMETHOD.


  METHOD get_default_currency_code.

    DATA:
      lt_config TYPE TABLE OF tcurt, "ehfndc_config,
      ls_config LIKE LINE OF  lt_config.

    SELECT SINGLE * FROM tcurt INTO ls_config.

    IF sy-subrc <> 0 OR ls_config-waers IS INITIAL.
*      ls_config-default_cur_code = 'USD'.
      ls_config-waers = 'USD'.
    ENDIF.

    rv_cur_code = ls_config-waers.

  ENDMETHOD.


  METHOD get_default_language.
    DATA:
      lt_config TYPE TABLE OF tcurt,
      ls_config LIKE LINE OF  lt_config.


    SELECT SINGLE * FROM tcurt INTO ls_config.

    IF sy-subrc <> 0.
      ls_config-spras = 'E'.

    ELSEIF ls_config-spras IS INITIAL.
      ls_config-spras = 'E'.
    ENDIF.

    rv_language_key = ls_config-spras.

  ENDMETHOD.
ENDCLASS.
