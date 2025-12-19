FUNCTION /vpcoe/call_rm07mlbd.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_FAILED) TYPE  XFELD
*"  TABLES
*"      IT_RSPAR TYPE  RSPARAMS_TT
*"      ET_STOCK TYPE  /VPCOE/T_STOCK
*"----------------------------------------------------------------------
  DATA: lo_data              TYPE REF TO data.

  FIELD-SYMBOLS <lt_result> TYPE ANY TABLE.

  CLEAR: et_stock,
         ev_failed.

  "call report (tcode mb5b)
  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                          metadata = abap_false
                                          data     = abap_true ).

  SUBMIT rm07mlbd WITH SELECTION-TABLE it_rspar EXPORTING LIST TO MEMORY AND RETURN .

  TRY.
      "Get data from SALV model
      cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lo_data ).
      ASSIGN lo_data->* TO <lt_result>.
    CATCH cx_salv_bs_sc_runtime_info.
      ev_failed = abap_true.
      RETURN.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  IF <lt_result> IS NOT ASSIGNED.
    ev_failed = abap_true.
    RETURN.
  ENDIF.

  "get result
  LOOP AT <lt_result> ASSIGNING FIELD-SYMBOL(<ls_tab>).
    APPEND INITIAL LINE TO et_stock ASSIGNING FIELD-SYMBOL(<ls_stock_result>).

    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_matnr>).
    IF sy-subrc = 0.
      <ls_stock_result>-product = <lv_matnr>.
    ENDIF.

    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_werks>).
    IF sy-subrc = 0.
      <ls_stock_result>-plant = <lv_werks>.
    ENDIF.

    ASSIGN COMPONENT 'MEINS' OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_meins>).
    IF sy-subrc = 0.
      <ls_stock_result>-base_unit_of_measure = <lv_meins>.
    ENDIF.

    ASSIGN COMPONENT 'ENDMENGE' OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_menge>).
    IF sy-subrc = 0.
      <ls_stock_result>-stock_quantity_in_base_unit = <lv_menge>.
    ENDIF.

    ASSIGN COMPONENT 'END_DATE' OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_end_date>).
    IF sy-subrc = 0.
      <ls_stock_result>-calendar_month =  <lv_end_date>+04(02).
      <ls_stock_result>-calendar_year = <lv_end_date>+00(04).
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
