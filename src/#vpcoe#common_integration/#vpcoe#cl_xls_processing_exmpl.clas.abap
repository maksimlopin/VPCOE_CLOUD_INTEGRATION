class /VPCOE/CL_XLS_PROCESSING_EXMPL definition
  public
  final
  create public .

public section.

  interfaces /VPCOE/IF_XLS_PROCESSING .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS /VPCOE/CL_XLS_PROCESSING_EXMPL IMPLEMENTATION.


METHOD /vpcoe/if_xls_processing~adjust_file_saving.

  IF iv_background = abap_true.
    " Call custom logic for data saving
    cl_vsi=>itab_to_xstring(
       EXPORTING
         it_itab    = ct_xls_data
       IMPORTING
         ef_xstring = DATA(lv_string) ).

    OPEN DATASET cv_file_path FOR OUTPUT IN BINARY MODE.

    IF sy-subrc <> 0.
      io_log->add_sy_msg( ).
      RETURN.
    ENDIF.
    TRANSFER lv_string TO cv_file_path.
    CLOSE DATASET cv_file_path.

    ev_skip_standard_process = abap_true.
  ENDIF.

ENDMETHOD.
ENDCLASS.
