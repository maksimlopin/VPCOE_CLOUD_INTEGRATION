class /VPCOE/CL_UPH_WRAP_MCL_VERSION definition
  public
  create public .

public section.       "#EC INTF_IN_CLASS

  methods CONSTRUCTOR
    importing
      !IS_MCL_HEADER type /VPCOE/IF_UPH_ENTITY_MCL_PROC=>GTY_MAT_CLASS_PARAMETER
      !IT_MCL_CHARACTER_DATA type TT_BAPI1003_ALLOC_VALUES_CHAR
      !IT_MCL_NUMBER_DATA type TT_BAPI1003_ALLOC_VALUES_NUM
      !IT_MCL_CURRENCY_DATA type TT_BAPI1003_ALLOC_VALUES_CURR
      !IO_CHARACTERISTIC_DETAILS type ref to /VPCOE/IF_UPH_CHARACT_DETAILS optional .
    "! Get characteristic value as character sequence from Characteristic Value Assignment table.
  methods GET_CHARACTER_VAL
    importing
      !IV_NAME_CHAR type ATNAM
    returning
      value(RV_RESULT) type ATWRT30 .
    "! Get characteristics neutral value from character data
  methods GET_NEUTRAL_VAL
    importing
      !IV_NAME_CHAR type ATNAM
    returning
      value(RV_RESULT) type ATWRT70 .
    "! Get characteristic value as Boolean (X, _).
  methods GET_BOOLEAN_VAL                "#EC METH_RET_BOOL
    importing
      !IV_NAME_CHAR type ATNAM
    returning
      value(RV_RESULT) type ABAP_BOOL .            "#EC METH_RET_BOOL
    "! Get characteristic value as number simple from Characteristic Value Assignment table.
  methods GET_NUMBER_VAL
    importing
      !IV_NAME_CHAR type ATNAM
    returning
      value(RV_RESULT) type ATFLV .
    "! Get characteristic value as number simple from Characteristic Value Assignment table.
  methods GET_NUMBER_RANGE
    importing
      !IV_NAME_CHAR type ATNAM
    exporting
      !EV_NUM_VALUE_FROM type ATFLV
      !EV_NUM_VALUE_TO type ATFLB .
    "! Get characteristic value as number simple from Characteristic Value Assignment table.
  methods GET_QUANTITY
    importing
      !IV_NAME_CHAR type ATNAM
    exporting
      !EV_VALUE type ATFLV
      !EV_UNIT type MEINS .
    "! Get characteristic value as number simple from Characteristic Value Assignment table.
  methods GET_QUANTITY_VAL
    importing
      !IV_NAME_CHAR type ATNAM
    returning
      value(RV_RESULT) type ATFLV .
    "! Extract unit of measure from Characteristic Value Assignment table
  methods GET_QUANTITY_UNIT
    importing
      !IV_NAME_CHAR type ATNAM
    returning
      value(RV_RESULT) type MEINS .
    "! Get characteristic value as number with extend output.
  methods GET_QUANTITY_RANGE
    importing
      !IV_NAME_CHAR type ATNAM
    exporting
      !EV_QUANTITY_VALUE_FROM type ATFLV
      !EV_QUANTITY_UNIT_FROM type MEINS
      !EV_QUANTITY_VALUE_TO type ATFLB
      !EV_QUANTITY_UNIT_TO type MEINS .
    "! Get characteristic value as number with extend output.
  methods GET_CURRENCY
    importing
      !IV_NAME_CHAR type ATNAM
    exporting
      !EV_CURRENCY_VALUE_FROM type ATFLV
      !EV_CURRENCY_FROM type WAERS
      !EV_CURRENCY_VALUE_TO type ATFLB
      !EV_CURRENCY_TO type WAERS .
  methods GET_OBJEK_ID
    returning
      value(RV_RESULT) type CUOBN .
    "! Get characteristic value as character sequence from Characteristic Value Assignment table.
  methods GET_PRODUCT_ID
    returning
      value(RV_RESULT) type CUOBN .
    "! Get the valid from
  methods GET_VALID_FROM
    returning
      value(RV_RESULT) type DATUV .
    "! Get delete indicator from header structure.
  methods IS_MARKED_DELETED
    returning
      value(RV_RESULT) type ABAP_BOOL .
    "! Get unit of measure that is defined for the characteristic.
  methods GET_UOM_OF_CHARACTERISTIC
    importing
      !IV_CHARACTERISTIC_NAME type ATNAM
    returning
      value(RV_RESULT) type MSEHI .
    "! Provides the internal data structures for external consumption.
  methods GET_INTERNAL_DATA
    exporting
      !ES_MCL_HEADER type /VPCOE/IF_UPH_ENTITY_MCL_PROC=>GTY_MAT_CLASS_PARAMETER
      !ET_MCL_CHARACTER_DATA type TT_BAPI1003_ALLOC_VALUES_CHAR
      !ET_MCL_NUMBER_DATA type TT_BAPI1003_ALLOC_VALUES_NUM
      !ET_MCL_CURRENCY_DATA type TT_BAPI1003_ALLOC_VALUES_CURR .
  PROTECTED SECTION.

    METHODS call_bapi_charact_getdetail IMPORTING iv_characteristic_name TYPE atnam
                                        RETURNING VALUE(rv_details)      TYPE bapicharactdetail.

private section.

  data MS_MCL_HEADER type /VPCOE/IF_UPH_ENTITY_MCL_PROC=>GTY_MAT_CLASS_PARAMETER .
  data MT_MCL_CHARACTER_DATA type /VPCOE/TT_BAPI1003_ALLOC_VAL_T .
  data MT_MCL_NUMBER_DATA type TT_BAPI1003_ALLOC_VALUES_NUM .
  data MT_MCL_CURRENCY_DATA type TT_BAPI1003_ALLOC_VALUES_CURR .
  data MO_CHARACTERISTIC_DETAILS type ref to /VPCOE/IF_UPH_CHARACT_DETAILS .
ENDCLASS.



CLASS /VPCOE/CL_UPH_WRAP_MCL_VERSION IMPLEMENTATION.


  METHOD CALL_BAPI_CHARACT_GETDETAIL.
    " Call BAPI to get characteristics details

*    CLEAR rv_details.
*
*    DATA lv_rettab TYPE bapirettab.
*
*    CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
*      EXPORTING charactname   = iv_characteristic_name
*      IMPORTING charactdetail = rv_details
*      TABLES    return        = lv_rettab.
  ENDMETHOD.


  METHOD constructor.
    ms_mcl_header = is_mcl_header.
    mt_mcl_character_data = it_mcl_character_data.
    mt_mcl_number_data = it_mcl_number_data.
    mt_mcl_currency_data = it_mcl_currency_data.

    IF io_characteristic_details IS BOUND.
      mo_characteristic_details = io_characteristic_details.
    ELSE.
      mo_characteristic_details = /vpcoe/cl_uph_factory=>get_instance( )->get_characteristic_details( ).
    ENDIF.
*    DATA:
*      lv_object_key   TYPE /vpcoe/ehs_objnum,
*      lt_va_char_data TYPE /vpcoe/t_ehs_bapi1077pr,
*      ls_va_usage     TYPE bapi1077du,
*      lt_va_comp_data TYPE /vpcoe/t_bapi1077vp,
*      ls_material     TYPE /vpcoe/s_uph_ent_pack_prod.
*
*    ms_header_data = is_header_data.
*
*    LOOP AT it_vat_header REFERENCE INTO DATA(lr_vat_header) WHERE recno_root = is_header_data-recno_root.
*
*      LOOP AT it_va_instances REFERENCE INTO DATA(lr_va_instances)
*        WHERE recno_root = lr_vat_header->recno_root AND ref_recnvh = lr_vat_header->record_no.
*
*        CLEAR ls_va_usage.
*        READ TABLE it_va_usage WITH KEY ref_recn = lr_va_instances->record_no INTO ls_va_usage.
*
*        CLEAR lt_va_char_data.
*
*        " Create the object key to read the characteristic values
*        lv_object_key = |{ lr_va_instances->record_no }{ lr_va_instances->chngstatus }|.
*
*        LOOP AT it_va_char_data REFERENCE INTO DATA(lr_va_char_data) WHERE obj_key = lv_object_key.
*
*          INSERT lr_va_char_data->* INTO TABLE lt_va_char_data.
*
*        ENDLOOP.
*
*        " Read composition data
*        CLEAR lt_va_comp_data.
*        LOOP AT it_va_comp_data REFERENCE INTO DATA(lr_va_comp_data) WHERE ref_recnva = lr_va_instances->record_no.
*
*          INSERT lr_va_comp_data->* INTO TABLE lt_va_comp_data.
*
*        ENDLOOP.
*
*        DATA(lo_va_data) = NEW cl_surdp_uph_wrap_va( is_vat_header   = lr_vat_header->*
*                                                     is_va_instance  = lr_va_instances->*
*                                                     it_va_char_data = lt_va_char_data
*                                                     is_va_usage     = ls_va_usage
*                                                     it_va_comp_data = lt_va_comp_data
*                                                     it_va_comp_subid = it_va_comp_subid
*                                                     it_mat_join     = it_mat_join ).
*        INSERT lo_va_data INTO TABLE mt_va_data.
*
*      ENDLOOP.
*    ENDLOOP.
*
*    LOOP AT it_mat_join REFERENCE INTO DATA(lr_mat_data)
*          WHERE recno_root = is_header_data-recno_root.
*
*      ls_material-productid = lr_mat_data->material.
*      ls_material-valid_from = lr_mat_data->valid_from.
*      ls_material-valid_to = lr_mat_data->valid_to.
*      INSERT ls_material INTO TABLE mt_mat_data.
*    ENDLOOP.
*
*    LOOP AT it_ident_header INTO DATA(ls_ident_header)
*          WHERE recno_root = is_header_data-recno_root.
*
*      INSERT ls_ident_header INTO TABLE mt_ident_header.
*    ENDLOOP.
  ENDMETHOD.


  METHOD get_boolean_val.
    READ TABLE mt_mcl_character_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).

    IF sy-subrc = 0.
      DATA(lv_charac_value) = lr_va_char_data->value_neutral.

      IF lv_charac_value = 'N'.
        rv_result = abap_false.
      ELSEIF lv_charac_value = 'Y'.
        rv_result = abap_true.
      ELSE.
        rv_result = abap_undefined.
      ENDIF.
    ELSE.
      rv_result = abap_undefined.
    ENDIF.
*    READ TABLE mt_mcl_character_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).
*
*    IF sy-subrc = 0.
*      DATA(lv_charac_value) = lr_va_char_data->value_neutral.
*
*      IF lv_charac_value IS INITIAL OR lv_charac_value = 'N'.
*        rv_result = abap_false.
*      ELSEIF lv_charac_value = 'X' OR lv_charac_value = 'Y'.
*        rv_result = abap_true.
*      ENDIF.
*    ELSE.
*      rv_result = abap_undefined.
*    ENDIF.
  ENDMETHOD.


  METHOD GET_CHARACTER_VAL.
    " Get characteristics value as character sequence
    READ TABLE mt_mcl_character_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).
    IF sy-subrc = 0.
      rv_result = lr_va_char_data->value_neutral."char.
    ENDIF.
  ENDMETHOD.


  METHOD GET_CURRENCY.
    " Get characteristics value as number

    CLEAR: ev_currency_from, ev_currency_to, ev_currency_value_from, ev_currency_value_to.

    READ TABLE mt_mcl_currency_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_currency_data).
    IF sy-subrc = 0.
      ev_currency_value_from = lr_va_currency_data->value_from.
      ev_currency_from       = lr_va_currency_data->currency_from.
      ev_currency_value_to   = lr_va_currency_data->value_to.
      ev_currency_to         = lr_va_currency_data->currency_to.
    ENDIF.
  ENDMETHOD.


  METHOD GET_INTERNAL_DATA.
    " Provides the internal data structures for external consumption

    CLEAR: es_mcl_header, et_mcl_character_data, et_mcl_number_data, et_mcl_currency_data.

    es_mcl_header = ms_mcl_header.
    et_mcl_character_data = mt_mcl_character_data.
    et_mcl_number_data = mt_mcl_number_data.
    et_mcl_currency_data = mt_mcl_currency_data.
  ENDMETHOD.


  METHOD GET_NEUTRAL_VAL.
    " Get characteristics neutral value from character data
    READ TABLE mt_mcl_character_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).
    IF sy-subrc = 0.
      rv_result = lr_va_char_data->value_neutral."value_neutral_long.
    ENDIF.
  ENDMETHOD.


  METHOD GET_NUMBER_RANGE.
    " Get characteristics value as number with from and to values

    CLEAR: ev_num_value_from, ev_num_value_to.

    READ TABLE mt_mcl_number_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).
    IF sy-subrc = 0.
      ev_num_value_from = lr_va_char_data->value_from.
      ev_num_value_to   = lr_va_char_data->value_to.
    ENDIF.
  ENDMETHOD.


  METHOD GET_NUMBER_VAL.
    " Get characteristics value as number simple

    READ TABLE mt_mcl_number_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).
    IF sy-subrc = 0.
      rv_result =  lr_va_char_data->value_from.

    ENDIF.
  ENDMETHOD.


  method GET_OBJEK_ID.
     rv_result = ms_mcl_header-objek.
  endmethod.


  METHOD GET_PRODUCT_ID.
    rv_result = ms_mcl_header-matnr.
  ENDMETHOD.


  METHOD GET_QUANTITY.
    " Get characteristics value as quantity with a value and a unit

    CLEAR: ev_unit, ev_value.

    READ TABLE mt_mcl_number_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).
    IF sy-subrc = 0.
      ev_value =  lr_va_char_data->value_from.
      ev_unit  =  lr_va_char_data->unit_from.
    ENDIF.
  ENDMETHOD.


  METHOD get_quantity_range.
    " Get characteristics value as number with extend output (be aware this might be used in customer implementations)

    CLEAR: ev_quantity_unit_from, ev_quantity_unit_to, ev_quantity_value_from, ev_quantity_value_to.

    READ TABLE mt_mcl_number_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).
    IF sy-subrc = 0.
      ev_quantity_value_from = lr_va_char_data->value_from.
      ev_quantity_unit_from  = lr_va_char_data->unit_from.
      ev_quantity_value_to   = lr_va_char_data->value_to.
      ev_quantity_unit_to    = lr_va_char_data->unit_to.
    ENDIF.
  ENDMETHOD.


  METHOD GET_QUANTITY_UNIT.
    READ TABLE mt_mcl_number_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).
    IF sy-subrc = 0.
      rv_result = lr_va_char_data->unit_from.
    ENDIF.
  ENDMETHOD.


  METHOD GET_QUANTITY_VAL.
    " Get characteristics value as number

    READ TABLE mt_mcl_number_data WITH KEY charact = iv_name_char REFERENCE INTO DATA(lr_va_char_data).
    IF sy-subrc = 0.
      rv_result =  lr_va_char_data->value_from.
    ENDIF.
  ENDMETHOD.


  METHOD get_uom_of_characteristic.
    " Get unit of measure defined for characteristic

*    DATA(lv_details) = call_bapi_charact_getdetail( iv_characteristic_name ).
*
*    IF lv_details IS NOT INITIAL.
*      rv_result = lv_details-unit_of_measurement.
*    ENDIF.
    rv_result = mo_characteristic_details->get_uom_of_characteristic( iv_characteristic_name ).
  ENDMETHOD.


  METHOD GET_VALID_FROM.
    rv_result = ms_mcl_header-datuv.
  ENDMETHOD.


  METHOD IS_MARKED_DELETED.
    rv_result = ms_mcl_header-lvorm.
  ENDMETHOD.
ENDCLASS.
