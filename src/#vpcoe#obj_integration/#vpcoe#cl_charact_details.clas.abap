class /VPCOE/CL_CHARACT_DETAILS definition
  public
  create public .

public section.

  interfaces /VPCOE/IF_UPH_CHARACT_DETAILS .

  aliases GET_DETAILS_OF_CHARACTERISTIC
    for /VPCOE/IF_UPH_CHARACT_DETAILS~GET_DETAILS_OF_CHARACTERISTIC .
  aliases GET_UOM_OF_CHARACTERISTIC
    for /VPCOE/IF_UPH_CHARACT_DETAILS~GET_UOM_OF_CHARACTERISTIC .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS /VPCOE/CL_CHARACT_DETAILS IMPLEMENTATION.


  METHOD /vpcoe/if_uph_charact_details~get_details_of_characteristic.
    CLEAR rv_result.

    DATA lv_rettab TYPE bapirettab.

    CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
      EXPORTING
        charactname   = iv_characteristic_name
      IMPORTING
        charactdetail = rv_result
      TABLES
        return        = lv_rettab.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_charact_details~get_uom_of_characteristic.
    DATA(lv_details) = get_details_of_characteristic( iv_characteristic_name ).

    IF lv_details IS NOT INITIAL.
      rv_result = lv_details-unit_of_measurement.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
