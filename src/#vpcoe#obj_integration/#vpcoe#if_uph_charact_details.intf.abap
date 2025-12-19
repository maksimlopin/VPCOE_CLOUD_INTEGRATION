interface /VPCOE/IF_UPH_CHARACT_DETAILS
  public .


    "! Get unit of measure that is defined for the characteristic.
  methods GET_UOM_OF_CHARACTERISTIC
    importing
      !IV_CHARACTERISTIC_NAME type ATNAM
    returning
      value(RV_RESULT) type MSEHI .
    "! Call BAPI to get characteristics details
  methods GET_DETAILS_OF_CHARACTERISTIC
    importing
      !IV_CHARACTERISTIC_NAME type ATNAM
    returning
      value(RV_RESULT) type BAPICHARACTDETAIL .
endinterface.
