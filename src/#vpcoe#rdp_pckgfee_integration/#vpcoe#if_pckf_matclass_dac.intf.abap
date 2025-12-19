INTERFACE /vpcoe/if_pckf_matclass_dac
  PUBLIC .


  TYPES:
    "! Characteristic value parameter type definition
    BEGIN OF gtys_characteristic_value,
      characteristic     TYPE atnam,
      value              TYPE atwrt,
      value_char         TYPE	atwrt,
      value_char_neutral TYPE atwrt,
      value_num          TYPE atflv,
      unit_num           TYPE meins,
      value_curr         TYPE atflv,
      value_to           TYPE atflb,
      data_type          TYPE atfor,
    END OF gtys_characteristic_value .
  TYPES:
    gtyt_characteristic_value TYPE STANDARD TABLE OF gtys_characteristic_value .

  CONSTANTS:
    BEGIN OF cs_characteristic_type,
      char TYPE atfor VALUE 'CHAR',
      curr TYPE atfor VALUE 'CURR',
      date TYPE atfor VALUE 'DATE',
      num  TYPE atfor VALUE 'NUM',
      time TYPE atfor VALUE 'TIME',
      udef TYPE atfor VALUE 'UDEF',
    END OF cs_characteristic_type .

  "! Updates the classification values of a material.
  "! The material to update and the used key date or change number must be set.
  "! The class and the characteristic values must be given by parameter iv_class and it_characteristics.
  "! If the update fails, the failed status is returned as abap_true.
  METHODS update_classification
    IMPORTING
      !iv_material        TYPE matnr
      !iv_keydate         TYPE dats DEFAULT sy-datum
      !iv_changenum       TYPE aennr OPTIONAL
      !iv_class           TYPE klasse_d
      !it_characteristics TYPE gtyt_characteristic_value
      !iv_commit          TYPE abap_bool DEFAULT abap_false
    EXPORTING
      !ev_failed          TYPE abap_bool
      !et_messages        TYPE /vpcoe/t_uph_msg .
  "! Creates a change number. If external numbering is allowed, the number can be passed via parameter iv_change_no.
  "! Otherwise the number will be generated automatically and passed back in parameter ev_changenum.
  "! In case the creation fails, the parameter ev_failed will be set to abap_true.
  "! The creation of a change number requires a commit work otherwise it won't be visible. Using the parmeter iv_commit this
  "! commit work (and wait) will be triggered.
  METHODS create_change_number
    IMPORTING
      !iv_change_no   TYPE aennr OPTIONAL
      !iv_valid_from  TYPE dats
      !iv_auth_group  TYPE cc_aenbe OPTIONAL
      !iv_description TYPE aetxt OPTIONAL
      !iv_commit      TYPE abap_bool DEFAULT abap_false
    EXPORTING
      !ev_changenum   TYPE aennr
      !ev_failed      TYPE abap_bool
      !et_messages    TYPE /vpcoe/t_uph_msg .
  "! Checks if there is already a change number defined with the given number or optional description.
  "! If the change number is not valid, it is indicated by the parameter ev_valid.
  "! If only a change number description is passed, the parameter ev_changenum will be set with the existing number if there exists one with the same.
  METHODS check_change_number
    IMPORTING
      !iv_changenum   TYPE aennr OPTIONAL
      !iv_description TYPE aetxt OPTIONAL
    EXPORTING
      !ev_changenum   TYPE aennr
      !ev_exists      TYPE abap_bool
      !ev_valid       TYPE abap_bool .
  "! Read the classification values of a material
  "! The material to read and the used key date or change number must be set.
  "! The class to read must be given by parameter iv_class and it_characteristics.
  "! If the read fails, the failed status is returned as abap_true.
  METHODS read_classification
    IMPORTING
      !iv_material        TYPE matnr
      !iv_keydate         TYPE dats DEFAULT sy-datum
      !iv_changenum       TYPE aennr OPTIONAL
      !iv_class           TYPE klasse_d
    EXPORTING
      !et_characteristics TYPE gtyt_characteristic_value
      !ev_failed          TYPE abap_bool
      !et_messages        TYPE /vpcoe/t_uph_msg .
ENDINTERFACE.
