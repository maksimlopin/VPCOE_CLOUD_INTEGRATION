class /VPCOE/CL_UPH_WRAP_MCL definition
  public
  create public .

public section.       "#EC INTF_IN_CLASS

  methods CONSTRUCTOR
    importing
      !IV_OBJEK type CUOBN
      !IV_MATNR type MATNR
      !IT_MCL_VERSIONS type /VPCOE/UPH_WRAP_MCL_VERSION .
  methods GET_OBJEK_ID
    returning
      value(RV_RESULT) type CUOBN .
    "! Return the object key
  methods GET_PRODUCT_ID
    returning
      value(RV_RESULT) type CUOBN .
    "! Get characteristic value as character sequence from Characteristic Value Assignment table.
  methods GET_PRODUCT_DESCRIPTION
    returning
      value(RV_RESULT) type CUOBN .
    "! Return all material classification versions belonging to the object key
  methods GET_VERSIONS
    returning
      value(RV_RESULT) type /VPCOE/UPH_WRAP_MCL_VERSION .
    "! Return the material classification version by class and key date belonging to the object key
  methods GET_VERSIONS_BY_CLASS
    importing
      !IV_CLASS type KLASSE_D
      !IV_VALID_FROM type DATUV optional
    returning
      value(RV_RESULT) type /VPCOE/UPH_WRAP_MCL_VERSION .
    "! Add a material classification version belonging to the object key
  methods ADD_VERSION
    importing
      !IO_MCL_VERSION type ref to /VPCOE/CL_UPH_WRAP_MCL_VERSION .
  PROTECTED SECTION.

    METHODS call_bapi_material_get_detail IMPORTING iv_matnr       TYPE matnr
                                          EXPORTING es_mat_general TYPE bapimatdoa.

private section.

  data MV_OBJEK type CUOBN .
  data MV_MATNR type MATNR .
  data MT_MCL_VERSIONS type /VPCOE/UPH_WRAP_MCL_VERSION .
ENDCLASS.



CLASS /VPCOE/CL_UPH_WRAP_MCL IMPLEMENTATION.


  METHOD ADD_VERSION.
    " Adds a material classification version to the internal table
    CHECK io_mcl_version IS BOUND.

    APPEND io_mcl_version TO mt_mcl_versions.
  ENDMETHOD.


  METHOD call_bapi_material_get_detail.
    " Call BAPI to get material details

    CLEAR es_mat_general.

*    CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
*      EXPORTING
*        material_long         = iv_matnr
*      IMPORTING
*        material_general_data = es_mat_general.

    CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
      EXPORTING
        material              = iv_matnr
      IMPORTING
        material_general_data = es_mat_general.

  ENDMETHOD.


  METHOD CONSTRUCTOR.
    mv_objek = iv_objek.
    mv_matnr = iv_matnr.
    mt_mcl_versions = it_mcl_versions.
  ENDMETHOD.


  method GET_OBJEK_ID.
    rv_result = mv_objek.
  endmethod.


  METHOD GET_PRODUCT_DESCRIPTION.

    call_bapi_material_get_detail( EXPORTING iv_matnr       = mv_matnr
                                   IMPORTING es_mat_general = DATA(lv_material_details) ).

    rv_result = lv_material_details-matl_desc.
  ENDMETHOD.


  METHOD GET_PRODUCT_ID.
    rv_result = mv_matnr.
  ENDMETHOD.


  METHOD GET_VERSIONS.
    rv_result = mt_mcl_versions.
  ENDMETHOD.


  METHOD get_versions_by_class.
    " Retrieve all versions according a specific class and optional valid for a key date

    TYPES: BEGIN OF ltys_wrap_mcl_version_sorted,
             objek  TYPE cuobn,
             datuv  TYPE datuv,
             datub  TYPE datub,
             refmcl TYPE REF TO /vpcoe/cl_uph_wrap_mcl_version,
           END OF ltys_wrap_mcl_version_sorted.

    DATA: lt_sorted_versions  TYPE STANDARD TABLE OF ltys_wrap_mcl_version_sorted,
          lr_previous_version TYPE REF TO ltys_wrap_mcl_version_sorted.

    LOOP AT mt_mcl_versions INTO DATA(ls_version).

      ls_version->get_internal_data( IMPORTING es_mcl_header = DATA(ls_mcl_header) ).

      IF ls_mcl_header-class <> iv_class.
        CONTINUE.
      ENDIF.

      APPEND ls_version TO rv_result.

      INSERT VALUE #( objek = ls_version->get_objek_id(  ) datuv = ls_version->get_valid_from(  ) datub = '99991231' refmcl = ls_version ) INTO TABLE lt_sorted_versions REFERENCE INTO DATA(lr_current_version).
    ENDLOOP.

      IF iv_valid_from IS SUPPLIED.
      CLEAR rv_result.

      SORT lt_sorted_versions BY objek datuv ASCENDING.

      LOOP AT lt_sorted_versions REFERENCE INTO lr_current_version.

        IF lr_previous_version IS BOUND.
          lr_previous_version->datub = lr_current_version->datuv - 1.
        ENDIF.

        lr_previous_version = lr_current_version.
      ENDLOOP.

      LOOP AT lt_sorted_versions REFERENCE INTO lr_current_version.

        IF iv_valid_from >= lr_current_version->datuv AND iv_valid_from <= lr_current_version->datub.
          APPEND lr_current_version->refmcl TO rv_result.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
