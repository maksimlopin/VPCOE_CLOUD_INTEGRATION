CLASS lcl_surdp_uph_wrap_mcl_map DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.                                    "#EC INTF_IN_CLASS
    TYPES:
      BEGIN OF ty_map_key_value_pair,
        objek        TYPE cuobn,
        matnr        TYPE matnr,
        mcl_versions TYPE /vpcoe/uph_wrap_mcl_version,
      END OF ty_map_key_value_pair,
      ty_t_map_key_value_pair TYPE HASHED TABLE OF ty_map_key_value_pair WITH UNIQUE KEY objek.

    METHODS constructor           IMPORTING it_map           TYPE ty_t_map_key_value_pair OPTIONAL.

    METHODS add_version           IMPORTING iv_mcl_version   TYPE REF TO /vpcoe/cl_uph_wrap_mcl_version.

    METHODS get_mat_class_wrapper RETURNING VALUE(rv_result) TYPE /vpcoe/uph_wrap_mcl.

    METHODS get_internal_map      RETURNING VALUE(rv_result) TYPE ty_t_map_key_value_pair.

  PRIVATE SECTION.
    DATA mt_map TYPE ty_t_map_key_value_pair.

ENDCLASS.



CLASS lcl_surdp_uph_wrap_mcl_map IMPLEMENTATION.
  METHOD constructor.
    IF it_map IS SUPPLIED.
      mt_map = it_map.
    ENDIF.
  ENDMETHOD.


  METHOD add_version.
    READ TABLE mt_map REFERENCE INTO DATA(lv_map_entry) WITH KEY objek = iv_mcl_version->get_product_id( ).

    IF sy-subrc = 0 AND lv_map_entry IS NOT INITIAL.
      APPEND iv_mcl_version TO lv_map_entry->mcl_versions.
    ELSE.
      DATA lo_mcl_version TYPE ty_map_key_value_pair.
      lo_mcl_version-objek        = iv_mcl_version->get_objek_id( ).
      lo_mcl_version-matnr        = iv_mcl_version->get_product_id( ).
      lo_mcl_version-mcl_versions = VALUE #( ( iv_mcl_version ) ).
      INSERT lo_mcl_version INTO TABLE mt_map.
    ENDIF.
  ENDMETHOD.


  METHOD get_mat_class_wrapper.
    LOOP AT mt_map INTO DATA(lv_key_value_pair).

      DATA(lo_mcl_wrapper) = NEW /vpcoe/cl_uph_wrap_mcl( iv_objek        = lv_key_value_pair-objek
                                                         iv_matnr        = lv_key_value_pair-matnr
                                                         it_mcl_versions = lv_key_value_pair-mcl_versions ).

      APPEND lo_mcl_wrapper TO rv_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_internal_map.
    rv_result = mt_map.
  ENDMETHOD.
ENDCLASS.
