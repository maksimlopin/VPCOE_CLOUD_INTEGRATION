CLASS tcl_surdp_uph_ent_pckg_frac DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO /vpcoe/cl_ent_pckg_fraction.  "class under test

    METHODS: setup.
    METHODS: teardown.
    METHODS: is_marked_deleted FOR TESTING.
    METHODS: to_console FOR TESTING.
    METHODS: get_data FOR TESTING.

ENDCLASS.       "tcl_Surdp_Uph_Ent_Pckg_Frac


CLASS tcl_surdp_uph_ent_pckg_frac IMPLEMENTATION.

  METHOD setup.

    DATA: ls_data    TYPE /vpcoe/s_uph_ent_pack_frac,
          lv_deleted TYPE abap_bool.

    "Packaging Elements Fraction
    ls_data-basicmaterialfractionid = 'BOX'.
    ls_data-weight = '10'.
    ls_data-weightunitofmeasure = 'KG'.
    ls_data-color = 'RED'.

    lv_deleted = 'X'.
    f_cut =  NEW /vpcoe/cl_ent_pckg_fraction( is_data = ls_data
                                              iv_deleted   = lv_deleted ).

  ENDMETHOD.


  METHOD teardown.

    FREE f_cut.

  ENDMETHOD.


  METHOD is_marked_deleted.

    DATA: lv_mark_deleted_act TYPE abap_bool,
          lv_mark_deleted_exp TYPE abap_bool.

    lv_mark_deleted_act = f_cut->/vpcoe/if_uph_entity_data~is_marked_deleted(  ).
    lv_mark_deleted_exp = 'X'.

    cl_abap_unit_assert=>assert_equals(
      act   = lv_mark_deleted_act
      exp   = lv_mark_deleted_exp   ).

  ENDMETHOD.


  METHOD to_console.

    f_cut->/vpcoe/if_uph_entity_data~to_console(  ).

  ENDMETHOD.


  METHOD get_data.

    DATA: ls_exp_data TYPE /vpcoe/s_uph_ent_pack_frac,
          ls_act_data TYPE /vpcoe/s_uph_ent_pack_frac.

    ls_act_data = f_cut->get_data(  ).

    "Packaging Elements Fraction
    ls_exp_data-basicmaterialfractionid = 'BOX'.
    ls_exp_data-weight = '10'.
    ls_exp_data-weightunitofmeasure = 'KG'.
    ls_exp_data-color = 'RED'.

    cl_abap_unit_assert=>assert_equals(
      act   = ls_act_data
      exp   = ls_exp_data  ).

  ENDMETHOD.


ENDCLASS.
