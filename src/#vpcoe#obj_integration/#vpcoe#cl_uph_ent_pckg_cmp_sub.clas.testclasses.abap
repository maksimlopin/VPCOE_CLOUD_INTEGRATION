*CLASS t/vpcoe/cl_uph_ent_pckg_cmp_sub DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*.
*  PRIVATE SECTION.
*    DATA:
*      f_cut TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_sub.  "class under test
*
*    METHODS: setup.
*    METHODS: teardown.
*    METHODS: is_marked_deleted FOR TESTING.
*    METHODS: to_console FOR TESTING.
*    METHODS: get_data FOR TESTING.
*ENDCLASS.       "t/vpcoe/cl_uph_ent_pckg_cmp_sub
*
*
*CLASS t/vpcoe/cl_uph_ent_pckg_cmp_sub IMPLEMENTATION.
*
*  METHOD setup.
*
*    DATA ls_data TYPE /vpcoe/s_uph_ent_pack_cmp_subitm.
*    DATA lv_deleted TYPE abap_bool.
*
*    ls_data   = VALUE  #( packagingelementdisplayid = 'SUB-01'
*                          quantity = 10 quantityunitofmeasureid = 'EA' ).
*
*    lv_deleted = 'X'.
*    f_cut  = NEW #( is_data = ls_data
*                    iv_deleted = lv_deleted ).
*
*  ENDMETHOD.
*
*
*  METHOD teardown.
*  ENDMETHOD.
*
*
*  METHOD is_marked_deleted.
*
*    DATA lv_mark_deleted TYPE abap_bool.
*
*    lv_mark_deleted = f_cut->/vpcoe/if_uph_entity_data~is_marked_deleted(  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_mark_deleted
*      exp   = 'X'   ).
*
*  ENDMETHOD.
*
*
*  METHOD to_console.
*
*    f_cut->/vpcoe/if_uph_entity_data~to_console(  ).
*
*  ENDMETHOD.
*
*
*  METHOD get_data.
*
*    DATA ls_item_data TYPE /vpcoe/s_uph_ent_pack_cmp_subitm.
*
*    ls_item_data = f_cut->get_data(  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = ls_item_data-packagingelementdisplayid
*      exp   = 'SUB-01'
*    ).
*  ENDMETHOD.
*
*ENDCLASS.
