*"* use this source file for your ABAP unit test classes

CLASS /vpcoe/tcl_Uph_Ent_Pckg_CmpIte DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      f_Cut TYPE REF TO /vpcoe/cl_Uph_Ent_Pckg_Cmp_Itm.  "class under test

    CLASS-METHODS: class_Setup.
    CLASS-METHODS: class_Teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: is_Marked_Deleted FOR TESTING.
    METHODS: to_Console FOR TESTING.
    METHODS: get_Data FOR TESTING.
ENDCLASS.       "tcl_Surdp_Uph_Ent_Pckg_Cmp_Ite


CLASS /vpcoe/tcl_Uph_Ent_Pckg_CmpIte IMPLEMENTATION.

  METHOD class_Setup.



  ENDMETHOD.


  METHOD class_Teardown.



  ENDMETHOD.


  METHOD setup.

    DATA ls_data TYPE /vpcoe/s_Uph_Ent_Pack_Cmp_Item.
    DATA lv_deleted TYPE abap_Bool.
    ls_data   = VALUE  #( packagingelementdisplayid = 'Maggi' packagingelementvalidfrom = '2020.02.20' levelcode = 'level'
                          quantity = 10 quantityunitofmeasureid = 'EA' wwfgroup ='WW' eprgroup ='EP' ).

    f_Cut  = NEW #( is_data = ls_data
                    iv_deleted = lv_deleted ).

  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD is_Marked_Deleted.

    DATA lv_Mark_Deleted TYPE abap_Bool.

    lv_Mark_Deleted = f_Cut->/vpcoe/if_Uph_Entity_Data~is_Marked_Deleted(  ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act   = lv_Mark_Deleted
      exp   = ''
    ).
  ENDMETHOD.


  METHOD to_Console.


    f_Cut->/vpcoe/if_Uph_Entity_Data~to_Console(  ).

  ENDMETHOD.


  METHOD get_Data.

    DATA ls_Item_Data TYPE /vpcoe/s_Uph_Ent_Pack_Cmp_Item.

    ls_Item_Data = f_Cut->get_Data(  ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act   = ls_Item_Data-packagingelementdisplayid
      exp   = 'Maggi'
    ).
  ENDMETHOD.




ENDCLASS.
