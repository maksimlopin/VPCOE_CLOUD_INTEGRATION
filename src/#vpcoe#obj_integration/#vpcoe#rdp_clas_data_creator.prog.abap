*&---------------------------------------------------------------------*
*& Report  /VPCOE/RDP_CLAS_DATA_CREATOR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/rdp_clas_data_creator.

INCLUDE /vpcoe/version.

DATA lv_classi_prefix TYPE string.
DATA lv_charac_prefix TYPE string.
DATA lv_default_cls_prfx TYPE char4.
DATA lv_default_chr_prfx TYPE char4.
DATA lo_data_creator TYPE REF TO /vpcoe/cl_rdp_clas_data_create.
TABLES : sscrfields.

*SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE TEXT-001.
*  PARAMETERS: p_test TYPE abap_bool AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN END OF BLOCK mode.

SELECTION-SCREEN BEGIN OF BLOCK prefix WITH FRAME TITLE text-002.
PARAMETERS: p_clspre TYPE char4,
            p_chrpre TYPE char4.
SELECTION-SCREEN COMMENT /1(79) comm1.
SELECTION-SCREEN END OF BLOCK prefix.

SELECTION-SCREEN BEGIN OF BLOCK operation_scope WITH FRAME TITLE text-003.
PARAMETERS: p_specdb TYPE char1 RADIOBUTTON GROUP scop DEFAULT 'X' USER-COMMAND fcode1.
SELECTION-SCREEN COMMENT /1(79) text-018.
PARAMETERS: p_matcls TYPE char1 RADIOBUTTON GROUP scop.
SELECTION-SCREEN COMMENT /1(79) text-017.
PARAMETERS: p_bom    TYPE char1 RADIOBUTTON GROUP scop.
SELECTION-SCREEN COMMENT /1(79) text-019.
PARAMETERS: p_plst TYPE char1 RADIOBUTTON GROUP scop,
            p_cust TYPE char1 RADIOBUTTON GROUP scop.
SELECTION-SCREEN END OF BLOCK operation_scope.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    comm1 = text-011.

    IF p_specdb = abap_true.
      p_clspre = 'ZRDP'.
      p_chrpre = 'ZRDP'.
    ELSEIF p_matcls = abap_true.
      p_clspre = 'ZMCL'.
      p_chrpre = 'ZRDP'.
    ELSEIF p_plst = abap_true.
      p_clspre = 'ZRDP'.
      p_chrpre = 'ZRDP'.
    ELSEIF p_cust = abap_true.
      p_clspre = 'ZCCL'.
      p_chrpre = 'ZRDP'.

    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

INITIALIZATION.

  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD'  FIELD /vpcoe/cl_rdp_clas_data_create=>gc_tcode.

  IF sy-subrc <> 0.
    WRITE: / text-012.
    LEAVE LIST-PROCESSING.
  ENDIF.

  INCLUDE /vpcoe/version_set.

START-OF-SELECTION.
  DATA lv_scope TYPE string.

  IF p_specdb = abap_true.
    lv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_specdb.
  ELSEIF p_matcls = abap_true.
    lv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_matcls.
  ELSEIF p_bom = abap_true.
    lv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_main_and_subordinate.
  ELSEIF p_plst = abap_true.
    lv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_plst.
  ELSEIF p_cust = abap_true.
    lv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_cust.
  ENDIF.

  IF p_clspre IS INITIAL OR
     p_chrpre IS INITIAL.
    MESSAGE text-004 TYPE 'E' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  lo_data_creator = NEW /vpcoe/cl_rdp_clas_data_create( iv_charac_prefix = p_chrpre
                                                        iv_classi_prefix = p_clspre
                                                        iv_scope         = lv_scope ).

  PERFORM f_main_processing.

FORM f_main_processing.

  lo_data_creator->check_existing_classi_charact( ).
  lo_data_creator->create_or_update_char_and_clas( ).

  lo_data_creator->get_fails_on_update_or_create( IMPORTING et_fail_create_char = DATA(lt_fail_create_char)
                                                            et_fail_create_clas = DATA(lt_fail_create_clas)
                                                            et_fail_update_clas = DATA(lt_fail_update_clas)
                                                            et_fail_update_char = DATA(lt_fail_update_char) ).
  IF lt_fail_create_clas IS NOT INITIAL.
    WRITE / '======================================================================================='.
    WRITE / text-008.
    LOOP AT lt_fail_create_clas REFERENCE INTO DATA(lr_fail_create_clas).
      WRITE / lr_fail_create_clas->*.
    ENDLOOP.
  ENDIF.

  IF lt_fail_update_clas IS NOT INITIAL.
    WRITE / '======================================================================================='.
    WRITE / text-021.
    LOOP AT lt_fail_update_clas REFERENCE INTO DATA(lr_fail_update_clas).
      WRITE / lr_fail_update_clas->*.
    ENDLOOP.
  ENDIF.

  IF lt_fail_create_char IS NOT INITIAL.
    WRITE / '======================================================================================='.
    WRITE / text-009.
    LOOP AT lt_fail_create_char REFERENCE INTO DATA(lr_fail_create_char).
      WRITE / lr_fail_create_char->*.
    ENDLOOP.
  ENDIF.

  IF lt_fail_update_char IS NOT INITIAL.
    WRITE / '======================================================================================='.
    WRITE / text-023.
    LOOP AT lt_fail_update_char REFERENCE INTO DATA(lr_fail_update_char).
      WRITE / lr_fail_update_char->*.
    ENDLOOP.
  ENDIF.

  lo_data_creator->get_success_update_or_create( IMPORTING et_created_char = DATA(lt_created_char)
                                               et_created_clas = DATA(lt_created_clas)
                                               et_updated_clas = DATA(lt_updated_clas)
                                               et_updated_char = DATA(lt_updated_char) ).
  IF lt_created_clas IS NOT INITIAL.
    WRITE / '======================================================================================='.
    WRITE: / text-006.
    LOOP AT lt_created_clas REFERENCE INTO DATA(lr_created_clas).
      WRITE: / lr_created_clas->class_number.
    ENDLOOP.
  ENDIF.

  IF lt_updated_clas IS NOT INITIAL.
    WRITE: / '======================================================================================='.
    WRITE: / text-020.
    LOOP AT lt_updated_clas REFERENCE INTO DATA(lr_updated_clas).
      WRITE: / lr_updated_clas->class_number.
    ENDLOOP.
  ENDIF.

  IF lt_created_char IS NOT INITIAL.
    WRITE: / '======================================================================================='.
    WRITE: / text-007.
    LOOP AT lt_created_char REFERENCE INTO DATA(lr_created_char).
      WRITE: / lr_created_char->char_name.
*      WRITE: / lr_created_char->*.
    ENDLOOP.
  ENDIF.

  IF lt_updated_char IS NOT INITIAL.
    WRITE: / '======================================================================================='.
    WRITE: / text-022.
    LOOP AT lt_updated_char REFERENCE INTO DATA(lr_updated_char).
      WRITE: / lr_updated_char->char_name.
*      WRITE: / lr_updated_char->*.
    ENDLOOP.
  ENDIF.
ENDFORM.

"local test double
CLASS ltd_creator DEFINITION FOR TESTING INHERITING FROM /vpcoe/cl_rdp_clas_data_create.
  PUBLIC SECTION.
    DATA mv_checked TYPE abap_bool.
    DATA mv_created TYPE abap_bool.
    DATA mv_got_created TYPE abap_bool.
    DATA mv_got_updated TYPE abap_bool.
    DATA mv_got_uncreated TYPE abap_bool.
    DATA mv_update_failed TYPE abap_bool.
    METHODS check_existing_classi_charact REDEFINITION.
    METHODS create_or_update_char_and_clas REDEFINITION.
    METHODS get_success_update_or_create REDEFINITION.
    METHODS get_fails_on_update_or_create REDEFINITION.
ENDCLASS.
CLASS ltd_creator IMPLEMENTATION.
  METHOD check_existing_classi_charact.
    mv_checked = abap_true.
  ENDMETHOD.
  METHOD create_or_update_char_and_clas.
    mv_created = abap_true.
  ENDMETHOD.
  METHOD get_success_update_or_create.
    mv_got_created = abap_true.
    mv_got_updated = abap_true.
  ENDMETHOD.
  METHOD get_fails_on_update_or_create.
    mv_got_uncreated = abap_true.
  ENDMETHOD.
ENDCLASS.


"local test class
CLASS lcl_test DEFINITION FOR TESTING INHERITING FROM cl_aunit_assert
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_data_creator TYPE REF TO ltd_creator.

    METHODS setup.
    METHODS teardown.

    METHODS test_main_processing FOR TESTING.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    mo_data_creator = NEW #( iv_charac_prefix = 'ZABC' iv_classi_prefix = 'ZABC' iv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_specdb ).
    lo_data_creator = mo_data_creator.
  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_data_creator.
  ENDMETHOD.

  METHOD test_main_processing.
    "given
    "when
    PERFORM f_main_processing.
    "then
    assert_equals( act = mo_data_creator->mv_checked exp = 'X' ).
    assert_equals( act = mo_data_creator->mv_created exp = 'X' ).
    assert_equals( act = mo_data_creator->mv_got_created exp = 'X' ).
    assert_equals( act = mo_data_creator->mv_got_uncreated exp = 'X' ).
    assert_equals( act = mo_data_creator->mv_got_updated exp = 'X' ).

  ENDMETHOD.

ENDCLASS.
