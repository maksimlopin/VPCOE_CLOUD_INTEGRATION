*&---------------------------------------------------------------------*
*& Report R_PCKF_POSTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /vpcoe/r_pckf_posting.

INCLUDE /vpcoe/version.

CONSTANTS:
  gc_ucomm_onli TYPE syst_ucomm VALUE 'ONLI',
  gc_tcode      TYPE syst_tcode VALUE '/VPCOE/PCKF_POSTING'.

TABLES: sscrfields.

DATA lo_exec_posting TYPE REF TO /vpcoe/cl_pckf_exec_posting.

INCLUDE /vpcoe/pckf_sel_scr_posting.

INITIALIZATION.
  lo_exec_posting = NEW /vpcoe/cl_pckf_exec_posting( ).

  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD'  FIELD gc_tcode.
  IF sy-subrc <> 0.
    MESSAGE e010(/vpcoe/pckf) WITH gc_tcode.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM f_determine_available.
  INCLUDE /vpcoe/version_set.

START-OF-SELECTION.
  PERFORM f_main_processing.


FORM f_main_processing.                                    "#EC CI_FORM

  lo_exec_posting->/vpcoe/if_pckf_exec_posting~execute_posting( is_parameters = VALUE /vpcoe/s_pckf_posting_input( package_size = /vpcoe/if_pckf_entity_proc=>gc_defaults-posting_package_size
                                                                                                                   test_mode             = p_test
                                                                                                                   failed_retention_days = p_failrp )
                                                                iv_test = p_test ).
ENDFORM.

FORM f_determine_available.

  DATA(lv_available) = 0.
  DATA(lo_cache) = /vpcoe/cl_pckf_factory=>get_instance( )->get_entity_cache( ).

  DATA(lt_references) = lo_cache->get_references( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee ).
  LOOP AT lt_references REFERENCE INTO DATA(lr_reference).
    ADD lr_reference->rec_cnt TO lv_available.
  ENDLOOP.

  t_cnt = |{ lv_available }|.

ENDFORM.
