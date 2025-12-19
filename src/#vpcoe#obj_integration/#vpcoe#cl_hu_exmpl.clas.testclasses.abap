*CLASS ltc_pack_elem_mcl_example DEFINITION FOR TESTING RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    METHODS setup.
*
*    METHODS given_version_for_packelem IMPORTING iv_objek         TYPE cuobn
*                                                 iv_datuv         TYPE datuv    OPTIONAL
*                                                 iv_class         TYPE klasse_d OPTIONAL
*                                                 iv_lvorm         TYPE lvorm    OPTIONAL
*                                       RETURNING VALUE(rv_result) TYPE REF TO cl_surdp_uph_wrap_mcl_version.
*
*    METHODS given_version_for_fraction IMPORTING iv_objek         TYPE cuobn
*                                                 iv_datuv         TYPE datuv    OPTIONAL
*                                                 iv_class         TYPE klasse_d OPTIONAL
*                                                 iv_lvorm         TYPE lvorm    OPTIONAL
*                                                 iv_frac_number   TYPE i
*                                       RETURNING VALUE(rv_result) TYPE REF TO cl_surdp_uph_wrap_mcl_version.
*
*    METHODS get_relevant_mat_clas         FOR TESTING.
*    METHODS map_mat_clas_data             FOR TESTING.
*    METHODS map_mat_clas_data_4_fractions FOR TESTING.
*    METHODS map_mat_clas_data_no_input    FOR TESTING.
*
*    DATA mo_cut                  TYPE REF TO cl_surdp_uph_pckelem_mcl_exmpl.
*    DATA mo_charact_details_mock TYPE REF TO if_surdp_uph_charact_details.
*ENDCLASS.
*
*
*
*CLASS ltc_pack_elem_mcl_example IMPLEMENTATION.
*  METHOD setup.
*    DATA(lo_factory_double) = NEW td_surdp_uph_factory( ).
*    th_surdp_uph_factory_injector=>inject_factory_double( io_double = lo_factory_double ).
*    mo_charact_details_mock = lo_factory_double->get_characteristic_details( ).
*    mo_cut = NEW cl_surdp_uph_pckelem_mcl_exmpl( ).
*  ENDMETHOD.
*
*
*  METHOD given_version_for_packelem.
*    DATA ls_mcl_header         TYPE if_surdp_uph_entity_mcl_proc=>gty_mat_class_parameter.
*    DATA lt_mcl_character_data TYPE tt_bapi1003_alloc_values_char.
*    DATA lt_mcl_number_data    TYPE tt_bapi1003_alloc_values_num.
*    DATA lt_mcl_currency_data  TYPE tt_bapi1003_alloc_values_curr.
*
*    ls_mcl_header-objek = iv_objek.
*    ls_mcl_header-datuv = iv_datuv.
*    ls_mcl_header-class = iv_class.
*    ls_mcl_header-lvorm = iv_lvorm.
*
*    rv_result = NEW #( is_mcl_header         = ls_mcl_header
*                       it_mcl_character_data = lt_mcl_character_data
*                       it_mcl_number_data    = lt_mcl_number_data
*                       it_mcl_currency_data  = lt_mcl_currency_data ).
*  ENDMETHOD.
*
*
*  METHOD given_version_for_fraction.
*    DATA ls_mcl_header         TYPE if_surdp_uph_entity_mcl_proc=>gty_mat_class_parameter.
*    DATA lt_mcl_character_data TYPE tt_bapi1003_alloc_values_char.
*    DATA lt_mcl_number_data    TYPE tt_bapi1003_alloc_values_num.
*    DATA lt_mcl_currency_data  TYPE tt_bapi1003_alloc_values_curr.
*
*    ls_mcl_header-objek = iv_objek.
*    ls_mcl_header-datuv = iv_datuv.
*    ls_mcl_header-class = iv_class.
*    ls_mcl_header-lvorm = iv_lvorm.
*
*    INSERT VALUE #( charact = 'ZRDP_PACKELEM_FRAC' && iv_frac_number && '_CODE' value_neutral_long = '1310' ) INTO TABLE lt_mcl_character_data.
*    INSERT VALUE #( charact = 'ZRDP_PACKELEM_FRAC' && iv_frac_number && '_WEIGHT' value_from = '5' ) INTO TABLE lt_mcl_number_data.
*
*    rv_result = NEW #( is_mcl_header         = ls_mcl_header
*                       it_mcl_character_data = lt_mcl_character_data
*                       it_mcl_number_data    = lt_mcl_number_data
*                       it_mcl_currency_data  = lt_mcl_currency_data ).
*  ENDMETHOD.
*
*
*
*  METHOD get_relevant_mat_clas.
*    " When
*    DATA(lt_act_mat_classes) = mo_cut->if_surdp_uph_entity_mcl_proc~get_relevant_mat_clas( ).
*
*    " Then
*    DATA lt_exp_mat_classes TYPE if_surdp_uph_entity_mcl_proc=>gty_t_mat_class.
*    lt_exp_mat_classes = VALUE #( ( class_number = 'ZMCL_PACKELEM_ATTR' )
*                                  ( class_number = 'ZMCL_PACKELE_FRAC1' )
*                                  ( class_number = 'ZMCL_PACKELE_FRAC2' )
*                                  ( class_number = 'ZMCL_PACKELE_FRAC3' )
*                                  ( class_number = 'ZMCL_PACKELE_FRAC4' ) ).
*    cl_abap_unit_assert=>assert_equals( exp = lt_exp_mat_classes
*                                        act = lt_act_mat_classes ).
*  ENDMETHOD.
*
*
*  METHOD map_mat_clas_data.
*    " Given
*    th_surdp_uph_packaging_elem=>given_input_params( io_cut = mo_cut iv_source_id = 'S4H' ).
*
*    DATA(lo_mat_clas_version_attribute) = given_version_for_packelem( iv_objek = 'TST_1' iv_class = 'ZMCL_PACKELEM_ATTR' ).
*    DATA(lo_mat_clas_version_fraction)  = given_version_for_fraction( iv_objek = 'TST_1' iv_class = 'ZMCL_PACKELE_FRAC1' iv_frac_number = 1 ).
*
*    DATA lt_mcl_versions TYPE surdpt_uph_wrap_mcl_version.
*    lt_mcl_versions = VALUE #( ( lo_mat_clas_version_attribute )
*                               ( lo_mat_clas_version_fraction  ) ).
*
*    DATA(lo_mat_clas_data) = NEW cl_surdp_uph_wrap_mcl( iv_objek = 'PE_TST_1' it_mcl_versions = lt_mcl_versions ).
*
*    DATA lt_mat_clas_data TYPE surdpt_uph_wrap_mcl.
*    INSERT lo_mat_clas_data INTO TABLE lt_mat_clas_data.
*
*    th_surdp_uph_packaging_elem=>given_uom_of_characteristic( iv_characteristic_name  = 'ZRDP_PACKELEM_ATTR_LENGTH'
*                                                              iv_uom_value            = 'CM'
*                                                              io_charact_details_mock = mo_charact_details_mock ).
*
*    th_surdp_uph_packaging_elem=>given_uom_of_characteristic( iv_characteristic_name  = 'ZRDP_PACKELEM_ATTR_VOLUME'
*                                                              iv_uom_value            = 'CDM'
*                                                              io_charact_details_mock = mo_charact_details_mock ).
*
*    th_surdp_uph_packaging_elem=>given_uom_of_characteristic( iv_characteristic_name  = 'ZRDP_PACKELEM_FRAC1_WEIGHT'
*                                                              iv_uom_value            = 'G'
*                                                              io_charact_details_mock = mo_charact_details_mock ).
*
*    " When
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_mcl_proc~map_mat_clas_data( lt_mat_clas_data ).
*
*    " Then
*    cl_abap_testdouble=>verify_expectations( mo_charact_details_mock ).
*
*    DATA(lo_actual_entiy_data) = th_surdp_uph_packaging_elem=>then_assert_single_pack_elem( lt_actual_entity_data ).
*    th_surdp_uph_packaging_elem=>then_assert_pack_elem_data( io_actual_entiy_data = lo_actual_entiy_data
*                                                             iv_source_id         = 'S4H'
*                                                             iv_display_id        = 'PE_TST_1' ).
*
*    DATA(lo_frac_data) = th_surdp_uph_packaging_elem=>then_assert_single_fraction( io_actual_entity_data = lo_actual_entiy_data iv_frac_number = 1 ).
*    th_surdp_uph_packaging_elem=>then_assert_fraction_data( io_actual_frac_data = lo_frac_data ).
*  ENDMETHOD.
*
*
*  METHOD map_mat_clas_data_4_fractions.
*    " Given
*    th_surdp_uph_packaging_elem=>given_input_params( io_cut = mo_cut iv_source_id = 'S4H' ).
*
*    DATA(lo_mat_clas_version_attribute) = given_version_for_packelem( iv_objek = 'TST_1' iv_class = 'ZMCL_PACKELEM_ATTR' ).
*    DATA(lo_mat_clas_version_fraction1)  = given_version_for_fraction( iv_objek = 'TST_1' iv_class = 'ZMCL_PACKELE_FRAC1' iv_frac_number = 1 ).
*    DATA(lo_mat_clas_version_fraction2)  = given_version_for_fraction( iv_objek = 'TST_1' iv_class = 'ZMCL_PACKELE_FRAC2' iv_frac_number = 2 ).
*    DATA(lo_mat_clas_version_fraction3)  = given_version_for_fraction( iv_objek = 'TST_1' iv_class = 'ZMCL_PACKELE_FRAC3' iv_frac_number = 3 ).
*    DATA(lo_mat_clas_version_fraction4)  = given_version_for_fraction( iv_objek = 'TST_1' iv_class = 'ZMCL_PACKELE_FRAC4' iv_frac_number = 4 ).
*
*    DATA lt_mcl_versions TYPE surdpt_uph_wrap_mcl_version.
*    lt_mcl_versions = VALUE #( ( lo_mat_clas_version_attribute )
*                               ( lo_mat_clas_version_fraction1  )
*                               ( lo_mat_clas_version_fraction2  )
*                               ( lo_mat_clas_version_fraction3  )
*                               ( lo_mat_clas_version_fraction4  ) ).
*
*    DATA(lo_mat_clas_data) = NEW cl_surdp_uph_wrap_mcl( iv_objek = 'PE_TST_1' it_mcl_versions = lt_mcl_versions ).
*
*    DATA lt_mat_clas_data TYPE surdpt_uph_wrap_mcl.
*    INSERT lo_mat_clas_data INTO TABLE lt_mat_clas_data.
*
*    th_surdp_uph_packaging_elem=>given_uom_of_characteristic( iv_characteristic_name  = 'ZRDP_PACKELEM_ATTR_LENGTH'
*                                                              iv_uom_value            = 'CM'
*                                                              io_charact_details_mock = mo_charact_details_mock ).
*
*    th_surdp_uph_packaging_elem=>given_uom_of_characteristic( iv_characteristic_name  = 'ZRDP_PACKELEM_ATTR_VOLUME'
*                                                              iv_uom_value            = 'CDM'
*                                                              io_charact_details_mock = mo_charact_details_mock ).
*
*    th_surdp_uph_packaging_elem=>given_uom_of_characteristic( iv_characteristic_name  = 'ZRDP_PACKELEM_FRAC1_WEIGHT'
*                                                              iv_uom_value            = 'G'
*                                                              io_charact_details_mock = mo_charact_details_mock ).
*
*    th_surdp_uph_packaging_elem=>given_uom_of_characteristic( iv_characteristic_name  = 'ZRDP_PACKELEM_FRAC2_WEIGHT'
*                                                              iv_uom_value            = 'G'
*                                                              io_charact_details_mock = mo_charact_details_mock ).
*
*    th_surdp_uph_packaging_elem=>given_uom_of_characteristic( iv_characteristic_name  = 'ZRDP_PACKELEM_FRAC3_WEIGHT'
*                                                              iv_uom_value            = 'G'
*                                                              io_charact_details_mock = mo_charact_details_mock ).
*
*    th_surdp_uph_packaging_elem=>given_uom_of_characteristic( iv_characteristic_name  = 'ZRDP_PACKELEM_FRAC4_WEIGHT'
*                                                              iv_uom_value            = 'G'
*                                                              io_charact_details_mock = mo_charact_details_mock ).
*
*    " When
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_mcl_proc~map_mat_clas_data( lt_mat_clas_data ).
*
*    " Then
*    cl_abap_testdouble=>verify_expectations( mo_charact_details_mock ).
*
*    DATA(lo_actual_entiy_data) = th_surdp_uph_packaging_elem=>then_assert_single_pack_elem( lt_actual_entity_data ).
*    th_surdp_uph_packaging_elem=>then_assert_pack_elem_data( io_actual_entiy_data = lo_actual_entiy_data
*                                                             iv_source_id         = 'S4H'
*                                                             iv_display_id        = 'PE_TST_1' ).
*
*    DATA(lo_frac_data) = th_surdp_uph_packaging_elem=>then_assert_single_fraction( io_actual_entity_data = lo_actual_entiy_data iv_frac_number = 1 ).
*    th_surdp_uph_packaging_elem=>then_assert_fraction_data( io_actual_frac_data = lo_frac_data ).
*
*    lo_frac_data = th_surdp_uph_packaging_elem=>then_assert_single_fraction( io_actual_entity_data = lo_actual_entiy_data iv_frac_number = 2 ).
*    th_surdp_uph_packaging_elem=>then_assert_fraction_data( io_actual_frac_data = lo_frac_data ).
*
*    lo_frac_data = th_surdp_uph_packaging_elem=>then_assert_single_fraction( io_actual_entity_data = lo_actual_entiy_data iv_frac_number = 3 ).
*    th_surdp_uph_packaging_elem=>then_assert_fraction_data( io_actual_frac_data = lo_frac_data ).
*
*    lo_frac_data = th_surdp_uph_packaging_elem=>then_assert_single_fraction( io_actual_entity_data = lo_actual_entiy_data iv_frac_number = 4 ).
*    th_surdp_uph_packaging_elem=>then_assert_fraction_data( io_actual_frac_data = lo_frac_data ).
*  ENDMETHOD.
*
*  METHOD map_mat_clas_data_no_input.
*    " Given empty input table
*    DATA lt_mat_clas_data TYPE surdpt_uph_wrap_mcl.
*
*    " When
*    DATA(lt_actual_entity_data) = mo_cut->if_surdp_uph_entity_mcl_proc~map_mat_clas_data( lt_mat_clas_data ).
*
*    " Then
*    cl_abap_unit_assert=>assert_initial( lt_actual_entity_data ).
*
*  ENDMETHOD.
*ENDCLASS.
