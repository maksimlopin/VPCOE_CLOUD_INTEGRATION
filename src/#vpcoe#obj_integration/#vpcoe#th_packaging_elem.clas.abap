CLASS /vpcoe/th_packaging_elem DEFINITION FOR TESTING RISK LEVEL HARMLESS
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS given_input_params IMPORTING io_cut       TYPE REF TO /vpcoe/if_uph_entity_proc
                                               iv_source_id TYPE /vpcoe/uph_source_id.

    CLASS-METHODS given_uom_of_characteristic IMPORTING iv_characteristic_name  TYPE atnam
                                                        iv_uom_value            TYPE msehi
                                                        io_charact_details_mock TYPE REF TO /vpcoe/if_uph_charact_details.

    CLASS-METHODS then_assert_single_pack_elem IMPORTING it_entity_data   TYPE /vpcoe/uph_entity_data
                                               RETURNING VALUE(rv_result) TYPE REF TO /vpcoe/cl_uph_ent_pckg_element.

    CLASS-METHODS then_assert_pack_elem_data IMPORTING io_actual_entiy_data TYPE REF TO /vpcoe/cl_uph_ent_pckg_element
                                                       iv_source_id         TYPE char64
                                                       iv_display_id        TYPE char40
                                                       iv_version           TYPE string.

    CLASS-METHODS then_assert_product_data IMPORTING io_actual_entiy_data TYPE REF TO /vpcoe/cl_uph_ent_pckg_element
                                                     iv_product_id        TYPE any OPTIONAL.

    CLASS-METHODS then_assert_single_fraction IMPORTING io_actual_entity_data TYPE REF TO /vpcoe/cl_uph_ent_pckg_element
                                                        iv_frac_number        TYPE i
                                              RETURNING VALUE(rv_result)      TYPE REF TO /vpcoe/cl_ent_pckg_fraction.

    CLASS-METHODS then_assert_fraction_data IMPORTING io_actual_frac_data TYPE REF TO /vpcoe/cl_ent_pckg_fraction.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /VPCOE/TH_PACKAGING_ELEM IMPLEMENTATION.


  METHOD given_input_params.
    DATA ls_input_parameters TYPE /vpcoe/s_pckg_elem_input.

    ls_input_parameters-source_id = iv_source_id.

    io_cut->init_processor( iv_upload_entity = ''
                            iv_upload_mode   = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
                            is_parameters    = ls_input_parameters ).
  ENDMETHOD.


  METHOD given_uom_of_characteristic.
    cl_abap_testdouble=>configure_call( double = io_charact_details_mock )->returning( value = iv_uom_value )->times( 1 ).
    io_charact_details_mock->get_uom_of_characteristic( iv_characteristic_name = iv_characteristic_name ).
  ENDMETHOD.


  METHOD then_assert_fraction_data.
    DATA ls_fraction_data TYPE /vpcoe/s_uph_ent_pack_frac.

    ls_fraction_data-basicmaterialfractionid        = '1310'.
    ls_fraction_data-weight                         = '5'.
    ls_fraction_data-weightunitofmeasure            = 'G'.
    ls_fraction_data-isreinforced                   = abap_undefined.
    ls_fraction_data-chmlrecycledcntntpct           = '10'.
    ls_fraction_data-chmlrecycledcntntvalmeth       = 'MB'.
    ls_fraction_data-mechanicalrecycledcntntpct     = '15'.
    ls_fraction_data-mechanicalrecycledcntntvalmeth = 'MB'.

    cl_abap_unit_assert=>assert_equals( exp = ls_fraction_data
                                        act = io_actual_frac_data->get_data( ) ).
  ENDMETHOD.


  METHOD then_assert_pack_elem_data.
    DATA ls_pack_element TYPE /vpcoe/s_uph_ent_pack_elem.

    ls_pack_element-source                 = iv_source_id.
    ls_pack_element-displayid              = iv_display_id.
    ls_pack_element-version                = iv_version.

    ls_pack_element-dimensionunitofmeasure = 'CM'.
    ls_pack_element-volumeunitofmeasure    = 'CDM'.

    ls_pack_element-isreusable             = abap_undefined.
    ls_pack_element-isaseptic              = abap_undefined.
    ls_pack_element-isnotempty             = abap_undefined.
    ls_pack_element-isdeposit              = abap_undefined.
    ls_pack_element-isservicepackaging     = abap_undefined.
    ls_pack_element-isopticallydetectable  = abap_undefined.
    ls_pack_element-reuselifetimeinyears   = 0.
    cl_abap_unit_assert=>assert_equals( exp = ls_pack_element act = io_actual_entiy_data->get_data( ) ).
  ENDMETHOD.


  METHOD then_assert_product_data.
    DATA lo_prod_entity_data      TYPE REF TO /vpcoe/cl_uph_ent_pckg_product.
    DATA lt_pack_prod_entity_data TYPE /vpcoe/t_uph_entity_data.
    DATA ls_packcomp_prod_data    TYPE /vpcoe/s_uph_ent_pack_prod.

    ls_packcomp_prod_data-productid = iv_product_id.
    lo_prod_entity_data = NEW #( ls_packcomp_prod_data ).
    INSERT lo_prod_entity_data INTO TABLE lt_pack_prod_entity_data.

    DATA(lv_actual_products) = io_actual_entiy_data->get_products( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lv_actual_products ) ).
    DATA lv_actual_product TYPE REF TO /vpcoe/cl_uph_ent_pckg_product.
    lv_actual_product ?= lv_actual_products[ 1 ].
    cl_abap_unit_assert=>assert_equals( exp = ls_packcomp_prod_data act = lv_actual_product->get_data( ) ).
  ENDMETHOD.


  METHOD then_assert_single_fraction.
    DATA(lt_frac_data) = io_actual_entity_data->get_fractions( ).
    rv_result ?= lt_frac_data[ iv_frac_number ].
  ENDMETHOD.


  METHOD then_assert_single_pack_elem.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( it_entity_data ) ).
    rv_result ?= it_entity_data[ 1 ].
  ENDMETHOD.
ENDCLASS.
