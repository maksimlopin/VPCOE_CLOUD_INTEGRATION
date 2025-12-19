class /VPCOE/CL_PCKELEM_MCL_EXMPL definition
  public
  inheriting from /VPCOE/CL_UPH_PROC_BASE_MCL
  create public .

public section.

  methods /VPCOE/IF_UPH_ENTITY_MCL_PROC~GET_RELEVANT_MAT_CLAS
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_MCL_PROC~MAP_MAT_CLAS_DATA
    redefinition .
protected section.
private section.

  constants MC_MAX_FRACTION_COUNT type I value 4 ##NO_TEXT.

  methods MAP_FRACTION_DATA
    importing
      !IO_MAT_CLAS_DATA type ref to /VPCOE/CL_UPH_WRAP_MCL
      !IO_MAT_CLAS_VERSION type ref to /VPCOE/CL_UPH_WRAP_MCL_VERSION
    returning
      value(RT_PACK_FRACT_ENTITY_DATA) type /VPCOE/T_UPH_ENTITY_DATA .
ENDCLASS.



CLASS /VPCOE/CL_PCKELEM_MCL_EXMPL IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_mcl_proc~get_relevant_mat_clas.
    rt_relevant_mat_clas = VALUE #( ( class_number = 'ZMCL_PACKELEM_ATTR' )
                                    ( class_number = 'ZMCL_PACKELE_FRAC1' )
                                    ( class_number = 'ZMCL_PACKELE_FRAC2' )
                                    ( class_number = 'ZMCL_PACKELE_FRAC3' )
                                    ( class_number = 'ZMCL_PACKELE_FRAC4' ) ).
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_mcl_proc~map_mat_clas_data.
    DATA: ls_packelem_data          TYPE /vpcoe/s_uph_ent_pack_elem,
          ls_packelem_prod_data     TYPE /vpcoe/s_uph_ent_pack_prod,
          lt_pack_fract_entity_data TYPE /vpcoe/t_uph_entity_data,
          lt_pack_prod_entity_data  TYPE /vpcoe/t_uph_entity_data,
          lv_valid_from             TYPE datuv.

    " loop all materials with material class data
    LOOP AT it_mat_clas_data INTO DATA(lo_mat_clas_data).

      DATA(lt_mat_clas_versions) = lo_mat_clas_data->get_versions_by_class( iv_class = 'ZMCL_PACKELEM_ATTR' ).

      IF lt_mat_clas_versions IS INITIAL.
        CONTINUE.
      ENDIF.

      " Packaging element cardinality [0..n]
      LOOP AT lt_mat_clas_versions INTO DATA(lo_mat_clas_version).

        CLEAR: ls_packelem_data, lv_valid_from.

        ls_packelem_data-displayid              = |{ lo_mat_clas_data->get_product_id( ) }|.
        ls_packelem_data-source                 = ms_parameters-source_id.

        lv_valid_from                           = COND #( WHEN lo_mat_clas_version->get_valid_from( ) = '00000000'
                                                          THEN '00010101'
                                                          ELSE lo_mat_clas_version->get_valid_from( ) ).

        " convert date to required format YYYY-MM-DD
        ls_packelem_data-version                = |{ lv_valid_from(4) }-{ lv_valid_from+4(2) }-{ lv_valid_from+6(2) }|.
*        ls_packelem_data-version                = '1'.
        ls_packelem_data-description            = lo_mat_clas_data->get_product_description( ).
        ls_packelem_data-unitofmeasure          = lo_mat_clas_version->get_character_val( 'ZRDP_PACKELEM_ATTR_UOM'  ).
        ls_packelem_data-packagingtype          = lo_mat_clas_version->get_neutral_val( 'ZRDP_PACKELEM_TYPE'  ).
        ls_packelem_data-usage_type             = lo_mat_clas_version->get_neutral_val( 'ZRDP_PACKELEM_ATTR_USAGE'  ).
        ls_packelem_data-flexibility            = lo_mat_clas_version->get_neutral_val( 'ZRDP_PACKELEM_ATTR_FLEXIBILTY'  ).
        ls_packelem_data-isreusable             = lo_mat_clas_version->get_boolean_val( 'ZRDP_PACKELEM_ATTR_IS_REUSABLE'  ).
        ls_packelem_data-reusecount             = lo_mat_clas_version->get_number_val( 'ZRDP_PACKELEM_ATTR_REUSE_TIMES'  ).
        ls_packelem_data-recyclablepercent      = lo_mat_clas_version->get_number_val( 'ZRDP_PACKELEM_ATTR_RECYCL_PERC'  ).
        ls_packelem_data-compostablepercent     = lo_mat_clas_version->get_number_val( 'ZRDP_PACKELEM_ATTR_COMPOT_PERC'  ).
        ls_packelem_data-isaseptic              = lo_mat_clas_version->get_boolean_val( 'ZRDP_PACKELEM_ATTR_IS_ASEPTIC'  ).
        ls_packelem_data-isnotempty             = lo_mat_clas_version->get_boolean_val( 'ZRDP_PACKELEM_ATTR_IS_NOTEMPTY'  ).
        ls_packelem_data-dimensionlength        = lo_mat_clas_version->get_number_val( 'ZRDP_PACKELEM_ATTR_LENGTH'  ).
        ls_packelem_data-dimensionwidth         = lo_mat_clas_version->get_number_val( 'ZRDP_PACKELEM_ATTR_WIDTH'  ).
        ls_packelem_data-dimensionheight        = lo_mat_clas_version->get_number_val( 'ZRDP_PACKELEM_ATTR_HEIGHT'  ).
        ls_packelem_data-dimensionunitofmeasure = lo_mat_clas_version->get_uom_of_characteristic( 'ZRDP_PACKELEM_ATTR_LENGTH' ).
        ls_packelem_data-volume                 = lo_mat_clas_version->get_number_val( 'ZRDP_PACKELEM_ATTR_VOLUME'  ).
        ls_packelem_data-volumeunitofmeasure    = lo_mat_clas_version->get_uom_of_characteristic( 'ZRDP_PACKELEM_ATTR_VOLUME' ).
        ls_packelem_data-isdeposit              = lo_mat_clas_version->get_boolean_val( 'ZRDP_PACKELEM_ATTR_IS_DEPOSIT' ).
        ls_packelem_data-isservicepackaging     = lo_mat_clas_version->get_boolean_val( 'ZRDP_PACKELEM_ATTR_IS_SERVPACK' ).
        ls_packelem_data-isopticallydetectable  = lo_mat_clas_version->get_boolean_val( 'ZRDP_PACKELEM_ATTR_IS_OPT_DECT' ).
        ls_packelem_data-laminationtype         = lo_mat_clas_version->get_neutral_val( 'ZRDP_PACKELEM_ATTR_LAMINA_TYPE' ).
        ls_packelem_data-reuselifetimeinyears   = 0.
        ls_packelem_data-refillconsumertype     = lo_mat_clas_version->get_neutral_val( 'ZRDP_PACKELEM_ATTR_REFILL_CON' ).

        " Packaging Element to Products [0..1]
        CLEAR lt_pack_prod_entity_data.

        CLEAR ls_packelem_prod_data.
        ls_packelem_prod_data-productid = lo_mat_clas_data->get_product_id( ).
        DATA(lo_ent_packelem_prod) = NEW /vpcoe/cl_uph_ent_pckg_product(
                                             is_data    = ls_packelem_prod_data
                                             iv_deleted = lo_mat_clas_version->is_marked_deleted( ) ).
        INSERT lo_ent_packelem_prod INTO TABLE lt_pack_prod_entity_data.

        " Packaging fractions [0..n]
        lt_pack_fract_entity_data = map_fraction_data( io_mat_clas_data = lo_mat_clas_data io_mat_clas_version = lo_mat_clas_version ).

        DATA(lo_packelem_data) = NEW /vpcoe/cl_uph_ent_pckg_element(
                                         is_data      = ls_packelem_data
                                         it_fractions = lt_pack_fract_entity_data
                                         it_products  = lt_pack_prod_entity_data
                                         iv_deleted   = lo_mat_clas_version->is_marked_deleted( ) ).
        INSERT lo_packelem_data INTO TABLE rt_entity_data.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD map_fraction_data.
    DATA ls_packelem_frac_data TYPE /vpcoe/s_uph_ent_pack_frac.
    DATA lt_packelem_fract     TYPE /vpcoe/uph_wrap_mcl_version.
    DATA lo_packelem_frac      TYPE REF TO /vpcoe/cl_ent_pckg_fraction.
    DATA lv_counter            TYPE i VALUE 1.

    WHILE lv_counter <= mc_max_fraction_count.

      CLEAR lt_packelem_fract.

      lt_packelem_fract = io_mat_clas_data->get_versions_by_class(
                             iv_class      = 'ZMCL_PACKELE_FRAC' && lv_counter
                             iv_valid_from = io_mat_clas_version->get_valid_from( ) ).

      LOOP AT lt_packelem_fract INTO DATA(lo_packelem_fract).

        CLEAR ls_packelem_frac_data.

        ls_packelem_frac_data-basicmaterialfractionid = lo_packelem_fract->get_neutral_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_CODE' ).

        IF ls_packelem_frac_data-basicmaterialfractionid IS INITIAL.
          CONTINUE.
        ENDIF.

        ls_packelem_frac_data-weight                  = lo_packelem_fract->get_number_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_WEIGHT' ).
        ls_packelem_frac_data-weightunitofmeasure     = lo_packelem_fract->get_uom_of_characteristic( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_WEIGHT' ).
        ls_packelem_frac_data-transparency            = lo_packelem_fract->get_neutral_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_TRANSPAREN'  ).
        ls_packelem_frac_data-color                   = lo_packelem_fract->get_neutral_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_COLOR' ).
        ls_packelem_frac_data-recycledcontentpercent  = lo_packelem_fract->get_number_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_RECYCO_PER'  ).
        ls_packelem_frac_data-thickness               = lo_packelem_fract->get_number_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_THICKNESS'  ).
        ls_packelem_frac_data-thicknessunitofmeasure  = lo_packelem_fract->get_uom_of_characteristic( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_THICKNESS' ).
        ls_packelem_frac_data-density                 = lo_packelem_fract->get_number_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_DENSITY'  ).
        ls_packelem_frac_data-densityunitofmeasure    = lo_packelem_fract->get_uom_of_characteristic( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_DENSITY' ).
        ls_packelem_frac_data-isreinforced            = lo_packelem_fract->get_boolean_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_IS_REINFOR'  ).
        ls_packelem_frac_data-isexpanded              = lo_packelem_fract->get_boolean_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_IS_EXPNDED'  ).
        ls_packelem_frac_data-renewablepercent        = lo_packelem_fract->get_number_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_RENEWA_PER' ).
        ls_packelem_frac_data-chmlrecycledcntntpct              = lo_packelem_fract->get_number_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_CHEMRC_PER' ).
        ls_packelem_frac_data-chmlrecycledcntntvalmeth          = lo_packelem_fract->get_neutral_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_CHEMRC_VAM'  ).
        ls_packelem_frac_data-mechanicalrecycledcntntpct        = lo_packelem_fract->get_number_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_MECHRC_PER' ).
        ls_packelem_frac_data-mechanicalrecycledcntntvalmeth    = lo_packelem_fract->get_neutral_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_MECHRC_VAM'  ).

        ls_packelem_frac_data-functions = VALUE #( ( function = lo_packelem_fract->get_neutral_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_FUNCTION1')
                                                     weightpercent = lo_packelem_fract->get_number_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_WGHT_PERC1' ) )
                                                   ( function = lo_packelem_fract->get_neutral_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_FUNCTION2')
                                                     weightpercent = lo_packelem_fract->get_number_val( 'ZRDP_PACKELEM_FRAC' && lv_counter && '_WGHT_PERC2' ) ) ).

        DELETE ls_packelem_frac_data-functions WHERE function IS INITIAL AND weightpercent IS INITIAL.

        lo_packelem_frac = NEW /vpcoe/cl_ent_pckg_fraction( is_data    = ls_packelem_frac_data
                                                            iv_deleted = io_mat_clas_version->is_marked_deleted( ) ).

        INSERT lo_packelem_frac INTO TABLE rt_pack_fract_entity_data.
      ENDLOOP.

      ADD 1 TO lv_counter.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
