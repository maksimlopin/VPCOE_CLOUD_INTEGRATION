CLASS /vpcoe/cl_uph_ent_pckg_cmp_gp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.                                    "#EC INTF_IN_CLASS

    INTERFACES /vpcoe/if_uph_entity_data .

    TYPES:

      BEGIN OF lty_s_sel_period,
        from TYPE dats,
        to   TYPE dats,
      END OF lty_s_sel_period,

      BEGIN OF lty_s_packcomp_for_prod,
        id                         TYPE c LENGTH 40,
        valid_from                 TYPE dats,
        valid_to                   TYPE dats,
        supplier_id                TYPE c LENGTH 10,
        business_process_direction TYPE c LENGTH 20,
      END OF lty_s_packcomp_for_prod,
      lty_t_packcomp_for_prod TYPE STANDARD TABLE OF lty_s_packcomp_for_prod WITH DEFAULT KEY,

      BEGIN OF lty_s_prd_assigment,
        product_id                 TYPE string,
        business_process_direction TYPE string,
        supplier_id                TYPE string,
        packaging_compositions     TYPE lty_t_packcomp_for_prod,
      END OF lty_s_prd_assigment,
      lty_t_prd_assigments TYPE STANDARD TABLE OF lty_s_prd_assigment WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        is_sel_period      TYPE lty_s_sel_period
        it_prd_assignments TYPE lty_t_prd_assigments.

    METHODS:
      get_selection_period
        RETURNING VALUE(rv_result) TYPE lty_s_sel_period,

      get_product_assignments
        RETURNING VALUE(rv_result) TYPE lty_t_prd_assigments,

      add_packcomp_for_product
        IMPORTING
          is_product_assignment    TYPE lty_s_prd_assigment
          is_packaging_composition TYPE lty_s_packcomp_for_prod.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      ms_sel_period      TYPE lty_s_sel_period,
      mt_prd_assignments TYPE lty_t_prd_assigments.

ENDCLASS.



CLASS /VPCOE/CL_UPH_ENT_PCKG_CMP_GP IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_data~is_marked_deleted.
    rv_mark_deleted = abap_false.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_data~to_console.
  ENDMETHOD.


  METHOD add_packcomp_for_product.
    LOOP AT mt_prd_assignments REFERENCE INTO DATA(lr_product_assignment).

      IF     is_product_assignment-product_id                 = lr_product_assignment->product_id
         AND is_product_assignment-business_process_direction = lr_product_assignment->business_process_direction
         AND is_product_assignment-supplier_id                = lr_product_assignment->supplier_id.

        APPEND VALUE #( id                         = is_packaging_composition-id
                        valid_from                 = is_packaging_composition-valid_from
                        valid_to                   = is_packaging_composition-valid_to
                        supplier_id                = is_packaging_composition-supplier_id
                        business_process_direction = is_packaging_composition-business_process_direction ) TO lr_product_assignment->packaging_compositions.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.

    ms_sel_period = is_sel_period.
    mt_prd_assignments = it_prd_assignments.

  ENDMETHOD.


  METHOD get_product_assignments.
    rv_result = mt_prd_assignments.
  ENDMETHOD.


  METHOD get_selection_period.
    rv_result = ms_sel_period.
  ENDMETHOD.
ENDCLASS.
