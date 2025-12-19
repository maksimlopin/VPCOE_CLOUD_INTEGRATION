INTERFACE /vpcoe/if_uph_entity_hu_proc
  PUBLIC.

  TYPES: BEGIN OF gty_s_hu_for_product,
           product       TYPE matnr,
           handling_unit TYPE cuobn,
           amount        TYPE p LENGTH 14 DECIMALS 6,
           base_unit     TYPE meins,
           prd_amount    TYPE p LENGTH 14 DECIMALS 6,
         END OF gty_s_hu_for_product,
         gty_t_hu_for_product TYPE STANDARD TABLE OF gty_s_hu_for_product WITH NON-UNIQUE KEY primary_key COMPONENTS product handling_unit.

  TYPES: BEGIN OF gty_s_api_sel_criteria,
           product_id                 TYPE string,
           business_process_direction TYPE string,
           supplier_id                TYPE string,
         END OF gty_s_api_sel_criteria.

  TYPES: BEGIN OF gty_s_api_packcomp_for_prod,
           id                         TYPE c LENGTH 40,
           valid_from                 TYPE dats,
           valid_to                   TYPE dats,
           supplier_id                TYPE c LENGTH 10,
           business_process_direction TYPE c LENGTH 20,
         END OF gty_s_api_packcomp_for_prod,
         gty_t_api_packcomp_for_prod TYPE STANDARD TABLE OF gty_s_api_packcomp_for_prod WITH DEFAULT KEY.

  TYPES: BEGIN OF gty_s_api_rp_packcomp_for_prod,
           selection_criteria     TYPE gty_s_api_sel_criteria,
           packaging_compositions TYPE gty_t_api_packcomp_for_prod,
         END OF gty_s_api_rp_packcomp_for_prod,
         gty_t_api_rp_packcomp_for_prod TYPE STANDARD TABLE OF gty_s_api_rp_packcomp_for_prod WITH DEFAULT KEY.

  METHODS:

    map_hu_data DEFAULT IGNORE
      IMPORTING it_packcomp_data      TYPE gty_t_api_rp_packcomp_for_prod
                it_handling_units     TYPE gty_t_hu_for_product
      RETURNING VALUE(rt_entity_data) TYPE /vpcoe/t_uph_entity_data.


ENDINTERFACE.
