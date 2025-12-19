class /VPCOE/CL_EXMPL_UPH_CUSTOM definition
  public
  final
  create public .

public section.

  interfaces /VPCOE/IF_UPH_CUSTOM .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS /VPCOE/CL_EXMPL_UPH_CUSTOM IMPLEMENTATION.


METHOD /vpcoe/if_uph_custom~get_entity_mapper.
* In case of a missing implementation, the fallback class will be used:
* - For V1: /vpcoe/cl_uph_tm_pckg_cmp_v1, /vpcoe/cl_uph_tm_pckg_elem_v1, /vpcoe/cl_uph_tm_hu.
* - For V2: /vpcoe/cl_uph_tm_pckg_cmp_v2, /vpcoe/cl_uph_tm_pckg_elem_v2, /vpcoe/cl_uph_tm_hu.

* Example implementation:
* If the upload entity matches one of the packaging composition types (PLM, Recipe, or BOM),
* the V2 mapper class /vpcoe/cl_tm_pckg_cmp_v2 and  /vpcoe/cl_uph_tm_pckg_elem_v2 are assigned.

  CASE iv_upload_entity.
    WHEN "/vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_plm OR
         "/vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_rcp OR
         /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom.

      ro_mapper = NEW /vpcoe/cl_uph_tm_pckg_cmp_v1( ).
*      ro_mapper = NEW /vpcoe/cl_uph_tm_pckg_cmp_v2( ).

    WHEN "/vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm OR
          /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl.

      ro_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_elem_v1( ).
*      ro_mapper ?= NEW /vpcoe/cl_uph_tm_pckg_elem_v2( ).

  ENDCASE.

ENDMETHOD.


METHOD /vpcoe/if_uph_custom~get_entity_processor.
* For v1 is needed to specify the processor class itself
* For v2 the mapper handler class can be specified only (in the method GET_ENTITY_MAPPER),
* example processor class can be left, in case of more adjustments need the processor class
* can be replaced by custom

  CASE iv_upload_entity.
      "packaging elements
    WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pckg_plm.
*      ro_processor ?= NEW /vpcoe/cl_pckelem_plm_exmpl( ).
      "packaging composition from plm

    WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_plm.
*      ro_processor ?= NEW /vpcoe/cl_pckcomp_plm_exmpl( ).

      "packaging composition from recipe
    WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_rcp.
*      ro_processor ?= NEW /vpcoe/cl_pckcomp_rcp_exmpl( ).

    WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom.
      ro_processor ?= NEW /vpcoe/cl_pckcomp_bom_exmpl( ).

    WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pe_mcl.
      ro_processor ?= NEW /vpcoe/cl_pckelem_mcl_exmpl( ).

    WHEN /vpcoe/if_uph_entity_proc=>gc_upload_entities-gc_ue_pc_hu.
      ro_processor ?= NEW /vpcoe/cl_pckcomp_hu_exmpl( )."/vpcoe/cl_hu_exmpl( ).

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDMETHOD.


  METHOD /vpcoe/if_uph_custom~get_target_endpoint_dest.
    "Define target destination if it is required
    RETURN.
  ENDMETHOD.
ENDCLASS.
