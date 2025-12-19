class /VPCOE/CL_EXMPL_PCKF_CUSTOM definition
  public
  final
  create public .

public section.

  interfaces /VPCOE/IF_PCKF_CUSTOM .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS /VPCOE/CL_EXMPL_PCKF_CUSTOM IMPLEMENTATION.


  METHOD /vpcoe/if_pckf_custom~determine_version.
    /vpcoe/cl_pckf_proc_base=>mv_version = 2.

  ENDMETHOD.


  method /VPCOE/IF_PCKF_CUSTOM~GET_ENTITY_PROCESSOR.

  "create instance of the class for entity processor
    CASE iv_entity_type.

      WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_pckg_fee.

       ro_processor ?= NEW lcl_exmpl_pckg_fee( ).

      WHEN OTHERS.
    ENDCASE.

  endmethod.


  method /VPCOE/IF_PCKF_CUSTOM~GET_TARGET_ENDPOINT_DEST.
    "return no specific endpoint destination (use default)
  endmethod.


  METHOD /vpcoe/if_pckf_custom~skip_determine_version.
    cv_skip = 2.
  ENDMETHOD.
ENDCLASS.
