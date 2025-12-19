class /VPCOE/CL_UPH_PCG_CMP_BOM_LOAD definition
  public
  inheriting from /VPCOE/CL_UPH_REPORT_BASE
  final
  create public .

public section.

  interfaces /VPCOE/IF_UPH_PCG_CMP_BOM_LOAD .

  aliases DERIVE_INPUT_FROM_PARAMETERS
    for /VPCOE/IF_UPH_PCG_CMP_BOM_LOAD~DERIVE_INPUT_FROM_PARAMETERS .
  aliases DERIVE_RFC_TARGET_DESTINATION
    for /VPCOE/IF_UPH_REPORT~DERIVE_RFC_TARGET_DESTINATION .
  aliases DERIVE_UPLOAD_MODE
    for /VPCOE/IF_UPH_REPORT~DERIVE_UPLOAD_MODE .
  aliases EXECUTE_UPLOAD
    for /VPCOE/IF_UPH_REPORT~EXECUTE_UPLOAD .
  aliases IS_AUTHORIZED
    for /VPCOE/IF_UPH_PCG_CMP_BOM_LOAD~IS_AUTHORIZED .

  methods CONSTRUCTOR
    importing
      !IO_EXEC_LOAD_WRAPPER type ref to /VPCOE/IF_UPH_EXEC_LOAD_WRAP optional
      !IO_SURDP_UPH_FACTORY type ref to /VPCOE/IF_UPH_FACTORY optional .

  methods /VPCOE/IF_UPH_REPORT~DERIVE_RFC_TARGET_DESTINATION
    redefinition .
  methods /VPCOE/IF_UPH_REPORT~DERIVE_UPLOAD_MODE
    redefinition .
  methods /VPCOE/IF_UPH_REPORT~EXECUTE_UPLOAD
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /VPCOE/CL_UPH_PCG_CMP_BOM_LOAD IMPLEMENTATION.


  METHOD /vpcoe/if_uph_pcg_cmp_bom_load~derive_input_from_parameters.

    rv_value-source_id    = iv_source_id.
    rv_value-rfc_des      = iv_rfc_destination.
    rv_value-api_type     = iv_api_type.
    rv_value-package_size = iv_package_size.
    rv_value-path_prefix  = iv_path_prefix.
    rv_value-material    = iv_material_range.
    rv_value-mat_type    = iv_material_type_range.
    rv_value-plant       = iv_plant_range.
    rv_value-bom_alter   = iv_alternative_bom_range.
    rv_value-bom_usage   = iv_bom_usage_range.
    rv_value-bom_chgdon  = iv_bom_change_date_range.
    rv_value-valfromdate = iv_bom_validity_date.
    rv_value-bom_status  = iv_bom_status.
    rv_value-max_exp_lvl = iv_max_explosion_level.
    rv_value-load_id     = iv_load_id.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_pcg_cmp_bom_load~is_authorized.
    AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD'  FIELD /vpcoe/if_uph_pcg_cmp_bom_load~gc_tcode.

    IF sy-subrc <> 0.
      rv_value = abap_false.
    ELSE.
      rv_value = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_report~derive_rfc_target_destination.
    rv_value = super->/vpcoe/if_uph_report~derive_rfc_target_destination( ).
  ENDMETHOD.


  METHOD derive_upload_mode.
    " The method was refactored and is kept as deleting methods is not allowed in downports
    rv_value = super->/vpcoe/if_uph_report~derive_upload_mode( ic_initial_load = ic_initial_load
                                                               ic_delta_load   = ic_delta_load ).
  ENDMETHOD.


  METHOD /vpcoe/if_uph_report~execute_upload.
    " The method was refactored and is kept as deleting methods is not allowed in downports
    super->/vpcoe/if_uph_report~execute_upload( iv_upload_entity = iv_upload_entity
                                                iv_upload_mode   = iv_upload_mode
                                                is_parameters    = is_parameters
                                                iv_test          = iv_test ).
  ENDMETHOD.


  METHOD constructor.
    " The constructor was refactored and is kept to ease downports
    super->constructor( io_exec_load_wrapper = io_exec_load_wrapper io_surdp_uph_factory = io_surdp_uph_factory ).
  ENDMETHOD.
ENDCLASS.
