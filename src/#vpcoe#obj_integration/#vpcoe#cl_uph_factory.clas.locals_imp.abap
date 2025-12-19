
CLASS lcl_local_badi_wrapper IMPLEMENTATION.

  METHOD constructor.

    TRY.
        GET BADI mo_badi_uph_custom.
      CATCH cx_badi_not_implemented  ##NO_HANDLER.     "#EC EMPTY_CATCH
        "badi implementation error will be handled later
    ENDTRY.
*          GET BADI lo_instance->mo_badi_uph_custom.
*        CATCH cx_badi_not_implemented  ##NO_HANDLER.   "#EC EMPTY_CATCH
  ENDMETHOD.

  METHOD lif_local_badi_wrapper~get_config_badi.

    ro_badi = mo_badi_uph_custom.

  ENDMETHOD.

  METHOD lif_local_badi_wrapper~get_entity_processor.

    IF mo_badi_uph_custom IS BOUND.
      TRY.
          CALL BADI mo_badi_uph_custom->get_entity_processor
            EXPORTING
              iv_upload_entity = iv_upload_entity
              iv_upload_mode   = iv_upload_mode
              is_parameters    = is_parameters
            RECEIVING
              ro_processor     = ro_processor.

        CATCH cx_badi_initial_reference   ##NO_HANDLER. "#EC EMPTY_CATCH
          "should not happen, since badi reference is checked as bound
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD lif_local_badi_wrapper~get_entity_mapper.

    IF mo_badi_uph_custom IS BOUND.
      TRY.
          CALL BADI mo_badi_uph_custom->get_entity_mapper
            EXPORTING
              iv_upload_entity = iv_upload_entity
            RECEIVING
              ro_mapper        = ro_mapper.

        CATCH cx_badi_initial_reference   ##NO_HANDLER. "#EC EMPTY_CATCH
          "should not happen, since badi reference is checked as bound
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD lif_local_badi_wrapper~get_target_endpoint_dest.

    IF mo_badi_uph_custom IS BOUND.
      TRY.
          CALL BADI mo_badi_uph_custom->get_target_endpoint_dest
            IMPORTING
              ev_endpoint_dest = ev_endpoint_dest.

        CATCH cx_badi_initial_reference   ##NO_HANDLER. "#EC EMPTY_CATCH
          "should not happen, since badi reference is checked as bound
      ENDTRY.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
